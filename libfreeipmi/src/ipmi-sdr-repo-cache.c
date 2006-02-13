/* 
   sdr-repo-cache.c - IPMI SDR Caching functions

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#include "freeipmi.h"

int 
ipmi_sdr_repo_info_write (ipmi_device_t *dev, FILE *fp)
{
  uint8_t *data_rs = NULL;
  uint64_t val;
  
  if (!dev || !fp)
    {
      errno = EINVAL;
      return -1;
    }

  data_rs = alloca (fiid_obj_len_bytes (tmpl_get_sdr_repo_info_rs));
  
  if (ipmi_cmd_get_sdr_repo_info2 (dev, data_rs) != 0)
    return (-1);
  
  fiid_obj_get (data_rs, 
		tmpl_get_sdr_repo_info_rs, 
		(uint8_t *)"comp_code", 
		&val);
  if (val != 0)
    return (-1);
  
  if (fwrite (data_rs, 
	      fiid_obj_len_bytes (tmpl_get_sdr_repo_info_rs), 
	      1, 
	      fp) < 0)
    return (-1);
  return (0);
}

int 
ipmi_sdr_records_write (ipmi_device_t *dev, FILE *fp)
{
  uint16_t record_id = 0;
  uint8_t record_length = 0;
  fiid_obj_t obj_cmd_rs = NULL;
  fiid_obj_t obj_sdr_record = NULL;
  uint64_t val = 0;
  
  if (!dev || !fp)
    {
      errno = EINVAL;
      return (-1);
    }
  
  fiid_obj_alloca (obj_cmd_rs, tmpl_get_sdr_rs);
  
  record_id = 0;
  while (record_id != 0xFFFF)
    {
      fiid_obj_memset (obj_cmd_rs, 0, tmpl_get_sdr_rs);
      if (obj_sdr_record)
	{
	  free (obj_sdr_record);
	  obj_sdr_record = NULL;
	}
      
      if (ipmi_cmd_get_sdr2 (dev, 
			     record_id, 
			     obj_cmd_rs, 
			     &obj_sdr_record) != 0)
	{
	  return (-1);
	}
      
      fiid_obj_get (obj_cmd_rs, 
		    tmpl_get_sdr_rs, 
		    (uint8_t *)"next_record_id", 
		    &val);
      record_id = (uint16_t) val;
      
      fiid_obj_get (obj_sdr_record, 
		    tmpl_sdr_sensor_record_header, 
		    (uint8_t *)"record_length", 
		    &val);
      record_length = (uint8_t) val;
      
      record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
      
      if (fwrite (obj_sdr_record, 
		  record_length, 
		  1, 
		  fp) < 0)
	return (-1);
      
    }
  
  return (0);
}

int 
ipmi_sdr_cache_create (ipmi_device_t *dev, char *sdr_cache_file)
{
  FILE *cache_fp;
  
  if (!dev || !sdr_cache_file)
    {
      errno = EINVAL;
      return -1;
    }

  if ((cache_fp = fopen (sdr_cache_file, "w")) == NULL)
    return (-1);
  
  if (ipmi_sdr_repo_info_write (dev, cache_fp) != 0)
    {
      fclose (cache_fp);
      return (-1);
    }
  
  if (ipmi_sdr_records_write (dev, cache_fp) != 0)
    {
      fclose (cache_fp);
      return (-1);
    }
  
  fclose (cache_fp);
  return (0);
}

int 
ipmi_sdr_repo_cache_load (sdr_repo_cache_t *sdr_repo_cache, char *sdr_cache_file)
{
  struct stat buf;
  uint64_t val;
  
  if (!(sdr_repo_cache && sdr_cache_file))
    {
      errno = EINVAL;
      return -1;
    }

  sdr_repo_cache->fd = open (sdr_cache_file, O_RDONLY);
  
  if (sdr_repo_cache->fd <= 0)
    return (-1);
  
  if (fstat (sdr_repo_cache->fd, &buf) != 0)
    return (-1);
  
  sdr_repo_cache->file_length = buf.st_size;
  
  sdr_repo_cache->cache_start = (uint8_t *) mmap (NULL, 
						   sdr_repo_cache->file_length, 
						   PROT_READ, 
						   MAP_PRIVATE, 
						   sdr_repo_cache->fd, 
						   0);
  
  if (sdr_repo_cache->cache_start <= 0)
    return (-1);
  
  fiid_obj_get (sdr_repo_cache->cache_start, 
		tmpl_get_sdr_repo_info_rs, 
		(uint8_t *)"record_count", 
		&val);
  sdr_repo_cache->total_records = (uint32_t) val;
  
  sdr_repo_cache->cache_curr = sdr_repo_cache->cache_start + 
    fiid_obj_len_bytes (tmpl_get_sdr_repo_info_rs);
  
  sdr_repo_cache->cache_curr_rec_no = 1;
  
  return (0);
}

int 
ipmi_sdr_repo_cache_unload (sdr_repo_cache_t *sdr_repo_cache)
{
  if (sdr_repo_cache == NULL)
    {
      errno = EINVAL;
      return -1;
    }

  if (sdr_repo_cache->cache_start)
    {
      if (munmap (sdr_repo_cache->cache_start, sdr_repo_cache->file_length) != 0)
	return (-1);
    }
  
  if (sdr_repo_cache->fd)
    {
      sdr_repo_cache->file_length = 0;
      close (sdr_repo_cache->fd);
    }
  
  sdr_repo_cache->fd = 0;
  sdr_repo_cache->file_length = 0;
  sdr_repo_cache->cache_start = NULL;
  
  return 0;
}

int 
ipmi_sdr_repo_cache_seek (sdr_repo_cache_t *sdr_repo_cache, uint16_t rec_no)
{
  int i;
  
  if (sdr_repo_cache == NULL)
    {
      errno = EINVAL;
      return -1;
    }

  if (rec_no <= 0 || rec_no > sdr_repo_cache->total_records)
    {
      errno = ERANGE;
      return (-1);
    }
  
  if (rec_no >= sdr_repo_cache->cache_curr_rec_no)
    {
      /* skip (rec_no - sdr_repo_cache->cache_curr_rec_no) records */
      for (i = 0; i < (rec_no - sdr_repo_cache->cache_curr_rec_no); i++)
        {
          sdr_repo_cache->cache_curr = (sdr_repo_cache->cache_curr + 
                                        sdr_repo_cache->cache_curr[4] + 
                                        fiid_obj_len_bytes (tmpl_sdr_sensor_record_header));
        }
      sdr_repo_cache->cache_curr_rec_no += (rec_no - sdr_repo_cache->cache_curr_rec_no);
    }
  else
    {
      sdr_repo_cache->cache_curr = sdr_repo_cache->cache_start + 
        fiid_obj_len_bytes (tmpl_get_sdr_repo_info_rs);
      for (i = 1; i < rec_no; i++)
        {
          sdr_repo_cache->cache_curr = (sdr_repo_cache->cache_curr + 
                                        sdr_repo_cache->cache_curr[4] + 
                                        fiid_obj_len_bytes (tmpl_sdr_sensor_record_header));
        }
      sdr_repo_cache->cache_curr_rec_no = i;
    }
  
  return (0);
}

int 
ipmi_sdr_repo_cache_first (sdr_repo_cache_t *sdr_repo_cache)
{
  if (sdr_repo_cache == NULL)
    {
      errno = EINVAL;
      return -1;
    }

  return (ipmi_sdr_repo_cache_seek (sdr_repo_cache, 1));
}

int 
ipmi_sdr_repo_cache_next (sdr_repo_cache_t *sdr_repo_cache)
{
  if (sdr_repo_cache == NULL)
    {
      errno = EINVAL;
      return -1;
    }

  return (ipmi_sdr_repo_cache_seek (sdr_repo_cache, 
				    sdr_repo_cache->cache_curr_rec_no + 1));
}

int 
ipmi_is_sensor_reading_available (sdr_repo_cache_t *sdr_repo_cache)
{
  uint64_t val;
  
  if (sdr_repo_cache == NULL)
    {
      errno = EINVAL;
      return -1;
    }
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_sensor_record_header, 
		(uint8_t *)"record_type", 
		&val);
  
  switch (val)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      fiid_obj_get (sdr_repo_cache->cache_curr, 
		    tmpl_sdr_full_sensor_record, 
		    (uint8_t *)"slave_system_software_id", 
		    &val);
      if (ipmi_get_system_software_type (val) == IPMI_SYS_SOFT_ID_RESERVED)
	return (0);
      return (1);
      
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      fiid_obj_get (sdr_repo_cache->cache_curr, 
		    tmpl_sdr_compact_sensor_record, 
		    (uint8_t *)"slave_system_software_id", 
		    &val);
      if (ipmi_get_system_software_type (val) == IPMI_SYS_SOFT_ID_RESERVED)
	return (0);
      return (1);
      
    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
    case IPMI_SDR_FORMAT_ENTITY_ASSO_RECORD:
    case IPMI_SDR_FORMAT_DEV_ENTITY_ASSO_RECORD:
    case IPMI_SDR_FORMAT_GEN_DEV_LOCATOR_RECORD:
    case IPMI_SDR_FORMAT_FRU_DEV_LOCATOR_RECORD:
    case IPMI_SDR_FORMAT_MGMT_CNTRLR_DEV_LOCATOR_RECORD:
    case IPMI_SDR_FORMAT_MGMT_CNTRLR_CONFIRMATION_RECORD:
    case IPMI_SDR_FORMAT_BMC_MSG_CHANNEL_INFO_RECORD:
    case IPMI_SDR_FORMAT_OEM_RECORD:
      return (0);
      
    default:
      return (0);
    }
}

int 
ipmi_sdr_repo_cache_sensor_classify (sdr_repo_cache_t *sdr_repo_cache)
{
  uint64_t record_type, val;
  
  if (sdr_repo_cache == NULL)
    {
      errno = EINVAL;
      return -1;
    }

  fiid_obj_get(sdr_repo_cache->cache_curr, 
               tmpl_sdr_sensor_record_header,
               (uint8_t *)"record_type", 
               &record_type);

  if (record_type == IPMI_SDR_FORMAT_FULL_RECORD)
    fiid_obj_get (sdr_repo_cache->cache_curr, 
                  tmpl_sdr_full_sensor_record, 
                  (uint8_t *)"event_reading_type", 
                  &val);
  else if (record_type == IPMI_SDR_FORMAT_COMPACT_RECORD)
    fiid_obj_get (sdr_repo_cache->cache_curr, 
                  tmpl_sdr_compact_sensor_record, 
                  (uint8_t *)"event_reading_type", 
                  &val);
  else
    return IPMI_SENSOR_CLASS_NOT_AVAILABLE;
    
  return (ipmi_sensor_classify (val));
}

const char *
ipmi_sdr_repo_cache_get_sensor_group (sdr_repo_cache_t *sdr_repo_cache)
{
  uint8_t sensor_type;
  uint64_t val;
  
  if (sdr_repo_cache == NULL)
    {
      errno = EINVAL;
      return NULL;
    }

  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_sensor_record_header, 
		(uint8_t *)"record_type", 
		&val);
  
  if (val == IPMI_SDR_FORMAT_FULL_RECORD)
    {
      fiid_obj_get (sdr_repo_cache->cache_curr, 
		    tmpl_sdr_full_sensor_record, 
		    (uint8_t *)"sensor_type", 
		    &val);
      sensor_type = val;
      return ipmi_get_sensor_group (sensor_type);
    }
  
  if (val == IPMI_SDR_FORMAT_COMPACT_RECORD)
    {
      fiid_obj_get (sdr_repo_cache->cache_curr, 
		    tmpl_sdr_compact_sensor_record, 
		    (uint8_t *)"sensor_type", 
		    &val);
      sensor_type = val;
      return ipmi_get_sensor_group (sensor_type);
    }
  
  return NULL;
}

int
ipmi_sdr_repo_cache_get_sensor_name (sdr_repo_cache_t *sdr_repo_cache,
                                     uint8_t *buffer,
                                     size_t len)
{
  uint64_t val;
  uint32_t record_length;
  uint8_t record_type;

  if (sdr_repo_cache == NULL
      || buffer == NULL)
    {
      errno = EINVAL;
      return -1;
    }

  ERR (!(fiid_obj_get(sdr_repo_cache->cache_curr, 
                      tmpl_sdr_sensor_record_header, 
                      (uint8_t *)"record_type", 
                      &val) < 0));
  record_type = val;

  if (record_type == IPMI_SDR_FORMAT_FULL_RECORD)
    {
      ERR (fiid_obj_get(sdr_repo_cache->cache_curr,
                        tmpl_sdr_full_sensor_record,
                        (uint8_t *)"record_length",
                        &val) >= 0);
      record_length = val;
      record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);

      /* No name */
      if (record_length <= 48)
        return 0;

      /* +1 for null termination */
      if (len < (record_length - 48 + 1))
        {
          errno = ERANGE;
          return -1;
        }

      memset(buffer, '\0', len);
      /* No guarantee for null termination, must use memcpy */
      memcpy(buffer, &(sdr_repo_cache->cache_curr[48]), 
             (record_length - 48));
      
      return (record_length - 48);
    }
  else if (record_type == IPMI_SDR_FORMAT_COMPACT_RECORD)
    {
      ERR (fiid_obj_get(sdr_repo_cache->cache_curr,
                        tmpl_sdr_compact_sensor_record,
                        (uint8_t *)"record_length",
                        &val) >= 0);
      record_length = val;
      record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);

      /* No name */
      if (record_length <= 32)
        return 0;

      /* +1 for null termination */
      if (len < (record_length - 32 + 1))
        {
          errno = ERANGE;
          return -1;
        }

      memset(buffer, '\0', len);
      /* No guarantee for null termination, must use memcpy */
      memcpy(buffer, &(sdr_repo_cache->cache_curr[32]), 
             (record_length - 32));

      return (record_length - 32);
    }
  else
    {
      /* This record type does not have a sensor name */
      return 0;
    }

  return 0;                     /* NOT REACHED */
}
