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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

#include "freeipmi.h"

int 
ipmi_sdr_repo_info_write (FILE *fp)
{
  u_int8_t *data_rs = NULL;
  u_int64_t val;
  
  if (fp == NULL)
    {
      errno = EINVAL;
      return -1;
    }

  data_rs = alloca (fiid_obj_len_bytes (tmpl_get_sdr_repo_info_rs));
  
  if (ipmi_kcs_get_repo_info (data_rs) != 0)
    return (-1);
  
  fiid_obj_get (data_rs, 
		tmpl_get_sdr_repo_info_rs, 
		"comp_code", 
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
ipmi_sdr_records_write (FILE *fp)
{
  u_int16_t record_id;
  u_int8_t data_rs[4] = {0, 0, 0, 0};
  u_int8_t record_header[5];
  u_int8_t *sensor_record = NULL;
  u_int64_t val;
  u_int8_t record_length;
  u_int8_t comp_code;
  
  if (fp == NULL)
    {
      errno = EINVAL;
      return -1;
    }

  record_id = 0;
  while (record_id != 0xFFFF)
    {
      // printf ("writing %d\n", record_id);
      if (ipmi_kcs_get_sensor_record_header (record_id, 
					     data_rs, 
                                             record_header,
                                             5) != 0)
	return (-1);
      
      fiid_obj_get (data_rs, 
		    tmpl_get_sdr_rs, 
		    "comp_code", 
		    &val);
      if (val != 0)
	return (-1);
      
      fiid_obj_get (record_header, 
		    tmpl_sdr_sensor_record_header, 
		    "record_length", 
		    &val);
      record_length = (u_int8_t) val;
      
      record_length += fiid_obj_len_bytes (tmpl_sdr_sensor_record_header);
      
      sensor_record = alloca (record_length);
      if (ipmi_kcs_get_sdr (record_id, 
			    record_length, 
			    sensor_record, 
			    &comp_code) != 0)
	return (-1);
      
      if (comp_code != IPMI_KCS_STATUS_SUCCESS)
	return (-1);
      
/*       { */
/* 	int i; */
	
/* 	if (record_id >= 130 && record_id <= 134) */
/* 	  { */
/* 	    for (i = 0; i < record_length; i++) */
/* 	      printf ("%02X ", sensor_record[i]); */
/* 	    printf ("\n\n"); */
/* 	  } */
/*       } */
      
      if (fwrite (sensor_record, 
		  record_length, 
		  1, 
		  fp) < 0)
	return (-1);
      
      fiid_obj_get (data_rs, 
		    tmpl_get_sdr_rs, 
		    "next_record_id", 
		    &val);
      record_id = val;
    }
  
  return (0);
}

int 
ipmi_sdr_cache_create (char *sdr_cache_file)
{
  FILE *cache_fp;
  
  if (sdr_cache_file == NULL)
    {
      errno = EINVAL;
      return -1;
    }

  if ((cache_fp = fopen (sdr_cache_file, "w")) == NULL)
    return (-1);
  
  if (ipmi_sdr_repo_info_write (cache_fp) != 0)
    {
      fclose (cache_fp);
      return (-1);
    }
  
  if (ipmi_sdr_records_write (cache_fp) != 0)
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
  u_int64_t val;
  
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
  
  sdr_repo_cache->cache_start = (u_int8_t *) mmap (NULL, 
						   sdr_repo_cache->file_length, 
						   PROT_READ, 
						   MAP_PRIVATE, 
						   sdr_repo_cache->fd, 
						   0);
  
  if (sdr_repo_cache->cache_start <= 0)
    return (-1);
  
  fiid_obj_get (sdr_repo_cache->cache_start, 
		tmpl_get_sdr_repo_info_rs, 
		"record_count", 
		&val);
  sdr_repo_cache->total_records = (u_int32_t) val;
  
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
ipmi_sdr_repo_cache_seek (sdr_repo_cache_t *sdr_repo_cache, u_int16_t rec_no)
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
  u_int64_t val;
  
  if (sdr_repo_cache == NULL)
    {
      errno = EINVAL;
      return -1;
    }
  
  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_sensor_record_header, 
		"record_type", 
		&val);
  
  switch (val)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
      fiid_obj_get (sdr_repo_cache->cache_curr, 
		    tmpl_sdr_full_sensor_record, 
		    "slave_system_software_id", 
		    &val);
      if (ipmi_get_system_software_type (val) == IPMI_SYS_SOFT_ID_RESERVED)
	return (0);
      return (1);
      
    case IPMI_SDR_FORMAT_COMPACT_RECORD:
      fiid_obj_get (sdr_repo_cache->cache_curr, 
		    tmpl_sdr_compact_sensor_record, 
		    "slave_system_software_id", 
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
  u_int64_t record_type, val;
  
  if (sdr_repo_cache == NULL)
    {
      errno = EINVAL;
      return -1;
    }

  fiid_obj_get(sdr_repo_cache->cache_curr, 
               tmpl_sdr_sensor_record_header,
               "record_type", 
               &record_type);

  if (record_type == IPMI_SDR_FORMAT_FULL_RECORD)
    fiid_obj_get (sdr_repo_cache->cache_curr, 
                  tmpl_sdr_full_sensor_record, 
                  "event_reading_type", 
                  &val);
  else if (record_type == IPMI_SDR_FORMAT_COMPACT_RECORD)
    fiid_obj_get (sdr_repo_cache->cache_curr, 
                  tmpl_sdr_compact_sensor_record, 
                  "event_reading_type", 
                  &val);
  else
    return IPMI_SENSOR_CLASS_NOT_AVAILABLE;
    
  return (ipmi_sensor_classify (val));
}

const char *
ipmi_sdr_repo_cache_get_sensor_group (sdr_repo_cache_t *sdr_repo_cache)
{
  u_int8_t sensor_type;
  u_int64_t val;
  
  if (sdr_repo_cache == NULL)
    {
      errno = EINVAL;
      return NULL;
    }

  fiid_obj_get (sdr_repo_cache->cache_curr, 
		tmpl_sdr_sensor_record_header, 
		"record_type", 
		&val);
  
  if (val == IPMI_SDR_FORMAT_FULL_RECORD)
    {
      fiid_obj_get (sdr_repo_cache->cache_curr, 
		    tmpl_sdr_full_sensor_record, 
		    "sensor_type", 
		    &val);
      sensor_type = val;
      return ipmi_get_sensor_group (sensor_type);
    }
  
  if (val == IPMI_SDR_FORMAT_COMPACT_RECORD)
    {
      fiid_obj_get (sdr_repo_cache->cache_curr, 
		    tmpl_sdr_compact_sensor_record, 
		    "sensor_type", 
		    &val);
      sensor_type = val;
      return ipmi_get_sensor_group (sensor_type);
    }
  
  return NULL;
}

int
ipmi_sdr_repo_cache_get_sensor_name (sdr_repo_cache_t *sdr_repo_cache,
                                     u_int8_t *buffer,
                                     size_t len)
{
  u_int64_t val;
  u_int32_t record_length;
  u_int8_t record_type;

  if (sdr_repo_cache == NULL
      || buffer == NULL)
    {
      errno = EINVAL;
      return -1;
    }

  ERR (!(fiid_obj_get(sdr_repo_cache->cache_curr, 
                      tmpl_sdr_sensor_record_header, 
                      "record_type", 
                      &val) < 0));
  record_type = val;

  if (record_type == IPMI_SDR_FORMAT_FULL_RECORD)
    {
      ERR (fiid_obj_get(sdr_repo_cache->cache_curr,
                        tmpl_sdr_full_sensor_record,
                        "record_length",
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
                        "record_length",
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
