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
ipmi_sdr_repo_info_write (ipmi_device_t *dev, FILE *fp)
{
  fiid_obj_t obj_data_rs = NULL;
  uint8_t *buf = NULL;
  uint64_t val;
  int32_t len;
  int rv = -1;

  if (!dev || !fp)
    {
      errno = EINVAL;
      return -1;
    }

  if (!(obj_data_rs = fiid_obj_create (tmpl_get_sdr_repo_info_rs)))
    goto cleanup;

  if (ipmi_cmd_get_sdr_repo_info2 (dev, obj_data_rs) != 0)
    return (-1);
  
  if (ipmi_comp_test (obj_data_rs) != 1)
    goto cleanup;
 
  if ((len = fiid_obj_len_bytes (obj_data_rs)) < 0)
    goto cleanup;

  if (!len)
    goto cleanup;

  if (!(buf = (uint8_t *)malloc(len)))
    goto cleanup;

  if (fiid_obj_get_all(obj_data_rs, buf, len) < 0)
    goto cleanup;

  if (fwrite (buf, len, 1, fp) < 0)
    goto cleanup;

  return (0);

  rv = 0;
 cleanup:
  if (obj_data_rs)
    fiid_obj_destroy(obj_data_rs);
  if (buf)
    free(buf);
  return (rv);
}

int 
ipmi_sdr_records_write (ipmi_device_t *dev, FILE *fp)
{
  uint16_t record_id = 0;
  uint8_t record_length = 0;
  fiid_obj_t obj_cmd_rs = NULL;
  fiid_obj_t obj_sdr_record = NULL;
  uint64_t val = 0;
  int rv = -1;
  int32_t len, get_len;
  uint8_t *buf = NULL;

  if (!dev || !fp)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_get_sdr_rs)))
    goto cleanup;

  record_id = 0;
  while (record_id != 0xFFFF)
    {
      if (fiid_obj_clear (obj_cmd_rs) < 0)
	goto cleanup;

      if (obj_sdr_record)
	{
	  fiid_obj_destroy (obj_sdr_record);
	  obj_sdr_record = NULL;
	}
      
      if (ipmi_cmd_get_sdr2 (dev, 
			     record_id, 
			     obj_cmd_rs, 
			     &obj_sdr_record) != 0)
	goto cleanup;
      
      if (fiid_obj_get (obj_cmd_rs, 
			(uint8_t *)"next_record_id", 
			&val) < 0)
	goto cleanup;

      record_id = (uint16_t) val;
      
      if (fiid_obj_get (obj_sdr_record, 
			(uint8_t *)"record_length", 
			&val) < 0)
	goto cleanup;

      record_length = (uint8_t) val;
      
      if ((len = fiid_template_len_bytes(tmpl_sdr_sensor_record_header)) < 0)
	goto cleanup;

      record_length += len;
      
      if (!(buf = (uint8_t *)malloc(record_length)))
	goto cleanup;

      if ((get_len = fiid_obj_get_all(obj_sdr_record,
				      buf,
				      record_length)) < 0)
	goto cleanup;

      if (fwrite (buf,
		  record_length, 
		  1, 
		  fp) < 0)
	goto cleanup;

      free(buf);
      buf = NULL;
    }
 
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  if (obj_sdr_record)
    fiid_obj_destroy(obj_sdr_record);
  if (buf)
    free(buf);
  return (rv);
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
  fiid_obj_t obj_data_rs = NULL;
  struct stat buf;
  uint64_t val;
  int32_t len;
  int rv = -1;

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
    goto cleanup;
  
  if (!(obj_data_rs = fiid_obj_create(obj_data_rs)))
    goto cleanup;

  if ((len = fiid_template_len_bytes (tmpl_get_sdr_repo_info_rs)) < 0)
    goto cleanup;

  if (fiid_obj_set_all(obj_data_rs, sdr_repo_cache->cache_start, len) < 0)
    goto cleanup;

  if (fiid_obj_get (obj_data_rs,
		    (uint8_t *)"record_count", 
		    &val) < 0)
    goto cleanup;

  sdr_repo_cache->total_records = (uint32_t) val;
  
  sdr_repo_cache->cache_curr = sdr_repo_cache->cache_start + len;
  
  sdr_repo_cache->cache_curr_rec_no = 1;
  
  rv = 0;
 cleanup:
  if (obj_data_rs)
    fiid_obj_destroy(obj_data_rs);
  return (rv);
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
  fiid_obj_t obj_data_rs = NULL;
  int i, rv = -1;
  int32_t hdr_len;
  uint64_t val;

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

  if ((hdr_len = fiid_template_len_bytes (tmpl_sdr_sensor_record_header)) < 0)
    goto cleanup;
  
  if (!(obj_data_rs = fiid_obj_create(tmpl_sdr_sensor_record_header)))
    goto cleanup;

  if (rec_no >= sdr_repo_cache->cache_curr_rec_no)
    {

      /* skip (rec_no - sdr_repo_cache->cache_curr_rec_no) records */
      for (i = 0; i < (rec_no - sdr_repo_cache->cache_curr_rec_no); i++)
        {
	  if (fiid_obj_clear(obj_data_rs) < 0)
	    goto cleanup;

	  if (fiid_obj_set_all(obj_data_rs,
			       sdr_repo_cache->cache_curr,
			       hdr_len) < 0)
	    goto cleanup;

	  if (fiid_obj_get(obj_data_rs,
			   (uint8_t *)"record_length",
			   &val) < 0)
	    goto cleanup;
	  
          sdr_repo_cache->cache_curr = (sdr_repo_cache->cache_curr + 
					hdr_len +
					val);
        }
      sdr_repo_cache->cache_curr_rec_no += (rec_no - sdr_repo_cache->cache_curr_rec_no);
    }
  else
    {
      int32_t len;

      if ((len = fiid_template_len_bytes (tmpl_get_sdr_repo_info_rs)) < 0)
	goto cleanup;

      sdr_repo_cache->cache_curr = sdr_repo_cache->cache_start + len;

      for (i = 1; i < rec_no; i++)
        {
	  if (fiid_obj_clear(obj_data_rs) < 0)
	    goto cleanup;

	  if (fiid_obj_set_all(obj_data_rs,
			       sdr_repo_cache->cache_curr,
			       hdr_len) < 0)
	    goto cleanup;

	  if (fiid_obj_get(obj_data_rs,
			   (uint8_t *)"record_length",
			   &val) < 0)
	    goto cleanup;

          sdr_repo_cache->cache_curr = (sdr_repo_cache->cache_curr + 
					hdr_len +
					val);
        }
      sdr_repo_cache->cache_curr_rec_no = i;
    }
  
  rv = 0;
 cleanup:
  if (obj_data_rs)
    fiid_obj_destroy(obj_data_rs);
  return (rv);
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
  fiid_obj_t obj_data_rs = NULL;
  uint64_t record_type, val;
  int32_t len;
  int rv = -1;
  
  if (sdr_repo_cache == NULL)
    {
      errno = EINVAL;
      return -1;
    }
  
  if (!(obj_data_rs = fiid_obj_create(tmpl_sdr_sensor_record_header)))
    goto cleanup;

  if ((len = fiid_template_len_bytes (tmpl_sdr_sensor_record_header)) < 0)
    goto cleanup;

  if (fiid_obj_set_all(obj_data_rs,
		       sdr_repo_cache->cache_curr,
		       len) < 0)
    goto cleanup;

  if (fiid_obj_get (obj_data_rs,
		    (uint8_t *)"record_type", 
		    &record_type) < 0)
    goto cleanup;

  fiid_obj_destroy(obj_data_rs);
  obj_data_rs = NULL;

  switch (record_type)
    {
    case IPMI_SDR_FORMAT_FULL_RECORD:
    case IPMI_SDR_FORMAT_COMPACT_RECORD:

      if (record_type == IPMI_SDR_FORMAT_FULL_RECORD)
	{
	  if (!(obj_data_rs = fiid_obj_create(tmpl_sdr_full_sensor_record)))
	    goto cleanup;
	  
	  if ((len = fiid_template_len_bytes(tmpl_sdr_full_sensor_record)) < 0)
	    goto cleanup;
	}
      else
	{
	  if (!(obj_data_rs = fiid_obj_create(tmpl_sdr_compact_sensor_record)))
	    goto cleanup;
	  
	  if ((len = fiid_template_len_bytes(tmpl_sdr_compact_sensor_record)) < 0)
	    goto cleanup;
	}
      
      if (fiid_obj_set_all(obj_data_rs,
			   sdr_repo_cache->cache_curr,
			   len) < 0)
	goto cleanup;
      
      if (fiid_obj_get (obj_data_rs
			(uint8_t *)"slave_system_software_id", 
			&val) < 0)
	goto cleanup;
	
      if (ipmi_get_system_software_type (val) == IPMI_SYS_SOFT_ID_RESERVED)
	rv = 0;
      else
	rv = 1;
      break;

    case IPMI_SDR_FORMAT_EVENT_ONLY_RECORD:
    case IPMI_SDR_FORMAT_ENTITY_ASSO_RECORD:
    case IPMI_SDR_FORMAT_DEV_ENTITY_ASSO_RECORD:
    case IPMI_SDR_FORMAT_GEN_DEV_LOCATOR_RECORD:
    case IPMI_SDR_FORMAT_FRU_DEV_LOCATOR_RECORD:
    case IPMI_SDR_FORMAT_MGMT_CNTRLR_DEV_LOCATOR_RECORD:
    case IPMI_SDR_FORMAT_MGMT_CNTRLR_CONFIRMATION_RECORD:
    case IPMI_SDR_FORMAT_BMC_MSG_CHANNEL_INFO_RECORD:
    case IPMI_SDR_FORMAT_OEM_RECORD:
    default:
      rv = 0;
    }

 cleanup:
  if (obj_data_rs)
    fiid_obj_destroy(obj_data_rs);
  return (rv);
}

int 
ipmi_sdr_repo_cache_sensor_classify (sdr_repo_cache_t *sdr_repo_cache)
{
  fiid_obj_t obj_data_rs = NULL;
  uint64_t record_type, val;
  int32_t len;
  int rv = -1;
  
  if (sdr_repo_cache == NULL)
    {
      errno = EINVAL;
      return -1;
    }
  
  if (!(obj_data_rs = fiid_obj_create(tmpl_sdr_sensor_record_header)))
    goto cleanup;

  if ((len = fiid_template_len_bytes (tmpl_sdr_sensor_record_header)) < 0)
    goto cleanup;

  if (fiid_obj_set_all(obj_data_rs,
		       sdr_repo_cache->cache_curr,
		       len) < 0)
    goto cleanup;

  if (fiid_obj_get (obj_data_rs,
		    (uint8_t *)"record_type", 
		    &record_type) < 0)
    goto cleanup;

  fiid_obj_destroy(obj_data_rs);
  obj_data_rs = NULL;

  if (record_type == IPMI_SDR_FORMAT_FULL_RECORD
      || record_type == IPMI_SDR_FORMAT_COMPACT_RECORD)
    {
      if (record_type == IPMI_SDR_FORMAT_FULL_RECORD)
	{
	  if (!(obj_data_rs = fiid_obj_create(tmpl_sdr_full_sensor_record)))
	    goto cleanup;
	  
	  if ((len = fiid_template_len_bytes(tmpl_sdr_full_sensor_record)) < 0)
	    goto cleanup;
	}
      else
	{
	  if (!(obj_data_rs = fiid_obj_create(tmpl_sdr_compact_sensor_record)))
	    goto cleanup;
	  
	  if ((len = fiid_template_len_bytes(tmpl_sdr_compact_sensor_record)) < 0)
	    goto cleanup;
	}
      
      if (fiid_obj_set_all(obj_data_rs,
			   sdr_repo_cache->cache_curr,
			   len) < 0)
	goto cleanup;
      
      if (fiid_obj_get (obj_data_rs
			(uint8_t *)"event_reading_type", 
			&val) < 0)
	goto cleanup;

      rv = ipmi_sensor_classify (val);
    }
  else
    rv = IPMI_SENSOR_CLASS_NOT_AVAILABLE;
    
 cleanup:
  if (obj_data_rs)
    fiid_obj_destroy(obj_data_rs);
  return (rv);
}

const char *
ipmi_sdr_repo_cache_get_sensor_group (sdr_repo_cache_t *sdr_repo_cache)
{
  fiid_obj_t obj_data_rs = NULL;
  uint64_t record_type, val;
  int32_t len;
  char *rv = NULL;

  if (sdr_repo_cache == NULL)
    {
      errno = EINVAL;
      return -1;
    }
  
  if (!(obj_data_rs = fiid_obj_create(tmpl_sdr_sensor_record_header)))
    goto cleanup;

  if ((len = fiid_template_len_bytes (tmpl_sdr_sensor_record_header)) < 0)
    goto cleanup;

  if (fiid_obj_set_all(obj_data_rs,
		       sdr_repo_cache->cache_curr,
		       len) < 0)
    goto cleanup;

  if (fiid_obj_get (obj_data_rs,
		    (uint8_t *)"record_type", 
		    &record_type) < 0)
    goto cleanup;

  fiid_obj_destroy(obj_data_rs);
  obj_data_rs = NULL;

  if (record_type == IPMI_SDR_FORMAT_FULL_RECORD
      || record_type == IPMI_SDR_FORMAT_COMPACT_RECORD)
    {
      if (record_type == IPMI_SDR_FORMAT_FULL_RECORD)
	{
	  if (!(obj_data_rs = fiid_obj_create(tmpl_sdr_full_sensor_record)))
	    goto cleanup;
	  
	  if ((len = fiid_template_len_bytes(tmpl_sdr_full_sensor_record)) < 0)
	    goto cleanup;
	}
      else
	{
	  if (!(obj_data_rs = fiid_obj_create(tmpl_sdr_compact_sensor_record)))
	    goto cleanup;
	  
	  if ((len = fiid_template_len_bytes(tmpl_sdr_compact_sensor_record)) < 0)
	    goto cleanup;
	}
      
      if (fiid_obj_set_all(obj_data_rs,
			   sdr_repo_cache->cache_curr,
			   len) < 0)
	goto cleanup;
      
      if (fiid_obj_get (obj_data_rs
			(uint8_t *)"sensor_type", 
			&val) < 0)
	goto cleanup;

      sensor_type = val;
      rv = ipmi_get_sensor_group (sensor_type);
    }
 
 cleanup:
  if (obj_data_rs)
    fiid_obj_destroy(obj_data_rs);
  return (rv);
}

int
ipmi_sdr_repo_cache_get_sensor_name (sdr_repo_cache_t *sdr_repo_cache,
                                     uint8_t *buffer,
                                     size_t len)
{
  fiid_obj_t obj_data_rs = NULL;
  uint64_t record_type, val;
  int32_t hdr_len, string_len;
  int rv = -1;

  if (sdr_repo_cache == NULL
      || buffer == NULL)
    {
      errno = EINVAL;
      return -1;
    }
  
  if (!(obj_data_rs = fiid_obj_create(tmpl_sdr_sensor_record_header)))
    goto cleanup;

  if ((len = fiid_template_len_bytes (tmpl_sdr_sensor_record_header)) < 0)
    goto cleanup;

  if (fiid_obj_set_all(obj_data_rs,
		       sdr_repo_cache->cache_curr,
		       len) < 0)
    goto cleanup;

  if (fiid_obj_get (obj_data_rs,
		    (uint8_t *)"record_type", 
		    &record_type) < 0)
    goto cleanup;

  fiid_obj_destroy(obj_data_rs);
  obj_data_rs = NULL;

  if (record_type == IPMI_SDR_FORMAT_FULL_RECORD
      || record_type == IPMI_SDR_FORMAT_COMPACT_RECORD)
    {
      if (record_type == IPMI_SDR_FORMAT_FULL_RECORD)
	{
	  if (!(obj_data_rs = fiid_obj_create(tmpl_sdr_full_sensor_record)))
	    goto cleanup;
	  
	  if ((len = fiid_template_len_bytes(tmpl_sdr_full_sensor_record)) < 0)
	    goto cleanup;
	}
      else
	{
	  if (!(obj_data_rs = fiid_obj_create(tmpl_sdr_compact_sensor_record)))
	    goto cleanup;
	  
	  if ((len = fiid_template_len_bytes(tmpl_sdr_compact_sensor_record)) < 0)
	    goto cleanup;
	}
      
      if (fiid_obj_set_all(obj_data_rs,
			   sdr_repo_cache->cache_curr,
			   len) < 0)
	goto cleanup;
      
      if ((str_len = fiid_obj_field_len_bytes(obj_data_rs,
					      (uint8_t *)"sensor_id_string")) < 0)
	goto cleanup;

      if (str_len)
	{
	  if (len < str_len)
	    {
	      errno = ERANGE;
	      goto cleanup;
	    }
	  memset(buffer, '\0', len);
	  if (fiid_obj_get_data(obj_data_rs,
				(uint8_t *)"sensor_id_string",
				buffer,
				len) < 0)
	    goto cleanup;
	  rv = str_len;
	}
      else
	rv = 0;
    }
  else
    {
      /* This record type does not have a sensor name */
      rv = 0;
    }

 cleanup:
  if (obj_data_rs)
    fiid_obj_destroy(obj_data_rs);
  return (rv);
}
