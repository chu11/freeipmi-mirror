/* 
   ipmi-sdr-repository-cache-api.c - IPMI SDR Caching functions

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
ipmi_sdr_repository_info_write (ipmi_device_t *dev, FILE *fp)
{
  fiid_obj_t obj_data_rs = NULL;
  uint8_t *buf = NULL;
  int32_t len;
  int rv = -1;

  if (!dev || !fp)
    {
      errno = EINVAL;
      return -1;
    }

  if (!(obj_data_rs = fiid_obj_create (tmpl_get_sdr_repository_info_rs)))
    goto cleanup;

  if (ipmi_cmd_get_sdr_repository_info2 (dev, obj_data_rs) != 0)
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
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val = 0;
  uint8_t sensor_record_buf[1024];
  uint32_t sensor_record_len;
  int rv = -1;

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

      sensor_record_len = 1024;
      if (ipmi_cmd_get_sdr2 (dev, 
			     record_id, 
			     obj_cmd_rs, 
			     sensor_record_buf,
			     &sensor_record_len) < 0)
	goto cleanup;
      
      if (fiid_obj_get (obj_cmd_rs, 
			(uint8_t *)"next_record_id", 
			&val) < 0)
	goto cleanup;

      record_id = (uint16_t) val;
      
      if (fwrite (sensor_record_buf,
		  sensor_record_len,
		  1, 
		  fp) < 0)
	goto cleanup;
    }
  
  rv = 0;
 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
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
  
  if (ipmi_sdr_repository_info_write (dev, cache_fp) != 0)
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
ipmi_sdr_repository_cache_load (sdr_repository_cache_t *sdr_repository_cache, char *sdr_cache_file)
{
  fiid_obj_t obj_data_rs = NULL;
  struct stat buf;
  uint64_t val;
  int32_t len;
  int rv = -1;

  if (!(sdr_repository_cache && sdr_cache_file))
    {
      errno = EINVAL;
      return -1;
    }

  sdr_repository_cache->fd = open (sdr_cache_file, O_RDONLY);
  
  if (sdr_repository_cache->fd <= 0)
    return (-1);
  
  if (fstat (sdr_repository_cache->fd, &buf) != 0)
    return (-1);
  
  sdr_repository_cache->file_length = buf.st_size;
  
  sdr_repository_cache->cache_start = (uint8_t *) mmap (NULL, 
                                                        sdr_repository_cache->file_length, 
                                                        PROT_READ, 
                                                        MAP_PRIVATE, 
                                                        sdr_repository_cache->fd, 
                                                        0);
  
  if (sdr_repository_cache->cache_start <= 0)
    goto cleanup;
  
  if (!(obj_data_rs = fiid_obj_create(tmpl_get_sdr_repository_info_rs)))
    goto cleanup;

  if ((len = fiid_template_len_bytes (tmpl_get_sdr_repository_info_rs)) < 0)
    goto cleanup;

  if (fiid_obj_set_all(obj_data_rs, sdr_repository_cache->cache_start, len) < 0)
    goto cleanup;

  if (fiid_obj_get (obj_data_rs,
		    (uint8_t *)"record_count", 
		    &val) < 0)
    goto cleanup;

  sdr_repository_cache->total_records = (uint32_t) val;
  
  sdr_repository_cache->cache_curr = sdr_repository_cache->cache_start + len;
  
  sdr_repository_cache->cache_curr_rec_no = 1;
  
  rv = 0;
 cleanup:
  if (obj_data_rs)
    fiid_obj_destroy(obj_data_rs);
  return (rv);
}

int 
ipmi_sdr_repository_cache_unload (sdr_repository_cache_t *sdr_repository_cache)
{
  if (sdr_repository_cache == NULL)
    {
      errno = EINVAL;
      return -1;
    }

  if (sdr_repository_cache->cache_start)
    {
      if (munmap (sdr_repository_cache->cache_start, sdr_repository_cache->file_length) != 0)
	return (-1);
    }
  
  if (sdr_repository_cache->fd)
    {
      sdr_repository_cache->file_length = 0;
      close (sdr_repository_cache->fd);
    }
  
  sdr_repository_cache->fd = 0;
  sdr_repository_cache->file_length = 0;
  sdr_repository_cache->cache_start = NULL;
  
  return 0;
}

int 
ipmi_sdr_repository_cache_seek (sdr_repository_cache_t *sdr_repository_cache, uint16_t rec_no)
{
  fiid_obj_t obj_data_rs = NULL;
  int i, rv = -1;
  int32_t hdr_len;
  uint64_t val;

  if (sdr_repository_cache == NULL)
    {
      errno = EINVAL;
      return -1;
    }

  if (rec_no <= 0 || rec_no > sdr_repository_cache->total_records)
    {
      errno = ERANGE;
      return (-1);
    }

  if ((hdr_len = fiid_template_len_bytes (tmpl_sdr_sensor_record_header)) < 0)
    goto cleanup;
  
  if (!(obj_data_rs = fiid_obj_create(tmpl_sdr_sensor_record_header)))
    goto cleanup;

  if (rec_no >= sdr_repository_cache->cache_curr_rec_no)
    {

      /* skip (rec_no - sdr_repository_cache->cache_curr_rec_no) records */
      for (i = 0; i < (rec_no - sdr_repository_cache->cache_curr_rec_no); i++)
        {
	  if (fiid_obj_clear(obj_data_rs) < 0)
	    goto cleanup;

	  if (fiid_obj_set_all(obj_data_rs,
			       sdr_repository_cache->cache_curr,
			       hdr_len) < 0)
	    goto cleanup;

	  if (fiid_obj_get(obj_data_rs,
			   (uint8_t *)"record_length",
			   &val) < 0)
	    goto cleanup;
	  
          sdr_repository_cache->cache_curr = (sdr_repository_cache->cache_curr + 
                                              hdr_len +
                                              val);
        }
      sdr_repository_cache->cache_curr_rec_no += (rec_no - sdr_repository_cache->cache_curr_rec_no);
    }
  else
    {
      int32_t len;

      if ((len = fiid_template_len_bytes (tmpl_get_sdr_repository_info_rs)) < 0)
	goto cleanup;

      sdr_repository_cache->cache_curr = sdr_repository_cache->cache_start + len;

      for (i = 1; i < rec_no; i++)
        {
	  if (fiid_obj_clear(obj_data_rs) < 0)
	    goto cleanup;

	  if (fiid_obj_set_all(obj_data_rs,
			       sdr_repository_cache->cache_curr,
			       hdr_len) < 0)
	    goto cleanup;

	  if (fiid_obj_get(obj_data_rs,
			   (uint8_t *)"record_length",
			   &val) < 0)
	    goto cleanup;

          sdr_repository_cache->cache_curr = (sdr_repository_cache->cache_curr + 
                                              hdr_len +
                                              val);
        }
      sdr_repository_cache->cache_curr_rec_no = i;
    }
  
  rv = 0;
 cleanup:
  if (obj_data_rs)
    fiid_obj_destroy(obj_data_rs);
  return (rv);
}

int 
ipmi_sdr_repository_cache_first (sdr_repository_cache_t *sdr_repository_cache)
{
  if (sdr_repository_cache == NULL)
    {
      errno = EINVAL;
      return -1;
    }

  return (ipmi_sdr_repository_cache_seek (sdr_repository_cache, 1));
}

int 
ipmi_sdr_repository_cache_next (sdr_repository_cache_t *sdr_repository_cache)
{
  if (sdr_repository_cache == NULL)
    {
      errno = EINVAL;
      return -1;
    }

  return (ipmi_sdr_repository_cache_seek (sdr_repository_cache, 
                                          sdr_repository_cache->cache_curr_rec_no + 1));
}
