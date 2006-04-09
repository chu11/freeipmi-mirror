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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <errno.h>

#include "ipmi-sdr-repository-cache-api.h"

#include "freeipmi/fiid.h"
#include "freeipmi/ipmi-sdr-record-types.h"
#include "freeipmi/ipmi-sdr-repository-cmds.h"
#include "freeipmi/udm/ipmi-sdr-repository-cmds-udm.h"

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"
#include "ipmi-sensor-api.h"

int 
ipmi_sdr_repository_info_write (ipmi_device_t *dev, FILE *fp)
{
  fiid_obj_t obj_data_rs = NULL;
  uint8_t *buf = NULL;
  int32_t len;
  int rv = -1;

  ERR_EINVAL(dev && fp);

  FIID_OBJ_CREATE(obj_data_rs, tmpl_get_sdr_repository_info_rs);

  ERR_CLEANUP (!(ipmi_cmd_get_sdr_repository_info (dev, obj_data_rs) != 0));
 
  FIID_OBJ_LEN_BYTES_CLEANUP (len, obj_data_rs); 
      
  if (!len)
    goto cleanup;

  ERR_CLEANUP ((buf = (uint8_t *)malloc(len)));

  FIID_OBJ_GET_ALL_CLEANUP (obj_data_rs, buf, len);

  ERR_CLEANUP (!(fwrite (buf, len, 1, fp) < 0));

  return (0);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_data_rs);
  if (buf)
    free(buf);
  return (rv);
}

int 
ipmi_sdr_records_write (ipmi_device_t *dev, FILE *fp)
{
  uint16_t next_record_id = 0;
  fiid_obj_t obj_data_rs = NULL;
  uint64_t val = 0;
  uint8_t sensor_record_buf[1024];
  uint32_t sensor_record_len;
  int rv = -1;

  ERR_EINVAL(dev && fp);
  
  FIID_OBJ_CREATE(obj_data_rs, tmpl_get_sdr_rs);

  next_record_id = 0;
  while (next_record_id != 0xFFFF)
    {
      FIID_OBJ_CLEAR_CLEANUP (obj_data_rs);

      sensor_record_len = 1024;
      ERR_CLEANUP (!(get_sdr_sensor_record (dev, 
					    next_record_id, 
					    obj_data_rs, 
					    sensor_record_buf,
					    &sensor_record_len) < 0));
      
      FIID_OBJ_GET_CLEANUP (obj_data_rs, "next_record_id", &val);
      next_record_id = (uint16_t) val;
      
      ERR_CLEANUP (!(fwrite (sensor_record_buf,
			     sensor_record_len,
			     1, 
			     fp) < 0));
    }
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_data_rs);
  return (rv);
}

int 
ipmi_sdr_cache_create (ipmi_device_t *dev, char *sdr_cache_file)
{
  FILE *cache_fp;
  int rv = -1;

  ERR_EINVAL(dev && sdr_cache_file);

  ERR_CLEANUP ((cache_fp = fopen (sdr_cache_file, "w")));
  ERR_CLEANUP (!(ipmi_sdr_repository_info_write (dev, cache_fp) != 0));
  ERR_CLEANUP (!(ipmi_sdr_records_write (dev, cache_fp) != 0));
  
  rv = 0;
 cleanup:
  fclose (cache_fp);
  return (rv);
}

int 
ipmi_sdr_repository_cache_load (sdr_repository_cache_t *sdr_repository_cache, char *sdr_cache_file)
{
  fiid_obj_t obj_data_rs = NULL;
  struct stat buf;
  uint64_t val;
  uint8_t record_count_ls_byte;
  uint8_t record_count_ms_byte;
  uint16_t record_count;
  uint8_t *ptr;
  int32_t len;
  int rv = -1;

  ERR_EINVAL (sdr_repository_cache && sdr_cache_file);

  sdr_repository_cache->fd = -1;
  sdr_repository_cache->cache_start = NULL;

  ERR_CLEANUP (!((sdr_repository_cache->fd = open (sdr_cache_file, O_RDONLY)) < 0));
  
  ERR_CLEANUP (!(fstat (sdr_repository_cache->fd, &buf) < 0));
  
  sdr_repository_cache->file_length = buf.st_size;
  
  ERR_CLEANUP ((sdr_repository_cache->cache_start = (uint8_t *) mmap (NULL, 
								      sdr_repository_cache->file_length, 
								      PROT_READ, 
								      MAP_PRIVATE, 
								      sdr_repository_cache->fd, 
								      0)));
  
  FIID_OBJ_CREATE_CLEANUP(obj_data_rs, tmpl_get_sdr_repository_info_rs);

  FIID_TEMPLATE_LEN_BYTES_CLEANUP(len, tmpl_get_sdr_repository_info_rs);

  FIID_OBJ_SET_ALL_CLEANUP (obj_data_rs, sdr_repository_cache->cache_start, len);

  FIID_OBJ_GET_CLEANUP (obj_data_rs, "record_count_ls_byte", &val);
  record_count_ls_byte = val;
  FIID_OBJ_GET_CLEANUP (obj_data_rs, "record_count_ms_byte", &val);
  record_count_ms_byte = val;
  ptr = (uint8_t *)record_count;
#if WORDS_BIGENDIAN
  ptr[1] = record_count_ls_byte;
  ptr[0] = record_count_ms_byte;
#else
  ptr[0] = record_count_ls_byte;
  ptr[1] = record_count_ms_byte;
#endif


  sdr_repository_cache->total_records = (uint32_t) record_count;
  
  sdr_repository_cache->cache_curr = sdr_repository_cache->cache_start + len;
  
  sdr_repository_cache->cache_curr_rec_no = 1;
  
  rv = 0;
 cleanup:
  if (rv < 0)
    {
      if (sdr_repository_cache->fd >= 0)
	close(sdr_repository_cache->fd);
      if (sdr_repository_cache->cache_start)
	munmap (sdr_repository_cache->cache_start, sdr_repository_cache->file_length);
      sdr_repository_cache->fd = -1;
      sdr_repository_cache->cache_start = NULL;
    }
  FIID_OBJ_DESTROY_NO_RETURN(obj_data_rs);
  return (rv);
}

int 
ipmi_sdr_repository_cache_unload (sdr_repository_cache_t *sdr_repository_cache)
{
  ERR_EINVAL (sdr_repository_cache);

  if (sdr_repository_cache->cache_start)
    {
      if (munmap (sdr_repository_cache->cache_start, sdr_repository_cache->file_length) != 0)
	return (-1);
    }
  
  if (sdr_repository_cache->fd)
    close (sdr_repository_cache->fd);
  
  sdr_repository_cache->fd = -1;
  sdr_repository_cache->file_length = 0;
  sdr_repository_cache->cache_start = NULL;
  
  return 0;
}

int 
ipmi_sdr_repository_cache_seek (sdr_repository_cache_t *sdr_repository_cache, uint16_t rec_no)
{
  fiid_obj_t obj_data_rs = NULL;
  int iterations, iteration_start, i, rv = -1;
  int32_t hdr_len, info_len;
  uint64_t val;

  ERR_EINVAL (sdr_repository_cache);

  if (rec_no <= 0 || rec_no > sdr_repository_cache->total_records)
    {
      errno = ERANGE;
      return (-1);
    }

  FIID_OBJ_CREATE(obj_data_rs, tmpl_sdr_record_header);

  FIID_TEMPLATE_LEN_BYTES_CLEANUP (hdr_len, tmpl_sdr_record_header);
  
  if (rec_no >= sdr_repository_cache->cache_curr_rec_no)
    {
      iteration_start = 0;
      iterations = (rec_no - sdr_repository_cache->cache_curr_rec_no);
    }
  else
    {
      FIID_TEMPLATE_LEN_BYTES_CLEANUP(info_len, tmpl_get_sdr_repository_info_rs);
      sdr_repository_cache->cache_curr = sdr_repository_cache->cache_start + info_len;
      iteration_start = 1;
      iterations = rec_no;
    }

  for (i = iteration_start; i < iterations; i++)
    {
      FIID_OBJ_CLEAR_CLEANUP (obj_data_rs);

      FIID_OBJ_SET_ALL_CLEANUP (obj_data_rs,
				sdr_repository_cache->cache_curr,
				hdr_len);

      FIID_OBJ_GET_CLEANUP (obj_data_rs, "record_length", &val);
      
      sdr_repository_cache->cache_curr = (sdr_repository_cache->cache_curr + 
					  hdr_len +
					  val);
    }
  
  if (rec_no >= sdr_repository_cache->cache_curr_rec_no)
    sdr_repository_cache->cache_curr_rec_no += (rec_no - sdr_repository_cache->cache_curr_rec_no);
  else
    sdr_repository_cache->cache_curr_rec_no = i;
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_data_rs);
  return (rv);
}

int 
ipmi_sdr_repository_cache_first (sdr_repository_cache_t *sdr_repository_cache)
{
  ERR_EINVAL (sdr_repository_cache);

  return (ipmi_sdr_repository_cache_seek (sdr_repository_cache, 1));
}

int 
ipmi_sdr_repository_cache_next (sdr_repository_cache_t *sdr_repository_cache)
{
  ERR_EINVAL (sdr_repository_cache);

  return (ipmi_sdr_repository_cache_seek (sdr_repository_cache, 
                                          sdr_repository_cache->cache_curr_rec_no + 1));
}
