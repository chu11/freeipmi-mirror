/* 
   ipmi-sdr-repo-cache.h - IPMI SDR Caching functions

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

#ifndef _IPMI_SDR_REPO_CACHE_H
#define _IPMI_SDR_REPO_CACHE_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_DEFAULT_SDR_REPO_CACHE_FILENAME    "/var/lib/freeipmi/sdr-repo-cache"

typedef struct sdr_repo_cache
{
  int fd;
  size_t file_length;
  uint8_t *cache_start;
  uint8_t *cache_curr;
  uint16_t cache_curr_rec_no;
  uint32_t total_records;
} sdr_repo_cache_t;

int ipmi_sdr_repo_info_write (FILE *fp);
int ipmi_sdr_records_write (FILE *fp);
int ipmi_sdr_cache_create (char *sdr_cache_file);
int ipmi_sdr_repo_cache_load (sdr_repo_cache_t *sdr_repo_cache, char *sdr_cache_file);
int ipmi_sdr_repo_cache_unload (sdr_repo_cache_t *sdr_repo_cache);
int ipmi_sdr_repo_cache_seek (sdr_repo_cache_t *sdr_repo_cache, uint16_t rec_no);
int ipmi_sdr_repo_cache_first (sdr_repo_cache_t *sdr_repo_cache);
int ipmi_sdr_repo_cache_next (sdr_repo_cache_t *sdr_repo_cache);
int ipmi_is_sensor_reading_available (sdr_repo_cache_t *sdr_repo_cache);
int ipmi_sdr_repo_cache_sensor_classify (sdr_repo_cache_t *sdr_repo_cache);
const char *ipmi_sdr_repo_cache_get_sensor_group (sdr_repo_cache_t *sdr_repo_cache);
int ipmi_sdr_repo_cache_get_sensor_name (sdr_repo_cache_t *sdr_repo_cache,
                                         uint8_t *buffer,
                                         size_t len);

#ifdef __cplusplus
}
#endif

#endif
