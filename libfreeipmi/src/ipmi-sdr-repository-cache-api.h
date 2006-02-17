/* 
   ipmi-sdr-repository-cache-api.h - IPMI SDR Caching functions

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

#ifndef _IPMI_SDR_REPOSITORY_CACHE_API_H
#define _IPMI_SDR_REPOSITORY_CACHE_API_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_DEFAULT_SDR_REPOSITORY_CACHE_FILENAME    "/var/lib/freeipmi/sdr-repository-cache"

typedef struct sdr_repository_cache
{
  int fd;
  size_t file_length;
  uint8_t *cache_start;
  uint8_t *cache_curr;
  uint16_t cache_curr_rec_no;
  uint32_t total_records;
} sdr_repository_cache_t;

int ipmi_sdr_repository_info_write (ipmi_device_t *dev, FILE *fp);
int ipmi_sdr_records_write (ipmi_device_t *dev, FILE *fp);
int ipmi_sdr_cache_create (ipmi_device_t *dev, char *sdr_cache_file);
int ipmi_sdr_repository_cache_load (sdr_repository_cache_t *sdr_repository_cache, char *sdr_cache_file);
int ipmi_sdr_repository_cache_unload (sdr_repository_cache_t *sdr_repository_cache);
int ipmi_sdr_repository_cache_seek (sdr_repository_cache_t *sdr_repository_cache, uint16_t rec_no);
int ipmi_sdr_repository_cache_first (sdr_repository_cache_t *sdr_repository_cache);
int ipmi_sdr_repository_cache_next (sdr_repository_cache_t *sdr_repository_cache);

#ifdef __cplusplus
}
#endif

#endif
