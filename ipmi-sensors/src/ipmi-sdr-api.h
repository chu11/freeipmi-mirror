/*
ipmi-sdr-api.h: SDR cache creation and management apis.
Copyright (C) 2006 FreeIPMI Core Team

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#ifndef _IPMI_SDR_API_H
#define _IPMI_SDR_API_H

struct sdr_repository_info
{
  int sdr_version_major;
  int sdr_version_minor;
  int record_count;
  int free_space;
  int most_recent_addition_timestamp;
  int most_recent_erase_timestamp;
  int get_sdr_repository_allocation_info_command_supported;
  int reserve_sdr_repository_command_supported;
  int partial_add_sdr_command_supported;
  int delete_sdr_command_supported;
  int modal_non_modal_sdr_repository_update_operation_supported;
  int overflow_flag;
};
typedef struct sdr_repository_info sdr_repository_info_t;

int get_sdr_repository_info (ipmi_device_t dev, sdr_repository_info_t *sdr_info);

char *get_sdr_cache_filename (char *host);
int setup_sdr_cache_directory ();
int flush_sdr_cache_file (char *host);
int create_sdr_cache (ipmi_device_t dev, FILE *fp, int verbose);
int load_sdr_cache (FILE *fp, sdr_repository_info_t *sdr_info, 
		    sdr_record_t **sdr_record_list, int *count);


#endif
