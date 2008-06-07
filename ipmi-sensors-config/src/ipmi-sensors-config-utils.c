/* 
   Copyright (C) 2007-2008 FreeIPMI Core Team
   
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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "ipmi-sensors-config-utils.h"

#include "freeipmi-portability.h"

config_err_t 
convert_id_string (ipmi_sensors_config_state_data_t *state_data, 
                   char *id_string)
{
  char *ptr;

  assert(state_data);
  assert(id_string);

  /* Convert stuff to underscore.  I will not convert +/-/. for now.
   * I think they are fine.
   */
  while ((ptr = strchr(id_string, ' ')))
    *ptr = '_';

  while ((ptr = strchr(id_string, '/')))
    *ptr = '_';

  return CONFIG_ERR_SUCCESS;
}

config_err_t
get_sdr_record (ipmi_sensors_config_state_data_t *state_data,
                const char *section_name,
                uint8_t *sdr_record,
                unsigned int *sdr_record_len)
{
  uint16_t record_id;
  char *str = NULL;
  char *ptr;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  int len;

  assert(state_data);
  assert(section_name);
  assert(sdr_record);
  assert(sdr_record_len);

  if (!(str = strdup(section_name)))
    {
      if (state_data->prog_data->args->config_args.common.debug)
        perror("strdup");
      goto cleanup;
    }

  if (!(ptr = strchr(str, '_')))
    {
      if (state_data->prog_data->args->config_args.common.debug)
        fprintf(stderr,
                "Invalid section_name: %s\n",
                section_name);
      goto cleanup;
    }

  *ptr = '\0';

  record_id = strtoul(str, &ptr,0);
  if (*ptr != '\0')
    {
      if (state_data->prog_data->args->config_args.common.debug)
        fprintf(stderr,
                "Invalid section_name: %s\n",
                section_name);
      goto cleanup;
    }
    
  if (ipmi_sdr_cache_search_record_id(state_data->ipmi_sdr_cache_ctx,
                                      record_id) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        fprintf(stderr,
                "Record_id not found: %u\n",
                record_id);
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if ((len = ipmi_sdr_cache_record_read(state_data->ipmi_sdr_cache_ctx,
                                        sdr_record,
                                        *sdr_record_len)) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        fprintf(stderr,
                "ipmi_sdr_cache_record_read: %s\n",
                ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(state_data->ipmi_sdr_cache_ctx)));
      rv = CONFIG_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  *sdr_record_len = len;

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (str)
    free(str);
  return rv;
}
