/* 
   Copyright (C) 2008 FreeIPMI Core Team
   
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
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "ipmi-sensors-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-sdr-cache-common.h"

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
create_section_name (ipmi_sensors_config_state_data_t *state_data, 
                     uint8_t *sdr_record,
                     unsigned int sdr_record_len,
                     char *section_name,
                     unsigned int section_name_len)
{
  char id_string[IPMI_SDR_CACHE_MAX_ID_STRING + 1];
  uint16_t record_id;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret;

  assert(state_data);
  assert(sdr_record);
  assert(sdr_record_len);
  assert(section_name);
  assert(section_name_len);

  memset(section_name, '\0', section_name_len);
  memset(id_string, '\0', IPMI_SDR_CACHE_MAX_ID_STRING + 1);

  if (sdr_cache_get_record_id_and_type (NULL,
                                        sdr_record,
                                        sdr_record_len,
                                        &record_id,
                                        NULL) < 0)
    goto cleanup;

  if (sdr_cache_get_id_string (NULL,
                               sdr_record,
                               sdr_record_len,
                               id_string,
                               IPMI_SDR_CACHE_MAX_ID_STRING) < 0)
    goto cleanup;

  if ((ret = convert_id_string (state_data, id_string)) != CONFIG_ERR_SUCCESS)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "convert_id_string: %s\n",
                        strerror(errno));
      rv = ret;
      goto cleanup;
    }

  /* We will name sections by record_id then name, since id_strings
   * could be identical.
   */
  if (strlen(id_string) > 0)
    snprintf(section_name,
             CONFIG_MAX_SECTION_NAME_LEN,
             "%u_%s",
             record_id,
             id_string);
  else
    /* I guess its conceivable the sensor won't have a name, so we
     * make one up.
     */
    snprintf(section_name,
             CONFIG_MAX_SECTION_NAME_LEN,
             "%u_%s",
             record_id,
             "Unknown_Sensor_Name");
  
  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return rv;
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
        pstdout_perror(state_data->pstate, 
                       "strdup");
      goto cleanup;
    }

  if (!(ptr = strchr(str, '_')))
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "Invalid section_name: %s\n",
                        section_name);
      goto cleanup;
    }

  *ptr = '\0';

  record_id = strtoul(str, &ptr,0);
  if (*ptr != '\0')
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "Invalid section_name: %s\n",
                        section_name);
      goto cleanup;
    }
    
  if (ipmi_sdr_cache_search_record_id(state_data->ipmi_sdr_cache_ctx,
                                      record_id) < 0)
    {
      if (state_data->prog_data->args->config_args.common.debug)
        pstdout_fprintf(state_data->pstate,
                        stderr,
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
        pstdout_fprintf(state_data->pstate,
                        stderr,
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
