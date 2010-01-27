/*
  Copyright (C) 2003-2010 FreeIPMI Core Team

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
#include <errno.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-sensors-util.h"
#include "ipmi-sensors-output-common.h"

#include "freeipmi-portability.h"

#define IPMI_SENSORS_BUFLEN   1024
#define IPMI_SENSORS_MAX_LIST   32

#define UNRECOGNIZED_STATE "Unrecognized State"

char *
get_sensor_state_str (unsigned sensor_state)
{
  if (sensor_state == IPMI_INTERPRET_SENSOR_STATE_NOMINAL)
    return "Nominal";
  else if (sensor_state == IPMI_INTERPRET_SENSOR_STATE_WARNING)
    return "Warning";
  else if (sensor_state == IPMI_INTERPRET_SENSOR_STATE_CRITICAL)
    return "Critical";

  return (IPMI_SENSORS_NA_STRING);
}

int
get_msg_message_list (struct ipmi_sensors_state_data *state_data,
                      char ***event_message_list,
                      unsigned int *event_message_list_len,
                      char *msg)
{
  char **tmp_event_message_list = NULL;
  int count = 0;

  assert (state_data);
  assert (event_message_list);
  assert (event_message_list_len);
  assert (msg);

  count = 1;

  if (!(tmp_event_message_list = (char **) malloc (sizeof (char *) * (count + 1))))
    {
      pstdout_perror (state_data->pstate, "malloc");
      goto cleanup;
    }

  if (!(tmp_event_message_list[0] = strdup (msg)))
    {
      pstdout_perror (state_data->pstate, "strdup");
      goto cleanup;
    }

  tmp_event_message_list[count] = NULL;
  *event_message_list = tmp_event_message_list;
  *event_message_list_len = 1;

  return (0);

 cleanup:
  if (tmp_event_message_list)
    free (tmp_event_message_list);
  return (-1);
}

int
get_threshold_message_list (struct ipmi_sensors_state_data *state_data,
                            char ***event_message_list,
                            unsigned int *event_message_list_len,
                            uint16_t sensor_event_bitmask,
                            char *no_event_msg)
{
  char **tmp_event_message_list = NULL;
  char *tmp_message_list[IPMI_SENSORS_MAX_LIST];
  unsigned int num_messages = 0;
  unsigned int count = 0;
  unsigned int i;
  int j;

  assert (state_data);
  assert (event_message_list);
  assert (event_message_list_len);
  assert (no_event_msg);

  /* achu: multiple threshold flags can be set (e.g. if we pass the
   * critical threshold, we've also passed the non-critical threshold)
   * but we only want to output one message at the max.  Luckily for
   * us (and due to smarts by the IPMI specification authors) if we go
   * from high bits to low bits, we will read the flags in the correct
   * order for output.
   *
   * If you're confused why were use 'ipmi_get_threshold_message'
   * instead of 'ipmi_get_generic_event_message' (b/c this is
   * presumably event_reading_type_code == 0x01), the reason is b/c we
   * actually called the get_sensor_reading command.  In other IPMI
   * subsystems (like the SEL) we don't call the get_sensor_reading
   * command, which is when you fall back on
   * 'ipmi_get_generic_event_message'.
   */

  for (j = 5; j >= 0; j--)
    {
      char buf[IPMI_SENSORS_BUFLEN];
      uint16_t bit;
      
      bit = 0x1 << j;

      if (sensor_event_bitmask & bit)
        {
          if (ipmi_get_threshold_message (j,
                                          buf,
                                          IPMI_SENSORS_BUFLEN) <= 0)
            continue;

          if (!(tmp_message_list[num_messages] = strdup (buf)))
            {
              pstdout_perror (state_data->pstate, "strdup");
              goto cleanup;
            }

          num_messages++;
          break;
        }
    }

  if (num_messages)
    count = num_messages;
  else
    count = 1;

  if (!(tmp_event_message_list = (char **) malloc (sizeof (char *) * (count + 1))))
    {
      pstdout_perror (state_data->pstate, "malloc");
      goto cleanup;
    }

  if (num_messages)
    {
      for (i = 0; i < num_messages; i++)
        tmp_event_message_list[i] = tmp_message_list[i];
    }
  else
    {
      if (!(tmp_event_message_list[0] = strdup (no_event_msg)))
        {
          pstdout_perror (state_data->pstate, "strdup");
          goto cleanup;
        }
    }

  tmp_event_message_list[count] = NULL;
  *event_message_list = tmp_event_message_list;
  /* achu: note, not like generic_event_message_list or
   * sensor_specific_event_message_list, max of one message output
   */
  *event_message_list_len = 1;

  return (0);

 cleanup:
  for (i = 0; i < num_messages; i++)
    free (tmp_message_list[i]);
  if (tmp_event_message_list)
    free (tmp_event_message_list);
  return (-1);
}

static int
_event_reading_type_code_is_oem_interpretable (struct ipmi_sensors_state_data *state_data,
                                               uint8_t event_reading_type_code)
{
  assert (state_data);
  assert (state_data->prog_data->args->interpret_oem_data);
  
  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   */
  if (state_data->oem_data.manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (state_data->oem_data.product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || state_data->oem_data.product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && event_reading_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_DELL_STATUS)
    return (1);
  
  return (0);
}

int
get_generic_event_message_list (struct ipmi_sensors_state_data *state_data,
                                char ***event_message_list,
                                unsigned int *event_message_list_len,
                                uint8_t event_reading_type_code,
                                uint16_t sensor_event_bitmask,
                                char *no_event_msg)
{
  char **tmp_event_message_list = NULL;
  char *tmp_message_list[IPMI_SENSORS_MAX_LIST];
  unsigned int num_messages = 0;
  unsigned int count = 0;
  unsigned int i;

  assert (state_data);
  assert (event_message_list);
  assert (event_message_list_len);
  assert (no_event_msg);

  for (i = 0; i < 16; i++)
    {
      char buf[IPMI_SENSORS_BUFLEN];
      uint16_t bit;
      int ret;

      bit = 0x1 << i;
      if (sensor_event_bitmask & bit)
        {
          if (IPMI_EVENT_READING_TYPE_CODE_IS_OEM (event_reading_type_code)
              && state_data->prog_data->args->interpret_oem_data
              && _event_reading_type_code_is_oem_interpretable (state_data, event_reading_type_code))
            {
              ret = ipmi_get_oem_generic_event_message (state_data->oem_data.manufacturer_id,
                                                        state_data->oem_data.product_id,
                                                        event_reading_type_code,
                                                        i,
                                                        buf,
                                                        IPMI_SENSORS_BUFLEN);
            }
          else
            {
              if (!state_data->prog_data->args->verbose_count)
                ret = ipmi_get_generic_event_message_short (event_reading_type_code,
                                                            i,
                                                            buf,
                                                            IPMI_SENSORS_BUFLEN);
              else
                ret = ipmi_get_generic_event_message (event_reading_type_code,
                                                      i,
                                                      buf,
                                                      IPMI_SENSORS_BUFLEN);
            }

          if (ret < 0)
            {
              if (!(tmp_message_list[num_messages++] = strdup (UNRECOGNIZED_STATE)))
                {
                  pstdout_perror (state_data->pstate, "strdup");
                  goto cleanup;
                }
              continue;
            }

          if (ret)
            {
              if (!(tmp_message_list[num_messages++] = strdup (buf)))
                {
                  pstdout_perror (state_data->pstate, "strdup");
                  goto cleanup;
                }
            }
        }
    }

  if (num_messages)
    count = num_messages;
  else
    count = 1;

  if (!(tmp_event_message_list = (char **) malloc (sizeof (char *) * (count + 1))))
    {
      pstdout_perror (state_data->pstate, "malloc");
      goto cleanup;
    }

  if (num_messages)
    {
      for (i = 0; i < num_messages; i++)
        tmp_event_message_list[i] = tmp_message_list[i];
    }
  else
    {
      if (!(tmp_event_message_list[0] = strdup (no_event_msg)))
        {
          pstdout_perror (state_data->pstate, "strdup");
          goto cleanup;
        }
    }

  tmp_event_message_list[count] = NULL;
  *event_message_list = tmp_event_message_list;
  *event_message_list_len = count;

  return (0);

 cleanup:
  for (i = 0; i < num_messages; i++)
    free (tmp_message_list[i]);
  if (tmp_event_message_list)
    free (tmp_event_message_list);
  return (-1);
}

static int
_sensor_type_is_oem_interpretable (struct ipmi_sensors_state_data *state_data,
                                   uint8_t sensor_type)
{
  assert (state_data);
  assert (state_data->prog_data->args->interpret_oem_data);
  
  /* OEM Interpretation
   *
   * Dell Poweredge R610
   * Dell Poweredge R710
   */
  if (state_data->oem_data.manufacturer_id == IPMI_IANA_ENTERPRISE_ID_DELL
      && (state_data->oem_data.product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R610
          || state_data->oem_data.product_id == IPMI_DELL_PRODUCT_ID_POWEREDGE_R710)
      && (sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_SYSTEM_PERFORMANCE_DEGRADATION_STATUS
          || sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_LINK_TUNING
          || sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_NON_FATAL_ERROR
          || sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_FATAL_IO_ERROR
          || sensor_type == IPMI_SENSOR_TYPE_OEM_DELL_UPGRADE))
    return (1);
  
  return (0);
}

int
get_sensor_specific_event_message_list (struct ipmi_sensors_state_data *state_data,
                                        char ***event_message_list,
                                        unsigned int *event_message_list_len,
                                        uint8_t sensor_type,
                                        uint16_t sensor_event_bitmask,
                                        char *no_event_msg)
{
  char **tmp_event_message_list = NULL;
  char *tmp_message_list[IPMI_SENSORS_MAX_LIST];
  unsigned int num_messages = 0;
  unsigned int count = 0;
  unsigned int i;

  assert (state_data);
  assert (event_message_list);
  assert (event_message_list_len);
  assert (no_event_msg);

  for (i = 0; i < 16; i++)
    {
      char buf[IPMI_SENSORS_BUFLEN];
      uint16_t bit;
      int ret;

      bit = 0x1 << i;

      if (sensor_event_bitmask & bit)
        {
          if (IPMI_SENSOR_TYPE_IS_OEM (sensor_type)
              && state_data->prog_data->args->interpret_oem_data
              && _sensor_type_is_oem_interpretable (state_data, sensor_type))
            {
              ret = ipmi_get_oem_sensor_type_message (state_data->oem_data.manufacturer_id,
                                                      state_data->oem_data.product_id,
                                                      sensor_type,
                                                      i,
                                                      buf,
                                                      IPMI_SENSORS_BUFLEN);
            }
          else
            {
              if (!state_data->prog_data->args->verbose_count)
                ret = ipmi_get_sensor_type_message_short (sensor_type,
                                                          i,
                                                          buf,
                                                          IPMI_SENSORS_BUFLEN);
              else
                ret = ipmi_get_sensor_type_message (sensor_type,
                                                    i,
                                                    buf,
                                                    IPMI_SENSORS_BUFLEN);
            }

          if (ret < 0)
            {
              if (!(tmp_message_list[num_messages++] = strdup (UNRECOGNIZED_STATE)))
                {
                  pstdout_perror (state_data->pstate, "strdup");
                  goto cleanup;
                }
              continue;
            }

          if (ret)
            {
              if (!(tmp_message_list[num_messages++] = strdup (buf)))
                {
                  pstdout_perror (state_data->pstate, "strdup");
                  goto cleanup;
                }
            }
        }
    }

  if (num_messages)
    count = num_messages;
  else
    count = 1;

  if (!(tmp_event_message_list = (char **) malloc (sizeof (char *) * (count + 1))))
    {
      pstdout_perror (state_data->pstate, "malloc");
      goto cleanup;
    }

  if (num_messages)
    {
      for (i = 0; i < num_messages; i++)
        tmp_event_message_list[i] = tmp_message_list[i];
    }
  else
    {
      if (!(tmp_event_message_list[0] = strdup (no_event_msg)))
        {
          pstdout_perror (state_data->pstate, "strdup");
          goto cleanup;
        }
    }

  tmp_event_message_list[count] = NULL;
  *event_message_list = tmp_event_message_list;
  *event_message_list_len = count;

  return (0);

 cleanup:
  for (i = 0; i < num_messages; i++)
    free (tmp_message_list[i]);
  if (tmp_event_message_list)
    free (tmp_event_message_list);
  return (-1);
}
