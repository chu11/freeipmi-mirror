/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team
   
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

#include "ipmi-sensors-util.h"

#include "freeipmi-portability.h"

#define IPMI_SENSORS_BUFLEN   1024
#define IPMI_SENSORS_MAX_LIST   32

#define UNRECOGNIZED_STATE "Unrecognized State"

int
get_msg_message_list (struct ipmi_sensors_state_data *state_data,
                      char ***event_message_list,
                      unsigned int *event_message_list_len,
                      char *msg)
{
  char **tmp_event_message_list = NULL;
  int count = 0;
  
  assert(state_data);
  assert(event_message_list);
  assert(event_message_list_len);
  assert(msg);

  count = 1;
    
  if (!(tmp_event_message_list = (char **) malloc (sizeof (char *) * (count + 1))))
    {
      pstdout_perror(state_data->pstate, "malloc");
      goto cleanup;
    }
    
  if (!(tmp_event_message_list[0] = strdup(msg)))
    {
      pstdout_perror(state_data->pstate, "strdup");
      goto cleanup;
    }
  
  tmp_event_message_list[count] = NULL;
  *event_message_list = tmp_event_message_list;
  *event_message_list_len = 1;

  return 0;

 cleanup:
  if (tmp_event_message_list)
    free(tmp_event_message_list);
  return -1;
}

int
get_threshold_message_list (struct ipmi_sensors_state_data *state_data,
                            char ***event_message_list,
                            unsigned int *event_message_list_len,
                            uint8_t sensor_event_bitmask,
                            char *no_event_msg)
{
  char **tmp_event_message_list = NULL;
  char *tmp_message_list[IPMI_SENSORS_MAX_LIST];
  int num_messages = 0;
  int count = 0;
  int i;
  
  assert(state_data);
  assert(event_message_list);
  assert(event_message_list_len);
  assert(no_event_msg);

  /* achu: multiple threshold flags can be set (i.e. if we pass the
   * critical threshold, we've also passed the non-critical threshold)
   * but we only want to * output one message at the max.  Luckily for
   * us (and due to smarts * by the IPMI specification authors) if we
   * go from high bits to low * bits, we will read the flags in the
   * correct order for output.
   *
   * If you're confused why were use 'ipmi_get_threshold_message'
   * instead of 'ipmi_get_generic_event_message' (b/c this is
   * presumably event_reading_type_code == 0x01), the reason is b/c we
   * actually called the get_sensor_reading command.  In other IPMI
   * subsystems (like the SEL) we don't call the get_sensor_reading
   * command, which is when you fall back on
   * 'ipmi_get_generic_event_message'.
   */

  for (i = 5; i >= 0; i--)
    {
      char buf[IPMI_SENSORS_BUFLEN];
      uint16_t bit; 

      bit = 0x1 << i;

      if (sensor_event_bitmask & bit)
	{
	  if (ipmi_get_threshold_message (i,
                                          buf,
                                          IPMI_SENSORS_BUFLEN) < 0)
            continue;
	  
	  if (!(tmp_message_list[num_messages] = strdup(buf)))
            {
              pstdout_perror(state_data->pstate, "strdup");
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
      pstdout_perror(state_data->pstate, "malloc");
      goto cleanup;
    }
    
  if (num_messages)
    {
      for (i = 0; i < num_messages; i++)
	tmp_event_message_list[i] = tmp_message_list[i];
    }
  else
    {
      if (!(tmp_event_message_list[0] = strdup(no_event_msg)))
        {
          pstdout_perror(state_data->pstate, "strdup");
          goto cleanup;
        }
    }
  
  tmp_event_message_list[count] = NULL;
  *event_message_list = tmp_event_message_list;
  /* achu: note, not like generic_event_message_list or
   * sensor_specific_event_message_list, max of one message output
   */
  *event_message_list_len = 1;

  return 0;

 cleanup:
  for (i = 0; i < num_messages; i++)
    free(tmp_message_list[i]);
  if (tmp_event_message_list)
    free(tmp_event_message_list);
  return -1;
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
  int num_messages = 0;
  int count = 0;
  int i;
  
  assert(state_data);
  assert(event_message_list);
  assert(event_message_list_len);
  assert(no_event_msg);

  for (i = 0; i < 16; i++)
    {
      char buf[IPMI_SENSORS_BUFLEN];
      uint16_t bit; 
      int ret;
      
      bit = 0x1 << i;
      if (sensor_event_bitmask & bit)
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

          if (ret < 0)
            {
              if (!(tmp_message_list[num_messages++] = strdup(UNRECOGNIZED_STATE)))
                {
                  pstdout_perror(state_data->pstate, "strdup");
                  goto cleanup;
                }
              continue;
            }

	  if (!(tmp_message_list[num_messages++] = strdup(buf)))
            {
              pstdout_perror(state_data->pstate, "strdup");
              goto cleanup;
            }
	}
    }
  
  if (num_messages)
    count = num_messages;
  else
    count = 1;

  if (!(tmp_event_message_list = (char **) malloc (sizeof (char *) * (count + 1))))
    {
      pstdout_perror(state_data->pstate, "malloc");
      goto cleanup;
    }
      
  if (num_messages)
    {
      for (i = 0; i < num_messages; i++)
	tmp_event_message_list[i] = tmp_message_list[i];
    }
  else
    {
      if (!(tmp_event_message_list[0] = strdup(no_event_msg)))
        {
          pstdout_perror(state_data->pstate, "strdup");
          goto cleanup;
        }
    }

  tmp_event_message_list[count] = NULL;
  *event_message_list = tmp_event_message_list;
  *event_message_list_len = count;
  
  return 0;

 cleanup:
  for (i = 0; i < num_messages; i++)
    free(tmp_message_list[i]);
  if (tmp_event_message_list)
    free(tmp_event_message_list);
  return -1;
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
  int num_messages = 0;
  int count = 0;
  int i;
  
  assert(state_data);
  assert(event_message_list);
  assert(event_message_list_len);
  assert(no_event_msg);

  for (i = 0; i < 16; i++)
    {
      char buf[IPMI_SENSORS_BUFLEN];
      uint16_t bit; 
      int ret;

      bit = 0x1 << i;

      if (sensor_event_bitmask & bit)
	{
          if (!state_data->prog_data->args->verbose_count)
            ret = ipmi_get_sensor_type_code_message_short (sensor_type,
                                                           i,
                                                           buf,
                                                           IPMI_SENSORS_BUFLEN);
          else
            ret = ipmi_get_sensor_type_code_message (sensor_type,
                                                     i,
                                                     buf,
                                                     IPMI_SENSORS_BUFLEN);

          if (ret < 0)
            {
              if (!(tmp_message_list[num_messages++] = strdup(UNRECOGNIZED_STATE)))
                {
                  pstdout_perror(state_data->pstate, "strdup");
                  goto cleanup;
                }
              continue;
            }

	  if (!(tmp_message_list[num_messages++] = strdup(buf)))
            {
              pstdout_perror(state_data->pstate, "strdup");
              goto cleanup;
            }
	}
    }
  
  if (num_messages)
    count = num_messages;
  else
    count = 1;

  if (!(tmp_event_message_list = (char **) malloc (sizeof (char *) * (count + 1))))
    {
      pstdout_perror(state_data->pstate, "malloc");
      goto cleanup;
    }
      
  if (num_messages)
    {
      for (i = 0; i < num_messages; i++)
	tmp_event_message_list[i] = tmp_message_list[i];
    }
  else
    {
      if (!(tmp_event_message_list[0] = strdup(no_event_msg)))
        {
          pstdout_perror(state_data->pstate, "strdup");
          goto cleanup;
        }
    }

  tmp_event_message_list[count] = NULL;
  *event_message_list = tmp_event_message_list;
  *event_message_list_len = count;

  return 0;

 cleanup:
  for (i = 0; i < num_messages; i++)
    free(tmp_message_list[i]);
  if (tmp_event_message_list)
    free(tmp_event_message_list);
  return -1;
}

void 
str_replace_char (char *str, char chr, char with)
{
  char *p = NULL;
  char *s = NULL;
  
  assert(str);
  
  for (s = str;
       (p = strchr (s, chr));
       s = p + 1)
    *p = with;
}

