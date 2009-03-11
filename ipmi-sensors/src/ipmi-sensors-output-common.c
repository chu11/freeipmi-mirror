/*
  Copyright (C) 2003-2009 FreeIPMI Core Team

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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-sensors.h"
#include "ipmi-sensors-output-common.h"
#include "ipmi-sensors-util.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"

#define IPMI_SENSORS_SPACE_BUFFER 1024

int
ipmi_sensors_group_specified (ipmi_sensors_state_data_t *state_data,
                              uint8_t *sdr_record,
                              unsigned int sdr_record_len)
{
  const char *sensor_group = NULL;
  uint8_t record_type;
  uint8_t sensor_type;
  int i;

  assert (state_data);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (state_data->prog_data->args->groups_wanted);

  if (ipmi_sdr_parse_record_id_and_type (state_data->sdr_parse_ctx,
                                         sdr_record,
                                         sdr_record_len,
                                         NULL,
                                         &record_type) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_record_id_and_type: %s\n",
                       ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
      return (-1);
    }

  if (record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
      || record_type == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD
      || record_type == IPMI_SDR_FORMAT_EVENT_ONLY_RECORD)
    {
      if (ipmi_sdr_parse_sensor_type (state_data->sdr_parse_ctx,
                                      sdr_record,
                                      sdr_record_len,
                                      &sensor_type) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_sdr_parse_sensor_type: %s\n",
                           ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
          return (-1);
        }

      /* Don't use get_sensor_group_output_string() - want NULL if invalid */
      sensor_group = ipmi_get_sensor_type_string (sensor_type);
    }

  if (sensor_group)
    {
      char sensor_group_cmdline[IPMI_SENSORS_MAX_GROUPS_STRING_LENGTH];

      strcpy (sensor_group_cmdline, sensor_group);
      get_sensor_group_cmdline_string (sensor_group_cmdline);

      for (i = 0; i < state_data->prog_data->args->groups_length; i++)
        {
          if (!strcasecmp (sensor_group,
                           state_data->prog_data->args->groups[i])
              || !strcasecmp (sensor_group_cmdline,
                              state_data->prog_data->args->groups[i]))
            return (1);
        }
    }

  return (0);
}

int
ipmi_sensors_output_event_message_list (ipmi_sensors_state_data_t *state_data,
                                        char **event_message_list,
                                        unsigned int event_message_list_len,
                                        char *prefix,
                                        unsigned int each_on_newline)
{
  char spcbuf[IPMI_SENSORS_SPACE_BUFFER + 1];
  int i;

  assert (state_data);

  if (prefix)
    pstdout_printf (state_data->pstate, "%s", prefix);

  memset (spcbuf, '\0', IPMI_SENSORS_SPACE_BUFFER + 1);
  if (prefix && each_on_newline)
    {
      unsigned int len;

      len = strlen (prefix);
      if (len > IPMI_SENSORS_SPACE_BUFFER)
        len = IPMI_SENSORS_SPACE_BUFFER;

      for (i = 0; i < len; i++)
        strcat (spcbuf, " ");
    }

  if (event_message_list)
    {
      if (event_message_list_len >= 1)
        {
          if (state_data->prog_data->args->legacy_output)
            pstdout_printf (state_data->pstate,
                            "[%s]",
                            event_message_list[0]);
          else
            {
              if (!strcmp (event_message_list[0], IPMI_SENSORS_NA_STRING))
                pstdout_printf (state_data->pstate,
                                "%s",
                                event_message_list[0]);
              else
                pstdout_printf (state_data->pstate,
                                "'%s'",
                                event_message_list[0]);
            }
        }

      for (i = 1; i < event_message_list_len; i++)
        {
          if (each_on_newline)
            pstdout_printf (state_data->pstate,
                            "\n");
          if (state_data->prog_data->args->legacy_output)
            pstdout_printf (state_data->pstate,
                            "%s[%s]",
                            spcbuf,
                            event_message_list[i]);
          else
            pstdout_printf (state_data->pstate,
                            "%s'%s'",
                            prefix,
                            event_message_list[i]);
        }

      pstdout_printf (state_data->pstate,
                      "\n");
    }
  else
    {
      if (state_data->prog_data->args->legacy_output)
        pstdout_printf (state_data->pstate,
                        "[%s]\n",
                        "Unknown");
      else
        pstdout_printf (state_data->pstate,
                        "%s\n",
                        "Unknown");
    }

  return (0);
}

/* emulate a call to ipmi_cmd_get_sensor_thresholds succeeding by
 * stuffing the response with data from the SDR
 */
static int
_get_sdr_sensor_thresholds (ipmi_sensors_state_data_t *state_data,
                            uint8_t *sdr_record,
                            unsigned int sdr_record_len,
                            fiid_obj_t obj_get_sensor_thresholds_rs)
{
  uint8_t lower_non_critical_threshold_readable = 0;
  uint8_t lower_critical_threshold_readable = 0;
  uint8_t lower_non_recoverable_threshold_readable = 0;
  uint8_t upper_non_critical_threshold_readable = 0;
  uint8_t upper_critical_threshold_readable = 0;
  uint8_t upper_non_recoverable_threshold_readable = 0;
  uint8_t lower_non_critical_threshold_temp = 0;
  uint8_t lower_critical_threshold_temp = 0;
  uint8_t lower_non_recoverable_threshold_temp = 0;
  uint8_t upper_non_critical_threshold_temp = 0;
  uint8_t upper_critical_threshold_temp = 0;
  uint8_t upper_non_recoverable_threshold_temp = 0;
  uint8_t lower_non_critical_threshold = 0;
  uint8_t lower_critical_threshold = 0;
  uint8_t lower_non_recoverable_threshold = 0;
  uint8_t upper_non_critical_threshold = 0;
  uint8_t upper_critical_threshold = 0;
  uint8_t upper_non_recoverable_threshold = 0;
  int rv = -1;

  assert (state_data);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (obj_get_sensor_thresholds_rs);
  assert (fiid_obj_template_compare (obj_get_sensor_thresholds_rs,
                                     tmpl_cmd_get_sensor_thresholds_rs) > 0);

  if (fiid_obj_set (obj_get_sensor_thresholds_rs,
                    "cmd",
                    IPMI_CMD_GET_SENSOR_THRESHOLDS) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set: 'cmd': %s\n",
                       fiid_obj_errormsg (obj_get_sensor_thresholds_rs));
      goto cleanup;
    }
  if (fiid_obj_set (obj_get_sensor_thresholds_rs,
                    "comp_code",
                    IPMI_COMP_CODE_COMMAND_SUCCESS) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set: 'comp_code': %s\n",
                       fiid_obj_errormsg (obj_get_sensor_thresholds_rs));
      goto cleanup;
    }

  if (ipmi_sdr_parse_threshold_readable (state_data->sdr_parse_ctx,
                                         sdr_record,
                                         sdr_record_len,
                                         &lower_non_critical_threshold_readable,
                                         &lower_critical_threshold_readable,
                                         &lower_non_recoverable_threshold_readable,
                                         &upper_non_critical_threshold_readable,
                                         &upper_critical_threshold_readable,
                                         &upper_non_recoverable_threshold_readable) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_threshold_readable: %s\n",
                       ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
      goto cleanup;
    }

  if (fiid_obj_set (obj_get_sensor_thresholds_rs,
                    "readable_thresholds.lower_non_critical_threshold",
                    lower_non_critical_threshold_readable) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set: 'readable_thresholds.lower_non_critical_threshold': %s\n",
                       fiid_obj_errormsg (obj_get_sensor_thresholds_rs));
      goto cleanup;
    }
  if (fiid_obj_set (obj_get_sensor_thresholds_rs,
                    "readable_thresholds.lower_critical_threshold",
                    lower_critical_threshold_readable) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set: 'readable_thresholds.lower_critical_threshold': %s\n",
                       fiid_obj_errormsg (obj_get_sensor_thresholds_rs));
      goto cleanup;
    }
  if (fiid_obj_set (obj_get_sensor_thresholds_rs,
                    "readable_thresholds.lower_non_recoverable_threshold",
                    lower_non_recoverable_threshold_readable) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set: 'readable_thresholds.lower_non_recoverable_threshold': %s\n",
                       fiid_obj_errormsg (obj_get_sensor_thresholds_rs));
      goto cleanup;
    }
  if (fiid_obj_set (obj_get_sensor_thresholds_rs,
                    "readable_thresholds.upper_non_critical_threshold",
                    upper_non_critical_threshold_readable) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set: 'readable_thresholds.upper_non_critical_threshold': %s\n",
                       fiid_obj_errormsg (obj_get_sensor_thresholds_rs));
      goto cleanup;
    }
  if (fiid_obj_set (obj_get_sensor_thresholds_rs,
                    "readable_thresholds.upper_critical_threshold",
                    upper_critical_threshold_readable) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set: 'readable_thresholds.upper_critical_threshold': %s\n",
                       fiid_obj_errormsg (obj_get_sensor_thresholds_rs));
      goto cleanup;
    }
  if (fiid_obj_set (obj_get_sensor_thresholds_rs,
                    "readable_thresholds.upper_non_recoverable_threshold",
                    upper_non_recoverable_threshold_readable) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set: 'readable_thresholds.upper_non_recoverable_threshold': %s\n",
                       fiid_obj_errormsg (obj_get_sensor_thresholds_rs));
      goto cleanup;
    }
  if (fiid_obj_set (obj_get_sensor_thresholds_rs,
                    "reserved",
                    0) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set: 'reserved': %s\n",
                       fiid_obj_errormsg (obj_get_sensor_thresholds_rs));
      goto cleanup;
    }

  if (ipmi_sdr_parse_thresholds_raw (state_data->sdr_parse_ctx,
                                     sdr_record,
                                     sdr_record_len,
                                     &lower_non_critical_threshold_temp,
                                     &lower_critical_threshold_temp,
                                     &lower_non_recoverable_threshold_temp,
                                     &upper_non_critical_threshold_temp,
                                     &upper_critical_threshold_temp,
                                     &upper_non_recoverable_threshold_temp) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_thresholds_raw: %s\n",
                       ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
      goto cleanup;
    }

  if (lower_non_critical_threshold_readable)
    lower_non_critical_threshold = lower_non_critical_threshold_temp;
  if (lower_critical_threshold_readable)
    lower_critical_threshold = lower_critical_threshold_temp;
  if (lower_non_recoverable_threshold_readable)
    lower_non_recoverable_threshold = lower_non_recoverable_threshold_temp;
  if (upper_non_critical_threshold_readable)
    upper_non_critical_threshold = upper_non_critical_threshold_temp;
  if (upper_critical_threshold_readable)
    upper_critical_threshold = upper_critical_threshold_temp;
  if (upper_non_recoverable_threshold_readable)
    upper_non_recoverable_threshold = upper_non_recoverable_threshold_temp;

  if (fiid_obj_set (obj_get_sensor_thresholds_rs,
                    "lower_non_critical_threshold",
                    lower_non_critical_threshold) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set: 'lower_non_critical_threshold': %s\n",
                       fiid_obj_errormsg (obj_get_sensor_thresholds_rs));
      goto cleanup;
    }
  if (fiid_obj_set (obj_get_sensor_thresholds_rs,
                    "lower_critical_threshold",
                    lower_critical_threshold) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set: 'lower_critical_threshold': %s\n",
                       fiid_obj_errormsg (obj_get_sensor_thresholds_rs));
      goto cleanup;
    }
  if (fiid_obj_set (obj_get_sensor_thresholds_rs,
                    "lower_non_recoverable_threshold",
                    lower_non_recoverable_threshold) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set: 'lower_non_recoverable_threshold': %s\n",
                       fiid_obj_errormsg (obj_get_sensor_thresholds_rs));
      goto cleanup;
    }
  if (fiid_obj_set (obj_get_sensor_thresholds_rs,
                    "upper_non_critical_threshold",
                    upper_non_critical_threshold) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set: 'upper_non_critical_threshold': %s\n",
                       fiid_obj_errormsg (obj_get_sensor_thresholds_rs));
      goto cleanup;
    }
  if (fiid_obj_set (obj_get_sensor_thresholds_rs,
                    "upper_critical_threshold",
                    upper_critical_threshold) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set: 'upper_critical_threshold': %s\n",
                       fiid_obj_errormsg (obj_get_sensor_thresholds_rs));
      goto cleanup;
    }
  if (fiid_obj_set (obj_get_sensor_thresholds_rs,
                    "upper_non_recoverable_threshold",
                    upper_non_recoverable_threshold) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_set: 'upper_non_recoverable_threshold': %s\n",
                       fiid_obj_errormsg (obj_get_sensor_thresholds_rs));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_sensors_get_thresholds (ipmi_sensors_state_data_t *state_data,
                             uint8_t *sdr_record,
                             unsigned int sdr_record_len,
                             double **lower_non_critical_threshold,
                             double **lower_critical_threshold,
                             double **lower_non_recoverable_threshold,
                             double **upper_non_critical_threshold,
                             double **upper_critical_threshold,
                             double **upper_non_recoverable_threshold)
{
  int8_t r_exponent, b_exponent;
  int16_t m, b;
  uint8_t linearization, analog_data_format;
  uint8_t sensor_number;
  fiid_obj_t obj_cmd_rs = NULL;
  double *tmp_lower_non_critical_threshold = NULL;
  double *tmp_lower_critical_threshold = NULL;
  double *tmp_lower_non_recoverable_threshold = NULL;
  double *tmp_upper_non_critical_threshold = NULL;
  double *tmp_upper_critical_threshold = NULL;
  double *tmp_upper_non_recoverable_threshold = NULL;
  double threshold;
  uint8_t threshold_access_support;
  uint64_t val;
  int rv = -1;

  assert (state_data);
  assert (sdr_record);
  assert (sdr_record_len);

  if (lower_non_critical_threshold)
    *lower_non_critical_threshold = NULL;
  if (lower_critical_threshold)
    *lower_critical_threshold = NULL;
  if (lower_non_recoverable_threshold)
    *lower_non_recoverable_threshold = NULL;
  if (upper_non_critical_threshold)
    *upper_non_critical_threshold = NULL;
  if (upper_critical_threshold)
    *upper_critical_threshold = NULL;
  if (upper_non_recoverable_threshold)
    *upper_non_recoverable_threshold = NULL;

  /* achu: first lets check if we have anything to output */
  if (ipmi_sdr_parse_sensor_capabilities (state_data->sdr_parse_ctx,
                                          sdr_record,
                                          sdr_record_len,
                                          NULL,
                                          &threshold_access_support,
                                          NULL,
                                          NULL,
                                          NULL) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_capabilities: %s\n",
                       ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
      goto cleanup;
    }

  if (threshold_access_support == IPMI_SDR_NO_THRESHOLDS_SUPPORT
      || threshold_access_support == IPMI_SDR_FIXED_UNREADABLE_THRESHOLDS_SUPPORT)
    {
      rv = 0;
      goto cleanup;
    }

  /* achu:
   *
   * I will admit I'm not entirely sure what the best way is
   * to get thresholds.  It seems the information is
   * stored/retrievable in the SDR and through an IPMI command.
   *
   * Since the readable_threshold_mask in the SDR record indicates the
   * mask is for the "Get Sensor Thresholds" command, it suggests the
   * best/right way is to get the values via that command.  Sounds
   * good to me.  Also, I suppose its possible that changes to the
   * thresholds may not be written to the SDR.
   *
   * Also, because the results from the get_sensor_thresholds include
   * readability flags, we can ignore the readability flags in the
   * SDR.
   *
   */

  if (ipmi_sdr_parse_sensor_number (state_data->sdr_parse_ctx,
                                    sdr_record,
                                    sdr_record_len,
                                    &sensor_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_number: %s\n",
                       ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_decoding_data (state_data->sdr_parse_ctx,
                                           sdr_record,
                                           sdr_record_len,
                                           &r_exponent,
                                           &b_exponent,
                                           &m,
                                           &b,
                                           &linearization,
                                           &analog_data_format) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_decoding_data: %s\n",
                       ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
      goto cleanup;
    }

  /* if the sensor is not analog, this is most likely a bug in the
   * SDR, since we shouldn't be decoding a non-threshold sensor.
   *
   * Don't return an error.  Allow code to output "NA" or something.
   */
  if (!IPMI_SDR_ANALOG_DATA_FORMAT_VALID (analog_data_format))
    {
      if (state_data->prog_data->args->common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "Attempting to decode non-analog sensor\n");
      rv = 0;
      goto cleanup;
    }

  /* if the sensor is non-linear, I just don't know what to do
   *
   * Don't return an error.  Allow code to output "NA" or something.
   */
  if (!IPMI_SDR_LINEARIZATION_IS_LINEAR (linearization))
    {
      if (state_data->prog_data->args->common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "Cannot decode non-linear sensor\n");
      rv = 0;
      goto cleanup;
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sensor_thresholds_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_sensor_thresholds (state_data->ipmi_ctx,
                                      sensor_number,
                                      obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_sensor_thresholds: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));
      if ((ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE_REQUEST_DATA_INVALID)
          && (ipmi_check_completion_code (obj_cmd_rs,
                                          IPMI_COMP_CODE_COMMAND_ILLEGAL_FOR_SENSOR_OR_RECORD_TYPE) == 1
              || ipmi_check_completion_code (obj_cmd_rs,
                                             IPMI_COMP_CODE_REQUEST_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1))
        {
          /* The thresholds cannot be gathered for one reason or
           * another, maybe b/c its a OEM sensor or something.  We can
           * return (0) gracefully.
           */
          rv = 0;
          goto cleanup;
        }

      /*
       * IPMI Workaround (achu)
       *
       * Discovered on HP DL585
       *
       * Seems that the HP machine doesn't support the "Get Sensor
       * Thresholds" command.  When this occurs, we assume the
       * information in the SDR is legit and up to date.  Go get it
       * and fill in the object respectively.
       */
      if ((ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE_INVALID_COMMAND)
          && (ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_COMMAND_INVALID) == 1))
        {
          if (state_data->prog_data->args->common.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "Get Sensor Thresholds failed, using SDR information\n");

          if (_get_sdr_sensor_thresholds (state_data,
                                          sdr_record,
                                          sdr_record_len,
                                          obj_cmd_rs) < 0)
            goto cleanup;

          goto continue_get_sensor_thresholds;
        }

      goto cleanup;
    }

 continue_get_sensor_thresholds:

  if (lower_non_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "readable_thresholds.lower_non_critical_threshold",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'readable_thresholds.lower_non_critical_threshold': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      if (val)
        {
          if (FIID_OBJ_GET (obj_cmd_rs,
                            "lower_non_critical_threshold",
                            &val) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "fiid_obj_get: 'lower_non_critical_threshold': %s\n",
                               fiid_obj_errormsg (obj_cmd_rs));
              goto cleanup;
            }

          if (ipmi_sensor_decode_value (r_exponent,
                                        b_exponent,
                                        m,
                                        b,
                                        linearization,
                                        analog_data_format,
                                        val,
                                        &threshold) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sensor_decode_value: %s\n",
                               strerror (errno));
              goto cleanup;
            }

          if (!(tmp_lower_non_critical_threshold = (double *)malloc (sizeof(double))))
            {
              pstdout_perror (state_data->pstate, "malloc");
              goto cleanup;
            }
          *tmp_lower_non_critical_threshold = threshold;
        }
    }
  if (lower_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "readable_thresholds.lower_critical_threshold",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'readable_thresholds.lower_critical_threshold': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      if (val)
        {
          if (FIID_OBJ_GET (obj_cmd_rs,
                            "lower_critical_threshold",
                            &val) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "fiid_obj_get: 'lower_critical_threshold': %s\n",
                               fiid_obj_errormsg (obj_cmd_rs));
              goto cleanup;
            }

          if (ipmi_sensor_decode_value (r_exponent,
                                        b_exponent,
                                        m,
                                        b,
                                        linearization,
                                        analog_data_format,
                                        val,
                                        &threshold) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sensor_decode_value: %s\n",
                               strerror (errno));
              goto cleanup;
            }

          if (!(tmp_lower_critical_threshold = (double *)malloc (sizeof(double))))
            {
              pstdout_perror (state_data->pstate, "malloc");
              goto cleanup;
            }
          *tmp_lower_critical_threshold = threshold;
        }
    }
  if (lower_non_recoverable_threshold)
    {
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "readable_thresholds.lower_non_recoverable_threshold",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'readable_thresholds.lower_non_recoverable_threshold': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      if (val)
        {
          if (FIID_OBJ_GET (obj_cmd_rs,
                            "lower_non_recoverable_threshold",
                            &val) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "fiid_obj_get: 'lower_non_recoverable_threshold': %s\n",
                               fiid_obj_errormsg (obj_cmd_rs));
              goto cleanup;
            }

          if (ipmi_sensor_decode_value (r_exponent,
                                        b_exponent,
                                        m,
                                        b,
                                        linearization,
                                        analog_data_format,
                                        val,
                                        &threshold) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sensor_decode_value: %s\n",
                               strerror (errno));
              goto cleanup;
            }

          if (!(tmp_lower_non_recoverable_threshold = (double *)malloc (sizeof(double))))
            {
              pstdout_perror (state_data->pstate, "malloc");
              goto cleanup;
            }
          *tmp_lower_non_recoverable_threshold = threshold;
        }
    }
  if (upper_non_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "readable_thresholds.upper_non_critical_threshold",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'readable_thresholds.upper_non_critical_threshold': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      if (val)
        {
          if (FIID_OBJ_GET (obj_cmd_rs,
                            "upper_non_critical_threshold",
                            &val) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "fiid_obj_get: 'upper_non_critical_threshold': %s\n",
                               fiid_obj_errormsg (obj_cmd_rs));
              goto cleanup;
            }

          if (ipmi_sensor_decode_value (r_exponent,
                                        b_exponent,
                                        m,
                                        b,
                                        linearization,
                                        analog_data_format,
                                        val,
                                        &threshold) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sensor_decode_value: %s\n",
                               strerror (errno));
              goto cleanup;
            }

          if (!(tmp_upper_non_critical_threshold = (double *)malloc (sizeof(double))))
            {
              pstdout_perror (state_data->pstate, "malloc");
              goto cleanup;
            }
          *tmp_upper_non_critical_threshold = threshold;
        }
    }
  if (upper_critical_threshold)
    {
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "readable_thresholds.upper_critical_threshold",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'readable_thresholds.upper_critical_threshold': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      if (val)
        {
          if (FIID_OBJ_GET (obj_cmd_rs,
                            "upper_critical_threshold",
                            &val) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "fiid_obj_get: 'upper_critical_threshold': %s\n",
                               fiid_obj_errormsg (obj_cmd_rs));
              goto cleanup;
            }

          if (ipmi_sensor_decode_value (r_exponent,
                                        b_exponent,
                                        m,
                                        b,
                                        linearization,
                                        analog_data_format,
                                        val,
                                        &threshold) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sensor_decode_value: %s\n",
                               strerror (errno));
              goto cleanup;
            }

          if (!(tmp_upper_critical_threshold = (double *)malloc (sizeof(double))))
            {
              pstdout_perror (state_data->pstate, "malloc");
              goto cleanup;
            }
          *tmp_upper_critical_threshold = threshold;
        }
    }
  if (upper_non_recoverable_threshold)
    {
      if (FIID_OBJ_GET (obj_cmd_rs,
                        "readable_thresholds.upper_non_recoverable_threshold",
                        &val) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get: 'readable_thresholds.upper_non_recoverable_threshold': %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }

      if (val)
        {
          if (FIID_OBJ_GET (obj_cmd_rs,
                            "upper_non_recoverable_threshold",
                            &val) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "fiid_obj_get: 'upper_non_recoverable_threshold': %s\n",
                               fiid_obj_errormsg (obj_cmd_rs));
              goto cleanup;
            }

          if (ipmi_sensor_decode_value (r_exponent,
                                        b_exponent,
                                        m,
                                        b,
                                        linearization,
                                        analog_data_format,
                                        val,
                                        &threshold) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sensor_decode_value: %s\n",
                               strerror (errno));
              goto cleanup;
            }

          if (!(tmp_upper_non_recoverable_threshold = (double *)malloc (sizeof(double))))
            {
              pstdout_perror (state_data->pstate, "malloc");
              goto cleanup;
            }
          *tmp_upper_non_recoverable_threshold = threshold;
        }
    }

  if (lower_non_critical_threshold)
    *lower_non_critical_threshold = tmp_lower_non_critical_threshold;
  if (lower_critical_threshold)
    *lower_critical_threshold = tmp_lower_critical_threshold;
  if (lower_non_recoverable_threshold)
    *lower_non_recoverable_threshold = tmp_lower_non_recoverable_threshold;
  if (upper_non_critical_threshold)
    *upper_non_critical_threshold = tmp_upper_non_critical_threshold;
  if (upper_critical_threshold)
    *upper_critical_threshold = tmp_upper_critical_threshold;
  if (upper_non_recoverable_threshold)
    *upper_non_recoverable_threshold = tmp_upper_non_recoverable_threshold;

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  if (rv < 0)
    {
      if (tmp_lower_non_critical_threshold)
        free (tmp_lower_non_critical_threshold);
      if (tmp_lower_critical_threshold)
        free (tmp_lower_critical_threshold);
      if (tmp_lower_non_recoverable_threshold)
        free (tmp_lower_non_recoverable_threshold);
      if (tmp_upper_non_critical_threshold)
        free (tmp_upper_non_critical_threshold);
      if (tmp_upper_critical_threshold)
        free (tmp_upper_critical_threshold);
      if (tmp_upper_non_recoverable_threshold)
        free (tmp_upper_non_recoverable_threshold);
    }
  return (rv);
}

int
ipmi_sensors_get_units_string (ipmi_sensors_state_data_t *state_data,
                               uint8_t *sdr_record,
                               unsigned int sdr_record_len,
                               char *sensor_units_buf,
                               unsigned int sensor_units_buflen,
                               unsigned int abbreviated_units_flag)
{
  uint8_t sensor_units_percentage;
  uint8_t sensor_units_modifier;
  uint8_t sensor_units_rate;
  uint8_t sensor_base_unit_type;
  uint8_t sensor_modifier_unit_type;
  int sensor_units_ret = 0;
  int rv = -1;

  assert (state_data);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (sensor_units_buf);
  assert (sensor_units_buflen);

  if (ipmi_sdr_parse_sensor_units (state_data->sdr_parse_ctx,
                                   sdr_record,
                                   sdr_record_len,
                                   &sensor_units_percentage,
                                   &sensor_units_modifier,
                                   &sensor_units_rate,
                                   &sensor_base_unit_type,
                                   &sensor_modifier_unit_type) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_units: %s\n",
                       ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
      goto cleanup;
    }
  
  memset (sensor_units_buf, '\0', sensor_units_buflen);
  sensor_units_ret = ipmi_sensor_units_string (sensor_units_percentage,
                                               sensor_units_modifier,
                                               sensor_units_rate,
                                               sensor_base_unit_type,
                                               sensor_modifier_unit_type,
                                               sensor_units_buf,
                                               sensor_units_buflen,
                                               abbreviated_units_flag);
  
  if (sensor_units_ret <= 0)
    snprintf (sensor_units_buf,
              sensor_units_buflen,
              "%s",
              ipmi_sensor_units[IPMI_SENSOR_UNIT_UNSPECIFIED]);
  
  rv = 0;
 cleanup:
  return rv;
}
