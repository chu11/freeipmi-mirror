/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
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

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"

#define IPMI_SENSORS_SPACE_BUFFER 1024

int
ipmi_sensors_output_event_message_list (ipmi_sensors_state_data_t *state_data,
                                        int event_message_output_type,
                                        uint16_t sensor_event_bitmask,
                                        char **event_message_list,
                                        unsigned int event_message_list_len,
                                        char *prefix,
                                        unsigned int each_on_newline)
{
  char spcbuf[IPMI_SENSORS_SPACE_BUFFER + 1];
  unsigned int i;

  assert (state_data);
  assert (IPMI_SENSORS_EVENT_VALID (event_message_output_type));

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

  if (event_message_output_type == IPMI_SENSORS_EVENT_NA)
    {
      if (state_data->prog_data->args->legacy_output)
        pstdout_printf (state_data->pstate,
                        "[%s]\n",
                        IPMI_SENSORS_NA_STRING_OUTPUT);
      else if (state_data->prog_data->args->ipmimonitoring_legacy_output)
        pstdout_printf (state_data->pstate,
                        "%s\n",
                        IPMIMONITORING_NA_STRING_LEGACY);
      else
        pstdout_printf (state_data->pstate,
                        "%s\n",
                        IPMI_SENSORS_NA_STRING_OUTPUT);
    }
  else if (event_message_output_type == IPMI_SENSORS_EVENT_UNKNOWN)
    {
      if (state_data->prog_data->args->legacy_output)
        pstdout_printf (state_data->pstate,
                        "[%s]\n",
                        "Unknown");
      else if (state_data->prog_data->args->ipmimonitoring_legacy_output)
        pstdout_printf (state_data->pstate,
                        "'%s'\n",
                        IPMIMONITORING_NA_STRING_LEGACY);
      else
        pstdout_printf (state_data->pstate,
                        "%s\n",
                        "Unknown");
    }
  else
    {
      if (state_data->prog_data->args->legacy_output)
        pstdout_printf (state_data->pstate,
                        "[%s]",
                        event_message_list[0]);
      else if (state_data->prog_data->args->ipmimonitoring_legacy_output)
        pstdout_printf (state_data->pstate,
                        "'%s'",
                        event_message_list[0]);
      else if (state_data->prog_data->args->output_event_bitmask)
        pstdout_printf (state_data->pstate,
                        "%04Xh",
                        sensor_event_bitmask);
      else
        pstdout_printf (state_data->pstate,
                        "'%s'",
                        event_message_list[0]);

      if (event_message_list_len > 1)
        {
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
              else if (state_data->prog_data->args->ipmimonitoring_legacy_output)
                pstdout_printf (state_data->pstate,
                                " '%s'",
                                event_message_list[i]);
              else if (prefix)
                pstdout_printf (state_data->pstate,
                                "%s'%s'",
                                prefix,
                                event_message_list[i]);
              else
                pstdout_printf (state_data->pstate,
                                " '%s'",
                                event_message_list[i]);
            }
        }
      
      pstdout_printf (state_data->pstate,
                      "\n");
    }

  return (0);
}

/* emulate a call to ipmi_cmd_get_sensor_thresholds succeeding by
 * stuffing the response with data from the SDR
 */
static int
_get_sdr_sensor_thresholds (ipmi_sensors_state_data_t *state_data,
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

  if (ipmi_sdr_parse_threshold_readable (state_data->sdr_ctx,
					 NULL,
					 0,
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
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
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

  if (ipmi_sdr_parse_thresholds_raw (state_data->sdr_ctx,
				     NULL,
				     0,
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
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
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
  uint8_t threshold_raw;
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
  if (ipmi_sdr_parse_sensor_capabilities (state_data->sdr_ctx,
					  NULL,
					  0,
                                          NULL,
                                          &threshold_access_support,
                                          NULL,
                                          NULL,
                                          NULL) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_capabilities: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  /* threshold access support is very confusing.  From the spec:
   *
   * Sensor Threshold Access Support
   * [3:2] - 00b = no thresholds.
   *    01b = thresholds are readable, per Reading Mask, below.
   *    10b = thresholds are readable and settable per Reading Mask and
   *          Settable Threshold Mask, respectively.
   *    11b = Fixed, unreadable, thresholds. Which thresholds are supported is
   *          reflected by the Reading Mask. The threshold value fields report
   *          the values that hard-coded in the sensor.
   *
   * The 11b case is very hard to interpret.  Right now, the code
   * assumes the this means that the thresholds are not readable from
   * the "Get Sensor Threshold" command.  They are only readable from
   * the SDR.  So that's what the code below assumes.
   */

  if (threshold_access_support == IPMI_SDR_NO_THRESHOLDS_SUPPORT)
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

  if (ipmi_sdr_parse_sensor_number (state_data->sdr_ctx,
				    NULL,
				    0,
                                    &sensor_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_sdr_parse_sensor_number: %s\n",
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (ipmi_sdr_parse_sensor_decoding_data (state_data->sdr_ctx,
                                           NULL,
					   0,
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
                       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  /* if the sensor is not analog, this is most likely a bug in the
   * SDR, since we shouldn't be decoding a non-threshold sensor.
   *
   * Don't return an error.  Allow code to output "NA" or something.
   */
  if (!IPMI_SDR_ANALOG_DATA_FORMAT_VALID (analog_data_format))
    {
      if (state_data->prog_data->args->common_args.debug)
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
      if (state_data->prog_data->args->common_args.debug)
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


  if (threshold_access_support == IPMI_SDR_FIXED_UNREADABLE_THRESHOLDS_SUPPORT)
    {
      if (_get_sdr_sensor_thresholds (state_data, obj_cmd_rs) < 0)
	goto cleanup;

      goto continue_get_sensor_thresholds;
    }

  if (ipmi_cmd_get_sensor_thresholds (state_data->ipmi_ctx,
                                      sensor_number,
                                      obj_cmd_rs) < 0)
    {
      if (state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "ipmi_cmd_get_sensor_thresholds: %s\n",
                         ipmi_ctx_errormsg (state_data->ipmi_ctx));
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
          && (ipmi_check_completion_code (obj_cmd_rs,
                                          IPMI_COMP_CODE_COMMAND_ILLEGAL_FOR_SENSOR_OR_RECORD_TYPE) == 1
              || ipmi_check_completion_code (obj_cmd_rs,
                                             IPMI_COMP_CODE_REQUESTED_SENSOR_DATA_OR_RECORD_NOT_PRESENT) == 1))
        {
          /* The thresholds cannot be gathered for one reason or
           * another, maybe b/c its a OEM sensor or something.  We can
           * return (0) gracefully.
           */
          rv = 0;
          goto cleanup;
        }

      /* Likely error from failed bridge or something similar.  Fall through to SDR */
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
	  && (ipmi_check_completion_code (obj_cmd_rs,
					  IPMI_COMP_CODE_PARAMETER_OUT_OF_RANGE) == 1
	      || ipmi_check_completion_code (obj_cmd_rs,
					     IPMI_COMP_CODE_UNSPECIFIED_ERROR) == 1))
	{
          if (state_data->prog_data->args->common_args.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "Get Sensor Thresholds failed, using SDR information\n");
	  
          if (_get_sdr_sensor_thresholds (state_data, obj_cmd_rs) < 0)
            goto cleanup;
	  
          goto continue_get_sensor_thresholds;
	}

      /* IPMI Workaround 
       *
       * HP DL 585
       *
       * Get Sensor Thresholds is an optional IPMI command.  If it's
       * not supported, use the SDR information.
       *
       * Vadatech VT001 BMC w/ ATCA blades
       *
       * Similar to HP but w/ Parameter out of Range error
       */
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_OR_UNSUPPORTED
          && (ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_INVALID_COMMAND) == 1
	      || ipmi_check_completion_code (obj_cmd_rs, IPMI_COMP_CODE_PARAMETER_OUT_OF_RANGE) == 1))
	{
          if (state_data->prog_data->args->common_args.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "Get Sensor Thresholds failed, using SDR information\n");

          if (_get_sdr_sensor_thresholds (state_data, obj_cmd_rs) < 0)
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
          threshold_raw = val;

          if (ipmi_sensor_decode_value (r_exponent,
                                        b_exponent,
                                        m,
                                        b,
                                        linearization,
                                        analog_data_format,
                                        threshold_raw,
                                        &threshold) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sensor_decode_value: %s\n",
                               strerror (errno));
              goto cleanup;
            }

          if (!(tmp_lower_non_critical_threshold = (double *)malloc (sizeof (double))))
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
          threshold_raw = val;

          if (ipmi_sensor_decode_value (r_exponent,
                                        b_exponent,
                                        m,
                                        b,
                                        linearization,
                                        analog_data_format,
                                        threshold_raw,
                                        &threshold) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sensor_decode_value: %s\n",
                               strerror (errno));
              goto cleanup;
            }

          if (!(tmp_lower_critical_threshold = (double *)malloc (sizeof (double))))
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
          threshold_raw = val;

          if (ipmi_sensor_decode_value (r_exponent,
                                        b_exponent,
                                        m,
                                        b,
                                        linearization,
                                        analog_data_format,
                                        threshold_raw,
                                        &threshold) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sensor_decode_value: %s\n",
                               strerror (errno));
              goto cleanup;
            }

          if (!(tmp_lower_non_recoverable_threshold = (double *)malloc (sizeof (double))))
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
          threshold_raw = val;

          if (ipmi_sensor_decode_value (r_exponent,
                                        b_exponent,
                                        m,
                                        b,
                                        linearization,
                                        analog_data_format,
                                        threshold_raw,
                                        &threshold) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sensor_decode_value: %s\n",
                               strerror (errno));
              goto cleanup;
            }

          if (!(tmp_upper_non_critical_threshold = (double *)malloc (sizeof (double))))
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
          threshold_raw = val;

          if (ipmi_sensor_decode_value (r_exponent,
                                        b_exponent,
                                        m,
                                        b,
                                        linearization,
                                        analog_data_format,
                                        threshold_raw,
                                        &threshold) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sensor_decode_value: %s\n",
                               strerror (errno));
              goto cleanup;
            }

          if (!(tmp_upper_critical_threshold = (double *)malloc (sizeof (double))))
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
          threshold_raw = val;

          if (ipmi_sensor_decode_value (r_exponent,
                                        b_exponent,
                                        m,
                                        b,
                                        linearization,
                                        analog_data_format,
                                        threshold_raw,
                                        &threshold) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sensor_decode_value: %s\n",
                               strerror (errno));
              goto cleanup;
            }

          if (!(tmp_upper_non_recoverable_threshold = (double *)malloc (sizeof (double))))
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
      free (tmp_lower_non_critical_threshold);
      free (tmp_lower_critical_threshold);
      free (tmp_lower_non_recoverable_threshold);
      free (tmp_upper_non_critical_threshold);
      free (tmp_upper_critical_threshold);
      free (tmp_upper_non_recoverable_threshold);
    }
  return (rv);
}

int
ipmi_sensors_get_sensor_state (ipmi_sensors_state_data_t *state_data,
                               int event_message_output_type,
                               uint16_t sensor_event_bitmask,
                               char **sensor_state_str)
{
  assert (state_data);
  assert (state_data->prog_data->args->output_sensor_state);
  assert (IPMI_SENSORS_EVENT_VALID (event_message_output_type));
  assert (sensor_state_str);

  if (event_message_output_type == IPMI_SENSORS_EVENT_NORMAL)
    {
      uint8_t sensor_type;
      uint8_t event_reading_type_code;
      unsigned int sensor_state;

      if (ipmi_sdr_parse_sensor_type (state_data->sdr_ctx,
				      NULL,
				      0,
                                      &sensor_type) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_sdr_parse_sensor_type: %s\n",
                           ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
          return (-1);
        }
      
      if (ipmi_sdr_parse_event_reading_type_code (state_data->sdr_ctx,
						  NULL,
						  0,
                                                  &event_reading_type_code) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_sdr_parse_event_reading_type_code: %s\n",
                           ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
          return (-1);
        }
      
      if (ipmi_interpret_sensor (state_data->interpret_ctx,
                                 event_reading_type_code,
                                 sensor_type,
                                 sensor_event_bitmask,
                                 &sensor_state) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_interpret_sensor: %s\n",
                           ipmi_interpret_ctx_errormsg (state_data->interpret_ctx));
          return (-1);
        }
      
      if (sensor_state == IPMI_INTERPRET_STATE_NOMINAL)
        (*sensor_state_str) = "Nominal";
      else if (sensor_state == IPMI_INTERPRET_STATE_WARNING)
        (*sensor_state_str) = "Warning";
      else if (sensor_state == IPMI_INTERPRET_STATE_CRITICAL)
        (*sensor_state_str) = "Critical";
      else
        (*sensor_state_str) = IPMI_SENSORS_NA_STRING;
    }
  else
    (*sensor_state_str) = IPMI_SENSORS_NA_STRING;

  return (0);
}
