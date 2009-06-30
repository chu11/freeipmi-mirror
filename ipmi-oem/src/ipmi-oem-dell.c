/*
  Copyright (C) 2008-2009 FreeIPMI Core Team

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
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif  /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"
#include "ipmi-oem-dell.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

/* 256 b/c length is 8 bit field */
#define IPMI_OEM_DELL_MAX_BYTES 256

static int
_get_dell_system_info (ipmi_oem_state_data_t *state_data,
                       uint8_t parameter_selector,
                       char *string,
                       unsigned int string_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  int len;
  int rv = -1;

  assert (state_data);
  assert (string);
  assert (string_len);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_system_info_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_cmd_get_system_info_parameters (state_data->ipmi_ctx,
                                           IPMI_GET_SYSTEM_INFO_PARAMETER,
                                           parameter_selector,
                                           0,
                                           0,
                                           obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_system_info_parameters: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if ((len = fiid_obj_get_data (obj_cmd_rs,
                                "configuration_parameter_data",
                                configuration_parameter_data,
                                IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'configuration_parameter_data': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }

  /*
   * configuration_parameter_data[0] - string length
   * configuration_parameter_data[1-n] - string
   *
   * We ignore the first byte, assume its correct.
   */

  if ((len - 1) > 0)
    {
      if ((len - 1) > string_len)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "internal buffer overflow\n");
          goto cleanup;
        }

      memcpy (string, &(configuration_parameter_data[1]), (len - 1));
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_dell_get_asset_tag (ipmi_oem_state_data_t *state_data)
{
  char asset_tag[IPMI_OEM_DELL_MAX_BYTES+1];
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell OEM
   *
   * From http://linux.dell.com/files/openipmi/ipmitool/
   *
   * Uses Get System Info command, OEM parameter number 0xC4.
   *
   * Parameter data response formatted:
   *
   * 1 byte = length
   * ? bytes = string
   */

  memset (asset_tag, '\0', IPMI_OEM_DELL_MAX_BYTES + 1);

  if (_get_dell_system_info (state_data,
                             0xC4,
                             asset_tag,
                             IPMI_OEM_DELL_MAX_BYTES) < 0)
    goto cleanup;
 
  pstdout_printf (state_data->pstate,
                  "Asset Tag: %s\n",
                  asset_tag);
 
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_service_tag (ipmi_oem_state_data_t *state_data)
{
  char service_tag[IPMI_OEM_DELL_MAX_BYTES+1];
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell OEM
   *
   * From http://linux.dell.com/files/openipmi/ipmitool/
   *
   * Uses Get System Info command, OEM parameter number 0xC5.
   *
   * Parameter data response formatted:
   *
   * 1 byte = length
   * ? bytes = string
   */

  memset (service_tag, '\0', IPMI_OEM_DELL_MAX_BYTES + 1);

  if (_get_dell_system_info (state_data,
                             0xC5,
                             service_tag,
                             IPMI_OEM_DELL_MAX_BYTES) < 0)
    goto cleanup;
 
  pstdout_printf (state_data->pstate,
                  "Service Tag: %s\n",
                  service_tag);
 
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_power_information (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint32_t cumulative_start_time;
  uint32_t cumulative_reading;
  uint32_t peak_start_time;
  uint32_t peak_amp_time;
  uint16_t peak_amp_reading;
  uint32_t peak_watt_time;
  uint16_t peak_watt_reading;
  double cumulative_reading_val;
  double peak_amp_reading_val;
  time_t timetmp;
  struct tm time_tm;
  char time_buf[IPMI_OEM_TIME_BUFLEN + 1];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell OEM
   *
   * From http://linux.dell.com/files/openipmi/ipmitool/
   *
   * 0x30 - OEM network function
   * 0x9c - OEM cmd
   * 0x07 - ??
   * 0x01 - ??
   * 
   * Get NIC Status Response
   *
   * 0x9c - OEM cmd
   * 0x?? - Completion Code
   * bytes 2-5 - cumulative start time
   * bytes 6-9 - cumulative reading
   * bytes 10-13 - peak start time
   * bytes 14-17 - peak amp time
   * bytes 18-21 - peak amp reading
   * bytes 22-25 - peak watt time
   * bytes 26-29 - peak watt reading
   */

  bytes_rq[0] = 0x9c;
  bytes_rq[1] = 0x07;
  bytes_rq[2] = 0x01;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              0x30, /* network function */
                              bytes_rq, /* data */
                              3, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   26,
                                                   0x9c,
                                                   0x30) < 0)
    goto cleanup;

  cumulative_start_time = bytes_rs[2];
  cumulative_start_time |= (bytes_rs[3] << 8);
  cumulative_start_time |= (bytes_rs[4] << 16);
  cumulative_start_time |= (bytes_rs[5] << 24);

  cumulative_reading = bytes_rs[6];
  cumulative_reading |= (bytes_rs[7] << 8);
  cumulative_reading |= (bytes_rs[8] << 16);
  cumulative_reading |= (bytes_rs[9] << 24);

  peak_start_time = bytes_rs[10];
  peak_start_time |= (bytes_rs[11] << 8);
  peak_start_time |= (bytes_rs[12] << 16);
  peak_start_time |= (bytes_rs[13] << 24);

  peak_amp_time = bytes_rs[14];
  peak_amp_time |= (bytes_rs[15] << 8);
  peak_amp_time |= (bytes_rs[16] << 16);
  peak_amp_time |= (bytes_rs[17] << 24);

  peak_amp_reading = bytes_rs[18];
  peak_amp_reading |= (bytes_rs[19] << 8);

  peak_watt_time = bytes_rs[20];
  peak_watt_time |= (bytes_rs[21] << 8);
  peak_watt_time |= (bytes_rs[22] << 16);
  peak_watt_time |= (bytes_rs[23] << 24);

  peak_watt_reading = bytes_rs[24];
  peak_watt_reading |= (bytes_rs[25] << 8);

  cumulative_reading_val = ((double)cumulative_reading) / 1000.0;

  timetmp = cumulative_start_time;
  localtime_r (&timetmp, &time_tm);
  memset (time_buf, '\0', IPMI_OEM_TIME_BUFLEN + 1);
  strftime (time_buf, IPMI_OEM_TIME_BUFLEN, "%D - %T", &time_tm);

  pstdout_printf (state_data->pstate,
                  "Cumulative Energy Start Time : %s\n",
                  time_buf);

  pstdout_printf (state_data->pstate,
                  "Cumulative Energy            : %.2f kWh\n",
                  cumulative_reading_val);

  peak_amp_reading_val = ((double)peak_amp_reading) / 10.0;

  timetmp = peak_amp_time;
  localtime_r (&timetmp, &time_tm);
  memset (time_buf, '\0', IPMI_OEM_TIME_BUFLEN + 1);
  strftime (time_buf, IPMI_OEM_TIME_BUFLEN, "%D - %T", &time_tm);

  pstdout_printf (state_data->pstate,
                  "Peak Amp Time                : %s\n",
                  time_buf);

  pstdout_printf (state_data->pstate,
                  "Peak Amp                     : %.2f A\n",
                  peak_amp_reading);

  timetmp = peak_watt_time;
  localtime_r (&timetmp, &time_tm);
  memset (time_buf, '\0', IPMI_OEM_TIME_BUFLEN + 1);
  strftime (time_buf, IPMI_OEM_TIME_BUFLEN, "%D - %T", &time_tm);

  pstdout_printf (state_data->pstate,
                  "Peak Watt Time               : %s\n",
                  time_buf);

  pstdout_printf (state_data->pstate,
                  "Peak Watt                    : %u W\n",
                  peak_watt_reading);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_reset_cumulative_power (ipmi_oem_state_data_t *state_data)
{
  return (0);
}

int
ipmi_oem_dell_reset_peak_power (ipmi_oem_state_data_t *state_data)
{
  return (0);
}
