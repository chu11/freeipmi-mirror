/*
 * Copyright (C) 2008-2012 FreeIPMI Core Team
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
#include <ctype.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <limits.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"
#include "ipmi-oem-thirdparty.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

/* Common functions for OEM extensions shared between multiple vendors
 * b/c they share a common third party firmware
 */

static int
_thirdparty_get_reservation (ipmi_oem_state_data_t *state_data,
			     uint8_t *reservation_id)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  /* Inventec 5441/5442 OEM
   * Quanta S99Q/Dell FS12-TY OEM
   *
   * Get Reservation Request
   *
   * 0x30 - OEM network function
   * 0x01 - OEM cmd
   *
   * Get Reservation Response 
   *
   * 0x01 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - Reservation ID
   */

  assert (state_data);
  assert (reservation_id);
  assert (IPMI_CMD_OEM_INVENTEC_RESERVED_EXTENDED_CONFIGURATION == IPMI_CMD_OEM_QUANTA_RESERVED_EXTENDED_CONFIGURATION);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ == IPMI_NET_FN_OEM_QUANTA_GENERIC_RQ);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS == IPMI_NET_FN_OEM_QUANTA_GENERIC_RS);

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_RESERVED_EXTENDED_CONFIGURATION;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              1, /* num bytes */
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
                                                   3,
                                                   IPMI_CMD_OEM_INVENTEC_RESERVED_EXTENDED_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;
  
  (*reservation_id) = bytes_rs[2];

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_thirdparty_get_extended_config_value (ipmi_oem_state_data_t *state_data,
					       uint8_t configuration_id,
					       uint8_t attribute_id,
					       uint8_t index,
					       unsigned int value_return_length,
					       uint32_t *value)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  uint8_t reservation_id;
  int rv = -1;

  assert (state_data);
  assert (value_return_length == 1
          || value_return_length == 2
          || value_return_length == 4);
  assert (value);
  assert (IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION == IPMI_CMD_OEM_QUANTA_GET_EXTENDED_CONFIGURATION);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ == IPMI_NET_FN_OEM_QUANTA_GENERIC_RQ);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS == IPMI_NET_FN_OEM_QUANTA_GENERIC_RS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_READ_ALL_BYTES == IPMI_OEM_QUANTA_EXTENDED_CONFIG_READ_ALL_BYTES);

  /* Inventec 5441/5442 OEM
   * Quanta S99Q/Dell FS12-TY OEM
   *
   * Get Extended Configuration Request
   *
   * 0x30 - OEM network function
   * 0x02 - OEM cmd
   * 0x?? - Reservation ID
   * 0x?? - Configuration ID
   * 0x?? - Attribute ID
   * 0x00 - Index 
   * 0x00 - Data Offset - LSB (unused here??)
   * 0x00 = Data Offset - MSB (unused here??)
   * 0xFF - Bytes to read (0xFF = all)
   * 
   * Get Extended Configuration Response
   *
   * 0x02 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - Configuration ID
   * 0x?? - Attribute ID
   * 0x00 - Index
   * 0x?? - number of bytes returned
   * bytes ...
   */

  if (_thirdparty_get_reservation (state_data, &reservation_id) < 0)
    goto cleanup;

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION;
  bytes_rq[1] = reservation_id;
  bytes_rq[2] = configuration_id;
  bytes_rq[3] = attribute_id;
  bytes_rq[4] = index;
  bytes_rq[5] = 0x00;
  bytes_rq[6] = 0x00;
  bytes_rq[7] = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_READ_ALL_BYTES;
  
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              8, /* num bytes */
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
                                                   6 + value_return_length,
                                                   IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  (*value) = 0;
  if (value_return_length == 1)
    (*value) = bytes_rs[6];
  else if (value_return_length == 2)
    {
      (*value) = bytes_rs[6];
      (*value) |= (bytes_rs[7] << 8);
    }
  else
    {
      (*value) = bytes_rs[6];
      (*value) |= (bytes_rs[7] << 8);
      (*value) |= (bytes_rs[8] << 16);
      (*value) |= (bytes_rs[9] << 24);
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_thirdparty_get_extended_config_string (ipmi_oem_state_data_t *state_data,
						uint8_t configuration_id,
						uint8_t attribute_id,
						uint8_t index,
						char *buf,
						unsigned int buflen)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  uint8_t reservation_id;
  int rv = -1;

  assert (state_data);
  assert (buf);
  assert (buflen);
  assert (IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION == IPMI_CMD_OEM_QUANTA_GET_EXTENDED_CONFIGURATION);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ == IPMI_NET_FN_OEM_QUANTA_GENERIC_RQ);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS == IPMI_NET_FN_OEM_QUANTA_GENERIC_RS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_READ_ALL_BYTES == IPMI_OEM_QUANTA_EXTENDED_CONFIG_READ_ALL_BYTES);

  /* Inventec 5441/5442 OEM
   * Quanta S99Q/Dell FS12-TY OEM
   *
   * Get Extended Configuration Request
   *
   * 0x30 - OEM network function
   * 0x02 - OEM cmd
   * 0x?? - Reservation ID
   * 0x?? - Configuration ID
   * 0x?? - Attribute ID
   * 0x00 - Index
   * 0x00 - Data Offset - LSB (unused here??)
   * 0x00 = Data Offset - MSB (unused here??)
   * 0xFF - Bytes to read (0xFF = all)
   * 
   * Get Extended Configuration Response
   *
   * 0x02 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - Configuration ID
   * 0x?? - Attribute ID
   * 0x00 - Index
   * 0x?? - number of bytes returned
   * bytes ...
   */

  if (_thirdparty_get_reservation (state_data, &reservation_id) < 0)
    goto cleanup;

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION;
  bytes_rq[1] = reservation_id;
  bytes_rq[2] = configuration_id;
  bytes_rq[3] = attribute_id;
  bytes_rq[4] = index;
  bytes_rq[5] = 0x00;
  bytes_rq[6] = 0x00;
  bytes_rq[7] = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_READ_ALL_BYTES;
  
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              8, /* num bytes */
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
                                                   6,
                                                   IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  memset (buf, '\0', buflen);
  if ((rs_len - 6) > 0)
    {
      uint8_t len;

      /* According to docs - all strings are stored as P-strings */
      
      len = bytes_rs[6];
      
      if (len != (rs_len - 7))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "P-string length returned invalid: len = %u, rs_len = %u\n",
                           len, rs_len);
          goto cleanup;
        }

      if ((rs_len - 7) > buflen)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "internal buffer overflow: rs_len = %u, buflen = %u\n",
                           rs_len, buflen);
          goto cleanup;
        }
      memcpy (buf, &bytes_rs[7], rs_len - 7);
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_thirdparty_set_extended_config_value (ipmi_oem_state_data_t *state_data,
					       uint8_t configuration_id,
					       uint8_t attribute_id,
					       uint8_t index,
					       unsigned int value_length,
					       uint32_t value)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  uint8_t reservation_id;
  int rv = -1;

  assert (state_data);
  assert (value_length == 1
          || value_length == 2
          || value_length == 4);
  assert (IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION == IPMI_CMD_OEM_QUANTA_SET_EXTENDED_CONFIGURATION);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ == IPMI_NET_FN_OEM_QUANTA_GENERIC_RQ);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS == IPMI_NET_FN_OEM_QUANTA_GENERIC_RS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_READ_ALL_BYTES == IPMI_OEM_QUANTA_EXTENDED_CONFIG_READ_ALL_BYTES);

  /* Inventec 5441/5442 OEM
   * Quanta S99Q/Dell FS12-TY OEM
   *
   * Set Extended Configuration Request
   *
   * 0x30 - OEM network function
   * 0x03 - OEM cmd
   * 0x?? - Reservation ID
   * 0x?? - Configuration ID
   * 0x?? - Attribute ID
   * 0x00 - Index
   * 0x00 - Data Offset - LSB (unused here??)
   * 0x00 = Data Offset - MSB (unused here??)
   * 0x01 - In progress bit (0x00 in progress, 0x01 - last config in this request)
   * bytes ... 
   * 
   * Set Extended Configuration Response
   *
   * 0x03 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - bytes written
   */

  if (_thirdparty_get_reservation (state_data, &reservation_id) < 0)
    goto cleanup;

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION;
  bytes_rq[1] = reservation_id;
  bytes_rq[2] = configuration_id;
  bytes_rq[3] = attribute_id;
  bytes_rq[4] = index;
  bytes_rq[5] = 0x00;
  bytes_rq[6] = 0x00;
  bytes_rq[7] = 0x01;

  if (value_length == 1)
    bytes_rq[8] = (value & 0x000000FF);
  else if (value_length == 2)
    {
      bytes_rq[8] = (value & 0x000000FF);
      bytes_rq[9] = (value & 0x0000FF00) >> 8;
    }
  else
    {
      bytes_rq[8] = (value & 0x000000FF);
      bytes_rq[9] = (value & 0x0000FF00) >> 8;
      bytes_rq[10] = (value & 0x00FF0000) >> 16;
      bytes_rq[11] = (value & 0xFF000000) >> 24;
    }

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              8 + value_length, /* num bytes */
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
                                                   2,
                                                   IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_thirdparty_set_extended_config_string (ipmi_oem_state_data_t *state_data,
						uint8_t configuration_id,
						uint8_t attribute_id,
						uint8_t index,
						char *buf,
						unsigned int buflen)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  uint8_t reservation_id;
  int rv = -1;

  assert (state_data);
  assert (buf);
  assert (IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION == IPMI_CMD_OEM_QUANTA_SET_EXTENDED_CONFIGURATION);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ == IPMI_NET_FN_OEM_QUANTA_GENERIC_RQ);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS == IPMI_NET_FN_OEM_QUANTA_GENERIC_RS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_READ_ALL_BYTES == IPMI_OEM_QUANTA_EXTENDED_CONFIG_READ_ALL_BYTES);

  /* Inventec 5441/5442 OEM
   * Quanta S99Q/Dell FS12-TY OEM
   *
   * Set Extended Configuration Request
   *
   * 0x30 - OEM network function
   * 0x03 - OEM cmd
   * 0x?? - Reservation ID
   * 0x?? - Configuration ID
   * 0x?? - Attribute ID
   * 0x00 - Index
   * 0x00 - Data Offset - LSB (unused here??)
   * 0x00 = Data Offset - MSB (unused here??)
   * 0x01 - In progress bit (0x00 in progress, 0x01 - last config in this request)
   * bytes ... 
   * 
   * Set Extended Configuration Response
   *
   * 0x03 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - bytes written
   */

  if (_thirdparty_get_reservation (state_data, &reservation_id) < 0)
    goto cleanup;

  bytes_rq[0] = IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION;
  bytes_rq[1] = reservation_id;
  bytes_rq[2] = configuration_id;
  bytes_rq[3] = attribute_id;
  bytes_rq[4] = index;
  bytes_rq[5] = 0x00;
  bytes_rq[6] = 0x00;
  bytes_rq[7] = 0x01;
  bytes_rq[8] = strlen (buf);

  if (buflen)
    memcpy (&bytes_rq[9], buf, buflen);
  
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              9 + buflen, /* num bytes */
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
                                                   2,
                                                   IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}
