/*
 * Copyright (C) 2008-2014 FreeIPMI Core Team
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

int
ipmi_oem_thirdparty_get_system_info_block_pstring (ipmi_oem_state_data_t *state_data,
						   uint8_t parameter_selector,
						   char *string,
						   unsigned int string_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  uint8_t set_selector = 0;
  uint8_t string_length = 0;
  unsigned int string_count = 0;
  uint8_t string_encoding;
  int len;
  int rv = -1;

  assert (state_data);
  assert (string);
  assert (string_len);
  assert (IPMI_OEM_DELL_SYSTEM_INFO_STRING_ENCODING_BITMASK == IPMI_OEM_WISTRON_SYSTEM_INFO_STRING_ENCODING_BITMASK);
  assert (IPMI_OEM_DELL_SYSTEM_INFO_STRING_ENCODING_SHIFT == IPMI_OEM_WISTRON_SYSTEM_INFO_STRING_ENCODING_SHIFT);
  assert (IPMI_SYSTEM_INFO_ENCODING_ASCII_LATIN1 == IPMI_OEM_WISTRON_SYSTEM_INFO_STRING_ENCODING_PRINTABLE_ASCII);

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
                                           set_selector,
                                           IPMI_SYSTEM_INFO_PARAMETERS_NO_BLOCK_SELECTOR,
                                           obj_cmd_rs) < 0)
    {
      if ((ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
	   && ipmi_check_completion_code (obj_cmd_rs,
					  IPMI_COMP_CODE_GET_SYSTEM_INFO_PARAMETERS_PARAMETER_NOT_SUPPORTED) == 1)
	  || (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_OR_UNSUPPORTED
	      && ipmi_check_completion_code (obj_cmd_rs,
					     IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST) == 1))
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "%s:%s '%s' option not supported on this system\n",
			   state_data->prog_data->args->oem_id,
			   state_data->prog_data->args->oem_command,
			   state_data->prog_data->args->oem_options[0]);
	  goto cleanup;
	}
      
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

  if (len < 3)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_system_info_parameters: invalid buffer length returned: %d\n",
                       len);
      goto cleanup;
    }

  /* configuration_parameter_data[0] is the set selector, we don't care */

  string_encoding = (configuration_parameter_data[1] & IPMI_OEM_DELL_SYSTEM_INFO_STRING_ENCODING_BITMASK);
  string_encoding >>= IPMI_OEM_DELL_SYSTEM_INFO_STRING_ENCODING_SHIFT;

  if (string_encoding != IPMI_SYSTEM_INFO_ENCODING_ASCII_LATIN1)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Cannot handle non-ASCII encoding: %Xh\n",
                       configuration_parameter_data[0]);
      goto cleanup;
    }

  string_length = configuration_parameter_data[2];

  if (!string_length)
    goto out;

  /* -3 b/c of set selector, encoding, and string length bytes */

  if (len - 3)
    {
      if ((len - 3) > (string_len - string_count))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "internal buffer overflow\n");
          goto cleanup;
        }

      memcpy (string + string_count,
              &(configuration_parameter_data[3]),
              (len - 3));
      string_count += (len - 3);
    }

  /* string_length is 8 bits, so we should not call >= 17 times,
   *
   * ceiling ( (255 - 14) / 16 ) + 1 = 17
   *
   */

  set_selector++;
  while (string_count < string_length && set_selector < 17)
    {
      if (fiid_obj_clear (obj_cmd_rs) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_clear: %s\n",
                           fiid_obj_errormsg (obj_cmd_rs));
          goto cleanup;
        }
      
      if (ipmi_cmd_get_system_info_parameters (state_data->ipmi_ctx,
                                               IPMI_GET_SYSTEM_INFO_PARAMETER,
                                               parameter_selector,
                                               set_selector,
                                               IPMI_SYSTEM_INFO_PARAMETERS_NO_BLOCK_SELECTOR,
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
      
      if (len < 2)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_cmd_get_system_info_parameters: invalid buffer length returned: %d\n",
                           len);
          goto cleanup;
        }
      
      /* configuration_parameter_data[0] is the set selector, we don't care */

      if ((string_count + (len - 1)) > (string_len - string_count))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "internal buffer overflow\n");
          goto cleanup;
        }
      
      memcpy (string + string_count,
              &(configuration_parameter_data[1]),
              (len - 1));
      
      string_count += (len - 1);
      
      set_selector++;
    }

 out:
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
_thirdparty_get_reservation (ipmi_oem_state_data_t *state_data,
			     uint8_t *reservation_id)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  /* Inventec 5441/Dell Xanadu II OEM
   * Inventec 5442/Dell Xanadu III OEM
   * Quanta S99Q/Dell FS12-TY OEM
   * Wistron/Dell Poweredge C6220
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
  assert (IPMI_CMD_OEM_INVENTEC_RESERVED_EXTENDED_CONFIGURATION == IPMI_CMD_OEM_WISTRON_RESERVED_EXTENDED_CONFIGURATION);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ == IPMI_NET_FN_OEM_QUANTA_GENERIC_RQ);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ == IPMI_NET_FN_OEM_WISTRON_GENERIC_RQ);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS == IPMI_NET_FN_OEM_QUANTA_GENERIC_RS);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS == IPMI_NET_FN_OEM_WISTRON_GENERIC_RS);

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
          || value_return_length == 3
          || value_return_length == 4);
  assert (value);
  assert (IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION == IPMI_CMD_OEM_QUANTA_GET_EXTENDED_CONFIGURATION);
  assert (IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION == IPMI_CMD_OEM_WISTRON_GET_EXTENDED_CONFIGURATION);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ == IPMI_NET_FN_OEM_QUANTA_GENERIC_RQ);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ == IPMI_NET_FN_OEM_WISTRON_GENERIC_RQ);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS == IPMI_NET_FN_OEM_QUANTA_GENERIC_RS);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS == IPMI_NET_FN_OEM_WISTRON_GENERIC_RS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_READ_ALL_BYTES == IPMI_OEM_QUANTA_EXTENDED_CONFIG_READ_ALL_BYTES);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_READ_ALL_BYTES == IPMI_OEM_WISTRON_EXTENDED_CONFIG_READ_ALL_BYTES);

  /* Inventec 5441/Dell Xanadu II OEM
   * Inventec 5442/Dell Xanadu III OEM
   * Quanta S99Q/Dell FS12-TY OEM
   * Wistron/Dell Poweredge C6220
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
  else if (value_return_length == 3)
    {
      (*value) = bytes_rs[6];
      (*value) |= (bytes_rs[7] << 8);
      (*value) |= (bytes_rs[8] << 16);
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
  assert (IPMI_CMD_OEM_INVENTEC_GET_EXTENDED_CONFIGURATION == IPMI_CMD_OEM_WISTRON_GET_EXTENDED_CONFIGURATION);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ == IPMI_NET_FN_OEM_QUANTA_GENERIC_RQ);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ == IPMI_NET_FN_OEM_WISTRON_GENERIC_RQ);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS == IPMI_NET_FN_OEM_QUANTA_GENERIC_RS);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS == IPMI_NET_FN_OEM_WISTRON_GENERIC_RS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_READ_ALL_BYTES == IPMI_OEM_QUANTA_EXTENDED_CONFIG_READ_ALL_BYTES);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_READ_ALL_BYTES == IPMI_OEM_WISTRON_EXTENDED_CONFIG_READ_ALL_BYTES);

  /* Inventec 5441/Dell Xanadu II OEM
   * Inventec 5442/Dell Xanadu III OEM
   * Quanta S99Q/Dell FS12-TY OEM
   * Wistron/Dell Poweredge C6220
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
          || value_length == 3
          || value_length == 4);
  assert (IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION == IPMI_CMD_OEM_QUANTA_SET_EXTENDED_CONFIGURATION);
  assert (IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION == IPMI_CMD_OEM_WISTRON_SET_EXTENDED_CONFIGURATION);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ == IPMI_NET_FN_OEM_QUANTA_GENERIC_RQ);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ == IPMI_NET_FN_OEM_WISTRON_GENERIC_RQ);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS == IPMI_NET_FN_OEM_QUANTA_GENERIC_RS);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS == IPMI_NET_FN_OEM_WISTRON_GENERIC_RS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_READ_ALL_BYTES == IPMI_OEM_QUANTA_EXTENDED_CONFIG_READ_ALL_BYTES);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_READ_ALL_BYTES == IPMI_OEM_WISTRON_EXTENDED_CONFIG_READ_ALL_BYTES);

  /* Inventec 5441/Dell Xanadu II OEM
   * Inventec 5442/Dell Xanadu III OEM
   * Quanta S99Q/Dell FS12-TY OEM
   * Wistron/Dell Poweredge C6220
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
  else if (value_length == 3)
    {
      bytes_rq[8] = (value & 0x000000FF);
      bytes_rq[9] = (value & 0x0000FF00) >> 8;
      bytes_rq[10] = (value & 0x00FF0000) >> 16;
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
  assert (IPMI_CMD_OEM_INVENTEC_SET_EXTENDED_CONFIGURATION == IPMI_CMD_OEM_WISTRON_SET_EXTENDED_CONFIGURATION);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ == IPMI_NET_FN_OEM_QUANTA_GENERIC_RQ);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RQ == IPMI_NET_FN_OEM_WISTRON_GENERIC_RQ);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS == IPMI_NET_FN_OEM_QUANTA_GENERIC_RS);
  assert (IPMI_NET_FN_OEM_INVENTEC_GENERIC_RS == IPMI_NET_FN_OEM_WISTRON_GENERIC_RS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_READ_ALL_BYTES == IPMI_OEM_QUANTA_EXTENDED_CONFIG_READ_ALL_BYTES);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_READ_ALL_BYTES == IPMI_OEM_WISTRON_EXTENDED_CONFIG_READ_ALL_BYTES);

  /* Inventec 5441/Dell Xanadu II OEM
   * Inventec 5442/Dell Xanadu III OEM
   * Quanta S99Q/Dell FS12-TY OEM
   * Wistron/Dell Poweredge C6220
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

int
ipmi_oem_thirdparty_get_nic_mode (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_NIC == IPMI_OEM_QUANTA_EXTENDED_CONFIGURATION_ID_NIC);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_NIC == IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_NIC);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_NIC_MODE == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_NIC_MODE);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_NIC_MODE == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_NIC_SELECTION);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_NIC_MODE_SHARED == IPMI_OEM_QUANTA_EXTENDED_CONFIG_NIC_MODE_SHARED);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_NIC_MODE_SHARED == IPMI_OEM_WISTRON_EXTENDED_CONFIG_NIC_MODE_SHARED);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_NIC_MODE_DEDICATED == IPMI_OEM_QUANTA_EXTENDED_CONFIG_NIC_MODE_DEDICATED);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_NIC_MODE_DEDICATED == IPMI_OEM_WISTRON_EXTENDED_CONFIG_NIC_MODE_DEDICATED);

  /* Inventec 5441/Dell Xanadu II OEM
   * Inventec 5442/Dell Xanadu III OEM
   * Quanta S99Q/Dell FS12-TY OEM
   * Wistron/Dell Poweredge C6220
   *
   * achu: For Inventec 5441/Dell Xanadu II, Dell appears to have also
   * implemented an additional OEM command that duplicates this
   * configuration.  Currently, we do not implement the Dell
   * equivalent in ipmi-oem.  It is listed below for documentation.
   *
   * Get LAN Source Request
   *
   * 0x34 - OEM network function
   * 0x14 - OEM cmd
   *
   * Get LAN Source Response
   *
   * 0x14 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - LAN Source Setting
   *      - 00h = shared, 01h = dedicated
   */

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_NIC,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_NIC_MODE,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;

  switch (tmpvalue)
    {
    case IPMI_OEM_INVENTEC_EXTENDED_CONFIG_NIC_MODE_SHARED:
      pstdout_printf (state_data->pstate, "shared\n");
      break;
    case IPMI_OEM_INVENTEC_EXTENDED_CONFIG_NIC_MODE_DEDICATED:
      pstdout_printf (state_data->pstate, "dedicated\n");
      break;
    default:
      pstdout_printf (state_data->pstate, "unknown NIC mode: %Xh\n", tmpvalue);
      break;
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_thirdparty_set_nic_mode (ipmi_oem_state_data_t *state_data)
{
  uint8_t mode;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_NIC == IPMI_OEM_QUANTA_EXTENDED_CONFIGURATION_ID_NIC);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_NIC == IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_NIC);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_NIC_MODE == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_NIC_MODE);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_NIC_MODE == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_NIC_SELECTION);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_NIC_MODE_SHARED == IPMI_OEM_QUANTA_EXTENDED_CONFIG_NIC_MODE_SHARED);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_NIC_MODE_SHARED == IPMI_OEM_WISTRON_EXTENDED_CONFIG_NIC_MODE_SHARED);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_NIC_MODE_DEDICATED == IPMI_OEM_QUANTA_EXTENDED_CONFIG_NIC_MODE_DEDICATED);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_NIC_MODE_DEDICATED == IPMI_OEM_WISTRON_EXTENDED_CONFIG_NIC_MODE_DEDICATED);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "shared")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "dedicated"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  /* Inventec 5441/Dell Xanadu II OEM
   * Inventec 5442/Dell Xanadu III OEM
   * Quanta S99Q/Dell FS12-TY OEM
   * Wistron/Dell Poweredge C6220
   *
   * achu: For Inventec 5441/Dell Xanadu II, Dell appears to have also
   * implemented an additional OEM command that duplicates this
   * configuration.  Currently, we do not implement the Dell
   * equivalent in ipmi-oem.  It is listed below for documentation.
   *
   * Set LAN Source Request
   *
   * 0x34 - OEM network function
   * 0x13 - OEM cmd
   * 0x?? - LAN Source
   *      - 00h = shared, 01h = dedicated
   *
   * Set LAN Source Response
   *
   * 0x13 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - LAN Source Setting
   */

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "shared"))
    mode = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_NIC_MODE_SHARED;
  else
    mode = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_NIC_MODE_DEDICATED;

  if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_NIC,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_NIC_MODE,
						     0,
						     1,
						     (uint32_t)mode) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_thirdparty_get_bmc_services_bitmask (ipmi_oem_state_data_t *state_data,
					      uint8_t *services)
{
  uint32_t tmpvalue;
  int rv = -1;

  assert (state_data);
  assert (services);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SECURITY == IPMI_OEM_QUANTA_EXTENDED_CONFIGURATION_ID_SECURITY);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SECURITY == IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_SECURITY);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SECURITY_SERVICE_DISABLED == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_SECURITY_SERVICE_DISABLED);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SECURITY_SERVICE_DISABLED == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SECURITY_SERVICE_DISABLED);

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SECURITY,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SECURITY_SERVICE_DISABLED,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;

  (*services) = tmpvalue;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_thirdparty_get_bmc_services_v1 (ipmi_oem_state_data_t *state_data)
{
  uint8_t services = 0;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL == IPMI_OEM_QUANTA_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM_VIRTUAL_STORAGE == IPMI_OEM_QUANTA_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM_VIRTUAL_STORAGE);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP_HTTPS == IPMI_OEM_QUANTA_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP_HTTPS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH_TELNET == IPMI_OEM_QUANTA_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH_TELNET);

  if (ipmi_oem_thirdparty_get_bmc_services_bitmask (state_data, &services) < 0)
    goto cleanup;

  if (services)
    {
      /* achu: it is not clear if only one bit or multiple bits can be
       * set.  I'm assuming if the "all" bit is set, there is no need
       * to output anything else.
       */
      if (services & IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL)
        {
          pstdout_printf (state_data->pstate, "All services except IPMI disabled\n");
          goto out;
        }
      if (services & IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM_VIRTUAL_STORAGE)
        pstdout_printf (state_data->pstate, "KVM/Virtual Storage disabled\n");
      if (services & IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP_HTTPS)
        pstdout_printf (state_data->pstate, "HTTP/HTTPS disabled\n");
      if (services & IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH_TELNET)
        pstdout_printf (state_data->pstate, "SSH/Telnet disabled\n");
    }
  else
    pstdout_printf (state_data->pstate, "All services enabled\n");

 out:
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_thirdparty_set_bmc_services_v1 (ipmi_oem_state_data_t *state_data)
{
  int enable = 0;
  uint8_t services = 0;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 2);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_ENABLE_ALL == IPMI_OEM_QUANTA_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_ENABLE_ALL);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL == IPMI_OEM_QUANTA_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM_VIRTUAL_STORAGE == IPMI_OEM_QUANTA_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM_VIRTUAL_STORAGE);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP_HTTPS == IPMI_OEM_QUANTA_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP_HTTPS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH_TELNET == IPMI_OEM_QUANTA_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH_TELNET);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SECURITY == IPMI_OEM_QUANTA_EXTENDED_CONFIGURATION_ID_SECURITY);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SECURITY_SERVICE_DISABLED == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_SECURITY_SERVICE_DISABLED);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "enable")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "disable"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  if (strcasecmp (state_data->prog_data->args->oem_options[1], "all")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "kvm")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "http")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "ssh"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "enable"))
    enable = 1;
        
  /* if all, it's an easy special case */
  if (!strcasecmp (state_data->prog_data->args->oem_options[1], "all"))
    {
      if (enable)
        services = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_ENABLE_ALL;
      else
        services = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL;
    }
  else
    {
      if (ipmi_oem_thirdparty_get_bmc_services_bitmask (state_data, &services) < 0)
        goto cleanup;

      if (enable && (services & IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL))
        {
          /* clear out "all" bit, and replace with remaining bits */
          services &= (~IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_ALL);
          services |= IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM_VIRTUAL_STORAGE;
          services |= IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP_HTTPS;
          services |= IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH_TELNET;
        }

      if (!strcasecmp (state_data->prog_data->args->oem_options[1], "kvm"))
        {
          if (enable)
            services &= (~IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM_VIRTUAL_STORAGE);
          else
            services |= IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_KVM_VIRTUAL_STORAGE;
        }
      else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "http"))
        {
          if (enable)
            services &= (~IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP_HTTPS);
          else
            services |= IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_HTTP_HTTPS;
        }
      else /* !strcasecmp (state_data->prog_data->args->oem_options[1], "ssh") */
        {
          if (enable)
            services &= (~IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH_TELNET);
          else
            services |= IPMI_OEM_INVENTEC_EXTENDED_CONFIG_SECURITY_SERVICES_DISABLED_BITMASK_SSH_TELNET;
        }
    }

  if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SECURITY,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SECURITY_SERVICE_DISABLED,
						     0,
						     1,
						     (uint32_t)services) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_thirdparty_get_account_status (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t get_user_access_obj_cmd_rs = NULL;
  fiid_obj_t get_user_name_obj_cmd_rs = NULL;
  uint32_t tmpvalue;
  uint8_t lan_channel_number;
  uint8_t number_of_users;
  uint64_t val;
  unsigned int i;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_USER_NAME == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_USER_NAME);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_USER_NAME == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_USER_NAME);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_ACCOUNT_STATUS == IPMI_OEM_QUANTA_EXTENDED_CONFIGURATION_ID_ACCOUNT_STATUS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_ACCOUNT_STATUS == IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_ACCOUNT_STATUS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_ACCOUNT_STATUS == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_ACCOUNT_STATUS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_ACCOUNT_STATUS == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_ACCOUNT_STATUS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_UNSPECIFIED == IPMI_OEM_QUANTA_EXTENDED_CONFIG_ACCOUNT_STATUS_UNSPECIFIED);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_UNSPECIFIED == IPMI_OEM_WISTRON_EXTENDED_CONFIG_ACCOUNT_STATUS_UNSPECIFIED);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_ENABLED == IPMI_OEM_QUANTA_EXTENDED_CONFIG_ACCOUNT_STATUS_ENABLED);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_ENABLED == IPMI_OEM_WISTRON_EXTENDED_CONFIG_ACCOUNT_STATUS_ENABLED);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_DISABLED == IPMI_OEM_QUANTA_EXTENDED_CONFIG_ACCOUNT_STATUS_DISABLED);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_DISABLED == IPMI_OEM_WISTRON_EXTENDED_CONFIG_ACCOUNT_STATUS_DISABLED);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_LOCKOUT == IPMI_OEM_QUANTA_EXTENDED_CONFIG_ACCOUNT_STATUS_LOCKOUT);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_LOCKOUT == IPMI_OEM_WISTRON_EXTENDED_CONFIG_ACCOUNT_STATUS_LOCKOUT);

  /* achu:
   *
   * IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_NUMBER_OF_USER
   * returns the number of enabled/disabled users, not the total,
   * which is not what we need in this function.
   */

  if (!(get_user_access_obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_user_access_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (ipmi_get_channel_number (state_data->ipmi_ctx,
                               IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3,
                               &lan_channel_number) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_get_channel_number: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }
  
  if (ipmi_cmd_get_user_access (state_data->ipmi_ctx,
                                lan_channel_number,
                                1, /* user_id number - any will do for this call */
                                get_user_access_obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_user_access: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (FIID_OBJ_GET (get_user_access_obj_cmd_rs,
                    "max_channel_user_ids",
                    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "FIID_OBJ_GET: 'max_channel_user_ids': %s\n",
                       fiid_obj_errormsg (get_user_access_obj_cmd_rs));
      goto cleanup;
    }

  if (!(get_user_name_obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_user_name_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  number_of_users = val;
  
  pstdout_printf (state_data->pstate,
                  "Username         | Status\n");

  for (i = 0; i < number_of_users; i++)
    {
      char user_name[IPMI_MAX_USER_NAME_LENGTH + 1];
      uint8_t account_status;
      char *account_status_str = NULL;

      memset (user_name, '\0', IPMI_MAX_USER_NAME_LENGTH + 1);

      /* achu:
       *
       * Docs say
       * IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_USER_NAME / IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_USER_NAME
       * string should be returned a P-string.  On Inventec
       * 5441/Xanadu II returned as ASCII, on Inventec 5442/Xanadu III
       * strings returned as P-string.  On Quanta S99Q/Dell FS12-TY
       * reports returned as P-string.  Screw all that, use normal
       * IPMI get username function instead.
       */

      if (fiid_obj_clear (get_user_name_obj_cmd_rs) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_clear: %s\n",
                           fiid_obj_errormsg (get_user_name_obj_cmd_rs));
          goto cleanup;
        }
      
      if (ipmi_cmd_get_user_name (state_data->ipmi_ctx,
                                  i + 1,
                                  get_user_name_obj_cmd_rs) < 0)
        {
          /* Username is not set yet */
          if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_OR_UNSUPPORTED
              && (ipmi_check_completion_code (get_user_name_obj_cmd_rs,
                                              IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST) == 1))
            continue;

          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_cmd_get_user_name: %s\n",
                           ipmi_ctx_errormsg (state_data->ipmi_ctx));
          goto cleanup;
        }

      if (fiid_obj_get_data (get_user_name_obj_cmd_rs,
                             "user_name",
                             user_name,
                             IPMI_MAX_USER_NAME_LENGTH) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get_data: 'user_name': %s\n",
                           fiid_obj_errormsg (get_user_name_obj_cmd_rs));
          goto cleanup;
        }
      
      /* if not the first user id - but username is empty, skip output */
      if (i && !strlen (user_name))
        continue;

      if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
							 IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_ACCOUNT_STATUS,
							 IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_ACCOUNT_STATUS_ACCOUNT_STATUS,
							 i + 1,
							 1,
							 &tmpvalue) < 0)
        goto cleanup;
      account_status = tmpvalue;

      switch (account_status)
	{
	case IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_UNSPECIFIED:
	  account_status_str = "Unspecified";
	  break;
	case IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_ENABLED:
	  account_status_str = "Enabled";
	  break;
	case IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_DISABLED:
	  account_status_str = "Disabled";
	  break;
	case IPMI_OEM_INVENTEC_EXTENDED_CONFIG_ACCOUNT_STATUS_LOCKOUT:
	  account_status_str = "Lockout";
	  break;
	default:
	  account_status_str = "Unknown";
	}

      pstdout_printf (state_data->pstate,
                      "%-16s | %s\n",
                      user_name,
                      account_status_str);
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (get_user_access_obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_thirdparty_get_dns_config_v1 (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  uint8_t dnsdhcpenable;
  uint32_t dnsserver1;
  uint32_t dnsserver2;
  uint8_t dnsregisterbmc;
  char dnsbmchostname[IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX + 1];
  uint8_t dnsdomainnamedhcpenable;
  char dnsdomainname[IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX + 1];
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX == IPMI_OEM_QUANTA_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX == IPMI_OEM_QUANTA_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS == IPMI_OEM_QUANTA_EXTENDED_CONFIGURATION_ID_DNS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DHCP_ENABLE == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DHCP_ENABLE);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER1 == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER1);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER2 == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER2);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_REGISTER_BMC == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_DNS_DNS_REGISTER_BMC);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_BMC_HOST_NAME == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_DNS_DNS_BMC_HOST_NAME);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME_DHCP_ENABLE == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME_DHCP_ENABLE);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME);

  memset (dnsbmchostname, '\0', IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX + 1);
  memset (dnsdomainname, '\0', IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX + 1);

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DHCP_ENABLE,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  dnsdhcpenable = tmpvalue;

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER1,
						     0,
						     4,
						     &tmpvalue) < 0)
    goto cleanup;
  dnsserver1 = tmpvalue;

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER2,
						     0,
						     4,
						     &tmpvalue) < 0)
    goto cleanup;
  dnsserver2 = tmpvalue;
  
  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_REGISTER_BMC,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  dnsregisterbmc = tmpvalue;

  if (ipmi_oem_thirdparty_get_extended_config_string (state_data,
						      IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
						      IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_BMC_HOST_NAME,
						      0,
						      dnsbmchostname,
						      IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX) < 0)
    goto cleanup;

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME_DHCP_ENABLE,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  dnsdomainnamedhcpenable = tmpvalue;

  if (ipmi_oem_thirdparty_get_extended_config_string (state_data,
						      IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
						      IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME,
						      0,
						      dnsdomainname,
						      IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX) < 0)
    goto cleanup;

  pstdout_printf (state_data->pstate,
		  "DNS DHCP             : %s\n",
		  (dnsdhcpenable) ? "Enabled" : "Disabled");

  pstdout_printf (state_data->pstate,
                  "DNS Server 1         : %u.%u.%u.%u\n",
                  (dnsserver1 & 0x000000FF),
                  (dnsserver1 & 0x0000FF00) >> 8,
                  (dnsserver1 & 0x00FF0000) >> 16,
                  (dnsserver1 & 0xFF000000) >> 24);

  pstdout_printf (state_data->pstate,
                  "DNS Server 2         : %u.%u.%u.%u\n",
                  (dnsserver2 & 0x000000FF),
                  (dnsserver2 & 0x0000FF00) >> 8,
                  (dnsserver2 & 0x00FF0000) >> 16,
                  (dnsserver2 & 0xFF000000) >> 24);
  
  pstdout_printf (state_data->pstate,
		  "DNS Register BMC     : %s\n",
		  (dnsdomainnamedhcpenable) ? "Enabled" : "Disabled");
  
  pstdout_printf (state_data->pstate,
                  "DNS BMC Host Name    : %s\n",
                  dnsbmchostname);
  
  pstdout_printf (state_data->pstate,
		  "DNS Domain Name DHCP : %s\n",
		  (dnsdomainnamedhcpenable) ? "Enabled" : "Disabled");
  
  pstdout_printf (state_data->pstate,
                  "DNS Domain Name      : %s\n",
                  dnsdomainname);
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_thirdparty_set_dns_config_v1 (ipmi_oem_state_data_t *state_data)
{
  uint8_t dnsdhcpenable = 0;
  uint32_t dnsserver1 = 0;
  uint32_t dnsserver2 = 0;
  uint8_t dnsregisterbmc = 0;
  char dnsbmchostname[IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX + 1];
  uint8_t dnsdomainnamedhcpenable = 0;
  char dnsdomainname[IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX + 1];
  int rv = -1;
  unsigned int i;

  assert (state_data);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX == IPMI_OEM_QUANTA_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX == IPMI_OEM_QUANTA_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS == IPMI_OEM_QUANTA_EXTENDED_CONFIGURATION_ID_DNS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DHCP_ENABLE == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DHCP_ENABLE);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER1 == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER1);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER2 == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER2);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_REGISTER_BMC == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_DNS_DNS_REGISTER_BMC);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_BMC_HOST_NAME == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_DNS_DNS_BMC_HOST_NAME);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME_DHCP_ENABLE == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME_DHCP_ENABLE);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME);

  memset (dnsbmchostname, '\0', IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX + 1);
  memset (dnsdomainname, '\0', IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX + 1);

  if (!state_data->prog_data->args->oem_options_count)
    {
      pstdout_printf (state_data->pstate,
		      "Option: dnsdhcp=enable|disable\n"
                      "Option: dnsserver1=ipaddress\n"
                      "Option: dnsserver2=ipaddress\n"
		      "Option: dnsregisterbmc=enable|disable\n"
                      "Option: dnsbmchostname=string\n"
		      "Option: dnsdomainnamedhcp=enable|disable\n"
                      "Option: dnsdomainname=string\n");
      return (0); 
    }

  for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
                                    i,
                                    &key,
                                    &value) < 0)
        goto cleanup;

      if (!strcasecmp (key, "dnsdhcp"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &dnsdhcpenable) < 0)
            goto cleanup;
          
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
							     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DHCP_ENABLE,
							     0,
							     1,
							     (uint32_t)dnsdhcpenable) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsserver1"))
        {
          if (ipmi_oem_parse_ip_address (state_data, i, value, &dnsserver1) < 0)
            goto cleanup;
          
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
							     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER1,
							     0,
							     4,
							     (uint32_t)dnsserver1) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsserver2"))
        {
          if (ipmi_oem_parse_ip_address (state_data, i, value, &dnsserver2) < 0)
            goto cleanup;
          
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
							     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_SERVER2,
							     0,
							     4,
							     (uint32_t)dnsserver2) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsregisterbmc"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &dnsregisterbmc) < 0)
            goto cleanup;
          
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
							     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_REGISTER_BMC,
							     0,
							     1,
							     (uint32_t)dnsregisterbmc) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsbmchostname"))
        {
          uint8_t string_length = 0;

          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &string_length,
                                     dnsbmchostname,
                                     IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_BMC_HOST_NAME_MAX) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_string (state_data,
							      IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
							      IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_BMC_HOST_NAME,
							      0,
							      dnsbmchostname,
							      (unsigned int)string_length) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsdomainnamedhcp"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &dnsdomainnamedhcpenable) < 0)
            goto cleanup;
          
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
							     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME_DHCP_ENABLE,
							     0,
							     1,
							     (uint32_t)dnsdomainnamedhcpenable) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "dnsdomainname"))
        {
          uint8_t string_length = 0;

          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &string_length,
                                     dnsdomainname,
                                     IPMI_OEM_INVENTEC_EXTENDED_CONFIG_DNS_DNS_DOMAIN_NAME_MAX) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_string (state_data,
							      IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_DNS,
							      IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_DNS_DNS_DOMAIN_NAME,
							      0,
							      dnsdomainname,
							      (unsigned int)string_length) < 0)
            goto cleanup;
        }
      else
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s' : invalid key\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[i]);
          goto cleanup;
        }

      free (key);
      free (value);
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_thirdparty_get_web_server_config_v1 (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  uint8_t webserverenabled;
  uint8_t maxwebsessions;
  uint8_t activewebsessions;
  uint32_t webservertimeout;
  uint16_t httpportnum;
  uint16_t httpsportnum;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION == IPMI_OEM_QUANTA_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION == IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_ENABLED == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_ENABLED);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_ENABLED == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_ENABLED);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_MAX_WEB_SESSIONS == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_MAX_WEB_SESSIONS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_MAX_WEB_SESSIONS == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_MAX_WEB_SESSIONS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_ACTIVE_WEB_SESSIONS == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_ACTIVE_WEB_SESSIONS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_ACTIVE_WEB_SESSIONS == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_ACTIVE_WEB_SESSIONS);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_TIMEOUT == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_TIMEOUT);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_TIMEOUT == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_TIMEOUT);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTP_PORT_NUM == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTP_PORT_NUM);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTP_PORT_NUM == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTP_PORT_NUM);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTPS_PORT_NUM == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTPS_PORT_NUM);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTPS_PORT_NUM == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTPS_PORT_NUM);

  /* Inventec 5441/Dell Xanadu II OEM
   *
   * achu: For Inventec 5441/Dell Xanadu II, Dell appears to have also
   * implemented an additional OEM command that duplicates this
   * configuration.  Currently, we do not implement the Dell
   * equivalent in ipmi-oem.  It is listed below for documentation.
   *
   * achu: The document states "web port" and "http port".  That
   * probably means "http" vs. "https" port.  The below documents this
   * typo.
   *
   * Get Web Port Num Request
   *
   * 0x34 - OEM network function
   * 0x03 - OEM cmd
   *
   * Get Web Port Num Response
   *
   * 0x03 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - web port num (LSB)
   * 0x?? - web port num (MSB)
   * 0x?? - http num (LSB)
   * 0x?? - http num (MSB)
   */

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_ENABLED,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  webserverenabled = tmpvalue;
  
  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_MAX_WEB_SESSIONS,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  maxwebsessions = tmpvalue;
  
  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_ACTIVE_WEB_SESSIONS,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  activewebsessions = tmpvalue;
  
  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_TIMEOUT,
						     0,
						     4,
						     &tmpvalue) < 0)
    goto cleanup;
  webservertimeout = tmpvalue;
  
  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTP_PORT_NUM,
						     0,
						     2,
						     &tmpvalue) < 0)
    goto cleanup;
  httpportnum = tmpvalue;
  
  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTPS_PORT_NUM,
						     0,
						     2,
						     &tmpvalue) < 0)
    goto cleanup;
  httpsportnum = tmpvalue;
  
  pstdout_printf (state_data->pstate,
		  "Web Server          : %s\n",
		  (webserverenabled) ? "Enabled" : "Disabled");
  
  pstdout_printf (state_data->pstate,
		  "Max Web Sessions    : %u\n",
		  maxwebsessions);

  pstdout_printf (state_data->pstate,
		  "Active Web Sessions : %u\n",
		  activewebsessions);

  pstdout_printf (state_data->pstate,
		  "Web Server Timeout  : %u seconds\n",
		  webservertimeout);

  pstdout_printf (state_data->pstate,
		  "http Port Number    : %u\n",
		  httpportnum);

  pstdout_printf (state_data->pstate,
		  "https Port Number   : %u\n",
		  httpsportnum);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_thirdparty_set_web_server_config_v1 (ipmi_oem_state_data_t *state_data)
{
  uint8_t webserverenabled = 0;
  uint32_t webservertimeout = 0;
  uint16_t httpportnumber = 0;
  uint16_t httpsportnumber = 0;
  int rv = -1;
  unsigned int i;

  assert (state_data);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION == IPMI_OEM_QUANTA_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION == IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_ENABLED == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_ENABLED);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_ENABLED == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_ENABLED);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_TIMEOUT == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_TIMEOUT);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_TIMEOUT == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_TIMEOUT);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTP_PORT_NUM == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTP_PORT_NUM);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTP_PORT_NUM == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTP_PORT_NUM);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTPS_PORT_NUM == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTPS_PORT_NUM);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTPS_PORT_NUM == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTPS_PORT_NUM);

  /* Inventec 5441/Dell Xanadu II OEM
   *
   * achu: For Inventec 5441/Dell Xanadu II, Dell appears to have also
   * implemented an additional OEM command that duplicates this
   * configuration.  Currently, we do not implement the Dell
   * equivalent in ipmi-oem.  It is listed below for documentation.
   *
   * achu: The document states "web port" and "http port".  That
   * probably means "http" vs. "https" port.  The below documents this
   * typo.
   *
   * Set Web Port Num Request
   *
   * 0x34 - OEM network function
   * 0x02 - OEM cmd
   * 0x?? - web port num (LSB)
   * 0x?? - web port num (MSB)
   * 0x?? - http num (LSB)
   * 0x?? - http num (MSB)
   *
   * Set Web Port Num Response
   *
   * 0x02 - OEM cmd
   * 0x?? - Completion Code
   */

  if (!state_data->prog_data->args->oem_options_count)
    {
      pstdout_printf (state_data->pstate,
		      "Option: webserver=enable|disable\n"
		      "Option: webservertimeout=seconds\n"
		      "Option: httpportnumber=num\n"
		      "Option: httpsportnumber=num\n");
      return (0); 
    }

  for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
                                    i,
                                    &key,
                                    &value) < 0)
        goto cleanup;

      if (!strcasecmp (key, "webserver"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &webserverenabled) < 0)
            goto cleanup;

          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
							     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_ENABLED,
							     0,
							     1,
							     (uint32_t)webserverenabled) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "webservertimeout"))
        {
          if (ipmi_oem_parse_4_byte_field (state_data, i, value, &webservertimeout) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
							     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_WEB_SERVER_TIMEOUT,
							     0,
							     4,
							     webservertimeout) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "httpportnumber"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &httpportnumber) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
							     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTP_PORT_NUM,
							     0,
							     2,
							     (uint32_t)httpportnumber) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "httpsportnumber"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &httpsportnumber) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_WEB_SERVER_CONFIGURATION,
							     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_WEB_SERVER_CONFIGURATION_HTTPS_PORT_NUM,
							     0,
							     2,
							     (uint32_t)httpsportnumber) < 0)
            goto cleanup;
        }
      else
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s' : invalid key\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[i]);
          goto cleanup;
        }

      free (key);
      free (value);
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_thirdparty_get_power_management_config_v1 (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  uint8_t powermanagementenable;
  uint8_t dpnmpowermanagement;
  uint8_t powerstaggeringacrecovery;
  uint16_t powerondelay;
  uint16_t minpowerondelay;
  uint16_t maxpowerondelay;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT == IPMI_OEM_QUANTA_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_MANAGEMENT_ENABLE == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_MANAGEMENT_ENABLE);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_STAGGERING_AC_RECOVERY == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_STAGGERING_AC_RECOVERY);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_ON_DELAY == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_ON_DELAY);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MINIMUM_POWER_ON_DELAY == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MINIMUM_POWER_ON_DELAY);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MAXIMUM_POWER_ON_DELAY == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MAXIMUM_POWER_ON_DELAY);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_IMMEDIATE == IPMI_OEM_QUANTA_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_IMMEDIATE);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_AUTO == IPMI_OEM_QUANTA_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_AUTO);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_USER_DEFINED == IPMI_OEM_QUANTA_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_USER_DEFINED);

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_MANAGEMENT_ENABLE,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  powermanagementenable = tmpvalue;

  dpnmpowermanagement = (powermanagementenable & IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_MANAGEMENT_ENABLE_DPNM_BITMASK);
  dpnmpowermanagement >>= IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_MANAGEMENT_ENABLE_DPNM_SHIFT;
  
  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_STAGGERING_AC_RECOVERY,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  powerstaggeringacrecovery = tmpvalue;
  
  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_ON_DELAY,
						     0,
						     2,
						     &tmpvalue) < 0)
    goto cleanup;
  powerondelay = tmpvalue;

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MINIMUM_POWER_ON_DELAY,
						     0,
						     2,
						     &tmpvalue) < 0)
    goto cleanup;
  minpowerondelay = tmpvalue; 

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MAXIMUM_POWER_ON_DELAY,
						     0,
						     2,
						     &tmpvalue) < 0)
    goto cleanup;
  maxpowerondelay = tmpvalue; 
  
  pstdout_printf (state_data->pstate,
		  "DPNM Power Management        : %s\n",
		  (dpnmpowermanagement) ? "Enabled" : "Disabled");
  
  if (powerstaggeringacrecovery == IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_IMMEDIATE)
    pstdout_printf (state_data->pstate,
                    "Power Staggering AC Recovery : Immediate\n");
  else if (powerstaggeringacrecovery == IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_AUTO)
    pstdout_printf (state_data->pstate,
                    "Power Staggering AC Recovery : Auto\n");
  else if (powerstaggeringacrecovery == IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_USER_DEFINED)
    pstdout_printf (state_data->pstate,
                    "Power Staggering AC Recovery : User Defined\n");
  else
    pstdout_printf (state_data->pstate,
                    "Power Staggering AC Recovery : %Xh\n",
                    powerstaggeringacrecovery);
  
  pstdout_printf (state_data->pstate,
		  "Power On Delay               : %u seconds\n",
                  powerondelay);
  
  pstdout_printf (state_data->pstate,
		  "Minimum Power On Delay       : %u seconds\n",
                  minpowerondelay);

  pstdout_printf (state_data->pstate,
		  "Maximum Power On Delay       : %u seconds\n",
                  maxpowerondelay);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_thirdparty_set_power_management_config_v1 (ipmi_oem_state_data_t *state_data)
{
  uint8_t powermanagementenable = 0;
  uint8_t dpnmpowermanagement = 0;
  uint8_t powerstaggeringacrecovery = 0;
  uint16_t powerondelay = 0;
  uint16_t maxpowerondelay = 0;
  int rv = -1;
  unsigned int i;

  assert (state_data);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT == IPMI_OEM_QUANTA_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_MANAGEMENT_ENABLE == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_MANAGEMENT_ENABLE);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_STAGGERING_AC_RECOVERY == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_STAGGERING_AC_RECOVERY);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_ON_DELAY == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_ON_DELAY);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MAXIMUM_POWER_ON_DELAY == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MAXIMUM_POWER_ON_DELAY);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_IMMEDIATE == IPMI_OEM_QUANTA_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_IMMEDIATE);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_AUTO == IPMI_OEM_QUANTA_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_AUTO);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_USER_DEFINED == IPMI_OEM_QUANTA_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_USER_DEFINED);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_MANAGEMENT_ENABLE_DPNM_SHIFT == IPMI_OEM_QUANTA_EXTENDED_CONFIG_POWER_MANAGEMENT_ENABLE_DPNM_SHIFT);

  if (!state_data->prog_data->args->oem_options_count)
    {
      pstdout_printf (state_data->pstate,
		      "Option: dpnmpowermanagement=enable|disable\n"
		      "Option: powerstaggeringacrecovery=immediate|auto|user\n"
		      "Option: powerondelay=seconds\n"
		      "Option: maxpowerondelay=seconds\n");
      return (0); 
    }

  for (i = 0; i < state_data->prog_data->args->oem_options_count; i++)
    {
      char *key = NULL;
      char *value = NULL;
      
      if (ipmi_oem_parse_key_value (state_data,
                                    i,
                                    &key,
                                    &value) < 0)
        goto cleanup;

      if (!strcasecmp (key, "dpnmpowermanagement"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &dpnmpowermanagement) < 0)
            goto cleanup;

          powermanagementenable |= (dpnmpowermanagement << IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_MANAGEMENT_ENABLE_DPNM_SHIFT);
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
							     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_MANAGEMENT_ENABLE,
							     0,
							     1,
							     (uint32_t)powermanagementenable) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "powerstaggeringacrecovery"))
        {
          if (strcasecmp (value, "immediate")
              && strcasecmp (value, "auto")
              && strcasecmp (value, "user"))
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "%s:%s invalid OEM option argument '%s' : invalid value\n",
                               state_data->prog_data->args->oem_id,
                               state_data->prog_data->args->oem_command,
                               state_data->prog_data->args->oem_options[i]);
              goto cleanup;
            }

          if (!strcasecmp (value, "immediate"))
            powerstaggeringacrecovery = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_IMMEDIATE;
          else if (!strcasecmp (value, "auto"))
            powerstaggeringacrecovery = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_AUTO;
          else /* !strcasecmp (value, "user")) */
            powerstaggeringacrecovery = IPMI_OEM_INVENTEC_EXTENDED_CONFIG_POWER_STAGGERING_AC_RECOVERY_USER_DEFINED;

          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
							     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_STAGGERING_AC_RECOVERY,
							     0,
							     1,
							     (uint32_t)powerstaggeringacrecovery) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "powerondelay"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &powerondelay) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
							     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_POWER_ON_DELAY,
							     0,
							     2,
							     (uint32_t)powerondelay) < 0)
            goto cleanup;
        }
      else if (!strcasecmp (key, "maxpowerondelay"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &maxpowerondelay) < 0)
            goto cleanup;
          
          if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
							     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_POWER_MANAGEMENT,
							     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_POWER_MANAGEMENT_MAXIMUM_POWER_ON_DELAY,
							     0,
							     2,
							     (uint32_t)maxpowerondelay) < 0)
            goto cleanup;
        }
      else
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s' : invalid key\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[i]);
          goto cleanup;
        }

      free (key);
      free (value);
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_thirdparty_get_sol_idle_timeout (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  uint16_t timeout;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SOL == IPMI_OEM_QUANTA_EXTENDED_CONFIGURATION_ID_SOL);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SOL == IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_SOL);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SOL_SOL_IDLE_TIMEOUT == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_SOL_SOL_IDLE_TIMEOUT);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SOL_SOL_IDLE_TIMEOUT == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_SOL_IDLE_TIMEOUT);

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SOL,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SOL_SOL_IDLE_TIMEOUT,
						     0,
						     2,
						     &tmpvalue) < 0)
    goto cleanup;
  
  timeout = tmpvalue;
  
  pstdout_printf (state_data->pstate,
                  "%u minutes\n",
                  timeout);
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_thirdparty_set_sol_idle_timeout (ipmi_oem_state_data_t *state_data)
{
  uint16_t timeout;
  int rv = -1;
  
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SOL == IPMI_OEM_QUANTA_EXTENDED_CONFIGURATION_ID_SOL);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SOL == IPMI_OEM_WISTRON_EXTENDED_CONFIGURATION_ID_SOL);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SOL_SOL_IDLE_TIMEOUT == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_SOL_SOL_IDLE_TIMEOUT);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SOL_SOL_IDLE_TIMEOUT == IPMI_OEM_WISTRON_EXTENDED_ATTRIBUTE_ID_SOL_SOL_IDLE_TIMEOUT);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "none"))
    {
      unsigned int temp;
      char *endptr = NULL;
      
      errno = 0;
      
      temp = strtoul (state_data->prog_data->args->oem_options[0], &endptr, 10);
      if (errno
          || endptr[0] != '\0'
          || temp > USHRT_MAX)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s'\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[0]);
          goto cleanup;
        }

      timeout = temp;
    }
  else
    timeout = 0;

  if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SOL,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SOL_SOL_IDLE_TIMEOUT,
						     0,
						     2,
						     (uint32_t)timeout) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_thirdparty_get_telnet_ssh_redirect_status (ipmi_oem_state_data_t *state_data)
{
  uint32_t tmpvalue;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SOL == IPMI_OEM_QUANTA_EXTENDED_CONFIGURATION_ID_SOL);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_SSH_REDIRECT_ENABLE == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_SSH_REDIRECT_ENABLE);

  if (ipmi_oem_thirdparty_get_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SOL,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_SSH_REDIRECT_ENABLE,
						     0,
						     1,
						     &tmpvalue) < 0)
    goto cleanup;
  
  pstdout_printf (state_data->pstate,
                  "%s\n",
                  (tmpvalue) ? "enabled" : "disabled");
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_thirdparty_set_telnet_ssh_redirect_status (ipmi_oem_state_data_t *state_data)
{
  uint8_t enable = 0;
  int rv = -1;
  
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);
  assert (IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SOL == IPMI_OEM_QUANTA_EXTENDED_CONFIGURATION_ID_SOL);
  assert (IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_SSH_REDIRECT_ENABLE == IPMI_OEM_QUANTA_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_SSH_REDIRECT_ENABLE);
  
  if (strcasecmp (state_data->prog_data->args->oem_options[0], "enable")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "disable"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }
  
  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "enable"))
    enable = 1;
  else
    enable = 0;
  
  if (ipmi_oem_thirdparty_set_extended_config_value (state_data,
						     IPMI_OEM_INVENTEC_EXTENDED_CONFIGURATION_ID_SOL,
						     IPMI_OEM_INVENTEC_EXTENDED_ATTRIBUTE_ID_SOL_TELNET_SSH_REDIRECT_ENABLE,
						     0,
						     1,
						     (uint32_t)enable) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}
