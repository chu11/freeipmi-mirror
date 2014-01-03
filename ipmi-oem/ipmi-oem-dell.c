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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <math.h>
#include <arpa/inet.h>
#include <limits.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"
#include "ipmi-oem-dell.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"
#include "tool-util-common.h"

/* Some slots resolve to 2.0 Watts when "off" */  
#define IPMI_OEM_DELL_ZERO_DEGREE_EPSILON 2.5

/* Will call ipmi_cmd_get_system_info_parameters only once, b/c field
 * requested is defined by OEM to be < 16 bytes in length
 */
static int
_get_dell_system_info_short_string (ipmi_oem_state_data_t *state_data,
                                    uint8_t parameter_selector,
                                    char *string,
                                    unsigned int string_len,
				    unsigned int *string_len_ret)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  int len;
  int rv = -1;

  assert (state_data);
  assert (string);
  assert (string_len);
  assert (state_data->prog_data->args->oem_options_count == 1);

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
                                           IPMI_SYSTEM_INFO_PARAMETERS_NO_SET_SELECTOR,
                                           IPMI_SYSTEM_INFO_PARAMETERS_NO_BLOCK_SELECTOR,
                                           obj_cmd_rs) < 0)
    {
      if ((ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
	   && ((ipmi_check_completion_code (obj_cmd_rs,
					    IPMI_COMP_CODE_GET_SYSTEM_INFO_PARAMETERS_PARAMETER_NOT_SUPPORTED) == 1)
	       || (ipmi_check_completion_code (obj_cmd_rs,
					       IPMI_COMP_CODE_OEM_DELL_NOT_LICENSED) == 1)))
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

  /* configuration_parameter_data[0] - string length
   * configuration_parameter_data[1-n] - string
   */

  if (len < 1)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_system_info_parameters: invalid buffer length returned: %d\n",
                       len);
      goto cleanup;
    }

  if (configuration_parameter_data[0] != (len - 1))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_system_info_parameters: invalid string length returned: %u\n",
                       configuration_parameter_data[0]);
      goto cleanup;
    }

  if (configuration_parameter_data[0])
    {
      if (configuration_parameter_data[0] > string_len)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "internal buffer overflow\n");
          goto cleanup;
        }
      
      memcpy (string,
              &(configuration_parameter_data[1]),
              configuration_parameter_data[0]);
    }

  if (string_len_ret)
    (*string_len_ret) = configuration_parameter_data[0];

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
_get_dell_system_info_long_string (ipmi_oem_state_data_t *state_data,
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
  assert (state_data->prog_data->args->oem_options_count == 1);

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
	   && ((ipmi_check_completion_code (obj_cmd_rs,
					    IPMI_COMP_CODE_GET_SYSTEM_INFO_PARAMETERS_PARAMETER_NOT_SUPPORTED) == 1)
	       || (ipmi_check_completion_code (obj_cmd_rs,
					       IPMI_COMP_CODE_OEM_DELL_NOT_LICENSED) == 1)))
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
_get_dell_system_info_bytes (ipmi_oem_state_data_t *state_data,
			     uint8_t parameter_selector,
			     uint8_t *bytes,
			     unsigned int bytes_len,
			     unsigned int minimum_bytes_expected,
			     unsigned int *bytes_len_ret)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  int len;
  int rv = -1;

  assert (state_data);
  assert (bytes);
  assert (bytes_len);
  assert (minimum_bytes_expected);
  assert (state_data->prog_data->args->oem_options_count == 1);

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
					   IPMI_SYSTEM_INFO_PARAMETERS_NO_SET_SELECTOR,
                                           IPMI_SYSTEM_INFO_PARAMETERS_NO_BLOCK_SELECTOR,
                                           obj_cmd_rs) < 0)
    {
      if ((ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
	   && ((ipmi_check_completion_code (obj_cmd_rs,
					    IPMI_COMP_CODE_GET_SYSTEM_INFO_PARAMETERS_PARAMETER_NOT_SUPPORTED) == 1)
	       || (ipmi_check_completion_code (obj_cmd_rs,
					       IPMI_COMP_CODE_OEM_DELL_NOT_LICENSED) == 1)))
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
  
  if (len < minimum_bytes_expected)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_cmd_get_system_info_parameters: invalid buffer length returned: %d\n",
		       len);
      goto cleanup;
    }

  if (len > bytes_len)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Internal buffer overflow\n");
      goto cleanup;
    }
  
  memcpy (bytes, configuration_parameter_data, len);
  
  if (bytes_len_ret)
    (*bytes_len_ret) = len;

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

/* returns 1 on success, 0 on not supported, -1 on error */
static int
_get_dell_system_info_idrac_info (ipmi_oem_state_data_t *state_data,
				  uint8_t *dhcp_or_static,
				  char *ip_address_buf,
				  unsigned int ip_address_buflen,
				  char *idrac_firmware_version_buf,
				  unsigned int idrac_firmware_version_buflen,
                                  uint8_t *idrac_type)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  uint8_t idrac_info[IPMI_OEM_MAX_BYTES];
  unsigned int idrac_info_len = 0;
  uint8_t string_encoding;
  int len;
  int rv = -1;
  int i;

  assert (state_data);
  assert (idrac_type);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Source Code
   *
   * Uses Get System Info command
   *
   * iDRAC Info Parameter = 0xDD
   * iDRAC Info Set Selector = ... see below ...
   *
   * Parameter data response formatted:
   *
   * Each block first byte is set-selector.  Not counting that, the
   * total block of data is.
   *
   * 1st byte
   * - 7:4 - reserved
   * - 3:0 - string encoding, 0 = printable ascii  
   * 2nd byte - string length
   * 3rd byte - IP address format
   * - 0x00 - IPv4
   * - 0x01 - IPv6
   * 4th byte - DHCP or static
   * - 0x00 - dhcp
   * - 0x01 - static
   * bytes 5-20 - IP address
   * - achu: seems to be stored binary, not ascii.  Why??
   * bytes 21-40 - IDRAC firmware version
   * byte 41 - idrac type
   * - 0x08 - 10G
   * - 0x09 - CMC
   * - 0x0A - 11G monolithic
   * - 0x0B - 11g modular
   * - 0x0D - maser lite
   * - 0x10 - 12g monolothic
   * - 0x11 - 12g modular
   *
   * set selector 0 = bytes 1-16
   * set selector 1 = bytes 17-32
   * set selector 2 = bytes 33-41
   */

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_system_info_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }
  
  for (i = 0; i < 3; i++)
    {
      if (ipmi_cmd_get_system_info_parameters (state_data->ipmi_ctx,
					       IPMI_GET_SYSTEM_INFO_PARAMETER,
					       IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_IDRAC_INFO,
					       i,
					       IPMI_SYSTEM_INFO_PARAMETERS_NO_BLOCK_SELECTOR,
					       obj_cmd_rs) < 0)
	{
	  if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
	      && (ipmi_check_completion_code (obj_cmd_rs,
					      IPMI_COMP_CODE_GET_SYSTEM_INFO_PARAMETERS_PARAMETER_NOT_SUPPORTED) == 1))
	    {
	      rv = 0;
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
      
      if (!len)
	{
	  if (!idrac_info_len)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s possibly not supported, no information available for reading\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command);
	      goto cleanup;
	    }

	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_get_system_info_parameters: invalid buffer length returned: %d\n",
			   len);
	  goto cleanup;
	}

      memcpy (&idrac_info[idrac_info_len],
	      configuration_parameter_data + 1,	/* remove set selector */
	      len - 1);

      idrac_info_len += (len - 1);
    }
  
  if (idrac_info_len < IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_MIN_LEN)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_cmd_get_system_info_parameters: invalid data length returned: %u\n",
		       idrac_info_len);
      goto cleanup;
    }

  string_encoding = (idrac_info[0] & IPMI_OEM_DELL_SYSTEM_INFO_STRING_ENCODING_BITMASK);
  string_encoding >>= IPMI_OEM_DELL_SYSTEM_INFO_STRING_ENCODING_SHIFT;

  if (string_encoding != IPMI_SYSTEM_INFO_ENCODING_ASCII_LATIN1)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Cannot handle non-ASCII encoding: %Xh\n",
                       configuration_parameter_data[0]);
      goto cleanup;
    }

  if (dhcp_or_static)
    (*dhcp_or_static) = idrac_info[3];

  if (ip_address_buf && ip_address_buflen)
    {
      uint8_t ip_address_format;

      ip_address_format = idrac_info[2];

      if (ip_address_format == IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_IP_ADDRESS_FORMAT_IPV4)
	{
	  uint32_t ip_address;

	  ip_address = idrac_info[4];
	  ip_address |= (idrac_info[5] << 8);
	  ip_address |= (idrac_info[6] << 16);
	  ip_address |= (idrac_info[7] << 24);
	  
	  if (!inet_ntop (AF_INET, &ip_address, ip_address_buf, ip_address_buflen))
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "inet_ntop: %s\n",
			       strerror (errno));
	      goto cleanup;
	    }
	}
      else if (ip_address_format == IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_IP_ADDRESS_FORMAT_IPV6)
	{
	  uint32_t ip_address[4];

	  ip_address[0] = idrac_info[4];
	  ip_address[0] |= (idrac_info[5] << 8);
	  ip_address[0] |= (idrac_info[6] << 16);
	  ip_address[0] |= (idrac_info[7] << 24);
	  ip_address[1] = idrac_info[8];
	  ip_address[1] |= (idrac_info[9] << 8);
	  ip_address[1] |= (idrac_info[10] << 16);
	  ip_address[1] |= (idrac_info[11] << 24);
	  ip_address[2] = idrac_info[12];
	  ip_address[2] |= (idrac_info[13] << 8);
	  ip_address[2] |= (idrac_info[14] << 16);
	  ip_address[2] |= (idrac_info[15] << 24);
	  ip_address[3] = idrac_info[16];
	  ip_address[3] |= (idrac_info[17] << 8);
	  ip_address[3] |= (idrac_info[18] << 16);
	  ip_address[3] |= (idrac_info[19] << 24);

	  if (!inet_ntop (AF_INET6, ip_address, ip_address_buf, ip_address_buflen))
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "inet_ntop: %s\n",
			       strerror (errno));
	      goto cleanup;
	    }
	}
      else
	memset (ip_address_buf, '\0', ip_address_buflen);
    } 

  if (idrac_firmware_version_buf
      && idrac_firmware_version_buflen
      && idrac_firmware_version_buflen >= IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_IDRAC_FIRMWARE_VERSION_STRING_LENGTH)
    memcpy (idrac_firmware_version_buf,
	    &idrac_info[20],
	    IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_IDRAC_FIRMWARE_VERSION_STRING_LENGTH);
  
  (*idrac_type) = idrac_info[40];

  rv = 1;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
_output_dell_system_info_idrac_info (ipmi_oem_state_data_t *state_data)
{
  uint8_t dhcp_or_static;
  char ip_address_buf[IPMI_OEM_STR_BUFLEN + 1]; 
  char idrac_firmware_version_buf[IPMI_OEM_STR_BUFLEN + 1];
  uint8_t idrac_type;
  char *dhcp_or_static_str;
  char *idrac_type_str;
  int rv = -1;
  int ret;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  memset (ip_address_buf, '\0', IPMI_OEM_STR_BUFLEN + 1);
  memset (idrac_firmware_version_buf, '\0', IPMI_OEM_STR_BUFLEN + 1);

  if ((ret = _get_dell_system_info_idrac_info (state_data,
					       &dhcp_or_static,
					       ip_address_buf,
					       IPMI_OEM_STR_BUFLEN,
					       idrac_firmware_version_buf,
					       IPMI_OEM_STR_BUFLEN,
					       &idrac_type)) < 0)
    goto cleanup;

  if (!ret)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "%s:%s '%s' option not supported on this system\n",
		       state_data->prog_data->args->oem_id,
		       state_data->prog_data->args->oem_command,
		       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
		  "IP Address             : %s\n",
		  ip_address_buf);

  if (dhcp_or_static == IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_IP_ADDRESS_CONFIG_DHCP)
    dhcp_or_static_str = "DHCP";
  else if (dhcp_or_static == IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_IP_ADDRESS_CONFIG_STATIC)
    dhcp_or_static_str = "Static";
  else
    dhcp_or_static_str = "Unknown";

  pstdout_printf (state_data->pstate,
		  "IP Configuration       : %s\n",
		  dhcp_or_static_str);

  pstdout_printf (state_data->pstate,
		  "iDRAC Firmware Version : %s\n",
		  idrac_firmware_version_buf);

  switch (idrac_type)
    {
    case IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_IDRAC_TYPE_10G:
      idrac_type_str = "Dell Poweredge 10G";
      break;
    case IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_IDRAC_TYPE_CMC:
      idrac_type_str = "Dell Chassis Management Controller ";
      break;
    case IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_IDRAC_TYPE_11G_MONOLITHIC:
      idrac_type_str = "Dell Poweredge 11G (Monolithic)";
      break;
    case IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_IDRAC_TYPE_11G_MODULAR:
      idrac_type_str = "Dell Poweredge 11G (Modular)";
      break;
    case IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_IDRAC_TYPE_MASER_LITE_BMC:
      idrac_type_str = "Maser Lite BMC";
      break;
    case IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_IDRAC_TYPE_12G_MONOLITHIC:
      idrac_type_str = "Dell Poweredge 12G (Monolithic)";
      break;
    case IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_IDRAC_TYPE_12G_MODULAR:
      idrac_type_str = "Dell Poweredge 12G (Modular)";
      break;
    default:
      idrac_type_str = "Unknown";
    }

  pstdout_printf (state_data->pstate,
		  "iDRAC Type             : %s\n",
		  idrac_type_str);

  rv = 0;
 cleanup:
  return (rv);
}

static int
_output_dell_system_info_cmc_info (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  uint8_t cmc_info[IPMI_OEM_MAX_BYTES];
  unsigned int cmc_info_len = 0;
  uint8_t string_encoding;
  uint8_t ip_address_format;
  char *ip_address_format_str;
  uint8_t ip_address_source;
  char * ip_address_source_str;
  char ip_str_buf[IPMI_OEM_STR_BUFLEN + 1];
  char cmc_firmware_version_buf[IPMI_OEM_STR_BUFLEN + 1];
  uint8_t gui_status;
  char *gui_status_str;
  uint8_t nic_state;
  char *nic_state_str;
  uint8_t link_connect;
  char *link_connect_str;
  uint8_t cmc_failover_racreset;
  char *cmc_failover_racreset_str;
  uint8_t hardware_vendor_mode;
  char *hardware_vendor_mode_str;
  int len;
  int rv = -1;
  int i;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * Uses Get System Info command
   *
   * CMC Info Parameter = 0xDF
   * CMC Info Set Selector = ... see below ...
   *
   * Parameter data response formatted:
   *
   * Each block first byte is set-selector.  Not counting that, the
   * total block of data is.
   *
   * 1st byte
   * - 7:4 - reserved
   * - 3:0 - string encoding, 0 = printable ascii  
   * 2nd byte - string length
   * 3rd byte - IP address format
   * - bit 0 - 0 = ipv4, 1 = ipv6
   * - bit 7:1 - reserved
   * 4th byte - dhcp/static
   * - 0 = dhcp, 1 = static
   * bytes 5-20 - ip address
   * bytes 21-40 - cmc firmware version
   * bytes 41 - type (0x09 for CMC)
   * byte 42 - gui status
   * - 0 = disabled, 1 = enabled
   * byte 53 - nic status
   * - bit 0 - nic enable/disable - 0 = disabled, 1 = enabled
   * - bit 1 - link connect/disconnect - 0 = disconnected, 1 = connected
   * - bit 2 - cmc failover or racreset - 0 = not from failover, 1 = from failover 
   * - bit 3 - hardware vendor mode - 0 = does not have hardware vendor mode, 1 = has hardware vendor mode
   *
   * set selector 0 = bytes 1-16
   * set selector 1 = bytes 17-32
   * set selector 2 = bytes 33-41
   * set selector 3 = bytes 42-57
   */

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_system_info_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }
  
  for (i = 0; i < 4; i++)
    {
      if (ipmi_cmd_get_system_info_parameters (state_data->ipmi_ctx,
					       IPMI_GET_SYSTEM_INFO_PARAMETER,
					       IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_CMC_INFO,
					       i,
					       IPMI_SYSTEM_INFO_PARAMETERS_NO_BLOCK_SELECTOR,
					       obj_cmd_rs) < 0)
	{
	  if ((ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
	       && ((ipmi_check_completion_code (obj_cmd_rs,
						IPMI_COMP_CODE_GET_SYSTEM_INFO_PARAMETERS_PARAMETER_NOT_SUPPORTED) == 1)
		   || (ipmi_check_completion_code (obj_cmd_rs,
						   IPMI_COMP_CODE_OEM_DELL_NOT_LICENSED) == 1)))
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
      
      if (!len)
	{
	  if (!cmc_info_len)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s possibly not supported, no information available for reading\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command);
	      goto cleanup;
	    }

	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_get_system_info_parameters: invalid buffer length returned: %d\n",
			   len);
	  goto cleanup;
	}
      
      memcpy (&cmc_info[cmc_info_len],
	      configuration_parameter_data + 1,	/* remove set selector */
	      len - 1);
      
      cmc_info_len += (len - 1);
    }

  if (cmc_info_len < IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_MIN_LEN)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_cmd_get_system_info_parameters: invalid data length returned: %u\n",
		       cmc_info_len);
      goto cleanup;
    }

  string_encoding = (cmc_info[0] & IPMI_OEM_DELL_SYSTEM_INFO_STRING_ENCODING_BITMASK);
  string_encoding >>= IPMI_OEM_DELL_SYSTEM_INFO_STRING_ENCODING_SHIFT;

  if (string_encoding != IPMI_SYSTEM_INFO_ENCODING_ASCII_LATIN1)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Cannot handle non-ASCII encoding: %Xh\n",
                       configuration_parameter_data[0]);
      goto cleanup;
    }

  ip_address_format = cmc_info[2];

  switch (ip_address_format)
    {
    case IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_IP_ADDRESS_FORMAT_IPV4:
      ip_address_format_str = "IPv4";
      break;
    case IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_IP_ADDRESS_FORMAT_IPV6:
      ip_address_format_str = "IPv6";
      break;
    default:
      ip_address_format_str = "Unspecified";
    }

  pstdout_printf (state_data->pstate,
		  "IP Address Format     : %s\n",
		  ip_address_format_str);
  
  ip_address_source = cmc_info[3];

  switch (ip_address_source)
    {
    case IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_IP_ADDRESS_SOURCE_DHCP:
      ip_address_source_str = "DHCP";
      break;
    case IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_IP_ADDRESS_SOURCE_STATIC:
      ip_address_source_str = "Static";
      break;
    default:
      ip_address_source_str = "Unspecified";
    }

  pstdout_printf (state_data->pstate,
		  "IP Address Source     : %s\n",
		  ip_address_source_str);

  memset (ip_str_buf, '\0', IPMI_OEM_STR_BUFLEN + 1);

  memcpy (ip_str_buf,
	  &cmc_info[4],
	  IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_IP_ADDRESS_STRING_LENGTH);

  pstdout_printf (state_data->pstate,
		  "IP Address            : %s\n",
		  ip_str_buf);

  memset (cmc_firmware_version_buf, '\0', IPMI_OEM_STR_BUFLEN + 1);

  memcpy (cmc_firmware_version_buf,
	  &cmc_info[20],
	  IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_CMC_FIRMWARE_VERSION_STRING_LENGTH);

  pstdout_printf (state_data->pstate,
		  "CMC Firmware Version  : %s\n",
		  cmc_firmware_version_buf);

  gui_status = cmc_info[41];

  switch (gui_status)
    {
    case IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_GUI_STATUS_DISABLED:
      gui_status_str = "Disabled";
      break;
    case IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_GUI_STATUS_ENABLED:
      gui_status_str = "Enabled";
      break;
    default:
      gui_status_str = "Unspecified";
    }

  pstdout_printf (state_data->pstate,
		  "GUI Status            : %s\n",
		  gui_status_str);
  
  nic_state = (cmc_info[42] & IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_NIC_STATUS_NIC_STATE_BITMASK);
  nic_state >>= IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_NIC_STATUS_NIC_STATE_SHIFT;
		  
  if (nic_state == IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_NIC_STATUS_NIC_STATE_DISABLED)
    nic_state_str = "Disabled";
  else
    nic_state_str = "Enabled";
  
  pstdout_printf (state_data->pstate,
		  "NIC State             : %s\n",
		  nic_state_str);
  
  link_connect = (cmc_info[42] & IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_NIC_STATUS_LINK_CONNECT_BITMASK);
  link_connect >>= IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_NIC_STATUS_LINK_CONNECT_SHIFT;
  
  if (link_connect == IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_NIC_STATUS_LINK_CONNECT_DISCONNECTED)
    link_connect_str = "Disconnected";
  else
    link_connect_str = "Connected";
  
  pstdout_printf (state_data->pstate,
		  "Link Connect          : %s\n",
		  link_connect_str);
  
  cmc_failover_racreset = (cmc_info[42] & IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_NIC_STATUS_CMC_FAILOVER_RACRESET_BITMASK);
  cmc_failover_racreset >>= IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_NIC_STATUS_CMC_FAILOVER_RACRESET_SHIFT;
  
  if (cmc_failover_racreset == IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_NIC_STATUS_CMC_FAILOVER_RACRESET_NOT_FROM_FAILOVER)
    cmc_failover_racreset_str = "Not from failover";
  else
    cmc_failover_racreset_str = "From failover";
  
  pstdout_printf (state_data->pstate,
		  "CMC Failover/RACreset : %s\n",
		  cmc_failover_racreset_str);
  
  hardware_vendor_mode = (cmc_info[42] & IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_NIC_STATUS_HARDWARE_VENDOR_MODE_BITMASK);
  hardware_vendor_mode >>= IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_NIC_STATUS_HARDWARE_VENDOR_MODE_SHIFT;
  
  if (hardware_vendor_mode == IPMI_OEM_DELL_SYSTEM_INFO_CMC_INFO_NIC_STATUS_HARDWARE_VENDOR_MODE_NO_HARDWARE_VENDOR_MODE)
    hardware_vendor_mode_str = "Does not have hardware vendor mode ";
  else
    hardware_vendor_mode_str = "Has hardware vendor mode";
  
  pstdout_printf (state_data->pstate,
		  "Hardware Vendor Mode  : %s\n",
		  hardware_vendor_mode_str);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
_output_dell_system_info_cmc_ipv6_info (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  uint8_t cmc_ipv6_info[IPMI_OEM_MAX_BYTES];
  unsigned int cmc_ipv6_info_len = 0;
  uint8_t string_encoding;
  uint8_t ipv6_status;
  uint8_t autoconfiguration;
  char ipv6_str_buf[IPMI_OEM_STR_BUFLEN + 1];
  int len;
  int rv = -1;
  int i;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * Uses Get System Info command
   *
   * CMC IPv6 Info Parameter = 0xF2
   * CMC IPv6 Info Set Selector = ... see below ...
   *
   * Parameter data response formatted:
   *
   * Each block first byte is set-selector.  Not counting that, the
   * total block of data is.
   *
   * 1st byte
   * - 7:4 - reserved
   * - 3:0 - string encoding, 0 = printable ascii  
   * 2nd byte - string length
   * 3rd byte - IPv6 status
   * - bit 0 - 0 = disabled, 1 = enabled
   * - bit 7:1 - reserved
   * 4th byte - autoconfiguration
   * - bit 0 - 0 = disabled, 1 = enabled
   * - bit 7:1 - reserved
   * bytes 5-16 - reserved
   * byte 17 - prefix length
   * byte 18 - string length
   * byte 19-57 - IPv6 address (string)
   *
   * set selector 0 = bytes 1-16
   * set selector 1 = bytes 17-32
   * set selector 2 = bytes 33-41
   * set selector 3 = bytes 42-57
   */

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_system_info_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }
  
  for (i = 0; i < 4; i++)
    {
      if (ipmi_cmd_get_system_info_parameters (state_data->ipmi_ctx,
					       IPMI_GET_SYSTEM_INFO_PARAMETER,
					       IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_CMC_IPV6_INFO,
					       i,
					       IPMI_SYSTEM_INFO_PARAMETERS_NO_BLOCK_SELECTOR,
					       obj_cmd_rs) < 0)
	{
	  if ((ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
	       && ((ipmi_check_completion_code (obj_cmd_rs,
						IPMI_COMP_CODE_GET_SYSTEM_INFO_PARAMETERS_PARAMETER_NOT_SUPPORTED) == 1)
		   || (ipmi_check_completion_code (obj_cmd_rs,
						   IPMI_COMP_CODE_OEM_DELL_NOT_LICENSED) == 1)))
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
      
      if (!len)
	{
	  if (!cmc_ipv6_info_len)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s possibly not supported, no information available for reading\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command);
	      goto cleanup;
	    }

	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_get_system_info_parameters: invalid buffer length returned: %d\n",
			   len);
	  goto cleanup;
	}
      
      memcpy (&cmc_ipv6_info[cmc_ipv6_info_len],
	      configuration_parameter_data + 1,	/* remove set selector */
	      len - 1);
      
      cmc_ipv6_info_len += (len - 1);
    }

  if (cmc_ipv6_info_len < IPMI_OEM_DELL_SYSTEM_INFO_CMC_IPV6_INFO_MIN_LEN)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_cmd_get_system_info_parameters: invalid data length returned: %u\n",
		       cmc_ipv6_info_len);
      goto cleanup;
    }

  string_encoding = (cmc_ipv6_info[0] & IPMI_OEM_DELL_SYSTEM_INFO_STRING_ENCODING_BITMASK);
  string_encoding >>= IPMI_OEM_DELL_SYSTEM_INFO_STRING_ENCODING_SHIFT;

  if (string_encoding != IPMI_SYSTEM_INFO_ENCODING_ASCII_LATIN1)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "Cannot handle non-ASCII encoding: %Xh\n",
                       configuration_parameter_data[0]);
      goto cleanup;
    }

  ipv6_status = (cmc_ipv6_info[2] & IPMI_OEM_DELL_SYSTEM_INFO_CMC_IPV6_INFO_IPV6_STATUS_BITMASK);
  ipv6_status >>= IPMI_OEM_DELL_SYSTEM_INFO_CMC_IPV6_INFO_IPV6_STATUS_SHIFT;

  pstdout_printf (state_data->pstate,
		  "IPv6 Status       : %s\n",
		  ipv6_status == IPMI_OEM_DELL_SYSTEM_INFO_CMC_IPV6_INFO_IPV6_STATUS_ENABLED ? "Enabled" : "Disabled");

  autoconfiguration = (cmc_ipv6_info[3] & IPMI_OEM_DELL_SYSTEM_INFO_CMC_IPV6_INFO_AUTOCONFIGURATION_BITMASK);
  autoconfiguration >>= IPMI_OEM_DELL_SYSTEM_INFO_CMC_IPV6_INFO_AUTOCONFIGURATION_SHIFT;

  pstdout_printf (state_data->pstate,
		  "Autoconfiguration : %s\n",
		  ipv6_status == IPMI_OEM_DELL_SYSTEM_INFO_CMC_IPV6_INFO_AUTOCONFIGURATION_ENABLED ? "Enabled" : "Disabled");

  memset (ipv6_str_buf, '\0', IPMI_OEM_STR_BUFLEN + 1);

  memcpy (ipv6_str_buf,
	  &cmc_ipv6_info[18],
	  IPMI_OEM_DELL_SYSTEM_INFO_CMC_IPV6_INFO_IPV6_ADDRESS_STRING_LENGTH);

  pstdout_printf (state_data->pstate,
		  "IPv6 Address      : %s\n",
		  ipv6_str_buf);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

#if 0
/* cannot verify */
static int
_output_dell_system_info_snmp_ipv6_info (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  uint8_t ipv6_snmp_trap_destination_addresses[IPMI_OEM_MAX_BYTES];
  unsigned int ipv6_snmp_trap_destination_addresses_len = 0;
  uint8_t string_encoding;
  uint8_t data_length;
  uint8_t destination_type;
  uint8_t alert_ack_timeout;
  uint8_t retries;
  char *destination_type_str;
  char ipv6_str_buf[IPMI_OEM_STR_BUFLEN + 1];
  int len;
  int rv = -1;
  int i;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * Uses Get System Info command
   *
   * IPv6 SNMP Trap Destination Addresses Parameter = 0xF0
   * IPv6 SNMP Trap Destination Addresses Set Selector = ... see below ...
   *
   * Parameter data response formatted:
   *
   * Each block first byte is set-selector.  Not counting that, the
   * total block of data is.
   *
   * 1st byte
   * - 7:4 - reserved
   * - 3:0 - string encoding, 0 = printable ascii  
   * 2nd byte - string length
   * 3rd byte - destination type
   * - bit 0 - 0 = disabled, 1 = enabled
   * - bit 7:1 - reserved
   * 4th byte - alert ack timeout
   * - bit 0 - 0 = disabled, 1 = enabled
   * - bit 7:1 - reserved
   * 5th byte - retries
   * bytes 6-N - IPv6 string (max 39 bytes)
   *
   * set selector 0 = bytes 1-16
   * set selector 1 = bytes 17-32
   * etc.
   */

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
					   IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_IPV6_SNMP_TRAP_DESTINATION_ADDRESS,
					   0,
					   IPMI_SYSTEM_INFO_PARAMETERS_NO_BLOCK_SELECTOR,
					   obj_cmd_rs) < 0)
    {
      if ((ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
	   && ((ipmi_check_completion_code (obj_cmd_rs,
					    IPMI_COMP_CODE_GET_SYSTEM_INFO_PARAMETERS_PARAMETER_NOT_SUPPORTED) == 1)
	       || (ipmi_check_completion_code (obj_cmd_rs,
					       IPMI_COMP_CODE_OEM_DELL_NOT_LICENSED) == 1)))
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

  if (!len)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "%s:%s possibly not supported, no information available for reading\n",
		       state_data->prog_data->args->oem_id,
		       state_data->prog_data->args->oem_command);
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

  data_length = configuration_parameter_data[2];

  if (!data_length)
    goto out;

  memcpy (&ipv6_snmp_trap_destination_addresses[ipv6_snmp_trap_destination_addresses_len],
	  configuration_parameter_data + 1,	/* remove set selector */
	  len - 1);
      
  ipv6_snmp_trap_destination_addresses_len += (len - 1);

  /* + 2 for string encoding and data length fields, aren't accounted
   * for in Dell's OEM count
   */
  for (i = 1; i <  (data_length + 2) / 16; i++)
    {
      if (ipmi_cmd_get_system_info_parameters (state_data->ipmi_ctx,
					       IPMI_GET_SYSTEM_INFO_PARAMETER,
					       IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_IPV6_SNMP_TRAP_DESTINATION_ADDRESS,
					       i,
					       IPMI_SYSTEM_INFO_PARAMETERS_NO_BLOCK_SELECTOR,
					       obj_cmd_rs) < 0)
	{
	  if ((ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
	       && ((ipmi_check_completion_code (obj_cmd_rs,
						IPMI_COMP_CODE_GET_SYSTEM_INFO_PARAMETERS_PARAMETER_NOT_SUPPORTED) == 1)
		   || (ipmi_check_completion_code (obj_cmd_rs,
						   IPMI_COMP_CODE_OEM_DELL_NOT_LICENSED) == 1)))
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
      
      if (!len)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_get_system_info_parameters: invalid buffer length returned: %d\n",
			   len);
	  goto cleanup;
	}
      
      memcpy (&ipv6_snmp_trap_destination_addresses[ipv6_snmp_trap_destination_addresses_len],
	      configuration_parameter_data + 1,	/* remove set selector */
	      len - 1);
      
      ipv6_snmp_trap_destination_addresses_len += (len - 1);
    }

  if (ipv6_snmp_trap_destination_addresses_len < IPMI_OEM_DELL_SYSTEM_INFO_IPV6_SNMP_TRAP_DESTINATION_ADDRESS_MIN_LEN)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_cmd_get_system_info_parameters: invalid data length returned: %u\n",
		       ipv6_snmp_trap_destination_addresses_len);
      goto cleanup;
    }

  destination_type = ipv6_snmp_trap_destination_addresses[2];
  alert_ack_timeout = ipv6_snmp_trap_destination_addresses[3];
  retries = ipv6_snmp_trap_destination_addresses[4];

  switch (destination_type)
    {
    case IPMI_DESTINATION_TYPE_PET_TRAP_DESTINATION:
      destination_type_str = "PET Trap destination";
      break;
    case IPMI_DESTINATION_TYPE_OEM1:
      destination_type_str = "OEM 1";
      break;
    case IPMI_DESTINATION_TYPE_OEM2:
      destination_type_str = "OEM 2";
      break;
    default:
      destination_type_str = "Unknown";
    }

  pstdout_printf (state_data->pstate,
		  "Destination Type          : %u s\n",
		  destination_type_str);

  pstdout_printf (state_data->pstate,
		  "Alert Acknowledge Timeout : %u s\n",
		  alert_ack_timeout);

  pstdout_printf (state_data->pstate,
		  "Retries                   : %u\n",
		  retries);

  memset (ipv6_str_buf, '\0', IPMI_OEM_STR_BUFLEN + 1);

  memcpy (ipv6_str_buf,
	  &ipv6_snmp_trap_destination_addresses[5],
	  (data_length - 3));

  pstdout_printf (state_data->pstate,
		  "IPv6 Address              : %s\n",
		  ipv6_str_buf);

 out:
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}
#endif

static int
_output_dell_system_info_10g_mac_addresses (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t number_of_nics;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  int len;
  int i;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

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
                                           IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_EMBEDDED_NICS_MAC_ADDRESSES,
                                           IPMI_SYSTEM_INFO_PARAMETERS_NO_SET_SELECTOR,
                                           IPMI_SYSTEM_INFO_PARAMETERS_NO_BLOCK_SELECTOR,
                                           obj_cmd_rs) < 0)
    {
      if ((ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
	   && ((ipmi_check_completion_code (obj_cmd_rs,
					    IPMI_COMP_CODE_GET_SYSTEM_INFO_PARAMETERS_PARAMETER_NOT_SUPPORTED) == 1)
	       || (ipmi_check_completion_code (obj_cmd_rs,
					       IPMI_COMP_CODE_OEM_DELL_NOT_LICENSED) == 1)))
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

  number_of_nics = configuration_parameter_data[0];

  if (!number_of_nics)
    {
      rv = 0;
      goto cleanup;
    }

  if ((number_of_nics * IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_LENGTH) != (len - 1))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_system_info_parameters: invalid buffer length returned: number of nics = %u, bytes = %d\n",
		       number_of_nics,
                       len);
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
		  "NIC Number\tMAC Address\n");
  for (i = 0; i < number_of_nics; i++)
    pstdout_printf (state_data->pstate,
		    "%u\t\t%02X:%02X:%02X:%02X:%02X:%02X\n",
		    i,
		    configuration_parameter_data[i*6 + 1],
		    configuration_parameter_data[i*6 + 2],
		    configuration_parameter_data[i*6 + 3],
		    configuration_parameter_data[i*6 + 4],
		    configuration_parameter_data[i*6 + 5],
		    configuration_parameter_data[i*6 + 6]);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
_output_dell_system_info_11g_or_12g_mac_addresses (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t total_bytes;
  int rs_len;
  int i;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  /* see info below in ipmi_oem_dell_get_system_info() for packet
   * format.  We cannot use normal Get System Info b/c Dell hacked it
   * to include/support extra bytes. 
   */

  bytes_rq[0] = IPMI_CMD_GET_SYSTEM_INFO_PARAMETERS;
  bytes_rq[1] = 0x00;		/* get parameter */
  bytes_rq[2] = IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_11G_MAC_ADDRESSES; /* parameter selector */
  bytes_rq[3] = 0x00;		/* set selector */
  bytes_rq[4] = 0x00;		/* block selector */
  bytes_rq[5] = 0x00;		/* offset */
  bytes_rq[6] = 0x00;		/* length */

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_APP_RQ, /* network function */
                              bytes_rq, /* data */
                              7, /* num bytes */
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
                                                   IPMI_CMD_GET_SYSTEM_INFO_PARAMETERS,
                                                   IPMI_NET_FN_APP_RS,
                                                   NULL) < 0)
    goto cleanup;
  
  total_bytes = bytes_rs[3];

  if (!total_bytes)
    {
      rv = 0;
      goto cleanup;
    }

#if 0
  /* Apparently on some systems return a non-multiple of 8 bytes
   *
   * Just fall through and output whatever you can.
   */
  if (total_bytes % 8)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "invalid total bytes of data returned: %u\n",
		       total_bytes);
      goto cleanup;
    }
#endif

  /* see record format below in ipmi_oem_dell_get_system_info(), record length = 8 */
  pstdout_printf (state_data->pstate,
		  "NIC Number\tMAC Address\t\tNIC Status\n");
  for (i = 0; i < (total_bytes / IPMI_OEM_DELL_SYSTEM_INFO_11G_OR_12G_MAC_ADDRESS_LENGTH); i++)
    {
      uint8_t mac_type;
      
      bytes_rq[0] = IPMI_CMD_GET_SYSTEM_INFO_PARAMETERS;
      bytes_rq[1] = 0x00;		/* get parameter */
      bytes_rq[2] = IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_11G_MAC_ADDRESSES; /* parameter selector */
      bytes_rq[3] = 0x00;		/* set selector */
      bytes_rq[4] = 0x00;		/* block selector */
      bytes_rq[5] = i * IPMI_OEM_DELL_SYSTEM_INFO_11G_OR_12G_MAC_ADDRESS_LENGTH; /* offset */
      bytes_rq[6] = IPMI_OEM_DELL_SYSTEM_INFO_11G_OR_12G_MAC_ADDRESS_LENGTH; /* length */
      
      if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
				  0, /* lun */
				  IPMI_NET_FN_APP_RQ, /* network function */
				  bytes_rq, /* data */
				  7, /* num bytes */
				  bytes_rs,
				  IPMI_OEM_MAX_BYTES)) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_raw: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  goto cleanup;
	}
      
      /* 11 = IPMI_OEM_DELL_SYSTEM_INFO_11G_OR_12G_MAC_ADDRESS_LENGTH + 3 (for cmd, completion code, parameter revision) */
      if (ipmi_oem_check_response_and_completion_code (state_data,
						       bytes_rs,
						       rs_len,
						       11,
						       IPMI_CMD_GET_SYSTEM_INFO_PARAMETERS,
						       IPMI_NET_FN_APP_RS,
                                                       NULL) < 0)
	goto cleanup;
      
      mac_type = (bytes_rs[3] & IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_TYPE_BITMASK);
      mac_type >>= IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_TYPE_SHIFT;
      
      if (mac_type == IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_TYPE_ETHERNET)
	{
	  uint8_t nic_number;
	  uint8_t nic_status;
	  char *nic_status_str = NULL;

	  nic_status = (bytes_rs[3] & IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_NIC_STATUS_BITMASK);
          nic_status >>= IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_NIC_STATUS_SHIFT;

	  nic_number = (bytes_rs[4] & IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_NIC_NUMBER_BITMASK);
          nic_number >>= IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_NIC_NUMBER_SHIFT;

	  switch (nic_status)
	    {
	    case IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_STATUS_ENABLED:
	      nic_status_str = "Enabled";
	      break;
	    case IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_STATUS_DISABLED:
	      nic_status_str = "Disabled";
	      break;
	    case IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_STATUS_PLAYING_DEAD:
	      nic_status_str = "Playing Dead";
	      break;
	    default:
	      nic_status_str = "Unknown";
	    }

	  pstdout_printf (state_data->pstate,
			  "%u\t\t%02X:%02X:%02X:%02X:%02X:%02X\t%s\n",
			  nic_number,
			  bytes_rs[5],
			  bytes_rs[6],
			  bytes_rs[7],
			  bytes_rs[8],
			  bytes_rs[9],
			  bytes_rs[10],
			  nic_status_str);
	}
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_system_info (ipmi_oem_state_data_t *state_data)
{
  char string[IPMI_OEM_DELL_SYSTEM_INFO_MAX_STRING_BYTES+1];
  unsigned int string_len = 0;
  uint8_t bytes[IPMI_OEM_DELL_SYSTEM_INFO_MAX_STRING_BYTES+1];
  unsigned int bytes_len = 0;
  int rv = -1;

  assert (state_data);

  if (!state_data->prog_data->args->oem_options_count)
    {
      pstdout_printf (state_data->pstate,
                      "Option: guid\n"
                      "Option: asset-tag\n"
                      "Option: service-tag\n"
		      "Option: chassis-service-tag\n"
		      "Option: chassis-related-service-tag\n"
		      "Option: board-revision\n"
		      "Option: platform-model-name\n"
#if 0
		      /* legacy */
		      "Option: slot-number\n"
#else
		      "Option: blade-slot-info\n"
#endif
		      "Option: system-revision\n"
		      "Option: embedded-video-status\n"
		      "Option: idrac-info\n"
		      "Option: idrac-ipv4-url\n"
		      "Option: idrac-gui-webserver-control\n"
		      "Option: cmc-info\n"
		      "Option: cmc-ipv4-url\n"
		      "Option: cmc-ipv6-info\n"
		      "Option: cmc-ipv6-url\n"
#if 0
/* cannot verify */
		      "Option: snmp-ipv6-info\n"
#endif
                      "Option: mac-addresses\n");
      return (0);
    }

  if (state_data->prog_data->args->oem_options_count != 1)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s please specify one get-system-info KEY\n",
		       state_data->prog_data->args->oem_id,
		       state_data->prog_data->args->oem_command);
      goto cleanup;
    }

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "guid")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "asset-tag")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "service-tag")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "chassis-service-tag")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "chassis-related-service-tag")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "board-revision")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "product-name") /* legacy */
      && strcasecmp (state_data->prog_data->args->oem_options[0], "platform-model-name")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "slot-number") /* legacy */
      && strcasecmp (state_data->prog_data->args->oem_options[0], "blade-slot-info") /* legacy */
      && strcasecmp (state_data->prog_data->args->oem_options[0], "system-revision")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "embedded-video-status")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "idrac-info")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "idrac-ipv4-url")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "idrac-gui-webserver-control")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "cmc-info")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "cmc-ipv4-url")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "cmc-ipv6-info")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "cmc-ipv6-url")
#if 0
/* cannot verify */
      && strcasecmp (state_data->prog_data->args->oem_options[0], "snmp-ipv6-info")
#endif
      && strcasecmp (state_data->prog_data->args->oem_options[0], "mac-addresses"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  /* Dell Poweredge OEM
   *
   * Some from http://linux.dell.com/files/openipmi/ipmitool/
   * Some from Dell Provided Source Code
   * Some from Dell Provided Docs
   *
   * Uses Get System Info command
   *
   * For asset-tag and service-tag, the response format is different
   * than product name.
   *
   * Format #1)
   *
   * guid parameter = 0xC3
   * asset-tag parameter = 0xC4
   * service-tag parameter = 0xC5
   * chassis-service-tag parameter = 0xC6
   * chassis-related-service-tag parameter = 0xC7
   * board-revision parameter = 0xC8
   *
   * Parameter data response formatted:
   *
   * 1st byte = length
   * ? bytes = string/buf
   *
   * Format #2)
   *
   * platform-model-name parameter = 0xD1
   * blade-slot-info / slot number = 0xDC
   * iDRAC IPv4 URL = 0xDE
   * CMC IPv4 URL = 0xE0
   * CMC IPv6 URL = 0xF3
   *
   * Parameter data response formatted:
   *
   * Set Selector 0:
   *
   * 1st byte = set selector
   * 2nd byte
   * - 7:4 - reserved
   * - 3:0 - string encoding, 0 = printable ascii  
   * 3rd byte = string length
   * ? bytes = string
   *
   * Set Selector > 0
   *
   * 1st byte = set selector
   * ? bytes = string
   *
   * Format #3)
   *
   * iDRAC info = 0xDD
   *
   * See format in _get_dell_system_info_idrac_info().
   *
   * Format #4)
   * 
   * iDRAC GUI/Webserver Control = 0xE1
   *
   * 1st byte - 0x00 = Disabled, 0x01 = Enabled
   *
   * Format #5)
   *
   * CMC IPv6 Info = 0xF2
   *
   * See format in _output_dell_system_info_cmc_ipv6_info().
   *
   * Format #6)
   *
   * IPv6 SNMP Trap Destination Addresses = 0xF0
   *
   * See format in _output_dell_system_info_snmp_ipv6_info()
   *
   * Format #7)
   *
   * Dell 10G systems, mac-addresses = 0xCB
   *
   * Parameter data response formatted:
   *
   * 1st byte = number of NICs
   * ? bytes = MAC address of NICS, number of NICS * 6 total bytes
   *
   * Format #8)
   *
   * Dell 11G systems, mac-addresses = 0xDA
   * + 2 extra bytes
   * byte 5 : offset into data to read
   * byte 6 : length of data to read
   *
   *
   * Parameter data response formatted:
   *
   * if byte 5 and byte 6 are 0x00
   *
   * 1st byte - total bytes of MAC address data
   *
   * if byte 5 and byte 6 have real offsets/lengths
   *
   * parameter revision byte = total number of bytes returned
   * ? bytes = record stored in following format
   *   byte 1 - 0:3 - blade slot number
   *   byte 1 - 4:5 - mac address type
   *                - 0 = ethernet
   *                - 1 = iSCSI
   *                - 2 = ???
   *                - 3 = reserved
   *   byte 1 - 6:7 - ethernet status
   *                - 0 = enabled
   *                - 1 = disabled
   *                - 2 = playing dead
   *                - 3 = reserved
   *   byte 2 - 0:4 - NIC number
   *   byte 2 - 5:7 - reserved
   *   bytes 3 - 8 - MAC address
   *
   * Format #9)
   *
   * cmc info = 0xDF
   *
   * See format in _get_dell_system_info_cmc_info().
   */

  memset (string, '\0', IPMI_OEM_DELL_SYSTEM_INFO_MAX_STRING_BYTES + 1);

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "guid"))
    {
      if (_get_dell_system_info_short_string (state_data,
                                              IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_SYSTEM_GUID,
                                              string,
                                              IPMI_OEM_DELL_SYSTEM_INFO_MAX_STRING_BYTES,
					      &string_len) < 0)
        goto cleanup;
      
      if (string_len != IPMI_SYSTEM_GUID_LENGTH)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "Invalid GUID length returned: %s\n",
			   string_len);
	  goto cleanup;
	}

      pstdout_printf (state_data->pstate,
		      "%02X%02X%02X%02X-%02X%02X-%02X%02X-%02X%02X-%02X%02X%02X%02X%02X%02X\n",
		      (uint8_t)string[15],  /* time low */
		      (uint8_t)string[14],
		      (uint8_t)string[13],
		      (uint8_t)string[12],
		      (uint8_t)string[11],  /* time mid */
		      (uint8_t)string[10],
		      (uint8_t)string[9],   /* time high and version */
		      (uint8_t)string[8],
		      (uint8_t)string[6],   /* clock seq high and reserved - comes before clock seq low */
		      (uint8_t)string[7],   /* clock seq low */
		      (uint8_t)string[5],   /* node */
		      (uint8_t)string[4],
		      (uint8_t)string[3],
		      (uint8_t)string[2],
		      (uint8_t)string[1],
		      (uint8_t)string[0]);
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "asset-tag"))
    {
      if (_get_dell_system_info_short_string (state_data,
                                              IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_SYSTEM_ASSET_TAG,
                                              string,
                                              IPMI_OEM_DELL_SYSTEM_INFO_MAX_STRING_BYTES,
					      NULL) < 0)
        goto cleanup;

      pstdout_printf (state_data->pstate,
		      "%s\n",
		      string);
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "service-tag"))
    {
      if (_get_dell_system_info_short_string (state_data,
                                              IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_SYSTEM_SERVICE_TAG,
                                              string,
                                              IPMI_OEM_DELL_SYSTEM_INFO_MAX_STRING_BYTES,
					      NULL) < 0)
        goto cleanup;

      pstdout_printf (state_data->pstate,
		      "%s\n",
		      string);
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "chassis-service-tag"))
    {
      if (_get_dell_system_info_short_string (state_data,
                                              IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_CHASSIS_SERVICE_TAG,
                                              string,
                                              IPMI_OEM_DELL_SYSTEM_INFO_MAX_STRING_BYTES,
					      NULL) < 0)
        goto cleanup;

      pstdout_printf (state_data->pstate,
		      "%s\n",
		      string);
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "chassis-related-service-tag"))
    {
      if (_get_dell_system_info_short_string (state_data,
                                              IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_CHASSIS_RELATED_SERVICE_TAG,
                                              string,
                                              IPMI_OEM_DELL_SYSTEM_INFO_MAX_STRING_BYTES,
					      NULL) < 0)
        goto cleanup;

      pstdout_printf (state_data->pstate,
		      "%s\n",
		      string);
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "board-revision"))
    {
      if (_get_dell_system_info_short_string (state_data,
                                              IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_BOARD_REVISION,
                                              string,
                                              IPMI_OEM_DELL_SYSTEM_INFO_MAX_STRING_BYTES,
					      NULL) < 0)
        goto cleanup;

      pstdout_printf (state_data->pstate,
		      "%s\n",
		      string);
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "product-name")
           || !strcasecmp (state_data->prog_data->args->oem_options[0], "platform-model-name"))
    {
      if (_get_dell_system_info_long_string (state_data,
                                             IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_PLATFORM_MODEL_NAME,
                                             string,
                                             IPMI_OEM_DELL_SYSTEM_INFO_MAX_STRING_BYTES) < 0)
        goto cleanup;

      pstdout_printf (state_data->pstate,
		      "%s\n",
		      string);
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "slot-number")
	   || !strcasecmp (state_data->prog_data->args->oem_options[0], "blade-slot-info"))
    {
      if (_get_dell_system_info_long_string (state_data,
                                             IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_SLOT_NUMBER,
                                             string,
                                             IPMI_OEM_DELL_SYSTEM_INFO_MAX_STRING_BYTES) < 0)
        goto cleanup;
      
      pstdout_printf (state_data->pstate,
		      "%s\n",
		      string);
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "system-revision"))
    {
      if (_get_dell_system_info_bytes (state_data,
				       IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_SYSTEM_REVISION,
				       bytes,
				       IPMI_OEM_DELL_SYSTEM_INFO_MAX_STRING_BYTES,
				       1,
				       &bytes_len) < 0)
        goto cleanup;

      pstdout_printf (state_data->pstate,
		      "%u\n",
		      bytes[0]);
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "embedded-video-status"))
    {
      char *embedded_video_status_str;

      if (_get_dell_system_info_bytes (state_data,
				       IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_EMBEDDED_VIDEO_STATUS,
				       bytes,
				       IPMI_OEM_DELL_SYSTEM_INFO_MAX_STRING_BYTES,
				       1,
				       &bytes_len) < 0)
        goto cleanup;

      if (bytes[0] == IPMI_OEM_DELL_SYSTEM_INFO_EMBEDDED_VIDEO_STATUS_DISABLED)
	embedded_video_status_str = "Disabled";
      else if (bytes[0] == IPMI_OEM_DELL_SYSTEM_INFO_EMBEDDED_VIDEO_STATUS_ENABLED)
	embedded_video_status_str = "Enabled";
      else
	embedded_video_status_str = "Unknown";

      pstdout_printf (state_data->pstate,
		      "%s\n",
		      embedded_video_status_str);
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "idrac-info"))
    {
      if (_output_dell_system_info_idrac_info (state_data) < 0)
	goto cleanup;
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "idrac-ipv4-url"))
    {
      if (_get_dell_system_info_long_string (state_data,
                                             IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_IDRAC_IPV4_URL,
                                             string,
                                             IPMI_OEM_DELL_SYSTEM_INFO_MAX_STRING_BYTES) < 0)
        goto cleanup;
      
      pstdout_printf (state_data->pstate,
		      "%s\n",
		      string);
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "idrac-gui-webserver-control"))
    {
      char *idrac_web_gui_server_control_str;
 
      if (_get_dell_system_info_bytes (state_data,
				       IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_IDRAC_GUI_WEBSERVER_CONTROL,
				       bytes,
				       IPMI_OEM_DELL_SYSTEM_INFO_MAX_STRING_BYTES,
				       1,
				       &bytes_len) < 0)
        goto cleanup;

      if (bytes[0] == IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_WEB_GUI_SERVER_CONTROL_DISABLED)
	idrac_web_gui_server_control_str = "Disabled";
      else if (bytes[1] == IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_WEB_GUI_SERVER_CONTROL_ENABLED)
	idrac_web_gui_server_control_str = "Enabled";
      else
	idrac_web_gui_server_control_str = "Unknown";
      
      pstdout_printf (state_data->pstate,
		      "%s\n",
		      idrac_web_gui_server_control_str);
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "cmc-info"))
    {
      if (_output_dell_system_info_cmc_info (state_data) < 0)
	goto cleanup;
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "cmc-ipv4-url"))
    {
      if (_get_dell_system_info_long_string (state_data,
                                             IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_CMC_IPV4_URL,
                                             string,
                                             IPMI_OEM_DELL_SYSTEM_INFO_MAX_STRING_BYTES) < 0)
        goto cleanup;
      
      pstdout_printf (state_data->pstate,
		      "%s\n",
		      string);
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "cmc-ipv6-info"))
    {
      if (_output_dell_system_info_cmc_ipv6_info (state_data) < 0)
	goto cleanup;
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "cmc-ipv6-url"))
    {
      if (_get_dell_system_info_long_string (state_data,
                                             IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_CMC_IPV6_URL,
                                             string,
                                             IPMI_OEM_DELL_SYSTEM_INFO_MAX_STRING_BYTES) < 0)
        goto cleanup;
      
      pstdout_printf (state_data->pstate,
		      "%s\n",
		      string);
    }
#if 0
/* cannot verify */
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "snmp-ipv6-info"))
    {
      if (_output_dell_system_info_snmp_ipv6_info (state_data) < 0)
	goto cleanup;
    }
#endif
  else /* (!strcasecmp (state_data->prog_data->args->oem_options[0], "mac-addresses")) */
    {
      uint8_t idrac_type = 0;
      int ret;
      
      if ((ret = _get_dell_system_info_idrac_info (state_data,
						   NULL,
						   NULL,
						   0,
						   NULL,
						   0,
						   &idrac_type)) < 0)
	goto cleanup;

      if (ret)
	{
	  /* iDRAC 10g */
	  if (idrac_type == IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_IDRAC_TYPE_10G)
	    {
	      if (_output_dell_system_info_10g_mac_addresses (state_data) < 0)
		goto cleanup;
	    }
	  /* iDRAC 11g or iDRAC 12g */
	  else if (idrac_type == IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_IDRAC_TYPE_11G_MONOLITHIC
		   || idrac_type == IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_IDRAC_TYPE_11G_MODULAR
		   || idrac_type == IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_IDRAC_TYPE_12G_MONOLITHIC
		   || idrac_type == IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_INFO_IDRAC_TYPE_12G_MODULAR)
	    {
	      if (_output_dell_system_info_11g_or_12g_mac_addresses (state_data) < 0)
		goto cleanup;
	    }
	  else
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "Unrecognized iDRAC system %02Xh\n",
			       idrac_type);
	      goto cleanup;
	    }
	}
      else
	{
	  /* assume iDRAC 10g */
	  if (_output_dell_system_info_10g_mac_addresses (state_data) < 0)
	    goto cleanup;
	}

    }
 
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_nic_selection (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * Get NIC Selection Request
   *
   * 0x30 - OEM network function
   * 0x25 - OEM cmd
   * 
   * Get NIC Selection Response
   *
   * 0x25 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - NIC selection
   *      - 0x00 = shared
   *      - 0x01 = shared w/ failover to NIC2
   *      - 0x02 = dedicated
   *      - 0x03 = shared w/ failover to all
   */

  bytes_rq[0] = IPMI_CMD_OEM_DELL_GET_NIC_SELECTION;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
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
                                                   IPMI_CMD_OEM_DELL_GET_NIC_SELECTION,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  switch (bytes_rs[2])
    {
    case IPMI_OEM_DELL_NIC_SELECTION_SHARED:
      pstdout_printf (state_data->pstate, "shared\n");
      break;
    case IPMI_OEM_DELL_NIC_SELECTION_SHARED_WITH_FAILOVER_TO_NIC2:
      pstdout_printf (state_data->pstate, "shared with failover to NIC2\n");
      break;
    case IPMI_OEM_DELL_NIC_SELECTION_DEDICATED:
      pstdout_printf (state_data->pstate, "dedicated\n");
      break;
    case IPMI_OEM_DELL_NIC_SELECTION_SHARED_WITH_FAILOVER_TO_ALL_NICS:
      pstdout_printf (state_data->pstate, "shared with failover to all NICs\n");
      break;
    default:
      pstdout_printf (state_data->pstate, "unknown NIC selection: %Xh\n", bytes_rs[2]);
      break;
    }
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_set_nic_selection (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "dedicated")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "shared")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "shared_failover_nic2")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "shared_failover_all"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * Set NIC Selection Request
   *
   * 0x30 - OEM network function
   * 0x24 - OEM cmd
   * 0x?? - NIC selection
   *      - 0x00 = shared
   *      - 0x01 = shared w/ failover to NIC2
   *      - 0x02 = dedicated
   *      - 0x03 = shared w/ failover to all
   * 
   * Set NIC Selection Response
   *
   * 0x24 - OEM cmd
   * 0x?? - Completion Code
   */

  bytes_rq[0] = IPMI_CMD_OEM_DELL_SET_NIC_SELECTION;

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "shared"))
    bytes_rq[1] = IPMI_OEM_DELL_NIC_SELECTION_SHARED;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "shared_failover_nic2"))
    bytes_rq[1] = IPMI_OEM_DELL_NIC_SELECTION_SHARED_WITH_FAILOVER_TO_NIC2;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "dedicated"))
    bytes_rq[1] = IPMI_OEM_DELL_NIC_SELECTION_DEDICATED;
  else
    bytes_rq[1] = IPMI_OEM_DELL_NIC_SELECTION_SHARED_WITH_FAILOVER_TO_ALL_NICS;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              2, /* num bytes */
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
                                                   2, /* don't care about the 3rd byte, don't know what it is used for */
                                                   IPMI_CMD_OEM_DELL_SET_NIC_SELECTION,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_nic_selection_failover (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  char *nic_str;
  char *failover_str;
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * Get NIC Selection Failover Request
   *
   * 0x29 - OEM network function
   * 0x25 - OEM cmd
   * 
   * Get NIC Selection Failover Response
   *
   * 0x29 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - NIC selection
   *      - 0x01 = dedicated
   *      - 0x02 = lom1
   *      - 0x03 = lom2
   *      - 0x04 = lom3
   *      - 0x05 = lom4
   * 0x?? - NIC failover
   *      - 0x00 = none
   *      - 0x02 = lom1
   *      - 0x03 = lom2
   *      - 0x04 = lom3
   *      - 0x05 = lom4
   *      - 0x05 = all
   */

  bytes_rq[0] = IPMI_CMD_OEM_DELL_GET_NIC_SELECTION_FAILOVER;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
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
                                                   4,
                                                   IPMI_CMD_OEM_DELL_GET_NIC_SELECTION_FAILOVER,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  switch (bytes_rs[2])
    {
    case IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_NIC_DEDICATED:
      nic_str = "dedicated";
      break;
    case IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_NIC_LOM1:
      nic_str = "lom1";
      break;
    case IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_NIC_LOM2:
      nic_str = "lom2";
      break;
    case IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_NIC_LOM3:
      nic_str = "lom3";
      break;
    case IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_NIC_LOM4:
      nic_str = "lom4";
      break;
    default:
      nic_str = "unknown";
      break;
    }

  switch (bytes_rs[3])
    {
    case IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_FAILOVER_NONE:
      failover_str = "none";
      break;
    case IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_FAILOVER_LOM1:
      failover_str = "lom1";
      break;
    case IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_FAILOVER_LOM2:
      failover_str = "lom2";
      break;
    case IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_FAILOVER_LOM3:
      failover_str = "lom3";
      break;
    case IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_FAILOVER_LOM4:
      failover_str = "lom4";
      break;
    case IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_FAILOVER_ALL:
      failover_str = "all";
      break;
    default:
      failover_str = "unknown";
      break;
    }
  
  pstdout_printf (state_data->pstate,
		  "NIC selection        : %s",
		  nic_str);

  pstdout_printf (state_data->pstate,
		  "NIC failover network : %s",
		  failover_str);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_set_nic_selection_failover (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 2);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "dedicated")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "lom1")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "lom2")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "lom3")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "lom4"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  if (strcasecmp (state_data->prog_data->args->oem_options[1], "none")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "lom1")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "lom2")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "lom3")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "lom4")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "all"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }

  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * Set NIC Selection Failover Request
   *
   * 0x28 - OEM network function
   * 0x24 - OEM cmd
   * 0x?? - NIC selection
   *      - 0x01 = dedicated
   *      - 0x02 = lom1
   *      - 0x03 = lom2
   *      - 0x04 = lom3
   *      - 0x05 = lom4
   * 0x?? - NIC failover
   *      - 0x00 = none
   *      - 0x02 = lom1
   *      - 0x03 = lom2
   *      - 0x04 = lom3
   *      - 0x05 = lom4
   *      - 0x05 = all
   * 
   * Set NIC Selection Failover Response
   *
   * 0x28 - OEM cmd
   * 0x?? - Completion Code
   */

  bytes_rq[0] = IPMI_CMD_OEM_DELL_SET_NIC_SELECTION_FAILOVER;

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "dedicated"))
    bytes_rq[1] = IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_NIC_DEDICATED;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "lom1"))
    bytes_rq[1] = IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_NIC_LOM1;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "lom2"))
    bytes_rq[1] = IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_NIC_LOM2;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "lom3"))
    bytes_rq[1] = IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_NIC_LOM3;
  else /* !strcasecmp (state_data->prog_data->args->oem_options[0], "lom4") */
    bytes_rq[1] = IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_NIC_LOM4;

  if (!strcasecmp (state_data->prog_data->args->oem_options[1], "none"))
    bytes_rq[1] = IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_FAILOVER_NONE;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "lom1"))
    bytes_rq[1] = IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_FAILOVER_LOM1;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "lom2"))
    bytes_rq[1] = IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_FAILOVER_LOM2;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "lom3"))
    bytes_rq[1] = IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_FAILOVER_LOM3;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "lom4"))
    bytes_rq[1] = IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_FAILOVER_LOM4;
  else /* !strcasecmp (state_data->prog_data->args->oem_options[1], "all") */
    bytes_rq[1] = IPMI_OEM_DELL_NIC_SELECTION_FAILOVER_FAILOVER_ALL;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
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
                                                   2,
                                                   IPMI_CMD_OEM_DELL_SET_NIC_SELECTION_FAILOVER,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_oem_dell_get_active_lom_status (ipmi_oem_state_data_t *state_data,
				      uint8_t *bytes_rs,
				      unsigned int bytes_rs_len,
				      uint8_t sub_command)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (bytes_rs);
  assert (bytes_rs_len);
  assert (sub_command == IPMI_OEM_DELL_GET_ACTIVE_LOM_STATUS_GET_STATUS
	  || sub_command == IPMI_OEM_DELL_GET_ACTIVE_LOM_STATUS_LINK_STATUS
	  || sub_command == IPMI_OEM_DELL_GET_ACTIVE_LOM_STATUS_LINK_SPEED
	  || sub_command == IPMI_OEM_DELL_GET_ACTIVE_LOM_STATUS_LINK_MODE);

  bytes_rq[0] = IPMI_CMD_OEM_DELL_GET_ACTIVE_LOM_STATUS;
  bytes_rq[1] = sub_command;
  bytes_rq[2] = 0;
  bytes_rq[3] = 0;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              4, /* num bytes */
                              bytes_rs,
			      bytes_rs_len)) < 0)
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
                                                   4,
                                                   IPMI_CMD_OEM_DELL_GET_ACTIVE_LOM_STATUS,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

/* Dell Poweredge R610/R710 (11G) */
static int
_ipmi_oem_dell_get_active_lom_status_v1 (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rv = -1;

  assert (state_data);

  if (_ipmi_oem_dell_get_active_lom_status (state_data,
					    bytes_rs,
					    IPMI_OEM_MAX_BYTES,
					    IPMI_OEM_DELL_GET_ACTIVE_LOM_STATUS_GET_STATUS) < 0)
    goto cleanup;

  switch (bytes_rs[2])
    {
    case IPMI_OEM_DELL_LOM_STATUS_NO_ACTIVE_LOM:
      pstdout_printf (state_data->pstate, "dedicated\n");
      break;
    case IPMI_OEM_DELL_LOM_STATUS_LOM_1:
      pstdout_printf (state_data->pstate, "LOM 1\n");
      break;
    case IPMI_OEM_DELL_LOM_STATUS_LOM_2:
      pstdout_printf (state_data->pstate, "LOM 2\n");
      break;
    case IPMI_OEM_DELL_LOM_STATUS_LOM_3:
      pstdout_printf (state_data->pstate, "LOM 3\n");
      break;
    case IPMI_OEM_DELL_LOM_STATUS_LOM_4:
      pstdout_printf (state_data->pstate, "LOM 4\n");
      break;
    default:
      pstdout_printf (state_data->pstate, "unknown active LOM\n");
      break;
    }
  
  rv = 0;
 cleanup:
  return (rv);
}

/* Dell Poweredge R720 (12G) */
static int
_ipmi_oem_dell_get_active_lom_status_v2 (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t active_lom_status;
  char *active_lom_status_str;
  uint8_t link_status;
  char *link_status_str;
  uint8_t link_speed;
  char *link_speed_str;
  uint8_t link_mode;
  char *link_mode_str;
  int rv = -1;

  assert (state_data);

  if (_ipmi_oem_dell_get_active_lom_status (state_data,
					    bytes_rs,
					    IPMI_OEM_MAX_BYTES,
					    IPMI_OEM_DELL_GET_ACTIVE_LOM_STATUS_GET_STATUS) < 0)
    goto cleanup;

  active_lom_status = bytes_rs[2];

  switch (active_lom_status)
    {
    case IPMI_OEM_DELL_LOM_STATUS_NO_ACTIVE_LOM:
      active_lom_status_str = "No LOM active";
      break;
    case IPMI_OEM_DELL_LOM_STATUS_LOM_1:
      active_lom_status_str = "LOM 1";
      break;
    case IPMI_OEM_DELL_LOM_STATUS_LOM_2:
      active_lom_status_str = "LOM 2";
      break;
    case IPMI_OEM_DELL_LOM_STATUS_LOM_3:
      active_lom_status_str = "LOM 3";
      break;
    case IPMI_OEM_DELL_LOM_STATUS_LOM_4:
      active_lom_status_str = "LOM 4";
      break;
    case IPMI_OEM_DELL_LOM_STATUS_DEDICATED:
      active_lom_status_str = "Dedicated";
      break;
    default:
      active_lom_status_str = "Unknown";
      break;
    }

  if (_ipmi_oem_dell_get_active_lom_status (state_data,
					    bytes_rs,
					    IPMI_OEM_MAX_BYTES,
					    IPMI_OEM_DELL_GET_ACTIVE_LOM_STATUS_LINK_STATUS) < 0)
    goto cleanup;

  link_status = (bytes_rs[3] & IPMI_OEM_DELL_LOM_LINK_STATUS_BITMASK);
  link_status >>= IPMI_OEM_DELL_LOM_LINK_STATUS_SHIFT;

  switch (link_status)
    {
    case IPMI_OEM_DELL_LOM_LINK_STATUS_NO_LINK:
      link_status_str = "No Link";
      break;
    case IPMI_OEM_DELL_LOM_LINK_STATUS_LINK_PRESENT:
      link_status_str = "Link Present";
      break;
    default:
      link_status_str = "Unknown";
      break;
    }

  if (_ipmi_oem_dell_get_active_lom_status (state_data,
					    bytes_rs,
					    IPMI_OEM_MAX_BYTES,
					    IPMI_OEM_DELL_GET_ACTIVE_LOM_STATUS_LINK_SPEED) < 0)
    goto cleanup;

  link_speed = (bytes_rs[3] & IPMI_OEM_DELL_LOM_LINK_SPEED_BITMASK);
  link_speed >>= IPMI_OEM_DELL_LOM_LINK_SPEED_SHIFT;

  switch (link_speed)
    {
    case IPMI_OEM_DELL_LOM_LINK_SPEED_10MBPS:
      link_speed_str = "10 Mbps";
      break;
    case IPMI_OEM_DELL_LOM_LINK_SPEED_100MBPS:
      link_speed_str = "100 Mbps";
      break;
    case IPMI_OEM_DELL_LOM_LINK_SPEED_1GBPS:
      link_speed_str = "1 Gbps";
      break;
    default:
      link_speed_str = "Unknown";
      break;
    }

  if (_ipmi_oem_dell_get_active_lom_status (state_data,
					    bytes_rs,
					    IPMI_OEM_MAX_BYTES,
					    IPMI_OEM_DELL_GET_ACTIVE_LOM_STATUS_LINK_MODE) < 0)
    goto cleanup;

  link_mode = (bytes_rs[3] & IPMI_OEM_DELL_LOM_LINK_MODE_BITMASK);
  link_mode >>= IPMI_OEM_DELL_LOM_LINK_MODE_SHIFT;

  switch (link_mode)
    {
    case IPMI_OEM_DELL_LOM_LINK_MODE_HALF_DUPLEX:
      link_mode_str = "Half duplex";
      break;
    case IPMI_OEM_DELL_LOM_LINK_MODE_FULL_DUPLEX:
      link_mode_str = "Full duplex";
      break;
    default:
      link_mode_str = "Unknown";
      break;
    }

  pstdout_printf (state_data->pstate,
		  "Active LOM  : %s\n",
		  active_lom_status_str);

  pstdout_printf (state_data->pstate,
		  "Link Status : %s\n",
		  link_status_str);

  pstdout_printf (state_data->pstate,
		  "Link Speed  : %s\n",
		  link_speed_str);

  pstdout_printf (state_data->pstate,
		  "Link Mode   : %s\n",
		  link_mode_str);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_active_lom_status (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * Get Active LOM Status
   *
   * 0x30 - OEM network function
   * 0xC1 - OEM cmd
   * 0x?? - subcommand
   *        - (11G/12G) - 0h - active lom
   *        - (12G) - 1h - link status  
   *        - (12G) - 2h - link speed
   *        - (12G) - 3h - link mode
   * 0x?? - reserved
   * 0x?? - reserved
   * 
   * Get NIC Selection Response
   *
   * 0xC1 - OEM cmd
   * 0x?? - Completion Code
   * if subcommand active lom
   *   0x?? - 0x00 = No Lom Active (i.e. dedicated)
   *        - 0x01 = LOM 1
   *        - 0x02 = LOM 2
   *        - 0x03 = LOM 3
   *        - 0x04 = LOM 4
   *        - 0x05 = dedicated (12G) 
   *        - 0xff = unknown
   *   0x?? - reserved
   *   0x?? - reserved
   * if subcommand link status
   *   0x?? - 0x00 = No Lom Active (i.e. dedicated)
   *        - 0x01 = LOM 1
   *        - 0x02 = LOM 2
   *        - 0x03 = LOM 3
   *        - 0x04 = LOM 4
   *        - 0x05 = dedicated (12G) 
   *        - 0xff = unknown
   *   0x?? - bits 0:3 - link status
   *        - 0 = no link
   *        - 1 = link present
   *        - bits 4:7 - reserved
   *   0x?? - reserved
   * if subcommand link speed
   *   0x?? - 0x00 = No Lom Active (i.e. dedicated)
   *        - 0x01 = LOM 1
   *        - 0x02 = LOM 2
   *        - 0x03 = LOM 3
   *        - 0x04 = LOM 4
   *        - 0x05 = dedicated (12G) 
   *        - 0xff = unknown
   *   0x?? - bits 0:3 - link speed
   *        - 00h = 10 mbps 
   *        - 01h = 100 mbps
   *        - 02h = 1 gbps
   *        - 0Fh = unknown
   *        - bits 4:7 - reserved
   *   0x?? - reserved
   * if subcommand link mode
   *   0x?? - 0x00 = No Lom Active (i.e. dedicated)
   *        - 0x01 = LOM 1
   *        - 0x02 = LOM 2
   *        - 0x03 = LOM 3
   *        - 0x04 = LOM 4
   *        - 0x05 = dedicated (12G) 
   *        - 0xff = unknown
   *   0x?? - bits 0:3 - link mode
   *        - 00h = half duplex
   *        - 01h = full duplex
   *        - 0Fh = unknown
   *        - bits 4:7 - reserved
   *   0x?? - reserved
   */

  if (!state_data->prog_data->args->oem_options_count)
    return (_ipmi_oem_dell_get_active_lom_status_v1 (state_data));

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "v1")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "v2"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      return (-1);
    }

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "v1"))
    return (_ipmi_oem_dell_get_active_lom_status_v1 (state_data));
  return (_ipmi_oem_dell_get_active_lom_status_v2 (state_data));
}

static int
_dell_reserve_extended_configuration (ipmi_oem_state_data_t *state_data,
				      uint8_t *reservation_id)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  /* Dell OEM
   *
   * Reserve Extended Configuration Request
   *
   * 0x2E - OEM Group (is IPMI_NET_FN_OEM_GROUP_RQ)
   * 0x01 - OEM cmd
   * 0x?? - Dell IANA (LSB first)
   * 0x?? - Dell IANA
   * 0x?? - Dell IANA
   *
   * Reserve Extended Configuration Response
   *
   * 0x01 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - Dell IANA (LSB first)
   * 0x?? - Dell IANA
   * 0x?? - Dell IANA
   * 0x?? - reservation id
   */

  assert (state_data);
  assert (reservation_id);

  bytes_rq[0] = IPMI_CMD_OEM_DELL_RESERVED_EXTENDED_CONFIGURATION;
  bytes_rq[1] = (IPMI_IANA_ENTERPRISE_ID_DELL & 0x0000FF);
  bytes_rq[2] = (IPMI_IANA_ENTERPRISE_ID_DELL & 0x00FF00) >> 8;
  bytes_rq[3] = (IPMI_IANA_ENTERPRISE_ID_DELL & 0xFF0000) >> 16;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
                              bytes_rq, /* data */
                              4, /* num bytes */
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
                                                   IPMI_CMD_OEM_DELL_RESERVED_EXTENDED_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_GROUP_RS,
                                                   NULL) < 0)
    goto cleanup;
  
  (*reservation_id) = bytes_rs[5];

  rv = 0;
 cleanup:
  return (rv);
}

static int
_dell_extended_oem_strerror (ipmi_oem_state_data_t *state_data,
			     uint8_t comp_code,
			     uint8_t cmd,
			     uint8_t netfn,
			     char *errbuf,
			     unsigned int errbuflen)
{
  assert (state_data);
  assert (comp_code != IPMI_COMP_CODE_COMMAND_SUCCESS);

  switch (netfn)
    {
    case IPMI_NET_FN_OEM_GROUP_RQ:
    case IPMI_NET_FN_OEM_GROUP_RS:
      switch (cmd)
        {
	case IPMI_CMD_OEM_DELL_GET_EXTENDED_CONFIGURATION:
	case IPMI_CMD_OEM_DELL_SET_EXTENDED_CONFIGURATION:
          switch (comp_code)
            {
	    case IPMI_COMP_CODE_OEM_DELL_NOT_LICENSED:
	      snprintf (errbuf, errbuflen, "%s", IPMI_COMP_CODE_OEM_DELL_NOT_LICENSED_STR);
	      return (1);
	      break;
            }
          break;
        }
      break;
    case IPMI_NET_FN_OEM_DELL_GENERIC_RQ:
    case IPMI_NET_FN_OEM_DELL_GENERIC_RS:
      switch (cmd)
	{
	case IPMI_CMD_OEM_DELL_POWER_MONITORING_OVER_A_SPECIFIED_AVERAGING_INTERVAL2:
	case IPMI_CMD_OEM_DELL_POWER_MONITORING_AVERAGING_INTERVAL_RANGE2:
	  switch (comp_code)
	    {
	    case IPMI_COMP_CODE_OEM_DELL_SUBSYSTEM_LEVEL_POWER_IS_NOT_SUPPORTED:
	      snprintf (errbuf, errbuflen, "%s", IPMI_COMP_CODE_OEM_DELL_SUBSYSTEM_LEVEL_POWER_IS_NOT_SUPPORTED_STR);
	      return (1);
	      break;
	    }
	  break;
	}
      break;
    }
  
  return (0);
}

static int
_dell_get_extended_configuration (ipmi_oem_state_data_t *state_data,
				  uint8_t token_id,
				  uint8_t *token_data,
				  unsigned int token_data_len,
				  unsigned int expected_valid_field_mask,
				  unsigned int *token_data_read)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_read;
  uint8_t expected_bytes_read;
  uint16_t token_len = 0;
  uint8_t token_version; 
  uint16_t valid_field_mask;
  uint8_t reservation_id = 0;
  uint16_t offset = 0;
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (token_data);
  assert (token_data_len);
  assert (expected_valid_field_mask);
  assert (token_data_read);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * Get Extended Configuration Request
   *
   * 0x2E - OEM network function (is IPMI_NET_FN_OEM_GROUP_RQ)
   * 0x02 - OEM cmd
   * 0x?? - Dell IANA (LSB first)
   * 0x?? - Dell IANA
   * 0x?? - Dell IANA
   * 0x?? - reservation id
   * 0x?? - token ID
   * 0x?? - index (used by index objects only)
   * 0x?? - data offset - LSB
   * 0x?? - data offset - MSB
   * 0x?? - bytes to read (1 based, 0xFF = all)
   *
   * Get Extended Configuration Response
   *
   * 0x02 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - Dell IANA (LSB first)
   * 0x?? - Dell IANA
   * 0x?? - Dell IANA
   * 0x?? - token ID
   * 0x?? - index (used by index objects only)
   * 0x?? - bytes of data returned (1 based)
   * 0x??+ - token data
   */

  while (offset < 0xFFFF)
    {
      if (_dell_reserve_extended_configuration (state_data,
						&reservation_id) < 0)
	goto cleanup;

      bytes_rq[0] = IPMI_CMD_OEM_DELL_GET_EXTENDED_CONFIGURATION;
      bytes_rq[1] = (IPMI_IANA_ENTERPRISE_ID_DELL & 0x0000FF);
      bytes_rq[2] = (IPMI_IANA_ENTERPRISE_ID_DELL & 0x00FF00) >> 8;
      bytes_rq[3] = (IPMI_IANA_ENTERPRISE_ID_DELL & 0xFF0000) >> 16;
      bytes_rq[4] = reservation_id;
      bytes_rq[5] = token_id;
      bytes_rq[6] = 0x00;
      bytes_rq[7] = (offset & 0x00FF);
      bytes_rq[8] = (offset & 0xFF00) >> 8;
      bytes_rq[9] = IPMI_OEM_DELL_EXTENDED_CONFIG_READ_ALL_BYTES;

      if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
				  0, /* lun */
				  IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
				  bytes_rq, /* data */
				  10, /* num bytes */
				  bytes_rs,
				  IPMI_OEM_MAX_BYTES)) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_raw: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  goto cleanup;
	}
      
      /* atleast 8 bytes of non-token-data + common header */
      if (ipmi_oem_check_response_and_completion_code (state_data,
						       bytes_rs,
						       rs_len,
						       8 + IPMI_OEM_DELL_TOKEN_DATA_COMMON_HEADER_LEN,
						       IPMI_CMD_OEM_DELL_GET_EXTENDED_CONFIGURATION,
						       IPMI_NET_FN_OEM_GROUP_RS,
                                                       _dell_extended_oem_strerror) < 0)
	goto cleanup;
      
      bytes_read = bytes_rs[7];
      expected_bytes_read = rs_len - 8;

      if (bytes_read != expected_bytes_read)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "invalid bytes read returned: expected = %u, returned %u\n",
			   bytes_read, expected_bytes_read);
	  goto cleanup;
	}
      
      /* check token common header */
      
      if (!offset)
	{
	  token_len = bytes_rs[8];
	  token_len |= (bytes_rs[9] << 8);
	  
	  token_version = bytes_rs[10];
	  
	  if (token_version != IPMI_OEM_DELL_TOKEN_VERSION)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "invalid token version returned: expected = %Xh, returned %Xh\n",
			       IPMI_OEM_DELL_TOKEN_VERSION, token_version);
	      goto cleanup;
	    }

	  valid_field_mask = bytes_rs[11];
	  valid_field_mask |= (bytes_rs[12] << 8);
	  
	  if (valid_field_mask != expected_valid_field_mask)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "invalid field mask returned: expected = %Xh, returned %Xh\n",
			       expected_valid_field_mask, valid_field_mask);
	      goto cleanup;
	    }
	}

      if ((offset + bytes_read) > token_data_len)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "buffer overflow: offset + bytes_read = %u\n",
			   (offset + bytes_read));
	  goto cleanup;
	}
      
      memcpy (token_data + offset,
	      &bytes_rs[8],
	      bytes_read);

      offset += bytes_read;

      if (token_len <= offset)
	break;
    }
  
  if (offset != token_len)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "invalid token length returned: expected = %u, read = %u\n",
		       token_len, offset);
      goto cleanup;
    }

  (*token_data_read) = offset;

  rv = 0;
 cleanup:
  return (rv);
}

static int
_dell_set_extended_configuration (ipmi_oem_state_data_t *state_data,
				  uint8_t token_id,
				  uint8_t *token_data,
				  unsigned int token_data_len,
				  unsigned int valid_field_mask)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint16_t token_len; 
  uint8_t reservation_id = 0;
  uint16_t offset = 0;
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (token_data);
  assert (token_data_len);
  assert (valid_field_mask);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * Set Extended Configuration Request
   *
   * 0x2E - OEM network function (is IPMI_NET_FN_OEM_GROUP_RQ)
   * 0x03 - OEM cmd
   * 0x?? - Dell IANA (LSB first)
   * 0x?? - Dell IANA
   * 0x?? - Dell IANA
   * 0x?? - reservation id
   * 0x?? - token ID
   * 0x?? - index (used by index objects only)
   * 0x?? - data offset - LSB
   * 0x?? - data offset - MSB
   * 0x?? - [7:4] - reserved
   *      - [3:0] - in progress
   *              - 0 in progress
   *              - 1 last token data being transfered in this request
   * 0x??+ - token data
   *
   * Response
   *
   * 0x03 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - Dell IANA (LSB first)
   * 0x?? - Dell IANA
   * 0x?? - Dell IANA
   * 0x?? - bytes written
   */

  token_len = IPMI_OEM_DELL_TOKEN_DATA_COMMON_HEADER_LEN + token_data_len;
	  
  while (offset < 0xFFFF)
    {
      unsigned int write_length = 0;
      unsigned int token_write_length = 0;

      if (_dell_reserve_extended_configuration (state_data,
						&reservation_id) < 0)
	goto cleanup;

      /* bytes_written response is 1 byte, so presumably you can write
       * up to 255 bytes of data.  However, IPMI over LAN has a max
       * payload length of 255 total.  At this point stage of the API,
       * there's no way to know what is legit to use.  So we round
       * down IPMI_OEM_DELL_TOKEN_WRITE_MAX to some reasonable
       * guestimate of what is good.
       */

      bytes_rq[0] = IPMI_CMD_OEM_DELL_SET_EXTENDED_CONFIGURATION;
      bytes_rq[1] = (IPMI_IANA_ENTERPRISE_ID_DELL & 0x0000FF);
      bytes_rq[2] = (IPMI_IANA_ENTERPRISE_ID_DELL & 0x00FF00) >> 8;
      bytes_rq[3] = (IPMI_IANA_ENTERPRISE_ID_DELL & 0xFF0000) >> 16;
      bytes_rq[4] = reservation_id;
      bytes_rq[5] = token_id;
      bytes_rq[6] = 0x00;
      bytes_rq[7] = (offset & 0x00FF);
      bytes_rq[8] = (offset & 0xFF00) >> 8;

      if (!offset)
	{
	  /* common header */
	  
	  bytes_rq[10] = (token_len & 0x00FF);
	  bytes_rq[11] = (token_len & 0xFF00) >> 8;
	  bytes_rq[12] = IPMI_OEM_DELL_TOKEN_VERSION;
	  bytes_rq[13] = (valid_field_mask & 0x00FF); 
	  bytes_rq[14] = (valid_field_mask & 0xFF00) >> 8; 

	  /* - 5 for the common header */
	  if ((token_data_len - offset) > (IPMI_OEM_DELL_TOKEN_WRITE_MAX - IPMI_OEM_DELL_TOKEN_DATA_COMMON_HEADER_LEN))
	    write_length = IPMI_OEM_DELL_TOKEN_WRITE_MAX - IPMI_OEM_DELL_TOKEN_DATA_COMMON_HEADER_LEN;
	  else
	    write_length = (token_data_len - offset);
	  
	  memcpy (&bytes_rq[15],
		  token_data + offset,
		  write_length);

	  token_write_length = IPMI_OEM_DELL_TOKEN_DATA_COMMON_HEADER_LEN + write_length;
	}
      else
	{
	  if ((token_data_len - offset) > IPMI_OEM_DELL_TOKEN_WRITE_MAX)
	    write_length = IPMI_OEM_DELL_TOKEN_WRITE_MAX;
	  else
	    write_length = (token_data_len - offset);

	  memcpy (&bytes_rq[10],
		  token_data + offset,
		  write_length);

	  token_write_length = write_length;
	}

      if (token_len <= (offset + token_write_length))
        bytes_rq[9] = IPMI_OEM_DELL_EXTENDED_CONFIG_LAST_TOKEN;
      else
        bytes_rq[9] = IPMI_OEM_DELL_EXTENDED_CONFIG_IN_PROGRESS;

      if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
				  0, /* lun */
				  IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
				  bytes_rq, /* data */
				  10 + token_write_length, /* num bytes */
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
						       IPMI_CMD_OEM_DELL_SET_EXTENDED_CONFIGURATION,
						       IPMI_NET_FN_OEM_GROUP_RS,
                                                       _dell_extended_oem_strerror) < 0)
	goto cleanup;
      
      if (bytes_rs[5] != token_write_length)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "invalid data length written: expected = %u, returned %u\n",
			   token_write_length, bytes_rs[5]);
	  goto cleanup;
	}

      offset += bytes_rs[5];

      if (token_len <= offset)
	break;
    }

  if (offset != token_len)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "invalid token length written: expected = %u, write = %u\n",
		       token_len, offset);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_ssh_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t token_data[IPMI_OEM_DELL_TOKEN_DATA_MAX];
  uint16_t expected_valid_field_mask = IPMI_OEM_DELL_EXTENDED_CONFIG_SSH_CONFIGURATION_ALL_FIELD_MASK;
  unsigned int token_data_read = 0;
  unsigned int expected_token_data_read = 0;
  uint8_t sshenable;
  uint8_t maxconnections;
  uint8_t activeconnections;
  uint32_t idletimeout;
  uint16_t portnumber;
  int rv = -1;

  /* Dell OEM
   *
   * SSH Token Data - Token ID 0Ah
   *
   * Common Header
   *
   * byte 1 - total size of token data (including common header) - LSB
   * byte 2 - total size of token data (including common header) - MSB
   * byte 3 - token version (0x01)
   * byte 4 - valid field mask (LSB)
   * byte 5 - valid field mask (MSB)
   *
   * SSH data
   *
   * byte 6 - SSHEnable
   * byte 7 - MaxConnections (read only)
   * byte 8 - ActiveConnections (read only)
   * byte 9 - 12 - IdleTimeout
   * byte 13 - 14 - PortNumber
   */

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_dell_get_extended_configuration (state_data,
					IPMI_OEM_DELL_TOKEN_ID_SSH_CONFIGURATION,
					token_data,
					IPMI_OEM_DELL_TOKEN_DATA_MAX,
					expected_valid_field_mask,
					&token_data_read) < 0)
    goto cleanup;

  expected_token_data_read = IPMI_OEM_DELL_TOKEN_DATA_COMMON_HEADER_LEN + 9;

  if (token_data_read != expected_token_data_read)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "invalid token data length returned: expected = %u, returned %u\n",
		       expected_token_data_read, token_data_read);
      goto cleanup;
    }

  sshenable = token_data[5];
  maxconnections = token_data[6];
  activeconnections = token_data[7];
  idletimeout = token_data[8];
  idletimeout |= (token_data[9] << 8);
  idletimeout |= (token_data[10] << 16);
  idletimeout |= (token_data[11] << 24);
  portnumber = token_data[12];
  portnumber |= (token_data[13] << 8);

  pstdout_printf (state_data->pstate,
		  "SSH                : %s\n",
		  (sshenable) ? "Enabled" : "Disabled");

  pstdout_printf (state_data->pstate,
		  "Max Connections    : %u\n",
		  maxconnections);

  pstdout_printf (state_data->pstate,
		  "Active Connections : %u\n",
		  activeconnections);

  pstdout_printf (state_data->pstate,
		  "Idle Timeout       : %u seconds\n",
		  idletimeout);

  pstdout_printf (state_data->pstate,
		  "Port Number        : %u\n",
		  portnumber);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_set_ssh_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t token_data[IPMI_OEM_DELL_TOKEN_DATA_MAX];
  uint16_t valid_field_mask = 0;
  uint8_t sshenable = 0;
  uint32_t idletimeout = 0;
  uint16_t portnumber = 0;
  int rv = -1;
  unsigned int i;

  /* Dell OEM
   *
   * SSH Token Data - Token ID 0Ah
   *
   * Common Header
   *
   * byte 1 - total size of token data (including common header) - LSB
   * byte 2 - total size of token data (including common header) - MSB
   * byte 3 - token version (0x01)
   * byte 4 - valid field mask (LSB)
   * byte 5 - valid field mask (MSB)
   *
   * SSH data
   *
   * byte 6 - SSHEnable
   * byte 7 - MaxConnections (read only)
   * byte 8 - ActiveConnections (read only)
   * byte 9 - 12 - IdleTimeout
   * byte 13 - 14 - PortNumber
   */

  assert (state_data);

  if (!state_data->prog_data->args->oem_options_count)
    {
      pstdout_printf (state_data->pstate,
		      "Option: ssh=enable|disable\n"
		      "Option: idletimeout=seconds\n"
		      "Option: portnumber=num\n");
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

      if (!strcasecmp (key, "ssh"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &sshenable) < 0)
            goto cleanup;
          
          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_SSH_CONFIGURATION_ENABLE_FIELD_MASK;
        }
      else if (!strcasecmp (key, "idletimeout"))
        {
          if (ipmi_oem_parse_4_byte_field (state_data, i, value, &idletimeout) < 0)
            goto cleanup;
          
          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_SSH_CONFIGURATION_IDLE_TIMEOUT_FIELD_MASK;
        }
      else if (!strcasecmp (key, "portnumber"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &portnumber) < 0)
            goto cleanup;
          
          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_SSH_CONFIGURATION_PORT_NUMBER_FIELD_MASK;
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
      
  token_data[0] = sshenable;
  token_data[1] = 0;            /* maxconnections is read only */
  token_data[2] = 0;            /* activeconnections is read only */
  token_data[3] = (idletimeout & 0x000000FF);
  token_data[4] = (idletimeout & 0x0000FF00) >> 8;
  token_data[5] = (idletimeout & 0x00FF0000) >> 16;
  token_data[6] = (idletimeout & 0xFF000000) >> 24;
  token_data[7] = (portnumber & 0x00FF);
  token_data[8] = (portnumber & 0xFF00) >> 8;

  if (_dell_set_extended_configuration (state_data,
					IPMI_OEM_DELL_TOKEN_ID_SSH_CONFIGURATION,
					token_data,
					9,
					valid_field_mask) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_telnet_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t token_data[IPMI_OEM_DELL_TOKEN_DATA_MAX];
  uint16_t expected_valid_field_mask = IPMI_OEM_DELL_EXTENDED_CONFIG_TELNET_CONFIGURATION_ALL_FIELD_MASK;
  unsigned int token_data_read = 0;
  unsigned int expected_token_data_read = 0;
  uint8_t telnetenable;
  uint8_t maxsessions;
  uint8_t activesessions;
  uint32_t sessiontimeout;
  uint16_t portnumber;
  uint8_t telnet7flsbackspace;
  int rv = -1;

  /* Dell OEM
   *
   * Telnet Token Data - Token ID 0Bh
   *
   * Common Header
   *
   * byte 1 - total size of token data (including common header) - LSB
   * byte 2 - total size of token data (including common header) - MSB
   * byte 3 - token version (0x01)
   * byte 4 - valid field mask (LSB)
   * byte 5 - valid field mask (MSB)
   *
   * Telnet data
   *
   * byte 6 - telnetenable
   * byte 7 - maxsessions (read only)
   * byte 8 - activesessions (read only)
   * byte 9 - 12 - sessiontimeout
   * byte 13 - 14 - portnumber
   * byte 15 - telnet7flsbackspace
   */

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_dell_get_extended_configuration (state_data,
					IPMI_OEM_DELL_TOKEN_ID_TELNET_CONFIGURATION,
					token_data,
					IPMI_OEM_DELL_TOKEN_DATA_MAX,
					expected_valid_field_mask,
					&token_data_read) < 0)
    goto cleanup;

  expected_token_data_read = IPMI_OEM_DELL_TOKEN_DATA_COMMON_HEADER_LEN + 10;

  if (token_data_read != expected_token_data_read)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "invalid token data length returned: expected = %u, returned %u\n",
		       expected_token_data_read, token_data_read);
      goto cleanup;
    }

  telnetenable = token_data[5];
  maxsessions = token_data[6];
  activesessions = token_data[7];
  sessiontimeout = token_data[8];
  sessiontimeout |= (token_data[9] << 8);
  sessiontimeout |= (token_data[10] << 16);
  sessiontimeout |= (token_data[11] << 24);
  portnumber = token_data[12];
  portnumber |= (token_data[13] << 8);
  telnet7flsbackspace = token_data[14];

  pstdout_printf (state_data->pstate,
		  "Telnet             : %s\n",
		  (telnetenable) ? "Enabled" : "Disabled");

  pstdout_printf (state_data->pstate,
		  "Max Sessions       : %u\n",
		  maxsessions);

  pstdout_printf (state_data->pstate,
		  "Active Sessions    : %u\n",
		  activesessions);

  pstdout_printf (state_data->pstate,
		  "Session Timeout    : %u seconds\n",
		  sessiontimeout);

  pstdout_printf (state_data->pstate,
		  "Port Number        : %u\n",
		  portnumber);

  /* 7 FLS backspace is apparently an alternate backspace char 
   * used in windows telnet implementations.
   */
  pstdout_printf (state_data->pstate,
		  "7 FLS Backspace    : %s\n",
		  (telnet7flsbackspace) ? "Enabled" : "Disabled");

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_set_telnet_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t token_data[IPMI_OEM_DELL_TOKEN_DATA_MAX];
  uint16_t valid_field_mask = 0;
  uint8_t telnetenable = 0;
  uint32_t sessiontimeout = 0;
  uint16_t portnumber = 0;
  uint8_t _7flsenable = 0;
  int rv = -1;
  unsigned int i;

  /* Dell OEM
   *
   * Telnet Token Data - Token ID 0Bh
   *
   * Common Header
   *
   * byte 1 - total size of token data (including common header) - LSB
   * byte 2 - total size of token data (including common header) - MSB
   * byte 3 - token version (0x01)
   * byte 4 - valid field mask (LSB)
   * byte 5 - valid field mask (MSB)
   *
   * Telnet data
   *
   * byte 6 - telnetenable
   * byte 7 - maxsessions (read only)
   * byte 8 - activesessions (read only)
   * byte 9 - 12 - sessiontimeout
   * byte 13 - 14 - portnumber
   * byte 15 - telnet7flsbackspace
   */

  assert (state_data);

  if (!state_data->prog_data->args->oem_options_count)
    {
      pstdout_printf (state_data->pstate,
		      "Option: telnet=enable|disable\n"
		      "Option: sessiontimeout=seconds\n"
		      "Option: portnumber=num\n"
		      "Option: 7fls=enable|disable");
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

      if (!strcasecmp (key, "telnet"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &telnetenable) < 0)
            goto cleanup;

          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_TELNET_CONFIGURATION_ENABLE_FIELD_MASK;
        }
      else if (!strcasecmp (key, "sessiontimeout"))
        {
          if (ipmi_oem_parse_4_byte_field (state_data, i, value, &sessiontimeout) < 0)
            goto cleanup;

          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_TELNET_CONFIGURATION_SESSION_TIMEOUT_FIELD_MASK;
        }
      else if (!strcasecmp (key, "portnumber"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &portnumber) < 0)
            goto cleanup;
          
          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_TELNET_CONFIGURATION_PORT_NUMBER_FIELD_MASK;
        }
      else if (!strcasecmp (key, "7fls"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &_7flsenable) < 0)
            goto cleanup;
          
          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_TELNET_CONFIGURATION_7FLS_BACKSPACE_FIELD_MASK;
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

  token_data[0] = telnetenable;
  token_data[1] = 0;            /* maxsessions is read only */
  token_data[2] = 0;            /* activesessions is read only */
  token_data[3] = (sessiontimeout & 0x000000FF);
  token_data[4] = (sessiontimeout & 0x0000FF00) >> 8;
  token_data[5] = (sessiontimeout & 0x00FF0000) >> 16;
  token_data[6] = (sessiontimeout & 0xFF000000) >> 24;
  token_data[7] = (portnumber & 0x00FF);
  token_data[8] = (portnumber & 0xFF00) >> 8;
  token_data[9] = _7flsenable;

  if (_dell_set_extended_configuration (state_data,
					IPMI_OEM_DELL_TOKEN_ID_TELNET_CONFIGURATION,
					token_data,
					10,
					valid_field_mask) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_web_server_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t token_data[IPMI_OEM_DELL_TOKEN_DATA_MAX];
  uint16_t expected_valid_field_mask = IPMI_OEM_DELL_EXTENDED_CONFIG_WEB_SERVER_CONFIGURATION_ALL_FIELD_MASK;
  unsigned int token_data_read = 0;
  unsigned int expected_token_data_read = 0;
  uint8_t webserverenable;
  uint8_t maxsessions;
  uint8_t activesessions;
  uint32_t sessiontimeout;
  uint16_t httpportnumber;
  uint16_t httpsportnumber;
  int rv = -1;

  /* Dell OEM
   *
   * Web Server Token Data - Token ID 0Ch
   *
   * Common Header
   *
   * byte 1 - total size of token data (including common header) - LSB
   * byte 2 - total size of token data (including common header) - MSB
   * byte 3 - token version (0x01)
   * byte 4 - valid field mask (LSB)
   * byte 5 - valid field mask (MSB)
   *
   * Web Server data
   *
   * byte 6 - webserverenable
   * byte 7 - maxsessions (read only)
   * byte 8 - activesessions (read only)
   * byte 9 - 12 - sessiontimeout
   * byte 13 - 14 - httpportnumber
   * byte 15 - 16 - httpsportnumber
   */

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_dell_get_extended_configuration (state_data,
					IPMI_OEM_DELL_TOKEN_ID_WEB_SERVER_CONFIGURATION,
					token_data,
					IPMI_OEM_DELL_TOKEN_DATA_MAX,
					expected_valid_field_mask,
					&token_data_read) < 0)
    goto cleanup;

  expected_token_data_read = IPMI_OEM_DELL_TOKEN_DATA_COMMON_HEADER_LEN + 11;

  if (token_data_read != expected_token_data_read)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "invalid token data length returned: expected = %u, returned %u\n",
		       expected_token_data_read, token_data_read);
      goto cleanup;
    }

  webserverenable = token_data[5];
  maxsessions = token_data[6];
  activesessions = token_data[7];
  sessiontimeout = token_data[8];
  sessiontimeout |= (token_data[9] << 8);
  sessiontimeout |= (token_data[10] << 16);
  sessiontimeout |= (token_data[11] << 24);
  httpportnumber = token_data[12];
  httpportnumber |= (token_data[13] << 8);
  httpsportnumber = token_data[14];
  httpsportnumber |= (token_data[15] << 8);

  pstdout_printf (state_data->pstate,
		  "Web Server         : %s\n",
		  (webserverenable) ? "Enabled" : "Disabled");

  pstdout_printf (state_data->pstate,
		  "Max Sessions       : %u\n",
		  maxsessions);

  pstdout_printf (state_data->pstate,
		  "Active Sessions    : %u\n",
		  activesessions);

  pstdout_printf (state_data->pstate,
		  "Session Timeout    : %u seconds\n",
		  sessiontimeout);

  pstdout_printf (state_data->pstate,
		  "http Port Number   : %u\n",
		  httpportnumber);

  pstdout_printf (state_data->pstate,
		  "https Port Number  : %u\n",
		  httpsportnumber);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_set_web_server_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t token_data[IPMI_OEM_DELL_TOKEN_DATA_MAX];
  uint16_t valid_field_mask = 0;
  uint8_t webserverenable = 0;
  uint32_t sessiontimeout = 0;
  uint16_t httpportnumber = 0;
  uint16_t httpsportnumber = 0;
  int rv = -1;
  unsigned int i;

  /* Dell OEM
   *
   * Web Server Token Data - Token ID 0Ch
   *
   * Common Header
   *
   * byte 1 - total size of token data (including common header) - LSB
   * byte 2 - total size of token data (including common header) - MSB
   * byte 3 - token version (0x01)
   * byte 4 - valid field mask (LSB)
   * byte 5 - valid field mask (MSB)
   *
   * Web Server data
   *
   * byte 6 - webserverenable
   * byte 7 - maxsessions (read only)
   * byte 8 - activesessions (read only)
   * byte 9 - 12 - sessiontimeout
   * byte 13 - 14 - httpportnumber
   * byte 15 - 16 - httpsportnumber
   */

  assert (state_data);

  if (!state_data->prog_data->args->oem_options_count)
    {
      pstdout_printf (state_data->pstate,
		      "Option: webserver=enable|disable\n"
		      "Option: sessiontimeout=seconds\n"
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
          if (ipmi_oem_parse_enable (state_data, i, value, &webserverenable) < 0)
            goto cleanup;

          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_WEB_SERVER_CONFIGURATION_ENABLE_FIELD_MASK;
        }
      else if (!strcasecmp (key, "sessiontimeout"))
        {
          if (ipmi_oem_parse_4_byte_field (state_data, i, value, &sessiontimeout) < 0)
            goto cleanup;
          
          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_WEB_SERVER_CONFIGURATION_SESSION_TIMEOUT_FIELD_MASK;
        }
      else if (!strcasecmp (key, "httpportnumber"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &httpportnumber) < 0)
            goto cleanup;
          
          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_WEB_SERVER_CONFIGURATION_HTTP_PORT_NUMBER_FIELD_MASK;
        }
      else if (!strcasecmp (key, "httpsportnumber"))
        {
          if (ipmi_oem_parse_2_byte_field (state_data, i, value, &httpsportnumber) < 0)
            goto cleanup;
          
          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_WEB_SERVER_CONFIGURATION_HTTPS_PORT_NUMBER_FIELD_MASK;
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

  token_data[0] = webserverenable;
  token_data[1] = 0;            /* maxessions is read only */
  token_data[2] = 0;            /* activesessions is read only */
  token_data[3] = (sessiontimeout & 0x000000FF);
  token_data[4] = (sessiontimeout & 0x0000FF00) >> 8;
  token_data[5] = (sessiontimeout & 0x00FF0000) >> 16;
  token_data[6] = (sessiontimeout & 0xFF000000) >> 24;
  token_data[7] = (httpportnumber & 0x00FF);
  token_data[8] = (httpportnumber & 0xFF00) >> 8;
  token_data[9] = (httpsportnumber & 0x00FF);
  token_data[10] = (httpsportnumber & 0xFF00) >> 8;

  if (_dell_set_extended_configuration (state_data,
					IPMI_OEM_DELL_TOKEN_ID_WEB_SERVER_CONFIGURATION,
					token_data,
					11,
					valid_field_mask) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_active_directory_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t token_data[IPMI_OEM_MAX_BYTES];
  uint16_t expected_valid_field_mask = IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_ALL_FIELD_MASK;
  unsigned int token_data_read = 0;
  unsigned int min_token_data_read = 0;
  uint8_t ad_enable;
  uint32_t ad_timeout;
  uint8_t ad_root_domain_string_length;
  char ad_root_domain_string[IPMI_OEM_DELL_TOKEN_STRING_MAX+1];
  uint8_t ad_rac_domain_string_length;
  char ad_rac_domain_string[IPMI_OEM_DELL_TOKEN_STRING_MAX+1];
  uint8_t ad_rac_name_string_length;
  char ad_rac_name_string[IPMI_OEM_DELL_TOKEN_STRING_MAX+1];
  uint8_t ad_type;
  char *ad_type_str;
  uint8_t scl_state;
  uint8_t crl_state;
  uint8_t ad_sso_enable;
  uint8_t ad_dc_filter1_string_length;
  char ad_dc_filter1_string[IPMI_OEM_DELL_TOKEN_STRING_MAX+1];
  uint8_t ad_dc_filter2_string_length;
  char ad_dc_filter2_string[IPMI_OEM_DELL_TOKEN_STRING_MAX+1];
  uint8_t ad_dc_filter3_string_length;
  char ad_dc_filter3_string[IPMI_OEM_DELL_TOKEN_STRING_MAX+1];
  uint8_t ad_gc_filter1_string_length;
  char ad_gc_filter1_string[IPMI_OEM_DELL_TOKEN_STRING_MAX+1];
  uint8_t ad_gc_filter2_string_length;
  char ad_gc_filter2_string[IPMI_OEM_DELL_TOKEN_STRING_MAX+1];
  uint8_t ad_gc_filter3_string_length;
  char ad_gc_filter3_string[IPMI_OEM_DELL_TOKEN_STRING_MAX+1];
  uint8_t ad_certificate_validation_enable; 
  unsigned int offset = 0;
  int rv = -1;

  /* Dell OEM
   *
   * Active Directory Token Data - Token ID 07h
   *
   * Common Header
   *
   * byte 1 - total size of token data (including common header) - LSB
   * byte 2 - total size of token data (including common header) - MSB
   * byte 3 - token version (0x01)
   * byte 4 - valid field mask (LSB)
   * byte 5 - valid field mask (MSB)
   *
   * Active Directory Token data
   *
   * byte 6 - active directory enable
   * byte 7-10 - active directory timeout
   * byte 11 - active directory root domain string length
   * byte y-z (0-255) - active directory root domain string
   * byte x - active directory remote access controller domain string length
   * byte y-z (0-255) - active directory remote access controller domain string
   * byte x - active directory remote access controller name string length
   * byte y-z (0-255) - active directory remote access controller name string
   * byte x - active directory type (1 == extended schema, 2 == standard schema)
   * byte x - Smart Card Logon State (a boolean on iDRAC6, possibly not on iDRAC5??)
   * byte x - Certificate Revocation List State (a boolean on iDRAC6, possibly not on iDRAC5??)
   * byte x - active directory single sign on enable
   * byte x - active directory domain controller filter 1 string length
   * byte y-z (0-255) - active directory domain controller filter 1 string
   * byte x - active directory domain controller filter 2 string length
   * byte y-z (0-255) - active directory domain controller filter 2 string
   * byte x - active directory domain controller filter 3 string length
   * byte y-z (0-255) - active directory domain controller filter 3 string
   * byte x - active directory global catalog filter 1 string length
   * byte y-z (0-255) - active directory global catalog filter 1 string
   * byte x - active directory global catalog filter 2 string length
   * byte y-z (0-255) - active directory global catalog filter 2 string
   * byte x - active directory global catalog filter 3 string length
   * byte y-z (0-255) - active directory global catalog filter 3 string
   * byte x - active directory certificate validate enable
   */

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  memset (ad_root_domain_string, '\0', IPMI_OEM_DELL_TOKEN_STRING_MAX+1);
  memset (ad_rac_domain_string, '\0', IPMI_OEM_DELL_TOKEN_STRING_MAX+1);
  memset (ad_rac_name_string, '\0', IPMI_OEM_DELL_TOKEN_STRING_MAX+1);
  memset (ad_dc_filter1_string, '\0', IPMI_OEM_DELL_TOKEN_STRING_MAX+1);
  memset (ad_dc_filter2_string, '\0', IPMI_OEM_DELL_TOKEN_STRING_MAX+1);
  memset (ad_dc_filter3_string, '\0', IPMI_OEM_DELL_TOKEN_STRING_MAX+1);
  memset (ad_gc_filter1_string, '\0', IPMI_OEM_DELL_TOKEN_STRING_MAX+1);
  memset (ad_gc_filter2_string, '\0', IPMI_OEM_DELL_TOKEN_STRING_MAX+1);
  memset (ad_gc_filter3_string, '\0', IPMI_OEM_DELL_TOKEN_STRING_MAX+1);

  if (_dell_get_extended_configuration (state_data,
					IPMI_OEM_DELL_TOKEN_ID_AD_CONFIGURATION,
					token_data,
					IPMI_OEM_DELL_TOKEN_DATA_MAX,
					expected_valid_field_mask,
					&token_data_read) < 0)
    goto cleanup;

  min_token_data_read = IPMI_OEM_DELL_TOKEN_DATA_COMMON_HEADER_LEN + 19;

  if (token_data_read < min_token_data_read)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "invalid token data length returned: min = %u, returned %u\n",
		       min_token_data_read, token_data_read);
      goto cleanup;
    }

  offset = 5;
  ad_enable = token_data[offset];
  offset++;

  ad_timeout = token_data[offset];
  offset++;
  ad_timeout |= (token_data[offset] << 8);
  offset++;
  ad_timeout |= (token_data[offset] << 16);
  offset++;
  ad_timeout |= (token_data[offset] << 24);
  offset++;

  ad_root_domain_string_length = token_data[offset];
  offset++;

  if (ad_root_domain_string_length)
    {
      memcpy (ad_root_domain_string,
	      &token_data[offset],
	      ad_root_domain_string_length);
      offset += ad_root_domain_string_length;
    }

  ad_rac_domain_string_length = token_data[offset];
  offset++;

  if (ad_rac_domain_string_length)
    {
      memcpy (ad_rac_domain_string,
	      &token_data[offset],
	      ad_rac_domain_string_length);
      offset += ad_rac_domain_string_length;
    }

  ad_rac_name_string_length = token_data[offset];
  offset++;

  if (ad_rac_name_string_length)
    {
      memcpy (ad_rac_name_string,
	      &token_data[offset],
	      ad_rac_name_string_length);
      offset += ad_rac_name_string_length;
    }

  ad_type = token_data[offset];
  offset++;

  scl_state = token_data[offset];
  offset++;

  crl_state = token_data[offset];
  offset++;

  ad_sso_enable = token_data[offset];
  offset++;

  ad_dc_filter1_string_length = token_data[offset];
  offset++;

  if (ad_dc_filter1_string_length)
    {
      memcpy (ad_dc_filter1_string,
	      &token_data[offset],
	      ad_dc_filter1_string_length);
      offset += ad_dc_filter1_string_length;
    }

  ad_dc_filter2_string_length = token_data[offset];
  offset++;

  if (ad_dc_filter2_string_length)
    {
      memcpy (ad_dc_filter2_string,
	      &token_data[offset],
	      ad_dc_filter2_string_length);
      offset += ad_dc_filter2_string_length;
    }

  ad_dc_filter3_string_length = token_data[offset];
  offset++;

  if (ad_dc_filter3_string_length)
    {
      memcpy (ad_dc_filter3_string,
	      &token_data[offset],
	      ad_dc_filter3_string_length);
      offset += ad_dc_filter3_string_length;
    }

  ad_gc_filter1_string_length = token_data[offset];
  offset++;

  if (ad_gc_filter1_string_length)
    {
      memcpy (ad_gc_filter1_string,
	      &token_data[offset],
	      ad_gc_filter1_string_length);
      offset += ad_gc_filter1_string_length;
    }

  ad_gc_filter2_string_length = token_data[offset];
  offset++;

  if (ad_gc_filter2_string_length)
    {
      memcpy (ad_gc_filter2_string,
	      &token_data[offset],
	      ad_gc_filter2_string_length);
      offset += ad_gc_filter2_string_length;
    }

  ad_gc_filter3_string_length = token_data[offset];
  offset++;

  if (ad_gc_filter3_string_length)
    {
      memcpy (ad_gc_filter3_string,
	      &token_data[offset],
	      ad_gc_filter3_string_length);
      offset += ad_gc_filter3_string_length;
    }

  ad_certificate_validation_enable = token_data[offset];
  offset++;

  pstdout_printf (state_data->pstate,
		  "Active Directory                : %s\n",
		  (ad_enable) ? "Enabled" : "Disabled");

  pstdout_printf (state_data->pstate,
		  "Timeout                         : %u seconds\n",
		  ad_timeout);

  pstdout_printf (state_data->pstate,
		  "Root Domain                     : %s\n",
		  ad_root_domain_string);

  pstdout_printf (state_data->pstate,
		  "Remote Access Controller Domain : %s\n",
		  ad_rac_domain_string);

  pstdout_printf (state_data->pstate,
		  "Remote Access Controller Name   : %s\n",
		  ad_rac_name_string);

  if (ad_type == 1)
    ad_type_str = "Extended Schema";
  else if (ad_type == 2)
    ad_type_str = "Standard Schema"; 
  else
    ad_type_str = "Unknown";
  
  pstdout_printf (state_data->pstate,
		  "Type                            : %s\n",
		  ad_type_str);

  pstdout_printf (state_data->pstate,
		  "Smart Card Logon                : %s\n",
		  (scl_state) ? "Enabled" : "Disabled");

  pstdout_printf (state_data->pstate,
		  "Certificate Revocation List     : %s\n",
		  (crl_state) ? "Enabled" : "Disabled");
  
  pstdout_printf (state_data->pstate,
		  "Single Sign On                  : %s\n",
		  (ad_sso_enable) ? "Enabled" : "Disabled");

  pstdout_printf (state_data->pstate,
		  "Domain Controller Filter 1      : %s\n",
		  ad_dc_filter1_string);

  pstdout_printf (state_data->pstate,
		  "Domain Controller Filter 2      : %s\n",
		  ad_dc_filter2_string);

  pstdout_printf (state_data->pstate,
		  "Domain Controller Filter 3      : %s\n",
		  ad_dc_filter3_string);

  pstdout_printf (state_data->pstate,
		  "Global Catalog Filter 1         : %s\n",
		  ad_gc_filter1_string);

  pstdout_printf (state_data->pstate,
		  "Global Catalog Filter 2         : %s\n",
		  ad_gc_filter2_string);

  pstdout_printf (state_data->pstate,
		  "Global Catalog Filter 3         : %s\n",
		  ad_gc_filter3_string);

  pstdout_printf (state_data->pstate,
		  "Certificate Validation          : %s\n",
		  (ad_certificate_validation_enable) ? "Enabled" : "Disabled");

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_set_active_directory_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t token_data[IPMI_OEM_DELL_TOKEN_DATA_MAX];
  uint16_t valid_field_mask = 0;
  uint8_t ad_enable = 0;
  uint32_t ad_timeout = 0;
  uint8_t ad_root_domain_string_length = 0;
  char ad_root_domain_string[IPMI_OEM_DELL_TOKEN_STRING_MAX+1];
  uint8_t ad_rac_domain_string_length = 0;
  char ad_rac_domain_string[IPMI_OEM_DELL_TOKEN_STRING_MAX+1];
  uint8_t ad_rac_name_string_length = 0;
  char ad_rac_name_string[IPMI_OEM_DELL_TOKEN_STRING_MAX+1];
  uint8_t ad_type = 0;
  uint8_t scl_state = 0;
  uint8_t crl_state = 0;
  uint8_t ad_sso_enable = 0;
  uint8_t ad_dc_filter1_string_length = 0;
  char ad_dc_filter1_string[IPMI_OEM_DELL_TOKEN_STRING_MAX+1];
  uint8_t ad_dc_filter2_string_length = 0;
  char ad_dc_filter2_string[IPMI_OEM_DELL_TOKEN_STRING_MAX+1];
  uint8_t ad_dc_filter3_string_length = 0;
  char ad_dc_filter3_string[IPMI_OEM_DELL_TOKEN_STRING_MAX+1];
  uint8_t ad_gc_filter1_string_length = 0;
  char ad_gc_filter1_string[IPMI_OEM_DELL_TOKEN_STRING_MAX+1];
  uint8_t ad_gc_filter2_string_length = 0;
  char ad_gc_filter2_string[IPMI_OEM_DELL_TOKEN_STRING_MAX+1];
  uint8_t ad_gc_filter3_string_length = 0;
  char ad_gc_filter3_string[IPMI_OEM_DELL_TOKEN_STRING_MAX+1];
  uint8_t ad_certificate_validation_enable = 0; 
  unsigned int offset = 0;
  int rv = -1;
  unsigned int i;

  /* Dell OEM
   *
   * Active Directory Token Data - Token ID 07h
   *
   * Common Header
   *
   * byte 1 - total size of token data (including common header) - LSB
   * byte 2 - total size of token data (including common header) - MSB
   * byte 3 - token version (0x01)
   * byte 4 - valid field mask (LSB)
   * byte 5 - valid field mask (MSB)
   *
   * Active Directory Token data
   *
   * byte 6 - active directory enable
   * byte 7-10 - active directory timeout
   * byte 11 - active directory root domain string length
   * byte y-z (0-255) - active directory root domain string
   * byte x - active directory remote access controller domain string length
   * byte y-z (0-255) - active directory remote access controller domain string
   * byte x - active directory remote access controller name string length
   * byte y-z (0-255) - active directory remote access controller name string
   * byte x - active directory type (1 == extended schema, 2 == standard schema)
   * byte x - Smart Card Logon State (a boolean on iDRAC6, possibly not on iDRAC5??)
   * byte x - Certificate Revocation List State (a boolean on iDRAC6, possibly not on iDRAC5??)
   * byte x - active directory single sign on enable
   * byte x - active directory domain controller filter 1 string length
   * byte y-z (0-255) - active directory domain controller filter 1 string
   * byte x - active directory domain controller filter 2 string length
   * byte y-z (0-255) - active directory domain controller filter 2 string
   * byte x - active directory domain controller filter 3 string length
   * byte y-z (0-255) - active directory domain controller filter 3 string
   * byte x - active directory global catalog filter 1 string length
   * byte y-z (0-255) - active directory global catalog filter 1 string
   * byte x - active directory global catalog filter 2 string length
   * byte y-z (0-255) - active directory global catalog filter 2 string
   * byte x - active directory global catalog filter 3 string length
   * byte y-z (0-255) - active directory global catalog filter 3 string
   * byte x - active directory certificate validate enable
   */

  assert (state_data);

  if (!state_data->prog_data->args->oem_options_count)
    {
      pstdout_printf (state_data->pstate,
		      "Option: activedirectory=enable|disable\n"
		      "Option: timeout=seconds\n"
		      "Option: type=extended|standard\n"
		      "Option: sso=enable|disable\n"
		      "Option: certificatevalidation=enable|disable\n");
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

      if (!strcasecmp (key, "activedirectory"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &ad_enable) < 0)
            goto cleanup;

          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_AD_ENABLE_FIELD_MASK;
        }
      else if (!strcasecmp (key, "timeout"))
        {
          if (ipmi_oem_parse_4_byte_field (state_data, i, value, &ad_timeout) < 0)
            goto cleanup;
          
          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_AD_TIMEOUT_FIELD_MASK;
        }
#if 0
      /* don't support for now */
      else if (!strcasecmp (key, "rootdomain"))
        {
          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &ad_root_domain_string_length,
                                     ad_root_domain_string,
                                     IPMI_OEM_DELL_TOKEN_STRING_MAX) < 0)
            goto cleanup;
          
          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_AD_ROOT_DOMAIN_FIELD_MASK;
        }
      else if (!strcasecmp (key, "racdomain"))
        {
          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &ad_rac_domain_string_length,
                                     ad_rac_domain_string,
                                     IPMI_OEM_DELL_TOKEN_STRING_MAX) < 0)
            goto cleanup;
          
          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_AD_RAC_DOMAIN_FIELD_MASK;
        }
      else if (!strcasecmp (key, "racname"))
        {
          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &ad_rac_name_string_length,
                                     ad_rac_name_string,
                                     IPMI_OEM_DELL_TOKEN_STRING_MAX) < 0)
            goto cleanup;
          
          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_AD_RAC_NAME_FIELD_MASK;
        }
#endif
      else if (!strcasecmp (key, "type"))
	{
	  if (strcasecmp (value, "extended") && strcasecmp (value, "standard"))
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "%s:%s invalid OEM option argument '%s' : invalid value\n",
			       state_data->prog_data->args->oem_id,
			       state_data->prog_data->args->oem_command,
			       state_data->prog_data->args->oem_options[i]);
	      return (-1);
	    }
	  
	  if (!strcasecmp (value, "extended"))
	    ad_type = IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_TYPE_EXTENDED;
	  else
	    ad_type = IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_TYPE_STANDARD;

	  valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_AD_TYPE_FIELD_MASK;
	}
#if 0
      /* read only on iDRAC6 */
      else if (!strcasecmp (key, "smartcardlogon"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &scl_state) < 0)
            goto cleanup;

          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_SCL_STATE_FIELD_MASK;
        }
#endif
#if 0
      /* read only on iDRAC6 */
      else if (!strcasecmp (key, "certificaterevocationlist"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &crl_state) < 0)
            goto cleanup;

          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_CRL_STATE_FIELD_MASK;
        }
#endif
      else if (!strcasecmp (key, "sso"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &ad_sso_enable) < 0)
            goto cleanup;

          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_AD_SSO_ENABLE_FIELD_MASK;
        }
#if 0
      /* don't support for now */
      else if (!strcasecmp (key, "dcfilter1"))
        {
          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &ad_dc_filter1_string_length,
                                     ad_dc_filter1_string,
                                     IPMI_OEM_DELL_TOKEN_STRING_MAX) < 0)
            goto cleanup;
          
          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_AD_DC_FILTER1_FIELD_MASK;
        }
      else if (!strcasecmp (key, "dcfilter2"))
        {
          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &ad_dc_filter2_string_length,
                                     ad_dc_filter2_string,
                                     IPMI_OEM_DELL_TOKEN_STRING_MAX) < 0)
            goto cleanup;
          
          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_AD_DC_FILTER2_FIELD_MASK;
        }
      else if (!strcasecmp (key, "dcfilter3"))
        {
          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &ad_dc_filter3_string_length,
                                     ad_dc_filter3_string,
                                     IPMI_OEM_DELL_TOKEN_STRING_MAX) < 0)
            goto cleanup;
          
          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_AD_DC_FILTER3_FIELD_MASK;
        }
      else if (!strcasecmp (key, "gcfilter1"))
        {
          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &ad_gc_filter1_string_length,
                                     ad_gc_filter1_string,
                                     IPMI_OEM_DELL_TOKEN_STRING_MAX) < 0)
            goto cleanup;
          
          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_AD_GC_FILTER1_FIELD_MASK;
        }
      else if (!strcasecmp (key, "gcfilter2"))
        {
          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &ad_gc_filter2_string_length,
                                     ad_gc_filter2_string,
                                     IPMI_OEM_DELL_TOKEN_STRING_MAX) < 0)
            goto cleanup;
          
          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_AD_GC_FILTER2_FIELD_MASK;
        }
      else if (!strcasecmp (key, "gcfilter3"))
        {
          if (ipmi_oem_parse_string (state_data,
                                     i,
                                     value,
                                     &ad_gc_filter3_string_length,
                                     ad_gc_filter3_string,
                                     IPMI_OEM_DELL_TOKEN_STRING_MAX) < 0)
            goto cleanup;
          
          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_AD_GC_FILTER3_FIELD_MASK;
        }
#endif
      else if (!strcasecmp (key, "certificatevalidation"))
        {
          if (ipmi_oem_parse_enable (state_data, i, value, &ad_certificate_validation_enable) < 0)
            goto cleanup;

          valid_field_mask |= IPMI_OEM_DELL_EXTENDED_CONFIG_AD_CONFIGURATION_AD_CERTIFICATE_VALIDATION_ENABLE_FIELD_MASK;
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

  offset = 0;
  
  token_data[offset] = ad_enable;
  offset++;

  token_data[offset] = (ad_timeout & 0x000000FF);
  offset++;
  token_data[offset] = (ad_timeout & 0x0000FF00) >> 8;
  offset++;
  token_data[offset] = (ad_timeout & 0x00FF0000) >> 16;
  offset++;
  token_data[offset] = (ad_timeout & 0xFF000000) >> 24;
  offset++;

  token_data[offset] = ad_root_domain_string_length;
  offset++;
  if (ad_root_domain_string_length)
    {
      memcpy (&token_data[offset],
	      ad_root_domain_string,
	      ad_root_domain_string_length);
      offset += ad_root_domain_string_length;
    }

  token_data[offset] = ad_rac_domain_string_length;
  offset++;
  if (ad_rac_domain_string_length)
    {
      memcpy (&token_data[offset],
	      ad_rac_domain_string,
	      ad_rac_domain_string_length);
      offset += ad_rac_domain_string_length;
    }

  token_data[offset] = ad_rac_name_string_length;
  offset++;
  if (ad_rac_name_string_length)
    {
      memcpy (&token_data[offset],
	      ad_rac_name_string,
	      ad_rac_name_string_length);
      offset += ad_rac_name_string_length;
    }

  token_data[offset] = ad_type;
  offset++;

  token_data[offset] = scl_state;
  offset++;

  token_data[offset] = crl_state;
  offset++;

  token_data[offset] = ad_sso_enable;
  offset++;

  token_data[offset] = ad_dc_filter1_string_length;
  offset++;
  if (ad_dc_filter1_string_length)
    {
      memcpy (&token_data[offset],
	      ad_dc_filter1_string,
	      ad_dc_filter1_string_length);
      offset += ad_dc_filter1_string_length;
    }

  token_data[offset] = ad_dc_filter2_string_length;
  offset++;
  if (ad_dc_filter2_string_length)
    {
      memcpy (&token_data[offset],
	      ad_dc_filter2_string,
	      ad_dc_filter2_string_length);
      offset += ad_dc_filter2_string_length;
    }

  token_data[offset] = ad_dc_filter3_string_length;
  offset++;
  if (ad_dc_filter3_string_length)
    {
      memcpy (&token_data[offset],
	      ad_dc_filter3_string,
	      ad_dc_filter3_string_length);
      offset += ad_dc_filter3_string_length;
    }

  token_data[offset] = ad_gc_filter1_string_length;
  offset++;
  if (ad_gc_filter1_string_length)
    {
      memcpy (&token_data[offset],
	      ad_gc_filter1_string,
	      ad_gc_filter1_string_length);
      offset += ad_gc_filter1_string_length;
    }

  token_data[offset] = ad_gc_filter2_string_length;
  offset++;
  if (ad_gc_filter2_string_length)
    {
      memcpy (&token_data[offset],
	      ad_gc_filter2_string,
	      ad_gc_filter2_string_length);
      offset += ad_gc_filter2_string_length;
    }

  token_data[offset] = ad_gc_filter3_string_length;
  offset++;
  if (ad_gc_filter3_string_length)
    {
      memcpy (&token_data[offset],
	      ad_gc_filter3_string,
	      ad_gc_filter3_string_length);
      offset += ad_gc_filter3_string_length;
    }

  token_data[offset] = ad_certificate_validation_enable;
  offset++;

  if (_dell_set_extended_configuration (state_data,
					IPMI_OEM_DELL_TOKEN_ID_AD_CONFIGURATION,
					token_data,
					offset,
					valid_field_mask) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_reset_to_defaults (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * From Dell Provided Source Code
   *
   * Request
   *
   * 0x30 - OEM network function
   * 0x21 - OEM cmd
   * 0x00 | 0xaa - 0x00 = get status
   *             - 0xaa = initiate reset to defaults
   * 
   * Response
   *
   * 0x21 - OEM cmd
   * 0x?? - Completion Code
   * 0x00 | 0x01 - 0x00 = reset to defaults in progress
   *             - 0x01 = reset to defaults complete 
   */

  bytes_rq[0] = IPMI_CMD_OEM_DELL_RESET_TO_DEFAULTS;
  bytes_rq[1] = IPMI_OEM_DELL_RESET_TO_DEFAULTS_INITIATE_RESULT_TO_DEFAULTS;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              2, /* num bytes */
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
                                                   IPMI_CMD_OEM_DELL_RESET_TO_DEFAULTS,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;


  /* don't quit until it is done */
  while (1)
    {
      bytes_rq[0] = IPMI_CMD_OEM_DELL_RESET_TO_DEFAULTS;
      bytes_rq[1] = IPMI_OEM_DELL_RESET_TO_DEFAULTS_GET_STATUS;
      
      if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
				  0, /* lun */
				  IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
				  bytes_rq, /* data */
				  2, /* num bytes */
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
						       IPMI_CMD_OEM_DELL_RESET_TO_DEFAULTS,
						       IPMI_NET_FN_OEM_DELL_GENERIC_RS,
                                                       NULL) < 0)
	goto cleanup;

      if (bytes_rs[2] == IPMI_OEM_DELL_RESET_TO_DEFAULTS_COMPLETE)
	break;

      sleep (1);
    }
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_power_consumption_data (ipmi_oem_state_data_t *state_data)
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
  char time_buf[IPMI_OEM_TIME_BUFLEN + 1];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell Poweredge OEM
   *
   * From http://linux.dell.com/files/openipmi/ipmitool/
   * From Dell Provided Docs
   *
   * Request
   *
   * 0x30 - OEM network function
   * 0x9c - OEM cmd
   * 0x07 - Entity ID
   * 0x01 - Entity Instance
   * 
   * Response
   *
   * 0x9c - OEM cmd
   * 0x?? - Completion Code
   * bytes 2-5 - cumulative start time
   * bytes 6-9 - cumulative reading (in WH)
   * bytes 10-13 - peak start time
   * bytes 14-17 - peak amp time
   * bytes 18-21 - peak amp reading
   * bytes 22-25 - peak watt time
   * bytes 26-29 - peak watt reading
   */

  bytes_rq[0] = IPMI_CMD_OEM_DELL_GET_POWER_CONSUMPTION_DATA;
  bytes_rq[1] = IPMI_ENTITY_ID_SYSTEM_BOARD;
  bytes_rq[2] = 0x01;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
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
                                                   IPMI_CMD_OEM_DELL_GET_POWER_CONSUMPTION_DATA,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_RS,
                                                   NULL) < 0)
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

  memset (time_buf, '\0', IPMI_OEM_TIME_BUFLEN + 1);

  if (ipmi_timestamp_string (cumulative_start_time,
			     state_data->prog_data->args->common_args.utc_offset,
			     get_timestamp_flags (&(state_data->prog_data->args->common_args),
						  IPMI_TIMESTAMP_FLAG_DEFAULT), 
			     "%D - %T",
			     time_buf,
			     IPMI_OEM_TIME_BUFLEN) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_timestamp_string: %s\n",
		       strerror (errno));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Cumulative Energy Start Time : %s\n",
                  time_buf);

  pstdout_printf (state_data->pstate,
                  "Cumulative Energy            : %.2f kWh\n",
                  cumulative_reading_val);

  peak_amp_reading_val = ((double)peak_amp_reading) / 10.0;

  memset (time_buf, '\0', IPMI_OEM_TIME_BUFLEN + 1);

  if (ipmi_timestamp_string (peak_amp_time,
			     state_data->prog_data->args->common_args.utc_offset,
			     get_timestamp_flags (&(state_data->prog_data->args->common_args),
						  IPMI_TIMESTAMP_FLAG_DEFAULT), 
			     "%D - %T",
			     time_buf,
			     IPMI_OEM_TIME_BUFLEN) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_timestamp_string: %s\n",
		       strerror (errno));
      goto cleanup;
    }

  pstdout_printf (state_data->pstate,
                  "Peak Amp Time                : %s\n",
                  time_buf);

  pstdout_printf (state_data->pstate,
                  "Peak Amp                     : %.2f A\n",
                  peak_amp_reading_val);

  memset (time_buf, '\0', IPMI_OEM_TIME_BUFLEN + 1);

  if (ipmi_timestamp_string (peak_watt_time,
			     state_data->prog_data->args->common_args.utc_offset,
			     get_timestamp_flags (&(state_data->prog_data->args->common_args),
						  IPMI_TIMESTAMP_FLAG_DEFAULT), 
			     "%D - %T",
			     time_buf,
			     IPMI_OEM_TIME_BUFLEN) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_timestamp_string: %s\n",
		       strerror (errno));
      goto cleanup;
    }

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
ipmi_oem_dell_reset_power_consumption_data (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  /* achu: XXX
   *
   * "both" or "all" byte doesn't appear to work, maybe in a later
   * firmware release?
   */
  if (strcasecmp (state_data->prog_data->args->oem_options[0], "cumulative")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "peak"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  /* Dell Poweredge OEM
   *
   * From http://linux.dell.com/files/openipmi/ipmitool/
   * From Dell Provided Docs
   *
   * Request
   *
   * 0x30 - OEM network function
   * 0x9d - OEM cmd
   * 0x07 - Entity ID
   * 0x01 - Entity Instance
   * 0x?? - field to clear (0x1 = cumulative, 0x2 = peak, 0x4 = all/both)
   * 
   * Response
   *
   * 0x9d - OEM cmd
   * 0x?? - Completion Code
   */

  bytes_rq[0] = IPMI_CMD_OEM_DELL_RESET_POWER_CONSUMPTION_DATA;
  bytes_rq[1] = IPMI_ENTITY_ID_SYSTEM_BOARD;
  bytes_rq[2] = 0x01;

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "cumulative"))
    bytes_rq[3] = IPMI_OEM_DELL_RESET_POWER_CONSUMPTION_DATA_CUMULATIVE;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "peak"))
    bytes_rq[3] = IPMI_OEM_DELL_RESET_POWER_CONSUMPTION_DATA_PEAK;
  else
    bytes_rq[3] = IPMI_OEM_DELL_RESET_POWER_CONSUMPTION_DATA_ALL;
  
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              4, /* num bytes */
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
                                                   IPMI_CMD_OEM_DELL_RESET_POWER_CONSUMPTION_DATA,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_oem_dell_power_supply_info_sdr_callback (ipmi_sdr_ctx_t sdr_ctx,
					       uint8_t record_type,
					       const void *sdr_record,
					       unsigned int sdr_record_len,
					       void *arg)
{
  ipmi_oem_state_data_t *state_data;
  uint8_t entity_id;
  uint8_t entity_instance;
  uint8_t entity_instance_type;
  uint8_t sensor_type;
  
  assert (sdr_ctx);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (arg);

  state_data = (ipmi_oem_state_data_t *)arg;

  /* Dell Poweredge OEM
   *
   * Power Supply Info Request
   * From Dell Provided Docs
   *
   * 0x30 - OEM network function
   * 0xB0 - OEM cmd
   * 0x?? - Power Supply Entity ID
   * 0x?? - Power Supply Entity Instance
   *
   * Get Power Supply Info Response
   *
   * 0xB0 - OEM cmd
   * 0x?? - Completion Code
   * bytes 2-3 - rated watts
   * bytes 4-5 - rated amps (in 0.1 amps)
   * bytes 6-7 - rated volts
   * bytes 8-11 - component ID (internal for Dell)
   * bytes 12-19 - firmware version (string, non-null terminated)
   * bytes 20 - power supply type
   * - 0x00 - AC
   * - 0x01 - DC
   * bytes 21-22 - rated dc watts
   * bytes 23 - online status
   * - maps to power supply sensor events
   * bytes 24 - reserved
   */

  if (record_type != IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
      && record_type != IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
    return (0);
  
  if (ipmi_sdr_parse_entity_id_instance_type (state_data->sdr_ctx,
					      sdr_record,
					      sdr_record_len,
					      &entity_id,
					      &entity_instance,
					      &entity_instance_type) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_parse_entity_id_instance_type: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      return (-1);
    }
	  
  if (ipmi_sdr_parse_sensor_type (state_data->sdr_ctx,
				  sdr_record,
				  sdr_record_len,
				  &sensor_type) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_parse_sensor_type: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      return (-1);
    }
  
  if (entity_id == IPMI_ENTITY_ID_POWER_SUPPLY
      && entity_instance_type == IPMI_SDR_PHYSICAL_ENTITY
      && sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY)
    {
      uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
      uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
      int rs_len;
      uint16_t ratedwatts;
      uint16_t ratedamps;
      uint16_t ratedvolts;
      uint32_t componentid;
      char firmwareversion[IPMI_OEM_MAX_BYTES];
      uint8_t powersupplytype;
      uint16_t rateddcwatts;
      uint8_t onlinestatus;
      double ratedamps_val;
      char sensor_name_buf[IPMI_SDR_MAX_SENSOR_NAME_LENGTH + 1];
      
      memset (firmwareversion, '\0', IPMI_OEM_MAX_BYTES);

      /* achu note:
       *
       * Dell code does not check for potential sdr record
       * sharing, so I won't either.
       */
	  
      if (ipmi_sdr_parse_entity_sensor_name (state_data->sdr_ctx,
					     NULL,
					     0,
					     0, /* sensor number */
					     IPMI_SDR_SENSOR_NAME_FLAGS_IGNORE_SHARED_SENSORS,
					     sensor_name_buf,
					     IPMI_SDR_MAX_SENSOR_NAME_LENGTH) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_sdr_parse_entity_sensor_name: %s\n",
			   ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	  return (-1);
	}
	      
      bytes_rq[0] = IPMI_CMD_OEM_DELL_POWER_SUPPLY_INFO;
      bytes_rq[1] = entity_id;
      bytes_rq[2] = entity_instance;
	  
      if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
				  0, /* lun */
				  IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
				  bytes_rq, /* data */
				  3, /* num bytes */
				  bytes_rs,
				  IPMI_OEM_MAX_BYTES)) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_cmd_raw: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  return (-1);
	}
	      
      if (ipmi_oem_check_response_and_completion_code (state_data,
						       bytes_rs,
						       rs_len,
						       25,
						       IPMI_CMD_OEM_DELL_POWER_SUPPLY_INFO,
						       IPMI_NET_FN_OEM_DELL_GENERIC_RS,
						       NULL) < 0)
	return (-1);
	      
      ratedwatts = bytes_rs[2];
      ratedwatts |= (bytes_rs[3] << 8);
	      
      ratedamps = bytes_rs[4];
      ratedamps |= (bytes_rs[5] << 8);
	  
      ratedvolts = bytes_rs[6];
      ratedvolts |= (bytes_rs[7] << 8);
	  
      componentid = bytes_rs[8];
      componentid |= (bytes_rs[9] << 8);
      componentid |= (bytes_rs[10] << 16);
      componentid |= (bytes_rs[11] << 24);
	  
      memcpy(firmwareversion, &(bytes_rs[12]), 8);
	  
      powersupplytype = bytes_rs[20];
	  
      rateddcwatts = bytes_rs[21];
      rateddcwatts |= (bytes_rs[22] << 8);
	  
      onlinestatus = bytes_rs[23];
	  
      pstdout_printf (state_data->pstate,
		      "Power Supply      : %s\n",
		      sensor_name_buf);
	  
      pstdout_printf (state_data->pstate,
		      "Rated Watts       : %u W\n",
		      ratedwatts);
	  
      ratedamps_val = ((double)ratedamps) / 10.0;
	  
      pstdout_printf (state_data->pstate,
		      "Rated Amps        : %.2f A\n",
		      ratedamps_val);
	  
      pstdout_printf (state_data->pstate,
		      "Rated Volts       : %u V\n",
		      ratedvolts);
	  
      pstdout_printf (state_data->pstate,
		      "Rated DC Watts    : %u W\n",
		      rateddcwatts);
	  
      pstdout_printf (state_data->pstate,
		      "Power Supply Type : %s\n",
		      (powersupplytype == IPMI_OEM_DELL_POWER_SUPPLY_INFO_DC) ? "DC" : "AC");
	  
      if (onlinestatus <= ipmi_sensor_type_power_supply_max_index)
	pstdout_printf (state_data->pstate,
			"Online Status     : %s\n",
			ipmi_sensor_type_power_supply[onlinestatus]);
      else
	pstdout_printf (state_data->pstate,
			"Online Status     : %02Xh\n",
			onlinestatus);
	  
      pstdout_printf (state_data->pstate,
		      "Firmare Version   : %s\n",
		      firmwareversion);
	  
      /* internal dell componentid code */
      pstdout_printf (state_data->pstate,
		      "Dell Componentid  : %u\n",
		      componentid);
	  
      pstdout_printf (state_data->pstate,
		      "\n");
    }

  return (0);
}

int
ipmi_oem_dell_power_supply_info (ipmi_oem_state_data_t *state_data)
{
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (sdr_cache_create_and_load (state_data->sdr_ctx,
                                 state_data->pstate,
                                 state_data->ipmi_ctx,
                                 state_data->hostname,
				 &state_data->prog_data->args->common_args) < 0)
    goto cleanup;

  if (ipmi_sdr_cache_iterate (state_data->sdr_ctx,
			      _ipmi_oem_dell_power_supply_info_sdr_callback,
			      state_data) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_cache_iterate: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_instantaneous_power_consumption_data (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint16_t instantaneous_power;
  uint16_t instantaneous_amps;
  double instantaneous_amps_val;
  int rs_len;
  int rv = -1;

  assert (state_data);
 
  /* Dell Poweredge OEM
   *
   * From Dell Provided Source Code
   * From Dell Provided Docs
   *
   * Request
   *
   * 0x30 - OEM network function
   * 0xB3 - OEM cmd
   * 0x0A - Entity ID
   * 0x?? - Entity Instance
   * - 0x00 - both/all power supplies
   * 
   * Response
   *
   * 0xB3 - OEM cmd
   * 0x?? - Completion Code
   * bytes 2-3 - instantaneous power
   * bytes 4-5 - instantaneous amps
   * bytes 6-8 - reserved
   */

  bytes_rq[0] = IPMI_CMD_OEM_DELL_POWER_CONSUMPTION;
  bytes_rq[1] = IPMI_ENTITY_ID_POWER_SUPPLY;
  if (!state_data->prog_data->args->oem_options_count)
    bytes_rq[2] = IPMI_OEM_DELL_POWER_CONSUMPTION_ENTITY_INSTANCE_ALL;
  else
    {
      char *endptr = NULL;
      unsigned int temp;
      
      errno = 0;
      temp = strtoul (state_data->prog_data->args->oem_options[0], &endptr, 10);
      if (errno
          || endptr[0] != '\0'
          || temp > UCHAR_MAX)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s'\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[0]);
          goto cleanup;
        }

      bytes_rq[2] = temp;
    }    

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
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
                                                   9,
                                                   IPMI_CMD_OEM_DELL_POWER_CONSUMPTION,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  instantaneous_power = bytes_rs[2];
  instantaneous_power |= (bytes_rs[3] << 8);

  instantaneous_amps = bytes_rs[4];
  instantaneous_amps |= (bytes_rs[5] << 8);

  instantaneous_amps_val = ((double)instantaneous_amps) / 10.0;

  pstdout_printf (state_data->pstate,
		  "Instantaneous Power : %u W\n",
		  instantaneous_power);

  pstdout_printf (state_data->pstate,
		  "Instantaneous Amps  : %.2f A\n",
		  instantaneous_amps_val);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_power_head_room (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint16_t instantaneous_head_room;
  uint16_t peak_head_room;
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Source Code
   * From Dell Provided Docs
   *
   * Request
   *
   * 0x30 - OEM network function
   * 0xBB - OEM cmd
   * 
   * Response
   *
   * 0xBB - OEM cmd
   * 0x?? - Completion Code
   * bytes 2-3 - instantaneous head room in watts
   * bytes 4-5 - peak head room in watts
   *
   * Notes: Head room is the difference between Max potential and the
   * current power consumption.
   */

  bytes_rq[0] = IPMI_CMD_OEM_DELL_GET_POWER_HEAD_ROOM;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
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
                                                   6,
                                                   IPMI_CMD_OEM_DELL_GET_POWER_HEAD_ROOM,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  instantaneous_head_room = bytes_rs[2];
  instantaneous_head_room |= (bytes_rs[3] << 8);

  peak_head_room = bytes_rs[4];
  peak_head_room |= (bytes_rs[5] << 8);

  pstdout_printf (state_data->pstate,
		  "Instantaneous Head Room : %u W\n",
		  instantaneous_head_room);

  pstdout_printf (state_data->pstate,
		  "Peak Head Room          : %u W\n",
		  peak_head_room);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_power_consumption_statistics (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  uint16_t last_minute_power = 0;
  uint16_t last_hour_power = 0;
  uint16_t last_day_power = 0;
  uint16_t last_week_power = 0;
  uint32_t last_minute_power_time = 0;
  uint32_t last_hour_power_time = 0;
  uint32_t last_day_power_time = 0;
  uint32_t last_week_power_time = 0;
  char time_buf[IPMI_OEM_TIME_BUFLEN + 1];
  int len;
  uint8_t system_info_parameter;
  char *system_info_string = NULL;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  /* achu: handle some additional possibilities */
  if (strcasecmp (state_data->prog_data->args->oem_options[0], "average")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "avg")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "maximum")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "max")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "minimum")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "min"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  /* Dell Poweredge OEM
   *
   * From Dell Provided Source Code
   * From Dell Provided Docs
   *
   * Uses Get System Info command
   *
   * Parameter data response formatted:
   *
   * For IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_AVERAGE_POWER_CONSUMPTION_STATISTICS
   *
   * bytes 1-2 - last minute average power
   * bytes 3-4 - last hour average power
   * bytes 5-6 - last day average power
   * bytes 7-8 - last week average power
   *
   * For IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_MAX_POWER_CONSUMPTION_STATISTICS
   * and IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_MIN_POWER_CONSUMPTION_STATISTICS
   *
   * bytes 1-2 - last minute max/min power
   * bytes 3-4 - last hour max/min power
   * bytes 5-6 - last day max/min power
   * bytes 7-8 - last week max/min power
   * bytes 9-12 - last minute max/min power time
   * bytes 13-16 - last hour max/min power time
   * bytes 17-20 - last day max/min power time
   * bytes 21-24 - last week max/min power time
   *
   */

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "average")
      || !strcasecmp (state_data->prog_data->args->oem_options[0], "avg"))
    system_info_parameter = IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_AVERAGE_POWER_CONSUMPTION_STATISTICS;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "maximum")
	   || !strcasecmp (state_data->prog_data->args->oem_options[0], "max"))
    system_info_parameter = IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_MAX_POWER_CONSUMPTION_STATISTICS;
  else  /* (!strcasecmp (state_data->prog_data->args->oem_options[0], "minimum")
	   || !strcasecmp (state_data->prog_data->args->oem_options[0], "min")) */
    system_info_parameter = IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_MIN_POWER_CONSUMPTION_STATISTICS;

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
                                           system_info_parameter,
                                           IPMI_SYSTEM_INFO_PARAMETERS_NO_SET_SELECTOR,
                                           IPMI_SYSTEM_INFO_PARAMETERS_NO_BLOCK_SELECTOR,
                                           obj_cmd_rs) < 0)
    {
      if ((ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
	   && ((ipmi_check_completion_code (obj_cmd_rs,
					    IPMI_COMP_CODE_GET_SYSTEM_INFO_PARAMETERS_PARAMETER_NOT_SUPPORTED) == 1)
	       || (ipmi_check_completion_code (obj_cmd_rs,
					       IPMI_COMP_CODE_OEM_DELL_NOT_LICENSED) == 1)))
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

  if (len < 8)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_system_info_parameters: invalid buffer length returned: %d\n",
                       len);
      goto cleanup;
    }
  
  last_minute_power = configuration_parameter_data[0];
  last_minute_power |= (configuration_parameter_data[1] << 8);

  last_hour_power = configuration_parameter_data[2];
  last_hour_power |= (configuration_parameter_data[3] << 8);

  last_day_power = configuration_parameter_data[4];
  last_day_power |= (configuration_parameter_data[5] << 8);

  last_week_power = configuration_parameter_data[6];
  last_week_power |= (configuration_parameter_data[7] << 8);

  if (system_info_parameter == IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_MAX_POWER_CONSUMPTION_STATISTICS
      || system_info_parameter == IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_MIN_POWER_CONSUMPTION_STATISTICS)
    {
      last_minute_power_time = configuration_parameter_data[8];
      last_minute_power_time |= (configuration_parameter_data[9] << 8);
      last_minute_power_time |= (configuration_parameter_data[10] << 16);
      last_minute_power_time |= (configuration_parameter_data[11] << 24);

      last_hour_power_time = configuration_parameter_data[12];
      last_hour_power_time |= (configuration_parameter_data[13] << 8);
      last_hour_power_time |= (configuration_parameter_data[14] << 16);
      last_hour_power_time |= (configuration_parameter_data[15] << 24);

      last_day_power_time = configuration_parameter_data[16];
      last_day_power_time |= (configuration_parameter_data[17] << 8);
      last_day_power_time |= (configuration_parameter_data[18] << 16);
      last_day_power_time |= (configuration_parameter_data[19] << 24);

      last_week_power_time = configuration_parameter_data[20];
      last_week_power_time |= (configuration_parameter_data[21] << 8);
      last_week_power_time |= (configuration_parameter_data[22] << 16);
      last_week_power_time |= (configuration_parameter_data[23] << 24);
    }

  if (system_info_parameter == IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_AVERAGE_POWER_CONSUMPTION_STATISTICS)
    system_info_string = "Average";
  else if (system_info_parameter == IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_MAX_POWER_CONSUMPTION_STATISTICS)
    system_info_string = "Max";
  else
    system_info_string = "Min";

  pstdout_printf (state_data->pstate,
		  "Last Minute %s Power      : %u W\n",
		  system_info_string,
		  last_minute_power);
  
  if (system_info_parameter == IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_MAX_POWER_CONSUMPTION_STATISTICS
      || system_info_parameter == IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_MIN_POWER_CONSUMPTION_STATISTICS)
    {
      memset (time_buf, '\0', IPMI_OEM_TIME_BUFLEN + 1);

      if (ipmi_timestamp_string (last_minute_power_time,
				 state_data->prog_data->args->common_args.utc_offset,
				 get_timestamp_flags (&(state_data->prog_data->args->common_args),
						      IPMI_TIMESTAMP_FLAG_DEFAULT), 
				 "%D - %T",
				 time_buf,
				 IPMI_OEM_TIME_BUFLEN) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_timestamp_string: %s\n",
			   strerror (errno));
	  goto cleanup;
	}
      
      pstdout_printf (state_data->pstate,
		      "Last Minute %s Power Time : %s\n",
		      system_info_string,
		      time_buf);
    }

  pstdout_printf (state_data->pstate,
		  "Last Hour %s Power        : %u W\n",
		  system_info_string,
		  last_hour_power);
  
  if (system_info_parameter == IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_MAX_POWER_CONSUMPTION_STATISTICS
      || system_info_parameter == IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_MIN_POWER_CONSUMPTION_STATISTICS)
    {
      memset (time_buf, '\0', IPMI_OEM_TIME_BUFLEN + 1);

      if (ipmi_timestamp_string (last_hour_power_time,
				 state_data->prog_data->args->common_args.utc_offset,
				 get_timestamp_flags (&(state_data->prog_data->args->common_args),
						      IPMI_TIMESTAMP_FLAG_DEFAULT), 
				 "%D - %T",
				 time_buf,
				 IPMI_OEM_TIME_BUFLEN) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_timestamp_string: %s\n",
			   strerror (errno));
	  goto cleanup;
	}

      pstdout_printf (state_data->pstate,
		      "Last Hour %s Power Time   : %s\n",
		      system_info_string,
		      time_buf);
    }

  pstdout_printf (state_data->pstate,
		  "Last Day %s Power         : %u W\n",
		  system_info_string,
		  last_day_power);
  
  if (system_info_parameter == IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_MAX_POWER_CONSUMPTION_STATISTICS
      || system_info_parameter == IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_MIN_POWER_CONSUMPTION_STATISTICS)
    {
      memset (time_buf, '\0', IPMI_OEM_TIME_BUFLEN + 1);

      if (ipmi_timestamp_string (last_day_power_time,
				 state_data->prog_data->args->common_args.utc_offset,
				 get_timestamp_flags (&(state_data->prog_data->args->common_args),
						      IPMI_TIMESTAMP_FLAG_DEFAULT), 
				 "%D - %T",
				 time_buf,
				 IPMI_OEM_TIME_BUFLEN) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_timestamp_string: %s\n",
			   strerror (errno));
	  goto cleanup;
	}

      pstdout_printf (state_data->pstate,
		      "Last Day %s Power Time    : %s\n",
		      system_info_string,
		      time_buf);
    }

  pstdout_printf (state_data->pstate,
		  "Last Week %s Power        : %u W\n",
		  system_info_string,
		  last_week_power);

  if (system_info_parameter == IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_MAX_POWER_CONSUMPTION_STATISTICS
      || system_info_parameter == IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_MIN_POWER_CONSUMPTION_STATISTICS)
    {
      memset (time_buf, '\0', IPMI_OEM_TIME_BUFLEN + 1);

      if (ipmi_timestamp_string (last_week_power_time,
				 state_data->prog_data->args->common_args.utc_offset,
				 get_timestamp_flags (&(state_data->prog_data->args->common_args),
						      IPMI_TIMESTAMP_FLAG_DEFAULT), 
				 "%D - %T",
				 time_buf,
				 IPMI_OEM_TIME_BUFLEN) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_timestamp_string: %s\n",
			   strerror (errno));
	  goto cleanup;
	}

      pstdout_printf (state_data->pstate,
		      "Last Week %s Power Time   : %s\n",
		      system_info_string,
		      time_buf);
    }
     
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

/* legacy */
int
ipmi_oem_dell_get_average_power_history (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (!(state_data->prog_data->args->oem_options[0] = strdup ("average")))
    {
      pstdout_perror (state_data->pstate, "strdup");
      return (-1);
    }
  state_data->prog_data->args->oem_options_count++;
  
  return (ipmi_oem_dell_get_power_consumption_statistics (state_data));
}

/* legacy */
int
ipmi_oem_dell_get_peak_power_history (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (!(state_data->prog_data->args->oem_options[0] = strdup ("max")))
    {
      pstdout_perror (state_data->pstate, "strdup");
      return (-1);
    }
  state_data->prog_data->args->oem_options_count++;

  return (ipmi_oem_dell_get_power_consumption_statistics (state_data));
}

static int
_get_power_capacity (ipmi_oem_state_data_t *state_data,
		     uint8_t *configuration_parameter_data,
		     unsigned int configuration_parameter_data_len)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int len;
  int rv = -1;

  assert (state_data);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Source Code
   *
   * Uses Get System Info command
   *
   * Parameter data response formatted:
   *
   * bytes 1-2 - power capacity
   * bytes 3 - units
   *         - 0x00 watts (?)
   *         - 0x01 btuphr (?)
   *         - 0x03 percent (???)
   * bytes 4-5 - maximum power consumption
   * bytes 6-7 - minimum power consumption
   * bytes 8 - total number of power supplies
   * bytes 9-10 - available power
   * bytes 11 - system throttling
   *          - 0 - Normal system operation
   *          - 1 - System needs to be throttled
   *          - 2 - System is overconfigured
   * bytes 12 - reserved
   *
   */

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
                                           IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_POWER_CAPACITY,
                                           IPMI_SYSTEM_INFO_PARAMETERS_NO_SET_SELECTOR,
                                           IPMI_SYSTEM_INFO_PARAMETERS_NO_BLOCK_SELECTOR,
                                           obj_cmd_rs) < 0)
    {
      if ((ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
	   && ((ipmi_check_completion_code (obj_cmd_rs,
					    IPMI_COMP_CODE_GET_SYSTEM_INFO_PARAMETERS_PARAMETER_NOT_SUPPORTED) == 1)
	       || (ipmi_check_completion_code (obj_cmd_rs,
					       IPMI_COMP_CODE_OEM_DELL_NOT_LICENSED) == 1)))
	  || (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_OR_UNSUPPORTED
	      && ipmi_check_completion_code (obj_cmd_rs,
					     IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST) == 1))
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "%s:%s option not supported on this system\n",
			   state_data->prog_data->args->oem_id,
			   state_data->prog_data->args->oem_command);
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
                                configuration_parameter_data_len)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'configuration_parameter_data': %s\n",
                       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  
  if (len < 12)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_system_info_parameters: invalid buffer length returned: %d\n",
                       len);
      goto cleanup;
    }
  
  rv = 0;
 cleanup:
  return (rv);
}

static int
_get_power_capacity_status (ipmi_oem_state_data_t *state_data,
			    uint8_t *power_capacity_status,
			    uint8_t *power_capacity_is_settable)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;
  
  assert (state_data);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Source Code
   *
   * Power Capacity Status Request
   *
   * 0x30 - OEM network function
   * 0xBA - OEM cmd
   * 0x01 - ?? (I'm guessing a "get" option)
   * 0xFF - ?? (I'm guessing a "get all" bitmask)
   * 
   * Power Capacity Status Response
   *
   * 0xBA - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - status
   *      - 0x01 bitmask = 0b = disabled, 1b = enabled
   *      - 0x02 bitmask = 0b = not-settable, 1b = settable
   */

  bytes_rq[0] = IPMI_CMD_OEM_DELL_POWER_CAPACITY_STATUS;
  bytes_rq[1] = 0x01;
  bytes_rq[2] = 0xFF;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
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
                                                   3,
                                                   IPMI_CMD_OEM_DELL_POWER_CAPACITY_STATUS,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  if (power_capacity_status)
    {
      (*power_capacity_status) = (bytes_rs[2] & IPMI_OEM_DELL_GET_POWER_CAPACITY_STATUS_BITMASK);
      (*power_capacity_status) >>= IPMI_OEM_DELL_GET_POWER_CAPACITY_STATUS_SHIFT;
    }

  if (power_capacity_is_settable)
    {
      (*power_capacity_is_settable) = (bytes_rs[2] & IPMI_OEM_DELL_GET_POWER_CAPACITY_IS_SETTABLE_BITMASK);
      (*power_capacity_is_settable) >>= IPMI_OEM_DELL_GET_POWER_CAPACITY_IS_SETTABLE_SHIFT;
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_power_capacity (ipmi_oem_state_data_t *state_data)
{
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  uint16_t power_capacity;
  uint8_t units;
  uint16_t maximum_power_consumption;
  uint16_t minimum_power_consumption;
  uint8_t total_number_power_supplies;
  uint16_t available_power;
  uint8_t system_throttling;
  char *system_throttling_str;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_get_power_capacity (state_data,
			   configuration_parameter_data,
			   IPMI_OEM_MAX_BYTES) < 0)
    goto cleanup;
  
  power_capacity = configuration_parameter_data[0];
  power_capacity |= (configuration_parameter_data[1] << 8);

  units = configuration_parameter_data[2];

  maximum_power_consumption = configuration_parameter_data[3];
  maximum_power_consumption |= (configuration_parameter_data[4] << 8);

  minimum_power_consumption = configuration_parameter_data[5];
  minimum_power_consumption |= (configuration_parameter_data[6] << 8);

  total_number_power_supplies = configuration_parameter_data[7];

  available_power = configuration_parameter_data[8];
  available_power |= (configuration_parameter_data[9] << 8);

  system_throttling = configuration_parameter_data[10];

  pstdout_printf (state_data->pstate,
                  "Power Capacity                 : %u W\n",
		  power_capacity);

  pstdout_printf (state_data->pstate,
                  "Minimum Power Consumption      : %u W\n",
		  minimum_power_consumption);

  pstdout_printf (state_data->pstate,
                  "Maximum Power Consumption      : %u W\n",
		  maximum_power_consumption);

  pstdout_printf (state_data->pstate,
		  "Total Number of Power Supplies : %u\n",
		  total_number_power_supplies);

  pstdout_printf (state_data->pstate,
		  "Available Power                : %u W\n",
		  available_power);

  switch (system_throttling)
    {
    case IPMI_OEM_DELL_SYSTEM_INFO_GET_POWER_CAPACITY_SYSTEM_THROTTLING_NORMAL_SYSTEM_OPERATION:
      system_throttling_str = "Normal system operation";
      break;
    case IPMI_OEM_DELL_SYSTEM_INFO_GET_POWER_CAPACITY_SYSTEM_THROTTLING_SYSTEM_NEEDS_TO_BE_THROTTLED:
      system_throttling_str = "System needs to be throttled";
      break;
    case IPMI_OEM_DELL_SYSTEM_INFO_GET_POWER_CAPACITY_SYSTEM_THROTTLING_SYSTEM_IS_OVERCONFIGURED:
      system_throttling_str = "System is overconfigured";
      break;
    default:
      system_throttling_str = "Unknown";
    }

  pstdout_printf (state_data->pstate,
		  "System Throttling              : %s\n",
		  system_throttling_str);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_set_power_capacity (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  uint8_t power_capacity_is_settable = 0;
  uint16_t power_capacity;
  uint16_t maximum_power_consumption;
  uint16_t minimum_power_consumption;
  unsigned int temp;
  char *endptr = NULL;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Source Code
   *
   * Uses Set System Info command
   *
   * Configuration Parameter Data formatted:
   *
   * bytes 1-2 - power capacity
   * bytes 3 - units
   *         - 0x00 watts (?)
   *         - 0x01 btuphr (?)
   *         - 0x03 percent (???)
   * bytes 4-5 - maximum power consumption
   * bytes 6-7 - minimum power consumption
   * bytes 8 - total number of power supplies
   * bytes 9-10 - available power
   * bytes 11 - system throttling
   * bytes 12 - reserved
   *
   */

  if (_get_power_capacity_status (state_data, NULL, &power_capacity_is_settable) < 0)
    goto cleanup;

  if (!power_capacity_is_settable)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "Power Capacity not settable\n");
      goto cleanup;
    }

  if (_get_power_capacity (state_data,
			   configuration_parameter_data,
			   IPMI_OEM_MAX_BYTES) < 0)
    goto cleanup;

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
  power_capacity = temp;

  maximum_power_consumption = configuration_parameter_data[3];
  maximum_power_consumption |= (configuration_parameter_data[4] << 8);

  minimum_power_consumption = configuration_parameter_data[5];
  minimum_power_consumption |= (configuration_parameter_data[6] << 8);

  if (power_capacity < minimum_power_consumption || power_capacity > maximum_power_consumption)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "Power Capacity '%u' out of range\n",
		       power_capacity);
      goto cleanup;
    }

  configuration_parameter_data[0] = (power_capacity & 0x00FF);
  configuration_parameter_data[1] = ((power_capacity & 0xFF00) >> 8);

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_system_info_parameters_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }
  
  if (ipmi_cmd_set_system_info_parameters (state_data->ipmi_ctx,
                                           IPMI_SYSTEM_INFO_PARAMETER_OEM_DELL_POWER_CAPACITY,
					   configuration_parameter_data,
					   12,
					   obj_cmd_rs) < 0)
    {
      if ((ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
	   && ((ipmi_check_completion_code (obj_cmd_rs,
					    IPMI_COMP_CODE_GET_SYSTEM_INFO_PARAMETERS_PARAMETER_NOT_SUPPORTED) == 1)
	       || (ipmi_check_completion_code (obj_cmd_rs,
					       IPMI_COMP_CODE_OEM_DELL_NOT_LICENSED) == 1)))
	  || (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_COMMAND_INVALID_OR_UNSUPPORTED
	      && ipmi_check_completion_code (obj_cmd_rs,
					     IPMI_COMP_CODE_INVALID_DATA_FIELD_IN_REQUEST) == 1))
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "%s:%s option not supported on this system\n",
			   state_data->prog_data->args->oem_id,
			   state_data->prog_data->args->oem_command);
	  goto cleanup;
	}
	
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_set_system_info_parameters: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_dell_get_power_capacity_status (ipmi_oem_state_data_t *state_data)
{
  uint8_t power_capacity_status = 0;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_get_power_capacity_status (state_data, &power_capacity_status, NULL) < 0)
    goto cleanup;

  if (power_capacity_status)
    pstdout_printf (state_data->pstate, "enabled\n");
  else
    pstdout_printf (state_data->pstate, "disabled\n");

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_set_power_capacity_status (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t power_capacity_is_settable = 0;
  int rs_len;
  int rv = -1;
  
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

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

  /* Dell Poweredge OEM
   *
   * From Dell Provided Source Code
   *
   * Power Capacity Status Request
   *
   * 0x30 - OEM network function
   * 0xBA - OEM cmd
   * 0x00 - ?? (I'm guessing a "set" option)
   * 0x?? - ?? (I'm guessing a bitmask)
   *      - 0x01 bitmask = 0b = disable, 1b = enable
   * 
   * Power Capacity Status Response
   *
   * 0xBA - OEM cmd
   * 0x?? - Completion Code
   */

  if (_get_power_capacity_status (state_data, NULL, &power_capacity_is_settable) < 0)
    goto cleanup;

  if (!power_capacity_is_settable)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "Power Capacity not settable\n");
      goto cleanup;
    }

  bytes_rq[0] = IPMI_CMD_OEM_DELL_POWER_CAPACITY_STATUS;
  bytes_rq[1] = 0x00;
  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "enable"))
    bytes_rq[2] = IPMI_OEM_DELL_SET_POWER_CAPACITY_STATUS_ENABLE;
  else
    bytes_rq[2] = IPMI_OEM_DELL_SET_POWER_CAPACITY_STATUS_DISABLE;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
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
                                                   2,
                                                   IPMI_CMD_OEM_DELL_POWER_CAPACITY_STATUS,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_chassis_identify_status (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t identify_status;
  char *identify_status_str;
  int rs_len;
  int rv = -1;
  
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * Query Chassis Identify Status Request
   *
   * 0x30 - OEM network function
   * 0x32 - OEM cmd
   * 
   * Query Chassis Identify Status Response
   * 
   * 0x32 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - identify status
   * - 0x00 - off
   * - 0x01 - on
   */

  bytes_rq[0] = IPMI_CMD_OEM_DELL_QUERY_CHASSIS_IDENTIFY_STATUS;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
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
                                                   IPMI_CMD_OEM_DELL_QUERY_CHASSIS_IDENTIFY_STATUS,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;

  identify_status = bytes_rs[2];

  switch (identify_status)
    {
    case IPMI_OEM_DELL_QUERY_CHASSIS_IDENTIFY_STATUS_OFF:
      identify_status_str = "Off";
      break;
    case IPMI_OEM_DELL_QUERY_CHASSIS_IDENTIFY_STATUS_ON:
      identify_status_str = "On";
      break;
    default:
      identify_status_str = "Unknown";
    }
  
  pstdout_printf (state_data->pstate,
		  "%s\n",
		  identify_status_str);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_power_monitoring_over_interval (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  unsigned int temp;
  char *endptr = NULL;
  unsigned int power_monitoring_averaging_interval;
  uint16_t average_power_consumption;
  uint16_t min_power_consumption;
  uint16_t max_power_consumption;
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 2);

  errno = 0;
  temp = strtoul (state_data->prog_data->args->oem_options[0], &endptr, 10);
  if (errno
      || endptr[0] != '\0'
      || temp < IPMI_OEM_DELL_POWER_MONITORING_INTERVAL_MIN
      || temp > IPMI_OEM_DELL_POWER_MONITORING_INTERVAL_MAX)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "%s:%s invalid OEM option argument '%s'\n",
		       state_data->prog_data->args->oem_id,
		       state_data->prog_data->args->oem_command,
		       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }
  power_monitoring_averaging_interval = temp;

  if (strcasecmp (state_data->prog_data->args->oem_options[1], "systempower")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "cpu1")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "cpu2")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "cpu3")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "cpu4")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "memory1")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "memory2")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "memory3")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "memory4")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "drives")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "fans")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "pciecards")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "gpucables"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }

  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * Power Monitoring Over Specified Averaging Interval Request
   *
   * 0x30 - OEM network function
   * 0xCC - OEM cmd
   * byte 3-4 - power monitoring averaging interval 
   * 0x?? - subsystem
   *      - 0h - system power
   *      - 1h - cpu1
   *      - 2h - cpu2
   *      - 3h - cpu3
   *      - 4h - cpu4
   *      - 5h - memory1
   *      - 6h - memory2
   *      - 7h - memory3
   *      - 8h - memory4
   *      - 9h - drives
   *      - ah - fans
   *      - bh - pcie add-in cards
   *      - ch - gpu cables
   * 0x?? - control switch
   *        - 0x00 - return average, min and max power value and the
   *          averaging interval is 30-900s
   *        - 0x01 - only return averaged power value and the averaging
   *          interval is 5-900s
   * 
   * Power Monitoring Over Specified Averaging Interval Response
   * 
   * 0xCC - OEM cmd
   * 0x?? - Completion Code
   * bytes 2-3 - average power consumption
   * bytes 4-5 - min power consumption
   * bytes 6-7 - max power consumption
   * bytes 8-12 - reserved
   */

  bytes_rq[0] = IPMI_CMD_OEM_DELL_POWER_MONITORING_OVER_A_SPECIFIED_AVERAGING_INTERVAL2;
  bytes_rq[1] = (power_monitoring_averaging_interval & 0x00FF);
  bytes_rq[2] = (power_monitoring_averaging_interval & 0xFF00) >> 8;

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "systempower"))
    bytes_rq[3] = IPMI_OEM_DELL_POWER_MONITORING_SYSTEM_POWER;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "cpu1"))
    bytes_rq[3] = IPMI_OEM_DELL_POWER_MONITORING_CPU1_SUBSYSTEM;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "cpu2"))
    bytes_rq[3] = IPMI_OEM_DELL_POWER_MONITORING_CPU2_SUBSYSTEM;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "cpu3"))
    bytes_rq[3] = IPMI_OEM_DELL_POWER_MONITORING_CPU3_SUBSYSTEM;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "cpu4"))
    bytes_rq[3] = IPMI_OEM_DELL_POWER_MONITORING_CPU4_SUBSYSTEM;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "memory1"))
    bytes_rq[3] = IPMI_OEM_DELL_POWER_MONITORING_MEMORY_POWER_OF_CPU_DOMAIN1;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "memory2"))
    bytes_rq[3] = IPMI_OEM_DELL_POWER_MONITORING_MEMORY_POWER_OF_CPU_DOMAIN2;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "memory3"))
    bytes_rq[3] = IPMI_OEM_DELL_POWER_MONITORING_MEMORY_POWER_OF_CPU_DOMAIN3;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "memory4"))
    bytes_rq[3] = IPMI_OEM_DELL_POWER_MONITORING_MEMORY_POWER_OF_CPU_DOMAIN4;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "drives"))
    bytes_rq[3] = IPMI_OEM_DELL_POWER_MONITORING_DRIVES;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "fans"))
    bytes_rq[3] = IPMI_OEM_DELL_POWER_MONITORING_FANS;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "pciecards"))
    bytes_rq[3] = IPMI_OEM_DELL_POWER_MONITORING_PCIE_ADD_IN_CARDS;
  else /* !strcasecmp (state_data->prog_data->args->oem_options[0], "gpucables") */
    bytes_rq[3] = IPMI_OEM_DELL_POWER_MONITORING_GPU_CABLES;

  if (power_monitoring_averaging_interval > IPMI_OEM_DELL_POWER_MONITORING_INTERVAL_AVG_MIN_MAX_MIN)
    bytes_rq[4] = IPMI_OEM_DELL_POWER_MONITORING_RETURN_AVG_MIN_MAX_POWER;
  else
    bytes_rq[4] = IPMI_OEM_DELL_POWER_MONITORING_RETURN_ONLY_AVG_POWER;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
                              bytes_rq, /* data */
                              5, /* num bytes */
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
                                                   8,
                                                   IPMI_CMD_OEM_DELL_POWER_MONITORING_OVER_A_SPECIFIED_AVERAGING_INTERVAL2,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_RS,
                                                   _dell_extended_oem_strerror) < 0)
    goto cleanup;

  average_power_consumption = bytes_rs[2];
  average_power_consumption |= (bytes_rs[3] << 8); 

  pstdout_printf (state_data->pstate,
		  "Average Power Consumption : %u W\n",
		  average_power_consumption);

  if (power_monitoring_averaging_interval > IPMI_OEM_DELL_POWER_MONITORING_INTERVAL_AVG_MIN_MAX_MIN)
    {
      min_power_consumption = bytes_rs[4];
      min_power_consumption |= (bytes_rs[5] << 8); 
      
      max_power_consumption = bytes_rs[6];
      max_power_consumption |= (bytes_rs[7] << 8); 

      pstdout_printf (state_data->pstate,
		      "Min Power Consumption     : %u W\n",
		      min_power_consumption);
      
      pstdout_printf (state_data->pstate,
		      "Max Power Consumption     : %u W\n",
		      max_power_consumption);
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_power_monitoring_interval_range (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint16_t min_power_monitoring_averaging_interval;
  uint16_t max_power_monitoring_averaging_interval;
  uint8_t stepping_interval;
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "systempower")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "cpu1")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "cpu2")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "cpu3")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "cpu4")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "memory1")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "memory2")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "memory3")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "memory4")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "drives")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "fans")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "pciecards")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "gpucables"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * Power Monitoring Over Specified Averaging Interval Request
   *
   * 0x30 - OEM network function
   * 0xCD - OEM cmd
   * 0x?? - subsystem
   *      - 0h - system power
   *      - 1h - cpu1
   *      - 2h - cpu2
   *      - 3h - cpu3
   *      - 4h - cpu4
   *      - 5h - memory1
   *      - 6h - memory2
   *      - 7h - memory3
   *      - 8h - memory4
   *      - 9h - drives
   *      - ah - fans
   *      - bh - pcie add-in cards
   *      - ch - gpu cables
   * 0x?? - control switch
   *        - 0x00 - return average, min and max power value and the
   *          averaging interval is 30-900s
   *        - 0x01 - only return averaged power value and the averaging
   *          interval is 5-900s
   * 
   * Power Monitoring Over Specified Averaging Interval Response
   * 
   * 0xCD - OEM cmd
   * 0x?? - Completion Code
   * bytes 2-3 - min power monitoring averaging interval
   * bytes 4-5 - max power monitoring averaging interval
   * bytes 6 - stepping interval
   */

  bytes_rq[0] = IPMI_CMD_OEM_DELL_POWER_MONITORING_AVERAGING_INTERVAL_RANGE2;

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "systempower"))
    bytes_rq[1] = IPMI_OEM_DELL_POWER_MONITORING_SYSTEM_POWER;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "cpu1"))
    bytes_rq[1] = IPMI_OEM_DELL_POWER_MONITORING_CPU1_SUBSYSTEM;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "cpu2"))
    bytes_rq[1] = IPMI_OEM_DELL_POWER_MONITORING_CPU2_SUBSYSTEM;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "cpu3"))
    bytes_rq[1] = IPMI_OEM_DELL_POWER_MONITORING_CPU3_SUBSYSTEM;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "cpu4"))
    bytes_rq[1] = IPMI_OEM_DELL_POWER_MONITORING_CPU4_SUBSYSTEM;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "memory1"))
    bytes_rq[1] = IPMI_OEM_DELL_POWER_MONITORING_MEMORY_POWER_OF_CPU_DOMAIN1;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "memory2"))
    bytes_rq[1] = IPMI_OEM_DELL_POWER_MONITORING_MEMORY_POWER_OF_CPU_DOMAIN2;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "memory3"))
    bytes_rq[1] = IPMI_OEM_DELL_POWER_MONITORING_MEMORY_POWER_OF_CPU_DOMAIN3;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "memory4"))
    bytes_rq[1] = IPMI_OEM_DELL_POWER_MONITORING_MEMORY_POWER_OF_CPU_DOMAIN4;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "drives"))
    bytes_rq[1] = IPMI_OEM_DELL_POWER_MONITORING_DRIVES;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "fans"))
    bytes_rq[1] = IPMI_OEM_DELL_POWER_MONITORING_FANS;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "pciecards"))
    bytes_rq[1] = IPMI_OEM_DELL_POWER_MONITORING_PCIE_ADD_IN_CARDS;
  else /* !strcasecmp (state_data->prog_data->args->oem_options[0], "gpucables") */
    bytes_rq[1] = IPMI_OEM_DELL_POWER_MONITORING_GPU_CABLES;

  /* achu: why would Dell allow you to input either control switch
   * option, makes no sense
   */
  bytes_rq[2] = IPMI_OEM_DELL_POWER_MONITORING_RETURN_ONLY_AVG_POWER;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
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
                                                   7,
						   IPMI_OEM_DELL_POWER_MONITORING_RETURN_ONLY_AVG_POWER,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_RS,
                                                   _dell_extended_oem_strerror) < 0)
    goto cleanup;

  min_power_monitoring_averaging_interval = bytes_rs[2];
  min_power_monitoring_averaging_interval |= (bytes_rs[3] << 8); 

  max_power_monitoring_averaging_interval = bytes_rs[4];
  max_power_monitoring_averaging_interval |= (bytes_rs[5] << 8); 

  stepping_interval = bytes_rs[6];

  pstdout_printf (state_data->pstate,
		  "Min Power Monitoring Averaging Interval : %u s\n",
		  min_power_monitoring_averaging_interval);
  
  pstdout_printf (state_data->pstate,
		  "Max Power Monitoring Averaging Interval : %u s\n",
		  max_power_monitoring_averaging_interval);
  
  pstdout_printf (state_data->pstate,
		  "Stepping Interval                       : %u s\n",
		  stepping_interval);
  
  rv = 0;
 cleanup:
  return (rv);
}

#if 0
/* cannot verify */
int
ipmi_oem_dell_get_blade_slot_id (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t sleeve_based_blade;
  uint8_t blade_slot_number;
  int rs_len;
  int rv = -1;
  
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * Get Blade Slot Id Request
   *
   * 0x30 - OEM network function
   * 0x18 - OEM cmd
   * 
   * Get Blade Slot Id Response
   * 
   * 0x18 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - bit 7 - sleeve based blade ; 0 = no, 1 = yes
   *      - bit 6 - reserved
   *      - bit 5:0 - blade slot number (1 based)
   *
   * For regular blades: blades are numbered like
   *
   * 1  2  3  4  5  6  7  8
   * 9 10 11 12 13 14 15 16 
   *
   * For sleeve based blades, blades are numbered like
   *
   * a  1  2  3  4  5  6  7  8
   * b 17 18 19 20 21 22 23 24
   * c  9 10 11 12 13 14 15 16
   * d 25 26 27 28 39 30 31 32
   */

  bytes_rq[0] = IPMI_CMD_OEM_DELL_GET_BLADE_SLOT_ID;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
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
                                                   IPMI_CMD_OEM_DELL_GET_BLADE_SLOT_ID,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;
  
  sleeve_based_blade = (bytes_rs[2] & IPMI_OEM_DELL_SLEEVE_BASED_BLADE_BITMASK);
  sleeve_based_blade >>= IPMI_OEM_DELL_SLEEVE_BASED_BLADE_SHIFT;

  blade_slot_number = (bytes_rs[2] & IPMI_OEM_DELL_BLADE_SLOT_NUMBER_BITMASK);
  blade_slot_number >>= IPMI_OEM_DELL_BLADE_SLOT_NUMBER_SHIFT;

  if (sleeve_based_blade == IPMI_OEM_DELL_SLEEVE_BASED_BLADE_YES)
    {
      char prefix_char;
  
      if (blade_slot_number >= IPMI_OEM_DELL_SLEEVE_A_RANGE_MIN
	  && blade_slot_number <= IPMI_OEM_DELL_SLEEVE_A_RANGE_MAX)
	prefix_char = 'A';
      else if (blade_slot_number >= IPMI_OEM_DELL_SLEEVE_B_RANGE_MIN
	       && blade_slot_number <= IPMI_OEM_DELL_SLEEVE_B_RANGE_MAX)
	prefix_char = 'B';
      else if (blade_slot_number >= IPMI_OEM_DELL_SLEEVE_C_RANGE_MIN
	       && blade_slot_number <= IPMI_OEM_DELL_SLEEVE_C_RANGE_MAX)
	prefix_char = 'C';
      else if (blade_slot_number >= IPMI_OEM_DELL_SLEEVE_D_RANGE_MIN
	       && blade_slot_number <= IPMI_OEM_DELL_SLEEVE_D_RANGE_MAX)
	prefix_char = 'D';
      else
	prefix_char = '\0';	/* unknown */

      pstdout_printf (state_data->pstate,
		      "%c%u\n",
		      prefix_char,
		      blade_slot_number);
    }
  else
    pstdout_printf (state_data->pstate,
		    "%u\n",
		    blade_slot_number);

  rv = 0;
 cleanup:
  return (rv);
}
#endif

int
ipmi_oem_dell_get_last_post_code (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t post_code;
  uint8_t string_length;
  char post_code_string[IPMI_OEM_STR_BUFLEN + 1];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * Get Last Post Code Request
   *
   * 0x30 - OEM network function
   * 0x99 - OEM cmd
   * 
   * Get Last Post Code Response
   * 
   * 0x99 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - post code
   * 0x?? - string length
   * bytes 4-N: string
   */

  memset (post_code_string, '\0', IPMI_OEM_STR_BUFLEN + 1);

  bytes_rq[0] = IPMI_CMD_OEM_DELL_GET_LAST_POST_CODE;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
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
                                                   4,
                                                   IPMI_CMD_OEM_DELL_GET_LAST_POST_CODE,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;
  
  post_code = bytes_rs[2];
  string_length = bytes_rs[3];

  if (string_length)
    memcpy (post_code_string, &bytes_rs[4], string_length);

  pstdout_printf (state_data->pstate,
		  "Post Code %02Xh : %s\n",
		  post_code,
		  post_code_string);

  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_oem_dell_do_slot_power_toggle (ipmi_oem_state_data_t *state_data,
				     unsigned int slot_number)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (slot_number >= IPMI_OEM_DELL_SLOT_POWER_CONTROL_SLOT_NUMBER_MIN
	  && slot_number <= IPMI_OEM_DELL_SLOT_POWER_CONTROL_SLOT_NUMBER_MAX);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * Slot Power Control Request
   *
   * 0x30 - OEM network function
   * 0xF0 - OEM cmd
   * 0x?? - bit 0 - slot 1
   *      - bit 1 - slot 2
   *      - ...
   *      - bit 7 - slot 8
   * 0x?? - bit 0 - slot 9
   *      - bit 1 - slot 10
   *      - ...
   *      - bit 7 - slot 16
   *
   * only should do one slot at a time
   *
   * Slot Power Control Response
   *
   * 0xF0 - OEM cmd
   * 0x?? - Completion Code
   */
  
  bytes_rq[0] = IPMI_CMD_OEM_DELL_SLOT_POWER_CONTROL;
  bytes_rq[1] = 0;
  bytes_rq[2] = 0;
  bytes_rq[1 + (slot_number - 1) / 8] = 0x1 << ((slot_number - 1) % 8);
  
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
			      0, /* lun */
			      IPMI_NET_FN_OEM_DELL_GENERIC_RQ, /* network function */
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
						   2,
						   IPMI_CMD_OEM_DELL_SLOT_POWER_CONTROL,
						   IPMI_NET_FN_OEM_DELL_GENERIC_RS,
						   NULL) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_slot_power_toggle (ipmi_oem_state_data_t *state_data)
{
  char *endptr = NULL;
  unsigned int slot_number;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);
  
  errno = 0;
  slot_number = strtoul (state_data->prog_data->args->oem_options[0], &endptr, 10);
  if (errno
      || endptr[0] != '\0')
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "%s:%s invalid OEM option argument '%s'\n",
		       state_data->prog_data->args->oem_id,
		       state_data->prog_data->args->oem_command,
		       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }
  
  if (slot_number < IPMI_OEM_DELL_SLOT_POWER_CONTROL_SLOT_NUMBER_MIN
      || slot_number > IPMI_OEM_DELL_SLOT_POWER_CONTROL_SLOT_NUMBER_MAX)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "%s:%s invalid OEM option argument '%s' : out of range\n",
		       state_data->prog_data->args->oem_id,
		       state_data->prog_data->args->oem_command,
		       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }
  
  if (_ipmi_oem_dell_do_slot_power_toggle (state_data, slot_number) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

/* achu
 *
 * Under normal circumstances we should find the proper SDR entry for
 * a given slot number input from the user, do a get sensor reading,
 * decode the raw sensor reading into a real reading, and compare to
 * 0.0 appropriately.
 *
 * However, for the C410x, it happens that all the SDR conversions are
 * simple.  It's simply the raw reading * 2 to get the real reading.
 * So if we are checking for 0 degrees Celsius, then a raw reading of
 * 0 is 0 degrees Celsius.
 *
 * In order to avoid reading, parsing, etc. the SDR, we'll just call
 * get sensor reading w/ the known sensor number and check the raw
 * response.  I leave the original code in b/c someday I may need to
 * work that code back in for different motherboards.
 */
#define IPMI_OEM_DELL_SLOT_POWER_CONTROL_OPTIMIZE 1

int
ipmi_oem_dell_slot_power_control (ipmi_oem_state_data_t *state_data)
{
  char *endptr = NULL;
  unsigned int slot_number;
  /* See comments above w/ IPMI_OEM_DELL_SLOT_POWER_CONTROL_OPTIMIZE */
#if IPMI_OEM_DELL_SLOT_POWER_CONTROL_OPTIMIZE
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t sensor_reading;
  uint8_t reading_state;
  uint8_t sensor_scanning;
  uint64_t val;
#else /* !IPMI_OEM_DELL_SLOT_POWER_CONTROL_OPTIMIZE */
  ipmi_sensor_read_ctx_t sensor_read_ctx = NULL;
  uint8_t sdr_record[IPMI_SDR_MAX_RECORD_LENGTH];
  int sdr_record_len = 0;
  int sensor_found = 0;
  double *sensor_reading = NULL;
  uint16_t sensor_reading_bitmask;
#endif /* !IPMI_OEM_DELL_SLOT_POWER_CONTROL_OPTIMIZE */
  int slot_power_on_flag;
  int rv = -1;
  
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 3);
  
  if (strcasecmp (state_data->prog_data->args->oem_options[0], "c410x"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }
  
  if (strcasecmp (state_data->prog_data->args->oem_options[1], "on")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "off")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "status"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }

  errno = 0;
  slot_number = strtoul (state_data->prog_data->args->oem_options[2], &endptr, 10);
  if (errno
      || endptr[0] != '\0')
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "%s:%s invalid OEM option argument '%s'\n",
		       state_data->prog_data->args->oem_id,
		       state_data->prog_data->args->oem_command,
		       state_data->prog_data->args->oem_options[2]);
      goto cleanup;
    }

  if (slot_number < IPMI_OEM_DELL_SLOT_POWER_CONTROL_SLOT_NUMBER_MIN
      || slot_number > IPMI_OEM_DELL_SLOT_POWER_CONTROL_SLOT_NUMBER_MAX)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "%s:%s invalid OEM option argument '%s' : out of range\n",
		       state_data->prog_data->args->oem_id,
		       state_data->prog_data->args->oem_command,
		       state_data->prog_data->args->oem_options[2]);
      goto cleanup;
    }
  
  /* See comments above w/ IPMI_OEM_DELL_SLOT_POWER_CONTROL_OPTIMIZE */
#if IPMI_OEM_DELL_SLOT_POWER_CONTROL_OPTIMIZE
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sensor_reading_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }
  
  /* sensor numbers for these slots range from 0x50 to 0x5F */
  if (ipmi_cmd_get_sensor_reading (state_data->ipmi_ctx,
				   IPMI_SENSOR_NUMBER_OEM_DELL_C410X_PCIE_1_WATT + (slot_number - 1),
				   obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_cmd_get_sensor_reading: %s\n",
		       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }
  
  if (FIID_OBJ_GET (obj_cmd_rs,
		    "sensor_reading",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "fiid_obj_get: 'sensor_reading': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sensor_reading = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "reading_state",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "fiid_obj_get: 'reading_state': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  reading_state = val;

  if (FIID_OBJ_GET (obj_cmd_rs,
		    "sensor_scanning",
		    &val) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "fiid_obj_get: 'sensor_scanning': %s\n",
		       fiid_obj_errormsg (obj_cmd_rs));
      goto cleanup;
    }
  sensor_scanning = val;

  if (reading_state == IPMI_SENSOR_READING_STATE_UNAVAILABLE
      || sensor_scanning == IPMI_SENSOR_SCANNING_ON_THIS_SENSOR_DISABLE)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "PCIe slot sensor reading cannot be determined\n");
      goto cleanup;
    }

  /* If non-zero, then it's on */
  /* achu: Sometimes "off" is 2.0 Watts, which equates to a sensor reading of 1 */
  if (sensor_reading > 1)
    slot_power_on_flag = 1;
  else
    slot_power_on_flag = 0;

#else /* !IPMI_OEM_DELL_SLOT_POWER_CONTROL_OPTIMIZE */
  if (!(sensor_read_ctx = ipmi_sensor_read_ctx_create (state_data->ipmi_ctx)))
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sensor_read_ctx_create: %s\n",
		       strerror (errno));
      goto cleanup;
    }
        
  if (sdr_cache_create_and_load (state_data->sdr_ctx,
				 state_data->pstate,
				 state_data->ipmi_ctx,
				 state_data->hostname,
 				 &state_data->prog_data->args->common_args) < 0)
    goto cleanup;

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "c410x"))
    {
      if (ipmi_sdr_cache_search_sensor (state_data->sdr_ctx,
					(IPMI_SENSOR_NUMBER_OEM_DELL_C410X_PCIE_1_WATT + (slot_number - 1)),
					IPMI_SLAVE_ADDRESS_BMC) < 0)
	{
	  if (ipmi_sdr_ctx_errnum (state_data->sdr_ctx) != IPMI_SDR_ERR_NOT_FOUND)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "ipmi_sdr_cache_search_sensor: %s\n",
			       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	      goto cleanup;
	    }
	}
      else
	sensor_found = 1;
    }
  
  if (!sensor_found)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "Sensor representing slot %u not found\n",
		       slot_number);
      goto cleanup;
    }
  
  if ((sdr_record_len = ipmi_sdr_cache_record_read (state_data->sdr_ctx,
						    sdr_record,
						    IPMI_SDR_MAX_RECORD_LENGTH)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_cache_record_read: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  if (ipmi_sensor_read (sensor_read_ctx,
			sdr_record,
			sdr_record_len,
			0,
			NULL,
			&sensor_reading,
			&sensor_reading_bitmask) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sensor_read: %s\n",
		       ipmi_sensor_read_ctx_errormsg (sensor_read_ctx));
      goto cleanup;
    }
      
  if (!sensor_reading)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "Failed to retrieve watt reading for PCIe slot\n");
      goto cleanup;
    }
  
  /* If non-zero, then it's on */
  if (fabs (*sensor_reading) < IPMI_OEM_DELL_ZERO_DEGREE_EPSILON)
    slot_power_on_flag = 0;
  else
    slot_power_on_flag = 1;

#endif /* !IPMI_OEM_DELL_SLOT_POWER_CONTROL_OPTIMIZE */

  if (!strcasecmp (state_data->prog_data->args->oem_options[1], "status"))
    {
      pstdout_printf (state_data->pstate,
		      "%s\n",
		      slot_power_on_flag ? "on" : "off");
      goto out;
    }
  
  /* don't do power toggle if situation not right for it */
  if ((!strcasecmp (state_data->prog_data->args->oem_options[1], "on")
       && slot_power_on_flag)
      || (!strcasecmp (state_data->prog_data->args->oem_options[1], "off")
	  && !slot_power_on_flag))
    goto out;
  
  if (_ipmi_oem_dell_do_slot_power_toggle (state_data, slot_number) < 0)
    goto cleanup;
  
 out:  
  rv = 0;
 cleanup:
  /* See comments above w/ IPMI_OEM_DELL_SLOT_POWER_CONTROL_OPTIMIZE */
#if IPMI_OEM_DELL_SLOT_POWER_CONTROL_OPTIMIZE
  fiid_obj_destroy (obj_cmd_rs);
#else /* !IPMI_OEM_DELL_SLOT_POWER_CONTROL_OPTIMIZE */
  ipmi_sensor_read_ctx_destroy (sensor_read_ctx);
  free (sensor_reading);
#endif /* !IPMI_OEM_DELL_SLOT_POWER_CONTROL_OPTIMIZE */
  return (rv);
}

int
ipmi_oem_dell_get_port_map (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t control_type;
  char *control_type_str;
  uint8_t ipass_mapping1;
  uint8_t ipass_mapping2;
  uint8_t ipass_mapping3;
  uint8_t ipass_mapping4;
  uint8_t slot_mapping1;
  uint8_t slot_mapping2;
  int rs_len;
  int rv = -1;
  
  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * Port Map Configure Request
   *
   * 0x34 - OEM network function
   * 0xC8 - OEM cmd
   * 0x?? - bit 7 - get/set
   *        - 0 - get
   *        - 1 - set
   *      - bit 6:4 - control type
   *        - 1 - jumper
   *        - 2 - bmc
   *      - bit 3 - failover ipass mapping 1
   *        - 0 - 1:4
   *        - 1 - 1:2
   *      - bit 2 - failover ipass mapping 2
   *        - 0 - 1:4
   *        - 1 - 1:2
   *      - bit 1 - failover ipass mapping 3
   *        - 0 - 1:4
   *        - 1 - 1:2
   *      - bit 0 - failover ipass mapping 4
   *        - 0 - 1:4
   *        - 1 - 1:2
   * 0x?? - optional byte - new port map setting
   *      - on get, must specify byte to get equivalent byte of response
   *      - bit 7:4 - slot mapping 1,2,3,4,13,14,15,16
   *      - bit 3:0 - slot mapping 5,6,7,8,9,10,11,12
   *        - for both
   *        - 1 - 1:2 or 1:4 mode (indicated in previous byte)
   *        - 2 - 1:8 mode
   *
   * Port Map Configure Response
   *
   * 0xC8 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - bit 7:4 - control type
   *        - 1 - jumper
   *        - 2 - bmc
   *      - bit 3 - failover ipass mapping 1
   *        - 0 - 1:4
   *        - 1 - 1:2
   *      - bit 2 - failover ipass mapping 2
   *        - 0 - 1:4
   *        - 1 - 1:2
   *      - bit 1 - failover ipass mapping 3
   *        - 0 - 1:4
   *        - 1 - 1:2
   *      - bit 0 - failover ipass mapping 4
   *        - 0 - 1:4
   *        - 1 - 1:2
   * 0x?? - optional byte
   *      - bit 7:4 - slot mapping 1,2,3,4,13,14,15,16
   *      - bit 3:0 - slot mapping 5,6,7,8,9,10,11,12
   *        - for both
   *        - 0 - 1:8 mode not supported
   *        - 1 - 1:2 or 1:4 mode (indicated in previous byte)
   *        - 2 - 1:8 mode
   */

  bytes_rq[0] = IPMI_CMD_OEM_DELL_PORT_MAP;
  bytes_rq[1] = (IPMI_OEM_DELL_PORT_MAP_GET << IPMI_OEM_DELL_PORT_MAP_GET_SET_SHIFT);
  bytes_rq[2] = 0x00;		/* to force proper return of data */

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_PORT_MAP_RQ, /* network function */
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
                                                   4,
                                                   IPMI_CMD_OEM_DELL_PORT_MAP,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_PORT_MAP_RS,
                                                   NULL) < 0)
    goto cleanup;


  control_type = (bytes_rs[2] & IPMI_OEM_DELL_PORT_MAP_CONTROL_TYPE_RESPONSE_BITMASK);
  control_type >>= IPMI_OEM_DELL_PORT_MAP_CONTROL_TYPE_REQUEST_SHIFT;

  ipass_mapping1 = (bytes_rs[2] & IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_BITMASK);
  ipass_mapping1 >>= IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_SHIFT;

  ipass_mapping2 = (bytes_rs[2] & IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_2_BITMASK);
  ipass_mapping2 >>= IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_2_SHIFT;

  ipass_mapping3 = (bytes_rs[2] & IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_3_BITMASK);
  ipass_mapping3 >>= IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_3_SHIFT;

  ipass_mapping4 = (bytes_rs[2] & IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_4_BITMASK);
  ipass_mapping4 >>= IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_4_SHIFT;

  slot_mapping1 = (bytes_rs[3] & IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_BITMASK);
  slot_mapping1 >>= IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_SHIFT;

  slot_mapping2 = (bytes_rs[3] & IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_2_BITMASK);
  slot_mapping2 >>= IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_2_SHIFT;

  switch (control_type)
    {
    case IPMI_OEM_DELL_PORT_MAP_CONTROL_TYPE_JUMPER:
      control_type_str = "Jumper";
      break;
    case IPMI_OEM_DELL_PORT_MAP_CONTROL_TYPE_BMC:
      control_type_str = "BMC";
      break;
    default:
      control_type_str = "Unknown";
    }

  pstdout_printf (state_data->pstate,
		  "Control Type: %s\n",
		  control_type_str);

  /* If 1:8 isn't supported, must be 1:2 or 1:4 */

  if (slot_mapping1 == IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_8_NOT_SUPPORTED)
    slot_mapping1 = IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_2_OR_1_4;

  if (slot_mapping2 == IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_8_NOT_SUPPORTED)
    slot_mapping2 = IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_2_OR_1_4;

  if ((slot_mapping1 != IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_2_OR_1_4
       && slot_mapping1 != IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_8)
      || (slot_mapping2 != IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_2_OR_1_4
	  && slot_mapping2 != IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_8))
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "Unrecognized slot mapping data\n");
      goto cleanup;
    }
  
  /* iPass 1 */
  if (slot_mapping1 == IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_8)
    pstdout_printf (state_data->pstate,
		    "iPass 1: PCIe1 PCIe2 PCIe3 PCIe4 PCIe13 PCIe14 PCIe15 PCIe16\n");
  else
    {
      if (ipass_mapping1 == IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_2)
	pstdout_printf (state_data->pstate,
			"iPass 1: PCIe1 PCIe15\n");
      else
	pstdout_printf (state_data->pstate,
			"iPass 1: PCIe1 PCIe2 PCIe15 PCIe16\n");
    }
  
  /* iPass 2 */
  if (slot_mapping1 == IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_8)
    pstdout_printf (state_data->pstate,
		    "iPASS 2:\n");
  else
    {
      if (ipass_mapping2 == IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_2)
	pstdout_printf (state_data->pstate,
			"iPass 2: PCIe3 PCIe13\n");
      else
	pstdout_printf (state_data->pstate,
			"iPass 2: PCIe3 PCIe4 PCIe13 PCIe14\n");
    }
  
  /* iPass 3 */
  if (slot_mapping2 == IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_8)
    pstdout_printf (state_data->pstate,
		    "iPass 3: PCIe5 PCIe6 PCIe7 PCIe8 PCIe9 PCIe10 PCIe11 PCIe12\n");
  else
    {
      if (ipass_mapping3 == IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_2)
	pstdout_printf (state_data->pstate,
			"iPass 3: PCIe5 PCIe11\n");
      else
	pstdout_printf (state_data->pstate,
			"iPass 3: PCIe5 PCIe6 PCIe11 PCIe12\n");
    }
  
  /* iPass 4 */
  if (slot_mapping2 == IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_8)
    pstdout_printf (state_data->pstate,
		    "iPass 4:\n");
  else
    {
      if (ipass_mapping4 == IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_2)
	pstdout_printf (state_data->pstate,
			"iPass 4: PCIe7 PCIe9\n");
      else
	pstdout_printf (state_data->pstate,
			"iPass 4: PCIe7 PCIe8 PCIe9 PCIe10\n");
    }
  
  /* iPass 5 */
  if (slot_mapping1 == IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_8)
    pstdout_printf (state_data->pstate,
		    "iPass 5:\n");
  else
    {
      if (ipass_mapping1 == IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_2)
	pstdout_printf (state_data->pstate,
			"iPass 5: PCIe2 PCIe16\n");
      else
	pstdout_printf (state_data->pstate,
			"iPass 5:\n");
    }
  
  /* iPass 6 */
  if (slot_mapping1 == IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_8)
    pstdout_printf (state_data->pstate,
		    "iPASS 6:\n");
  else
    {
      if (ipass_mapping2 == IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_2)
	pstdout_printf (state_data->pstate,
			"iPass 6: PCIe4 PCIe14\n");
      else
	pstdout_printf (state_data->pstate,
			"iPass 6:\n");
    }

  /* iPass 7 */
  if (slot_mapping2 == IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_8)
    pstdout_printf (state_data->pstate,
		    "iPass 7:\n");
  else
    {
      if (ipass_mapping3 == IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_2)
	pstdout_printf (state_data->pstate,
			"iPass 7: PCIe6 PCIe12\n");
      else
	pstdout_printf (state_data->pstate,
			"iPass 7:\n");
    }

  /* iPass 8 */
  if (slot_mapping2 == IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_8)
    pstdout_printf (state_data->pstate,
		    "iPass 8:\n");
  else
    {
      if (ipass_mapping4 == IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_2)
	pstdout_printf (state_data->pstate,
			"iPass 8: PCIe8 PCIe10\n");
      else
	pstdout_printf (state_data->pstate,
			"iPass 8:\n");
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_set_port_map (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  char *endptr = NULL;
  unsigned int ipass_mapping;
  uint8_t slot_mapping;
  uint8_t slot_mapping_subtype_is_1_2;
  uint8_t control_type;
  uint8_t ipass_mapping1;
  uint8_t ipass_mapping2;
  uint8_t ipass_mapping3;
  uint8_t ipass_mapping4;
  uint8_t slot_mapping1;
  uint8_t slot_mapping2;
  int rs_len;
  int rv = -1;
  
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 3);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "jumper")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "bmc"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }
  
  errno = 0;
  ipass_mapping = strtoul (state_data->prog_data->args->oem_options[1], &endptr, 10);
  if (errno
      || endptr[0] != '\0')
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "%s:%s invalid OEM option argument '%s'\n",
		       state_data->prog_data->args->oem_id,
		       state_data->prog_data->args->oem_command,
		       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }
  
  if (ipass_mapping < IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_MIN
      || ipass_mapping > IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_MAX)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "%s:%s invalid OEM option argument '%s' : out of range\n",
		       state_data->prog_data->args->oem_id,
		       state_data->prog_data->args->oem_command,
		       state_data->prog_data->args->oem_options[2]);
      goto cleanup;
    }
  
  if (strcasecmp (state_data->prog_data->args->oem_options[2], "1:2")
      && strcasecmp (state_data->prog_data->args->oem_options[2], "1:4")
      && strcasecmp (state_data->prog_data->args->oem_options[2], "1:8"))
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "%s:%s invalid OEM option argument '%s'\n",
		       state_data->prog_data->args->oem_id,
		       state_data->prog_data->args->oem_command,
		       state_data->prog_data->args->oem_options[2]);
      goto cleanup;
    }
  
  /* Dell Poweredge OEM
   *
   * From Dell Provided Docs
   *
   * Port Map Configure Request
   *
   * 0x34 - OEM network function
   * 0xC8 - OEM cmd
   * 0x?? - bit 7 - get/set
   *        - 0 - get
   *        - 1 - set
   *      - bit 6:4 - control type
   *        - 1 - jumper
   *        - 2 - bmc
   *      - bit 3 - failover ipass mapping 1
   *        - 0 - 1:4
   *        - 1 - 1:2
   *      - bit 2 - failover ipass mapping 2
   *        - 0 - 1:4
   *        - 1 - 1:2
   *      - bit 1 - failover ipass mapping 3
   *        - 0 - 1:4
   *        - 1 - 1:2
   *      - bit 0 - failover ipass mapping 4
   *        - 0 - 1:4
   *        - 1 - 1:2
   * 0x?? - optional byte - new port map setting
   *      - on get, must specify byte to get equivalent byte of response
   *      - bit 7:4 - slot mapping 1,2,3,4,13,14,15,16
   *      - bit 3:0 - slot mapping 5,6,7,8,9,10,11,12
   *        - for both
   *        - 1 - 1:2 or 1:4 mode (indicated in previous byte)
   *        - 2 - 1:8 mode
   *
   * Port Map Configure Response
   *
   * 0xC8 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - bit 7:4 - control type
   *        - 1 - jumper
   *        - 2 - bmc
   *      - bit 3 - failover ipass mapping 1
   *        - 0 - 1:4
   *        - 1 - 1:2
   *      - bit 2 - failover ipass mapping 2
   *        - 0 - 1:4
   *        - 1 - 1:2
   *      - bit 1 - failover ipass mapping 3
   *        - 0 - 1:4
   *        - 1 - 1:2
   *      - bit 0 - failover ipass mapping 4
   *        - 0 - 1:4
   *        - 1 - 1:2
   * 0x?? - optional byte
   *      - bit 7:4 - slot mapping 1,2,3,4,13,14,15,16
   *      - bit 3:0 - slot mapping 5,6,7,8,9,10,11,12
   *        - for both
   *        - 0 - 1:8 mode not supported
   *        - 1 - 1:2 or 1:4 mode (indicated in previous byte)
   *        - 2 - 1:8 mode
   */

  /* Get the current settings */

  bytes_rq[0] = IPMI_CMD_OEM_DELL_PORT_MAP;
  bytes_rq[1] = (IPMI_OEM_DELL_PORT_MAP_GET << IPMI_OEM_DELL_PORT_MAP_GET_SET_SHIFT);
  bytes_rq[2] = 0x00;		/* to force proper return of data */

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_PORT_MAP_RQ, /* network function */
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
                                                   4,
                                                   IPMI_CMD_OEM_DELL_PORT_MAP,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_PORT_MAP_RS,
                                                   NULL) < 0)
    goto cleanup;

  /* Figure out the old settings */

  ipass_mapping1 = (bytes_rs[2] & IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_BITMASK);
  ipass_mapping1 >>= IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_SHIFT;

  ipass_mapping2 = (bytes_rs[2] & IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_2_BITMASK);
  ipass_mapping2 >>= IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_2_SHIFT;

  ipass_mapping3 = (bytes_rs[2] & IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_3_BITMASK);
  ipass_mapping3 >>= IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_3_SHIFT;

  ipass_mapping4 = (bytes_rs[2] & IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_4_BITMASK);
  ipass_mapping4 >>= IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_4_SHIFT;

  slot_mapping1 = (bytes_rs[3] & IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_BITMASK);
  slot_mapping1 >>= IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_SHIFT;

  slot_mapping2 = (bytes_rs[3] & IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_2_BITMASK);
  slot_mapping2 >>= IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_2_SHIFT;

  /* Special case check */

  if (!strcasecmp (state_data->prog_data->args->oem_options[2], "1:2"))
    {
      slot_mapping = IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_2_OR_1_4;
      slot_mapping_subtype_is_1_2 = 1;
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[2], "1:4"))
    {
      slot_mapping = IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_2_OR_1_4;
      slot_mapping_subtype_is_1_2 = 0;
    }
  else
    {
      slot_mapping = IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_8;
      slot_mapping_subtype_is_1_2 = 0;
    }

  if (slot_mapping == IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_8
      && (slot_mapping1 == IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_8_NOT_SUPPORTED
	  || slot_mapping2 == IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_8_NOT_SUPPORTED))
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "Platform does not support 1:8 slot mapping\n");
      goto cleanup;
    }

  /* Build up the set request using the responses from the get
   * response and user input
   */

  bytes_rq[0] = IPMI_CMD_OEM_DELL_PORT_MAP;
  bytes_rq[1] = (IPMI_OEM_DELL_PORT_MAP_SET << IPMI_OEM_DELL_PORT_MAP_GET_SET_SHIFT);

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "jumper"))
    control_type = IPMI_OEM_DELL_PORT_MAP_CONTROL_TYPE_JUMPER;
  else
    control_type = IPMI_OEM_DELL_PORT_MAP_CONTROL_TYPE_BMC;

  bytes_rq[1] |= (control_type << IPMI_OEM_DELL_PORT_MAP_CONTROL_TYPE_REQUEST_SHIFT);
  
  if (ipass_mapping == 1
      && slot_mapping == IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_2_OR_1_4)
    {
      if (slot_mapping_subtype_is_1_2)
	ipass_mapping1 = IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_2;
      else
	ipass_mapping1 = IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_4;
    }

  if (ipass_mapping == 2
      && slot_mapping == IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_2_OR_1_4)
    {
      if (slot_mapping_subtype_is_1_2)
	ipass_mapping2 = IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_2;
      else
	ipass_mapping2 = IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_4;
    }

  if (ipass_mapping == 3
      && slot_mapping == IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_2_OR_1_4)
    {
      if (slot_mapping_subtype_is_1_2)
	ipass_mapping3 = IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_2;
      else
	ipass_mapping3 = IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_4;
    }

  if (ipass_mapping == 4
      && slot_mapping == IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_2_OR_1_4)
    {
      if (slot_mapping_subtype_is_1_2)
	ipass_mapping4 = IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_2;
      else
	ipass_mapping4 = IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_4;
    }

  bytes_rq[1] |= (ipass_mapping1 << IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_1_SHIFT);
  bytes_rq[1] |= (ipass_mapping2 << IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_2_SHIFT);
  bytes_rq[1] |= (ipass_mapping3 << IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_3_SHIFT);
  bytes_rq[1] |= (ipass_mapping4 << IPMI_OEM_DELL_PORT_MAP_IPASS_MAPPING_4_SHIFT);

  if (ipass_mapping == 1
      || ipass_mapping == 2)
    slot_mapping1 = slot_mapping;
  else
    slot_mapping2 = slot_mapping;

  bytes_rq[2] = (slot_mapping1 << IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_1_SHIFT);
  bytes_rq[2] |= (slot_mapping2 << IPMI_OEM_DELL_PORT_MAP_SLOT_MAPPING_2_SHIFT);

  /* Now set the new values */

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_DELL_GENERIC_PORT_MAP_RQ, /* network function */
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
                                                   2,
                                                   IPMI_CMD_OEM_DELL_PORT_MAP,
                                                   IPMI_NET_FN_OEM_DELL_GENERIC_PORT_MAP_RS,
                                                   NULL) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}
