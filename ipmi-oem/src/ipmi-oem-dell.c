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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
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

/* 256 b/c length is 8 bit field */
#define IPMI_OEM_DELL_MAX_BYTES 256

#define IPMI_OEM_DELL_SYSTEM_INFO_ASSET_TAG              0xC4
#define IPMI_OEM_DELL_SYSTEM_INFO_SERVICE_TAG            0xC5
#define IPMI_OEM_DELL_SYSTEM_INFO_PRODUCT_NAME           0xD1
#define IPMI_OEM_DELL_SYSTEM_INFO_10G_MAC_ADDRESSES      0xCB
#define IPMI_OEM_DELL_SYSTEM_INFO_11G_MAC_ADDRESSES      0xDA
#define IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_VALIDATOR        0xDD
#define IPMI_OEM_DELL_SYSTEM_INFO_POWER_CAPACITY         0xEA
#define IPMI_OEM_DELL_SYSTEM_INFO_AVERAGE_POWER_HISTORY  0xEB
#define IPMI_OEM_DELL_SYSTEM_INFO_PEAK_POWER_HISTORY     0xEC

#define IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_TYPE_10G            0x08
#define IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_TYPE_11G_MONOLITHIC 0x0A
#define IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_TYPE_11G_MODULAR    0x0B

#define IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_TYPE_ETHERNET  0
#define IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_TYPE_ISCSI     1
#define IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_TYPE_RESERVED  3

#define IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_STATUS_ENABLED      0
#define IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_STATUS_DISABLED     1
#define IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_STATUS_PLAYING_DEAD 2
#define IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_STATUS_RESERVED     3

#define IPMI_OEM_DELL_TOKEN_ID_SSH                 0x0A
#define IPMI_OEM_DELL_TOKEN_ID_TELNET              0x0B
#define IPMI_OEM_DELL_TOKEN_ID_WEB_SERVER          0x0C

#define IPMI_OEM_DELL_TOKEN_VERSION                0x01

#define IPMI_OEM_DELL_TOKEN_DATA_COMMON_HEADER_LEN 5

#define IPMI_OEM_DELL_MAC_ADDRESS_LENGTH 6

#define IPMI_OEM_DELL_11G_MAC_ADDRESS_LENGTH 8 

/* Will call ipmi_cmd_get_system_info_parameters only once, b/c field
 * requested is defined by OEM to be < 16 bytes in length
 */
static int
_get_dell_system_info_short_string (ipmi_oem_state_data_t *state_data,
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
                                           IPMI_SYSTEM_INFO_NO_BLOCK_SELECTOR,
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
                                           set_selector,
                                           IPMI_SYSTEM_INFO_NO_BLOCK_SELECTOR,
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

  if (len < 3)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_system_info_parameters: invalid buffer length returned: %d\n",
                       len);
      goto cleanup;
    }

  /* configuration_parameter_data[0] is the set selector, we don't care */

  if (configuration_parameter_data[1] != IPMI_SYSTEM_INFO_ENCODING_ASCII_LATIN1)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_system_info_parameters: invalid string type returned: %Xh\n",
                       configuration_parameter_data[0]);
      goto cleanup;
    }

  string_length = configuration_parameter_data[2];

  if (!string_length)
    goto out;

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
                                               IPMI_SYSTEM_INFO_NO_BLOCK_SELECTOR,
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

/* returns 1 on success, 0 on not supported, -1 on error */
static int
_get_dell_system_info_idrac_info (ipmi_oem_state_data_t *state_data,
                                  uint8_t *idrac_type)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  int len;
  int rv = -1;

  assert (state_data);
  assert (idrac_type);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Source Code
   *
   * Uses Get System Info command
   *
   * iDRAC Validator Parameter = 0xDD
   * iDRAC Validator Set Selector = 0x02
   *
   * Parameter data response formatted:
   *
   * 1st byte = ??
   * 2nd byte = ??
   * 3rd byte = ??
   * 4th byte = ??
   * 5th byte = ??
   * 6th byte = ??
   * 7th byte = ??
   * 8th byte = ??
   * 9th byte = ??
   * 10th byte = iDRAC type
   * - 0x08 = iDRAC 10g
   * - 0x0A = iDRAC 11g monolithic
   * - 0x0B = iDRAC 11g modular
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
                                           IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_VALIDATOR,
                                           0x02,
                                           IPMI_SYSTEM_INFO_NO_BLOCK_SELECTOR,
                                           obj_cmd_rs) < 0)
    {
      if (ipmi_ctx_errnum (state_data->ipmi_ctx) == IPMI_ERR_BAD_COMPLETION_CODE
          && (ipmi_check_completion_code (obj_cmd_rs,
                                          IPMI_COMP_CODE_GET_SYSTEM_INFO_PARAMETER_NOT_SUPPORTED) == 1))
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

  if (len < 1)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_system_info_parameters: invalid buffer length returned: %d\n",
                       len);
      goto cleanup;
    }

  (*idrac_type) = configuration_parameter_data[9];

  rv = 1;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

static int
_get_dell_system_info_10g_mac_addresses (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t number_of_nics;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  int len;
  int i;
  int rv = -1;

  assert (state_data);

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
                                           IPMI_OEM_DELL_SYSTEM_INFO_10G_MAC_ADDRESSES,
                                           0,
                                           IPMI_SYSTEM_INFO_NO_BLOCK_SELECTOR,
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

  number_of_nics = configuration_parameter_data[0];

  if (!number_of_nics)
    {
      rv = 0;
      goto cleanup;
    }

  if ((number_of_nics * IPMI_OEM_DELL_MAC_ADDRESS_LENGTH) != (len - 1))
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
_get_dell_system_info_11g_mac_addresses (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint8_t total_bytes;
  int rs_len;
  int i;
  int rv = -1;

  assert (state_data);

  /* see info below in ipmi_oem_dell_get_system_info() for packet
   * format.  We cannot use normal Get System Info b/c Dell hacked it
   * to include/support extra bytes. 
   */

  bytes_rq[0] = IPMI_CMD_GET_SYSTEM_INFO_PARAMETERS;
  bytes_rq[1] = 0x00;		/* get parameter */
  bytes_rq[2] = IPMI_OEM_DELL_SYSTEM_INFO_11G_MAC_ADDRESSES; /* parameter selector */
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
                                                   IPMI_NET_FN_APP_RQ) < 0)
    goto cleanup;
  
  total_bytes = bytes_rs[3];

  if (!total_bytes)
    {
      rv = 0;
      goto cleanup;
    }

  if (total_bytes % 8)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "invalid total bytes of data returned: %u\n",
		       total_bytes);
      goto cleanup;
    }

  /* see record format below in ipmi_oem_dell_get_system_info(), record length = 8 */
  pstdout_printf (state_data->pstate,
		  "NIC Number\tMAC Address\t\tNIC Status\n");
  for (i = 0; i < (total_bytes / IPMI_OEM_DELL_11G_MAC_ADDRESS_LENGTH); i++)
    {
      uint8_t mac_type;
      
      bytes_rq[0] = IPMI_CMD_GET_SYSTEM_INFO_PARAMETERS;
      bytes_rq[1] = 0x00;		/* get parameter */
      bytes_rq[2] = IPMI_OEM_DELL_SYSTEM_INFO_11G_MAC_ADDRESSES; /* parameter selector */
      bytes_rq[3] = 0x00;		/* set selector */
      bytes_rq[4] = 0x00;		/* block selector */
      bytes_rq[5] = i * IPMI_OEM_DELL_11G_MAC_ADDRESS_LENGTH; /* offset */
      bytes_rq[6] = IPMI_OEM_DELL_11G_MAC_ADDRESS_LENGTH; /* length */
      
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
      
      /* 11 = IPMI_OEM_DELL_11G_MAC_ADDRESS_LENGTH + 3 (for cmd, completion code, parameter revision) */
      if (ipmi_oem_check_response_and_completion_code (state_data,
						       bytes_rs,
						       rs_len,
						       11,
						       IPMI_CMD_GET_SYSTEM_INFO_PARAMETERS,
						       IPMI_NET_FN_APP_RQ) < 0)
	goto cleanup;
      
      mac_type = (bytes_rs[3] & 0x30) >> 4;
      
      if (mac_type == IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_TYPE_ETHERNET)
	{
	  uint8_t nic_number;
	  uint8_t nic_status;
	  char *nic_status_str = NULL;

	  nic_status = (bytes_rs[3] & 0xC0) >> 6;
	  nic_number = (bytes_rs[4] & 0x1F);

	  if (nic_status == IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_STATUS_ENABLED)
	    nic_status_str = "Enabled";
	  else if (nic_status == IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_STATUS_DISABLED)
	    nic_status_str = "Disabled";
	  else if (nic_status == IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_STATUS_PLAYING_DEAD)
	    nic_status_str = "Playing Dead";
	  else
	    nic_status_str = "Unknown";

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
  char string[IPMI_OEM_DELL_MAX_BYTES+1];
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  /* achu: handle some common typo situations */
  if (strcasecmp (state_data->prog_data->args->oem_options[0], "asset-tag")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "asset_tag")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "assettag")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "service-tag")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "service_tag")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "servicetag")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "product-name")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "product_name")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "productname")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "mac-addresses")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "mac_addresses")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "macaddresses"))
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
   *
   * Uses Get System Info command
   *
   * For asset-tag and service-tag, the response format is different
   * than product name.
   *
   * Format #1)
   *
   * asset-tag parameter = 0xC4
   * service-tag parameter = 0xC5
   *
   * Parameter data response formatted:
   *
   * 1st byte = length
   * ? bytes = string
   *
   * Format #2)
   *
   * product-name parameter = 0xD1
   *
   * Parameter data response formatted:
   *
   * Set Selector 0:
   *
   * 1st byte = set selector
   * 2nd byte = encoding
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
   * Dell 10G systems, mac-addresses = 0xCB
   *
   * Parameter data response formatted:
   *
   * 1st byte = number of NICs
   * ? bytes = MAC address of NICS, number of NICS * 6 total bytes
   *
   * Format #4)
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
   */

  memset (string, '\0', IPMI_OEM_DELL_MAX_BYTES + 1);

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "asset-tag")
      || !strcasecmp (state_data->prog_data->args->oem_options[0], "asset_tag")
      || !strcasecmp (state_data->prog_data->args->oem_options[0], "assettag"))
    {
      if (_get_dell_system_info_short_string (state_data,
                                              IPMI_OEM_DELL_SYSTEM_INFO_ASSET_TAG,
                                              string,
                                              IPMI_OEM_DELL_MAX_BYTES) < 0)
        goto cleanup;

      pstdout_printf (state_data->pstate,
		      "%s\n",
		      string);
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "service-tag")
           || !strcasecmp (state_data->prog_data->args->oem_options[0], "service_tag")
           || !strcasecmp (state_data->prog_data->args->oem_options[0], "servicetag"))
    {
      if (_get_dell_system_info_short_string (state_data,
                                              IPMI_OEM_DELL_SYSTEM_INFO_SERVICE_TAG,
                                              string,
                                              IPMI_OEM_DELL_MAX_BYTES) < 0)
        goto cleanup;

      pstdout_printf (state_data->pstate,
		      "%s\n",
		      string);
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "product-name")
           || !strcasecmp (state_data->prog_data->args->oem_options[0], "product_name")
           || !strcasecmp (state_data->prog_data->args->oem_options[0], "productname"))
    {
      if (_get_dell_system_info_long_string (state_data,
                                             IPMI_OEM_DELL_SYSTEM_INFO_PRODUCT_NAME,
                                             string,
                                             IPMI_OEM_DELL_MAX_BYTES) < 0)
        goto cleanup;

      pstdout_printf (state_data->pstate,
		      "%s\n",
		      string);

    }
  else /* (!strcasecmp (state_data->prog_data->args->oem_options[0], "mac-addresses")
          || !strcasecmp (state_data->prog_data->args->oem_options[0], "mac_addresses")
          || !strcasecmp (state_data->prog_data->args->oem_options[0], "macaddresses")) */
    {
      uint8_t idrac_type = 0;
      int ret;
      
      if ((ret = _get_dell_system_info_idrac_info (state_data, &idrac_type)) < 0)
	goto cleanup;

      if (ret)
	{
	  /* iDRAC 10g */
	  if (idrac_type == IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_TYPE_10G)
	    {
	      if (_get_dell_system_info_10g_mac_addresses (state_data) < 0)
		goto cleanup;
	    }
	  /* iDRAC 11g */
	  else if (idrac_type == IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_TYPE_11G_MONOLITHIC
		   || idrac_type == IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_TYPE_11G_MODULAR)
	    {
	      if (_get_dell_system_info_11g_mac_addresses (state_data) < 0)
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
	  if (_get_dell_system_info_10g_mac_addresses (state_data) < 0)
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

  bytes_rq[0] = 0x25;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              0x30, /* network function */
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
                                                   0x02,
                                                   0x30) < 0)
    goto cleanup;

  switch (bytes_rs[2])
    {
    case 0x00:
      pstdout_printf (state_data->pstate, "shared\n");
      break;
    case 0x01:
      pstdout_printf (state_data->pstate, "shared with failover to NIC2\n");
      break;
    case 0x02:
      pstdout_printf (state_data->pstate, "dedicated\n");
      break;
    case 0x03:
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

  bytes_rq[0] = 0x24;

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "shared"))
    bytes_rq[1] = 0x00;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "shared_failover_nic2"))
    bytes_rq[1] = 0x01;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "dedicated"))
    bytes_rq[1] = 0x02;
  else
    bytes_rq[1] = 0x03;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              0x30, /* network function */
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
                                                   0x03,
                                                   0x30) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
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

  bytes_rq[0] = 0x01;
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
                                                   0x01,
                                                   IPMI_NET_FN_OEM_GROUP_RQ) < 0)
    goto cleanup;
  
  (*reservation_id) = bytes_rs[5];

  rv = 0;
 cleanup:
  return (rv);
}

static int
_dell_get_extended_configuration (ipmi_oem_state_data_t *state_data,
				  uint8_t token_id,
				  uint8_t *token_data,
				  unsigned int token_data_len,
				  unsigned int expected_token_data_len)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint16_t token_len; 
  uint8_t token_version; 
  uint8_t reservation_id = 0;
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (token_data);
  assert (token_data_len);
  assert (expected_token_data_len);
  assert (token_data_len >= expected_token_data_len);

  /* Dell Poweredge OEM
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
   * 0x?? - byts to read (1 based, 0xFF = all)
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

  if (_dell_reserve_extended_configuration (state_data,
					    &reservation_id) < 0)
    goto cleanup;

  bytes_rq[0] = 0x02;
  bytes_rq[1] = (IPMI_IANA_ENTERPRISE_ID_DELL & 0x0000FF);
  bytes_rq[2] = (IPMI_IANA_ENTERPRISE_ID_DELL & 0x00FF00) >> 8;
  bytes_rq[3] = (IPMI_IANA_ENTERPRISE_ID_DELL & 0xFF0000) >> 16;
  bytes_rq[4] = reservation_id;
  bytes_rq[5] = token_id;
  bytes_rq[6] = 0x00;
  bytes_rq[7] = 0x00;
  bytes_rq[8] = 0x00;
  bytes_rq[9] = 0xFF;

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
  
  /* atleast 8 bytes of non-token-data */
  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   8 + expected_token_data_len,
                                                   0x02,
                                                   IPMI_NET_FN_OEM_GROUP_RQ) < 0)
    goto cleanup;

  if (bytes_rs[7] != expected_token_data_len)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "invalid data length returned: expected = %u, returned %u\n",
		       expected_token_data_len, bytes_rs[7]);
      goto cleanup;
    }

  /* check token common header */

  token_len = bytes_rs[8];
  token_len |= (bytes_rs[9] << 8);

  if (token_len != expected_token_data_len)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "invalid token data length returned: expected = %u, returned %u\n",
		       expected_token_data_len, token_len);
      goto cleanup;
    }
  
  token_version = bytes_rs[10];

  if (token_version != IPMI_OEM_DELL_TOKEN_VERSION)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "invalid token version returned: expected = %Xh, returned %Xh\n",
		       IPMI_OEM_DELL_TOKEN_VERSION, token_version);
      goto cleanup;
    }

  /* length checked above */
  memcpy (token_data, &bytes_rs[8], expected_token_data_len);
  
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
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (token_data);
  assert (token_data_len);
  assert (valid_field_mask);

  /* Dell Poweredge OEM
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

  if (_dell_reserve_extended_configuration (state_data,
                                            &reservation_id) < 0)
    goto cleanup;

  bytes_rq[0] = 0x03;
  bytes_rq[1] = (IPMI_IANA_ENTERPRISE_ID_DELL & 0x0000FF);
  bytes_rq[2] = (IPMI_IANA_ENTERPRISE_ID_DELL & 0x00FF00) >> 8;
  bytes_rq[3] = (IPMI_IANA_ENTERPRISE_ID_DELL & 0xFF0000) >> 16;
  bytes_rq[4] = reservation_id;
  bytes_rq[5] = token_id;
  bytes_rq[6] = 0x00;
  bytes_rq[7] = 0x00;
  bytes_rq[8] = 0x00;
  bytes_rq[9] = 0x01;

  /* common header */
  token_len = IPMI_OEM_DELL_TOKEN_DATA_COMMON_HEADER_LEN + token_data_len;
  bytes_rq[10] = (token_len & 0x00FF);
  bytes_rq[11] = (token_len & 0xFF00) >> 8;
  bytes_rq[12] = IPMI_OEM_DELL_TOKEN_VERSION;
  bytes_rq[13] = (valid_field_mask & 0x00FF); 
  bytes_rq[14] = (valid_field_mask & 0xFF00) >> 8; 

  memcpy (&bytes_rq[15], token_data, token_data_len);

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
                              bytes_rq, /* data */
                              15 + token_data_len, /* num bytes */
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
                                                   0x03,
                                                   IPMI_NET_FN_OEM_GROUP_RQ) < 0)
    goto cleanup;

  if (bytes_rs[5] != token_len)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "invalid data length written: expected = %u, returned %u\n",
                       token_len, bytes_rs[5]);
      goto cleanup;
    }
 
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_ssh_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t token_data[IPMI_OEM_MAX_BYTES];
  uint16_t valid_field_mask;
  uint16_t expected_field_mask = 0x1F; /* 5 fields */
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
   * byte 7 - MaxConnections
   * byte 8 - ActiveConnections
   * byte 9 - 12 - IdleTimeout
   * byte 13 - 14 - PortNumber
   */

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_dell_get_extended_configuration (state_data,
					IPMI_OEM_DELL_TOKEN_ID_SSH,
					token_data,
					IPMI_OEM_MAX_BYTES,
					IPMI_OEM_DELL_TOKEN_DATA_COMMON_HEADER_LEN + 9) < 0)
    goto cleanup;

  valid_field_mask = token_data[3];
  valid_field_mask |= (token_data[4] << 8);
 
  if (valid_field_mask != expected_field_mask)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "invalid field mask returned: expected = %Xh, returned %Xh\n",
		       expected_field_mask, valid_field_mask);
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

  /* XXX need units */
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
  uint8_t token_data[IPMI_OEM_MAX_BYTES];
  uint16_t valid_field_mask = 0x1F; /* 5 fields */
  unsigned int temp;
  uint8_t sshenable;
  uint8_t maxconnections;
  uint8_t activeconnections;
  uint32_t idletimeout;
  uint16_t portnumber;
  char *ptr = NULL;
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
   * byte 7 - MaxConnections
   * byte 8 - ActiveConnections
   * byte 9 - 12 - IdleTimeout
   * byte 13 - 14 - PortNumber
   */

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 5);

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
    sshenable = 1;
  else
    sshenable = 0;

  errno = 0;
  temp = strtoul (state_data->prog_data->args->oem_options[1], &ptr, 10);
  if (errno
      || ptr[0] != '\0'
      || temp > 255)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }
  maxconnections = temp;

  errno = 0;
  temp = strtoul (state_data->prog_data->args->oem_options[2], &ptr, 10);
  if (errno
      || ptr[0] != '\0'
      || temp > 255)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[2]);
      goto cleanup;
    }
  activeconnections = temp;

  errno = 0;
  temp = strtoul (state_data->prog_data->args->oem_options[3], &ptr, 10);
  if (errno
      || ptr[0] != '\0')
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[3]);
      goto cleanup;
    }
  idletimeout = temp;

  errno = 0;
  temp = strtoul (state_data->prog_data->args->oem_options[4], &ptr, 10);
  if (errno
      || ptr[0] != '\0'
      || temp > 65535)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[4]);
      goto cleanup;
    }
  portnumber = temp;
  
  token_data[0] = sshenable;
  token_data[1] = maxconnections;
  token_data[2] = activeconnections;
  token_data[3] = (idletimeout & 0x000000FF);
  token_data[4] = (idletimeout & 0x0000FF00) >> 8;
  token_data[5] = (idletimeout & 0x00FF0000) >> 16;
  token_data[6] = (idletimeout & 0xFF000000) >> 24;
  token_data[7] = (portnumber & 0x00FF);
  token_data[8] = (portnumber & 0xFF00) >> 8;

  if (_dell_set_extended_configuration (state_data,
                                        IPMI_OEM_DELL_TOKEN_ID_SSH,
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
  uint8_t token_data[IPMI_OEM_MAX_BYTES];
  uint16_t valid_field_mask;
  uint16_t expected_field_mask = 0x3F; /* 6 fields */
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
   * byte 7 - maxsessions;
   * byte 8 - activesessions;
   * byte 9 - 12 - sessiontimeout
   * byte 13 - 14 - portnumber
   * byte 15 - telnet7flsbackspace
   */

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_dell_get_extended_configuration (state_data,
					IPMI_OEM_DELL_TOKEN_ID_TELNET,
					token_data,
					IPMI_OEM_MAX_BYTES,
					IPMI_OEM_DELL_TOKEN_DATA_COMMON_HEADER_LEN + 10) < 0)
    goto cleanup;

  valid_field_mask = token_data[3];
  valid_field_mask |= (token_data[4] << 8);
 
  if (valid_field_mask != expected_field_mask)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "invalid field mask returned: expected = %Xh, returned %Xh\n",
		       expected_field_mask, valid_field_mask);
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

  /* XXX need units */
  pstdout_printf (state_data->pstate,
		  "Session Timeout    : %u seconds\n",
		  sessiontimeout);

  pstdout_printf (state_data->pstate,
		  "Port Number        : %u\n",
		  portnumber);

  /* XXX what is this */
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
  uint8_t token_data[IPMI_OEM_MAX_BYTES];
  uint16_t valid_field_mask = 0x3F; /* 6 fields */
  unsigned int temp;
  uint8_t telnetenable;
  uint8_t maxsessions;
  uint8_t activesessions;
  uint32_t sessiontimeout;
  uint16_t portnumber;
  uint8_t _7flsenable;
  char *ptr = NULL;
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
   * byte 7 - maxsessions;
   * byte 8 - activesessions;
   * byte 9 - 12 - sessiontimeout
   * byte 13 - 14 - portnumber
   * byte 15 - telnet7flsbackspace
   */

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 6);

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
    telnetenable = 1;
  else
    telnetenable = 0;

  errno = 0;
  temp = strtoul (state_data->prog_data->args->oem_options[1], &ptr, 10);
  if (errno
      || ptr[0] != '\0'
      || temp > 255)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }
  maxsessions = temp;

  errno = 0;
  temp = strtoul (state_data->prog_data->args->oem_options[2], &ptr, 10);
  if (errno
      || ptr[0] != '\0'
      || temp > 255)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[2]);
      goto cleanup;
    }
  activesessions = temp;

  errno = 0;
  temp = strtoul (state_data->prog_data->args->oem_options[3], &ptr, 10);
  if (errno
      || ptr[0] != '\0')
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[3]);
      goto cleanup;
    }
  sessiontimeout = temp;

  errno = 0;
  temp = strtoul (state_data->prog_data->args->oem_options[4], &ptr, 10);
  if (errno
      || ptr[0] != '\0'
      || temp > 65535)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[4]);
      goto cleanup;
    }
  portnumber = temp;
  
  if (strcasecmp (state_data->prog_data->args->oem_options[5], "7flsenable")
      && strcasecmp (state_data->prog_data->args->oem_options[5], "7flsdisable"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[5]);
      goto cleanup;
    }

  if (!strcasecmp (state_data->prog_data->args->oem_options[5], "7flsenable"))
    _7flsenable = 1;
  else
    _7flsenable = 0;

  token_data[0] = telnetenable;
  token_data[1] = maxsessions;
  token_data[2] = activesessions;
  token_data[3] = (sessiontimeout & 0x000000FF);
  token_data[4] = (sessiontimeout & 0x0000FF00) >> 8;
  token_data[5] = (sessiontimeout & 0x00FF0000) >> 16;
  token_data[6] = (sessiontimeout & 0xFF000000) >> 24;
  token_data[7] = (portnumber & 0x00FF);
  token_data[8] = (portnumber & 0xFF00) >> 8;
  token_data[9] = _7flsenable;

  if (_dell_set_extended_configuration (state_data,
                                        IPMI_OEM_DELL_TOKEN_ID_TELNET,
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
  uint8_t token_data[IPMI_OEM_MAX_BYTES];
  uint16_t valid_field_mask;
  uint16_t expected_field_mask = 0x3F; /* 6 fields */
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
   * byte 7 - maxsessions;
   * byte 8 - activesessions;
   * byte 9 - 12 - sessiontimeout
   * byte 13 - 14 - httpportnumber
   * byte 15 - 16 - httpsportnumber
   */

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_dell_get_extended_configuration (state_data,
					IPMI_OEM_DELL_TOKEN_ID_WEB_SERVER,
					token_data,
					IPMI_OEM_MAX_BYTES,
					IPMI_OEM_DELL_TOKEN_DATA_COMMON_HEADER_LEN + 11) < 0)
    goto cleanup;

  valid_field_mask = token_data[3];
  valid_field_mask |= (token_data[4] << 8);
 
  if (valid_field_mask != expected_field_mask)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "invalid field mask returned: expected = %Xh, returned %Xh\n",
		       expected_field_mask, valid_field_mask);
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

  /* XXX need units */
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
  uint8_t token_data[IPMI_OEM_MAX_BYTES];
  uint16_t valid_field_mask = 0x3F; /* 6 fields */
  unsigned int temp;
  uint8_t webserverenable;
  uint8_t maxsessions;
  uint8_t activesessions;
  uint32_t sessiontimeout;
  uint16_t httpportnumber;
  uint16_t httpsportnumber;
  char *ptr = NULL;
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
   * byte 7 - maxsessions;
   * byte 8 - activesessions;
   * byte 9 - 12 - sessiontimeout
   * byte 13 - 14 - httpportnumber
   * byte 15 - 16 - httpsportnumber
   */

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 6);

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
    webserverenable = 1;
  else
    webserverenable = 0;

  errno = 0;
  temp = strtoul (state_data->prog_data->args->oem_options[1], &ptr, 10);
  if (errno
      || ptr[0] != '\0'
      || temp > 255)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }
  maxsessions = temp;

  errno = 0;
  temp = strtoul (state_data->prog_data->args->oem_options[2], &ptr, 10);
  if (errno
      || ptr[0] != '\0'
      || temp > 255)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[2]);
      goto cleanup;
    }
  activesessions = temp;

  errno = 0;
  temp = strtoul (state_data->prog_data->args->oem_options[3], &ptr, 10);
  if (errno
      || ptr[0] != '\0')
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[3]);
      goto cleanup;
    }
  sessiontimeout = temp;

  errno = 0;
  temp = strtoul (state_data->prog_data->args->oem_options[4], &ptr, 10);
  if (errno
      || ptr[0] != '\0'
      || temp > 65535)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[4]);
      goto cleanup;
    }
  httpportnumber = temp;

  errno = 0;
  temp = strtoul (state_data->prog_data->args->oem_options[5], &ptr, 10);
  if (errno
      || ptr[0] != '\0'
      || temp > 65535)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[5]);
      goto cleanup;
    }
  httpsportnumber = temp;

  token_data[0] = webserverenable;
  token_data[1] = maxsessions;
  token_data[2] = activesessions;
  token_data[3] = (sessiontimeout & 0x000000FF);
  token_data[4] = (sessiontimeout & 0x0000FF00) >> 8;
  token_data[5] = (sessiontimeout & 0x00FF0000) >> 16;
  token_data[6] = (sessiontimeout & 0xFF000000) >> 24;
  token_data[7] = (httpportnumber & 0x00FF);
  token_data[8] = (httpportnumber & 0xFF00) >> 8;
  token_data[9] = (httpsportnumber & 0x00FF);
  token_data[10] = (httpsportnumber & 0xFF00) >> 8;

  if (_dell_set_extended_configuration (state_data,
                                        IPMI_OEM_DELL_TOKEN_ID_WEB_SERVER,
                                        token_data,
                                        11,
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

  bytes_rq[0] = 0x21;
  bytes_rq[1] = 0xaa;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              0x30, /* network function */
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
                                                   0x21,
                                                   0x30) < 0)
    goto cleanup;


  /* don't quit until it is done */
  while (1)
    {
      bytes_rq[0] = 0x21;
      bytes_rq[1] = 0x00;
      
      if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
				  0, /* lun */
				  0x30, /* network function */
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
						       0x21,
						       0x30) < 0)
	goto cleanup;

      if (bytes_rs[2] == 0x01)
	break;

      sleep (1);
    }
  
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_power_info (ipmi_oem_state_data_t *state_data)
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

  /* Dell Poweredge OEM
   *
   * From http://linux.dell.com/files/openipmi/ipmitool/
   *
   * Request
   *
   * 0x30 - OEM network function
   * 0x9c - OEM cmd
   * 0x07 - ??
   * 0x01 - ??
   * 
   * Response
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
                  peak_amp_reading_val);

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
ipmi_oem_dell_reset_power_info (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

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
   *
   * Request
   *
   * 0x30 - OEM network function
   * 0x9d - OEM cmd
   * 0x07 - ??
   * 0x01 - ??
   * 0x?? - field to clear (0x1 = cumulative, 0x2 = peak)
   * 
   * Response
   *
   * 0x9d - OEM cmd
   * 0x?? - Completion Code
   */

  bytes_rq[0] = 0x9d;
  bytes_rq[1] = 0x07;
  bytes_rq[2] = 0x01;

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "cumulative"))
    bytes_rq[3] = 1;
  else
    bytes_rq[3] = 2;
  
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              0x30, /* network function */
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
                                                   0x9d,
                                                   0x30) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
 
}

int
ipmi_oem_dell_get_power_supply_info (ipmi_oem_state_data_t *state_data)
{
  struct sensor_entity_id_counts entity_id_counts;
  uint16_t record_count;
  int i;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (sdr_cache_create_and_load (state_data->sdr_cache_ctx,
                                 state_data->pstate,
                                 state_data->ipmi_ctx,
                                 state_data->prog_data->args->sdr.quiet_cache,
                                 state_data->prog_data->args->sdr.sdr_cache_recreate,
                                 state_data->hostname,
                                 state_data->prog_data->args->sdr.sdr_cache_directory) < 0)
    goto cleanup;

  if (calculate_entity_id_counts (state_data->pstate,
				  state_data->sdr_cache_ctx,
				  state_data->sdr_parse_ctx,
				  &entity_id_counts) < 0)
    goto cleanup;

  if (ipmi_sdr_cache_record_count (state_data->sdr_cache_ctx, &record_count) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_cache_record_count: %s\n",
		       ipmi_sdr_cache_ctx_errormsg (state_data->sdr_cache_ctx));
      goto cleanup;
    }

  /* Dell Poweredge OEM
   *
   * Get Power Supply Info Request
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
   * bytes 4-5 - rated amps
   * bytes 6-7 - rated volts
   * bytes 8-11 - vendor ID
   * bytes 12-19 - firmware version (string, non-null terminated)
   * bytes 20 - power supply type
   * - 0x00 - AC
   * - 0x01 - DC
   * bytes 21-22 - rated dc watts
   * bytes 23-24 - reserved
   */

  for (i = 0; i < record_count; i++, ipmi_sdr_cache_next (state_data->sdr_cache_ctx))
    {
      uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
      int sdr_record_len = 0;
      uint16_t record_id;
      uint8_t record_type;

      if ((sdr_record_len = ipmi_sdr_cache_record_read (state_data->sdr_cache_ctx,
							sdr_record,
							IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH)) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_sdr_cache_record_read: %s\n",
			   ipmi_sdr_cache_ctx_errormsg (state_data->sdr_cache_ctx));
	  goto cleanup;
	}
      
      if (ipmi_sdr_parse_record_id_and_type (state_data->sdr_parse_ctx,
					     sdr_record,
					     sdr_record_len,
					     &record_id,
					     &record_type) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_sdr_parse_record_id_and_type: %s\n",
			   ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
	  goto cleanup;
	}
      
      if (record_type == IPMI_SDR_FORMAT_FULL_SENSOR_RECORD
	  || record_type == IPMI_SDR_FORMAT_COMPACT_SENSOR_RECORD)
	{
	  uint8_t entity_id;
	  uint8_t entity_instance;
	  uint8_t entity_instance_type;
	  uint8_t sensor_type;
	  
	  if (ipmi_sdr_parse_entity_id_instance_type (state_data->sdr_parse_ctx,
						      sdr_record,
						      sdr_record_len,
						      &entity_id,
						      &entity_instance,
						      &entity_instance_type) < 0)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "ipmi_sdr_parse_entity_id_instance_type: %s\n",
			       ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
	      goto cleanup;
	    }
	  
	  if (ipmi_sdr_parse_sensor_type (state_data->sdr_parse_ctx,
					  sdr_record,
					  sdr_record_len,
					  &sensor_type) < 0)
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "ipmi_sdr_parse_sensor_type: %s\n",
			       ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
	      goto cleanup;
	    }
	  
	  if (entity_id == IPMI_ENTITY_ID_POWER_SUPPLY
	      && entity_instance_type == IPMI_SDR_PHYSICAL_ENTITY
	      && sensor_type == IPMI_SENSOR_TYPE_POWER_SUPPLY)
	    {
	      uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
	      uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
	      int32_t rs_len;
	      uint16_t ratedwatts;
	      uint16_t ratedamps;
	      uint16_t ratedvolts;
	      uint32_t vendorid;
	      char firmwareversion[IPMI_OEM_MAX_BYTES];
	      uint8_t powersupplytype;
	      uint16_t rateddcwatts;
	      char sensor_name_buf[MAX_ENTITY_ID_SENSOR_NAME_STRING + 1];
	      
	      memset (firmwareversion, '\0', IPMI_OEM_MAX_BYTES);

	      if (get_entity_sensor_name_string (state_data->pstate,
						 state_data->sdr_parse_ctx,
						 sdr_record,
						 sdr_record_len,
						 &entity_id_counts,
						 sensor_name_buf,
						 MAX_ENTITY_ID_SENSOR_NAME_STRING) < 0)
		goto cleanup;
	      
	      bytes_rq[0] = 0xB0;
	      bytes_rq[1] = entity_id;
	      bytes_rq[2] = entity_instance;
	      
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
							       25,
							       0xB0,
							       0x30) < 0)
		goto cleanup;
	      
	      ratedwatts = bytes_rs[2];
	      ratedwatts |= (bytes_rs[3] << 8);
	      
	      ratedamps = bytes_rs[4];
	      ratedamps |= (bytes_rs[5] << 8);
	      
	      ratedvolts = bytes_rs[6];
	      ratedvolts |= (bytes_rs[7] << 8);
	      
	      vendorid = bytes_rs[8];
	      vendorid |= (bytes_rs[9] << 8);
	      vendorid |= (bytes_rs[10] << 16);
	      vendorid |= (bytes_rs[11] << 24);
	      
	      memcpy(firmwareversion, &(bytes_rs[12]), 8);
	      
	      powersupplytype = bytes_rs[20];
	      
	      rateddcwatts = bytes_rs[21];
	      rateddcwatts |= (bytes_rs[22] << 8);
	      
	      pstdout_printf (state_data->pstate,
			      "Power Supply        : %s\n",
			      sensor_name_buf);

	      pstdout_printf (state_data->pstate,
			      "Rated Input Wattage : %u W\n",
			      ratedwatts);
	      
	      pstdout_printf (state_data->pstate,
			      "Rated Ouput Wattage : %u W\n",
			      rateddcwatts);
	      
	      /* XXX need info */
#if 0
	      pstdout_printf (state_data->pstate,
			      "Rated Amps          : %u A\n",
			      ratedamps);
	      
	      pstdout_printf (state_data->pstate,
			      "Rated Volts         : %u V\n",
			      ratedvolts);
#endif
	      
	      pstdout_printf (state_data->pstate,
			      "Power Supply Type   : %s\n",
			      (powersupplytype == 0x01) ? "DC" : "AC");
	      
	      pstdout_printf (state_data->pstate,
			      "Firmare Version     : %s\n",
			      firmwareversion);
	      
	      /* XXX need info */
#if 0
	      pstdout_printf (state_data->pstate,
			      "Vendor              : %u\n",
			      vendorid);
#endif

	      pstdout_printf (state_data->pstate,
			      "\n");
	    }
	}
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_instantaneous_power_consumption_info (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint16_t instantaneous_power_consumption;
  uint16_t instantaneous_amps;
  double instantaneous_amps_val;
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Source Code
   *
   * Request
   *
   * 0x30 - OEM network function
   * 0xB3 - OEM cmd
   * 0x0A - ??
   * 0x00 - ??
   * 
   * Response
   *
   * 0xB3 - OEM cmd
   * 0x?? - Completion Code
   * bytes 2-3 - instantaneous power consumption
   * bytes 4-5 - instantaneous amps
   * bytes 6-8 - reserved
   */

  bytes_rq[0] = 0xB3;
  bytes_rq[1] = 0x0A;
  bytes_rq[2] = 0x00;

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
                                                   9,
                                                   0xB3,
                                                   0x30) < 0)
    goto cleanup;

  instantaneous_power_consumption = bytes_rs[2];
  instantaneous_power_consumption |= (bytes_rs[3] << 8);

  instantaneous_amps = bytes_rs[4];
  instantaneous_amps |= (bytes_rs[5] << 8);

  instantaneous_amps_val = ((double)instantaneous_amps) / 10.0;

  pstdout_printf (state_data->pstate,
		  "Instantaneous Power Consumption : %u W\n",
		  instantaneous_power_consumption);

  pstdout_printf (state_data->pstate,
		  "Instantaneous Amperage          : %.2f A\n",
		  instantaneous_amps_val);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_power_headroom_info (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  uint16_t instantaneous_power_headroom;
  uint16_t peak_power_headroom;
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Source Code
   *
   * Request
   *
   * 0x30 - OEM network function
   * 0xBB - OEM cmd
   * 
   * Response
   *
   * 0xB3 - OEM cmd
   * 0x?? - Completion Code
   * bytes 2-3 - instantaneous power headroom
   * bytes 4-5 - peak power headroom
   */

  bytes_rq[0] = 0xBB;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              0x30, /* network function */
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
                                                   0xBB,
                                                   0x30) < 0)
    goto cleanup;

  instantaneous_power_headroom = bytes_rs[2];
  instantaneous_power_headroom |= (bytes_rs[3] << 8);

  peak_power_headroom = bytes_rs[4];
  peak_power_headroom |= (bytes_rs[5] << 8);

  pstdout_printf (state_data->pstate,
		  "Instantaneous Power Headroom : %u W\n",
		  instantaneous_power_headroom);

  pstdout_printf (state_data->pstate,
		  "Peak Power Headroom          : %u W\n",
		  peak_power_headroom);

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_average_power_history (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  uint16_t last_minute_average_power;
  uint16_t last_hour_average_power;
  uint16_t last_day_average_power;
  uint16_t last_week_average_power;
  int len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Source Code
   *
   * Uses Get System Info command
   *
   * Parameter data response formatted:
   *
   * bytes 1-2 - last minute average power
   * bytes 3-4 - last hour average power
   * bytes 5-6 - last day average power
   * bytes 7-8 - last week average power
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
                                           IPMI_OEM_DELL_SYSTEM_INFO_AVERAGE_POWER_HISTORY,
                                           0,
                                           IPMI_SYSTEM_INFO_NO_BLOCK_SELECTOR,
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

  if (len < 8)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_system_info_parameters: invalid buffer length returned: %d\n",
                       len);
      goto cleanup;
    }
  
  last_minute_average_power = configuration_parameter_data[0];
  last_minute_average_power |= (configuration_parameter_data[1] << 8);

  last_hour_average_power = configuration_parameter_data[2];
  last_hour_average_power |= (configuration_parameter_data[3] << 8);

  last_day_average_power = configuration_parameter_data[4];
  last_day_average_power |= (configuration_parameter_data[5] << 8);

  last_week_average_power = configuration_parameter_data[6];
  last_week_average_power |= (configuration_parameter_data[7] << 8);

  pstdout_printf (state_data->pstate,
                  "Last Minute Average Power : %u W\n",
		  last_minute_average_power);

  pstdout_printf (state_data->pstate,
                  "Last Hour Average Power   : %u W\n",
		  last_hour_average_power);

  pstdout_printf (state_data->pstate,
                  "Last Day Average Power    : %u W\n",
		  last_day_average_power);

  pstdout_printf (state_data->pstate,
                  "Last Week Average Power   : %u W\n",
		  last_week_average_power);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_dell_get_peak_power_history (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  uint16_t last_minute_peak_power;
  uint16_t last_hour_peak_power;
  uint16_t last_day_peak_power;
  uint16_t last_week_peak_power;
  uint32_t last_minute_peak_power_time;
  uint32_t last_hour_peak_power_time;
  uint32_t last_day_peak_power_time;
  uint32_t last_week_peak_power_time;
  time_t timetmp;
  struct tm time_tm;
  char time_buf[IPMI_OEM_TIME_BUFLEN + 1];
  int len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell Poweredge OEM
   *
   * From Dell Provided Source Code
   *
   * Uses Get System Info command
   *
   * Parameter data response formatted:
   *
   * bytes 1-2 - last minute peak power
   * bytes 3-4 - last hour peak power
   * bytes 5-6 - last day peak power
   * bytes 7-8 - last week peak power
   * bytes 9-12 - last minute peak power time
   * bytes 13-16 - last hour peak power time
   * bytes 17-20 - last day peak power time
   * bytes 21-24 - last week peak power time
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
                                           IPMI_OEM_DELL_SYSTEM_INFO_PEAK_POWER_HISTORY,
                                           0,
                                           IPMI_SYSTEM_INFO_NO_BLOCK_SELECTOR,
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

  if (len < 24)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_system_info_parameters: invalid buffer length returned: %d\n",
                       len);
      goto cleanup;
    }
  
  last_minute_peak_power = configuration_parameter_data[0];
  last_minute_peak_power |= (configuration_parameter_data[1] << 8);

  last_hour_peak_power = configuration_parameter_data[2];
  last_hour_peak_power |= (configuration_parameter_data[3] << 8);

  last_day_peak_power = configuration_parameter_data[4];
  last_day_peak_power |= (configuration_parameter_data[5] << 8);

  last_week_peak_power = configuration_parameter_data[6];
  last_week_peak_power |= (configuration_parameter_data[7] << 8);

  last_minute_peak_power_time = configuration_parameter_data[8];
  last_minute_peak_power_time |= (configuration_parameter_data[9] << 8);
  last_minute_peak_power_time |= (configuration_parameter_data[10] << 16);
  last_minute_peak_power_time |= (configuration_parameter_data[11] << 24);

  last_hour_peak_power_time = configuration_parameter_data[12];
  last_hour_peak_power_time |= (configuration_parameter_data[13] << 8);
  last_hour_peak_power_time |= (configuration_parameter_data[14] << 16);
  last_hour_peak_power_time |= (configuration_parameter_data[15] << 24);

  last_day_peak_power_time = configuration_parameter_data[16];
  last_day_peak_power_time |= (configuration_parameter_data[17] << 8);
  last_day_peak_power_time |= (configuration_parameter_data[18] << 16);
  last_day_peak_power_time |= (configuration_parameter_data[19] << 24);

  last_week_peak_power_time = configuration_parameter_data[20];
  last_week_peak_power_time |= (configuration_parameter_data[21] << 8);
  last_week_peak_power_time |= (configuration_parameter_data[22] << 16);
  last_week_peak_power_time |= (configuration_parameter_data[23] << 24);

  pstdout_printf (state_data->pstate,
                  "Last Minute Peak Power      : %u W\n",
		  last_minute_peak_power);

  timetmp = last_minute_peak_power_time;
  localtime_r (&timetmp, &time_tm);
  memset (time_buf, '\0', IPMI_OEM_TIME_BUFLEN + 1);
  strftime (time_buf, IPMI_OEM_TIME_BUFLEN, "%D - %T", &time_tm);

  pstdout_printf (state_data->pstate,
                  "Last Minute Peak Power Time : %s\n",
                  time_buf);

  pstdout_printf (state_data->pstate,
                  "Last Hour Peak Power        : %u W\n",
		  last_hour_peak_power);

  timetmp = last_hour_peak_power_time;
  localtime_r (&timetmp, &time_tm);
  memset (time_buf, '\0', IPMI_OEM_TIME_BUFLEN + 1);
  strftime (time_buf, IPMI_OEM_TIME_BUFLEN, "%D - %T", &time_tm);

  pstdout_printf (state_data->pstate,
                  "Last Hour Peak Power Time   : %s\n",
                  time_buf);

  pstdout_printf (state_data->pstate,
                  "Last Day Peak Power         : %u W\n",
		  last_day_peak_power);

  timetmp = last_day_peak_power_time;
  localtime_r (&timetmp, &time_tm);
  memset (time_buf, '\0', IPMI_OEM_TIME_BUFLEN + 1);
  strftime (time_buf, IPMI_OEM_TIME_BUFLEN, "%D - %T", &time_tm);

  pstdout_printf (state_data->pstate,
                  "Last Day Peak Power Time    : %s\n",
                  time_buf);

  pstdout_printf (state_data->pstate,
                  "Last Week Peak Power        : %u W\n",
		  last_week_peak_power);

  timetmp = last_week_peak_power_time;
  localtime_r (&timetmp, &time_tm);
  memset (time_buf, '\0', IPMI_OEM_TIME_BUFLEN + 1);
  strftime (time_buf, IPMI_OEM_TIME_BUFLEN, "%D - %T", &time_tm);

  pstdout_printf (state_data->pstate,
                  "Last Week Peak Power Time   : %s\n",
                  time_buf);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
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
                                           IPMI_OEM_DELL_SYSTEM_INFO_POWER_CAPACITY,
                                           0,
                                           IPMI_SYSTEM_INFO_NO_BLOCK_SELECTOR,
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

  bytes_rq[0] = 0xBA;
  bytes_rq[1] = 0x01;
  bytes_rq[2] = 0xFF;

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
                                                   3,
                                                   0xBA,
                                                   0x30) < 0)
    goto cleanup;

  if (power_capacity_status)
    (*power_capacity_status) = (bytes_rs[2] & 0x01);

  if (power_capacity_is_settable)
    (*power_capacity_is_settable) = ((bytes_rs[2] & 0x02) >> 1); 

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

  /* XXX need info */
#if 0
  pstdout_printf (state_data->pstate,
		  "System Throttling              : %u\n",
		  system_throttling);
#endif

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
  char *ptr = NULL;
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
  power_capacity = strtoul (state_data->prog_data->args->oem_options[0], &ptr, 10);
  if (errno || ptr[0] != '\0')
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "%s:%s invalid OEM option argument '%s'\n",
		       state_data->prog_data->args->oem_id,
		       state_data->prog_data->args->oem_command,
		       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

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
                                           IPMI_OEM_DELL_SYSTEM_INFO_POWER_CAPACITY,
					   configuration_parameter_data,
					   12,
					   obj_cmd_rs) < 0)
    {
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

  bytes_rq[0] = 0xBA;
  bytes_rq[1] = 0x00;
  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "enable"))
    bytes_rq[2] = 0x01;
  else
    bytes_rq[2] = 0x00;

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
                                                   2,
                                                   0xBA,
                                                   0x30) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_dell_get_fcb_version (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int32_t rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell Xanadu2 OEM
   *
   * Get FCB Version Request
   *
   * 0x34 - OEM network function
   * 0x16 - OEM cmd
   *
   * Get FCB Version Response
   *
   * 0x16 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - major version (in hex)
   * 0x?? - minor version (in hex)
   */

  bytes_rq[0] = 0x16;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              0x34, /* network function */
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
                                                   0x16,
                                                   0x34) < 0)
    goto cleanup;

  pstdout_printf (state_data->pstate,
                  "%X.%02X\n",
                  bytes_rs[2],
                  bytes_rs[3]);

  rv = 0;
 cleanup:
  return (rv);
}

#if 0
/* cannot verify */

int
ipmi_oem_dell_get_dhcp_retry (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  uint8_t lan_channel_number;
  int len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell Poweredge OEM
   *
   * Uses Get/Set Lan Configuration
   *
   * parameter = 192
   *
   * Data format
   *
   * 1st byte = retry count, 1 based, 0h = no retries, ffh = infinite
   * 2nd byte = retry interval, 1 based, 10 second increments
   * 3rd byte = retry timeout, 1 based, 1 minute increments
   *
   */

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_rs)))
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

  if (ipmi_cmd_get_lan_configuration_parameters (state_data->ipmi_ctx,
                                                 lan_channel_number,
                                                 IPMI_GET_LAN_PARAMETER,
                                                 192,
                                                 0,
                                                 0,
                                                 obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_lan_configuration_parameters: %s\n",
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
                       "ipmi_cmd_get_lan_configuration_parameters: invalid buffer length returned: %d\n",
                       len);
      goto cleanup;
    }

  if (!configuration_parameter_data[0])
    pstdout_printf (state_data->pstate, "Retry Count    : no retries\n");
  else if (configuration_parameter_data[0] == 0xFF)
    pstdout_printf (state_data->pstate, "Retry Count    : infinite retries\n");
  else
    pstdout_printf (state_data->pstate, "Retry Count    : %u\n", configuration_parameter_data[0]);
  pstdout_printf (state_data->pstate, "Retry Interval : %u seconds\n", configuration_parameter_data[1] * 10);
  pstdout_printf (state_data->pstate, "Retry Timeout  : %u minutes\n", configuration_parameter_data[2]);
                  
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_dell_set_dhcp_retry (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 3);

  return (0);
}
#endif

int
ipmi_oem_dell_get_sol_inactivity_timeout (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  uint8_t lan_channel_number;
  uint16_t sol_inactivity_timeout;
  int len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Dell Poweredge OEM
   *
   * Uses Get/Set SOL Configuration
   *
   * parameter = 192
   *
   * Data format
   *
   * 1st & 2nd byte = inactivity timeout, 1 based, 1 minute
   * increments, LSbyte first
   *
   */

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_sol_configuration_parameters_rs)))
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

  if (ipmi_cmd_get_sol_configuration_parameters (state_data->ipmi_ctx,
                                                 lan_channel_number,
                                                 IPMI_GET_SOL_PARAMETER,
                                                 192,
                                                 0,
                                                 0,
                                                 obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_sol_configuration_parameters: %s\n",
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
                       "ipmi_cmd_get_sol_configuration_parameters: invalid buffer length returned: %d\n",
                       len);
      goto cleanup;
    }

  sol_inactivity_timeout = 0;
  sol_inactivity_timeout |= configuration_parameter_data[0];
  sol_inactivity_timeout |= (configuration_parameter_data[1] << 8);

  if (sol_inactivity_timeout)
    pstdout_printf (state_data->pstate, "SOL Inactivity Timeout : %u minutes\n", sol_inactivity_timeout);
  else
    pstdout_printf (state_data->pstate, "SOL Inactivity Timeout : no timeout\n", sol_inactivity_timeout);
                  
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_dell_set_sol_inactivity_timeout (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t configuration_parameter_data[IPMI_OEM_MAX_BYTES];
  uint8_t lan_channel_number;
  uint16_t sol_inactivity_timeout = 0;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "none"))
    {
      char *ptr = NULL;

      errno = 0;
      
      sol_inactivity_timeout = strtoul (state_data->prog_data->args->oem_options[0], &ptr, 10);
      if (errno || ptr[0] != '\0')
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "%s:%s invalid OEM option argument '%s'\n",
                           state_data->prog_data->args->oem_id,
                           state_data->prog_data->args->oem_command,
                           state_data->prog_data->args->oem_options[0]);
          goto cleanup;
        }
    }
  else
    sol_inactivity_timeout = 0;
  
  /* Dell Poweredge OEM
   *
   * Uses Get/Set SOL Configuration
   *
   * parameter = 192
   *
   * Data format
   *
   * 1st & 2nd byte = inactivity timeout, 1 based, 1 minute
   * increments, LSbyte first
   *
   */

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_set_sol_configuration_parameters_rs)))
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

  configuration_parameter_data[0] = sol_inactivity_timeout & 0x00FF;
  configuration_parameter_data[1] = (sol_inactivity_timeout & 0xFF00) >> 8;

  if (ipmi_cmd_set_sol_configuration_parameters (state_data->ipmi_ctx,
                                                 lan_channel_number,
                                                 192,
                                                 configuration_parameter_data,
                                                 2,
                                                 obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_sol_configuration_parameters: %s\n",
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}
