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

/* 256 b/c length is 8 bit field */
#define IPMI_OEM_DELL_MAX_BYTES 256

#define IPMI_OEM_DELL_SYSTEM_INFO_ASSET_TAG              0xC4
#define IPMI_OEM_DELL_SYSTEM_INFO_SERVICE_TAG            0xC5
#define IPMI_OEM_DELL_SYSTEM_INFO_PRODUCT_NAME           0xD1
#define IPMI_OEM_DELL_SYSTEM_INFO_10G_MAC_ADDRESSES      0xCB
#define IPMI_OEM_DELL_SYSTEM_INFO_11G_MAC_ADDRESSES      0xDA
#define IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_VALIDATOR        0xDD
#define IPMI_OEM_DELL_SYSTEM_INFO_AVERAGE_POWER_HISTORY  0xEB
#define IPMI_OEM_DELL_SYSTEM_INFO_PEAK_POWER_HISTORY     0xEC

#define IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_TYPE_10G            0x08
#define IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_TYPE_11G_MONOLITHIC 0x0A
#define IPMI_OEM_DELL_SYSTEM_INFO_IDRAC_TYPE_11G_MODULAR    0x0B

#define IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_TYPE_ETHERNET  0
#define IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_TYPE_ISCSI     1
#define IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_TYPE_RESERVED  3

/* achu: Dell calls an ethernet port that has been PCI disabled in the
 * BIOS, but has an active service processor "playing dead"
 */
#define IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_STATUS_ENABLED      0
#define IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_STATUS_DISABLED     1
#define IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_STATUS_PLAYING_DEAD 2
#define IPMI_OEM_DELL_SYSTEM_INFO_MAC_ADDRESS_STATUS_RESERVED     3

#define IPMI_OEM_DELL_TOKEN_ID_SSH                 0x0A
#define IPMI_OEM_DELL_TOKEN_ID_TELNET              0x0B
#define IPMI_OEM_DELL_TOKEN_ID_WEB_SERVER          0x0C
#define IPMI_OEM_DELL_TOKEN_ID_ACTIVE_DIRECTORY    0x07

#define IPMI_OEM_DELL_TOKEN_VERSION                0x01

#define IPMI_OEM_DELL_TOKEN_DATA_MAX               65536

#define IPMI_OEM_DELL_TOKEN_STRING_MAX             255

#define IPMI_OEM_DELL_TOKEN_WRITE_MAX              128

#define IPMI_OEM_DELL_TOKEN_DATA_COMMON_HEADER_LEN 5

#define IPMI_OEM_DELL_MAC_ADDRESS_LENGTH 6

#define IPMI_OEM_DELL_11G_MAC_ADDRESS_LENGTH 8 

#define IPMI_IANA_ENTERPRISE_ID_DELL 674

#define IPMI_NET_FN_OEM_GROUP_RQ         0x2E

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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
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
                       fiid_strerror (fiid_obj_errnum (obj_cmd_rs)));
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
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
                       fiid_strerror (fiid_obj_errnum (obj_cmd_rs)));
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

  /* 0h = ascii */
  if (configuration_parameter_data[1])
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
                           fiid_strerror (fiid_obj_errnum (obj_cmd_rs)));
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
                           ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
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
                           fiid_strerror (fiid_obj_errnum (obj_cmd_rs)));
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

  /* Dell OEM
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
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
                       fiid_strerror (fiid_obj_errnum (obj_cmd_rs)));
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
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
                       fiid_strerror (fiid_obj_errnum (obj_cmd_rs)));
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
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
			   ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
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

  /* Dell OEM
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
   * 2nd byte = encoding (0h = ascii)
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

  /* Dell OEM
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
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

  /* Dell OEM
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
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
   * 0x2E - OEM Group
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
                              0x2E, /* network function */
                              bytes_rq, /* data */
                              4, /* num bytes */
                              bytes_rs,
                              IPMI_OEM_MAX_BYTES)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_raw: %s\n",
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
      goto cleanup;
    }

  if (ipmi_oem_check_response_and_completion_code (state_data,
                                                   bytes_rs,
                                                   rs_len,
                                                   6,
                                                   0x01,
                                                   0x2E) < 0)
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
   * Get Extended Configuration Request
   *
   * 0x2E - OEM network function
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

  while (offset < 0xFFFF)
    {
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
      bytes_rq[7] = (offset & 0x00FF);
      bytes_rq[8] = (offset & 0xFF00) >> 8;
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
			   ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
          goto cleanup;
        }
      
      /* atleast 8 bytes of non-token-data + common header */
      if (ipmi_oem_check_response_and_completion_code (state_data,
                                                       bytes_rs,
                                                       rs_len,
                                                       8 + IPMI_OEM_DELL_TOKEN_DATA_COMMON_HEADER_LEN,
                                                       0x02,
                                                       IPMI_NET_FN_OEM_GROUP_RQ) < 0)
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

      bytes_rq[0] = 0x03;
      bytes_rq[1] = (IPMI_IANA_ENTERPRISE_ID_DELL & 0x0000FF);
      bytes_rq[2] = (IPMI_IANA_ENTERPRISE_ID_DELL & 0x00FF00) >> 8;
      bytes_rq[3] = (IPMI_IANA_ENTERPRISE_ID_DELL & 0xFF0000) >> 16;
      bytes_rq[4] = reservation_id;
      bytes_rq[5] = token_id;
      bytes_rq[6] = 0x00;
      bytes_rq[7] = (offset & 0x00FF);
      bytes_rq[8] = (offset & 0xFF00) >> 8;
      bytes_rq[9] = 0x01;

      if (!offset)
        {
          /* common header */
          
          bytes_rq[10] = (token_len & 0x00FF);
          bytes_rq[11] = (token_len & 0xFF00) >> 8;
          bytes_rq[12] = IPMI_OEM_DELL_TOKEN_VERSION;
          bytes_rq[13] = (valid_field_mask & 0x00FF); 
          bytes_rq[14] = (valid_field_mask & 0xFF00) >> 8; 

          /* - 5 for the common header */
          if ((token_data_len - offset) > (IPMI_OEM_DELL_TOKEN_WRITE_MAX - 5))
            write_length = IPMI_OEM_DELL_TOKEN_WRITE_MAX - 5;
          else
            write_length = (token_data_len - offset);
          
          memcpy (&bytes_rq[15],
                  token_data + offset,
                  write_length);

          token_write_length = 5 + write_length;
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
			   ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
          goto cleanup;
        }
      
      if (ipmi_oem_check_response_and_completion_code (state_data,
                                                       bytes_rs,
                                                       rs_len,
                                                       6,
                                                       0x03,
                                                       IPMI_NET_FN_OEM_GROUP_RQ) < 0)
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
  uint16_t expected_valid_field_mask = 0x1F; /* 5 fields */
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
					IPMI_OEM_DELL_TOKEN_ID_SSH,
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

static int 
_parse_key_value (ipmi_oem_state_data_t *state_data,
                  unsigned int option_num,
                  char **key,
                  char **value)
{  
  char *tempstr = NULL;
  char *tempptr = NULL;
  char *tempkey = NULL;
  char *tempvalue = NULL;
  int rv = -1;
   
  assert (state_data);
  assert (key);
  assert (value);
   
  if (!(tempstr = strdup (state_data->prog_data->args->oem_options[option_num])))
    {
      pstdout_perror (state_data->pstate,
                      "strdup");
      goto cleanup;
    }
   
  tempptr = strchr (tempstr, '=');
  if (!tempptr)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s' : no equal sign\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[option_num]);
      goto cleanup;
    }

  (*tempptr) = '\0'; 
  tempptr++;

  if (!(tempkey = strdup (tempstr)))
    {
      pstdout_perror (state_data->pstate,
                      "strdup");
      goto cleanup;
    }

  if (!(tempvalue = strdup (tempptr)))
    {
      pstdout_perror (state_data->pstate,
                      "strdup");
      goto cleanup; 
    }
  
  (*key) = tempkey;
  (*value) = tempvalue;
  
  rv = 0;
 cleanup:
  free (tempstr);
  if (rv < 0)
    {
      free (tempkey);
      free (tempvalue);
    }
  return (rv);
}

static int
_parse_enable (ipmi_oem_state_data_t *state_data,
               unsigned int option_num,
               const char *value,
               uint8_t *enable)
{
  assert (state_data);
  assert (value);
  assert (enable);

  if (strcasecmp (value, "enable") && strcasecmp (value, "disable"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s' : invalid value\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[option_num]);
      return (-1);
    }
  
  if (!strcasecmp (value, "enable"))
    (*enable) = 1;
  else
    (*enable) = 0;
  
  return (0);
}

static int
_parse_timeout (ipmi_oem_state_data_t *state_data,
                unsigned int option_num,
                const char *value,
                uint32_t *timeout)
{ 
  unsigned int temp;
  char *ptr = NULL;
  
  assert (state_data);
  assert (value);
  assert (timeout);
  
  errno = 0;
  
  temp = strtoul (value, &ptr, 10);
  
  if (errno
      || ptr[0] != '\0')
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s' : invalid value\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[option_num]);
      return (-1);
    }
  
  (*timeout) = temp;
  return (0);
}

static int
_parse_port (ipmi_oem_state_data_t *state_data,
             unsigned int option_num,
             const char *value,
             uint16_t *port)
{
  unsigned int temp;
  char *ptr = NULL;

  assert (state_data);
  assert (value);
  assert (port);

  errno = 0;
  
  temp = strtoul (value, &ptr, 10);
  
  if (errno
      || ptr[0] != '\0'
      || temp > 65535)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s' : invalid value\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[option_num]);
      return (-1);
    }
  
  (*port) = temp;
  return (0);
}

#if 0
/* don't support for now */
static int
_parse_string (ipmi_oem_state_data_t *state_data,
	       unsigned int option_num,
	       const char *value,
	       uint8_t *string_length,
	       char *stringbuf,
	       unsigned int stringbuflen)
{
  assert (state_data);
  assert (value);
  assert (string_length);
  assert (stringbuf);
  assert (stringbuflen);
  
  if (strlen (value) > stringbuflen)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s' : string length too long\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[option_num]);
      return (-1);
    }

  (*string_length) = strlen (value);

  /* use memcpy, do not need NULL termination */
  if ((*string_length))
    memcpy (stringbuf,
	    value,
	    (*string_length));
  
  return (0);
}
#endif

int
ipmi_oem_dell_set_ssh_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t token_data[IPMI_OEM_DELL_TOKEN_DATA_MAX];
  uint16_t valid_field_mask = 0;
  uint8_t sshenable = 0;
  uint32_t idletimeout = 0;
  uint16_t portnumber = 0;
  int rv = -1;
  int i;

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
      
      if (_parse_key_value (state_data,
                            i,
                            &key,
                            &value) < 0)
        goto cleanup;

      if (!strcasecmp (key, "ssh"))
        {
          if (_parse_enable (state_data, i, value, &sshenable) < 0)
            goto cleanup;
          
          valid_field_mask |= 0x0001;
        }
      else if (!strcasecmp (key, "idletimeout"))
        {
          if (_parse_timeout (state_data, i, value, &idletimeout) < 0)
            goto cleanup;
          
          valid_field_mask |= 0x0008;
        }
      else if (!strcasecmp (key, "portnumber"))
        {
          if (_parse_port (state_data, i, value, &portnumber) < 0)
            goto cleanup;
          
          valid_field_mask |= 0x0010;
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
  uint8_t token_data[IPMI_OEM_DELL_TOKEN_DATA_MAX];
  uint16_t expected_valid_field_mask = 0x3F; /* 6 fields */
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
					IPMI_OEM_DELL_TOKEN_ID_TELNET,
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
  int i;

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
      
      if (_parse_key_value (state_data,
                            i,
                            &key,
                            &value) < 0)
        goto cleanup;

      if (!strcasecmp (key, "telnet"))
        {
          if (_parse_enable (state_data, i, value, &telnetenable) < 0)
            goto cleanup;

          valid_field_mask |= 0x0001;
        }
      else if (!strcasecmp (key, "sessiontimeout"))
        {
          if (_parse_timeout (state_data, i, value, &sessiontimeout) < 0)
            goto cleanup;

          valid_field_mask |= 0x0008;
        }
      else if (!strcasecmp (key, "portnumber"))
        {
          if (_parse_port (state_data, i, value, &portnumber) < 0)
            goto cleanup;
          
          valid_field_mask |= 0x0010;
        }
      else if (!strcasecmp (key, "7fls"))
        {
          if (_parse_enable (state_data, i, value, &_7flsenable) < 0)
            goto cleanup;
          
          valid_field_mask |= 0x0020;
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
  uint8_t token_data[IPMI_OEM_DELL_TOKEN_DATA_MAX];
  uint16_t expected_valid_field_mask = 0x3F; /* 6 fields */
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
					IPMI_OEM_DELL_TOKEN_ID_WEB_SERVER,
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
  int i;

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
      
      if (_parse_key_value (state_data,
                            i,
                            &key,
                            &value) < 0)
        goto cleanup;

      if (!strcasecmp (key, "webserver"))
        {
          if (_parse_enable (state_data, i, value, &webserverenable) < 0)
            goto cleanup;

          valid_field_mask |= 0x0001;
        }
      else if (!strcasecmp (key, "sessiontimeout"))
        {
          if (_parse_timeout (state_data, i, value, &sessiontimeout) < 0)
            goto cleanup;
          
          valid_field_mask |= 0x0008;
        }
      else if (!strcasecmp (key, "httpportnumber"))
        {
          if (_parse_port (state_data, i, value, &httpportnumber) < 0)
            goto cleanup;
          
          valid_field_mask |= 0x0010;
        }
      else if (!strcasecmp (key, "httpsportnumber"))
        {
          if (_parse_port (state_data, i, value, &httpsportnumber) < 0)
            goto cleanup;
          
          valid_field_mask |= 0x0020;
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
ipmi_oem_dell_get_active_directory_config (ipmi_oem_state_data_t *state_data)
{
  uint8_t token_data[IPMI_OEM_MAX_BYTES];
  uint16_t expected_valid_field_mask = 0xFFFF; /* 16 fields */
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
					IPMI_OEM_DELL_TOKEN_ID_ACTIVE_DIRECTORY,
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
  int i;

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
      
      if (_parse_key_value (state_data,
                            i,
                            &key,
                            &value) < 0)
        goto cleanup;

      if (!strcasecmp (key, "activedirectory"))
        {
          if (_parse_enable (state_data, i, value, &ad_enable) < 0)
            goto cleanup;

          valid_field_mask |= 0x0001;
        }
      else if (!strcasecmp (key, "timeout"))
        {
          if (_parse_timeout (state_data, i, value, &ad_timeout) < 0)
            goto cleanup;
          
          valid_field_mask |= 0x0002;
        }
#if 0
/* don't support for now */
      else if (!strcasecmp (key, "rootdomain"))
        {
          if (_parse_string (state_data,
			     i,
			     value,
			     &ad_root_domain_string_length,
			     ad_root_domain_string,
			     IPMI_OEM_DELL_TOKEN_STRING_MAX) < 0)
            goto cleanup;
          
          valid_field_mask |= 0x0004;
        }
      else if (!strcasecmp (key, "racdomain"))
        {
          if (_parse_string (state_data,
			     i,
			     value,
			     &ad_rac_domain_string_length,
			     ad_rac_domain_string,
			     IPMI_OEM_DELL_TOKEN_STRING_MAX) < 0)
            goto cleanup;
          
          valid_field_mask |= 0x0008;
        }
      else if (!strcasecmp (key, "racname"))
        {
          if (_parse_string (state_data,
			     i,
			     value,
			     &ad_rac_name_string_length,
			     ad_rac_name_string,
			     IPMI_OEM_DELL_TOKEN_STRING_MAX) < 0)
            goto cleanup;
          
          valid_field_mask |= 0x0010;
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
	    ad_type = 1;
	  else
	    ad_type = 2;

	  valid_field_mask |= 0x0020;
	}
#if 0
      /* read only on iDRAC6 */
      else if (!strcasecmp (key, "smartcardlogon"))
        {
          if (_parse_enable (state_data, i, value, &scl_state) < 0)
            goto cleanup;

          valid_field_mask |= 0x0040;
        }
#endif
#if 0
      /* read only on iDRAC6 */
      else if (!strcasecmp (key, "certificaterevocationlist"))
        {
          if (_parse_enable (state_data, i, value, &crl_state) < 0)
            goto cleanup;

          valid_field_mask |= 0x0080;
        }
#endif
      else if (!strcasecmp (key, "sso"))
        {
          if (_parse_enable (state_data, i, value, &ad_sso_enable) < 0)
            goto cleanup;

          valid_field_mask |= 0x0100;
        }
#if 0
/* don't support for now */
      else if (!strcasecmp (key, "dcfilter1"))
        {
          if (_parse_string (state_data,
			     i,
			     value,
			     &ad_dc_filter1_string_length,
			     ad_dc_filter1_string,
			     IPMI_OEM_DELL_TOKEN_STRING_MAX) < 0)
            goto cleanup;
          
          valid_field_mask |= 0x0200;
        }
      else if (!strcasecmp (key, "dcfilter2"))
        {
          if (_parse_string (state_data,
			     i,
			     value,
			     &ad_dc_filter2_string_length,
			     ad_dc_filter2_string,
			     IPMI_OEM_DELL_TOKEN_STRING_MAX) < 0)
            goto cleanup;
          
          valid_field_mask |= 0x0400;
        }
      else if (!strcasecmp (key, "dcfilter3"))
        {
          if (_parse_string (state_data,
			     i,
			     value,
			     &ad_dc_filter3_string_length,
			     ad_dc_filter3_string,
			     IPMI_OEM_DELL_TOKEN_STRING_MAX) < 0)
            goto cleanup;
          
          valid_field_mask |= 0x0800;
        }
      else if (!strcasecmp (key, "gcfilter1"))
        {
          if (_parse_string (state_data,
			     i,
			     value,
			     &ad_gc_filter1_string_length,
			     ad_gc_filter1_string,
			     IPMI_OEM_DELL_TOKEN_STRING_MAX) < 0)
            goto cleanup;
          
          valid_field_mask |= 0x1000;
        }
      else if (!strcasecmp (key, "gcfilter2"))
        {
          if (_parse_string (state_data,
			     i,
			     value,
			     &ad_gc_filter2_string_length,
			     ad_gc_filter2_string,
			     IPMI_OEM_DELL_TOKEN_STRING_MAX) < 0)
            goto cleanup;
          
          valid_field_mask |= 0x2000;
        }
      else if (!strcasecmp (key, "gcfilter3"))
        {
          if (_parse_string (state_data,
			     i,
			     value,
			     &ad_gc_filter3_string_length,
			     ad_gc_filter3_string,
			     IPMI_OEM_DELL_TOKEN_STRING_MAX) < 0)
            goto cleanup;
          
          valid_field_mask |= 0x4000;
        }
#endif
      else if (!strcasecmp (key, "certificatevalidation"))
        {
          if (_parse_enable (state_data, i, value, &ad_certificate_validation_enable) < 0)
            goto cleanup;

          valid_field_mask |= 0x8000;
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
					IPMI_OEM_DELL_TOKEN_ID_ACTIVE_DIRECTORY,
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
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
			   ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
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

  /* Dell OEM
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
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

  /* Dell OEM
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
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
                       fiid_strerror (fiid_obj_errnum (obj_cmd_rs)));
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
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
                       fiid_strerror (fiid_obj_errnum (obj_cmd_rs)));
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

int
ipmi_oem_dell_get_fcb_version (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int32_t rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* llnlxanadu2 OEM
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
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
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
