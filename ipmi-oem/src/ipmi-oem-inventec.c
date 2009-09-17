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
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"
#include "ipmi-oem-inventec.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

#define IPMI_OEM_MAX_MACADDRLEN 24
#define IPMI_OEM_SET_SELECTOR   0x0
#define IPMI_OEM_BLOCK_SELECTOR 0x0

#define IPMI_OEM_EEPROM_AT24C256N_SLAVE_ADDRESS 0x53
#define IPMI_OEM_EEPROM_AT24C256N_BUS_ID        2
#define IPMI_OEM_EEPROM_AT24C256N_ADDRESS_MIN   0x0000
#define IPMI_OEM_EEPROM_AT24C256N_ADDRESS_MAX   0x7FFF
#define IPMI_OEM_EEPROM_AT24C256N_CLEAR_BYTE    0xFF

static int
_inventec_get_reservation (ipmi_oem_state_data_t *state_data,
                           uint8_t *reservation_id)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int32_t rs_len;
  int rv = -1;

  /* Inventec OEM
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

  bytes_rq[0] = 0x01;

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
                                                   0x01,
                                                   0x30) < 0)
    goto cleanup;
  
  (*reservation_id) = bytes_rs[2];

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_get_nic_status (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int32_t rs_len;
  uint8_t reservation_id;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Inventec OEM
   *
   * Get NIC Status Request
   *
   * 0x30 - OEM network function
   * 0x02 - OEM cmd
   * 0x?? - Reservation ID
   * 0x02 - Configuration ID (0x02 = ??)
   * 0x01 - Attribute ID (0x01 = ??)
   * 0x00 - Index (unused here??)
   * 0x00 - Data Offset - LSB (unused here??)
   * 0x00 = Data Offset - MSB (unused here??)
   * 0xFF - Bytes to read (0xFF = all)
   * 
   * Get NIC Status Response
   *
   * 0x02 - OEM cmd
   * 0x?? - Completion Code
   * 0x02 - Configuration ID (0x02 = ??)
   * 0x01 - Attribute ID (0x01 = ??)
   * 0x00 - Index (unused here??)
   * 0x01 - number of bytes returned
   * 0x00 | 0x01 - 0x00 = shared, 0x01 = dedicated
   */

  if (_inventec_get_reservation (state_data,
                                 &reservation_id) < 0)
    goto cleanup;

  bytes_rq[0] = 0x02;
  bytes_rq[1] = reservation_id;
  bytes_rq[2] = 0x02;
  bytes_rq[3] = 0x01;
  bytes_rq[4] = 0x00;
  bytes_rq[5] = 0x00;
  bytes_rq[6] = 0x00;
  bytes_rq[7] = 0xFF;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              0x30, /* network function */
                              bytes_rq, /* data */
                              8, /* num bytes */
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
                                                   7,
                                                   0x02,
                                                   0x30) < 0)
    goto cleanup;

  switch (bytes_rs[6])
    {
    case 0:
      pstdout_printf (state_data->pstate, "shared\n");
      break;
    case 1:
      pstdout_printf (state_data->pstate, "dedicated\n");
      break;
    default:
      pstdout_printf (state_data->pstate, "unknown NIC status: %Xh\n", bytes_rs[7]);
      break;
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_set_nic_status (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int32_t rs_len;
  uint8_t reservation_id;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

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

  /* Inventec OEM
   *
   * Set NIC Status Request
   *
   * 0x30 - OEM network function
   * 0x03 - OEM cmd
   * 0x?? - Reservation ID
   * 0x02 - Configuration ID (0x02 = ??)
   * 0x01 - Attribute ID (0x01 = ??)
   * 0x00 - Index (unused here??)
   * 0x00 - Data Offset - LSB (unused here??)
   * 0x00 = Data Offset - MSB (unused here??)
   * 0x01 - In progress bit (0x00 in progress, 0x01 - last config in this request)
   * 0x00 | 0x01 - 0x00 = shared, 0x01 = dedicated
   * 
   * Set NIC Status Response
   *
   * 0x03 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - bytes written
   */

  if (_inventec_get_reservation (state_data,
                                 &reservation_id) < 0)
    goto cleanup;

  bytes_rq[0] = 0x03;
  bytes_rq[1] = reservation_id;
  bytes_rq[2] = 0x02;
  bytes_rq[3] = 0x01;
  bytes_rq[4] = 0x00;
  bytes_rq[5] = 0x00;
  bytes_rq[6] = 0x00;
  bytes_rq[7] = 0x01;

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "shared"))
    bytes_rq[8] = 0x00;
  else
    bytes_rq[8] = 0x01;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              0x30, /* network function */
                              bytes_rq, /* data */
                              9, /* num bytes */
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

int
ipmi_oem_inventec_get_mac_address (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  char mac_address_str[IPMI_OEM_MAX_MACADDRLEN+1];
  uint8_t mac_address_bytes[6];
  int8_t lan_channel_number;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Use normal IPMI to get the MAC address.  This is offered more as
   * a convenience to the user rather, so there is always a "get" and
   * a "set" command in ipmi-oem.
   */

  if ((lan_channel_number = ipmi_get_channel_number (state_data->ipmi_ctx,
                                                     IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_get_channel_number: %s\n",
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
      goto cleanup;
    }

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_get_lan_configuration_parameters_mac_address_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }
  
  if (ipmi_cmd_get_lan_configuration_parameters_mac_address (state_data->ipmi_ctx,
                                                             lan_channel_number,
                                                             IPMI_GET_LAN_PARAMETER,
                                                             IPMI_OEM_SET_SELECTOR,
                                                             IPMI_OEM_BLOCK_SELECTOR,
                                                             obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_cmd_get_lan_configuration_parameters_mac_address: %s\n",
                       ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
      goto cleanup;
    }

  if (fiid_obj_get_data (obj_cmd_rs,
                         "mac_address",
                         mac_address_bytes,
                         6) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_get_data: 'mac_address': %s\n",
                       fiid_strerror (fiid_obj_errnum (obj_cmd_rs)));
      goto cleanup;
    }
  
  memset (mac_address_str, '\0', IPMI_OEM_MAX_MACADDRLEN+1);
  snprintf (mac_address_str,
            IPMI_OEM_MAX_MACADDRLEN,
            "%02X:%02X:%02X:%02X:%02X:%02X",
            mac_address_bytes[0],
            mac_address_bytes[1],
            mac_address_bytes[2],
            mac_address_bytes[3],
            mac_address_bytes[4],
            mac_address_bytes[5]);

  pstdout_printf (state_data->pstate,
                  "%s\n",
                  mac_address_str);

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_inventec_set_mac_address (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int32_t rs_len;
  unsigned int tmp;
  uint8_t cmd;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 2);

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

  if (sscanf (state_data->prog_data->args->oem_options[1],
              "%02x:%02x:%02x:%02x:%02x:%02x",
              &tmp,
              &tmp,
              &tmp,
              &tmp,
              &tmp,
              &tmp) != 6)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }

  /* Inventec OEM
   *
   * Set MAC Address Request
   *
   * 0x2E - OEM network function
   * 0x21 | 0x23 - OEM cmd - 0x21 = dedicated, 0x23 = shared
   * bytes 1-17: MAC address in ASCII (including semicolons)
   * 0x00 - sentinel value 0x00
   * 
   * Set MAC Address Response
   *
   * 0x21 | 0x23 - OEM cmd - 0x21 = dedicated, 0x23 = shared
   * 0x?? - Completion Code
   */

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "dedicated"))
    cmd = 0x21;
  else
    cmd = 0x23;
  
  bytes_rq[0] = cmd;
  memcpy (&bytes_rq[1],
          state_data->prog_data->args->oem_options[1],
          17);
  bytes_rq[18] = 0x00;
  
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              0x2E, /* network function */
                              bytes_rq, /* data */
                              19, /* num bytes */
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
                                                   cmd,
                                                   0x2E) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

static int
_inventec_get_bmc_services (ipmi_oem_state_data_t *state_data,
                            uint8_t *services)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int32_t rs_len;
  uint8_t reservation_id;
  int rv = -1;

  assert (state_data);
  assert (services);

  /* Inventec OEM
   *
   * Get BMC Services Request
   *
   * 0x30 - OEM network function
   * 0x02 - OEM cmd
   * 0x?? - Reservation ID
   * 0x04 - Configuration ID (0x04 = Security)
   * 0x01 - Attribute ID (0x01 = Service Disabled)
   * 0x00 - Index (unused here)
   * 0x00 - Data Offset - LSB (unused here)
   * 0x00 = Data Offset - MSB (unused here)
   * 0xFF - Bytes to read (0xFF = all)
   * 
   * Get BMC Services Response
   *
   * 0x03 - OEM cmd
   * 0x?? - Completion Code
   * 0x04 - Configuration ID (0x04 = Security)
   * 0x01 - Attribute ID (0x01 = Service Disabled)
   * 0x00 - Index (unused here)
   * 0x01 - number of bytes returned
   * 0xXX - services
   *
   * services bit 0 : All services except IPMI disabled
   * services bit 1 : KVM/Virtual Storage disabled
   * services bit 2 : HTTP/HTTPS disabled
   * services bit 3 : SSH/Telnet disabled
   */

  if (_inventec_get_reservation (state_data,
                                 &reservation_id) < 0)
    goto cleanup;

  bytes_rq[0] = 0x02;
  bytes_rq[1] = reservation_id;
  bytes_rq[2] = 0x04;
  bytes_rq[3] = 0x01;
  bytes_rq[4] = 0x00;
  bytes_rq[5] = 0x00;
  bytes_rq[6] = 0x00;
  bytes_rq[7] = 0xFF;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              0x30, /* network function */
                              bytes_rq, /* data */
                              8, /* num bytes */
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
                                                   7,
                                                   0x02,
                                                   0x30) < 0)
    goto cleanup;

  (*services) = bytes_rs[6];
  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_inventec_get_bmc_services (ipmi_oem_state_data_t *state_data)
{
  uint8_t services = 0;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  if (_inventec_get_bmc_services (state_data, &services) < 0)
    goto cleanup;

  if (services)
    {
      /* achu: it is not clear if only one bit or multiple bits can be
       * set.  I'm assuming if the "all" bit is set, there is no need
       * to output anything else.
       */
      if (services & 0x1)
        {
          pstdout_printf (state_data->pstate, "All services except IPMI disabled\n");
          goto out;
        }
      if (services & 0x2)
        pstdout_printf (state_data->pstate, "KVM/Virtual Storage disabled\n");
      if (services & 0x4)
        pstdout_printf (state_data->pstate, "HTTP/HTTPS disabled\n");
      if (services & 0x8)
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
ipmi_oem_inventec_set_bmc_services (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int enable = 0;
  int32_t rs_len;
  uint8_t reservation_id;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 2);

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

  /* Inventec OEM
   *
   * Disable/Enable Non-IPMI BMC Ports Request
   *
   * 0x30 - OEM network function
   * 0x03 - OEM cmd
   * 0x?? - Reservation ID
   * 0x04 - Configuration ID (0x04 = Security)
   * 0x01 - Attribute ID (0x01 = Service Disabled)
   * 0x00 - Index (unused here)
   * 0x00 - Data Offset - LSB (unused here)
   * 0x00 = Data Offset - MSB (unused here)
   * 0x01 - Bytes to read
   * 0xXX - 0x00 - enable all
   *        0x01 - disable all except IPMI
   *        0x02 - disable KVM/Virtual Storage
   *        0x04 - disable HTTP/HTTPS
   *        0x08 - disable SSH/Telent
   *
   * Disable Non-IPMI BMC Ports Response
   *
   * 0x03 - OEM cmd
   * 0x?? - Completion Code
   */

  /* achu: do bytes_rq[8] first, b/c we may call
   * _inventec_get_bmc_services, which does a get reservation id call
   * too.
   */

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "enable"))
    enable = 1;
        
  /* if all, it's an easy special case */
  if (!strcasecmp (state_data->prog_data->args->oem_options[1], "all"))
    {
      if (enable)
        bytes_rq[8] = 0x00;
      else
        bytes_rq[8] = 0x01;
    }
  else
    {
      uint8_t services = 0;

      if (_inventec_get_bmc_services (state_data, &services) < 0)
        goto cleanup;

      if (enable && (services & 0x1))
        {
          /* clear out "all" bit, and replace with remaining bits */
          services &= 0xFE;
          services |= 0x0E;
        }

      if (!strcasecmp (state_data->prog_data->args->oem_options[1], "kvm"))
        {
          if (enable)
            services &= 0xFD;
          else
            services |= 0x2;
        }
      else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "http"))
        {
          if (enable)
            services &= 0xFB;
          else
            services |= 0x4;
        }
      else /* !strcasecmp (state_data->prog_data->args->oem_options[1], "ssh") */
        {
          if (enable)
            services &= 0xF7;
          else
            services |= 0x8;
        }

      bytes_rq[8] = services;
    }

  if (_inventec_get_reservation (state_data,
                                 &reservation_id) < 0)
    goto cleanup;

  bytes_rq[0] = 0x03;
  bytes_rq[1] = reservation_id;
  bytes_rq[2] = 0x04;
  bytes_rq[3] = 0x01;
  bytes_rq[4] = 0x00;
  bytes_rq[5] = 0x00;
  bytes_rq[6] = 0x00;
  bytes_rq[7] = 0x01;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              0x30, /* network function */
                              bytes_rq, /* data */
                              9, /* num bytes */
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
                                                   0x02,
                                                   0x30) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_oem_inventec_read_eeprom_at24c256n (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t data_rq[IPMI_OEM_MAX_BYTES];
  uint8_t data_rs[IPMI_OEM_MAX_BYTES];
  unsigned int read_count = 0;
  int len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  /* Uses Master-Read Write Command
   *
   * Most addresses provided directly from Dell.
   *
   * byte 1 = 5 (channel = 0, bus id = 2, bus-type = 1 = private)
   * byte 2 = 0xA6 (slave address 7 bit = 0x53, lowest bit for r/w, 0b = read, 1b = write)
   * byte 3 = read count, we'll use 1
   * byte 4/5 - address to read, msb first
   * 
   * response
   *
   * byte 1 = comp-code
   * byte N = read data
   *
   * address ranges from 0x0000 - 0x7fff
   */

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_master_write_read_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }
  
  if (fiid_obj_clear (obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_clear: %s\n",
                       fiid_strerror (fiid_obj_errnum (obj_cmd_rs)));
      goto cleanup;
    }
  
  while (read_count <= IPMI_OEM_EEPROM_AT24C256N_ADDRESS_MAX)
    {
      data_rq[0] = (read_count & 0xFF00) >> 8;
      data_rq[1] = (read_count & 0x00FF);
      
      if (ipmi_cmd_master_write_read (state_data->ipmi_ctx,
                                      IPMI_BUS_TYPE_PRIVATE,
                                      IPMI_OEM_EEPROM_AT24C256N_BUS_ID,
                                      0,
                                      IPMI_OEM_EEPROM_AT24C256N_SLAVE_ADDRESS,
                                      1,
                                      data_rq,
                                      2,
                                      obj_cmd_rs) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_cmd_master_write_read: %s\n",
                           ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
          goto cleanup;
        }

      if ((len = fiid_obj_get_data (obj_cmd_rs,
                                    "data",
                                    data_rs,
                                    IPMI_OEM_MAX_BYTES)) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "fiid_obj_get_data: %s\n",
                           fiid_strerror (fiid_obj_errnum (obj_cmd_rs)));
          goto cleanup;
        }
      
      if (len)
        {
          int i;
          
          for (i = read_count; i < (read_count + len); i++)
            {
              if (i && (i % 8) == 0)
                pstdout_printf (state_data->pstate, "\n");
              
              pstdout_printf (state_data->pstate, "0x%02X ", data_rs[i - read_count]);
            }
          
          read_count += len;
        }
    }

  pstdout_printf (state_data->pstate, "\n");
  
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_inventec_read_eeprom (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "at24c256n"))
    return _ipmi_oem_inventec_read_eeprom_at24c256n (state_data);
  
  pstdout_fprintf (state_data->pstate,
                   stderr,
                   "%s:%s invalid OEM option argument '%s'\n",
                   state_data->prog_data->args->oem_id,
                   state_data->prog_data->args->oem_command,
                   state_data->prog_data->args->oem_options[0]);
  return (-1);
}

static int
_ipmi_oem_inventec_clear_eeprom_at24c256n (ipmi_oem_state_data_t *state_data)
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint8_t data_rq[IPMI_OEM_MAX_BYTES];
  unsigned int count = 0;
  unsigned int percent = 0;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  /* Uses Master-Read Write Command
   *
   * Most addresses provided directly from Dell.
   *
   * byte 1 = 5 (channel = 0, bus id = 2, bus-type = 1 = private)
   * byte 2 = 0xA6 (slave address 7 bit = 0x53, lowest bit for r/w, 0b = read, 1b = write)
   * byte 3 = read count, 0 to write
   * byte 4/5 - address to read, msb first
   * byte 6 - data to write
   * 
   * response
   *
   * byte 1 = comp-code
   * byte N = read data
   *
   * address ranges from 0x0000 - 0x7fff
   */

  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_master_write_read_rs)))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_create: %s\n",
                       strerror (errno));
      goto cleanup;
    }

  if (fiid_obj_clear (obj_cmd_rs) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_obj_clear: %s\n",
                       fiid_strerror (fiid_obj_errnum (obj_cmd_rs)));
      goto cleanup;
    }

  if (state_data->prog_data->args->verbose_count)
    fprintf (stderr, "%u%%\r", percent);

  while (count <= IPMI_OEM_EEPROM_AT24C256N_ADDRESS_MAX)
    {
      data_rq[0] = (count & 0xFF00) >> 8;
      data_rq[1] = (count & 0x00FF);
      data_rq[2] = IPMI_OEM_EEPROM_AT24C256N_CLEAR_BYTE;

      if (ipmi_cmd_master_write_read (state_data->ipmi_ctx,
                                      IPMI_BUS_TYPE_PRIVATE,
                                      IPMI_OEM_EEPROM_AT24C256N_BUS_ID,
                                      0,
                                      IPMI_OEM_EEPROM_AT24C256N_SLAVE_ADDRESS,
                                      0,
                                      data_rq,
                                      3,
                                      obj_cmd_rs) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_cmd_master_write_read: %s\n",
                           ipmi_ctx_strerror (ipmi_ctx_errnum (state_data->ipmi_ctx)));
          goto cleanup;
        }

      if (state_data->prog_data->args->verbose_count)
        {
          if ((unsigned int)(((double)count/IPMI_OEM_EEPROM_AT24C256N_ADDRESS_MAX) * 100) > percent)
            {
              fprintf (stderr, "%u%%\r", percent);
              percent++;
            }
        }

      count++;
    }

  if (state_data->prog_data->args->verbose_count)
    fprintf (stderr, "100%%\r\n");

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_cmd_rs);
  return (rv);
}

int
ipmi_oem_inventec_clear_eeprom (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "at24c256n"))
    return _ipmi_oem_inventec_clear_eeprom_at24c256n (state_data);
  
  pstdout_fprintf (state_data->pstate,
                   stderr,
                   "%s:%s invalid OEM option argument '%s'\n",
                   state_data->prog_data->args->oem_id,
                   state_data->prog_data->args->oem_command,
                   state_data->prog_data->args->oem_options[0]);
  return (-1);
}
