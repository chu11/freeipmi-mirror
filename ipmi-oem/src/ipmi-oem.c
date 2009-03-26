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
#include <limits.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"

#define IPMI_OEM_MAX_BYTES      256
#define IPMI_OEM_ERR_BUFLEN     1024
#define IPMI_OEM_MAX_MACADDRLEN 24
#define IPMI_OEM_SET_SELECTOR   0x0
#define IPMI_OEM_BLOCK_SELECTOR 0x0

typedef int (*oem_callback)(ipmi_oem_state_data_t *);

struct ipmi_oem_command
{
  char *oem_command;
  char *description;
  int required_oem_options;
  oem_callback func;
};

struct ipmi_oem_id
{
  char *oem_id;
  struct ipmi_oem_command *oem_commands;
};

static int _inventec_get_nic_status (ipmi_oem_state_data_t *);
static int _inventec_set_nic_status (ipmi_oem_state_data_t *);
static int _inventec_get_mac_address (ipmi_oem_state_data_t *);
static int _inventec_set_mac_address (ipmi_oem_state_data_t *);
static int _supermicro_reset_intrusion (ipmi_oem_state_data_t *);

struct ipmi_oem_command oem_inventec[] =
  {
    { "get-nic-status",
      "get NIC status",
      0,
      _inventec_get_nic_status},
    { "set-nic-status",
      "set NIC status",
      1,
      _inventec_set_nic_status},
    { "get-mac-address",
      "get MAC address",
      0,
      _inventec_get_mac_address},
    { "set-mac-address",
      "set MAC address",
      2,
      _inventec_set_mac_address},
    { NULL, NULL, 0, NULL},
  };

struct ipmi_oem_command oem_supermicro[] =
  {
    { "reset-intrusion",
      "reset motherboard intrusion flag",
      0,
      _supermicro_reset_intrusion},
    { NULL, NULL, 0, NULL},
  };

struct ipmi_oem_id oem_cb[] =
  {
    { "inventec", oem_inventec},
    { "supermicro", oem_supermicro},
    { NULL, NULL},
  };

static int
_list (void)
{
  struct ipmi_oem_id *oem_id = oem_cb;

  while (oem_id && oem_id->oem_id)
    {
      struct ipmi_oem_command *oem_cmd = oem_id->oem_commands;

      printf ("OEM ID: %s\n", oem_id->oem_id);

      while (oem_cmd && oem_cmd->oem_command)
        {
          printf ("    Command: %s - %s\n",
                  oem_cmd->oem_command,
                  oem_cmd->description);
          oem_cmd++;
        }

      printf ("\n");
      oem_id++;
    }

  return (0);
}

static int
_check_response_and_completion_code (ipmi_oem_state_data_t *state_data,
                                     uint8_t *bytes_rs,
                                     unsigned int bytes_rs_len,
                                     unsigned int expected_bytes_rs_len,
                                     uint8_t cmd,
                                     uint8_t netfn)
{
  assert (state_data);
  assert (bytes_rs);
  assert (expected_bytes_rs_len >= 2); /* need atleast cmd and completion code */

  if (bytes_rs_len < expected_bytes_rs_len)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid response length: %u, expected %u\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       bytes_rs_len,
                       expected_bytes_rs_len);
      return (-1);
    }
  
  if (bytes_rs[1] != IPMI_COMP_CODE_COMMAND_SUCCESS)
    {
      char errbuf[IPMI_OEM_ERR_BUFLEN];
      
      memset (errbuf, '\0', IPMI_OEM_ERR_BUFLEN);
      if (ipmi_completion_code_strerror_r (cmd, /* cmd */
                                           netfn, /* network function */
                                           bytes_rs[1], /* completion code */
                                           errbuf,
                                           IPMI_OEM_ERR_BUFLEN) < 0)
        {
          pstdout_perror (state_data->pstate, "ipmi_completion_code_strerror_r");
          snprintf (errbuf, IPMI_OEM_ERR_BUFLEN, "completion-code = 0x%X", bytes_rs[1]);
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s failed: %s\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       errbuf);
      return (-1);
    }

  return (0);
}

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
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (_check_response_and_completion_code (state_data,
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

static int
_inventec_get_nic_status (ipmi_oem_state_data_t *state_data)
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
   * 0x02 - ??
   * 0x01 - ??
   * 0x00 - ??
   * 0x00 - ??
   * 0x00 = ??
   * 0xFF - ??
   * 
   * Get NIC Status Response
   *
   * 0x02 - OEM cmd
   * 0x?? - Completion Code
   * 0x02 - ??
   * 0x01 - ??
   * 0x00 - ??
   * 0x00 - ??
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
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (_check_response_and_completion_code (state_data,
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

static int
_inventec_set_nic_status (ipmi_oem_state_data_t *state_data)
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
   * 0x02 - ??
   * 0x01 - ??
   * 0x00 - ??
   * 0x00 - ??
   * 0x00 = ??
   * 0x01 - ??
   * 0x00 | 0x01 - 0x00 = shared, 0x01 = dedicated
   * 
   * Set NIC Status Response
   *
   * 0x03 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - ??
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
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }

  if (_check_response_and_completion_code (state_data,
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
_inventec_get_mac_address (ipmi_oem_state_data_t *state_data)
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
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
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
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
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
                       fiid_obj_errormsg (obj_cmd_rs));
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

static int
_inventec_set_mac_address (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int32_t rs_len;
  unsigned int tmp;
  uint8_t cmd;
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

  if (sscanf (state_data->prog_data->args->oem_options[1],
              "%02x:%02x:%02x:%02x:%02x:%02x",
              &tmp,
              &tmp,
              &tmp,
              &tmp,
              &tmp,
              &tmp) == 6)
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
                       ipmi_ctx_errormsg (state_data->ipmi_ctx));
      goto cleanup;
    }
  
  if (_check_response_and_completion_code (state_data,
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
_supermicro_reset_intrusion (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int32_t rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Supermicro OEM
   *
   * 0x30 - OEM network function
   * 0x03 - OEM cmd
   */

  bytes_rq[0] = 0x03;

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

  if (_check_response_and_completion_code (state_data,
                                           bytes_rs,
                                           rs_len,
                                           2,
                                           0x03,
                                           0x30) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

static int
_run_oem_cmd (ipmi_oem_state_data_t *state_data)
{
  struct ipmi_oem_arguments *args;
  struct ipmi_oem_id *oem_id = oem_cb;
  int id_found = 0;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  while (oem_id && oem_id->oem_id)
    {
      if (!strcasecmp (oem_id->oem_id, args->oem_id))
        {
          struct ipmi_oem_command *oem_cmd = oem_id->oem_commands;
          int cmd_found = 0;

          id_found++;

          while (oem_cmd && oem_cmd->oem_command)
            {
              if (!strcasecmp (oem_cmd->oem_command,
                               args->oem_command))
                {
                  cmd_found++;

                  if (state_data->prog_data->args->oem_options_count != oem_cmd->required_oem_options)
                    {
                      pstdout_fprintf (state_data->pstate,
                                       stderr,
                                       "%s:%s invalid number of OEM option arguments\n",
                                       state_data->prog_data->args->oem_id,
                                       state_data->prog_data->args->oem_command);
                      goto cleanup;
                    }
                  

                  if (((*oem_cmd->func)(state_data)) < 0)
                    goto cleanup;

                  break;
                }

              oem_cmd++;
            }

          if (!cmd_found)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "OEM Command '%s' unknown\n",
                               args->oem_command);
              goto cleanup;
            }

          break;
        }

      oem_id++;
    }

  if (!id_found)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "OEM Id '%s' unknown\n",
                       args->oem_id);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
run_cmd_args (ipmi_oem_state_data_t *state_data)
{
  struct ipmi_oem_arguments *args;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  /* shouldn't be possible at this point, make sure we've already
   * exitted
   */
  assert (!args->list);

  if (!args->oem_id)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "OEM Id not specified\n");
      goto cleanup;
    }

  if (!args->oem_command)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "OEM Command not specified\n");
      goto cleanup;
    }

  if (_run_oem_cmd (state_data) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_oem (pstdout_state_t pstate,
           const char *hostname,
           void *arg)
{
  ipmi_oem_state_data_t state_data;
  ipmi_oem_prog_data_t *prog_data;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  int exit_code = -1;

  prog_data = (ipmi_oem_prog_data_t *)arg;
  memset (&state_data, '\0', sizeof (ipmi_oem_state_data_t));

  state_data.prog_data = prog_data;
  state_data.pstate = pstate;

  if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
                                         hostname,
                                         &(prog_data->args->common),
                                         errmsg,
                                         IPMI_OPEN_ERRMSGLEN)))
    {
      pstdout_fprintf (pstate,
                       stderr,
                       "%s\n",
                       errmsg);
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (run_cmd_args (&state_data) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = 0;
 cleanup:
  if (state_data.ipmi_ctx)
    {
      ipmi_ctx_close (state_data.ipmi_ctx);
      ipmi_ctx_destroy (state_data.ipmi_ctx);
    }
  return (exit_code);
}

int
main (int argc, char **argv)
{
  ipmi_oem_prog_data_t prog_data;
  struct ipmi_oem_arguments cmd_args;
  int exit_code;
  int rv;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (ipmi_oem_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_oem_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  /* Special case, just output list, don't do anything else */
  if (cmd_args.list)
    {
      if (_list () < 0)
        {
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
      exit_code = EXIT_SUCCESS;
      goto cleanup;
    }

  if (pstdout_setup (&(prog_data.args->common.hostname),
                     prog_data.args->hostrange.buffer_output,
                     prog_data.args->hostrange.consolidate_output,
                     prog_data.args->hostrange.fanout,
                     prog_data.args->hostrange.eliminate,
                     prog_data.args->hostrange.always_prefix) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if ((rv = pstdout_launch (prog_data.args->common.hostname,
                            _ipmi_oem,
                            &prog_data)) < 0)
    {
      fprintf (stderr,
               "pstdout_launch: %s\n",
               pstdout_strerror (pstdout_errnum));
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = rv;
 cleanup:
  return (exit_code);
}
