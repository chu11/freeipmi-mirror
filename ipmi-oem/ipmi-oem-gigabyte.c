/*
 * Copyright (C) 2008-2015 FreeIPMI Core Team
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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <limits.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"
#include "ipmi-oem-gigabyte.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

#define IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_GET_NIC_MODE_BYTE_0 0x5E
#define IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_GET_NIC_MODE_BYTE_1 0x2B
#define IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_GET_NIC_MODE_BYTE_2 0x00
#define IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_GET_NIC_MODE_BYTE_3 0x0C
#define IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_GET_NIC_MODE_BYTE_4 0x02

#define IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_SET_NIC_MODE_BYTE_0 0x5E
#define IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_SET_NIC_MODE_BYTE_1 0x2B
#define IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_SET_NIC_MODE_BYTE_2 0x00
#define IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_SET_NIC_MODE_BYTE_3 0x0C
#define IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_SET_NIC_MODE_BYTE_4 0x01

#define IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_SET_BMC_SERVICES_BYTE_0 0x0A
#define IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_SET_BMC_SERVICES_BYTE_1 0x3C
#define IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_SET_BMC_SERVICES_BYTE_2 0x00
#define IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_SET_BMC_SERVICES_BYTE_3 0x15

#define IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_GET_BMC_SERVICES_BYTE_0 0x0A
#define IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_GET_BMC_SERVICES_BYTE_1 0x3C
#define IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_GET_BMC_SERVICES_BYTE_2 0x00
#define IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_GET_BMC_SERVICES_BYTE_3 0x15

#define IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_SSH        22
#define IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_HTTP       80
#define IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_RPCBIND    111
#define IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_SVRLOC     427
#define IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_HTTPS      443
#define IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_AVOCENTKVM 2068
#define IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_WBEM_HTTP  5988
#define IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_WBEM_HTTPS 5989

int
ipmi_oem_gigabyte_get_nic_mode (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* From Gigabyte Provided Information
   *
   * GIGABYTE MD90-FS0-ZB
   *
   * Get NIC Mode Request
   *
   * 0x2E - OEM network function (IPMI_NET_FN_OEM_GROUP_RQ)
   * 0xCC - OEM cmd
   * 0x5E - ??? - no idea, given by vendor
   * 0x2B - ??? - no idea, given by vendor
   * 0x00 - ??? - no idea, given by vendor
   * 0x0C - ??? - no idea, given by vendor
   * 0x02 - ??? - no idea, given by vendor
   *
   * Set NIC Mode Response
   *
   * 0xCC - OEM cmd
   * 0x?? - Completion Code
   * 0x5E - ??? - no idea, given by vendor
   * 0x2B - ??? - no idea, given by vendor
   * 0x00 - ??? - no idea, given by vendor
   * 0x0C - ??? - no idea, given by vendor
   * 0x02 - ??? - no idea, given by vendor
   * 0x?? - 0x01 - dedicated
   *      - 0x02 - shared
   *      - 0x03 - failover
   */

  bytes_rq[0] = IPMI_CMD_OEM_GIGABYTE_CONFIGURATION;
  bytes_rq[1] = IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_GET_NIC_MODE_BYTE_0;
  bytes_rq[2] = IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_GET_NIC_MODE_BYTE_1;
  bytes_rq[3] = IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_GET_NIC_MODE_BYTE_2;
  bytes_rq[4] = IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_GET_NIC_MODE_BYTE_3;
  bytes_rq[5] = IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_GET_NIC_MODE_BYTE_4;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
                              bytes_rq, /* data */
                              6, /* num bytes */
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
                                                   IPMI_CMD_OEM_GIGABYTE_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_GROUP_RS,
                                                   NULL) < 0)
    goto cleanup;

  switch (bytes_rs[7])
    {
    case IPMI_OEM_GIGABYTE_NIC_MODE_DEDICATED:
      pstdout_printf (state_data->pstate, "dedicated\n");
      break;
    case IPMI_OEM_GIGABYTE_NIC_MODE_SHARED:
      pstdout_printf (state_data->pstate, "shared\n");
      break;
    case IPMI_OEM_GIGABYTE_NIC_MODE_FAILOVER:
      pstdout_printf (state_data->pstate, "failover\n");
      break;
    default:
      pstdout_printf (state_data->pstate, "unknown NIC selection: %Xh\n", bytes_rs[7]);
      break;
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_gigabyte_set_nic_mode (ipmi_oem_state_data_t *state_data)
{
  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 1);

  if (strcasecmp (state_data->prog_data->args->oem_options[0], "dedicated")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "shared")
      && strcasecmp (state_data->prog_data->args->oem_options[0], "failover"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[0]);
      goto cleanup;
    }

  /* From Gigabyte Provided Information
   *
   * GIGABYTE MD90-FS0-ZB
   *
   * Set NIC Mode Request
   *
   * 0x2E - OEM network function (IPMI_NET_FN_OEM_GROUP_RQ)
   * 0xCC - OEM cmd
   * 0x5E - ??? - no idea, given by vendor
   * 0x2B - ??? - no idea, given by vendor
   * 0x00 - ??? - no idea, given by vendor
   * 0x0C - ??? - no idea, given by vendor
   * 0x01 - ??? - no idea, given by vendor
   * 0x?? - 0x01 - dedicated
   *      - 0x02 - shared
   *      - 0x03 - failover
   *
   * Set NIC Mode Response
   *
   * 0xCC - OEM cmd
   * 0x?? - Completion Code
   */

  bytes_rq[0] = IPMI_CMD_OEM_GIGABYTE_CONFIGURATION;
  bytes_rq[1] = IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_SET_NIC_MODE_BYTE_0;
  bytes_rq[2] = IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_SET_NIC_MODE_BYTE_1;
  bytes_rq[3] = IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_SET_NIC_MODE_BYTE_2;
  bytes_rq[4] = IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_SET_NIC_MODE_BYTE_3;
  bytes_rq[5] = IPMI_CMD_OEM_GIGABYTE_CONFIGURATION_SET_NIC_MODE_BYTE_4;
  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "dedicated"))
    bytes_rq[6] = IPMI_OEM_GIGABYTE_NIC_MODE_DEDICATED;
  else if (!strcasecmp (state_data->prog_data->args->oem_options[0], "shared"))
    bytes_rq[6] = IPMI_OEM_GIGABYTE_NIC_MODE_SHARED;
  else /* !strcasecmp (state_data->prog_data->args->oem_options[0], "failover") */
    bytes_rq[6] = IPMI_OEM_GIGABYTE_NIC_MODE_FAILOVER;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
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
                                                   2,
                                                   IPMI_CMD_OEM_GIGABYTE_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_GROUP_RS,
                                                   NULL) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_gigabyte_get_bmc_services (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;
  int ports[] = { IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_SSH,
                  IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_HTTP,
                  IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_RPCBIND,
                  IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_SVRLOC,
                  IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_HTTPS,
                  IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_AVOCENTKVM,
                  IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_WBEM_HTTP,
                  IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_WBEM_HTTPS,
                  0 };
  char *portsstr[] = { "ssh",
                       "http",
                       "rpcbind",
                       "svrloc",
                       "https",
                       "avocentkvm",
                       "wbem-http",
                       "wbem-https",
                       NULL };
  int i;

  assert (state_data);
  assert (state_data->prog_data->args->oem_options_count == 0);

  /* From Gigabyte Provided Information
   *
   * GIGABYTE MD90-FS0-ZB
   *
   * Request
   *
   * 0x2E - OEM network function
   * 0x21 - OEM cmd
   * 0x0A - ??? - no idea, given by vendor
   * 0x3C - ??? - no idea, given by vendor
   * 0x00 - ??? - no idea, given by vendor
   * 0x15 - ??? - no idea, given by vendor
   * 0x?? - lsb port number to block/unblock
   * 0x?? - msb port number to block/unblock
   *
   * Response
   *
   * 0x21 - OEM cmd
   * 0x?? - Completion Code
   * 0x0A - ??? - no idea, given by vendor
   * 0x3C - ??? - no idea, given by vendor
   * 0x00 - ??? - no idea, given by vendor
   * 0x?? - enabled/disabled
   *      - 0 - disabled
   *      - 1 - enabled
   */

  bytes_rq[0] = IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION2;
  bytes_rq[1] = IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_GET_BMC_SERVICES_BYTE_0;
  bytes_rq[2] = IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_GET_BMC_SERVICES_BYTE_1;
  bytes_rq[3] = IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_GET_BMC_SERVICES_BYTE_2;
  bytes_rq[4] = IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_GET_BMC_SERVICES_BYTE_3;

  i = 0;
  while (ports[i])
    {
      char *outputstr = NULL;

      bytes_rq[5] = (ports[i] & 0x00FF);
      bytes_rq[6] = (ports[i] & 0xFF00) >> 8;

      if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                                  0, /* lun */
                                  IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
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
                                                       6,
                                                       IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION2,
                                                       IPMI_NET_FN_OEM_GROUP_RS,
                                                       NULL) < 0)
        goto cleanup;

      if (bytes_rs[5] == IPMI_OEM_GIGABYTE_PORT_ENABLED)
        outputstr = "enabled";
      else if (bytes_rs[5] == IPMI_OEM_GIGABYTE_PORT_DISABLED)
        outputstr = "disabled";
      else
        outputstr = "unknown";

      pstdout_printf (state_data->pstate,
                      "%s: %s\n", portsstr[i], outputstr);
      i++;
    }

  rv = 0;
 cleanup:
  return (rv);
}

int
ipmi_oem_gigabyte_set_bmc_services (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
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

  if (strcasecmp (state_data->prog_data->args->oem_options[1], "ssh")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "http")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "rpcbind")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "svrloc")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "https")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "avocentkvm")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "wbem-http")
      && strcasecmp (state_data->prog_data->args->oem_options[1], "wbem-https"))
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid OEM option argument '%s'\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       state_data->prog_data->args->oem_options[1]);
      goto cleanup;
    }

  /* From Gigabyte Provided Information
   *
   * GIGABYTE MD90-FS0-ZB
   *
   * Request
   *
   * 0x2E - OEM network function
   * 0x20 - OEM cmd
   * 0x0A - ??? - no idea, given by vendor
   * 0x3C - ??? - no idea, given by vendor
   * 0x00 - ??? - no idea, given by vendor
   * 0x15 - ??? - no idea, given by vendor
   * 0x?? - lsb port number to block/unblock
   * 0x?? - msb port number to block/unblock
   * 0x?? - block/unblock
   *      - 0 - block
   *      - 1 - unblock
   *
   * Response
   *
   * 0x20 - OEM cmd
   * 0x?? - Completion Code
   */

  bytes_rq[0] = IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION;
  bytes_rq[1] = IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_SET_BMC_SERVICES_BYTE_0;
  bytes_rq[2] = IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_SET_BMC_SERVICES_BYTE_1;
  bytes_rq[3] = IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_SET_BMC_SERVICES_BYTE_2;
  bytes_rq[4] = IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_SET_BMC_SERVICES_BYTE_3;

  if (!strcasecmp (state_data->prog_data->args->oem_options[1], "ssh"))
    {
      bytes_rq[5] = (IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_SSH & 0x00FF);
      bytes_rq[6] = (IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_SSH & 0xFF00) >> 8;
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "http"))
    {
      bytes_rq[5] = (IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_HTTP & 0x00FF);
      bytes_rq[6] = (IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_HTTP & 0xFF00) >> 8;
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "rpcbind"))
    {
      bytes_rq[5] = (IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_RPCBIND & 0x00FF);
      bytes_rq[6] = (IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_RPCBIND & 0xFF00) >> 8;
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "svrloc"))
    {
      bytes_rq[5] = (IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_SVRLOC & 0x00FF);
      bytes_rq[6] = (IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_SVRLOC & 0xFF00) >> 8;
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "https"))
    {
      bytes_rq[5] = (IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_HTTPS & 0x00FF);
      bytes_rq[6] = (IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_HTTPS & 0xFF00) >> 8;
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "avocentkvm"))
    {
      bytes_rq[5] = (IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_AVOCENTKVM & 0x00FF);
      bytes_rq[6] = (IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_AVOCENTKVM & 0xFF00) >> 8;
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "wbem-http"))
    {
      bytes_rq[5] = (IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_WBEM_HTTP & 0x00FF);
      bytes_rq[6] = (IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_WBEM_HTTP & 0xFF00) >> 8;
    }
  else if (!strcasecmp (state_data->prog_data->args->oem_options[1], "wbem-https"))
    {
      bytes_rq[5] = (IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_WBEM_HTTPS & 0x00FF);
      bytes_rq[6] = (IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION_WBEM_HTTPS & 0xFF00) >> 8;
    }

  if (!strcasecmp (state_data->prog_data->args->oem_options[0], "enable"))
    bytes_rq[7] = IPMI_OEM_GIGABYTE_UNBLOCK_PORT;
  else
    bytes_rq[7] = IPMI_OEM_GIGABYTE_BLOCK_PORT;

  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_GROUP_RQ, /* network function */
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
                                                   2,
                                                   IPMI_CMD_OEM_GIGABYTE_PORT_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_GROUP_RS,
                                                   NULL) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}
