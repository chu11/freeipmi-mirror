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
