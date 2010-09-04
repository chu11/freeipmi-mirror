/*
 * Copyright (C) 2008-2010 FreeIPMI Core Team
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
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-oem.h"
#include "ipmi-oem-argp.h"
#include "ipmi-oem-common.h"
#include "ipmi-oem-intel.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-oem-common.h"
#include "tool-sdr-cache-common.h"
#include "tool-sensor-common.h"

/* Intel S5500WB/Penguin Computing Relion 700
 *
 * Set Fault Indication
 *
 * "Satellite controllers on the IPMB or software can indicate to the
 * BMC that they have detectd a fault assertion or de-assertion using
 * this command.  The BMC generates a composite fault state from this
 * information and its own internal fault state, and uses this to
 * drive front panel indicators.  The fault indication state
 * contributes to the system status state, as reported by the fron
 * tpanel system status. ... This command also supports additional
 * parameters to control BMC-owned component fault LEDs associated
 * with teh specified fault condition."
 * 
 * Set Fault Indication Request
 *
 * 0x30 - OEM network function
 * 0x57 - OEM cmd
 * 0x?? - source id
 *      - 0x00 - unspecified source
 *      - 0x01 - hot-swap controller 0
 *      - 0x02 - hot-swap controller 1
 *      - 0x03 - bios
 * 0x?? - fault type
 *      - 0x00 - fan
 *      - 0x01 - temp
 *      - 0x02 - power
 *      - 0x03 - drive slot
 *      - 0x04 - software
 *      - 0x05 - memory
 * 0x?? - state to set
 *      - 0x00 - ok
 *      - 0x01 - degraded
 *      - 0x02 - non-critical
 *      - 0x03 - critical
 *      - 0x04 - non-recoverable
 * 0x?? - component fault LED group ID
 *      - 0xFF - if not used
 * 0x?? - LED state
 *      - LSbyte first, 1 bit will turn on LED.
 *
 * Set Fault Indication Response
 *
 * 0x57 - OEM cmd
 * 0x?? - Completion Code
 */

#define IPMI_OEM_INTEL_RESTORE_CONFIGURATION_OPERATION_INITIATE_RESTORE   0xAA
#define IPMI_OEM_INTEL_RESTORE_CONFIGURATION_OPERATION_GET_RESTORE_STATUS 0x00

#define IPMI_OEM_INTEL_RESTORE_CONFIGURATION_RESTORE_PROGRESS_RESTORE_IN_PROGRESS 0x00
#define IPMI_OEM_INTEL_RESTORE_CONFIGURATION_RESTORE_PROGRESS_RESTORE_COMPLETED   0x01

int
ipmi_oem_intel_restore_configuration (ipmi_oem_state_data_t *state_data)
{
  uint8_t bytes_rq[IPMI_OEM_MAX_BYTES];
  uint8_t bytes_rs[IPMI_OEM_MAX_BYTES];
  int rs_len;
  int rv = -1;

  assert (state_data);
  assert (!state_data->prog_data->args->oem_options_count);

  /* Intel S5500WB/Penguin Computing Relion 700
   *
   * Restore Configuration Request
   *
   * 0x30 - OEM network function
   * 0x02 - OEM cmd
   * 0x43 - 'C'
   * 0x4C - 'L'
   * 0x52 - 'R'
   * 0x?? - Operation
   *      - 0x00 - restore status
   *      - 0xAA - initiate restore
   * 
   * Restore Configuration Response
   *
   * 0x02 - OEM cmd
   * 0x?? - Completion Code
   * 0x?? - Restore progress
   *      - 0x00 - restore in progress
   *      - 0x01 - restore completed
   */

  bytes_rq[0] = IPMI_CMD_OEM_INTEL_RESTORE_CONFIGURATION;
  bytes_rq[1] = 'C';
  bytes_rq[2] = 'L';
  bytes_rq[3] = 'R';
  bytes_rq[4] = IPMI_OEM_INTEL_RESTORE_CONFIGURATION_OPERATION_INITIATE_RESTORE;
   
  if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                              0, /* lun */
                              IPMI_NET_FN_OEM_INTEL_GENERIC_RQ, /* network function */
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
                                                   3,
                                                   IPMI_CMD_OEM_INTEL_RESTORE_CONFIGURATION,
                                                   IPMI_NET_FN_OEM_INTEL_GENERIC_RS,
                                                   NULL) < 0)
    goto cleanup;
  
  /* don't quit until it is done */
  while (1)
    {
      bytes_rq[4] = IPMI_OEM_INTEL_RESTORE_CONFIGURATION_OPERATION_GET_RESTORE_STATUS;
      
      if ((rs_len = ipmi_cmd_raw (state_data->ipmi_ctx,
                                  0, /* lun */
                                  IPMI_NET_FN_OEM_INTEL_GENERIC_RQ, /* network function */
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
                                                       3,
						       IPMI_CMD_OEM_INTEL_RESTORE_CONFIGURATION,
						       IPMI_NET_FN_OEM_INTEL_GENERIC_RS,
                                                       NULL) < 0)
        goto cleanup;
      
      if (bytes_rs[2] == IPMI_OEM_INTEL_RESTORE_CONFIGURATION_RESTORE_PROGRESS_RESTORE_COMPLETED)
        break;

      sleep (1);
    }
  
  rv = 0;
 cleanup:
  return (rv);
}
