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

#include "freeipmi-portability.h"
#include "pstdout.h"

int
ipmi_oem_check_response_and_completion_code (ipmi_oem_state_data_t *state_data,
                                             uint8_t *bytes_rs,
                                             unsigned int bytes_rs_len,
                                             unsigned int expected_bytes_rs_len,
                                             uint8_t cmd,
                                             uint8_t netfn)
{
  assert (state_data);
  assert (bytes_rs);

  if (bytes_rs_len < expected_bytes_rs_len)
    {
      if (bytes_rs_len >= 2 && bytes_rs[1] != IPMI_COMP_CODE_COMMAND_SUCCESS)
        goto output_comp_code_error;

      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s:%s invalid response length: %u, expected %u\n",
                       state_data->prog_data->args->oem_id,
                       state_data->prog_data->args->oem_command,
                       bytes_rs_len,
                       expected_bytes_rs_len);
      return (-1);
    }

 output_comp_code_error:  
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
