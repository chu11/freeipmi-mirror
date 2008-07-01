/*
  Copyright (C) 2008 FreeIPMI Core Team

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
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "bmc-device.h"
#include "bmc-device-argp.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-fiid-wrappers.h"
#include "tool-hostrange-common.h"

static int
cold_reset (bmc_device_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  int rv = -1;

  assert(state_data);

  _FIID_OBJ_CREATE(cmd_rs, tmpl_cmd_cold_reset_rs);

  if (ipmi_cmd_cold_reset (state_data->ipmi_ctx, cmd_rs) != 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_cold_reset: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }
  
  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(cmd_rs);
  return rv;
}

static int
warm_reset (bmc_device_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  int rv = -1;

  assert(state_data);

  _FIID_OBJ_CREATE(cmd_rs, tmpl_cmd_warm_reset_rs);

  if (ipmi_cmd_warm_reset (state_data->ipmi_ctx, cmd_rs) != 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_warm_reset: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }
  
  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(cmd_rs);
  return rv;
}

static int
get_self_test_results (bmc_device_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  uint64_t val;
  int rv = -1;

  assert(state_data);

  _FIID_OBJ_CREATE(cmd_rs, tmpl_cmd_get_self_test_results_rs);

  if (ipmi_cmd_get_self_test_results (state_data->ipmi_ctx, cmd_rs) != 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_get_self_test_results: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }
  
  _FIID_OBJ_GET (cmd_rs,
                 "self_test_result",
                 &val);
  
  pstdout_printf(state_data->pstate,
                 "Self Test Result: ");
  if (val == IPMI_SELF_TEST_RESULT_NO_ERROR)
    pstdout_printf(state_data->pstate,
                   "No Error\n");
  else if (val == IPMI_SELF_TEST_RESULT_SELF_TEST_FUNCTION_NOT_IMPLEMENTED_IN_THIS_CONTROLLER)
    pstdout_printf(state_data->pstate,
                   "Self Test function not implemented in this controller.\n");
  else if (val == IPMI_SELF_TEST_RESULT_CORRUPTED_OR_INACCESSIBLE_DATA_OR_DEVICES)
    pstdout_printf(state_data->pstate,
                   "Corrupted or inaccessible data or devices\n");
  else if (val == IPMI_SELF_TEST_RESULT_FATAL_HARDWARE_ERROR)
    pstdout_printf(state_data->pstate,
                   "Fatal hardware error (system should consider BMC inoperative).  Controller hardware may need to be repaired or replaced.\n");
  else
    pstdout_printf(state_data->pstate,
                   "Device-specific error: 0x%X\n",
                   val);

  if (val == IPMI_SELF_TEST_RESULT_CORRUPTED_OR_INACCESSIBLE_DATA_OR_DEVICES)
    {
      _FIID_OBJ_GET (cmd_rs, 
                     "controller_operation_firmware_corrupted",
                     &val);
      if (val)
        pstdout_printf(state_data->pstate,
                       "                  [Controller operation firmware corrupted]\n");
                      
      _FIID_OBJ_GET (cmd_rs, 
                     "controller_update_boot_block_firmware_corrupted",
                     &val);
      if (val)
        pstdout_printf(state_data->pstate,
                       "                  [Controller update 'boot block' firmware corrupted]\n");

      _FIID_OBJ_GET (cmd_rs, 
                     "internal_use_area_of_bmc_fru_corrupted",
                     &val);
      if (val)
        pstdout_printf(state_data->pstate,
                       "                  [Internal Use Area of BMC FRU corrupted]\n");

      _FIID_OBJ_GET (cmd_rs, 
                     "sdr_repository_empty",
                     &val);
      if (val)
        pstdout_printf(state_data->pstate,
                       "                  [SDR Repository empty]\n");

      _FIID_OBJ_GET (cmd_rs, 
                     "ipmb_signal_lines_do_not_respond",
                     &val);
      if (val)
        pstdout_printf(state_data->pstate,
                       "                  [IPMB signal lines do not respond]\n");

      _FIID_OBJ_GET (cmd_rs, 
                     "cannot_access_bmc_fru_device",
                     &val);
      if (val)
        pstdout_printf(state_data->pstate,
                       "                  [Cannot access BMC FRU device]\n");

      _FIID_OBJ_GET (cmd_rs, 
                     "cannot_access_sdr_repository",
                     &val);
      if (val)
        pstdout_printf(state_data->pstate,
                       "                  [Cannot access SDR Repository]\n");

      _FIID_OBJ_GET (cmd_rs, 
                     "cannot_access_sel_device",
                     &val);
      if (val)
        pstdout_printf(state_data->pstate,
                       "                  [Cannot access SEL device]\n");

    }

  rv = 0;
 cleanup:
  _FIID_OBJ_DESTROY(cmd_rs);
  return rv;
}

int
run_cmd_args (bmc_device_state_data_t *state_data)
{
  struct bmc_device_arguments *args;
  int rv = -1;

  assert(state_data);
  
  args = state_data->prog_data->args;

  if (args->cold_reset)
    return cold_reset (state_data);

  if (args->warm_reset)
    return warm_reset (state_data);

  if (args->get_self_test_results)
    return get_self_test_results (state_data);

  rv = 0;
  return (rv);
}

static int
_bmc_device(pstdout_state_t pstate,
          const char *hostname,
          void *arg)
{
  bmc_device_state_data_t state_data;
  bmc_device_prog_data_t *prog_data;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  int exit_code = -1;

  prog_data = (bmc_device_prog_data_t *)arg;
  memset(&state_data, '\0', sizeof(bmc_device_state_data_t));

  state_data.prog_data = prog_data;
  state_data.pstate = pstate;
  
  if (!(state_data.ipmi_ctx = ipmi_open(prog_data->progname,
                                        hostname,
                                        &(prog_data->args->common),
                                        errmsg,
                                        IPMI_OPEN_ERRMSGLEN)))
    {
      pstdout_fprintf(pstate,
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
  return exit_code;
}

int 
main (int argc, char **argv)
{
  bmc_device_prog_data_t prog_data;
  struct bmc_device_arguments cmd_args;
  int exit_code;
  int rv;
  
  ipmi_disable_coredump();
  
  memset(&prog_data, '\0', sizeof(bmc_device_prog_data_t));
  prog_data.progname = argv[0];
  bmc_device_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  if (pstdout_setup(&(prog_data.args->common.hostname),
                    prog_data.args->hostrange.buffer_output,
                    prog_data.args->hostrange.consolidate_output,
                    prog_data.args->hostrange.fanout,
                    prog_data.args->hostrange.eliminate,
                    prog_data.args->hostrange.always_prefix) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if ((rv = pstdout_launch(prog_data.args->common.hostname,
                           _bmc_device,
                           &prog_data)) < 0)
    {
      fprintf(stderr, 
              "pstdout_launch: %s\n",
              pstdout_strerror(pstdout_errnum));
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = rv;
 cleanup:
  return (exit_code);
}
