/*****************************************************************************\
 *  $Id: ipmi-fru.c,v 1.59 2010-02-08 22:20:58 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2011 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-232183
 *
 *  This file is part of Ipmi-fru, a tool used for retrieving
 *  motherboard field replaceable unit (FRU) information. For details,
 *  see http://www.llnl.gov/linux/.
 *
 *  Ipmi-fru is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-fru is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-fru.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

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

#include "ipmi-fru.h"
#include "ipmi-fru-argp.h"
#include "ipmi-fru-output.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"
#include "tool-oem-common.h"
#include "tool-sdr-cache-common.h"

#define IPMI_FRU_DEFAULT_DEVICE_ID_STRING "Default FRU Device"

static int
_flush_cache (ipmi_fru_state_data_t *state_data)
{
  assert (state_data);

  if (sdr_cache_flush_cache (state_data->sdr_cache_ctx,
                             state_data->pstate,
                             state_data->prog_data->args->sdr.quiet_cache,
                             state_data->hostname,
                             state_data->prog_data->args->sdr.sdr_cache_directory,
                             state_data->prog_data->args->sdr.sdr_cache_file) < 0)
    return (-1);

  return (0);
}

static int
_output_fru (ipmi_fru_state_data_t *state_data,
             unsigned int *output_count,
             uint8_t device_id,
             const char *device_id_str)
{
  int ret = 0;
  int rv = -1;

  assert (state_data);
  assert (output_count);
  assert (device_id_str);

  if ((*output_count))
    pstdout_printf (state_data->pstate, "\n");
  (*output_count)++;

  pstdout_printf (state_data->pstate,
                  "FRU Inventory Device: %s (ID %02Xh)\n",
                  device_id_str,
                  device_id);

  if (ipmi_fru_parse_open_device_id (state_data->fru_parse_ctx, device_id) < 0)
    {
      if (IPMI_FRU_PARSE_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_parse_ctx))
        {
          /* Special case, not really an "error" */
          if (ipmi_fru_parse_ctx_errnum (state_data->fru_parse_ctx) != IPMI_FRU_PARSE_ERR_NO_FRU_INFORMATION)
            {
              pstdout_printf (state_data->pstate, "\n");
              pstdout_printf (state_data->pstate,
                              "  FRU Error: %s\n",
                              ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
            }
          goto out;
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_parse_open_device_id: %s\n",
                       ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
      goto cleanup;
    }

  if (ipmi_fru_parse_first (state_data->fru_parse_ctx) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_parse_first: %s\n",
                       ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
      goto cleanup;
    }

  do 
    {
      uint8_t areabuf[IPMI_FRU_PARSE_AREA_SIZE_MAX+1];
      unsigned int area_type = 0;
      unsigned int area_length = 0;
      
      memset (areabuf, '\0', IPMI_FRU_PARSE_AREA_SIZE_MAX + 1);
      if (ipmi_fru_parse_read_data_area (state_data->fru_parse_ctx,
                                         &area_type,
                                         &area_length,
                                         areabuf,
                                         IPMI_FRU_PARSE_AREA_SIZE_MAX) < 0)
        {
          if (IPMI_FRU_PARSE_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_parse_ctx))
            {
              /* Special case, not really an "error" */
              if (ipmi_fru_parse_ctx_errnum (state_data->fru_parse_ctx) != IPMI_FRU_PARSE_ERR_NO_FRU_INFORMATION)
                {
                  pstdout_printf (state_data->pstate, "\n");
                  pstdout_printf (state_data->pstate,
                                  "  FRU Error: %s\n",
                                  ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
                }
              goto next;
            }
          
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_fru_parse_open_device_id: %s\n",
                           ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
          goto cleanup;
        }

      if (area_length)
        {
          pstdout_printf (state_data->pstate, "\n");

          switch (area_type)
            {
            case IPMI_FRU_PARSE_AREA_TYPE_CHASSIS_INFO_AREA:
              if (ipmi_fru_output_chassis_info_area (state_data,
                                                     areabuf,
                                                     area_length) < 0)
                goto cleanup;
              break;
            case IPMI_FRU_PARSE_AREA_TYPE_BOARD_INFO_AREA:
              if (ipmi_fru_output_board_info_area (state_data,
                                                   areabuf,
                                                   area_length) < 0)
                goto cleanup;
              break;
            case IPMI_FRU_PARSE_AREA_TYPE_PRODUCT_INFO_AREA:
              if (ipmi_fru_output_product_info_area (state_data,
                                                     areabuf,
                                                     area_length) < 0)
                goto cleanup;
              break;
            case IPMI_FRU_PARSE_AREA_TYPE_MULTIRECORD_POWER_SUPPLY_INFORMATION:
              if (ipmi_fru_output_power_supply_information (state_data,
                                                            areabuf,
                                                            area_length) < 0)
                goto cleanup;
              break;
            case IPMI_FRU_PARSE_AREA_TYPE_MULTIRECORD_DC_OUTPUT:
              if (ipmi_fru_output_dc_output (state_data,
                                             areabuf,
                                             area_length) < 0)
                goto cleanup;
              break;
            case IPMI_FRU_PARSE_AREA_TYPE_MULTIRECORD_DC_LOAD:
              if (ipmi_fru_output_dc_load (state_data,
                                           areabuf,
                                           area_length) < 0)
                goto cleanup;
              break;
            case IPMI_FRU_PARSE_AREA_TYPE_MULTIRECORD_MANAGEMENT_ACCESS_RECORD:
              if (ipmi_fru_output_management_access_record (state_data,
                                                            areabuf,
                                                            area_length) < 0)
                goto cleanup;
              break;
            case IPMI_FRU_PARSE_AREA_TYPE_MULTIRECORD_BASE_COMPATABILITY_RECORD:
              if (ipmi_fru_output_base_compatibility_record (state_data,
                                                             areabuf,
                                                             area_length) < 0)
                goto cleanup;
              break;
            case IPMI_FRU_PARSE_AREA_TYPE_MULTIRECORD_EXTENDED_COMPATABILITY_RECORD:
              if (ipmi_fru_output_extended_compatibility_record (state_data,
                                                                 areabuf,
                                                                 area_length) < 0)
                goto cleanup;
              break;
            case IPMI_FRU_PARSE_AREA_TYPE_MULTIRECORD_OEM:
              if (ipmi_fru_output_oem_record (state_data,
                                              areabuf,
                                              area_length) < 0)
                goto cleanup;
              break;
            default:
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "  FRU Error: Unknown FRU Area Type Read: %02Xh\n",
                               area_type);
              goto next;
              break;
            }
        }

    next:
      ;
    } while ((ret = ipmi_fru_parse_next (state_data->fru_parse_ctx)) == 1);
    
  if (ret < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_parse_next: %s\n",
                       ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
      goto cleanup;
    }

 out:
  rv = 0;
 cleanup:
  ipmi_fru_parse_close_device_id (state_data->fru_parse_ctx);
  return (rv);
}

static int
run_cmd_args (ipmi_fru_state_data_t *state_data)
{
  struct ipmi_fru_arguments *args;
  uint16_t record_count;
  unsigned int output_count = 0;
  unsigned int i;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  if (args->sdr.flush_cache)
    return (_flush_cache (state_data));

  if (args->sdr.ignore_sdr_cache)
    {
      /* no SDR?  This is all you get :-) */
      if (_output_fru (state_data,
                       &output_count,
                       IPMI_FRU_DEVICE_ID_DEFAULT,
                       IPMI_FRU_DEFAULT_DEVICE_ID_STRING) < 0)
        goto cleanup;
      return (0);
    }

  if (!args->sdr.ignore_sdr_cache)
    {
      if (sdr_cache_create_and_load (state_data->sdr_cache_ctx,
                                     state_data->pstate,
                                     state_data->ipmi_ctx,
                                     args->sdr.quiet_cache,
                                     args->sdr.sdr_cache_recreate,
                                     state_data->hostname,
                                     args->sdr.sdr_cache_directory,
                                     args->sdr.sdr_cache_file) < 0)
        goto cleanup;

      if (ipmi_sdr_cache_record_count (state_data->sdr_cache_ctx, &record_count) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_sdr_cache_record_count: %s\n",
                           ipmi_sdr_cache_ctx_errormsg (state_data->sdr_cache_ctx));
          goto cleanup;
        }
    }

  if (args->interpret_oem_data)
    {
      if (ipmi_get_oem_data (state_data->pstate,
                             state_data->ipmi_ctx,
                             &state_data->oem_data) < 0)
        goto cleanup;

      if (ipmi_fru_parse_ctx_set_manufacturer_id (state_data->fru_parse_ctx,
                                                  state_data->oem_data.manufacturer_id) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_fru_parse_ctx_set_manufacturer_id: %s\n",
                           ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
          goto cleanup;
        }

      if (ipmi_fru_parse_ctx_set_product_id (state_data->fru_parse_ctx,
                                             state_data->oem_data.product_id) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_fru_parse_ctx_set_product_id: %s\n",
                           ipmi_fru_parse_ctx_errormsg (state_data->fru_parse_ctx));
          goto cleanup;
        }
    }

  if (args->device_id_set && args->device_id == IPMI_FRU_DEVICE_ID_DEFAULT)
    {
      if (_output_fru (state_data,
                       &output_count,
                       IPMI_FRU_DEVICE_ID_DEFAULT,
                       IPMI_FRU_DEFAULT_DEVICE_ID_STRING) < 0)
        goto cleanup;
    }
  else
    {
      int found = 0;

      if (!args->device_id_set)
        {
          if (_output_fru (state_data,
                           &output_count,
                           IPMI_FRU_DEVICE_ID_DEFAULT,
                           IPMI_FRU_DEFAULT_DEVICE_ID_STRING) < 0)
            goto cleanup;
        }

      for (i = 0; i < record_count; i++, ipmi_sdr_cache_next (state_data->sdr_cache_ctx))
        {
          uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
          uint8_t record_type, logical_physical_fru_device, logical_fru_device_device_slave_address;
          int sdr_record_len;

          memset (sdr_record, '\0', IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH);
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
                                                 NULL,
                                                 &record_type) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sdr_parse_record_id_and_type: %s\n",
                               ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
              goto cleanup;
            }

          if (record_type != IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD)
            continue;

          if (ipmi_sdr_parse_fru_device_locator_parameters (state_data->sdr_parse_ctx,
                                                            sdr_record,
                                                            sdr_record_len,
                                                            NULL,
                                                            &logical_fru_device_device_slave_address,
                                                            NULL,
                                                            NULL,
                                                            &logical_physical_fru_device,
                                                            NULL) < 0)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "ipmi_sdr_parse_fru_device_locator_parameters: %s\n",
                               ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
              goto cleanup;
            }

          if (logical_physical_fru_device
              && ((args->device_id_set && logical_fru_device_device_slave_address == args->device_id)
                  || (!args->device_id_set && logical_fru_device_device_slave_address != IPMI_FRU_DEVICE_ID_DEFAULT)))
            {
              char device_id_string[IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING+1];

              memset (device_id_string, '\0', IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING+1);
              if (ipmi_sdr_parse_device_id_string (state_data->sdr_parse_ctx,
                                                   sdr_record,
                                                   sdr_record_len,
                                                   device_id_string,
                                                   IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING) < 0)
                {
                  pstdout_fprintf (state_data->pstate,
                                   stderr,
                                   "ipmi_sdr_parse_device_id_string: %s\n",
                                   ipmi_sdr_parse_ctx_errormsg (state_data->sdr_parse_ctx));
                  goto cleanup;
                }

              if (_output_fru (state_data,
                               &output_count,
                               logical_fru_device_device_slave_address,
                               device_id_string) < 0)
                goto cleanup;
              found++;
            }
        }

      if (args->device_id_set && !found)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "device id not found\n");
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
_ipmi_fru (pstdout_state_t pstate,
           const char *hostname,
           void *arg)
{
  ipmi_fru_state_data_t state_data;
  ipmi_fru_prog_data_t *prog_data;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  int exit_code = -1;

  prog_data = (ipmi_fru_prog_data_t *)arg;
  memset (&state_data, '\0', sizeof (ipmi_fru_state_data_t));

  state_data.prog_data = prog_data;
  state_data.pstate = pstate;
  state_data.hostname = (char *)hostname;

  /* Special case, just flush, don't do an IPMI connection */
  if (!prog_data->args->sdr.flush_cache)
    {
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
    }

  if (!prog_data->args->sdr.flush_cache)
    {
      unsigned int flags = 0;

      if (!(state_data.fru_parse_ctx = ipmi_fru_parse_ctx_create (state_data.ipmi_ctx)))
        {
          pstdout_perror (pstate, "ipmi_fru_parse_ctx_create()");
          exit_code = EXIT_FAILURE;
          goto cleanup;
        }
      
      if (state_data.prog_data->args->common.debug)
        flags |= IPMI_FRU_PARSE_FLAGS_DEBUG_DUMP;
      if (state_data.prog_data->args->skip_checks)
        flags |= IPMI_FRU_PARSE_FLAGS_SKIP_CHECKSUM_CHECKS;
      if (state_data.prog_data->args->interpret_oem_data)
        flags |= IPMI_FRU_PARSE_FLAGS_INTERPRET_OEM_DATA;

      if (hostname)
        {
          if (ipmi_fru_parse_ctx_set_debug_prefix (state_data.fru_parse_ctx,
                                                   hostname) < 0)
            pstdout_fprintf (pstate,
                             stderr,
                             "ipmi_fru_parse_ctx_set_debug_prefix: %s\n",
                             ipmi_fru_parse_ctx_errormsg (state_data.fru_parse_ctx));
        }
      
      if (flags)
        {
          if (ipmi_fru_parse_ctx_set_flags (state_data.fru_parse_ctx, flags) < 0)
            {
              pstdout_fprintf (pstate,
                               stderr,
                               "ipmi_fru_parse_ctx_set_flags: %s\n",
                               ipmi_fru_parse_ctx_strerror (ipmi_fru_parse_ctx_errnum (state_data.fru_parse_ctx)));
              goto cleanup;
            }
        }
    }

  if (!(state_data.sdr_cache_ctx = ipmi_sdr_cache_ctx_create ()))
    {
      pstdout_perror (pstate, "ipmi_sdr_cache_ctx_create()");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (state_data.prog_data->args->common.debug)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sdr_cache_ctx_set_flags (state_data.sdr_cache_ctx,
                                        IPMI_SDR_CACHE_FLAGS_DEBUG_DUMP) < 0)
        pstdout_fprintf (pstate,
                         stderr,
                         "ipmi_sdr_cache_ctx_set_flags: %s\n",
                         ipmi_sdr_cache_ctx_errormsg (state_data.sdr_cache_ctx));

      if (hostname)
        {
          if (ipmi_sdr_cache_ctx_set_debug_prefix (state_data.sdr_cache_ctx,
                                                   hostname) < 0)
            pstdout_fprintf (pstate,
                             stderr,
                             "ipmi_sdr_cache_ctx_set_debug_prefix: %s\n",
                             ipmi_sdr_cache_ctx_errormsg (state_data.sdr_cache_ctx));
        }
    }

  if (!(state_data.sdr_parse_ctx = ipmi_sdr_parse_ctx_create ()))
    {
      pstdout_perror (pstate, "ipmi_sdr_parse_ctx_create()");
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
  ipmi_fru_parse_ctx_destroy (state_data.fru_parse_ctx);
  ipmi_sdr_cache_ctx_destroy (state_data.sdr_cache_ctx);
  ipmi_sdr_parse_ctx_destroy (state_data.sdr_parse_ctx);
  ipmi_ctx_close (state_data.ipmi_ctx);
  ipmi_ctx_destroy (state_data.ipmi_ctx);
  return (exit_code);
}

int
main (int argc, char **argv)
{
  ipmi_fru_prog_data_t prog_data;
  struct ipmi_fru_arguments cmd_args;
  int exit_code;
  int hosts_count;
  int rv;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (ipmi_fru_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_fru_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  /* Special case, if user specified workaround via flags instead of option */
  if (prog_data.args->common.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SKIP_CHECKS)
    prog_data.args->skip_checks = 1;

  if ((hosts_count = pstdout_setup (&(prog_data.args->common.hostname),
                                    prog_data.args->hostrange.buffer_output,
                                    prog_data.args->hostrange.consolidate_output,
                                    prog_data.args->hostrange.fanout,
                                    prog_data.args->hostrange.eliminate,
                                    prog_data.args->hostrange.always_prefix)) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (!hosts_count)
    {
      exit_code = EXIT_SUCCESS;
      goto cleanup;
    }

  /* We don't want caching info to output when are doing ranged output */
  if (hosts_count > 1)
    prog_data.args->sdr.quiet_cache = 1;

  if ((rv = pstdout_launch (prog_data.args->common.hostname,
                            _ipmi_fru,
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
