/*****************************************************************************\
 *  $Id: ipmi-fru.c,v 1.19 2008-05-15 17:34:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 2 of the License, or (at your
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
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <pthread.h>
#include <err.h>
#include <argp.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "ipmi-fru.h"
#include "ipmi-fru-argp.h"
#include "ipmi-fru-info-area.h"
#include "ipmi-fru-multirecord-area.h"
#include "ipmi-fru-util.h"

#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-fiid-wrappers.h"
#include "tool-sdr-cache-common.h"
#include "pstdout.h"
#include "hostrange.h"

#define IPMI_FRU_DEFAULT_DEVICE_ID_STRING "Default FRU Device"

static int
_flush_cache (ipmi_fru_state_data_t *state_data)
{
  assert(state_data);

  if (sdr_cache_flush_cache(state_data->ipmi_sdr_cache_ctx,
                            state_data->pstate,
                            state_data->hostname,
                            state_data->prog_data->args->sdr.sdr_cache_dir_wanted ? state_data->prog_data->args->sdr.sdr_cache_dir : NULL) < 0)
    return -1;
  
  return 0;
}

static fru_err_t
_output_fru(ipmi_fru_state_data_t *state_data,
            int *output_count,
            uint8_t device_id,
            const char *device_id_str)
{
  uint8_t frubuf[IPMI_FRU_INVENTORY_AREA_SIZE_MAX+1];
  fiid_obj_t fru_get_inventory_rs = NULL;
  fiid_obj_t fru_common_header = NULL;
  int32_t common_header_len;
  uint64_t format_version;
  uint64_t internal_use_area_starting_offset;
  uint64_t chassis_info_area_starting_offset;
  uint64_t board_info_area_starting_offset;
  uint64_t product_info_area_starting_offset;
  uint64_t multirecord_area_starting_offset;
  int32_t len;
  fru_err_t rv = FRU_ERR_FATAL_ERROR;
  fru_err_t ret;

  assert(state_data);
  assert(output_count);
  assert(device_id_str);

  /* In verbose mode, output this earlier to attach it to other error messages */
  if (state_data->prog_data->args->verbose_count)
    {
      if ((*output_count))
        pstdout_printf(state_data->pstate, "\n");
      (*output_count)++;
      pstdout_printf(state_data->pstate, 
                     "FRU Inventory Device: %s (ID 0x%02X)\n",
                     device_id_str,
                     device_id);
    }

  _FIID_OBJ_CREATE(fru_get_inventory_rs, tmpl_cmd_get_fru_inventory_area_info_rs);

  if (ipmi_cmd_get_fru_inventory_area_info (state_data->ipmi_ctx,
                                            device_id,
                                            fru_get_inventory_rs) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "  FRU Get FRU Inventory Area Failure: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      rv = FRU_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_OBJ_GET (fru_get_inventory_rs,
                 "fru_inventory_area_size",
                 &state_data->fru_inventory_area_size);

  if (state_data->prog_data->args->verbose_count >= 2)
    pstdout_printf(state_data->pstate,
                   "  FRU Inventory Area Size: %u bytes\n",
                   (unsigned int) state_data->fru_inventory_area_size);

  if (!state_data->fru_inventory_area_size)
    {
      pstdout_printf(state_data->pstate,
                     "  FRU Inventory Area Size Empty\n");
      rv = FRU_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  _FIID_TEMPLATE_LEN_BYTES(common_header_len, tmpl_fru_common_header);

  memset(frubuf, '\0', IPMI_FRU_INVENTORY_AREA_SIZE_MAX+1);

  if ((ret = ipmi_fru_read_fru_data (state_data,
                                     device_id,
                                     frubuf,
                                     IPMI_FRU_INVENTORY_AREA_SIZE_MAX,
                                     0,
                                     common_header_len)) != FRU_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if ((ret = ipmi_fru_dump_hex(state_data,
                               frubuf,
                               common_header_len,
                               "Common Header")) != FRU_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  if ((ret = ipmi_fru_check_checksum(state_data,
                                     frubuf,
                                     common_header_len,
                                     0,
                                     "Common Header")) != FRU_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  _FIID_OBJ_CREATE(fru_common_header, tmpl_fru_common_header);

  _FIID_OBJ_SET_ALL_LEN(len, fru_common_header, frubuf, common_header_len);

  _FIID_OBJ_GET (fru_common_header,
                 "format_version",
                 &format_version);
  _FIID_OBJ_GET (fru_common_header, 
                 "internal_use_area_starting_offset", 
                 &internal_use_area_starting_offset);
  _FIID_OBJ_GET (fru_common_header, 
                 "chassis_info_area_starting_offset", 
                 &chassis_info_area_starting_offset);
  _FIID_OBJ_GET (fru_common_header, 
                 "board_info_area_starting_offset", 
                 &board_info_area_starting_offset);
  _FIID_OBJ_GET (fru_common_header, 
                 "product_info_area_starting_offset", 
                 &product_info_area_starting_offset);
  _FIID_OBJ_GET (fru_common_header, 
                 "multirecord_area_starting_offset", 
                 &multirecord_area_starting_offset);

  if (state_data->prog_data->args->verbose_count >= 2)
    {
      pstdout_printf(state_data->pstate, 
                     "  FRU Common Header Format Version: 0x%02X\n",
                     format_version);
      pstdout_printf(state_data->pstate, 
                     "  Internal Use Area Starting Offset: 0x%02X\n",
                     internal_use_area_starting_offset);
      pstdout_printf(state_data->pstate, 
                     "  Chassis Info Area Starting Offset: 0x%02X\n",
                     chassis_info_area_starting_offset);
      pstdout_printf(state_data->pstate, 
                     "  Board Info Area Starting Offset: 0x%02X\n",
                     board_info_area_starting_offset);
      pstdout_printf(state_data->pstate, 
                     "  Product Info Area Starting Offset: 0x%02X\n",
                     product_info_area_starting_offset);
      pstdout_printf(state_data->pstate, 
                     "  Multirecord Area Starting Offset: 0x%02X\n",
                     multirecord_area_starting_offset);
    }

  if (format_version != IPMI_FRU_COMMON_HEADER_FORMAT_VERSION)
    {
      if (state_data->prog_data->args->verbose_count)
        pstdout_fprintf(state_data->pstate, 
                        stderr,
                        "  FRU Common Header Format Unknown: 0x%02X\n", 
                        format_version);
      rv = FRU_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  /* In non-verbose mode, now we're pretty sure we can output stuff for real */
  if (!state_data->prog_data->args->verbose_count)
    {
      if ((*output_count))
        pstdout_printf(state_data->pstate, "\n");
      (*output_count)++;
      pstdout_printf(state_data->pstate, 
                     "FRU Inventory Device ID: 0x%02X\n",
                     device_id);
    }

  if (chassis_info_area_starting_offset)
    {
      pstdout_printf(state_data->pstate, "\n");
      ret = ipmi_fru_output_chassis_info_area(state_data,
                                              device_id,
                                              chassis_info_area_starting_offset);
      if (ret == FRU_ERR_FATAL_ERROR)
        {
          rv = ret;
          goto cleanup;
        }
      /* else continue on */
    }

  if (board_info_area_starting_offset)
    {
      pstdout_printf(state_data->pstate, "\n");
      ret = ipmi_fru_output_board_info_area(state_data,
                                            device_id,
                                            board_info_area_starting_offset);
      if (ret == FRU_ERR_FATAL_ERROR)
        {
          rv = ret;
          goto cleanup;
        }
      /* else continue on */
    }

  if (product_info_area_starting_offset)
    {
      pstdout_printf(state_data->pstate, "\n");
      ret = ipmi_fru_output_product_info_area(state_data,
                                              device_id,
                                              product_info_area_starting_offset);
      if (ret == FRU_ERR_FATAL_ERROR)
        {
          rv = ret;
          goto cleanup;
        }
      /* else continue on */
    }    
  if (multirecord_area_starting_offset)
    {
      pstdout_printf(state_data->pstate, "\n");
      ret = ipmi_fru_output_multirecord_info_area(state_data,
                                                  device_id,
                                                  multirecord_area_starting_offset);
      if (ret == FRU_ERR_FATAL_ERROR)
        {
          rv = ret;
          goto cleanup;
        }
      /* else continue on */
    }

  rv = FRU_ERR_SUCCESS;
 cleanup:
  _FIID_OBJ_DESTROY(fru_get_inventory_rs);
  _FIID_OBJ_DESTROY(fru_common_header);
  return (rv);
}

int
run_cmd_args (ipmi_fru_state_data_t *state_data)
{
  struct ipmi_fru_arguments *args;
  uint16_t record_count;
  int output_count = 0;
  int found = 0;
  int i;

  assert(state_data);
  
  args = state_data->prog_data->args;

  if (args->sdr.flush_cache_wanted)
    return _flush_cache (state_data);

  if (args->sdr.ignore_sdr_cache_wanted)
    {
      /* no SDR?  This is all you get :-) */
      if (_output_fru(state_data, 
                      &output_count,
                      IPMI_FRU_DEVICE_ID_DEFAULT,
                      IPMI_FRU_DEFAULT_DEVICE_ID_STRING) != FRU_ERR_SUCCESS)
        return -1;
      return 0;
    }

  if (!args->sdr.ignore_sdr_cache_wanted)
    {
      if (sdr_cache_create_and_load (state_data->ipmi_sdr_cache_ctx,
                                     state_data->pstate,
                                     state_data->ipmi_ctx,
                                     args->sdr.quiet_cache_wanted,
                                     state_data->hostname,
                                     args->sdr.sdr_cache_dir_wanted ? args->sdr.sdr_cache_dir : NULL) < 0)
        return -1;
  
      if (ipmi_sdr_cache_record_count(state_data->ipmi_sdr_cache_ctx, &record_count) < 0)
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "ipmi_sdr_cache_record_count: %s\n",
                          ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(state_data->ipmi_sdr_cache_ctx)));
          return -1;
        }
    }

  if (args->device_id_wanted)
    {
      if (args->device_id == IPMI_FRU_DEVICE_ID_DEFAULT)
        {
          if (_output_fru(state_data, 
                          &output_count,
                          IPMI_FRU_DEVICE_ID_DEFAULT,
                          IPMI_FRU_DEFAULT_DEVICE_ID_STRING) != FRU_ERR_SUCCESS)
            return -1;
          found++;
        }
      else
        {
          for (i = 0; i < record_count; i++, ipmi_sdr_cache_next(state_data->ipmi_sdr_cache_ctx))
            {
              uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
              uint8_t record_type, logical_physical_fru_device, logical_fru_device_device_slave_address;
              int sdr_record_len;

              memset(sdr_record, '\0', IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH);
              if ((sdr_record_len = ipmi_sdr_cache_record_read(state_data->ipmi_sdr_cache_ctx,
                                                               sdr_record,
                                                               IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH)) < 0)
                {
                  pstdout_fprintf(state_data->pstate,
                                  stderr,
                                  "ipmi_sdr_cache_record_read: %s\n",
                                  ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(state_data->ipmi_sdr_cache_ctx)));
                  return -1;
                }

              if (sdr_cache_get_record_id_and_type (state_data->pstate,
                                                    sdr_record,
                                                    sdr_record_len,
                                                    NULL,
                                                    &record_type) < 0)
                return -1;

              if (record_type != IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD)
                continue;

              if (sdr_cache_get_logical_fru_info (state_data->pstate,
                                                  sdr_record,
                                                  sdr_record_len,
                                                  &logical_physical_fru_device,
                                                  &logical_fru_device_device_slave_address) < 0)
                return -1;
              
              if (logical_physical_fru_device
                  && logical_fru_device_device_slave_address == args->device_id)
                {
                  char device_id_string[IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING+1];
              
                  memset(device_id_string, '\0', IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING+1);
                  if (sdr_cache_get_device_id_string (state_data->pstate,
                                                      sdr_record,
                                                      sdr_record_len,
                                                      device_id_string,
                                                      IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING) < 0)
                    return -1;
                  
                  if (_output_fru(state_data, 
                                  &output_count,
                                  args->device_id,
                                  device_id_string) != FRU_ERR_SUCCESS)
                    return -1;
                  
                  found++;
                }
            }
        }

      if (!found)
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "device id not found\n");
          return -1;
        }
    }
  else
    {
      fru_err_t ret;
  
      ret = _output_fru(state_data, 
                        &output_count,
                        IPMI_FRU_DEVICE_ID_DEFAULT,
                        IPMI_FRU_DEFAULT_DEVICE_ID_STRING);
      if (ret == FRU_ERR_FATAL_ERROR)
        return -1;
      /* else continue on */
      
      for (i = 0; i < record_count; i++, ipmi_sdr_cache_next(state_data->ipmi_sdr_cache_ctx))
        {
          uint8_t sdr_record[IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH];
          uint8_t record_type, logical_physical_fru_device, logical_fru_device_device_slave_address;
          int sdr_record_len;
          
          memset(sdr_record, '\0', IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH);
          if ((sdr_record_len = ipmi_sdr_cache_record_read(state_data->ipmi_sdr_cache_ctx,
                                                           sdr_record,
                                                           IPMI_SDR_CACHE_MAX_SDR_RECORD_LENGTH)) < 0)
            {
              pstdout_fprintf(state_data->pstate,
                              stderr,
                              "ipmi_sdr_cache_record_read: %s\n",
                              ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(state_data->ipmi_sdr_cache_ctx)));
              return -1;
            }
          
          if (sdr_cache_get_record_id_and_type (state_data->pstate,
                                                sdr_record,
                                                sdr_record_len,
                                                NULL,
                                                &record_type) < 0)
            return -1;
          
          if (record_type != IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD)
            continue;
          
          if (sdr_cache_get_logical_fru_info (state_data->pstate,
                                              sdr_record,
                                              sdr_record_len,
                                              &logical_physical_fru_device,
                                              &logical_fru_device_device_slave_address) < 0)
            return -1;
          
          if (logical_physical_fru_device
              && logical_fru_device_device_slave_address != IPMI_FRU_DEVICE_ID_DEFAULT)
            {
              char device_id_string[IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING+1];
              
              memset(device_id_string, '\0', IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING+1);
              if (sdr_cache_get_device_id_string (state_data->pstate,
                                                  sdr_record,
                                                  sdr_record_len,
                                                  device_id_string,
                                                  IPMI_SDR_CACHE_MAX_DEVICE_ID_STRING) < 0)
                return -1;
              
              ret = _output_fru(state_data, 
                                &output_count,
                                logical_fru_device_device_slave_address,
                                device_id_string);
              if (ret == FRU_ERR_FATAL_ERROR)
                return -1;
              /* else continue on */
            }
        }
    }

  return 0;
}

static int
_ipmi_fru(pstdout_state_t pstate,
          const char *hostname,
          void *arg)
{
  ipmi_fru_state_data_t state_data;
  ipmi_fru_prog_data_t *prog_data;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  int exit_code = -1;

  prog_data = (ipmi_fru_prog_data_t *)arg;
  memset(&state_data, '\0', sizeof(ipmi_fru_state_data_t));

  state_data.prog_data = prog_data;
  state_data.pstate = pstate;
  state_data.hostname = (char *)hostname;

  /* Special case, just flush, don't do an IPMI connection */
  if (!prog_data->args->sdr.flush_cache_wanted)
    {
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
    }

  if (!(state_data.ipmi_sdr_cache_ctx = ipmi_sdr_cache_ctx_create()))
    {
      pstdout_perror (pstate, "ipmi_sdr_cache_ctx_create()");
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if (state_data.prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
    {
      /* Don't error out, if this fails we can still continue */
      if (ipmi_sdr_cache_ctx_set_flags(state_data.ipmi_sdr_cache_ctx,
                                       IPMI_SDR_CACHE_FLAGS_DEBUG_DUMP) < 0)
        pstdout_fprintf (pstate, 
                         stderr,
                         "ipmi_sdr_cache_ctx_set_flags: %s\n",
                         ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(state_data.ipmi_sdr_cache_ctx)));

      if (hostname)
        {
          if (ipmi_sdr_cache_ctx_set_debug_prefix(state_data.ipmi_sdr_cache_ctx,
                                                  hostname) < 0)
            pstdout_fprintf (pstate,
                             stderr,
                             "ipmi_sdr_cache_ctx_set_debug_prefix: %s\n",
                             ipmi_sdr_cache_ctx_strerror(ipmi_sdr_cache_ctx_errnum(state_data.ipmi_sdr_cache_ctx)));
        }
    }

  if (run_cmd_args (&state_data) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  exit_code = 0;
 cleanup:
  if (state_data.ipmi_sdr_cache_ctx)
    ipmi_sdr_cache_ctx_destroy(state_data.ipmi_sdr_cache_ctx);
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
  ipmi_fru_prog_data_t prog_data;
  struct ipmi_fru_arguments cmd_args;
  int exit_code;
  int hosts_count;
  int rv;
  
  ipmi_disable_coredump();
  
  memset(&prog_data, '\0', sizeof(ipmi_fru_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_fru_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  if ((hosts_count = pstdout_setup(&(prog_data.args->common.hostname),
                                   prog_data.args->hostrange.buffer_hostrange_output,
                                   prog_data.args->hostrange.consolidate_hostrange_output,
                                   prog_data.args->hostrange.fanout,
                                   prog_data.args->hostrange.eliminate,
                                   prog_data.args->hostrange.always_prefix)) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }
  
  /* We don't want caching info to output when are doing ranged output */
  if (hosts_count > 1)
    prog_data.args->sdr.quiet_cache_wanted = 1;

  if ((rv = pstdout_launch(prog_data.args->common.hostname,
                           _ipmi_fru,
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
