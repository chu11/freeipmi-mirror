/*****************************************************************************\
 *  $Id: ipmi-fru.c,v 1.9 2007-12-25 21:09:20 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007 Lawrence Livermore National Security, LLC.
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
#include "ipmi-fru-fiid.h"
#include "ipmi-fru-info-area.h"
#include "ipmi-fru-multirecord-area.h"
#include "ipmi-fru-util.h"

#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "ipmi-sdr-cache.h"
#include "pstdout.h"
#include "hostrange.h"

fru_err_t
output_fru(ipmi_fru_state_data_t *state_data,
           uint8_t device_id)
{
  uint8_t frubuf[IPMI_FRU_INVENTORY_AREA_SIZE_MAX+1];
  fiid_obj_t fru_common_header = NULL;
  int32_t common_header_len;
  uint64_t format_version;
  uint64_t internal_use_area_starting_offset;
  uint64_t chassis_info_area_starting_offset;
  uint64_t board_info_area_starting_offset;
  uint64_t product_info_area_starting_offset;
  uint64_t multirecord_area_starting_offset;
  int32_t len;
  unsigned int frusize;
  fru_err_t rv = FRU_ERR_FATAL_ERROR;
  fru_err_t ret;

  assert(state_data);

  /* In verbose mode, output this earlier to attach it to other error messages */
  if (state_data->prog_data->args->verbose_count)
    pstdout_printf(state_data->pstate, 
                   "FRU Inventory Device ID: 0x%02X\n",
                   device_id);

  memset(frubuf, '\0', IPMI_FRU_INVENTORY_AREA_SIZE_MAX+1);

  if ((ret = ipmi_fru_get_fru_inventory_area (state_data,
                                              device_id,
                                              frubuf,
                                              IPMI_FRU_INVENTORY_AREA_SIZE_MAX,
                                              &frusize)) != FRU_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }
  
  _FIID_TEMPLATE_LEN_BYTES(common_header_len, tmpl_fru_common_header);

  if (frusize < common_header_len)
    {
      if (state_data->prog_data->args->verbose_count)
        pstdout_fprintf(state_data->pstate,
                        stderr,
                        "  FRU Inventory Area size too small\n");
      rv = FRU_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if ((ret = ipmi_fru_dump_hex(state_data,
                               frubuf,
                               frusize,
                               0,
                               common_header_len,
                               "Common Header")) != FRU_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  if ((ret = ipmi_fru_check_checksum(state_data,
                                     frubuf,
                                     frusize,
                                     0,
                                     common_header_len,
                                     0,
                                     "Common Header")) != FRU_ERR_SUCCESS)
    {
      rv = ret;
      goto cleanup;
    }

  _FIID_OBJ_CREATE(fru_common_header, tmpl_fru_common_header);

  _FIID_OBJ_SET_ALL(len, fru_common_header, frubuf, frusize);

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
    pstdout_printf(state_data->pstate, 
                   "FRU Inventory Device ID: 0x%02X\n",
                   device_id);

  pstdout_printf(state_data->pstate, "\n");

  if (chassis_info_area_starting_offset)
    {
      ret = ipmi_fru_output_chassis_info_area(state_data,
                                              frubuf,
                                              frusize,
                                              chassis_info_area_starting_offset);
      if (ret == FRU_ERR_FATAL_ERROR)
        {
          rv = ret;
          goto cleanup;
        }
      /* else continue on */
      pstdout_printf(state_data->pstate, "\n");
    }

  if (board_info_area_starting_offset)
    {
      ret = ipmi_fru_output_board_info_area(state_data,
                                            frubuf,
                                            frusize,
                                            board_info_area_starting_offset);
      if (ret == FRU_ERR_FATAL_ERROR)
        {
          rv = ret;
          goto cleanup;
        }
      /* else continue on */
      pstdout_printf(state_data->pstate, "\n");
    }

  if (product_info_area_starting_offset)
    {
      ret = ipmi_fru_output_product_info_area(state_data,
                                              frubuf,
                                              frusize,
                                              product_info_area_starting_offset);
      if (ret == FRU_ERR_FATAL_ERROR)
        {
          rv = ret;
          goto cleanup;
        }
      /* else continue on */
      pstdout_printf(state_data->pstate, "\n");
    }    
  if (multirecord_area_starting_offset)
    {
      ret = ipmi_fru_output_multirecord_info_area(state_data,
                                                  frubuf,
                                                  frusize,
                                                  multirecord_area_starting_offset);
      if (ret == FRU_ERR_FATAL_ERROR)
        {
          rv = ret;
          goto cleanup;
        }
      /* else continue on */
      pstdout_printf(state_data->pstate, "\n");
    }

  rv = FRU_ERR_SUCCESS;
 cleanup:
  if (fru_common_header)
    fiid_obj_destroy(fru_common_header);
  return (rv);
}

int
run_cmd_args (ipmi_fru_state_data_t *state_data)
{
  struct ipmi_fru_arguments *args;
  char errmsg[IPMI_SDR_CACHE_ERRMSGLEN];
  sdr_record_t *sdr_record;
  int found = 0;
  int rv = -1;
  int i;

  assert(state_data);
  
  args = state_data->prog_data->args;

  if (args->sdr.flush_cache_wanted)
    {
      if (!args->sdr.quiet_cache_wanted)
        pstdout_printf (state_data->pstate, "flushing cache... ");
      if (sdr_cache_flush (state_data->sdr_cache_ctx,
                           state_data->hostname,
                           args->sdr.sdr_cache_dir) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "SDR Cache Flush failed: %s\n",
                           sdr_cache_ctx_strerror(sdr_cache_ctx_errnum(state_data->sdr_cache_ctx)));
          goto cleanup;
        }
      if (!args->sdr.quiet_cache_wanted)
        pstdout_printf (state_data->pstate, "done\n");
      return 0;
    }

  if (sdr_cache_create_and_load (state_data->sdr_cache_ctx,
                                 state_data->ipmi_ctx,
                                 state_data->hostname,
                                 args->sdr.sdr_cache_dir,
                                 (args->sdr.quiet_cache_wanted) ? 0 : 1,
                                 (state_data->prog_data->args->common.flags & IPMI_FLAGS_DEBUG_DUMP) ? 1 : 0,
                                 &(state_data->sdr_record_list),
                                 &(state_data->sdr_record_count),
                                 errmsg,
                                 IPMI_SDR_CACHE_ERRMSGLEN) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "%s\n",
                       errmsg);
      goto cleanup;
    }

  if (args->device_id_wanted)
    {
      if (args->device_id == IPMI_FRU_DEVICE_ID_DEFAULT)
        {
          if (output_fru(state_data, IPMI_FRU_DEVICE_ID_DEFAULT) != FRU_ERR_SUCCESS)
            goto cleanup;
          found++;
        }
      else
        {
          for (i = 0; i < state_data->sdr_record_count; i++)
            {
              sdr_record = state_data->sdr_record_list + i;
              
              if (sdr_record->record_type != IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD)
                continue;
              
              if (sdr_record->record.sdr_fru_device_locator_record.logical_physical_fru_device
                  && sdr_record->record.sdr_fru_device_locator_record.logical_fru_device_device_slave_address == args->device_id)
                {
                  if (output_fru(state_data, args->device_id) != FRU_ERR_SUCCESS)
                    goto cleanup;
                  pstdout_printf(state_data->pstate, "\n");
                  found++;
                }
            }
        }

      if (!found)
        {
          pstdout_fprintf(state_data->pstate,
                          stderr,
                          "device id not found\n");
          goto cleanup;
        }
    }
  else
    {
      fru_err_t ret;
  
      ret = output_fru(state_data, IPMI_FRU_DEVICE_ID_DEFAULT);
      if (ret == FRU_ERR_FATAL_ERROR)
        goto cleanup;
      /* else continue on */
      
      for (i = 0; i < state_data->sdr_record_count; i++)
        {
          sdr_record = state_data->sdr_record_list + i;
          
          if (sdr_record->record_type != IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD)
            continue;
          
          if (sdr_record->record.sdr_fru_device_locator_record.logical_physical_fru_device
              && sdr_record->record.sdr_fru_device_locator_record.logical_fru_device_device_slave_address != IPMI_FRU_DEVICE_ID_DEFAULT)
            {
              
              ret = output_fru(state_data, 
                               sdr_record->record.sdr_fru_device_locator_record.logical_fru_device_device_slave_address);
              if (ret == FRU_ERR_FATAL_ERROR)
                goto cleanup;
              /* else continue on */
              pstdout_printf(state_data->pstate, "\n");
            }
        }
    }

 cleanup:
  if (state_data->sdr_record_list)
    {
      free(state_data->sdr_record_list);
      state_data->sdr_record_list = NULL;
      state_data->sdr_record_count = 0;
    }
  return (rv);
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

  if (!(state_data.sdr_cache_ctx = sdr_cache_ctx_create()))
    {
      pstdout_perror (pstate, "sdr_cache_ctx_create()");
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
  if (state_data.sdr_cache_ctx)
    sdr_cache_ctx_destroy(state_data.sdr_cache_ctx);
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
  
  prog_data.progname = argv[0];
  ipmi_fru_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  if ((hosts_count = pstdout_setup(&(prog_data.args->common.hostname),
                                   prog_data.args->hostrange.buffer_hostrange_output,
                                   prog_data.args->hostrange.consolidate_hostrange_output,
                                   prog_data.args->hostrange.fanout,
                                   prog_data.args->hostrange.eliminate)) < 0)
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
