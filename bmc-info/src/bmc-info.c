/*
  Copyright (C) 2005 FreeIPMI Core Team

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
#include <sys/socket.h>
#include <netinet/in.h>
#include <pthread.h>
#include <err.h>
#include <argp.h>
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "bmc-info.h"
#include "bmc-info-argp.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-fiid-wrappers.h"
#include "freeipmi-portability.h"
#include "pstdout.h"
#include "hostrange.h"

typedef struct channel_info 
{
  uint8_t channel_number;
  uint8_t medium_type;
  uint8_t protocol_type;
} channel_info_t;

#define NUM_CHANNELS 8

static int
display_intel (bmc_info_state_data_t *state_data, fiid_obj_t device_id_rs)
{
  uint64_t bc_maj, bc_min, pia_maj, pia_min;
  fiid_obj_t intel_rs = NULL;
  uint8_t buf[1024];
  int32_t len;
  int rv = -1;

  assert(state_data);

  if (!(intel_rs = fiid_obj_create(tmpl_cmd_get_device_id_sr870bn4_rs)))
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "fiid_obj_create: %s\n",
                      strerror(errno));
      goto cleanup;
    }

  if ((len = fiid_obj_get_all(device_id_rs, buf, 1024)) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "fiid_obj_get_all: %s\n",
                      fiid_strerror(fiid_obj_errnum(device_id_rs)));
      goto cleanup;
    }

  if (fiid_obj_set_all(intel_rs, buf, len) < 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "fiid_obj_set_all: %s\n",
                      fiid_strerror(fiid_obj_errnum(intel_rs)));
      goto cleanup;
    }
  
  _FIID_OBJ_GET (intel_rs,
                 "auxiliary_firmware_revision_info.boot_code.major",
                 &bc_maj);
  _FIID_OBJ_GET (intel_rs,
                 "auxiliary_firmware_revision_info.boot_code.minor",
                 &bc_min);
  _FIID_OBJ_GET (intel_rs,
                 "auxiliary_firmware_revision_info.pia.major",
                 &pia_maj);
  _FIID_OBJ_GET (intel_rs,
                 "auxiliary_firmware_revision_info.pia.minor",
                 &pia_min);
  pstdout_printf(state_data->pstate, 
                 "Aux Firmware Revision Info: Boot Code v%02x.%2x, PIA v%02x.%2x\n",
                 (unsigned int) bc_maj, 
                 (unsigned int) bc_min, 
                 (unsigned int) pia_maj, 
                 (unsigned int) pia_min);
  
  rv = 0;
 cleanup:
  if (intel_rs)
    fiid_obj_destroy(intel_rs);
  return rv;
}

static int 
display_get_device_id (bmc_info_state_data_t *state_data)
{
  fiid_obj_t cmd_rs = NULL;
  uint64_t val = 0;
  int rv = -1;

  assert(state_data);

  if (!(cmd_rs = fiid_obj_create (tmpl_cmd_get_device_id_rs)))
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "fiid_obj_create: %s\n",
                      strerror(errno));
      return (-1);
    }

  if (ipmi_cmd_get_device_id (state_data->ipmi_ctx, cmd_rs) != 0)
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "ipmi_cmd_get_device_id: %s\n",
                      ipmi_ctx_strerror(ipmi_ctx_errnum(state_data->ipmi_ctx)));
      goto cleanup;
    }
  
  _FIID_OBJ_GET (cmd_rs, "device_id", &val);
  pstdout_printf(state_data->pstate, 
                 "Device ID:         %X\n", (unsigned int) val);
  
  _FIID_OBJ_GET (cmd_rs, "device_revision.revision", &val);
  pstdout_printf(state_data->pstate, 
                 "Device Revision:   %d\n", (unsigned int) val);
  
  _FIID_OBJ_GET (cmd_rs, "device_revision.sdr_support", &val);
  if (val)
    pstdout_printf(state_data->pstate, 
                   "                   [SDR Support]\n");
  
  {
    uint64_t maj, min;
    _FIID_OBJ_GET (cmd_rs, "firmware_revision1.major_revision", &maj);
    _FIID_OBJ_GET (cmd_rs, "firmware_revision2.minor_revision", &min);
    pstdout_printf(state_data->pstate, 
                   "Firmware Revision: %d.%d\n", 
                   (unsigned int) maj, 
                   (unsigned int) min);
  }
  
  _FIID_OBJ_GET (cmd_rs, 
                 "firmware_revision1.device_available", 
                 &val);
  if (val == 0)
    pstdout_printf(state_data->pstate, 
                   "                   [Device Available (normal operation)]\n");
  else
    {
      pstdout_printf(state_data->pstate, 
                     "                   [Device Not Available]\n");
      pstdout_printf(state_data->pstate, 
                     "                   [firmware, SDR update or self init in progress]\n");
    }
  
  {
    uint64_t ms, ls;
    _FIID_OBJ_GET (cmd_rs, "ipmi_version.ms_bits", &ms);
    _FIID_OBJ_GET (cmd_rs, "ipmi_version.ls_bits", &ls);
    pstdout_printf(state_data->pstate, 
                   "IPMI Version:      %d.%d\n",
                   (unsigned int) ms, 
                   (unsigned int) ls);
  }
  
  pstdout_printf(state_data->pstate, 
                 "Additional Device Support:\n");
  
  _FIID_OBJ_GET (cmd_rs, "additional_device_support.sensor_device", &val);
  if(val)
    pstdout_printf(state_data->pstate, 
                   "                   [Sensor Device]\n");
  
  _FIID_OBJ_GET (cmd_rs, "additional_device_support.sdr_repository_device", &val);
  if(val)
    pstdout_printf(state_data->pstate, 
                   "                   [SDR Repository Device]\n");
  
  _FIID_OBJ_GET (cmd_rs, "additional_device_support.sel_device", &val);
  if(val)
    pstdout_printf(state_data->pstate, 
                   "                   [SEL Device]\n");
  
  _FIID_OBJ_GET (cmd_rs, "additional_device_support.fru_inventory_device", &val);
  if(val)
    pstdout_printf(state_data->pstate, 
                   "                   [FRU Inventory Device]\n");
  
  _FIID_OBJ_GET (cmd_rs, "additional_device_support.ipmb_event_receiver", &val);
  if(val)
    pstdout_printf(state_data->pstate, 
                   "                   [IPMB Event Receiver]\n");
  
  _FIID_OBJ_GET (cmd_rs, "additional_device_support.ipmb_event_generator", &val);
  if(val)
    pstdout_printf(state_data->pstate, 
                   "                   [IPMB Event Generator]\n");
  
  _FIID_OBJ_GET (cmd_rs, "additional_device_support.bridge", &val);
  if(val)
    pstdout_printf(state_data->pstate, 
                   "                   [Bridge]\n");
  
  _FIID_OBJ_GET (cmd_rs, "additional_device_support.chassis_device", &val);
  if(val)
    pstdout_printf(state_data->pstate, 
                   "                   [Chassis Device]\n");
  
  {
    uint64_t manufacturer_id, product_id;
    
    _FIID_OBJ_GET (cmd_rs, "manufacturer_id.id", &manufacturer_id);
    pstdout_printf(state_data->pstate, "Manufacturer ID:   %Xh\n", (unsigned int) manufacturer_id);
    
    _FIID_OBJ_GET (cmd_rs, "product_id", &product_id);
    pstdout_printf(state_data->pstate, "Product ID:        %Xh\n", (unsigned int) product_id);
    
    _FIID_OBJ_GET (cmd_rs, "auxiliary_firmware_revision_info", &val);
    switch (manufacturer_id)
      {
      case IPMI_MANUFACTURER_ID_INTEL: 
	switch (product_id)
	  {
	    /* I am assuming all Intel products will decode alike.
               -- Anand Babu <ab@gnu.org.in>  */
	  case IPMI_PRODUCT_ID_SR870BN4:
	  default:
            if (display_intel(state_data, cmd_rs) < 0)
              goto cleanup;
            break;
	  }
	break;
      default:
        pstdout_printf(state_data->pstate, "Aux Firmware Revision Info: %Xh\n", (unsigned int) val);
      }
  }

  rv = 0;
 cleanup:
  if (cmd_rs)
    fiid_obj_destroy(cmd_rs);
  return rv;
}

static int
get_channel_info_list (bmc_info_state_data_t *state_data, channel_info_t *channel_info_list)
{
  fiid_obj_t data_rs = NULL; 
  uint8_t i;
  uint8_t ci;
  uint64_t val;
  int rv = -1;

  assert(state_data);
  assert(channel_info_list);

  if (!(data_rs = fiid_obj_create (tmpl_cmd_get_channel_info_rs)))
    {
      pstdout_fprintf(state_data->pstate,
                      stderr,
                      "fiid_obj_create: %s\n",
                      strerror(errno));
      goto cleanup;
    }

  for (i = 0, ci = 0; i < NUM_CHANNELS; i++)
    {
      if (ipmi_cmd_get_channel_info (state_data->ipmi_ctx, 
				     i, 
				     data_rs) != 0)
	continue;
      
      _FIID_OBJ_GET (data_rs, 
                     "actual_channel_number", 
                     &val);
      channel_info_list[ci].channel_number = (uint8_t) val;
      
      _FIID_OBJ_GET (data_rs, 
                     "channel_medium_type", 
                     &val);
      channel_info_list[ci].medium_type = (uint8_t) val;
      
      _FIID_OBJ_GET (data_rs, 
                     "channel_protocol_type", 
                     &val);
      channel_info_list[ci].protocol_type = (uint8_t) val;
      
      ci++;
    }

  rv = 0;
 cleanup:
  if (data_rs)
    fiid_obj_destroy(data_rs);
  return rv;
}

int 
display_channel_info (bmc_info_state_data_t *state_data)
{
  channel_info_t channel_info_list[NUM_CHANNELS];
  uint8_t i;

  assert(state_data);

  memset(channel_info_list, '\0', sizeof(channel_info_t) * NUM_CHANNELS);
  if (get_channel_info_list (state_data, channel_info_list) < 0)
    return (-1);

  pstdout_printf (state_data->pstate, "Channel Information:\n");
  for (i = 0; i < NUM_CHANNELS; i++)
    {
      char *medium_type = NULL;
      char *protocol_type = NULL;

      if (IPMI_CHANNEL_MEDIUM_TYPE_IS_RESERVED(channel_info_list[i].medium_type))
        continue;
      
      pstdout_printf (state_data->pstate, 
                      "       Channel No: %d\n", 
                      channel_info_list[i].channel_number);
      
      if (IPMI_CHANNEL_MEDIUM_TYPE_IS_RESERVED(channel_info_list[i].medium_type))
        medium_type = "Reserved";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_IPMB)
        medium_type = "IPMB (I2C)";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_ICMB_10)
        medium_type = "ICMB v1.0";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_ICMB_09)
        medium_type = "ICMB v0.9";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)
        medium_type = "802.3 LAN";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_RS232)
        medium_type = "Asynch. Serial/Modem (RS-232)";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_OTHER_LAN)
        medium_type = "Other LAN";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_PCI_SMBUS)
        medium_type = "PCI SMBus";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_SMBUS_10_11)
        medium_type = "SMBus v1.0/1.1";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_SMBUS_20)
        medium_type = "SMBus v2.0";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_USB_1X)
        medium_type = "USB 1.x";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_USB_2X)
        medium_type = "USB 2.x";
      else if (channel_info_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_SYS_IFACE)
        medium_type = "System Interface (KCS, SMIC, or BT)";
      else if (IPMI_CHANNEL_MEDIUM_TYPE_IS_OEM(channel_info_list[i].medium_type))
        medium_type = "OEM";

      if (IPMI_CHANNEL_PROTOCOL_TYPE_IS_RESERVED(channel_info_list[i].protocol_type))
        protocol_type = "Reserved";
      else if (channel_info_list[i].protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_IPMB)
        protocol_type = "IPMB-1.0";
      else if (channel_info_list[i].protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_ICMB_10)
        protocol_type = "ICMB-1.0";
      else if (channel_info_list[i].protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_SMBUS_1X_2X)
        protocol_type = "IPMI-SMBus";
      else if (channel_info_list[i].protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_KCS)
        protocol_type = "KCS";
      else if (channel_info_list[i].protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_SMIC)
        protocol_type = "SMIC";
      else if (channel_info_list[i].protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_BT_10)
        protocol_type = "BT-10";
      else if (channel_info_list[i].protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_BT_15)
        protocol_type = "BT-15";
      else if (channel_info_list[i].protocol_type == IPMI_CHANNEL_PROTOCOL_TYPE_TMODE)
        protocol_type = "TMODE";
      else if (IPMI_CHANNEL_PROTOCOL_TYPE_IS_OEM(channel_info_list[i].protocol_type))
        protocol_type = "OEM";

      if (medium_type)
        pstdout_printf (state_data->pstate, 
                        "      Medium Type: %s\n", 
                        medium_type);

      if (protocol_type)
        pstdout_printf (state_data->pstate, 
                        "    Protocol Type: %s\n", 
                        protocol_type);
    }
  
  return 0;
}

int
run_cmd_args (bmc_info_state_data_t *state_data)
{
  int rv = -1;

  assert(state_data);
  
  if (display_get_device_id (state_data) < 0)
    goto cleanup;
  
  if (display_channel_info (state_data) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  return (rv);
}

static int
_bmc_info(pstdout_state_t pstate,
          const char *hostname,
          void *arg)
{
  bmc_info_state_data_t state_data;
  bmc_info_prog_data_t *prog_data;
  char errmsg[IPMI_OPEN_ERRMSGLEN];
  int exit_code = -1;

  prog_data = (bmc_info_prog_data_t *)arg;
  memset(&state_data, '\0', sizeof(bmc_info_state_data_t));

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
  bmc_info_prog_data_t prog_data;
  struct bmc_info_arguments cmd_args;
  int exit_code;
  int rv;
  
  ipmi_disable_coredump();
  
  prog_data.progname = argv[0];
  bmc_info_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  if (pstdout_setup(&(prog_data.args->common.hostname),
                    prog_data.args->hostrange.buffer_hostrange_output,
                    prog_data.args->hostrange.consolidate_hostrange_output,
                    prog_data.args->hostrange.fanout,
                    prog_data.args->hostrange.eliminate) < 0)
    {
      exit_code = EXIT_FAILURE;
      goto cleanup;
    }

  if ((rv = pstdout_launch(prog_data.args->common.hostname,
                           _bmc_info,
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
