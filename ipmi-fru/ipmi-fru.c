/*****************************************************************************\
 *  $Id: ipmi-fru.c,v 1.59 2010-02-08 22:20:58 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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

#include "ipmi-fru_.h"
#include "ipmi-fru-argp.h"
#include "ipmi-fru-output.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"
#include "tool-hostrange-common.h"
#include "tool-oem-common.h"
#include "tool-sdr-cache-common.h"
#include "tool-util-common.h"

#define IPMI_FRU_DEFAULT_DEVICE_ID_STRING "Default FRU Device"

typedef int (*ipmi_fru_sdr_callback)(ipmi_fru_state_data_t *,
				     unsigned int *,
				     const void*,
				     unsigned int,
				     uint8_t,
				     void *);

struct ipmi_fru_sdr_find_data
{
  uint8_t device_id;
  int found;
};

struct ipmi_fru_sdr_callback
{
  ipmi_fru_state_data_t *state_data;
  unsigned int *output_count;
  ipmi_fru_sdr_callback fru_cb;
  void *arg;
};

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

  if (ipmi_fru_open_device_id (state_data->fru_ctx, device_id) < 0)
    {
      if (IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_ctx))
        {
          /* Special case, not really an "error" */
          if (ipmi_fru_ctx_errnum (state_data->fru_ctx) != IPMI_FRU_ERR_NO_FRU_INFORMATION)
            {
              pstdout_printf (state_data->pstate, "\n");
              pstdout_printf (state_data->pstate,
                              "  FRU Error: %s\n",
                              ipmi_fru_ctx_errormsg (state_data->fru_ctx));
            }
          goto out;
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_open_device_id: %s\n",
                       ipmi_fru_ctx_errormsg (state_data->fru_ctx));
      goto cleanup;
    }

  if (ipmi_fru_first (state_data->fru_ctx) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_first: %s\n",
                       ipmi_fru_ctx_errormsg (state_data->fru_ctx));
      goto cleanup;
    }

  do 
    {
      uint8_t areabuf[IPMI_FRU_AREA_SIZE_MAX+1];
      unsigned int area_type = 0;
      unsigned int area_length = 0;
      
      memset (areabuf, '\0', IPMI_FRU_AREA_SIZE_MAX + 1);
      if (ipmi_fru_read_data_area (state_data->fru_ctx,
				   &area_type,
				   &area_length,
				   areabuf,
				   IPMI_FRU_AREA_SIZE_MAX) < 0)
        {
          if (IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_ctx))
            {
              /* Special case, not really an "error" */
              if (ipmi_fru_ctx_errnum (state_data->fru_ctx) != IPMI_FRU_ERR_NO_FRU_INFORMATION)
                {
                  pstdout_printf (state_data->pstate, "\n");
                  pstdout_printf (state_data->pstate,
                                  "  FRU Error: %s\n",
                                  ipmi_fru_ctx_errormsg (state_data->fru_ctx));
                }
              goto next;
            }
          
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_fru_read_data_area: %s\n",
                           ipmi_fru_ctx_errormsg (state_data->fru_ctx));
          goto cleanup;
        }

      if (area_length)
        {
          pstdout_printf (state_data->pstate, "\n");

          switch (area_type)
            {
            case IPMI_FRU_AREA_TYPE_CHASSIS_INFO_AREA:
              if (ipmi_fru_output_chassis_info_area (state_data,
                                                     areabuf,
                                                     area_length) < 0)
                goto cleanup;
              break;
            case IPMI_FRU_AREA_TYPE_BOARD_INFO_AREA:
              if (ipmi_fru_output_board_info_area (state_data,
                                                   areabuf,
                                                   area_length) < 0)
                goto cleanup;
              break;
            case IPMI_FRU_AREA_TYPE_PRODUCT_INFO_AREA:
              if (ipmi_fru_output_product_info_area (state_data,
                                                     areabuf,
                                                     area_length) < 0)
                goto cleanup;
              break;
            case IPMI_FRU_AREA_TYPE_MULTIRECORD_POWER_SUPPLY_INFORMATION:
              if (ipmi_fru_output_power_supply_information (state_data,
                                                            areabuf,
                                                            area_length) < 0)
                goto cleanup;
              break;
            case IPMI_FRU_AREA_TYPE_MULTIRECORD_DC_OUTPUT:
            case IPMI_FRU_AREA_TYPE_MULTIRECORD_EXTENDED_DC_OUTPUT:
              if (ipmi_fru_output_dc_output (state_data,
					     area_type,
                                             areabuf,
                                             area_length) < 0)
                goto cleanup;
              break;
            case IPMI_FRU_AREA_TYPE_MULTIRECORD_DC_LOAD:
            case IPMI_FRU_AREA_TYPE_MULTIRECORD_EXTENDED_DC_LOAD:
              if (ipmi_fru_output_dc_load (state_data,
					   area_type,
                                           areabuf,
                                           area_length) < 0)
                goto cleanup;
              break;
            case IPMI_FRU_AREA_TYPE_MULTIRECORD_MANAGEMENT_ACCESS_RECORD:
              if (ipmi_fru_output_management_access_record (state_data,
                                                            areabuf,
                                                            area_length) < 0)
                goto cleanup;
              break;
            case IPMI_FRU_AREA_TYPE_MULTIRECORD_BASE_COMPATABILITY_RECORD:
              if (ipmi_fru_output_base_compatibility_record (state_data,
                                                             areabuf,
                                                             area_length) < 0)
                goto cleanup;
              break;
            case IPMI_FRU_AREA_TYPE_MULTIRECORD_EXTENDED_COMPATABILITY_RECORD:
              if (ipmi_fru_output_extended_compatibility_record (state_data,
                                                                 areabuf,
                                                                 area_length) < 0)
                goto cleanup;
              break;
            case IPMI_FRU_AREA_TYPE_MULTIRECORD_OEM:
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
    } while ((ret = ipmi_fru_next (state_data->fru_ctx)) == 1);
    
  if (ret < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_next: %s\n",
                       ipmi_fru_ctx_errormsg (state_data->fru_ctx));
      goto cleanup;
    }

 out:
  rv = 0;
 cleanup:
  ipmi_fru_close_device_id (state_data->fru_ctx);
  return (rv);
}

static int
_output_fru_with_sdr (ipmi_fru_state_data_t *state_data,
		      unsigned int *output_count,
		      const void *sdr_record,
		      unsigned int sdr_record_len,
		      uint8_t device_id)
{
  char device_id_string[IPMI_SDR_MAX_DEVICE_ID_STRING_LENGTH+1];
  int rv = -1;

  assert (state_data);
  assert (output_count);
  assert (sdr_record);
  assert (sdr_record_len);

  memset (device_id_string, '\0', IPMI_SDR_MAX_DEVICE_ID_STRING_LENGTH+1);

  if (ipmi_sdr_parse_device_id_string (state_data->sdr_ctx,
				       sdr_record,
				       sdr_record_len,
				       device_id_string,
				       IPMI_SDR_MAX_DEVICE_ID_STRING_LENGTH) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_parse_device_id_string: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }
  
  if (_output_fru (state_data,
		   output_count,
		   device_id,
		   device_id_string) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

static int
_output_dimm (ipmi_fru_state_data_t *state_data,
	      unsigned int *output_count,
	      uint8_t device_id,
	      const char *device_id_str)
{
  uint8_t areabuf[IPMI_FRU_AREA_SIZE_MAX+1];
  unsigned int area_type = 0;
  unsigned int area_length = 0;
  unsigned int orig_flags = 0;
  int block_len;
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

  if (ipmi_fru_ctx_get_flags (state_data->fru_ctx, &orig_flags) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_ctx_get_flags: %s\n",
                       ipmi_fru_ctx_errormsg (state_data->fru_ctx));
      goto cleanup;
    }
   
  if (ipmi_fru_ctx_set_flags (state_data->fru_ctx, orig_flags | IPMI_FRU_FLAGS_READ_RAW) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_ctx_set_flags: %s\n",
                       ipmi_fru_ctx_errormsg (state_data->fru_ctx));
      goto cleanup;
    }

  if (ipmi_fru_open_device_id (state_data->fru_ctx, device_id) < 0)
    {
      if (IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_ctx))
        {
          /* Special case, not really an "error" */
          if (ipmi_fru_ctx_errnum (state_data->fru_ctx) != IPMI_FRU_ERR_NO_FRU_INFORMATION)
            {
              pstdout_printf (state_data->pstate, "\n");
              pstdout_printf (state_data->pstate,
                              "  FRU Error: %s\n",
                              ipmi_fru_ctx_errormsg (state_data->fru_ctx));
            }
          goto out;
        }
      
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_open_device_id: %s\n",
                       ipmi_fru_ctx_errormsg (state_data->fru_ctx));
      goto flags_cleanup;
    }

  memset (areabuf, '\0', IPMI_FRU_AREA_SIZE_MAX + 1);
  if (ipmi_fru_read_data_area (state_data->fru_ctx,
			       &area_type,
			       &area_length,
			       areabuf,
			       IPMI_FRU_AREA_SIZE_MAX) < 0)
    {
      if (IPMI_FRU_ERRNUM_IS_NON_FATAL_ERROR (state_data->fru_ctx))
	{
	  /* Special case, not really an "error" */
	  if (ipmi_fru_ctx_errnum (state_data->fru_ctx) != IPMI_FRU_ERR_NO_FRU_INFORMATION)
	    {
	      pstdout_printf (state_data->pstate, "\n");
	      pstdout_printf (state_data->pstate,
			      "  FRU Error: %s\n",
			      ipmi_fru_ctx_errormsg (state_data->fru_ctx));
	    }
	  goto out;
	}
          
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_fru_read_data_area: %s\n",
		       ipmi_fru_ctx_errormsg (state_data->fru_ctx));
      goto flags_cleanup;
    }

  if (area_type != IPMI_FRU_AREA_TYPE_RAW_DATA)
    {
      pstdout_printf (state_data->pstate, "\n");
      pstdout_printf (state_data->pstate,
		      "  FRU Error: Invalid area type returned\n");
      goto out;
    }

  if ((block_len = fiid_template_block_len_bytes (tmpl_fru_dimm_spd_ddr_header,
						  "spd_bytes_used",
						  "reserved")) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "fiid_template_block_len_bytes: %s\n",
		       strerror (errno));
      goto flags_cleanup;
    }

  if (area_length < block_len)
    {
      pstdout_printf (state_data->pstate, "\n");
      pstdout_printf (state_data->pstate,
		      "  FRU Error: %s\n",
		      ipmi_fru_ctx_strerror (IPMI_FRU_ERR_FRU_AREA_LENGTH_INVALID));
      goto out;
    }

  if (ipmi_fru_output_dimm (state_data,
			    areabuf,
			    area_length) < 0)
    goto flags_cleanup;

 out:
  rv = 0;

flags_cleanup:
  if (ipmi_fru_ctx_set_flags (state_data->fru_ctx, orig_flags) < 0)
    {
      pstdout_fprintf (state_data->pstate,
                       stderr,
                       "ipmi_fru_ctx_set_flags: %s\n",
                       ipmi_fru_ctx_errormsg (state_data->fru_ctx));
      rv = -1;
      goto cleanup;
    }

 cleanup:
  ipmi_fru_close_device_id (state_data->fru_ctx);
  return (rv);
}

static int
_output_dimm_with_sdr (ipmi_fru_state_data_t *state_data,
		       unsigned int *output_count,
		       const void *sdr_record,
		       unsigned int sdr_record_len,
		       uint8_t device_id)
{
  char device_id_string[IPMI_SDR_MAX_DEVICE_ID_STRING_LENGTH+1];
  int rv = -1;

  assert (state_data);
  assert (output_count);
  assert (sdr_record);
  assert (sdr_record_len);
  
  memset (device_id_string, '\0', IPMI_SDR_MAX_DEVICE_ID_STRING_LENGTH+1);
  
  if (ipmi_sdr_parse_device_id_string (state_data->sdr_ctx,
				       sdr_record,
				       sdr_record_len,
				       device_id_string,
				       IPMI_SDR_MAX_DEVICE_ID_STRING_LENGTH) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_parse_device_id_string: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }
  
  if (_output_dimm (state_data,
		    output_count,
		    device_id,
		    device_id_string) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  return (rv);
}

static int
_is_logical_fru (uint8_t device_type, uint8_t device_type_modifier)
{
  /* achu: All this code and checks could be shortened if we abuse
   * knowledge of the actual values of these macros, but we list all
   * of this to be clear what we're doing.
   */
  if ((device_type == IPMI_DEVICE_TYPE_EEPROM_24C01_OR_EQUIVALENT
       && device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_EEPROM_24C01_OR_EQUIVALENT_IPMI_FRU_INVENTORY)
      || (device_type == IPMI_DEVICE_TYPE_EEPROM_24C02_OR_EQUIVALENT
	  && device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_EEPROM_24C02_OR_EQUIVALENT_IPMI_FRU_INVENTORY)
      || (device_type == IPMI_DEVICE_TYPE_EEPROM_24C04_OR_EQUIVALENT
	  && device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_EEPROM_24C04_OR_EQUIVALENT_IPMI_FRU_INVENTORY)
      || (device_type == IPMI_DEVICE_TYPE_EEPROM_24C08_OR_EQUIVALENT
	  && device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_EEPROM_24C08_OR_EQUIVALENT_IPMI_FRU_INVENTORY)
      || (device_type == IPMI_DEVICE_TYPE_EEPROM_24C16_OR_EQUIVALENT
	  && device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_EEPROM_24C16_OR_EQUIVALENT_IPMI_FRU_INVENTORY)
      || (device_type == IPMI_DEVICE_TYPE_EEPROM_24C17_OR_EQUIVALENT
	  && device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_EEPROM_24C17_OR_EQUIVALENT_IPMI_FRU_INVENTORY)
      || (device_type == IPMI_DEVICE_TYPE_EEPROM_24C32_OR_EQUIVALENT
	  && device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_EEPROM_24C32_OR_EQUIVALENT_IPMI_FRU_INVENTORY)
      || (device_type == IPMI_DEVICE_TYPE_EEPROM_24C64_OR_EQUIVALENT
	  && device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_EEPROM_24C64_OR_EQUIVALENT_IPMI_FRU_INVENTORY)
      || (device_type == IPMI_DEVICE_TYPE_FRU_INVENTORY_DEVICE_BEHIND_MANAGEMENT_CONTROLLER
	  && (device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_FRU_INVENTORY_DEVICE_BEHIND_MANAGEMENT_CONTROLLER_IPMI_FRU_INVENTORY_BACKWARDS_COMPATABILITY
	      || device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_FRU_INVENTORY_DEVICE_BEHIND_MANAGEMENT_CONTROLLER_IPMI_FRU_INVENTORY)))
    return (1);

  return (0);
}

static int
_is_dimm_fru (uint8_t device_type, uint8_t device_type_modifier)
{
  /* achu: All this code and checks could be shortened if we abuse
   * knowledge of the actual values of these macros, but we list all
   * of this to be clear what we're doing.
   */
  if ((device_type == IPMI_DEVICE_TYPE_EEPROM_24C01_OR_EQUIVALENT
       && device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_EEPROM_24C01_OR_EQUIVALENT_DIMM_MEMORY_ID)
      || (device_type == IPMI_DEVICE_TYPE_EEPROM_24C02_OR_EQUIVALENT
	  && device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_EEPROM_24C02_OR_EQUIVALENT_DIMM_MEMORY_ID)
      || (device_type == IPMI_DEVICE_TYPE_EEPROM_24C04_OR_EQUIVALENT
	  && device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_EEPROM_24C04_OR_EQUIVALENT_DIMM_MEMORY_ID)
      || (device_type == IPMI_DEVICE_TYPE_EEPROM_24C08_OR_EQUIVALENT
	  && device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_EEPROM_24C08_OR_EQUIVALENT_DIMM_MEMORY_ID)
      || (device_type == IPMI_DEVICE_TYPE_EEPROM_24C16_OR_EQUIVALENT
	  && device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_EEPROM_24C16_OR_EQUIVALENT_DIMM_MEMORY_ID)
      || (device_type == IPMI_DEVICE_TYPE_EEPROM_24C17_OR_EQUIVALENT
	  && device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_EEPROM_24C17_OR_EQUIVALENT_DIMM_MEMORY_ID)
      || (device_type == IPMI_DEVICE_TYPE_EEPROM_24C32_OR_EQUIVALENT
	  && device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_EEPROM_24C32_OR_EQUIVALENT_DIMM_MEMORY_ID)
      || (device_type == IPMI_DEVICE_TYPE_EEPROM_24C64_OR_EQUIVALENT
	  && device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_EEPROM_24C64_OR_EQUIVALENT_DIMM_MEMORY_ID)
      || (device_type == IPMI_DEVICE_TYPE_FRU_INVENTORY_DEVICE_BEHIND_MANAGEMENT_CONTROLLER
	  && device_type_modifier == IPMI_DEVICE_TYPE_MODIFIER_FRU_INVENTORY_DEVICE_BEHIND_MANAGEMENT_CONTROLLER_DIMM_MEMORY_ID))
    return (1);

  return (0);
}

static int
_print_except_default_fru_cb (ipmi_fru_state_data_t *state_data,
			      unsigned int *output_count,
			      const void *sdr_record,
			      unsigned int sdr_record_len,
			      uint8_t record_type,
			      void *arg)
{
  int rv = -1;

  assert (state_data);
  assert (output_count);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (record_type == IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD
	  || record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD);

  if (record_type == IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD)
    {
      uint8_t logical_physical_fru_device, logical_fru_device_device_slave_address;
      uint8_t device_access_address, channel_number;
      uint8_t device_type, device_type_modifier;

      if (ipmi_sdr_parse_fru_device_locator_parameters (state_data->sdr_ctx,
							sdr_record,
							sdr_record_len,
							&device_access_address,
							&logical_fru_device_device_slave_address,
							NULL,
							NULL,
							&logical_physical_fru_device,
							&channel_number) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_sdr_parse_fru_device_locator_parameters: %s\n",
			   ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	  goto cleanup;
	}

      if (ipmi_sdr_parse_device_type (state_data->sdr_ctx,
				      sdr_record,
				      sdr_record_len,
				      &device_type,
				      &device_type_modifier) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_sdr_parse_device_type: %s\n",
			   ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	  goto cleanup;
	}

      /* stored in 7-bit form, unlike sensor owner ids, need to shift */
      device_access_address <<= 1;

      if (logical_physical_fru_device
	  && logical_fru_device_device_slave_address != IPMI_FRU_DEVICE_ID_DEFAULT)
	{

	  if (device_access_address == IPMI_SLAVE_ADDRESS_BMC)
	    {
	      if (_is_logical_fru (device_type, device_type_modifier))
		{
		  if (_output_fru_with_sdr (state_data,
					    output_count,
					    sdr_record,
					    sdr_record_len,
					    logical_fru_device_device_slave_address) < 0)
		    goto cleanup;
		}
	      else if (_is_dimm_fru (device_type, device_type_modifier))
		{
		  if (_output_dimm_with_sdr (state_data,
					     output_count,
					     sdr_record,
					     sdr_record_len,
					     logical_fru_device_device_slave_address) < 0)
		    goto cleanup;
		}
	    }
	  else
	    {
	      if (state_data->prog_data->args->bridge_fru)
		{
		  if (ipmi_ctx_set_target (state_data->ipmi_ctx,
					   &channel_number,
					   &device_access_address) < 0)
		    {
		      pstdout_fprintf (state_data->pstate,
				       stderr,
				       "ipmi_ctx_set_target: %s\n",
				       ipmi_ctx_errormsg (state_data->ipmi_ctx));
		      goto cleanup;
		    }

		  if (_is_logical_fru (device_type, device_type_modifier))
		    {
		      if (_output_fru_with_sdr (state_data,
						output_count,
						sdr_record,
						sdr_record_len,
						logical_fru_device_device_slave_address) < 0)
			goto cleanup;
		    }
		  else if (_is_dimm_fru (device_type, device_type_modifier))
		    {
		      if (_output_dimm_with_sdr (state_data,
						 output_count,
						 sdr_record,
						 sdr_record_len,
						 logical_fru_device_device_slave_address) < 0)
			goto cleanup;
		    }
		  
		  if (ipmi_ctx_set_target (state_data->ipmi_ctx, NULL, NULL) < 0)
		    {
		      pstdout_fprintf (state_data->pstate,
				       stderr,
				       "ipmi_ctx_set_target: %s\n",
				       ipmi_ctx_errormsg (state_data->ipmi_ctx));
		      goto cleanup;
		    }
		}
	    }
	}
    }
  else
    {
      uint8_t device_slave_address;
      uint8_t channel_number;
      uint8_t device_capabilities_fru_inventory_device;

      assert (state_data->prog_data->args->bridge_fru);

      /* achu: Thanks to Michael L. Winiarski <michael.winiarski at
       * hp.com> patch on ipmitool mailing list Was not originaly
       * aware that FRU could be specified via this record.
       */

      if (ipmi_sdr_parse_management_controller_device_locator_parameters (state_data->sdr_ctx,
									  sdr_record,
									  sdr_record_len,
									  &device_slave_address,
									  &channel_number,
									  NULL,
									  NULL,
									  NULL,
									  NULL,
									  NULL,
									  NULL,
									  NULL,
									  NULL,
									  NULL,
									  &device_capabilities_fru_inventory_device,
									  NULL,
									  NULL,
									  NULL,
									  NULL) < 0)
	{
	  
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_sdr_parse_management_controller_device_locator_parameters: %s\n",
			   ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	  goto cleanup;
	}
      
      /* stored in 7-bit form, unlike sensor owner ids, need to shift */
      device_slave_address <<= 1;

      /* duplicate of base FRU Device ID 0 - skip
       *
       * Note that it is always Device ID 0 w/ management controller device locators
       */
      if (device_slave_address == IPMI_SLAVE_ADDRESS_BMC
	  && channel_number == IPMI_CHANNEL_NUMBER_PRIMARY_IPMB)
	goto out;

      if (!device_capabilities_fru_inventory_device)
	goto out;
      
      if (ipmi_ctx_set_target (state_data->ipmi_ctx,
			       &channel_number,
			       &device_slave_address) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_ctx_set_target: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  goto cleanup;
	}
			       
      /* 0 is defined FRU device ID when bridging - see IPMI spec SDR record info */
      if (_output_fru_with_sdr (state_data,
				output_count,
				sdr_record,
				sdr_record_len,
				0) < 0)
	goto cleanup;

      if (ipmi_ctx_set_target (state_data->ipmi_ctx, NULL, NULL) < 0)
	{
	  pstdout_fprintf (state_data->pstate,
			   stderr,
			   "ipmi_ctx_set_target: %s\n",
			   ipmi_ctx_errormsg (state_data->ipmi_ctx));
	  goto cleanup;
	}
    }

 out:
  rv = 0;
 cleanup:
  return (rv);
}

static int
_find_device_id_fru_cb (ipmi_fru_state_data_t *state_data,
			unsigned int *output_count,
			const void *sdr_record,
			unsigned int sdr_record_len,
			uint8_t record_type,
			void *arg)
{
  struct ipmi_fru_sdr_find_data *find_data;
  int rv = -1;

  assert (state_data);
  assert (output_count);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (record_type == IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD
	  || record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD);
  assert (arg);

  find_data = (struct ipmi_fru_sdr_find_data *)arg;

  if (record_type == IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD)
    {
      uint8_t logical_physical_fru_device, logical_fru_device_device_slave_address;

      if (ipmi_sdr_parse_fru_device_locator_parameters (state_data->sdr_ctx,
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
			   ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
	  goto cleanup;
	}
      
      /* Check 'found' flag too - some SDRs list the device 0 twice */
      if (logical_physical_fru_device
	  && find_data->device_id == logical_fru_device_device_slave_address
	  && !find_data->found)
	{
	  if (_output_fru_with_sdr (state_data,
				    output_count,
				    sdr_record,
				    sdr_record_len,
				    logical_fru_device_device_slave_address) < 0)
	    goto cleanup;
	  
	  find_data->found = 1;
	}
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
_loop_sdr_callback (ipmi_sdr_ctx_t sdr_ctx,
		    uint8_t record_type,
		    const void *sdr_record,
		    unsigned int sdr_record_len,
		    void *sdr_arg)
{
  struct ipmi_fru_sdr_callback *sdr_callback_arg;
  ipmi_fru_state_data_t *state_data;

  assert (sdr_ctx);
  assert (sdr_record);
  assert (sdr_record_len);
  assert (sdr_arg);

  sdr_callback_arg = (struct ipmi_fru_sdr_callback *)sdr_arg;
  state_data = sdr_callback_arg->state_data;

  if (record_type != IPMI_SDR_FORMAT_FRU_DEVICE_LOCATOR_RECORD
      && record_type != IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD)
    return (0);
      
  if (!state_data->prog_data->args->bridge_fru
      && record_type == IPMI_SDR_FORMAT_MANAGEMENT_CONTROLLER_DEVICE_LOCATOR_RECORD)
    return (0);

  return (sdr_callback_arg->fru_cb (state_data,
				    sdr_callback_arg->output_count,
				    sdr_record,
				    sdr_record_len,
				    record_type,
				    sdr_callback_arg->arg));
}

static int
_loop_sdr (ipmi_fru_state_data_t *state_data,
	   unsigned int *output_count,
	   ipmi_fru_sdr_callback fru_cb,
	   void *fru_arg)
{
  struct ipmi_fru_sdr_callback sdr_callback_arg;
  int rv = -1;

  assert (state_data);
  assert (output_count);
  assert (fru_cb);

  sdr_callback_arg.state_data = state_data;
  sdr_callback_arg.output_count = output_count;
  sdr_callback_arg.fru_cb = fru_cb;
  sdr_callback_arg.arg = fru_arg;

  if (ipmi_sdr_cache_iterate (state_data->sdr_ctx,
			      _loop_sdr_callback,
			      &sdr_callback_arg) < 0)
    {
      pstdout_fprintf (state_data->pstate,
		       stderr,
		       "ipmi_sdr_cache_iterate: %s\n",
		       ipmi_sdr_ctx_errormsg (state_data->sdr_ctx));
      goto cleanup;
    }

  rv = 0;
 cleanup:
  return (rv);
}

static int
run_cmd_args (ipmi_fru_state_data_t *state_data)
{
  struct ipmi_fru_arguments *args;
  struct ipmi_fru_sdr_find_data find_data;
  unsigned int output_count = 0;
  int rv = -1;

  assert (state_data);

  args = state_data->prog_data->args;

  assert (!args->common_args.flush_cache);

  if (args->common_args.ignore_sdr_cache)
    {
      /* no SDR?  This is all you get :-) */
      if (_output_fru (state_data,
                       &output_count,
                       IPMI_FRU_DEVICE_ID_DEFAULT,
                       IPMI_FRU_DEFAULT_DEVICE_ID_STRING) < 0)
        goto cleanup;
      return (0);
    }
  else
    {
      if (sdr_cache_create_and_load (state_data->sdr_ctx,
				     state_data->pstate,
				     state_data->ipmi_ctx,
				     state_data->hostname,
				     &state_data->prog_data->args->common_args) < 0)
	goto cleanup;
    }

  if (args->interpret_oem_data)
    {
      if (ipmi_get_oem_data (state_data->pstate,
                             state_data->ipmi_ctx,
                             &state_data->oem_data) < 0)
        goto cleanup;

      if (ipmi_fru_ctx_set_manufacturer_id (state_data->fru_ctx,
					    state_data->oem_data.manufacturer_id) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_fru_ctx_set_manufacturer_id: %s\n",
                           ipmi_fru_ctx_errormsg (state_data->fru_ctx));
          goto cleanup;
        }

      if (ipmi_fru_ctx_set_product_id (state_data->fru_ctx,
				       state_data->oem_data.product_id) < 0)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ipmi_fru_ctx_set_product_id: %s\n",
                           ipmi_fru_ctx_errormsg (state_data->fru_ctx));
          goto cleanup;
        }
    }

  if (args->device_id_set)
    {
      find_data.device_id = args->device_id;
      find_data.found = 0;

      if (_loop_sdr (state_data,
		     &output_count,
		     _find_device_id_fru_cb,
		     &find_data) < 0)
	goto cleanup;

      if (!find_data.found)
	{
	  if (find_data.device_id == IPMI_FRU_DEVICE_ID_DEFAULT)
	    {
	      if (_output_fru (state_data,
			       &output_count,
			       IPMI_FRU_DEVICE_ID_DEFAULT,
			       IPMI_FRU_DEFAULT_DEVICE_ID_STRING) < 0)
		goto cleanup;
	    }
	  else
	    {
	      pstdout_fprintf (state_data->pstate,
			       stderr,
			       "device id not found\n");
	    }
	}

      goto out;
    }
  else
    {
      /* We always print out the default one first */
      find_data.device_id = IPMI_FRU_DEVICE_ID_DEFAULT;
      find_data.found = 0;

      if (_loop_sdr (state_data,
		     &output_count,
		     _find_device_id_fru_cb,
		     &find_data) < 0)
	goto cleanup;

      /* It's ok if this one isn't found in the SDR, use a generic
       * output
       */
      if (!find_data.found)
	{
	  if (_output_fru (state_data,
			   &output_count,
			   IPMI_FRU_DEVICE_ID_DEFAULT,
			   IPMI_FRU_DEFAULT_DEVICE_ID_STRING) < 0)
	    goto cleanup;
	}

      /* print the rest */
      if (_loop_sdr (state_data,
		     &output_count,
		     _print_except_default_fru_cb,
		     NULL) < 0)
	goto cleanup;
    }

 out:
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
  int exit_code = EXIT_FAILURE;
  unsigned int flags = 0;

  assert (pstate);
  assert (arg);

  prog_data = (ipmi_fru_prog_data_t *)arg;

  if (prog_data->args->common_args.flush_cache)
    {
      if (sdr_cache_flush_cache (pstate,
                                 hostname,
                                 &prog_data->args->common_args) < 0)
	return (EXIT_FAILURE);
      return (EXIT_SUCCESS);
    }

  memset (&state_data, '\0', sizeof (ipmi_fru_state_data_t));
  state_data.prog_data = prog_data;
  state_data.pstate = pstate;
  state_data.hostname = (char *)hostname;

  if (!(state_data.ipmi_ctx = ipmi_open (prog_data->progname,
					 hostname,
					 &(prog_data->args->common_args),
					 state_data.pstate)))
    goto cleanup;

  if (!(state_data.fru_ctx = ipmi_fru_ctx_create (state_data.ipmi_ctx)))
    {
      pstdout_perror (pstate, "ipmi_fru_ctx_create()");
      goto cleanup;
    }
  
  if (hostname)
    {
      if (ipmi_fru_ctx_set_debug_prefix (state_data.fru_ctx,
					 hostname) < 0)
	pstdout_fprintf (pstate,
			 stderr,
			 "ipmi_fru_ctx_set_debug_prefix: %s\n",
			 ipmi_fru_ctx_errormsg (state_data.fru_ctx));
    }
      
  if (state_data.prog_data->args->common_args.debug)
    flags |= IPMI_FRU_FLAGS_DEBUG_DUMP;
  if (state_data.prog_data->args->skip_checks)
    flags |= IPMI_FRU_FLAGS_SKIP_CHECKSUM_CHECKS;
  if (state_data.prog_data->args->interpret_oem_data)
    flags |= IPMI_FRU_FLAGS_INTERPRET_OEM_DATA;
  
  if (flags)
    {
      if (ipmi_fru_ctx_set_flags (state_data.fru_ctx, flags) < 0)
	{
	  pstdout_fprintf (pstate,
			   stderr,
			   "ipmi_fru_ctx_set_flags: %s\n",
			   ipmi_fru_ctx_strerror (ipmi_fru_ctx_errnum (state_data.fru_ctx)));
	  goto cleanup;
	}
    }

  if (!(state_data.sdr_ctx = ipmi_sdr_ctx_create ()))
    {
      pstdout_perror (pstate, "ipmi_sdr_ctx_create()");
      goto cleanup;
    }

  if (run_cmd_args (&state_data) < 0)
    goto cleanup;

  exit_code = EXIT_SUCCESS;
 cleanup:
  ipmi_fru_ctx_destroy (state_data.fru_ctx);
  ipmi_sdr_ctx_destroy (state_data.sdr_ctx);
  ipmi_ctx_close (state_data.ipmi_ctx);
  ipmi_ctx_destroy (state_data.ipmi_ctx);
  return (exit_code);
}

int
main (int argc, char **argv)
{
  ipmi_fru_prog_data_t prog_data;
  struct ipmi_fru_arguments cmd_args;
  int hosts_count;
  int rv;

  ipmi_disable_coredump ();

  memset (&prog_data, '\0', sizeof (ipmi_fru_prog_data_t));
  prog_data.progname = argv[0];
  ipmi_fru_argp_parse (argc, argv, &cmd_args);
  prog_data.args = &cmd_args;

  /* Special case, if user specified workaround via flags instead of option */
  if (prog_data.args->common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SKIP_CHECKS)
    prog_data.args->skip_checks = 1;

  if ((hosts_count = pstdout_setup (&(prog_data.args->common_args.hostname),
				    &(prog_data.args->common_args))) < 0)
    return (EXIT_FAILURE);

  if (!hosts_count)
    return (EXIT_SUCCESS);

  /* We don't want caching info to output when are doing ranged output */
  if (hosts_count > 1)
    prog_data.args->common_args.quiet_cache = 1;

  if ((rv = pstdout_launch (prog_data.args->common_args.hostname,
                            _ipmi_fru,
                            &prog_data)) < 0)
    {
      fprintf (stderr,
               "pstdout_launch: %s\n",
               pstdout_strerror (pstdout_errnum));
      return (EXIT_FAILURE);
    }

  return (rv);
}
