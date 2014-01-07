/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "freeipmi/cmds/ipmi-device-global-cmds.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/spec/ipmi-cmd-spec.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-fill-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_cmd_get_device_id_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_device_id_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "device_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "device_revision.revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* binary encoded */
    { 3, "device_revision.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "device_revision.sdr_support", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "firmware_revision1.major_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "firmware_revision1.device_available", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "firmware_revision2.minor_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* BCD encoded */
    { 4, "ipmi_version_major", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "ipmi_version_minor", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "additional_device_support.sensor_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "additional_device_support.sdr_repository_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "additional_device_support.sel_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "additional_device_support.fru_inventory_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "additional_device_support.ipmb_event_receiver", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "additional_device_support.ipmb_event_generator", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "additional_device_support.bridge", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "additional_device_support.chassis_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 20, "manufacturer_id.id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 4, "manufacturer_id.reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "product_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "auxiliary_firmware_revision_information", FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_cold_reset_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_cold_reset_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_warm_reset_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_warm_reset_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_acpi_power_state_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "system_power_state_enumeration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "set_system_power_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "device_power_state_enumeration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "set_device_power_state", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_set_acpi_power_state_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_acpi_power_state_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_acpi_power_state_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 7, "system_power_state_enumeration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 7, "device_power_state_enumeration", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_self_test_results_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* note: bitfield results only if self_test_results == 0x57.  See
 * specification for more information
 */
fiid_template_t tmpl_cmd_get_self_test_results_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "self_test_result", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "controller_operation_firmware_corrupted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "controller_update_boot_block_firmware_corrupted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "internal_use_area_of_bmc_fru_corrupted", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "sdr_repository_empty", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "ipmb_signal_lines_do_not_respond", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "cannot_access_bmc_fru_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "cannot_access_sdr_repository", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 1, "cannot_access_sel_device", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_device_guid_rq =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

fiid_template_t tmpl_cmd_get_device_guid_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 128, "guid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

/* achu: format in IPMI spec differs from the format in the "Wired for
   Management Baseline" 2.0 document.  In the IPMI spec there is a 2 byte
   field called "clock_seq_and_reserved", and in "Wired for Management
   Baseline" there is a 1 byte field called
   "clock_seq_hi_and_reserved" and a 1 byte field called
   "clock_seq_low".  I am going with the "Wired for Management
   Baseline" format under the assumption the IPMI spec has a typo.
*/
fiid_template_t tmpl_cmd_get_device_guid_format_rs =
  {
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED | FIID_FIELD_MAKES_PACKET_SUFFICIENT},
    { 48, "node", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* LS byte first */
    { 8, "clock_seq_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "clock_seq_hi_and_reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 16, "time_high_and_version", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* LS byte first */
    { 16, "time_mid", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* LS byte first */
    { 32, "time_low", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},  /* LS byte first */
    { 0, "", 0}
  };

int
fill_cmd_get_device_id (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_device_id_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_DEVICE_ID);
  return (0);
}

int
fill_cmd_cold_reset (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_cold_reset_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_COLD_RESET);
  return (0);
}

int
fill_cmd_warm_reset (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_warm_reset_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_WARM_RESET);
  return (0);
}

int
fill_cmd_set_acpi_power_state (uint8_t system_power_state_enumeration,
                               uint8_t set_system_power_state,
                               uint8_t device_power_state_enumeration,
                               uint8_t set_device_power_state,
                               fiid_obj_t obj_cmd_rq)
{

  if (!IPMI_ACPI_SET_SYSTEM_POWER_STATE_VALID (set_system_power_state)
      || (set_system_power_state == IPMI_ACPI_SET_SYSTEM_POWER_STATE_SET_SYSTEM_POWER_STATE
          && !IPMI_ACPI_SYSTEM_POWER_STATE_VALID (system_power_state_enumeration))
      || !IPMI_ACPI_SET_DEVICE_POWER_STATE_VALID (set_device_power_state)
      || (set_device_power_state == IPMI_ACPI_SET_DEVICE_POWER_STATE_SET_DEVICE_POWER_STATE
          && !IPMI_ACPI_DEVICE_POWER_STATE_VALID (device_power_state_enumeration))
      || !fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_set_acpi_power_state_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_SET_ACPI_POWER_STATE);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "system_power_state_enumeration", system_power_state_enumeration);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "set_system_power_state", set_system_power_state);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "device_power_state_enumeration", device_power_state_enumeration);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "set_device_power_state", set_device_power_state);
  return (0);
}

int
fill_cmd_get_acpi_power_state (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_acpi_power_state_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_ACPI_POWER_STATE);
  return (0);
}

int
fill_cmd_get_self_test_results (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_self_test_results_rq) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_SELF_TEST_RESULTS);
  return (0);
}

int
fill_cmd_get_device_guid (fiid_obj_t obj_cmd_rq)
{
  if (!fiid_obj_valid (obj_cmd_rq))
    {
      SET_ERRNO (EINVAL);
      return (-1);
    }

  if (FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_device_guid_rq) < 0
      && FIID_OBJ_TEMPLATE_COMPARE (obj_cmd_rq, tmpl_cmd_get_device_guid_format_rs) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  FILL_FIID_OBJ_CLEAR (obj_cmd_rq);
  FILL_FIID_OBJ_SET (obj_cmd_rq, "cmd", IPMI_CMD_GET_DEVICE_GUID);
  return (0);
}
