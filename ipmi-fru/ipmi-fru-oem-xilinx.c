/*
 * Copyright (C) 2022, Advanced Micro Devices, Inc.
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

#include "ipmi-fru_.h"
#include "ipmi-fru-oem-xilinx.h"

#include "freeipmi-portability.h"

static char *
_version_str (uint8_t version)
{
  switch (version)
    {
    case IPMI_FRU_OEM_XILINX_MAC_ID_VERSION_BOARD:
      return "Board";
    case IPMI_FRU_OEM_XILINX_MAC_ID_VERSION_SYSCTL:
      return "System Controller";
    case IPMI_FRU_OEM_XILINX_MAC_ID_VERSION_MODULE:
      return "Module";
    case IPMI_FRU_OEM_XILINX_MAC_ID_VERSION_DUT_MAC:
      return "DUT - MAC";
    case IPMI_FRU_OEM_XILINX_MAC_ID_VERSION_DUT_ETHERCAT:
      return "DUT - EtherCAT";
    default:
      return "";
    }

  return (NULL);                /* NOT REACHED */
}

int
ipmi_fru_oem_xilinx_oem_record (ipmi_fru_state_data_t *state_data,
                                uint8_t record_type_id,
                                uint32_t manufacturer_id,
                                uint8_t *oem_data,
                                unsigned int oem_data_len)
{
  assert (state_data);
  assert (manufacturer_id == IPMI_IANA_ENTERPRISE_ID_XILINX);
  assert (oem_data);

  /* The MAC_ID record type ID is 0xD2. The MAC ID record consists of a 1 byte
   * version ID followed by one or more 6-byte MAC addresses. If the MAC ID
   * version is set to "DUT - EtherCAT", a 4-byte EtherCAT ID is used instead of
   * a 6-byte MAC address.
   */
  if (record_type_id == IPMI_FRU_OEM_XILINX_MAC_ID && oem_data_len)
    {
      uint8_t version = oem_data[0];
      unsigned int len = oem_data_len - 1;

      pstdout_printf (state_data->pstate,
                      "  FRU OEM MAC Version: %s (%xh)\n",
                      _version_str(version),
                      version);

      /* The MAC_ID record can hold multiple MAC addresses that are 6 bytes long
       * each if version is set to 0x31.
       */
      if ((version == IPMI_FRU_OEM_XILINX_MAC_ID_VERSION_BOARD ||
           version == IPMI_FRU_OEM_XILINX_MAC_ID_VERSION_SYSCTL ||
           version == IPMI_FRU_OEM_XILINX_MAC_ID_VERSION_MODULE ||
           version == IPMI_FRU_OEM_XILINX_MAC_ID_VERSION_DUT_MAC ) &&
          (len % 6) == 0)
        {
          unsigned int i, j, start, stop;
          unsigned int mac_cnt = len / 6;

          for (j = 0; j < mac_cnt; j++)
            {
              pstdout_printf (state_data->pstate, "  FRU OEM MAC ID %d: ", j);

              start = j*6 + 1;
              stop = start + 5;

              for (i = start; i < stop; i++)
                {
                  pstdout_printf (state_data->pstate, "%02x:", oem_data[i]);
                }

              pstdout_printf (state_data->pstate, "%02x\n", oem_data[i]);
            }

          return (1);
        }

      /* The MAC_ID record holds one EtherCAT ID that is 4 bytes long if version
       * is set to 0x32. The assigned EtherCAT ID for Xilinx is 0x0000056F.
       */
      if (version == IPMI_FRU_OEM_XILINX_MAC_ID_VERSION_DUT_ETHERCAT &&
          len == 4)
        {
          unsigned int i;

          pstdout_printf (state_data->pstate, "  FRU OEM EtherCAT ID: 0x");

          for (i = 1; i < len+1; i++)
            {
              pstdout_printf (state_data->pstate, "%02X", oem_data[i]);
            }

          pstdout_printf (state_data->pstate, "\n");

          return (1);
        }
    }

  /* The free form data record type ID is 0xD3. It consists of one or more
   * fields where each field is split into N byte identifier and M byte data
   * followed by a 0x00 end of field delimiter. The below code parses the free
   * form record and prints each field on a new line prefixed with 'FRU OEM '.
   */
  if (record_type_id == IPMI_FRU_OEM_XILINX_FREE_FORM && oem_data_len)
    {
      unsigned int i;
      unsigned int new_field = 1;

      for (i = 0; i < oem_data_len; i++)
        {
          /* 0x00 marks the end of the field */
          if (oem_data[i] == 0)
            {
              if (new_field == 0)
                {
                  pstdout_printf (state_data->pstate, "\n");
                }
              new_field = 1;
              continue;
            }

          /* Start of a new field */
          if (new_field == 1)
            {
              new_field = 0;
              pstdout_printf (state_data->pstate, "  FRU OEM ");
            }

          pstdout_printf (state_data->pstate, "%c", oem_data[i]);
        }

      return (1);
    }

  return (0);
}
