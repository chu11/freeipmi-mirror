/*
 * Copyright (C) 2003-2015 FreeIPMI Core Team
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
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/sel/ipmi-sel.h"

#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"
#include "freeipmi/spec/ipmi-product-id-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"
#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-spec.h"
#include "freeipmi/spec/oem/ipmi-event-reading-type-code-oem-supermicro-spec.h"
#include "freeipmi/spec/oem/ipmi-sensor-and-event-code-tables-oem-supermicro-spec.h"
#include "freeipmi/spec/oem/ipmi-sensor-types-oem-supermicro-spec.h"

#include "ipmi-sel-common.h"
#include "ipmi-sel-defs.h"
#include "ipmi-sel-string.h"
#include "ipmi-sel-string-supermicro-common.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_supermicro_overheat_event_data1_class_oem (ipmi_sel_ctx_t ctx,
                                                             struct ipmi_sel_entry *sel_entry,
                                                             uint8_t sel_record_type,
                                                             char *tmpbuf,
                                                             unsigned int tmpbuflen,
                                                             unsigned int flags,
                                                             unsigned int *wlen,
                                                             struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUPERMICRO
          || ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUPERMICRO_WORKAROUND
          || ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_PEPPERCON
          || ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_MAGNUM_TECHNOLOGIES);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);

  /*
   * Supermicro X7DBR-3 (X7DBR_3)
   * Supermicro X7DB8
   * Supermicro X8DTN
   * Supermicro X7SBI-LN4 (X7SBI_LN4)
   * Supermicro X8DTH
   * Supermicro X8DTG
   * Supermicro X8DTU
   * Supermicro X8DT3-LN4F (X8DT3_LN4F)
   * Supermicro X8DTU-6+ (X8DTU_6PLUS)
   * Supermicro X8DTL
   * Supermicro X8DTL-3F (X8DTL_3F)
   * Supermicro X8SIL-F  (X8SIL_F)
   * Supermicro X9SCL
   * Supermicro X9SCM
   * Supermicro X8DTN+-F (X8DTNPLUS_F)
   * Supermicro X8SIE
   * Supermicro X9SCA-F-O (X9SCA_F_O)
   * Supermicro H8DGU-F (H8DGU_F)
   * Supermicro X9DRi-F (X9DRI_F)
   * Supermicro X9DRI-LN4F+ (X9DRI_LN4F_PLUS)
   * Supermicro X9SPU-F-O (X9SPU_F_O)
   * Supermicro X9SCM-iiF (X9SCM_IIF)
   *
   * Note: Early Supermicro motherboards used the "Peppercon" Manufacturer ID
   * Note: Some Supermicro motherboards are rebranded with random manufacturer IDs
   */

  /* achu: Via reverse engineering, see
   *
   * "Supermicro X8DTG-QF System Event Log" thread in late
   * January/early February 2012.
   */

  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_OEM_SUPERMICRO_GENERIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP
      && system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_OEM_SUPERMICRO_CPU_TEMP_SEL_OVERHEAT)
    {
      snprintf (tmpbuf,
                tmpbuflen,
                "Overheat");

      return (1);
    }

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 *
 * in oem_rv, return
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
int
sel_string_output_supermicro_dimm_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
                                                           struct ipmi_sel_entry *sel_entry,
                                                           uint8_t sel_record_type,
                                                           char *buf,
                                                           unsigned int buflen,
                                                           unsigned int flags,
                                                           unsigned int *wlen,
                                                           struct ipmi_sel_system_event_record_data *system_event_record_data,
                                                           int *oem_rv)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUPERMICRO);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (oem_rv);
  assert (ctx->product_id == IPMI_SUPERMICRO_PRODUCT_ID_X10SLMPLUS_F);

  /* OEM Interpretation
   *
   * Supermicro X10DRH
   * Supermicro X10DRH-LN4
   * Supermicro X10DRW-E
   * Supermicro X10DRW-N
   * Supermicro X10DRI
   * Supermicro X10DRI-T
   * Supermicro X10SLH_F
   * Supermicro X10SLM+-F
   * Supermicro X10SLL-F
   * Supermicro X10DRL-I
   * Supermicro X10SLM-F
   * Supermicro X10SRW-F
   * Supermicro X10SRI-F
   * Supermicro X10DRW-I
   * Supermicro X10SRL-F
   * Supermicro X10DDW-I
   * Supermicro X10DRG-HT
   */

  /* Decoding information provided by ipmiutil
   *
   * http://ipmiutil.sourceforge.net/
   *
   * Code in ipmiutil was as follows
   */
#if 0
#define NPAIRS  26
char rgpair[NPAIRS] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
...
  /* ver 2 method: 2A 80 = P1_DIMMB1 */
  /* SuperMicro says:
   *  pair: %c (data2 >> 4) + 0x40 + (data3 & 0x3) * 3, (='B')
   *  dimm: %c (data2 & 0xf) + 0x27,
   *  cpu:  %x (data3 & 0x03) + 1);
   */
  cpu = (b3 & 0x0F) + 1; /*0x80=CPU1, 0x81=CPU2*/
  pair = ((bdata & 0xF0) >> 4) - 1; /*0x10=pairA, 0x20=pairB*/
  if (pair < 0) pair = 0;
  if (pair > NPAIRS) pair = NPAIRS - 1;
  dimm = (bdata & 0x0F) - 9; /*0x0A=dimmX1, 0x0B=dimmX2*/
  if (dimm < 0)
    n = sprintf(desc,DIMM_UNKNOWN);  /* invalid */
  else
    n = sprintf(desc,"P%d_DIMM%c%d",cpu,rgpair[pair],dimm);
#endif
  /*
   * Note that code does not necessarily follow comments, I believe
   * that is b/c multiple ways to do this (i.e. create char or create
   * integer for output).  I'm going to follow comments.
   *
   * Data2
   * [7:4] - Pair character, add 0x40 to get ascii char for it and add CPU multiple as needed.
   * [3:0] - Dimm number base 0, add 0x27 to get ascii char for it
   *
   * Data3
   * [2:0] - CPU number base 0 (i.e. normally add 1 for output)
   */
  if (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC
      && system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_MEMORY
      && (system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_CORRECTABLE_MEMORY_ERROR
          || system_event_record_data->offset_from_event_reading_type_code == IPMI_SENSOR_TYPE_MEMORY_UNCORRECTABLE_MEMORY_ERROR)
      && system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE
      && system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE)
    {
      char pair, dimm;
      uint8_t cpu;

      pair = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_SUPERMICRO_PAIR_BITMASK);
      pair >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_SUPERMICRO_PAIR_SHIFT;

      dimm = (system_event_record_data->event_data2 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_SUPERMICRO_DIMM_BITMASK);
      dimm >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA2_OEM_SUPERMICRO_DIMM_SHIFT;

      cpu = (system_event_record_data->event_data3 & IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_SUPERMICRO_CPU_BITMASK);
      cpu >>= IPMI_SENSOR_TYPE_MEMORY_EVENT_DATA3_OEM_SUPERMICRO_CPU_SHIFT;

      pair += 0x40;
      pair += (cpu * 3);        /* multiplier for other CPUs */

      dimm += 0x27;

      cpu += 1;

      if (sel_string_snprintf (buf,
                               buflen,
                               wlen,
                               "DIMM%c%c(CPU%d)",
                               pair,
                               dimm,
                               cpu))
        (*oem_rv) = 1;
      else
        (*oem_rv) = 0;

      return (1);
    }

  return (0);
}
