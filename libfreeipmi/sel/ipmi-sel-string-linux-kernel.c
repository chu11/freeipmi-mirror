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
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "freeipmi/sel/ipmi-sel.h"

#include "freeipmi/record-format/ipmi-sel-record-format.h"
#include "freeipmi/record-format/oem/ipmi-sel-oem-linux-kernel-record-format.h"
#include "freeipmi/spec/ipmi-event-reading-type-code-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"
#include "freeipmi/spec/oem/ipmi-slave-address-oem-linux-kernel-spec.h"

#include "ipmi-sel-common.h"
#include "ipmi-sel-defs.h"
#include "ipmi-sel-string.h"
#include "ipmi-sel-string-linux-kernel.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

/* achu
 *
 * Needless to say, this is not for a specific OEM, but for generic panics from the Linux kernel.  Any vendor applies.
 */

#define LINUX_KERNEL_EVENT_BUFFER_LENGTH 4096

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 *
 * in oem_rv, return
 * 0 - continue on
 * 1 - buffer full, return full buffer to user
 */
int
sel_string_output_linux_kernel_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
                                                        struct ipmi_sel_entry *sel_entry,
                                                        uint8_t sel_record_type,
                                                        char *buf,
                                                        unsigned int buflen,
                                                        unsigned int flags,
                                                        unsigned int *wlen,
                                                        struct ipmi_sel_system_event_record_data *system_event_record_data,
                                                        int *oem_rv)
{
  char panic_str[LINUX_KERNEL_EVENT_BUFFER_LENGTH];

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (oem_rv);

  assert (system_event_record_data->generator_id == IPMI_SLAVE_ADDRESS_OEM_LINUX_KERNEL);
  assert (system_event_record_data->event_type_code == IPMI_EVENT_READING_TYPE_CODE_SENSOR_SPECIFIC);
  assert (system_event_record_data->sensor_type == IPMI_SENSOR_TYPE_OS_CRITICAL_STOP);
  assert (system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  /* Format of Linux kernel panic is
   *
   * Sensor Number = First byte of panic
   * Event Data2 = Second byte of panic string
   * Event Data3 = Third byte of panic string
   */

  memset (panic_str, '\0', LINUX_KERNEL_EVENT_BUFFER_LENGTH);

  panic_str[0] = system_event_record_data->sensor_number;
  panic_str[1] = system_event_record_data->event_data2;
  panic_str[2] = system_event_record_data->event_data3;

  if (sel_string_snprintf (buf,
                           buflen,
                           wlen,
                           "%s",
                           panic_str))
    (*oem_rv) = 1;
  else
    (*oem_rv) = 0;

  return (1);
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
sel_string_output_linux_kernel_oem_record_data (ipmi_sel_ctx_t ctx,
                                                struct ipmi_sel_entry *sel_entry,
                                                uint8_t sel_record_type,
                                                char *buf,
                                                unsigned int buflen,
                                                unsigned int flags,
                                                unsigned int *wlen,
                                                int *oem_rv)
{
  char panic_str[LINUX_KERNEL_EVENT_BUFFER_LENGTH];

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ipmi_sel_record_type_class (sel_record_type) == IPMI_SEL_RECORD_TYPE_CLASS_TIMESTAMPED_OEM_RECORD
          || ipmi_sel_record_type_class (sel_record_type) == IPMI_SEL_RECORD_TYPE_CLASS_NON_TIMESTAMPED_OEM_RECORD);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (oem_rv);

  assert (sel_record_type == IPMI_SEL_RECORD_TYPE_NON_TIMESTAMPED_OEM_LINUX_KERNEL_PANIC);

  /* Format of Linux kernel panic is
   *
   * Byte 4 - Slave Address
   * Byte 5 - Sequence Number
   * Byte 6-16 - kernel panic string data
   */

  memset (panic_str, '\0', LINUX_KERNEL_EVENT_BUFFER_LENGTH);

  memcpy (panic_str, &sel_entry->sel_event_record[5], 11);

  if (sel_string_snprintf (buf,
                           buflen,
                           wlen,
                           "%s",
                           panic_str))
    (*oem_rv) = 1;
  else
    (*oem_rv) = 0;

  return (1);
}
