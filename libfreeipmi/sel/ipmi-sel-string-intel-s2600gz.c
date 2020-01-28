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
#include "freeipmi/spec/ipmi-iana-enterprise-numbers-spec.h"
#include "freeipmi/spec/ipmi-product-id-spec.h"

#include "ipmi-sel-common.h"
#include "ipmi-sel-defs.h"
#include "ipmi-sel-string.h"
#include "ipmi-sel-string-intel-node-manager.h"
#include "ipmi-sel-string-intel-xeon-common.h"
#include "ipmi-sel-string-intel-xeon-broadwell-common.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

int
sel_string_output_intel_s2600gz_sensor_name (ipmi_sel_ctx_t ctx,
                                             struct ipmi_sel_entry *sel_entry,
                                             uint8_t sel_record_type,
                                             char *buf,
                                             unsigned int buflen,
                                             unsigned int flags,
                                             unsigned int *wlen,
                                             struct ipmi_sel_system_event_record_data *system_event_record_data,
                                             int *oem_rv)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (oem_rv);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ);

  if ((ret = sel_string_output_intel_xeon_sensor_name (ctx,
                                                       sel_entry,
                                                       sel_record_type,
                                                       buf,
                                                       buflen,
                                                       flags,
                                                       wlen,
                                                       system_event_record_data,
                                                       oem_rv)) < 0)
    return (-1);

  if (ret)
    return (1);

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_s2600gz_event_data1_class_oem (ipmi_sel_ctx_t ctx,
                                                       struct ipmi_sel_entry *sel_entry,
                                                       uint8_t sel_record_type,
                                                       char *tmpbuf,
                                                       unsigned int tmpbuflen,
                                                       unsigned int flags,
                                                       unsigned int *wlen,
                                                       struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ);

  if ((ret = sel_string_output_intel_xeon_event_data1_class_oem (ctx,
                                                                 sel_entry,
                                                                 sel_record_type,
                                                                 tmpbuf,
                                                                 tmpbuflen,
                                                                 flags,
                                                                 wlen,
                                                                 system_event_record_data)) < 0)
    return (-1);

  if (ret)
    return (1);

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_s2600gz_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
                                                          struct ipmi_sel_entry *sel_entry,
                                                          uint8_t sel_record_type,
                                                          char *tmpbuf,
                                                          unsigned int tmpbuflen,
                                                          unsigned int flags,
                                                          unsigned int *wlen,
                                                          struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ);

  if ((ret = sel_string_output_intel_xeon_event_data2_discrete_oem (ctx,
                                                                    sel_entry,
                                                                    sel_record_type,
                                                                    tmpbuf,
                                                                    tmpbuflen,
                                                                    flags,
                                                                    wlen,
                                                                    system_event_record_data)) < 0)
    return (-1);

  if (ret)
    return (1);

  if ((ret = sel_string_output_intel_xeon_broadwell_event_data2_discrete_oem (ctx,
                                                                              sel_entry,
                                                                              sel_record_type,
                                                                              tmpbuf,
                                                                              tmpbuflen,
                                                                              flags,
                                                                              wlen,
                                                                              system_event_record_data)) < 0)
    return (-1);

  if (ret)
    return (1);

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_s2600gz_event_data2_class_oem (ipmi_sel_ctx_t ctx,
                                                       struct ipmi_sel_entry *sel_entry,
                                                       uint8_t sel_record_type,
                                                       char *tmpbuf,
                                                       unsigned int tmpbuflen,
                                                       unsigned int flags,
                                                       unsigned int *wlen,
                                                       struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ);

  if ((ret = sel_string_output_intel_xeon_event_data2_class_oem (ctx,
                                                                 sel_entry,
                                                                 sel_record_type,
                                                                 tmpbuf,
                                                                 tmpbuflen,
                                                                 flags,
                                                                 wlen,
                                                                 system_event_record_data)) < 0)
    return (-1);

  if (ret)
    return (1);

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_s2600gz_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
                                                          struct ipmi_sel_entry *sel_entry,
                                                          uint8_t sel_record_type,
                                                          char *tmpbuf,
                                                          unsigned int tmpbuflen,
                                                          unsigned int flags,
                                                          unsigned int *wlen,
                                                          struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ);

  if ((ret = sel_string_output_intel_xeon_event_data3_discrete_oem (ctx,
                                                                    sel_entry,
                                                                    sel_record_type,
                                                                    tmpbuf,
                                                                    tmpbuflen,
                                                                    flags,
                                                                    wlen,
                                                                    system_event_record_data)) < 0)
    return (-1);

  if (ret)
    return (1);

  if ((ret = sel_string_output_intel_xeon_broadwell_event_data2_discrete_oem (ctx,
                                                                              sel_entry,
                                                                              sel_record_type,
                                                                              tmpbuf,
                                                                              tmpbuflen,
                                                                              flags,
                                                                              wlen,
                                                                              system_event_record_data)) < 0)
    return (-1);

  if (ret)
    return (1);

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_intel_s2600gz_event_data3_class_oem (ipmi_sel_ctx_t ctx,
                                                       struct ipmi_sel_entry *sel_entry,
                                                       uint8_t sel_record_type,
                                                       char *tmpbuf,
                                                       unsigned int tmpbuflen,
                                                       unsigned int flags,
                                                       unsigned int *wlen,
                                                       struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ);

  if ((ret = sel_string_output_intel_xeon_event_data3_class_oem (ctx,
                                                                 sel_entry,
                                                                 sel_record_type,
                                                                 tmpbuf,
                                                                 tmpbuflen,
                                                                 flags,
                                                                 wlen,
                                                                 system_event_record_data)) < 0)
    return (-1);

  if (ret)
    return (1);

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
sel_string_output_intel_s2600gz_event_data2_event_data3 (ipmi_sel_ctx_t ctx,
                                                         struct ipmi_sel_entry *sel_entry,
                                                         uint8_t sel_record_type,
                                                         char *buf,
                                                         unsigned int buflen,
                                                         unsigned int flags,
                                                         unsigned int *wlen,
                                                         struct ipmi_sel_system_event_record_data *system_event_record_data,
                                                         int *oem_rv)
{
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_INTEL);
  assert (sel_entry);
  assert (buf);
  assert (buflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (oem_rv);
  assert (ctx->product_id == IPMI_INTEL_PRODUCT_ID_S2600GZ);

  if ((ret = sel_string_output_intel_xeon_broadwell_event_data2_event_data3 (ctx,
                                                                             sel_entry,
                                                                             sel_record_type,
                                                                             buf,
                                                                             buflen,
                                                                             flags,
                                                                             wlen,
                                                                             system_event_record_data,
                                                                             oem_rv)) < 0)
    return (-1);

  if (ret)
    return (1);

  return (0);
}

struct sel_string_oem sel_string_oem_intel_s2600gz =
  {
    sel_string_output_intel_s2600gz_sensor_name,
    NULL,
    sel_string_output_intel_s2600gz_event_data1_class_oem,
    NULL,
    sel_string_output_intel_s2600gz_event_data2_discrete_oem,
    sel_string_output_intel_s2600gz_event_data2_class_oem,
    NULL,
    sel_string_output_intel_s2600gz_event_data3_discrete_oem,
    sel_string_output_intel_s2600gz_event_data3_class_oem,
    sel_string_output_intel_s2600gz_event_data2_event_data3,
    NULL,
    NULL,
  };
