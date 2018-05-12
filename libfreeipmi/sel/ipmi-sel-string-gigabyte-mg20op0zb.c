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
#include "freeipmi/spec/ipmi-sensor-and-event-code-tables-spec.h"
#include "freeipmi/spec/ipmi-sensor-types-spec.h"
#include "freeipmi/spec/oem/ipmi-sensor-and-event-code-tables-oem-gigabyte-spec.h"

#include "ipmi-sel-common.h"
#include "ipmi-sel-defs.h"
#include "ipmi-sel-string.h"
#include "ipmi-sel-string-gigabyte-common.h"
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_gigabyte_mg20op0zb_event_data2_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_GIGABYTE);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data2_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  assert (ctx->product_id == IPMI_GIGABYTE_PRODUCT_ID_MG20_OP0_ZB);

  /*
   * Gigabyte MG20-OP0-ZB
   */
  if ((ret = sel_string_output_gigabyte_common_event_data2_discrete_oem (ctx,
                                                                         sel_entry,
                                                                         sel_record_type,
                                                                         tmpbuf,
                                                                         tmpbuflen,
                                                                         flags,
                                                                         wlen,
                                                                         system_event_record_data)) < 0)
    return (-1);

  if (ret)
    return (ret);

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_gigabyte_mg20op0zb_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_GIGABYTE);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  assert (ctx->product_id == IPMI_GIGABYTE_PRODUCT_ID_MG20_OP0_ZB);

  /*
   * Gigabyte MG20-OP0-ZB
   */

  if ((ret = sel_string_output_gigabyte_common_event_data3_discrete_oem (ctx,
                                                                         sel_entry,
                                                                         sel_record_type,
                                                                         tmpbuf,
                                                                         tmpbuflen,
                                                                         flags,
                                                                         wlen,
                                                                         system_event_record_data)) < 0)
    return (-1);

  if (ret)
    return (ret);

  return (0);
}

struct sel_string_oem sel_string_oem_gigabyte_mg20op0zb =
  {
    NULL,
    NULL,
    NULL,
    NULL,
    sel_string_output_gigabyte_mg20op0zb_event_data2_discrete_oem,
    NULL,
    NULL,
    sel_string_output_gigabyte_mg20op0zb_event_data3_discrete_oem,
    NULL,
    NULL,
    NULL,
    NULL,
  };
