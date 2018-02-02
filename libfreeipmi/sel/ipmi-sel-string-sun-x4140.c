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
#include "ipmi-sel-trace.h"
#include "ipmi-sel-util.h"

#include "freeipmi-portability.h"

int
_sel_string_output_sun_x4140_event_data3_fru_position_number (ipmi_sel_ctx_t ctx,
                                                              char *tmpbuf,
                                                              unsigned int tmpbuflen,
                                                              struct ipmi_sel_system_event_record_data *system_event_record_data)
{
  assert (ctx);
  assert (ctx->magic == IPMI_SEL_CTX_MAGIC);
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUN_MICROSYSTEMS);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);

  /* From Sun:
   *
   * "In general, the data stored in SEL EvtData3 (OEM) field for our
   * platforms is the "FRU Position Number".  The position number is used
   * internally and consumed by our diagnostic routines.
   *
   * The position number is context sensitive to the  name.
   *
   * e.g. For example, Sensor "/SYS/PS0/FAN_FAULT" and
   * "/SYS/PS0/PRSNT" for /SYS/PS0/PRSNT, EvtData3 field would contain
   * the sensor logical number while /SYS/PS0/FAN_FAULT, the upper 5
   * bits of EvtData3 is the Fan Module and the lower 3 bits the Fan
   * Number."
   */

  snprintf (tmpbuf,
            tmpbuflen,
            "FRU Position Number = %02Xh",
            system_event_record_data->event_data3);

  return (0);
}

/* return (0) - no OEM match
 * return (1) - OEM match
 * return (-1) - error, cleanup and return error
 */
int
sel_string_output_sun_x4140_event_data3_threshold_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUN_MICROSYSTEMS);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  assert (ctx->product_id == IPMI_SUN_MICROSYSTEMS_PRODUCT_ID_X4140);

  /*
   * Sun X4140
   */

  if ((ret = _sel_string_output_sun_x4140_event_data3_fru_position_number (ctx,
                                                                           tmpbuf,
                                                                           tmpbuflen,
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
sel_string_output_sun_x4140_event_data3_discrete_oem (ipmi_sel_ctx_t ctx,
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
  assert (ctx->manufacturer_id == IPMI_IANA_ENTERPRISE_ID_SUN_MICROSYSTEMS);
  assert (sel_entry);
  assert (tmpbuf);
  assert (tmpbuflen);
  assert (!(flags & ~IPMI_SEL_STRING_FLAGS_MASK));
  assert (flags & IPMI_SEL_STRING_FLAGS_INTERPRET_OEM_DATA);
  assert (wlen);
  assert (system_event_record_data);
  assert (system_event_record_data->event_data3_flag == IPMI_SEL_EVENT_DATA_OEM_CODE);
  assert (ctx->product_id == IPMI_SUN_MICROSYSTEMS_PRODUCT_ID_X4140);

  /*
   * Sun X4140
   */

  if ((ret = _sel_string_output_sun_x4140_event_data3_fru_position_number (ctx,
                                                                           tmpbuf,
                                                                           tmpbuflen,
                                                                           system_event_record_data)) < 0)
    return (-1);

  if (ret)
    return (1);

  return (0);
}

struct sel_string_oem sel_string_oem_sun_x4140 =
  {
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    sel_string_output_sun_x4140_event_data3_threshold_oem,
    sel_string_output_sun_x4140_event_data3_discrete_oem,
    NULL,
    NULL,
    NULL,
    NULL,
  };
