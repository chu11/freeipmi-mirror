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

#ifndef IPMI_SEL_STRING_H
#define IPMI_SEL_STRING_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdint.h>
#if STDC_HEADERS
#include <stdarg.h>
#endif /* STDC_HEADERS */

#include "freeipmi/sel/ipmi-sel.h"

#include "ipmi-sel-defs.h"

typedef int (*Sel_string_output_sensor_name) (ipmi_sel_ctx_t ctx,
                                              struct ipmi_sel_entry *sel_entry,
                                              uint8_t sel_record_type,
                                              char *buf,
                                              unsigned int buflen,
                                              unsigned int flags,
                                              unsigned int *wlen,
                                              struct ipmi_sel_system_event_record_data *system_event_record_data,
                                              int *oem_rv);

typedef int (*Sel_string_output_event_data1_class_sensor_specific_discrete) (ipmi_sel_ctx_t ctx,
                                                                             struct ipmi_sel_entry *sel_entry,
                                                                             uint8_t sel_record_type,
                                                                             char *tmpbuf,
                                                                             unsigned int tmpbuflen,
                                                                             unsigned int flags,
                                                                             unsigned int *wlen,
                                                                             struct ipmi_sel_system_event_record_data *system_event_record_data);

typedef int (*Sel_string_output_event_data1_class_oem) (ipmi_sel_ctx_t ctx,
                                                        struct ipmi_sel_entry *sel_entry,
                                                        uint8_t sel_record_type,
                                                        char *tmpbuf,
                                                        unsigned int tmpbuflen,
                                                        unsigned int flags,
                                                        unsigned int *wlen,
                                                        struct ipmi_sel_system_event_record_data *system_event_record_data);

typedef int (*Sel_string_output_event_data2_threshold_oem) (ipmi_sel_ctx_t ctx,
                                                            struct ipmi_sel_entry *sel_entry,
                                                            uint8_t sel_record_type,
                                                            char *tmpbuf,
                                                            unsigned int tmpbuflen,
                                                            unsigned int flags,
                                                            unsigned int *wlen,
                                                            struct ipmi_sel_system_event_record_data *system_event_record_data);

typedef int (*Sel_string_output_event_data2_discrete_oem) (ipmi_sel_ctx_t ctx,
                                                           struct ipmi_sel_entry *sel_entry,
                                                           uint8_t sel_record_type,
                                                           char *tmpbuf,
                                                           unsigned int tmpbuflen,
                                                           unsigned int flags,
                                                           unsigned int *wlen,
                                                           struct ipmi_sel_system_event_record_data *system_event_record_data);

typedef int (*Sel_string_output_event_data2_class_oem) (ipmi_sel_ctx_t ctx,
                                                        struct ipmi_sel_entry *sel_entry,
                                                        uint8_t sel_record_type,
                                                        char *tmpbuf,
                                                        unsigned int tmpbuflen,
                                                        unsigned int flags,
                                                        unsigned int *wlen,
                                                        struct ipmi_sel_system_event_record_data *system_event_record_data);

typedef int (*Sel_string_output_event_data3_threshold_oem) (ipmi_sel_ctx_t ctx,
                                                            struct ipmi_sel_entry *sel_entry,
                                                            uint8_t sel_record_type,
                                                            char *tmpbuf,
                                                            unsigned int tmpbuflen,
                                                            unsigned int flags,
                                                            unsigned int *wlen,
                                                            struct ipmi_sel_system_event_record_data *system_event_record_data);

typedef int (*Sel_string_output_event_data3_discrete_oem) (ipmi_sel_ctx_t ctx,
                                                           struct ipmi_sel_entry *sel_entry,
                                                           uint8_t sel_record_type,
                                                           char *tmpbuf,
                                                           unsigned int tmpbuflen,
                                                           unsigned int flags,
                                                           unsigned int *wlen,
                                                           struct ipmi_sel_system_event_record_data *system_event_record_data);

typedef int (*Sel_string_output_event_data3_class_oem) (ipmi_sel_ctx_t ctx,
                                                        struct ipmi_sel_entry *sel_entry,
                                                        uint8_t sel_record_type,
                                                        char *tmpbuf,
                                                        unsigned int tmpbuflen,
                                                        unsigned int flags,
                                                        unsigned int *wlen,
                                                        struct ipmi_sel_system_event_record_data *system_event_record_data);

typedef int (*Sel_string_output_event_data2_event_data3) (ipmi_sel_ctx_t ctx,
                                                          struct ipmi_sel_entry *sel_entry,
                                                          uint8_t sel_record_type,
                                                          char *buf,
                                                          unsigned int buflen,
                                                          unsigned int flags,
                                                          unsigned int *wlen,
                                                          struct ipmi_sel_system_event_record_data *system_event_record_data,
                                                          int *oem_rv);

typedef int (*Sel_string_output_oem_record_data) (ipmi_sel_ctx_t ctx,
                                                  struct ipmi_sel_entry *sel_entry,
                                                  uint8_t sel_record_type,
                                                  char *buf,
                                                  unsigned int buflen,
                                                  unsigned int flags,
                                                  unsigned int *wlen,
                                                  int *oem_rv);

typedef int (*Sel_string_output_oem_string) (ipmi_sel_ctx_t ctx,
                                             struct ipmi_sel_entry *sel_entry,
                                             uint8_t sel_record_type,
                                             char *buf,
                                             unsigned int buflen,
                                             unsigned int flags,
                                             unsigned int *wlen,
                                             int *oem_rv);


struct sel_string_oem
{
  Sel_string_output_sensor_name output_sensor_name;
  Sel_string_output_event_data1_class_sensor_specific_discrete output_event_data1_class_sensor_specific_discrete;
  Sel_string_output_event_data1_class_oem output_event_data1_class_oem;
  Sel_string_output_event_data2_threshold_oem output_event_data2_threshold_oem;
  Sel_string_output_event_data2_discrete_oem output_event_data2_discrete_oem;
  Sel_string_output_event_data2_class_oem output_event_data2_class_oem;
  Sel_string_output_event_data3_threshold_oem output_event_data3_threshold_oem;
  Sel_string_output_event_data3_discrete_oem output_event_data3_discrete_oem;
  Sel_string_output_event_data3_class_oem output_event_data3_class_oem;
  Sel_string_output_event_data2_event_data3 output_event_data2_event_data3;
  Sel_string_output_oem_record_data output_oem_record_data;
  Sel_string_output_oem_string output_oem_string;
};

/* returns 0 on success, 1 on success but w/ truncation */
int sel_string_snprintf (char *buf,
                         unsigned int buflen,
                         unsigned int *wlen,
                         const char *fmt,
                         ...);

/* returns 0 on success, 1 on success but w/ truncation
 *
 * Just like sel_string_snprintf, but just appends and does non-zero check beforehand
 */
int sel_string_strcat_comma_separate (char *buf,
                                      unsigned int buflen,
                                      unsigned int *wlen,
                                      const char *str);

int sel_format_record_string (ipmi_sel_ctx_t ctx,
                              const char *fmt,
                              const void *sel_record,
                              unsigned int sel_record_len,
                              char *buf,
                              unsigned int buflen,
                              unsigned int flags);

#endif /* IPMI_SEL_STRING_H */
