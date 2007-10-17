/*****************************************************************************\
 *  $Id: ipmi_monitoring_fiid_wrappers.h,v 1.5 2007-10-17 23:13:02 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
\*****************************************************************************/

#ifndef _IPMI_MONITORING_FIID_WRAPPERS_H
#define _IPMI_MONITORING_FIID_WRAPPERS_H

#include <stdint.h>
#include <freeipmi/freeipmi.h>

/* XXX */
/* XXX */
/* XXX */
/* XXX */
/* XXX */

int32_t Fiid_template_len_bytes(ipmi_monitoring_ctx_t c, fiid_template_t tmpl);
int32_t Fiid_template_block_len_bytes(ipmi_monitoring_ctx_t c, fiid_template_t tmpl, char *field_start, char *field_end);

fiid_obj_t Fiid_obj_create(ipmi_monitoring_ctx_t c, fiid_template_t tmpl);
int8_t Fiid_obj_clear(ipmi_monitoring_ctx_t c, fiid_obj_t obj);
void Fiid_obj_destroy(ipmi_monitoring_ctx_t c, fiid_obj_t obj);
int8_t Fiid_obj_get(ipmi_monitoring_ctx_t c, fiid_obj_t obj, char *field, uint64_t *val);
int32_t Fiid_obj_get_data(ipmi_monitoring_ctx_t c, fiid_obj_t obj, char *field, uint8_t *data, uint32_t data_len);
int8_t Fiid_obj_set(ipmi_monitoring_ctx_t c, fiid_obj_t obj, char *field, uint64_t val);
int32_t Fiid_obj_set_all(ipmi_monitoring_ctx_t c, fiid_obj_t obj, uint8_t *data, uint32_t data_len);

#endif /* _IPMI_MONITORING_FIID_WRAPPERS_H */
