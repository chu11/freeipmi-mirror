/*****************************************************************************\
 *  $Id: ipmi_monitoring_sel.h,v 1.1 2010-03-19 22:07:58 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef IPMI_MONITORING_SEL_H
#define IPMI_MONITORING_SEL_H

#include "ipmi_monitoring.h"

int ipmi_monitoring_sel_init (ipmi_monitoring_ctx_t c);

int ipmi_monitoring_sel_cleanup (ipmi_monitoring_ctx_t c);

int ipmi_monitoring_get_sel (ipmi_monitoring_ctx_t c,
                             unsigned int sel_flags,
                             unsigned int *record_ids,
                             unsigned int record_ids_len,
                             unsigned int *sensor_types,
                             unsigned int sensor_types_len,
                             unsigned int *date_begin,
                             unsigned int *date_end);

#endif /* IPMI_MONITORING_SEL_H */
