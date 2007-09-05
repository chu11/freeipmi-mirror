/*****************************************************************************\
 *  $Id: ipmi_monitoring_ipmi_communication.h,v 1.3 2007-09-05 20:13:32 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2006 The Regents of the University of California.
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

#ifndef _IPMI_MONITORING_IPMI_COMMUNICATION_H
#define _IPMI_MONITORING_IPMI_COMMUNICATION_H

#include <freeipmi/freeipmi.h>

#include "ipmi_monitoring.h"

int ipmi_monitoring_ipmi_communication_init(ipmi_monitoring_ctx_t c,
                                            const char *hostname,
                                            struct ipmi_monitoring_ipmi_config *config);

int ipmi_monitoring_ipmi_sendrecv(ipmi_monitoring_ctx_t c,
                                  uint8_t lun,
                                  uint8_t net_fn,
                                  fiid_obj_t obj_cmd_rq,
                                  fiid_obj_t obj_cmd_rs);

int ipmi_monitoring_ipmi_communication_cleanup(ipmi_monitoring_ctx_t c);

#endif /* _IPMI_MONITORING_IPMI_COMMUNICATION_H */
