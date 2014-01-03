/*****************************************************************************\
 *  $Id: ipmiconsole_checks.h,v 1.13 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-221226
 *
 *  This file is part of Ipmiconsole, a set of IPMI 2.0 SOL libraries
 *  and utilities.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiconsole is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiconsole is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiconsole.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef IPMICONSOLE_CHECKS_H
#define IPMICONSOLE_CHECKS_H

#include <freeipmi/freeipmi.h>
#include "ipmiconsole.h"
#include "ipmiconsole_defs.h"

int ipmiconsole_check_checksum (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p);
int ipmiconsole_check_authentication_code (ipmiconsole_ctx_t c,
                                           ipmiconsole_packet_type_t p,
                                           void *buf,
                                           unsigned int buflen);
int ipmiconsole_check_outbound_sequence_number (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p);
int ipmiconsole_check_session_id (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p);
int ipmiconsole_check_network_function (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p);
int ipmiconsole_check_command (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p);
int ipmiconsole_check_requester_sequence_number (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p);
int ipmiconsole_check_completion_code (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p);
int ipmiconsole_check_payload_type (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p);
int ipmiconsole_check_message_tag (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p);
int ipmiconsole_check_rmcpplus_status_code (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p);
int ipmiconsole_check_packet (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p);
int ipmiconsole_check_open_session_response_privilege (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p);
int ipmiconsole_check_rakp_2_key_exchange_authentication_code (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p);
int ipmiconsole_check_rakp_4_integrity_check_value (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p);
int ipmiconsole_check_payload_pad (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p);
int ipmiconsole_check_integrity_pad (ipmiconsole_ctx_t c, ipmiconsole_packet_type_t p);

#endif /* IPMICONSOLE_CHECKS_H */
