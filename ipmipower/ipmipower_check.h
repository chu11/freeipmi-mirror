/*****************************************************************************\
 *  $Id: ipmipower_check.h,v 1.20 2010-02-08 22:02:31 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2003-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmipower is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmipower is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmipower.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef IPMIPOWER_CHECKS_H
#define IPMIPOWER_CHECKS_H

#include "ipmipower.h"

/* ipmipower_check_checksum
 * - Check for a valid checksums
 * - Function can be passed IPMI 1.5 packets and IPMI 2.0 session packets
 * Returns 1 if checksums are valid, 0 if not
 */
int ipmipower_check_checksum (ipmipower_powercmd_t ip,
			      ipmipower_packet_type_t pkt);

/* ipmipower_check_authentication_code
 * - Check for valid authentication code
 * - Function can be passed IPMI 1.5 packets and IPMI 2.0 session packets
 * Returns 1 if authentication code is valid, 0 if not
 */
int ipmipower_check_authentication_code (ipmipower_powercmd_t ip,
                                         ipmipower_packet_type_t pkt,
                                         const void *buf,
                                         unsigned int buflen);

/* ipmipower_check_outbound_sequence_number
 * - Check for valid outbound sequence number
 * - Function can be passed all packet types
 * Returns 1 if outbound sequence number is valid, 0 if not
 */
int ipmipower_check_outbound_sequence_number (ipmipower_powercmd_t ip,
					      ipmipower_packet_type_t pkt);

/* ipmipower_check_session_id
 * - Check for valid session id
 * - Function can be passed all packet types
 * Returns 1 if session_id is valid, 0 if not
 */
int ipmipower_check_session_id (ipmipower_powercmd_t ip,
				ipmipower_packet_type_t pkt);

/* ipmipower_check_network_function
 * - Check for valid network function
 * - Function can be passed IPMI 1.5 packets and IPMI 2.0 session packets
 * Returns 1 if network function is valid, 0 if not
 */
int ipmipower_check_network_function (ipmipower_powercmd_t ip,
				      ipmipower_packet_type_t pkt);

/* ipmipower_check_command
 * - Check for valid command
 * - Function can be passed IPMI 1.5 packets and IPMI 2.0 session packets
 * Returns 1 if command is valid, 0 if not
 */
int ipmipower_check_command (ipmipower_powercmd_t ip,
			     ipmipower_packet_type_t pkt);

/* ipmipower_check_requester_sequence_number
 * - Check for valid requester sequence number
 * - Function can be passed IPMI 1.5 packets and IPMI 2.0 session packets
 * Returns 1 if requester sequence number is valid, 0 if not
 */
int ipmipower_check_requester_sequence_number (ipmipower_powercmd_t ip,
					       ipmipower_packet_type_t pkt);

/* ipmipower_check_completion_code
 * - Check for valid completion code
 * - Function can be passed IPMI 1.5 packets and IPMI 2.0 session packets
 * Returns 1 if completion code is valid, 0 if not
 */
int ipmipower_check_completion_code (ipmipower_powercmd_t ip,
				     ipmipower_packet_type_t pkt);

/* ipmipower_check_payload_type
 * - Check for valid payload type
 * - Function can be passed IPMI 2.0 packets
 * Returns 1 if payload type is valid, 0 if not
 */
int ipmipower_check_payload_type (ipmipower_powercmd_t ip,
				  ipmipower_packet_type_t pkt);

/* ipmipower_check_message_tag
 * - Check for valid message tag
 * - Function can be passed IPMI 2.0 RAKP packets
 * Returns 1 if message tag is valid, 0 if not
 */
int ipmipower_check_message_tag (ipmipower_powercmd_t ip,
				 ipmipower_packet_type_t pkt);

/* ipmipower_check_rmcpplus_status_code
 * - Check for valid rmcpplus status code
 * - Function can be passed IPMI 2.0 RAKP packets
 * Returns 1 if rmcpplus status code is valid, 0 if not
 */
int ipmipower_check_rmcpplus_status_code (ipmipower_powercmd_t ip,
					  ipmipower_packet_type_t pkt);

/* ipmipower_check_packet
 * - Check if packet contains everything it should.
 * - Function can be passed IPMI 1.5 packets and IPMI 2.0 session packets
 * Returns 1 if packet is valid, 0 if not
 */
int ipmipower_check_packet (ipmipower_powercmd_t ip,
			    ipmipower_packet_type_t pkt);

/* ipmipower_check_open_session_response_privilege
 * - Check for valid open session response data
 * - Function can be passed OPEN_SESSION_RES packets
 * Returns 1 if data is correct, 0 if not
 */
int ipmipower_check_open_session_response_privilege (ipmipower_powercmd_t ip,
						     ipmipower_packet_type_t pkt);

/* ipmipower_check_rakp_2_key_exchange_authentication_code
 * - Check for valid rakp 2 key exchange authentication code
 * - Function can be passed RAKP MESSAGE 2 packets
 * Returns 1 if rakp 2 key exchange authentication code is valid, 0 if not
 */
int ipmipower_check_rakp_2_key_exchange_authentication_code (ipmipower_powercmd_t ip,
							     ipmipower_packet_type_t pkt);

/* ipmipower_check_rakp_4_integrity_check_value
 * - Check for valid rakp 4 integrity check value
 * - Function can be passed RAKP MESSAGE 4 packets
 * Returns 1 if rakp 4 integrity check value is valid, 0 if not
 */
int ipmipower_check_rakp_4_integrity_check_value (ipmipower_powercmd_t ip,
						  ipmipower_packet_type_t pkt);

/* ipmipower_check_payload_pad
 * - Check for valid payload pad
 * - Function can be passed IPMI 2.0 session packets
 * Returns 1 if payload pad is valid, 0 if not
 */
int ipmipower_check_payload_pad (ipmipower_powercmd_t ip,
				 ipmipower_packet_type_t pkt);

/* ipmipower_check_integrity_pad
 * - Check for valid integrity pad
 * - Function can be passed IPMI 2.0 session packets
 * Returns 1 if integrity pad is valid, 0 if not
 */
int ipmipower_check_integrity_pad (ipmipower_powercmd_t ip,
				   ipmipower_packet_type_t pkt);

#endif /* IPMIPOWER_CHECKS_H */
