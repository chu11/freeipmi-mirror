/*****************************************************************************\
 *  $Id: ipmi-ping.h,v 1.7 2007-10-17 23:13:00 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2003-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155448
 *  
 *  This file is part of Ipmiping, tools for pinging IPMI and RMCP compliant
 *  remote systems. For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiping is free software; you can redistribute it and/or modify 
 *  it under the terms of the GNU General Public License as published by the 
 *  Free Software Foundation; either version 2 of the License, or (at your 
 *  option) any later version.
 *  
 *  Ipmiping is distributed in the hope that it will be useful, but 
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
 *  for more details.
 *  
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiping; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
\*****************************************************************************/

#ifndef _IPMI_PING_H
#define _IPMI_PING_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_PING_VERSION_1_5    0
#define IPMI_PING_VERSION_2_0    1

/* Ipmi_Ping_CreatePacket
 * - Create a ping request packet and store it in the buffer
 * - Return length of packet created, or -1 on error.
 */
typedef int (*Ipmi_Ping_CreatePacket)(char *buffer, 
                                      int buflen, 
                                      unsigned int sequence_number, 
                                      int version,
                                      int debug);

/* Ipmi_Ping_ParsePacket
 * - Parse packet stored in buffer and output info about received
 *   packet to stdout.
 * - Return 1 if packet matches sequence number, 0 if packet does not,
 *   -1 on error.
 */
typedef int (*Ipmi_Ping_ParsePacket)(char *buffer, 
                                     int buflen, 
                                     const char *from, 
                                     unsigned int sequence_number, 
                                     int verbose, 
                                     int version,
                                     int debug);

/* Ipmi_Ping_LatePacket
 * - Output info about timed out packet to stdout
 */
typedef void (*Ipmi_Ping_LatePacket)(unsigned int sequence_number);

/* Ipmi_Ping_EndResult
 * - Output final results to stdout and return exit code
 */ 
typedef int (*Ipmi_Ping_EndResult)(const char *progname, 
                                   const char *dest,
                                   unsigned int sent_count, 
                                   unsigned int recv_count);

/* ipmi_ping_err_exit
 * - exit with GNU style exit output
 */
void ipmi_ping_err_exit(char *fmt, ...);
  
/* ipmi_ping_setup
 * - setup ipmi ping code by parsing command line arguments
 */
void ipmi_ping_setup(int argc,
                     char **argv,
                     unsigned int min_sequence_number,
                     unsigned int max_sequence_number,
                     char *options);
  
/* ipmi_ping_loop
 * - handle looping ping code
 */
void ipmi_ping_loop(Ipmi_Ping_CreatePacket _create,
                    Ipmi_Ping_ParsePacket _parse,
                    Ipmi_Ping_LatePacket _late,
                    Ipmi_Ping_EndResult _end);

#ifdef __cplusplus
}
#endif

#endif /* _IPMI_PING_H */
