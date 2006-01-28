/*****************************************************************************\
 *  $Id: ipmipower_wrappers.h,v 1.5 2006-01-28 20:36:04 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2003 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *  
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see http://www.llnl.gov/linux/.
 *  
 *  Ipmipower is free software; you can redistribute it and/or modify 
 *  it under the terms of the GNU General Public License as published by the 
 *  Free Software Foundation; either version 2 of the License, or (at your 
 *  option) any later version.
 *  
 *  Ipmipower is distributed in the hope that it will be useful, but 
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
 *  for more details.
 *  
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmipower; if not, write to the Free Software Foundation, Inc.,
 *  59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.
\*****************************************************************************/

#ifndef _IPMIPOWER_WRAPPERS_H
#define _IPMIPOWER_WRAPPERS_H

#include "argv.h"
#include "hprintf.h"
#include "fd.h"
#include "list.h"
#include "cbuf.h"
#include "hostlist.h"
#include "wrappers.h"
#include "error.h"
#include "conffile.h"
#include "freeipmi.h"

/* Cbuf_create
 * - Create cbuf buffer
 * - Automaticaly sets CBUF_OPT_OVERWRITE to CBUF_WRAP_MANY
 */
cbuf_t Cbuf_create(int minsize, int maxsize);

/* Cbuf_drop_all
 * - drop all data from buffer
 */
void Cbuf_drop_all(cbuf_t buf); 

/* Cbuf_read_to_fd
 * - move data from buf to fd
 */
void Cbuf_read_to_fd(cbuf_t buf, int fd);

/* Cbuf_write_from_fd
 * - move data from fd to buf
 */
void Cbuf_write_from_fd(cbuf_t buf, int fd);

/* Cbuf_write
 * - wrapper for cbuf_write, with various error checks
 */
void Cbuf_write(cbuf_t buf, void *buffer, int len);

/* Cbuf_peek_and_drop
 * - wrapper for cbuf_peek and cbuf_drop, with various error checks
 * - will drop remaining data in cbuf if buffer not large enough
 * Returns length of packet received, 0 if no packet seen
 */
int Cbuf_peek_and_drop(cbuf_t buf, void *buffer, int len);

/* Cbuf_peek_to_fd
 * - wrapper for cbuf_peek_to_fd
 */
int Cbuf_peek_to_fd(cbuf_t src, int dstfd, int len);

/* Fiid_obj_calloc
 * - Allocate a fiid object
 */
void * Fiid_obj_calloc(fiid_template_t tmpl);

/* Fiid_obj_memset
 * - Memset a fiid object 
 */
void * Fiid_obj_memset(fiid_obj_t obj, int c, fiid_template_t tmpl);

/* Fiid_obj_free
 * - Free a fiid object
 */
void Fiid_obj_free(fiid_obj_t obj);

/* Fiid_obj_get
 * - Get fiid field data
 */
void Fiid_obj_get(fiid_obj_t obj, fiid_template_t tmpl, uint8_t *field, uint64_t *val);

/* Fiid_obj_dump_lan
 * - Dump lan contents
 */
void Fiid_obj_dump_lan(int fd, char *prefix, char *hdr, uint8_t *pkt, uint32_t pkt_len, fiid_template_t tmpl_session, fiid_template_t tmpl_msg_hdr, fiid_template_t tmpl_cmd);

/* Fiid_obj_dump_rmcp
 * - Dump rmcp contents
 */
void
Fiid_obj_dump_rmcp(int fd, char *prefix, char *hdr, uint8_t *pkt, uint32_t pkt_len, fiid_template_t tmpl_cmd);

#endif /* _IPMIPOWER_WRAPPERS_H */
