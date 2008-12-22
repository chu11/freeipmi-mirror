/*****************************************************************************\
 *  $Id: ipmi-sel-parse.h,v 1.1.2.4 2008-12-22 23:04:18 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
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
 *  with Ipmimonitoring.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef _IPMI_SEL_PARSE_H
#define _IPMI_SEL_PARSE_H

#include <stdint.h>
#include <freeipmi/api/ipmi-api.h>
#include <freeipmi/sdr-cache/ipmi-sdr-cache.h>

#define IPMI_SEL_PARSE_CTX_ERR_SUCCESS                           0
#define IPMI_SEL_PARSE_CTX_ERR_CONTEXT_NULL                      1
#define IPMI_SEL_PARSE_CTX_ERR_CONTEXT_INVALID                   2
#define IPMI_SEL_PARSE_CTX_ERR_PARAMETERS                        3
#define IPMI_SEL_PARSE_CTX_ERR_OUT_OF_MEMORY                     4
#define IPMI_SEL_PARSE_CTX_ERR_SDR_CACHE_FILESYSTEM              5
#define IPMI_SEL_PARSE_CTX_ERR_SDR_CACHE_PERMISSION              6
#define IPMI_SEL_PARSE_CTX_ERR_SDR_CACHE_ERROR                   7
#define IPMI_SEL_PARSE_CTX_ERR_NO_SEL_ENTRIES                    8
#define IPMI_SEL_PARSE_CTX_ERR_NOT_FOUND                         9
#define IPMI_SEL_PARSE_CTX_ERR_CALLBACK_ERROR                   10
#define IPMI_SEL_PARSE_CTX_ERR_IPMI_ERROR                       11 
#define IPMI_SEL_PARSE_CTX_ERR_SYSTEM_ERROR                     12
#define IPMI_SEL_PARSE_CTX_ERR_OVERFLOW                         13
#define IPMI_SEL_PARSE_CTX_ERR_INTERNAL_ERROR                   14
#define IPMI_SEL_PARSE_CTX_ERR_ERRNUMRANGE                      15

#define IPMI_SEL_PARSE_FLAGS_DEFAULT                        0x0000
#define IPMI_SEL_PARSE_FLAGS_DEBUG_DUMP                     0x0001

typedef struct ipmi_sel_parse_ctx *ipmi_sel_parse_ctx_t;

typedef int (*Ipmi_Sel_Parse_Callback)(ipmi_sel_parse_ctx_t c, void *callback_data);

/* SEL Parse Context Functions */
ipmi_sel_parse_ctx_t ipmi_sel_parse_ctx_create(ipmi_ctx_t ipmi_ctx, ipmi_sdr_cache_ctx_t sdr_cache_ctx);
void ipmi_sel_parse_ctx_destroy(ipmi_sel_parse_ctx_t ctx);
int ipmi_sel_parse_ctx_errnum(ipmi_sel_parse_ctx_t ctx);
char * ipmi_sel_parse_ctx_strerror(int errnum);

/* SEL Parse flag functions */
int ipmi_sel_parse_ctx_get_flags(ipmi_sel_parse_ctx_t ctx, unsigned int *flags);
int ipmi_sel_parse_ctx_set_flags(ipmi_sel_parse_ctx_t ctx, unsigned int flags);
char *ipmi_sel_parse_ctx_get_debug_prefix(ipmi_sel_parse_ctx_t ctx);
int ipmi_sel_parse_ctx_set_debug_prefix(ipmi_sel_parse_ctx_t ctx, const char *prefix);

/* SEL Parse Functions 
 * 
 * callback is called after each SEL entry is parsed
 */
int ipmi_sel_parse(ipmi_sel_parse_ctx_t ctx,
                   Ipmi_Sel_Parse_Callback callback,
                   void *callback_data);

/* SEL data retrieval functions after SEL is parsed */
int ipmi_sel_parse_entry_count(ipmi_sel_parse_ctx_t ctx, uint16_t *entry_count);

int ipmi_sel_parse_first(ipmi_sel_parse_ctx_t ctx);
int ipmi_sel_parse_next(ipmi_sel_parse_ctx_t ctx);
int ipmi_sel_parse_seek(ipmi_sel_parse_ctx_t ctx, unsigned int index);
int ipmi_sel_parse_search_record_id(ipmi_sel_parse_ctx_t ctx, uint16_t record_id);

/* SEL read functions - can be used after sel parsed or within callbacks */
int ipmi_sel_parse_read_record_id(ipmi_sel_parse_ctx_t ctx, uint16_t *record_id);
int ipmi_sel_parse_read_record_type(ipmi_sel_parse_ctx_t ctx, uint8_t *record_type);

/* returns length of data written into buffer */
int ipmi_sel_parse_read_record(ipmi_sel_parse_ctx_t ctx, 
                               uint8_t *buf,
                               unsigned int buflen);

int ipmi_sel_parse_read_record_string(ipmi_sel_parse_ctx_t ctx, 
                                      char *fmt, 
                                      uint8_t *buf, 
                                      unsigned int buflen);

/* Utility functions */
int ipmi_sel_parse_clear_sel(ipmi_sel_parse_ctx_t ctx);

int ipmi_sel_parse_delete_sel_entry(ipmi_sel_parse_ctx_t ctx, uint16_t record_id);

#endif /* _IPMI_SEL_PARSE_H */
