/* 
   Copyright (C) 2003-2009 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  

*/

#ifndef _IPMI_SDR_PARSE_H
#define _IPMI_SDR_PARSE_H

#include <stdint.h>

#define IPMI_SDR_PARSE_CTX_ERR_SUCCESS                                 0
#define IPMI_SDR_PARSE_CTX_ERR_CONTEXT_NULL                            1
#define IPMI_SDR_PARSE_CTX_ERR_CONTEXT_INVALID                         2
#define IPMI_SDR_PARSE_CTX_ERR_PARAMETERS                              3
#define IPMI_SDR_PARSE_CTX_ERR_OUT_OF_MEMORY                           4
#define IPMI_SDR_PARSE_CTX_ERR_INVALID_SDR_RECORD                      5
#define IPMI_SDR_PARSE_CTX_ERR_INCOMPLETE_SDR_RECORD                   6
#define IPMI_SDR_PARSE_CTX_ERR_SYSTEM_ERROR                            7
#define IPMI_SDR_PARSE_CTX_ERR_INTERNAL_ERROR                          8
#define IPMI_SDR_PARSE_CTX_ERR_ERRNUMRANGE                             9

#define IPMI_SDR_PARSE_FLAGS_DEFAULT                              0x0000

typedef struct ipmi_sdr_parse_ctx *ipmi_sdr_parse_ctx_t;

/* SDR Parse Context Functions */
ipmi_sdr_parse_ctx_t ipmi_sdr_parse_ctx_create(void);
void ipmi_sdr_parse_ctx_destroy(ipmi_sdr_parse_ctx_t ctx);
int ipmi_sdr_parse_ctx_errnum(ipmi_sdr_parse_ctx_t ctx);
char * ipmi_sdr_parse_ctx_strerror(int errnum);
char * ipmi_sdr_parse_ctx_errormsg(ipmi_sdr_parse_ctx_t ctx);

/* SDR Parse flag functions */
int ipmi_sdr_parse_ctx_get_flags(ipmi_sdr_parse_ctx_t ctx, unsigned int *flags);
int ipmi_sdr_parse_ctx_set_flags(ipmi_sdr_parse_ctx_t ctx, unsigned int flags);

#endif /* _IPMI_SDR_PARSE_H */
