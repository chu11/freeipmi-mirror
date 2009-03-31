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

#ifndef _IPMI_FRU_PARSE_H
#define _IPMI_FRU_PARSE_H

#include <stdint.h>
#include <freeipmi/api/ipmi-api.h>

#define IPMI_FRU_PARSE_ERR_SUCCESS                                 0
#define IPMI_FRU_PARSE_ERR_CONTEXT_NULL                            1
#define IPMI_FRU_PARSE_ERR_CONTEXT_INVALID                         2
#define IPMI_FRU_PARSE_ERR_PARAMETERS                              3
#define IPMI_FRU_PARSE_ERR_DEVICE_ID_NOT_OPEN                      4
#define IPMI_FRU_PARSE_ERR_DEVICE_ID_ALREADY_OPEN                  5
#define IPMI_FRU_PARSE_ERR_NO_FRU_INFORMATION                      6
#define IPMI_FRU_PARSE_ERR_COMMON_HEADER_CHECKSUM_INVALID          7
#define IPMI_FRU_PARSE_ERR_OUT_OF_MEMORY                           8
#define IPMI_FRU_PARSE_ERR_IPMI_ERROR                              9
#define IPMI_FRU_PARSE_ERR_SYSTEM_ERROR                           10
#define IPMI_FRU_PARSE_ERR_INTERNAL_ERROR                         11
#define IPMI_FRU_PARSE_ERR_ERRNUMRANGE                            12

#define IPMI_FRU_PARSE_FLAGS_DEFAULT                              0x0000
#define IPMI_FRU_PARSE_FLAGS_DEBUG_DUMP                           0x0001
#define IPMI_FRU_PARSE_FLAGS_SKIP_CHECKSUM_CHECKS                 0x0002

typedef struct ipmi_fru_parse_ctx *ipmi_fru_parse_ctx_t;

/* FRU Parse Context Functions */
ipmi_fru_parse_ctx_t ipmi_fru_parse_ctx_create (ipmi_ctx_t ipmi_ctx);
void ipmi_fru_parse_ctx_destroy (ipmi_fru_parse_ctx_t ctx);
int ipmi_fru_parse_ctx_errnum (ipmi_fru_parse_ctx_t ctx);
char * ipmi_fru_parse_ctx_strerror (int errnum);
char * ipmi_fru_parse_ctx_errormsg (ipmi_fru_parse_ctx_t ctx);

/* FRU Parse flag functions */
int ipmi_fru_parse_ctx_get_flags (ipmi_fru_parse_ctx_t ctx, unsigned int *flags);
int ipmi_fru_parse_ctx_set_flags (ipmi_fru_parse_ctx_t ctx, unsigned int flags);
char *ipmi_fru_parse_ctx_get_debug_prefix (ipmi_fru_parse_ctx_t ctx);
int ipmi_fru_parse_ctx_set_debug_prefix (ipmi_fru_parse_ctx_t ctx, const char *debug_prefix);

/* FRU data retrieval functions */
int ipmi_fru_parse_open_device_id (ipmi_fru_parse_ctx_t ctx, uint8_t fru_device_id);
int ipmi_fru_parse_close_device_id (ipmi_fru_parse_ctx_t ctx);

int ipmi_fru_parse_first (ipmi_fru_parse_ctx_t ctx);
int ipmi_fru_parse_next (ipmi_fru_parse_ctx_t ctx);
int ipmi_fru_parse_read_data_area (ipmi_fru_parse_ctx_t ctx,
                                   unsigned int *area_type,
                                   uint8_t *buf,
                                   unsigned int buflen);
                         

#endif /* _IPMI_FRU_PARSE_H */
