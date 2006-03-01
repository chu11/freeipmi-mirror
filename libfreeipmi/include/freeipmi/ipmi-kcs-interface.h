/* 
   ipmi-kcs-interface.h - IPMI KCS SMS Interface

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  

*/

#ifndef _IPMI_KCS_INTERFACE_H
#define _IPMI_KCS_INTERFACE_H 1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define IPMI_KCS_SMS_IO_BASE_DEFAULT    0x0CA2

#define IPMI_KCS_CTX_ERR_SUCCESS         0
#define IPMI_KCS_CTX_ERR_NULL            1
#define IPMI_KCS_CTX_ERR_INVALID         2
#define IPMI_KCS_CTX_ERR_PARAMETERS      3
#define IPMI_KCS_CTX_ERR_PERMISSION      4
#define IPMI_KCS_CTX_ERR_IO_PARAMETERS   5
#define IPMI_KCS_CTX_ERR_IO_INIT         6
#define IPMI_KCS_CTX_ERR_OVERFLOW        7
#define IPMI_KCS_CTX_ERR_BUSY            8
#define IPMI_KCS_CTX_ERR_OUTMEM          9
#define IPMI_KCS_CTX_ERR_INTERNAL        10
#define IPMI_KCS_CTX_ERR_ERRNUMRANGE     11

#define IPMI_KCS_MODE_BLOCKING    0
#define IPMI_KCS_MODE_NONBLOCKING 1
#define IPMI_KCS_MODE_DEFAULT     IPMI_KCS_MODE_BLOCKING

typedef struct ipmi_kcs_ctx *ipmi_kcs_ctx_t;

ipmi_kcs_ctx_t ipmi_kcs_ctx_create(void);
int8_t ipmi_kcs_ctx_destroy(ipmi_kcs_ctx_t ctx);
int32_t ipmi_kcs_ctx_errnum(ipmi_kcs_ctx_t ctx);
char *ipmi_kcs_ctx_strerror(int32_t errnum);

int8_t ipmi_kcs_ctx_get_bmc_iobase_addr(ipmi_kcs_ctx_t ctx, uint16_t *bmc_iobase_addr);
int8_t ipmi_kcs_ctx_get_register_space(ipmi_kcs_ctx_t ctx, uint8_t *reg_space);
int8_t ipmi_kcs_ctx_get_poll_interval(ipmi_kcs_ctx_t ctx, uint8_t *poll_interval);
int8_t ipmi_kcs_ctx_get_mode(ipmi_kcs_ctx_t ctx, uint8_t *mode);

int8_t ipmi_kcs_ctx_set_bmc_iobase_addr(ipmi_kcs_ctx_t ctx, uint16_t bmc_iobase_addr);
int8_t ipmi_kcs_ctx_set_register_space(ipmi_kcs_ctx_t ctx, uint8_t reg_space);
int8_t ipmi_kcs_ctx_set_poll_interval(ipmi_kcs_ctx_t ctx, uint8_t poll_interval);
int8_t ipmi_kcs_ctx_set_mode(ipmi_kcs_ctx_t ctx, uint8_t mode);

int8_t ipmi_kcs_ctx_io_init(ipmi_kcs_ctx_t ctx);

int32_t ipmi_kcs_write (ipmi_kcs_ctx_t ctx,
                        uint8_t *bytes,
                        uint32_t  bytes_len);
  
int32_t ipmi_kcs_read (ipmi_kcs_ctx_t ctx,
                       uint8_t* bytes,
                       uint32_t bytes_len);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-kcs-interface.h */

