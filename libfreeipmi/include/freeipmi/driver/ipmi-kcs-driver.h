/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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

#ifndef _IPMI_KCS_DRIVER_H
#define _IPMI_KCS_DRIVER_H 1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid/fiid.h>

#define IPMI_KCS_SMS_IO_BASE_DEFAULT          0x0CA2

#define IPMI_KCS_CTX_ERR_SUCCESS              0
#define IPMI_KCS_CTX_ERR_NULL                 1
#define IPMI_KCS_CTX_ERR_INVALID              2
#define IPMI_KCS_CTX_ERR_PARAMETERS           3
#define IPMI_KCS_CTX_ERR_PERMISSION           4
#define IPMI_KCS_CTX_ERR_IO_NOT_INITIALIZED   5
#define IPMI_KCS_CTX_ERR_OVERFLOW             6
#define IPMI_KCS_CTX_ERR_BUSY                 7
#define IPMI_KCS_CTX_ERR_OUT_OF_MEMORY        8
#define IPMI_KCS_CTX_ERR_DEVICE_NOT_FOUND     9
#define IPMI_KCS_CTX_ERR_DRIVER_TIMEOUT      10
#define IPMI_KCS_CTX_ERR_SYSTEM_ERROR        11
#define IPMI_KCS_CTX_ERR_INTERNAL_ERROR      12
#define IPMI_KCS_CTX_ERR_ERRNUMRANGE         13

#define IPMI_KCS_FLAGS_DEFAULT       0x00000000
#define IPMI_KCS_FLAGS_NONBLOCKING   0x00000001

typedef struct ipmi_kcs_ctx *ipmi_kcs_ctx_t;

ipmi_kcs_ctx_t ipmi_kcs_ctx_create(void);
int8_t ipmi_kcs_ctx_destroy(ipmi_kcs_ctx_t ctx);
int32_t ipmi_kcs_ctx_errnum(ipmi_kcs_ctx_t ctx);
char *ipmi_kcs_ctx_strerror(int32_t errnum);

int8_t ipmi_kcs_ctx_get_driver_address(ipmi_kcs_ctx_t ctx, uint16_t *bmc_iobase_address);
int8_t ipmi_kcs_ctx_get_register_spacing(ipmi_kcs_ctx_t ctx, uint8_t *register_spacing);
int8_t ipmi_kcs_ctx_get_poll_interval(ipmi_kcs_ctx_t ctx, uint8_t *poll_interval);
int8_t ipmi_kcs_ctx_get_flags(ipmi_kcs_ctx_t ctx, uint32_t *flags);

int8_t ipmi_kcs_ctx_set_driver_address(ipmi_kcs_ctx_t ctx, uint16_t bmc_iobase_address);
int8_t ipmi_kcs_ctx_set_register_spacing(ipmi_kcs_ctx_t ctx, uint8_t register_spacing);
int8_t ipmi_kcs_ctx_set_poll_interval(ipmi_kcs_ctx_t ctx, uint8_t poll_interval);
int8_t ipmi_kcs_ctx_set_flags(ipmi_kcs_ctx_t ctx, uint32_t flags);

int8_t ipmi_kcs_ctx_io_init(ipmi_kcs_ctx_t ctx);

int32_t ipmi_kcs_write (ipmi_kcs_ctx_t ctx,
                        uint8_t *bytes,
                        uint32_t  bytes_len);
  
int32_t ipmi_kcs_read (ipmi_kcs_ctx_t ctx,
                       uint8_t* bytes,
                       uint32_t bytes_len);

int8_t ipmi_kcs_cmd (ipmi_kcs_ctx_t ctx,
		     uint8_t lun,
		     uint8_t net_fn,
		     fiid_obj_t obj_cmd_rq,
		     fiid_obj_t obj_cmd_rs);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-kcs-driver.h */

