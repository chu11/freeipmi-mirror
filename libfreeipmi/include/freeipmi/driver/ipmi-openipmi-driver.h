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

#ifndef _IPMI_OPENIPMI_DRIVER_H
#define _IPMI_OPENIPMI_DRIVER_H 1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid/fiid.h>

#define IPMI_OPENIPMI_DRIVER_DEVICE_DEFAULT      "/dev/ipmi0"

#define IPMI_OPENIPMI_CTX_ERR_SUCCESS             0
#define IPMI_OPENIPMI_CTX_ERR_NULL                1
#define IPMI_OPENIPMI_CTX_ERR_INVALID             2
#define IPMI_OPENIPMI_CTX_ERR_PARAMETERS          3
#define IPMI_OPENIPMI_CTX_ERR_PERMISSION          4
#define IPMI_OPENIPMI_CTX_ERR_DEVICE_NOT_FOUND    5
#define IPMI_OPENIPMI_CTX_ERR_IO_NOT_INITIALIZED  6
#define IPMI_OPENIPMI_CTX_ERR_OUT_OF_MEMORY       7
#define IPMI_OPENIPMI_CTX_ERR_DRIVER_TIMEOUT      8
#define IPMI_OPENIPMI_CTX_ERR_SYSTEM_ERROR        9
#define IPMI_OPENIPMI_CTX_ERR_INTERNAL_ERROR     10
#define IPMI_OPENIPMI_CTX_ERR_ERRNUMRANGE        11

#define IPMI_OPENIPMI_FLAGS_DEFAULT              0x00000000

typedef struct ipmi_openipmi_ctx *ipmi_openipmi_ctx_t;

ipmi_openipmi_ctx_t ipmi_openipmi_ctx_create(void);
int8_t ipmi_openipmi_ctx_destroy(ipmi_openipmi_ctx_t ctx);
int32_t ipmi_openipmi_ctx_errnum(ipmi_openipmi_ctx_t ctx);
char *ipmi_openipmi_ctx_strerror(int32_t errnum);

int8_t ipmi_openipmi_ctx_get_driver_device(ipmi_openipmi_ctx_t ctx, char **driver_device);
int8_t ipmi_openipmi_ctx_get_flags(ipmi_openipmi_ctx_t ctx, uint32_t *flags);

int8_t ipmi_openipmi_ctx_set_driver_device(ipmi_openipmi_ctx_t ctx, char *driver_device);
int8_t ipmi_openipmi_ctx_set_flags(ipmi_openipmi_ctx_t ctx, uint32_t flags);

int8_t ipmi_openipmi_ctx_io_init(ipmi_openipmi_ctx_t ctx);

int8_t ipmi_openipmi_cmd (ipmi_openipmi_ctx_t ctx,
                          uint8_t lun,
                          uint8_t net_fn,
                          fiid_obj_t obj_cmd_rq,
                          fiid_obj_t obj_cmd_rs);

int8_t ipmi_openipmi_cmd_ipmb (ipmi_openipmi_ctx_t ctx,
			       uint8_t rs_addr,
			       uint8_t lun,
			       uint8_t net_fn,
			       fiid_obj_t obj_cmd_rq,
			       fiid_obj_t obj_cmd_rs);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-openipmi-driver.h */

