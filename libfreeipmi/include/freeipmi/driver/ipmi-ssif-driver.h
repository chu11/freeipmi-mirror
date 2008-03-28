/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

   Based on ipmitool.c provided by Amitoj Singh <amitoj@fnal.gov> and 
   Don Holmgren <djholm@fnal.gov>

   Under GNU/Linux, requires i2c-dev, i2c-i801, i2c-core drivers version >= 2.8.7

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

#ifndef IPMI_SSIF_DRIVER_H
#define IPMI_SSIF_DRIVER_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <freeipmi/fiid/fiid.h>

#define IPMI_DEFAULT_I2C_DEVICE                "/dev/i2c-0"
#define IPMI_DEFAULT_SSIF_IPMB_ADDR            0x42

#define IPMI_SSIF_CTX_ERR_SUCCESS              0
#define IPMI_SSIF_CTX_ERR_NULL                 1
#define IPMI_SSIF_CTX_ERR_INVALID              2
#define IPMI_SSIF_CTX_ERR_PARAMETERS           3
#define IPMI_SSIF_CTX_ERR_PERMISSION           4
#define IPMI_SSIF_CTX_ERR_IO_NOT_INITIALIZED   5
#define IPMI_SSIF_CTX_ERR_OVERFLOW             6
#define IPMI_SSIF_CTX_ERR_BUSY                 7
#define IPMI_SSIF_CTX_ERR_OUT_OF_MEMORY        8
#define IPMI_SSIF_CTX_ERR_DEVICE_NOT_FOUND     9
#define IPMI_SSIF_CTX_ERR_DRIVER_TIMEOUT      10
#define IPMI_SSIF_CTX_ERR_SYSTEM_ERROR        11
#define IPMI_SSIF_CTX_ERR_INTERNAL_ERROR      12
#define IPMI_SSIF_CTX_ERR_ERRNUMRANGE         13

#define IPMI_SSIF_FLAGS_DEFAULT       0x00000000
#define IPMI_SSIF_FLAGS_NONBLOCKING   0x00000001

typedef struct ipmi_ssif_ctx *ipmi_ssif_ctx_t;

/* Notes:
 *
 * IPMBAddress - slave address of the BMC on the SMBus (usually 0x42)
 *
 */
ipmi_ssif_ctx_t ipmi_ssif_ctx_create(void);
int8_t ipmi_ssif_ctx_destroy(ipmi_ssif_ctx_t ctx);
int32_t ipmi_ssif_ctx_errnum(ipmi_ssif_ctx_t ctx);
char *ipmi_ssif_ctx_strerror(int32_t errnum);

int8_t ipmi_ssif_ctx_get_driver_device(ipmi_ssif_ctx_t ctx, char **driver_device);
int8_t ipmi_ssif_ctx_get_driver_address(ipmi_ssif_ctx_t ctx, uint8_t *driver_address);
int8_t ipmi_ssif_ctx_get_flags(ipmi_ssif_ctx_t ctx, uint32_t *flags);

int8_t ipmi_ssif_ctx_set_driver_device(ipmi_ssif_ctx_t ctx, char *driver_device);
int8_t ipmi_ssif_ctx_set_driver_address(ipmi_ssif_ctx_t ctx, uint8_t driver_address);
int8_t ipmi_ssif_ctx_set_flags(ipmi_ssif_ctx_t ctx, uint32_t flags);

int8_t ipmi_ssif_ctx_io_init(ipmi_ssif_ctx_t ctx);

int32_t ipmi_ssif_write (ipmi_ssif_ctx_t ctx,
			 uint8_t *buf,
			 uint32_t buf_len);

int32_t ipmi_ssif_read (ipmi_ssif_ctx_t ctx,
			uint8_t* buf,
			uint32_t buf_len);

int8_t ipmi_ssif_cmd (ipmi_ssif_ctx_t ctx, 
                      uint8_t lun,
                      uint8_t net_fn,
                      fiid_obj_t obj_cmd_rq,
                      fiid_obj_t obj_cmd_rs);

#ifdef __cplusplus
}
#endif

#endif /* IPMI_SSIF_DRIVER_H */
