/* 
   ipmi-ssif-interface.c: IPMI - SMBus System Interface - SMS Interface

   Copyright (C) 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <sys/ioctl.h>
#include <errno.h>

#include "freeipmi/ipmi-ssif-interface.h"

#include "ipmi-semaphores.h"

#include "err-wrappers.h"
#include "ipmi-common.h"
#include "freeipmi-portability.h"
#include "xmalloc.h"

/* function error codes */
#define IPMI_SSIF_SUCCESS         0x00
#define IPMI_SSIF_UNDEF_ERROR     0xFFFF

#define IPMI_SSIF_UNKNOWN_INTERFACE       0x1000
#define IPMI_SSIF_ISA_MESSAGE_OVERFLOW    0x1100
#define IPMI_SSIF_BAD_ISA_STATE           0x1200
#define IPMI_SSIF_BMC_WRITE_STATE_FAIL    0x1300
#define IPMI_SSIF_BMC_READ_STATE_FAIL     0x1400
#define IPMI_SSIF_SMS_READY_TIMEOUT       0x1500
#define IPMI_SSIF_ERROR_READING_SMS       0x1600
#define IPMI_SSIF_PACKET_NUMBER_MISMATCH  0x1700
#define IPMI_SSIF_PACKET_SIZE_MISMATCH    0x1800
#define IPMI_SSIF_I2C_RETRY_ERROR         0x1900
#define IPMI_SSIF_SMS_SEND_ERROR_ON_FLUSH 0x2000
#define IPMI_SSIF_ISA_TIMEOUT_IN_IBF      0x2100
#define IPMI_SSIF_ISA_TIMEOUT_IN_OBF      0x2200

/* Response Packet Offsets */
#define IPMI_SSIF_RSP_OFFSET_COMPCODE 0x03

/* SMBus */
#define IPMI_SSIF_SMB_IPMI_REQUEST    2
#define IPMI_SSIF_SMB_IPMI_RESPONSE   3

/* this is for i2c-dev.c	*/
#define IPMI_I2C_SLAVE       0x0703  /* Change slave address                 */
			             /* Attn.: Slave address is 7 or 10 bits */
#define IPMI_I2C_SLAVE_FORCE 0x0706  /* Change slave address                 */
                                     /* Attn.: Slave address is 7 or 10 bits */
				     /* This changes the address, even if it */
				     /* is already taken!                    */

#define IPMI_I2C_SMBUS       0x0720  /* SMBus-level access */

/* SMBus transaction types (size parameter in the above functions) 
   Note: these no longer correspond to the (arbitrary) PIIX4 internal codes! */
#define IPMI_I2C_SMBUS_QUICK                0
#define IPMI_I2C_SMBUS_BYTE                 1
#define IPMI_I2C_SMBUS_BYTE_DATA            2 
#define IPMI_I2C_SMBUS_WORD_DATA            3
#define IPMI_I2C_SMBUS_PROC_CALL            4
#define IPMI_I2C_SMBUS_BLOCK_DATA           5
#define IPMI_I2C_SMBUS_I2C_BLOCK_DATA       6
#define IPMI_I2C_SMBUS_BLOCK_PROC_CALL      7     /* SMBus 2.0 */
#define IPMI_I2C_SMBUS_BLOCK_DATA_PEC       8     /* SMBus 2.0 */
#define IPMI_I2C_SMBUS_PROC_CALL_PEC        9     /* SMBus 2.0 */
#define IPMI_I2C_SMBUS_BLOCK_PROC_CALL_PEC  10    /* SMBus 2.0 */
#define IPMI_I2C_SMBUS_WORD_DATA_PEC        11    /* SMBus 2.0 */

/* 
 * Data for SMBus Messages 
 */
#define IPMI_I2C_SMBUS_BLOCK_MAX	32	/* As specified in SMBus standard */	
#define IPMI_I2C_SMBUS_I2C_BLOCK_MAX	32	/* Not specified but we use same structure */

/* smbus_access read or write markers */
#define IPMI_I2C_SMBUS_READ	1
#define IPMI_I2C_SMBUS_WRITE	0


union ipmi_i2c_smbus_data {
  uint8_t byte;
  uint16_t word;
  uint8_t block[IPMI_I2C_SMBUS_BLOCK_MAX + 3]; /* block[0] is used for length */
                          /* one more for read length in block process call */
	                                            /* and one more for PEC */
};

/* Note: 10-bit addresses are NOT supported! */
/* This is the structure as used in the I2C_SMBUS ioctl call */
struct ipmi_i2c_smbus_ioctl_data {
	char read_write;
	uint8_t command;
	int size;
	union ipmi_i2c_smbus_data *data;
};

static inline int32_t
ipmi_i2c_smbus_access (int file, char read_write, uint8_t command, int size, 
		       union ipmi_i2c_smbus_data *data)
{
	struct ipmi_i2c_smbus_ioctl_data args;

	args.read_write = read_write;
	args.command = command;
	args.size = size;
	args.data = data;
	return ioctl (file, IPMI_I2C_SMBUS, &args);
}

/* ipmi_i2c_smbus_read_block_data is based on
linux/i2c-dev.h:i2c_smbus_read_block_data. It is duplicated here to
reduce dependencies. -- Anand Babu */

static inline int32_t
ipmi_i2c_smbus_read_block_data (int file, uint8_t command, uint8_t *values)
{
	union ipmi_i2c_smbus_data data;
	int i;
	if (ipmi_i2c_smbus_access (file, IPMI_I2C_SMBUS_READ, command,
				   IPMI_I2C_SMBUS_BLOCK_DATA, &data))
		return -1;
	else {
		for (i = 1; i <= data.block[0]; i++)
			values[i-1] = data.block[i];
		return data.block[0];
	}
}

/* ipmi_i2c_smbus_write_block_data is based on
linux/i2c-dev.h:i2c_smbus_write_block_data. It is duplicated here to
reduce dependencies. -- Anand Babu */
static inline int32_t
ipmi_i2c_smbus_write_block_data (int file, uint8_t command, uint8_t length, uint8_t *values)
{
	union ipmi_i2c_smbus_data data;
	int i;
	if (length > 32)
		length = 32;
	for (i = 1; i <= length; i++)
		data.block[i] = values[i-1];
	data.block[0] = length;
	return ipmi_i2c_smbus_access (file, IPMI_I2C_SMBUS_WRITE, command,
				      IPMI_I2C_SMBUS_BLOCK_DATA, &data);
}

#define IPMI_SSIF_CTX_MAGIC 0xaddaabba

static char * ipmi_ssif_ctx_errmsg[] =
  {
    "success",
    "ssif context is null",
    "ssif context is invalid",
    "invalid parameter",
    "permission denied",
    "io not initialized",
    "buffer too small to hold result",
    "BMC busy",
    "out of memory",
    "internal error",
    "error number out of range",
    NULL,
  };

struct ipmi_ssif_ctx {
  uint32_t magic;
  int32_t errnum;
  char *i2c_device;
  uint8_t ipmb_addr;
  uint8_t mode;
  int i2c_fd;
  int io_init;
  int semid;
};

ipmi_ssif_ctx_t
ipmi_ssif_ctx_create(void)
{
  ipmi_ssif_ctx_t ctx = NULL;

  ERR_CLEANUP ((ctx = (ipmi_ssif_ctx_t)xmalloc(sizeof(struct ipmi_ssif_ctx))));

  ctx->magic = IPMI_SSIF_CTX_MAGIC;
  ERR_CLEANUP ((ctx->i2c_device = strdup(IPMI_DEFAULT_I2C_DEVICE)));
  ctx->ipmb_addr = IPMI_DEFAULT_IPMB_ADDRESS;
  ctx->mode = IPMI_SSIF_MODE_DEFAULT;
  ctx->i2c_fd = -1;
  ctx->io_init = 0;

  ERR_CLEANUP (!((ctx->semid = ipmi_mutex_init (IPMI_INBAND_IPCKEY())) < 0));

  ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;
  return ctx;

 cleanup:
  if (ctx)
    xfree(ctx);
  return (NULL);
}

int8_t
ipmi_ssif_ctx_destroy(ipmi_ssif_ctx_t ctx)
{
  if (!(ctx && ctx->magic == IPMI_SSIF_CTX_MAGIC))
    return (-1);

  ctx->magic = ~IPMI_SSIF_CTX_MAGIC;
  ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;
  if (ctx->i2c_device)
    free(ctx->i2c_device);
  close(ctx->i2c_fd);
  xfree(ctx);
  return (0);
}

char *
ipmi_ssif_ctx_strerror(int32_t errnum)
{
  if (errnum >= IPMI_SSIF_CTX_ERR_SUCCESS && errnum <= IPMI_SSIF_CTX_ERR_ERRNUMRANGE)
    return ipmi_ssif_ctx_errmsg[errnum];
  else
    return ipmi_ssif_ctx_errmsg[IPMI_SSIF_CTX_ERR_ERRNUMRANGE];
}

int32_t
ipmi_ssif_ctx_errnum(ipmi_ssif_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_SSIF_CTX_ERR_NULL);
  else if (ctx->magic != IPMI_SSIF_CTX_MAGIC)
    return (IPMI_SSIF_CTX_ERR_INVALID);
  else
    return (ctx->errnum);
}

int8_t
ipmi_ssif_ctx_get_i2c_device(ipmi_ssif_ctx_t ctx, char **i2c_device)
{
  if (!(ctx && ctx->magic == IPMI_SSIF_CTX_MAGIC))
    return (-1);

  if (!i2c_device)
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_PARAMETERS;
      return (-1);
    }
  
  *i2c_device = ctx->i2c_device;
  ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;
  return (0);
}

int8_t
ipmi_ssif_ctx_get_ipmb_addr(ipmi_ssif_ctx_t ctx, uint8_t *ipmb_addr)
{
  if (!(ctx && ctx->magic == IPMI_SSIF_CTX_MAGIC))
    return (-1);

  if (!ipmb_addr)
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_PARAMETERS;
      return (-1);
    }

  *ipmb_addr = ctx->ipmb_addr;
  ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;
  return (0);
}

int8_t
ipmi_ssif_ctx_get_mode(ipmi_ssif_ctx_t ctx, uint8_t *mode)
{
  if (!(ctx && ctx->magic == IPMI_SSIF_CTX_MAGIC))
    return (-1);

  if (!mode)
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_PARAMETERS;
      return (-1);
    }

  *mode = ctx->mode;
  ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;
  return (0);
}

int8_t
ipmi_ssif_ctx_set_i2c_device(ipmi_ssif_ctx_t ctx, char* i2c_device)
{
  if (!(ctx && ctx->magic == IPMI_SSIF_CTX_MAGIC))
    return (-1);

  if (!i2c_device)
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_PARAMETERS;
      return (-1);
    }

  if (ctx->i2c_device)
    free(ctx->i2c_device);
  ctx->i2c_device = NULL;
  
  if (!(ctx->i2c_device = strdup(IPMI_DEFAULT_I2C_DEVICE)))
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_OUTMEM;
      return (-1);
    }
  
  ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;
  return (0);
}

int8_t
ipmi_ssif_ctx_set_ipmb_addr(ipmi_ssif_ctx_t ctx, uint8_t ipmb_addr)
{
  if (!(ctx && ctx->magic == IPMI_SSIF_CTX_MAGIC))
    return (-1);

  ctx->ipmb_addr = ipmb_addr;
  ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;
  return (0);
}

int8_t
ipmi_ssif_ctx_set_mode(ipmi_ssif_ctx_t ctx, uint8_t mode)
{
  if (!(ctx && ctx->magic == IPMI_SSIF_CTX_MAGIC))
    return (-1);

  if (!(mode == IPMI_SSIF_MODE_BLOCKING
        || mode == IPMI_SSIF_MODE_NONBLOCKING))
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_PARAMETERS;
      return (-1);
    }

  ctx->mode = mode;
  ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;
  return (0);
}

int8_t
ipmi_ssif_ctx_io_init(ipmi_ssif_ctx_t ctx)
{
  if (!(ctx && ctx->magic == IPMI_SSIF_CTX_MAGIC))
    return (-1);

  if ((ctx->i2c_fd = open (ctx->i2c_device, O_RDWR)) < 0)
    {
      if (errno == EACCES || errno == EPERM)
	ctx->errnum = IPMI_SSIF_CTX_ERR_PERMISSION;
      else if (errno == ENOENT 
	       || errno == ENOTDIR 
	       || errno == ENAMETOOLONG)
	ctx->errnum = IPMI_SSIF_CTX_ERR_IO_INIT;
      else
	ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL;
      return (-1);
    }

  /* zresearch webserver ipmb_addr: 0x341A */
  if (ioctl (ctx->i2c_fd, IPMI_I2C_SLAVE, ctx->ipmb_addr) < 0)
    {
      if (errno == EACCES || errno == EPERM)
	ctx->errnum = IPMI_SSIF_CTX_ERR_PERMISSION;
      else
	ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL;
      return (-1);
    }

  ctx->io_init = 1;
  ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;
  return (0);
}

int32_t
ipmi_ssif_write (ipmi_ssif_ctx_t ctx,
		 uint8_t *buf,
		 uint32_t buf_len)
{
  int32_t count;

  if (!(ctx && ctx->magic == IPMI_SSIF_CTX_MAGIC))
    return (-1);

  if (!buf || !buf_len)
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_PARAMETERS;
      return (-1);
    }

  if (!ctx->io_init)
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_IO_INIT;
      return (-1);
    }

  if (ctx->mode == IPMI_SSIF_MODE_BLOCKING)
    IPMI_MUTEX_LOCK(ctx->semid);
  else
    {
      int ret;

      if ((ret = IPMI_MUTEX_LOCK_INTERRUPTIBLE(ctx->semid)) < 0)
        {
          if (errno == EINTR || errno == EAGAIN)
            ctx->errnum = IPMI_SSIF_CTX_ERR_BUSY;
          else
            ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL;
          goto cleanup;
        }
    }

  if ((count = ipmi_i2c_smbus_write_block_data (ctx->i2c_fd, IPMI_SSIF_SMB_IPMI_REQUEST, buf_len, (uint8_t *)buf)) < 0)
    {
      if (errno == EACCES || errno == EPERM)
	ctx->errnum = IPMI_SSIF_CTX_ERR_PERMISSION;
      else
	ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL;
      goto cleanup_unlock;
    }

  ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;
  return (count);

 cleanup_unlock:
  IPMI_MUTEX_UNLOCK (ctx->semid);
 cleanup:
  return (-1);
}

int32_t
ipmi_ssif_read (ipmi_ssif_ctx_t ctx,
               uint8_t* buf,
               uint32_t buf_len)
{
  int32_t count = 0;
  int32_t rv = -1;

  if (!(ctx && ctx->magic == IPMI_SSIF_CTX_MAGIC))
    goto cleanup_unlock;

  if (!buf || !buf_len)
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_PARAMETERS;
      goto cleanup_unlock;
    }

  if (!ctx->io_init)
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_IO_INIT;
      goto cleanup_unlock;
    }

  if (buf_len > IPMI_I2C_SMBUS_BLOCK_MAX)
    buf_len = IPMI_I2C_SMBUS_BLOCK_MAX;
    
  if ((count = ipmi_i2c_smbus_read_block_data (ctx->i2c_fd, IPMI_SSIF_SMB_IPMI_RESPONSE, (uint8_t *)buf)) < 0)
    {
      if (errno == EACCES || errno == EPERM)
	ctx->errnum = IPMI_SSIF_CTX_ERR_PERMISSION;
      else
	ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL;
      goto cleanup_unlock;
    }
  
  rv = count;
  ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;
 cleanup_unlock:
  IPMI_MUTEX_UNLOCK (ctx->semid);
  return (rv);
}
