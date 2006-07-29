/* 
   ipmi-ssif-api.c: IPMI - SMBus System Interface - SMS Api

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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <sys/ioctl.h>
#include <errno.h>
#include <assert.h>

#include "freeipmi/ipmi-ssif-api.h"

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

#define IPMI_SSIF_SINGLE_PART_WRITE_SMBUS_CMD          0x02
#define IPMI_SSIF_MULTI_PART_WRITE_START_SMBUS_CMD     0x06
#define IPMI_SSIF_MULTI_PART_WRITE_MIDDLE_SMBUS_CMD    0x07
#define IPMI_SSIF_MULTI_PART_WRITE_END_SMBUS_CMD       0x07

#define IPMI_SSIF_SINGLE_PART_READ_SMBUS_CMD           0x03
#define IPMI_SSIF_MULTI_PART_READ_START_SMBUS_CMD      0x03
#define IPMI_SSIF_MULTI_PART_READ_MIDDLE_SMBUS_CMD     0x09
#define IPMI_SSIF_MULTI_PART_READ_END_SMBUS_CMD        0x09
#define IPMI_SSIF_MULTI_PART_READ_RETRY_SMBUS_CMD      0x0A

#define IPMI_SSIF_MULTI_PART_READ_START_SIZE        30
#define IPMI_SSIF_MULTI_PART_READ_START_PATTERN1    0x0
#define IPMI_SSIF_MULTI_PART_READ_START_PATTERN2    0x1
#define IPMI_SSIF_MULTI_PART_READ_END_PATTERN       0xFF

/* START: copied from <linux/i2c.h> and <linux/i2c-dev.h>, */
/*        and prefixed IPMI.                               */
#define IPMI_I2C_SLAVE               0x0703
#define IPMI_I2C_SMBUS               0x0720
#define IPMI_I2C_SMBUS_BLOCK_DATA    5
#define IPMI_I2C_SMBUS_BLOCK_MAX     32
#define IPMI_I2C_SMBUS_READ          1
#define IPMI_I2C_SMBUS_WRITE         0

union ipmi_i2c_smbus_data
{
  uint8_t  byte;
  uint16_t word;
  uint8_t  block[IPMI_I2C_SMBUS_BLOCK_MAX + 3];
};

struct ipmi_i2c_smbus_ioctl_data
{
  uint8_t  read_write;
  uint8_t  command;
  uint32_t size;
  union ipmi_i2c_smbus_data *data;
};
/* END: copied from <linux/i2c.h> and <linux/i2c-dev.h>, */
/*      and prefixed IPMI.                               */

static inline int 
ipmi_i2c_smbus_access (int dev_fd, 
		       char read_write, 
		       uint8_t command, 
		       union ipmi_i2c_smbus_data *data)
{
  struct ipmi_i2c_smbus_ioctl_data args;
  
  args.read_write = read_write;
  args.command = command;
  args.size = IPMI_I2C_SMBUS_BLOCK_DATA;
  args.data = data;
  
  return (ioctl (dev_fd, IPMI_I2C_SMBUS, &args));
}

static inline ssize_t 
ipmi_ssif_single_part_write (int dev_fd, 
			     uint8_t *buf, 
			     size_t buf_len)
{

  union ipmi_i2c_smbus_data data;
  int i;
  
  data.block[0] = buf_len;
  for (i = 0; i < buf_len; i++)
    {
      data.block[i + 1] = buf[i];
    }
  
  return (ipmi_i2c_smbus_access (dev_fd, 
				 IPMI_I2C_SMBUS_WRITE, 
				 IPMI_SSIF_SINGLE_PART_WRITE_SMBUS_CMD, 
				 &data));
}

static inline ssize_t 
ipmi_ssif_multi_part_write (int dev_fd, 
			    uint8_t *buf, 
			    size_t buf_len)
{
  union ipmi_i2c_smbus_data data;
  int middle_parts;
  int i;
  int mpart;
  int index;
  
  if (buf_len % IPMI_I2C_SMBUS_BLOCK_MAX == 0)
    {
      fprintf (stderr, "%s:%s(): "
	       "PECULIAR IPMI COMMAND: As of this writing, "
	       "there are no standard IPMI messages to the "
	       "BMC that are exact multiples of %d.  This "
	       "command can be OEM/group network functions "
	       "(network function codes 2Ch:3Fh) in your "
	       "BMC implementation.  Please report to "
	       "FreeIPMI mailing list <freeipmi-devel@gnu.org>\n", 
	       __FILE__, __PRETTY_FUNCTION__, 
	       IPMI_I2C_SMBUS_BLOCK_MAX);
      return (-1);
    }
  
  middle_parts = (buf_len / IPMI_I2C_SMBUS_BLOCK_MAX) - 1;
  
  data.block[0] = IPMI_I2C_SMBUS_BLOCK_MAX;
  for (i = 0; i < IPMI_I2C_SMBUS_BLOCK_MAX; i++)
    {
      data.block[i + 1] = buf[i];
    }
  if (ipmi_i2c_smbus_access (dev_fd, 
			     IPMI_I2C_SMBUS_WRITE, 
			     IPMI_SSIF_MULTI_PART_WRITE_START_SMBUS_CMD, 
			     &data) == -1)
    {
      return (-1);
    }
  
  for (mpart = 1; mpart <= middle_parts; mpart++)
    {
      index = mpart * IPMI_I2C_SMBUS_BLOCK_MAX;
      data.block[0] = IPMI_I2C_SMBUS_BLOCK_MAX;
      for (i = 0; i < IPMI_I2C_SMBUS_BLOCK_MAX; i++)
	{
	  data.block[i + 1] = buf[index + i];
	}
      if (ipmi_i2c_smbus_access (dev_fd, 
				 IPMI_I2C_SMBUS_WRITE, 
				 IPMI_SSIF_MULTI_PART_WRITE_MIDDLE_SMBUS_CMD, 
				 &data) == -1)
	{
	  return (-1);
	}
    }
  
  index = (middle_parts + 1) * IPMI_I2C_SMBUS_BLOCK_MAX;
  data.block[0] = buf_len % IPMI_I2C_SMBUS_BLOCK_MAX;
  for (i = 0; i < data.block[0]; i++)
    {
      data.block[i + 1] = buf[index + i];
    }
  return (ipmi_i2c_smbus_access (dev_fd, 
				 IPMI_I2C_SMBUS_WRITE, 
				 IPMI_SSIF_MULTI_PART_WRITE_END_SMBUS_CMD, 
				 &data));
}

static inline ssize_t 
_ipmi_ssif_read (int dev_fd, 
		 uint8_t *buf, 
		 size_t buf_len)
{
  union ipmi_i2c_smbus_data data;
  int bytes_read = 0;
  int bytes_copied = 0;
  int length;
  int block_number;
  int sindex;
  int multi_read_start = 0;
  int i;
  
  if (buf == NULL || buf_len <= 0)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if (ipmi_i2c_smbus_access (dev_fd, 
			     IPMI_I2C_SMBUS_READ, 
			     IPMI_SSIF_SINGLE_PART_READ_SMBUS_CMD, 
			     &data) == -1)
    {
      return (-1);
    }
  
  if (data.block[0] == IPMI_SSIF_MULTI_PART_READ_START_SIZE && 
      data.block[1] == IPMI_SSIF_MULTI_PART_READ_START_PATTERN1 && 
      data.block[2] == IPMI_SSIF_MULTI_PART_READ_START_PATTERN2)
    {
      sindex = 3;
      multi_read_start = 1;
    }
  else
    {
      sindex = 1;
    }
  
  length = data.block[0];
  bytes_read = length;
  
  if (bytes_read > buf_len)
    {
      length = buf_len;
    }
  
  for (i = 0; i < length; i++)
    {
      buf[i] = data.block[sindex + i];
    }
  
  bytes_copied = length;
  
  while (multi_read_start)
    {
      if (ipmi_i2c_smbus_access (dev_fd, 
				 IPMI_I2C_SMBUS_READ, 
				 IPMI_SSIF_MULTI_PART_READ_MIDDLE_SMBUS_CMD, 
				 &data) == -1)
	{
	  return (-1);
	}
      
      length = data.block[0];
      block_number = data.block[1];
      bytes_read += length;
      
      if ((bytes_copied + length) > buf_len)
	{
	  length = buf_len - bytes_copied;
	}
      
      for (i = 0; i < length; i++)
	{
	  buf[bytes_copied + i] = data.block[i + 2];
	}
      
      bytes_copied += length;
      
      if (block_number == IPMI_SSIF_MULTI_PART_READ_END_PATTERN)
	{
	  break;
	}
    }
  
  return bytes_read;
}

#define IPMI_SSIF_FLAGS_MASK IPMI_SSIF_FLAGS_NONBLOCKING

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
  uint8_t ipmb_address;
  uint32_t flags;
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
  ctx->ipmb_address = IPMI_DEFAULT_SSIF_IPMB_ADDR;
  ctx->flags = IPMI_SSIF_FLAGS_DEFAULT;
  ctx->i2c_fd = -1;
  ctx->io_init = 0;

  ERR_CLEANUP (!((ctx->semid = ipmi_mutex_init ()) < 0));

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
ipmi_ssif_ctx_get_ipmb_address(ipmi_ssif_ctx_t ctx, uint8_t *ipmb_address)
{
  if (!(ctx && ctx->magic == IPMI_SSIF_CTX_MAGIC))
    return (-1);

  if (!ipmb_address)
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_PARAMETERS;
      return (-1);
    }

  *ipmb_address = ctx->ipmb_address;
  ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;
  return (0);
}

int8_t
ipmi_ssif_ctx_get_flags(ipmi_ssif_ctx_t ctx, uint32_t *flags)
{
  if (!(ctx && ctx->magic == IPMI_SSIF_CTX_MAGIC))
    return (-1);

  if (!flags)
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_PARAMETERS;
      return (-1);
    }

  *flags = ctx->flags;
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
ipmi_ssif_ctx_set_ipmb_address(ipmi_ssif_ctx_t ctx, uint8_t ipmb_address)
{
  if (!(ctx && ctx->magic == IPMI_SSIF_CTX_MAGIC))
    return (-1);

  ctx->ipmb_address = ipmb_address;
  ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;
  return (0);
}

int8_t
ipmi_ssif_ctx_set_flags(ipmi_ssif_ctx_t ctx, uint32_t flags)
{
  if (!(ctx && ctx->magic == IPMI_SSIF_CTX_MAGIC))
    return (-1);

  if (flags & ~IPMI_SSIF_FLAGS_MASK)
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_PARAMETERS;
      return (-1);
    }

  ctx->flags = flags;
  ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;
  return (0);
}

int8_t
ipmi_ssif_ctx_io_init(ipmi_ssif_ctx_t ctx)
{
  if (!(ctx && ctx->magic == IPMI_SSIF_CTX_MAGIC))
    return (-1);
  
  if (!(ctx->i2c_device && ctx->i2c_fd))
    {
      ctx->errnum = EINVAL;
      return (-1);
    }
  
  if ((ctx->i2c_fd = open (ctx->i2c_device, O_RDWR)) < 0)
    {
      if (errno == EACCES || errno == EPERM)
	ctx->errnum = IPMI_SSIF_CTX_ERR_PERMISSION;
      else if (errno == ENOENT 
	       || errno == ENOTDIR 
	       || errno == ENAMETOOLONG)
	ctx->errnum = IPMI_SSIF_CTX_ERR_IO_INIT;
      else
	ERR_LOG(ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL);
      return (-1);
    }

  if (ioctl (ctx->i2c_fd, IPMI_I2C_SLAVE, ctx->ipmb_address) < 0)
    {
      if (errno == EACCES || errno == EPERM)
	ctx->errnum = IPMI_SSIF_CTX_ERR_PERMISSION;
      else
	ERR_LOG(ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL);
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

  if (!(ctx->flags & IPMI_SSIF_FLAGS_NONBLOCKING))
    IPMI_MUTEX_LOCK(ctx->semid);
  else
    {
      int ret;

      if ((ret = IPMI_MUTEX_LOCK_INTERRUPTIBLE(ctx->semid)) < 0)
        {
          if (errno == EINTR || errno == EAGAIN)
            ERR_LOG(ctx->errnum = IPMI_SSIF_CTX_ERR_BUSY);
          else
            ERR_LOG(ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL);
          goto cleanup;
        }
    }
  
  if (buf_len <= IPMI_I2C_SMBUS_BLOCK_MAX)
    {
      count = ipmi_ssif_single_part_write (ctx->i2c_fd, 
					   buf, 
					   buf_len);
    }
  else 
    {
      count = ipmi_ssif_multi_part_write (ctx->i2c_fd, 
					  buf, 
					  buf_len);
    }
  
  if (count < 0)
    {
      if (errno == EACCES || errno == EPERM)
	ctx->errnum = IPMI_SSIF_CTX_ERR_PERMISSION;
      else
	ERR_LOG(ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL);
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
  
  if ((count = _ipmi_ssif_read (ctx->i2c_fd, 
				buf, 
				buf_len)) < 0)
    {
      if (errno == EACCES || errno == EPERM)
	ctx->errnum = IPMI_SSIF_CTX_ERR_PERMISSION;
      else
	ERR_LOG(ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL);
      goto cleanup_unlock;
    }
  
  rv = count;
  ctx->errnum = IPMI_SSIF_CTX_ERR_SUCCESS;
 cleanup_unlock:
  IPMI_MUTEX_UNLOCK (ctx->semid);
  return (rv);
}

static int8_t
_ipmi_ssif_cmd_write(ipmi_ssif_ctx_t ctx, 
		     uint8_t lun,
		     uint8_t net_fn,
		     fiid_obj_t obj_cmd_rq)
{
  uint8_t *pkt = NULL;
  uint32_t pkt_len;
  int32_t hdr_len, cmd_len;
  fiid_obj_t obj_hdr = NULL;
  int rv = -1;

  assert(ctx && ctx->magic == IPMI_SSIF_CTX_MAGIC);
  assert(IPMI_BMC_LUN_VALID(lun));
  assert(IPMI_NET_FN_RQ_VALID(net_fn));
  assert(fiid_obj_valid(obj_cmd_rq));
  assert(fiid_obj_packet_valid(obj_cmd_rq));

  if ((hdr_len = fiid_template_len_bytes(tmpl_hdr_kcs)) < 0)
    {
      ERR_LOG(ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL);
      return (-1);
    }
  
  if ((cmd_len = fiid_obj_len_bytes(obj_cmd_rq)) < 0)
    {
      ERR_LOG(ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL);
      return (-1);
    }
  
  if (!(obj_hdr = fiid_obj_create(tmpl_hdr_kcs)))
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_OUTMEM;
      goto cleanup;
    }
  
  pkt_len = hdr_len + cmd_len;
  if (!(pkt = (uint8_t *)malloc (pkt_len)))
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_OUTMEM;
      goto cleanup;
    }
  memset (pkt, 0, pkt_len);
    
  if (fill_hdr_ipmi_kcs (lun,
			 net_fn,
			 obj_hdr) < 0)
    {
      ERR_LOG(ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL);
      return (-1);
    }
  
  if (assemble_ipmi_kcs_pkt (obj_hdr,
			     obj_cmd_rq,
			     pkt,
			     pkt_len) < 0)
    {
      ERR_LOG(ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL);
      return (-1);
    }
  
  if (ipmi_ssif_write (ctx, pkt, pkt_len) < 0)
    {
      ERR_LOG(ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL);
      return (-1);
    }

  rv = 0;
 cleanup:
  if (obj_hdr)
    fiid_obj_destroy(obj_hdr);
  if (pkt)
    free(pkt);
  return rv;
}

static int8_t
_ipmi_ssif_cmd_read(ipmi_ssif_ctx_t ctx, 
		    fiid_obj_t obj_cmd_rs)
{
  uint8_t *pkt;
  uint32_t pkt_len;
  int32_t hdr_len, cmd_len;
  int32_t read_len;
  fiid_obj_t obj_hdr = NULL;
  fiid_field_t *tmpl = NULL;
  int8_t rv = -1;

  assert(ctx && ctx->magic == IPMI_SSIF_CTX_MAGIC);
  assert(fiid_obj_valid(obj_cmd_rs));

  if ((hdr_len = fiid_template_len_bytes(tmpl_hdr_kcs)) < 0)
    {
      ERR_LOG(ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL);
      return -1;
    }
  
  if (!(tmpl = fiid_obj_template(obj_cmd_rs)) < 0)
    {
      ERR_LOG(ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL);
      goto cleanup;
    }

  if ((cmd_len = fiid_template_len_bytes(tmpl)) < 0)
    {
      ERR_LOG(ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL);
      goto cleanup;
    }

  if (!(obj_hdr = fiid_obj_create(tmpl_hdr_kcs)))
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_OUTMEM;
      goto cleanup;
    }

  pkt_len = hdr_len + cmd_len;
  
  if (!(pkt = (uint8_t *)malloc(pkt_len)))
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_OUTMEM;
      goto cleanup;
    }
  memset (pkt, 0, pkt_len);

  if ((read_len = ipmi_ssif_read (ctx, 
				  pkt,
				  pkt_len)) < 0)
    {
      ERR_LOG(ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL);
      goto cleanup;
    }
  
  if (unassemble_ipmi_kcs_pkt (pkt,
			       read_len,
			       obj_hdr,
			       obj_cmd_rs) < 0)
    {
      ERR_LOG(ctx->errnum = IPMI_SSIF_CTX_ERR_INTERNAL);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  if (tmpl)
    fiid_template_free(tmpl);
  if (obj_hdr)
    fiid_obj_destroy(obj_hdr);
  return rv;
}

int8_t
ipmi_ssif_cmd (ipmi_ssif_ctx_t ctx, 
	       uint8_t lun,
	       uint8_t net_fn,
	       fiid_obj_t obj_cmd_rq,
	       fiid_obj_t obj_cmd_rs)
{
  if (!(ctx && ctx->magic == IPMI_SSIF_CTX_MAGIC))
    return (-1); 
 
  if (!IPMI_BMC_LUN_VALID(lun)
      || !IPMI_NET_FN_RQ_VALID(net_fn)
      || !fiid_obj_valid(obj_cmd_rq)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_PARAMETERS;
      return (-1); 
    }
  
  if (!ctx->io_init)
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_IO_INIT;
      return (-1); 
    }

  if (!fiid_obj_packet_valid(obj_cmd_rq))
    {
      ctx->errnum = IPMI_SSIF_CTX_ERR_PARAMETERS;
      return (-1); 
    }
 
  if (_ipmi_ssif_cmd_write(ctx, lun, net_fn, obj_cmd_rq) < 0)
    return -1;

  if (_ipmi_ssif_cmd_read(ctx, obj_cmd_rs) < 0)
    return -1;

  return (0);
}
