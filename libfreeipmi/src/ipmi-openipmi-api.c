/* 
   ipmi-openipmi-api.c: IPMI - OpenIPMI Api

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
#include <sys/select.h>
#include <sys/ioctl.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/ipmi-openipmi-api.h"

#include "ipmi-inband.h"
#include "ipmi-semaphores.h"

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"
#include "ipmi-common.h"
#include "xmalloc.h"

#define IPMI_OPENIPMI_BUFLEN    1024

#if HAVE_LINUX_IPMI_H
#include <linux/ipmi.h>
#elif HAVE_SYS_IPMI_H
#include <sys/ipmi.h>
#else  /* !HAVE_LINUX_IPMI_H && !HAVE_SYS_LINUX_H */
/* 
 * achu: Most of the definitions below are taken from linux/ipmi.h.
 *
 * Thanks to the ipmitool folks, who's code made it easier for me to
 * figure out the OpenIPMI interface more quickly.
 */

#define IPMI_SYSTEM_INTERFACE_ADDR_TYPE 0x0c
#define IPMI_BMC_CHANNEL                0xf

struct ipmi_system_interface_addr
{
  int           addr_type;
  short         channel;
  unsigned char lun;
};

struct ipmi_msg
{
  unsigned char   netfn;
  unsigned char   cmd;
  unsigned short  data_len;
  unsigned char  *data;
};

struct ipmi_req
{
  unsigned char      *addr;
  unsigned int        addr_len;
  long                msgid;
  struct ipmi_msg     msg;
};

struct ipmi_recv
{
  int                 recv_type; 
  unsigned char      *addr;
  unsigned int        addr_len;
  long                msgid; 
  struct ipmi_msg     msg;
};

#define IPMI_IOC_MAGIC             'i'
#define IPMICTL_RECEIVE_MSG_TRUNC  _IOWR(IPMI_IOC_MAGIC, 11, struct ipmi_recv)
#define IPMICTL_RECEIVE_MSG        _IOWR(IPMI_IOC_MAGIC, 12, struct ipmi_recv)
#if defined(__FreeBSD__)
#define IPMICTL_SEND_COMMAND       _IOW(IPMI_IOC_MAGIC,  13, struct ipmi_req)
#define IPMICTL_SET_MY_ADDRESS_CMD _IOW(IPMI_IOC_MAGIC,  17, unsigned int)
#else
#define IPMICTL_SEND_COMMAND       _IOR(IPMI_IOC_MAGIC,  13, struct ipmi_req)
#define IPMICTL_SET_MY_ADDRESS_CMD _IOR(IPMI_IOC_MAGIC,  17, unsigned int)
#endif
#define IPMICTL_GET_MY_ADDRESS_CMD _IOR(IPMI_IOC_MAGIC,  18, unsigned int)
#endif /* !HAVE_LINUX_IPMI_H && !HAVE_SYS_LINUX_H */

static char * ipmi_openipmi_ctx_errmsg[] =
  {
    "success",
    "openipmi context is null",
    "openipmi context is invalid",
    "invalid parameter",
    "permission denied",
    "device not found", 
    "io not initialized",
    "out of memory",
    "internal error",
    "error number out of range",
    NULL,
  };

#define IPMI_OPENIPMI_CTX_MAGIC 0xd00fd00f

#define IPMI_OPENIPMI_FLAGS_MASK IPMI_OPENIPMI_FLAGS_DEFAULT

struct ipmi_openipmi_ctx {
  uint32_t magic;
  int32_t errnum;
  uint32_t flags;
  char *driver_device;
  int device_fd;
  int io_init;
  int semid;
};

ipmi_openipmi_ctx_t
ipmi_openipmi_ctx_create(void)
{
  ipmi_openipmi_ctx_t ctx = NULL;

  ERR_CLEANUP ((ctx = (ipmi_openipmi_ctx_t)xmalloc(sizeof(struct ipmi_openipmi_ctx))));

  ctx->magic = IPMI_OPENIPMI_CTX_MAGIC;
  ctx->flags = IPMI_OPENIPMI_FLAGS_DEFAULT;
  ctx->io_init = 0;

  ERR_CLEANUP (!((ctx->semid = ipmi_mutex_init ()) < 0));
  ctx->errnum = IPMI_OPENIPMI_CTX_ERR_SUCCESS;
  return ctx;

 cleanup:
  if (ctx)
    xfree(ctx);
  return (NULL);
}

int8_t
ipmi_openipmi_ctx_destroy(ipmi_openipmi_ctx_t ctx)
{
  if (!(ctx && ctx->magic == IPMI_OPENIPMI_CTX_MAGIC))
    return (-1);

  ctx->magic = ~IPMI_OPENIPMI_CTX_MAGIC;
  ctx->errnum = IPMI_OPENIPMI_CTX_ERR_SUCCESS;
  if (ctx->driver_device)
    {
      xfree(ctx->driver_device);
      ctx->driver_device = NULL;
    }
  close(ctx->device_fd);
  xfree(ctx);
  return (0);
}

char *
ipmi_openipmi_ctx_strerror(int32_t errnum)
{
  if (errnum >= IPMI_OPENIPMI_CTX_ERR_SUCCESS && errnum <= IPMI_OPENIPMI_CTX_ERR_ERRNUMRANGE)
    return ipmi_openipmi_ctx_errmsg[errnum];
  else
    return ipmi_openipmi_ctx_errmsg[IPMI_OPENIPMI_CTX_ERR_ERRNUMRANGE];
}

int32_t
ipmi_openipmi_ctx_errnum(ipmi_openipmi_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_OPENIPMI_CTX_ERR_NULL);
  else if (ctx->magic != IPMI_OPENIPMI_CTX_MAGIC)
    return (IPMI_OPENIPMI_CTX_ERR_INVALID);
  else
    return (ctx->errnum);
}

int8_t 
ipmi_openipmi_ctx_get_driver_device(ipmi_openipmi_ctx_t ctx, char **driver_device)
{
  if (!(ctx && ctx->magic == IPMI_OPENIPMI_CTX_MAGIC))
    return (-1);

  if (!driver_device)
    {
      ctx->errnum = IPMI_OPENIPMI_CTX_ERR_PARAMETERS;
      return (-1);
    }

  *driver_device = ctx->driver_device;
  ctx->errnum = IPMI_OPENIPMI_CTX_ERR_SUCCESS;
  return (0);
}

int8_t 
ipmi_openipmi_ctx_get_flags(ipmi_openipmi_ctx_t ctx, uint32_t *flags)
{
  if (!(ctx && ctx->magic == IPMI_OPENIPMI_CTX_MAGIC))
    return (-1);

  if (!flags)
    {
      ctx->errnum = IPMI_OPENIPMI_CTX_ERR_PARAMETERS;
      return (-1);
    }

  *flags = ctx->flags;
  ctx->errnum = IPMI_OPENIPMI_CTX_ERR_SUCCESS;
  return (0);
}

int8_t 
ipmi_openipmi_ctx_set_driver_device(ipmi_openipmi_ctx_t ctx, char *device)
{
  if (!(ctx && ctx->magic == IPMI_OPENIPMI_CTX_MAGIC))
    return (-1);

  if (ctx->driver_device)
    {
      xfree(ctx->driver_device);
      ctx->driver_device = NULL;
    }

  if (device)
    {
      if (!(ctx->driver_device = strdup(device)))
        {
          ctx->errnum = IPMI_OPENIPMI_CTX_ERR_OUTMEM;
          return (-1);
        }
    }

  ctx->errnum = IPMI_OPENIPMI_CTX_ERR_SUCCESS;
  return (0);
}

int8_t 
ipmi_openipmi_ctx_set_flags(ipmi_openipmi_ctx_t ctx, uint32_t flags)
{
  if (!(ctx && ctx->magic == IPMI_OPENIPMI_CTX_MAGIC))
    return (-1);

  if (flags & ~IPMI_OPENIPMI_FLAGS_MASK)
    {
      ctx->errnum = IPMI_OPENIPMI_CTX_ERR_PARAMETERS;
      return (-1);
    }
  
  ctx->flags = flags;
  ctx->errnum = IPMI_OPENIPMI_CTX_ERR_SUCCESS;
  return (0);
}

int8_t
ipmi_openipmi_ctx_io_init(ipmi_openipmi_ctx_t ctx)
{
  unsigned int addr = IPMI_LAN_SLAVE_ADDRESS_BMC;
  char *device;

  if (!(ctx && ctx->magic == IPMI_OPENIPMI_CTX_MAGIC))
    return (-1);

  if (ctx->driver_device)
    device = ctx->driver_device;
  else
    device = IPMI_OPENIPMI_DRIVER_DEVICE_DEFAULT;

  ctx->device_fd = open (device, O_RDWR);
  if (ctx->device_fd < 0)
    {
      if (errno == EPERM || errno == EACCES)
        ctx->errnum = IPMI_OPENIPMI_CTX_ERR_PERMISSION;
      else if (errno == ENOENT)
        ctx->errnum = IPMI_OPENIPMI_CTX_ERR_DEVICE_NOTFOUND;
      else
        ERR_LOG(ctx->errnum = IPMI_OPENIPMI_CTX_ERR_INTERNAL);
      goto cleanup;
    }
  
  if (ioctl(ctx->device_fd, IPMICTL_SET_MY_ADDRESS_CMD, &addr) < 0) 
    {
      if (errno == EPERM || errno == EACCES)
        ctx->errnum = IPMI_OPENIPMI_CTX_ERR_PERMISSION;
      else
        ERR_LOG(ctx->errnum = IPMI_OPENIPMI_CTX_ERR_INTERNAL);
      goto cleanup;
    }

  ctx->io_init = 1;
  ctx->errnum = IPMI_OPENIPMI_CTX_ERR_SUCCESS;
  return (0);

 cleanup:
  if (ctx->device_fd)
    {
      close(ctx->device_fd);
      ctx->device_fd = 0;
    }
  return (-1);
}

static int8_t
_openipmi_write(ipmi_openipmi_ctx_t ctx, 
                uint8_t lun,
                uint8_t net_fn,
                fiid_obj_t obj_cmd_rq)
{
  uint8_t rq_buf_temp[IPMI_OPENIPMI_BUFLEN];
  uint8_t rq_buf[IPMI_OPENIPMI_BUFLEN];
  uint8_t rq_cmd;
  int32_t rq_buf_len;
  int32_t len;
  struct ipmi_system_interface_addr rq_addr;
  struct ipmi_req rq_packet;

  assert(ctx && ctx->magic == IPMI_OPENIPMI_CTX_MAGIC);
  assert(IPMI_BMC_LUN_VALID(lun));
  assert(IPMI_NET_FN_RQ_VALID(net_fn));
  assert(fiid_obj_valid(obj_cmd_rq));
  assert(fiid_obj_packet_valid(obj_cmd_rq));

  /* Due to API differences, we need to extract the cmd out of the
   * request.
   */
  memset(rq_buf_temp, '\0', IPMI_OPENIPMI_BUFLEN);
  if ((len = fiid_obj_get_all(obj_cmd_rq, rq_buf_temp, IPMI_OPENIPMI_BUFLEN)) <= 0)
    {
      ERR_LOG(ctx->errnum = IPMI_OPENIPMI_CTX_ERR_INTERNAL);
      return (-1);
    }

  rq_cmd = rq_buf_temp[0];
  if (len > 1)
    {
      memcpy(rq_buf, &rq_buf_temp[1], len - 1);
      rq_buf_len = len - 1;
    }
  else
    rq_buf_len = 0;

  rq_addr.addr_type = IPMI_SYSTEM_INTERFACE_ADDR_TYPE;
  rq_addr.channel = IPMI_BMC_CHANNEL;
  rq_addr.lun = lun;

  rq_packet.addr = (unsigned char *)&rq_addr;
  rq_packet.addr_len = sizeof(struct ipmi_system_interface_addr);
  rq_packet.msgid = 0;             
  rq_packet.msg.netfn = net_fn;
  rq_packet.msg.cmd = rq_cmd;
  rq_packet.msg.data_len = rq_buf_len;
  rq_packet.msg.data = rq_buf;

  if (ioctl(ctx->device_fd, IPMICTL_SEND_COMMAND, &rq_packet) < 0) 
    {
      ERR_LOG(ctx->errnum = IPMI_OPENIPMI_CTX_ERR_INTERNAL);
      return (-1);
    }

  return (0);
}

static int8_t
_openipmi_read (ipmi_openipmi_ctx_t ctx, 
                fiid_obj_t obj_cmd_rs)
{
  uint8_t rs_buf_temp[IPMI_OPENIPMI_BUFLEN];
  uint8_t rs_buf[IPMI_OPENIPMI_BUFLEN];
  struct ipmi_system_interface_addr rs_addr;
  struct ipmi_recv rs_packet;
  fd_set read_fds;
  int n;

  rs_packet.addr = (unsigned char *)&rs_addr;
  rs_packet.addr_len = sizeof(struct ipmi_system_interface_addr);
  rs_packet.msg.data = rs_buf_temp;
  rs_packet.msg.data_len = IPMI_OPENIPMI_BUFLEN;

  FD_ZERO(&read_fds);
  FD_SET(ctx->device_fd, &read_fds);

  if ((n = select(ctx->device_fd + 1, 
                  &read_fds,
                  NULL,
                  NULL,
                  NULL)) < 0)
    {
      ERR_LOG(ctx->errnum = IPMI_OPENIPMI_CTX_ERR_INTERNAL);
      return (-1);
    }

  if (!n)
    {
      ERR_LOG(ctx->errnum = IPMI_OPENIPMI_CTX_ERR_INTERNAL);
      return (-1);
    }

  if (ioctl(ctx->device_fd, IPMICTL_RECEIVE_MSG_TRUNC, &rs_packet) < 0) 
    {
      ERR_LOG(ctx->errnum = IPMI_OPENIPMI_CTX_ERR_INTERNAL);
      return (-1);
    }

  /* achu: atleast the completion code should be returned */
  if (!rs_packet.msg.data_len)
    {
      ERR_LOG(ctx->errnum = IPMI_OPENIPMI_CTX_ERR_INTERNAL);
      return (-1);
    }

  rs_buf[0] = rs_packet.msg.cmd;
  if (rs_packet.msg.data_len >= (IPMI_OPENIPMI_BUFLEN - 1))
    rs_packet.msg.data_len = IPMI_OPENIPMI_BUFLEN - 1;
  memcpy(rs_buf + 1, rs_buf_temp, rs_packet.msg.data_len);

  if (fiid_obj_set_all(obj_cmd_rs, rs_buf, rs_packet.msg.data_len + 1) < 0)
    {
      ERR_LOG(ctx->errnum = IPMI_OPENIPMI_CTX_ERR_INTERNAL);
      return (-1);
    }

  return (0);
}

int8_t
ipmi_openipmi_cmd (ipmi_openipmi_ctx_t ctx, 
                   uint8_t lun,
                   uint8_t net_fn,
                   fiid_obj_t obj_cmd_rq,
                   fiid_obj_t obj_cmd_rs)
{
  if (!(ctx && ctx->magic == IPMI_OPENIPMI_CTX_MAGIC))
    return (-1); 
 
  if (!IPMI_BMC_LUN_VALID(lun)
      || !IPMI_NET_FN_RQ_VALID(net_fn)
      || !fiid_obj_valid(obj_cmd_rq)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      ctx->errnum = IPMI_OPENIPMI_CTX_ERR_PARAMETERS;
      return (-1); 
    }
  
  if (!ctx->io_init)
    {
      ctx->errnum = IPMI_OPENIPMI_CTX_ERR_IO_INIT;
      return (-1); 
    }

  if (!fiid_obj_packet_valid(obj_cmd_rq))
    {
      ctx->errnum = IPMI_OPENIPMI_CTX_ERR_PARAMETERS;
      return (-1); 
    }
 
  if (_openipmi_write(ctx,
                      lun,
                      net_fn,
                      obj_cmd_rq) < 0)
    return (-1);

  if (_openipmi_read(ctx,
                     obj_cmd_rs) < 0)
    return (-1);

  return (0);
}
