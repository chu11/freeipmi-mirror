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
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_STROPTS_H
#include <sys/stropts.h>
#endif
#include <sys/select.h>
#include <sys/ioctl.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/driver/ipmi-sunbmc-driver.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"

#include "libcommon/ipmi-err-wrappers.h"
#include "libcommon/ipmi-fiid-wrappers.h"

#include "freeipmi-portability.h"

#define IPMI_SUNBMC_BUFLEN    1024

#define IPMI_SUNBMC_TIMEOUT     60

/* Thanks to the ipmitool folks, who's code made it easier for me to
 * figure out the Solaris /dev/bmc interface more quickly.
 */

static char * ipmi_sunbmc_ctx_errmsg[] =
  {
    "success",
    "sunbmc context null",
    "sunbmc context invalid",
    "invalid parameter",
    "permission denied",
    "device not found", 
    "io not initialized",
    "out of memory",
    "driver timeout",
    "internal system error"
    "internal error",
    "errnum out of range",
    NULL,
  };

#define IPMI_SUNBMC_CTX_MAGIC 0xd0bbd0bb

#define IPMI_SUNBMC_FLAGS_MASK IPMI_SUNBMC_FLAGS_DEFAULT

struct ipmi_sunbmc_ctx {
  uint32_t magic;
  int32_t errnum;
  uint32_t flags;
  char *driver_device;
  int device_fd;
  int io_init;
};

ipmi_sunbmc_ctx_t
ipmi_sunbmc_ctx_create(void)
{
  ipmi_sunbmc_ctx_t ctx = NULL;

  ERR_CLEANUP ((ctx = (ipmi_sunbmc_ctx_t)malloc(sizeof(struct ipmi_sunbmc_ctx))));
  memset(ctx, '\0', sizeof(struct ipmi_sunbmc_ctx));

  ctx->magic = IPMI_SUNBMC_CTX_MAGIC;
  ctx->flags = IPMI_SUNBMC_FLAGS_DEFAULT;
  ctx->driver_device = NULL;
  ctx->device_fd = -1;
  ctx->io_init = 0;

  ctx->errnum = IPMI_SUNBMC_CTX_ERR_SUCCESS;
  return ctx;

 cleanup:
  if (ctx)
    free(ctx);
  return (NULL);
}

int8_t
ipmi_sunbmc_ctx_destroy(ipmi_sunbmc_ctx_t ctx)
{
  ERR(ctx && ctx->magic == IPMI_SUNBMC_CTX_MAGIC);

  ctx->magic = ~IPMI_SUNBMC_CTX_MAGIC;
  ctx->errnum = IPMI_SUNBMC_CTX_ERR_SUCCESS;
  if (ctx->driver_device)
    {
      free(ctx->driver_device);
      ctx->driver_device = NULL;
    }
  close(ctx->device_fd);
  free(ctx);
  return (0);
}

char *
ipmi_sunbmc_ctx_strerror(int32_t errnum)
{
  if (errnum >= IPMI_SUNBMC_CTX_ERR_SUCCESS && errnum <= IPMI_SUNBMC_CTX_ERR_ERRNUMRANGE)
    return ipmi_sunbmc_ctx_errmsg[errnum];
  else
    return ipmi_sunbmc_ctx_errmsg[IPMI_SUNBMC_CTX_ERR_ERRNUMRANGE];
}

int32_t
ipmi_sunbmc_ctx_errnum(ipmi_sunbmc_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_SUNBMC_CTX_ERR_NULL);
  else if (ctx->magic != IPMI_SUNBMC_CTX_MAGIC)
    return (IPMI_SUNBMC_CTX_ERR_INVALID);
  else
    return (ctx->errnum);
}

int8_t 
ipmi_sunbmc_ctx_get_driver_device(ipmi_sunbmc_ctx_t ctx, char **driver_device)
{
  ERR(ctx && ctx->magic == IPMI_SUNBMC_CTX_MAGIC);

  SUNBMC_ERR_PARAMETERS(driver_device);

  *driver_device = ctx->driver_device;
  ctx->errnum = IPMI_SUNBMC_CTX_ERR_SUCCESS;
  return (0);
}

int8_t 
ipmi_sunbmc_ctx_get_flags(ipmi_sunbmc_ctx_t ctx, uint32_t *flags)
{
  ERR(ctx && ctx->magic == IPMI_SUNBMC_CTX_MAGIC);

  SUNBMC_ERR_PARAMETERS(flags);

  *flags = ctx->flags;
  ctx->errnum = IPMI_SUNBMC_CTX_ERR_SUCCESS;
  return (0);
}

int8_t 
ipmi_sunbmc_ctx_set_driver_device(ipmi_sunbmc_ctx_t ctx, char *device)
{
  ERR(ctx && ctx->magic == IPMI_SUNBMC_CTX_MAGIC);

  SUNBMC_ERR_PARAMETERS(device);

  if (ctx->driver_device)
    free(ctx->driver_device);
  ctx->driver_device = NULL;

  SUNBMC_ERR_OUT_OF_MEMORY((ctx->driver_device = strdup(device)));

  ctx->errnum = IPMI_SUNBMC_CTX_ERR_SUCCESS;
  return (0);
}

int8_t 
ipmi_sunbmc_ctx_set_flags(ipmi_sunbmc_ctx_t ctx, uint32_t flags)
{
  ERR(ctx && ctx->magic == IPMI_SUNBMC_CTX_MAGIC);

  SUNBMC_ERR_PARAMETERS(!(flags & ~IPMI_SUNBMC_FLAGS_MASK));
  
  ctx->flags = flags;
  ctx->errnum = IPMI_SUNBMC_CTX_ERR_SUCCESS;
  return (0);
}

int8_t
ipmi_sunbmc_ctx_io_init(ipmi_sunbmc_ctx_t ctx)
{
  char *device;

  ERR(ctx && ctx->magic == IPMI_SUNBMC_CTX_MAGIC);

  if (ctx->io_init)
    goto out;

  if (ctx->driver_device)
    device = ctx->driver_device;
  else
    device = IPMI_SUNBMC_DRIVER_DEVICE_DEFAULT;

  SUNBMC_ERR_CLEANUP(!((ctx->device_fd = open (device, 
                                               O_RDWR)) < 0));
  
  

  ctx->io_init = 1;
 out:
  ctx->errnum = IPMI_SUNBMC_CTX_ERR_SUCCESS;
  return (0);

 cleanup:
  close(ctx->device_fd);
  ctx->device_fd = -1;
  return (-1);
}

static int8_t
_sunbmc_write(ipmi_sunbmc_ctx_t ctx, 
              uint8_t lun,
              uint8_t net_fn,
              fiid_obj_t obj_cmd_rq)
{
  uint8_t rq_buf_temp[IPMI_SUNBMC_BUFLEN];
  uint8_t rq_buf[IPMI_SUNBMC_BUFLEN];
  uint8_t rq_cmd;
  int32_t rq_buf_len;
  int32_t len;
#if 0
  struct ipmi_system_interface_addr rq_addr;
  struct ipmi_req rq_packet;
#endif

  assert(ctx && ctx->magic == IPMI_SUNBMC_CTX_MAGIC);
  assert(IPMI_BMC_LUN_VALID(lun));
  assert(IPMI_NET_FN_RQ_VALID(net_fn));
  assert(fiid_obj_valid(obj_cmd_rq));
  assert(fiid_obj_packet_valid(obj_cmd_rq));

  /* Due to API differences, we need to extract the cmd out of the
   * request.
   */
  memset(rq_buf_temp, '\0', IPMI_SUNBMC_BUFLEN);

  SUNBMC_ERR_INTERNAL_ERROR(!((len = fiid_obj_get_all(obj_cmd_rq, 
                                                      rq_buf_temp, 
                                                      IPMI_SUNBMC_BUFLEN)) <= 0));

  rq_cmd = rq_buf_temp[0];
  if (len > 1)
    {
      memcpy(rq_buf, &rq_buf_temp[1], len - 1);
      rq_buf_len = len - 1;
    }
  else
    rq_buf_len = 0;

#if 0
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

  SUNBMC_ERR(!(ioctl(ctx->device_fd, 
                     IPMICTL_SEND_COMMAND,
                     &rq_packet) < 0));
#endif

  return (0);
}

static int8_t
_sunbmc_read (ipmi_sunbmc_ctx_t ctx, 
              fiid_obj_t obj_cmd_rs)
{
  uint8_t rs_buf_temp[IPMI_SUNBMC_BUFLEN];
  uint8_t rs_buf[IPMI_SUNBMC_BUFLEN];
#if 0
  struct ipmi_system_interface_addr rs_addr;
  struct ipmi_recv rs_packet;
#endif
  fd_set read_fds;
  struct timeval tv;
  int n;

#if 0
  rs_packet.addr = (unsigned char *)&rs_addr;
  rs_packet.addr_len = sizeof(struct ipmi_system_interface_addr);
  rs_packet.msg.data = rs_buf_temp;
  rs_packet.msg.data_len = IPMI_SUNBMC_BUFLEN;
#endif

  FD_ZERO(&read_fds);
  FD_SET(ctx->device_fd, &read_fds);

  tv.tv_sec = IPMI_SUNBMC_TIMEOUT;
  tv.tv_usec = 0;

  SUNBMC_ERR(!((n = select(ctx->device_fd + 1, 
                           &read_fds,
                           NULL,
                           NULL,
                           &tv)) < 0));

  if (!n)
    {
      /* Could be due to a different error, but we assume a timeout */
      SUNBMC_ERRNUM_SET(IPMI_SUNBMC_CTX_ERR_DRIVER_TIMEOUT);
      return (-1);
    }

#if 0
  SUNBMC_ERR(!(ioctl(ctx->device_fd, 
                     IPMICTL_RECEIVE_MSG_TRUNC, 
                     &rs_packet) < 0)); 

  /* achu: atleast the completion code should be returned */
  if (!rs_packet.msg.data_len)
    {
      SUNBMC_ERRNUM_SET(IPMI_SUNBMC_CTX_ERR_SYSTEM_ERROR);
      return (-1);
    }

  rs_buf[0] = rs_packet.msg.cmd;
  if (rs_packet.msg.data_len >= (IPMI_SUNBMC_BUFLEN - 1))
    rs_packet.msg.data_len = IPMI_SUNBMC_BUFLEN - 1;
  memcpy(rs_buf + 1, rs_buf_temp, rs_packet.msg.data_len);

  SUNBMC_ERR_INTERNAL_ERROR(!(fiid_obj_set_all(obj_cmd_rs, 
                                               rs_buf, 
                                               rs_packet.msg.data_len + 1) < 0));
#endif

  return (0);
}

int8_t
ipmi_sunbmc_cmd (ipmi_sunbmc_ctx_t ctx, 
                 uint8_t lun,
                 uint8_t net_fn,
                 fiid_obj_t obj_cmd_rq,
                 fiid_obj_t obj_cmd_rs)
{
  ERR(ctx && ctx->magic == IPMI_SUNBMC_CTX_MAGIC);
 
  SUNBMC_ERR_PARAMETERS(IPMI_BMC_LUN_VALID(lun)
                        && IPMI_NET_FN_RQ_VALID(net_fn)
                        && fiid_obj_valid(obj_cmd_rq)
                        && fiid_obj_valid(obj_cmd_rs)
                        && fiid_obj_packet_valid(obj_cmd_rq));
  
  SUNBMC_ERR_IO_NOT_INITIALIZED(ctx->io_init);

  if (_sunbmc_write(ctx,
                    lun,
                    net_fn,
                    obj_cmd_rq) < 0)
    return (-1);

  if (_sunbmc_read(ctx,
                   obj_cmd_rs) < 0)
    return (-1);

  return (0);
}
