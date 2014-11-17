/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

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
#if HAVE_SYS_IOCCOM_H
#include <sys/ioccom.h>         /* solaris _IOR, etc. */
#endif /* !HAVE_SYS_IOCCOM_H */
#include <sys/select.h>
#ifdef __CYGWIN__
#define __USE_LINUX_IOCTL_DEFS
#endif /* !__CYGWIN__ */
#include <sys/ioctl.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/driver/ipmi-openipmi-driver.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"

#include "ipmi-driver-trace.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"

#define IPMI_OPENIPMI_BUFLEN    1024

#define IPMI_OPENIPMI_TIMEOUT     60

#if HAVE_LINUX_IPMI_H
#include <linux/ipmi.h>
#elif HAVE_SYS_IPMI_H
#include <sys/ipmi.h>
#else  /* !HAVE_LINUX_IPMI_H && !HAVE_SYS_IPMI_H */
/*
 * achu: Most of the definitions below are taken from linux/ipmi.h.
 *
 * Thanks to the ipmitool folks, who's code made it easier for me to
 * figure out the OpenIPMI interface more quickly.
 */

#define IPMI_SYSTEM_INTERFACE_ADDR_TYPE 0x0c
#define IPMI_IPMB_ADDR_TYPE             0x01

struct ipmi_system_interface_addr
{
  int addr_type;
  short channel;
  unsigned char lun;
};

struct ipmi_ipmb_addr
{
  int addr_type;
  short channel;
  unsigned char slave_addr;
  unsigned char lun;
};

struct ipmi_msg
{
  unsigned char netfn;
  unsigned char cmd;
  unsigned short data_len;
  unsigned char  *data;
};

struct ipmi_req
{
  unsigned char      *addr;
  unsigned int addr_len;
  long msgid;
  struct ipmi_msg msg;
};

struct ipmi_recv
{
  int recv_type;
  unsigned char      *addr;
  unsigned int addr_len;
  long msgid;
  struct ipmi_msg msg;
};

#define IPMI_IOC_MAGIC             'i'
#define IPMICTL_RECEIVE_MSG_TRUNC  _IOWR (IPMI_IOC_MAGIC, 11, struct ipmi_recv)
#define IPMICTL_RECEIVE_MSG        _IOWR (IPMI_IOC_MAGIC, 12, struct ipmi_recv)
#if defined(__FreeBSD__)
#define IPMICTL_SEND_COMMAND       _IOW (IPMI_IOC_MAGIC,  13, struct ipmi_req)
#define IPMICTL_SET_MY_ADDRESS_CMD _IOW (IPMI_IOC_MAGIC,  17, unsigned int)
#else
#define IPMICTL_SEND_COMMAND       _IOR (IPMI_IOC_MAGIC,  13, struct ipmi_req)
#define IPMICTL_SET_MY_ADDRESS_CMD _IOR (IPMI_IOC_MAGIC,  17, unsigned int)
#endif
#define IPMICTL_GET_MY_ADDRESS_CMD _IOR (IPMI_IOC_MAGIC,  18, unsigned int)
#endif /* !HAVE_LINUX_IPMI_H && !HAVE_SYS_IPMI_H */

static char * ipmi_openipmi_ctx_errmsg[] =
  {
    "success",
    "openipmi context null",
    "openipmi context invalid",
    "invalid parameter",
    "permission denied",
    "device not found",
    "io not initialized",
    "out of memory",
    "driver timeout",
    "internal IPMI error",
    "internal system error",
    "internal error",
    "errnum out of range",
    NULL,
  };

#define IPMI_OPENIPMI_CTX_MAGIC 0xd00fd00f

#define IPMI_OPENIPMI_FLAGS_MASK IPMI_OPENIPMI_FLAGS_DEFAULT

struct ipmi_openipmi_ctx {
  uint32_t magic;
  int errnum;
  unsigned int flags;
  char *driver_device;
  int device_fd;
  int io_init;
};

static void
_set_openipmi_ctx_errnum_by_errno (ipmi_openipmi_ctx_t ctx, int _errno)
{
  if (!ctx || ctx->magic != IPMI_OPENIPMI_CTX_MAGIC)
    return;

  if (_errno == 0)
    ctx->errnum = IPMI_OPENIPMI_ERR_SUCCESS;
  else if (_errno == EPERM)
    ctx->errnum = IPMI_OPENIPMI_ERR_PERMISSION;
  else if (_errno == EACCES)
    ctx->errnum = IPMI_OPENIPMI_ERR_PERMISSION;
  else if (_errno == ENOENT)
    ctx->errnum = IPMI_OPENIPMI_ERR_DEVICE_NOT_FOUND;
  else if (_errno == ENOTDIR)
    ctx->errnum = IPMI_OPENIPMI_ERR_DEVICE_NOT_FOUND;
  else if (_errno == ENAMETOOLONG)
    ctx->errnum = IPMI_OPENIPMI_ERR_DEVICE_NOT_FOUND;
  else if (_errno == ENOMEM)
    ctx->errnum = IPMI_OPENIPMI_ERR_OUT_OF_MEMORY;
  else if (_errno == EINVAL)
    ctx->errnum = IPMI_OPENIPMI_ERR_INTERNAL_ERROR;
  else if (_errno == ETIMEDOUT)
    ctx->errnum = IPMI_OPENIPMI_ERR_DRIVER_TIMEOUT;
  else
    ctx->errnum = IPMI_OPENIPMI_ERR_SYSTEM_ERROR;
}

ipmi_openipmi_ctx_t
ipmi_openipmi_ctx_create (void)
{
  ipmi_openipmi_ctx_t ctx = NULL;

  if (!(ctx = (ipmi_openipmi_ctx_t)malloc (sizeof (struct ipmi_openipmi_ctx))))
    {
      ERRNO_TRACE (errno);
      return (NULL);
    }
  memset (ctx, '\0', sizeof (struct ipmi_openipmi_ctx));

  ctx->magic = IPMI_OPENIPMI_CTX_MAGIC;
  ctx->flags = IPMI_OPENIPMI_FLAGS_DEFAULT;
  ctx->driver_device = NULL;
  ctx->device_fd = -1;
  ctx->io_init = 0;

  ctx->errnum = IPMI_OPENIPMI_ERR_SUCCESS;
  return (ctx);
}

void
ipmi_openipmi_ctx_destroy (ipmi_openipmi_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_OPENIPMI_CTX_MAGIC)
    return;

  ctx->magic = ~IPMI_OPENIPMI_CTX_MAGIC;
  ctx->errnum = IPMI_OPENIPMI_ERR_SUCCESS;
  free (ctx->driver_device);
  /* ignore potential error, destroy path */
  close (ctx->device_fd);
  free (ctx);
}

int
ipmi_openipmi_ctx_errnum (ipmi_openipmi_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_OPENIPMI_ERR_NULL);
  else if (ctx->magic != IPMI_OPENIPMI_CTX_MAGIC)
    return (IPMI_OPENIPMI_ERR_INVALID);
  else
    return (ctx->errnum);
}

char *
ipmi_openipmi_ctx_strerror (int errnum)
{
  if (errnum >= IPMI_OPENIPMI_ERR_SUCCESS && errnum <= IPMI_OPENIPMI_ERR_ERRNUMRANGE)
    return (ipmi_openipmi_ctx_errmsg[errnum]);
  else
    return (ipmi_openipmi_ctx_errmsg[IPMI_OPENIPMI_ERR_ERRNUMRANGE]);
}

char *
ipmi_openipmi_ctx_errormsg (ipmi_openipmi_ctx_t ctx)
{
  return (ipmi_openipmi_ctx_strerror (ipmi_openipmi_ctx_errnum (ctx)));
}

int
ipmi_openipmi_ctx_get_driver_device (ipmi_openipmi_ctx_t ctx, char **driver_device)
{
  if (!ctx || ctx->magic != IPMI_OPENIPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_openipmi_ctx_errormsg (ctx), ipmi_openipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!driver_device)
    {
      OPENIPMI_SET_ERRNUM (ctx, IPMI_OPENIPMI_ERR_PARAMETERS);
      return (-1);
    }

  *driver_device = ctx->driver_device;
  ctx->errnum = IPMI_OPENIPMI_ERR_SUCCESS;
  return (0);
}

int
ipmi_openipmi_ctx_get_flags (ipmi_openipmi_ctx_t ctx, unsigned int *flags)
{
  if (!ctx || ctx->magic != IPMI_OPENIPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_openipmi_ctx_errormsg (ctx), ipmi_openipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!flags)
    {
      OPENIPMI_SET_ERRNUM (ctx, IPMI_OPENIPMI_ERR_PARAMETERS);
      return (-1);
    }

  *flags = ctx->flags;
  ctx->errnum = IPMI_OPENIPMI_ERR_SUCCESS;
  return (0);
}

int
ipmi_openipmi_ctx_set_driver_device (ipmi_openipmi_ctx_t ctx, const char *driver_device)
{
  if (!ctx || ctx->magic != IPMI_OPENIPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_openipmi_ctx_errormsg (ctx), ipmi_openipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!driver_device)
    {
      OPENIPMI_SET_ERRNUM (ctx, IPMI_OPENIPMI_ERR_PARAMETERS);
      return (-1);
    }

  free (ctx->driver_device);
  ctx->driver_device = NULL;

  if (!(ctx->driver_device = strdup (driver_device)))
    {
      OPENIPMI_SET_ERRNUM (ctx, IPMI_OPENIPMI_ERR_OUT_OF_MEMORY);
      return (-1);
    }

  ctx->errnum = IPMI_OPENIPMI_ERR_SUCCESS;
  return (0);
}

int
ipmi_openipmi_ctx_set_flags (ipmi_openipmi_ctx_t ctx, unsigned int flags)
{
  if (!ctx || ctx->magic != IPMI_OPENIPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_openipmi_ctx_errormsg (ctx), ipmi_openipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (flags & ~IPMI_OPENIPMI_FLAGS_MASK)
    {
      OPENIPMI_SET_ERRNUM (ctx, IPMI_OPENIPMI_ERR_PARAMETERS);
      return (-1);
    }

  ctx->flags = flags;
  ctx->errnum = IPMI_OPENIPMI_ERR_SUCCESS;
  return (0);
}

int
ipmi_openipmi_ctx_io_init (ipmi_openipmi_ctx_t ctx)
{
  unsigned int addr = IPMI_SLAVE_ADDRESS_BMC;
  char *driver_device;

  if (!ctx || ctx->magic != IPMI_OPENIPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_openipmi_ctx_errormsg (ctx), ipmi_openipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->io_init)
    goto out;

  if (ctx->driver_device)
    driver_device = ctx->driver_device;
  else
    driver_device = IPMI_OPENIPMI_DRIVER_DEVICE_DEFAULT;

  if ((ctx->device_fd = open (driver_device,
                              O_RDWR)) < 0)
    {
      OPENIPMI_ERRNO_TO_OPENIPMI_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (ioctl (ctx->device_fd,
             IPMICTL_SET_MY_ADDRESS_CMD,
             &addr) < 0)
    {
      OPENIPMI_ERRNO_TO_OPENIPMI_ERRNUM (ctx, errno);
      goto cleanup;
    }

  ctx->io_init = 1;
 out:
  ctx->errnum = IPMI_OPENIPMI_ERR_SUCCESS;
  return (0);

 cleanup:
  /* ignore potential error, error path */
  close (ctx->device_fd);
  ctx->device_fd = -1;
  return (-1);
}

static int
_openipmi_write (ipmi_openipmi_ctx_t ctx,
                 uint8_t channel_number,
                 uint8_t rs_addr,
                 uint8_t lun,
                 uint8_t net_fn,
                 fiid_obj_t obj_cmd_rq,
                 unsigned int is_ipmb)
{
  uint8_t rq_buf_temp[IPMI_OPENIPMI_BUFLEN];
  uint8_t rq_buf[IPMI_OPENIPMI_BUFLEN];
  uint8_t rq_cmd;
  unsigned int rq_buf_len;
  int len;
  struct ipmi_system_interface_addr system_interface_addr;
  struct ipmi_ipmb_addr ipmb_addr;
  struct ipmi_req rq_packet;

  assert (ctx);
  assert (ctx->magic == IPMI_OPENIPMI_CTX_MAGIC);
  assert (IPMI_CHANNEL_NUMBER_VALID (channel_number));
  assert (IPMI_BMC_LUN_VALID (lun));
  assert (IPMI_NET_FN_RQ_VALID (net_fn));
  assert (fiid_obj_valid (obj_cmd_rq));
  assert (fiid_obj_packet_valid (obj_cmd_rq) == 1);

  /* Due to API differences, we need to extract the cmd out of the
   * request.
   */
  memset (rq_buf_temp, '\0', IPMI_OPENIPMI_BUFLEN);

  if ((len = fiid_obj_get_all (obj_cmd_rq,
                               rq_buf_temp,
                               IPMI_OPENIPMI_BUFLEN)) <= 0)
    {
      OPENIPMI_SET_ERRNUM (ctx, IPMI_OPENIPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }

  rq_cmd = rq_buf_temp[0];
  if (len > 1)
    {
      /* -1 b/c of cmd */
      memcpy (rq_buf, &rq_buf_temp[1], len - 1);
      rq_buf_len = len - 1;
    }
  else
    rq_buf_len = 0;

  if (!is_ipmb)
    {
      system_interface_addr.addr_type = IPMI_SYSTEM_INTERFACE_ADDR_TYPE; /* openipmi macro */
      system_interface_addr.channel = IPMI_CHANNEL_NUMBER_SYSTEM_INTERFACE; /* freeipmi macro */
      system_interface_addr.lun = lun;

      rq_packet.addr = (unsigned char *)&system_interface_addr;
      rq_packet.addr_len = sizeof (struct ipmi_system_interface_addr);
    }
  else
    {
      ipmb_addr.addr_type = IPMI_IPMB_ADDR_TYPE; /* openipmi macro */
      ipmb_addr.channel = channel_number;
      ipmb_addr.slave_addr = rs_addr;
      ipmb_addr.lun = lun;

      rq_packet.addr = (unsigned char *)&ipmb_addr;
      rq_packet.addr_len = sizeof (struct ipmi_ipmb_addr);
    }

  rq_packet.msgid = 0;
  rq_packet.msg.netfn = net_fn;
  rq_packet.msg.cmd = rq_cmd;
  rq_packet.msg.data_len = rq_buf_len;
  rq_packet.msg.data = rq_buf;

  if (ioctl (ctx->device_fd,
             IPMICTL_SEND_COMMAND,
             &rq_packet) < 0)
    {
      OPENIPMI_ERRNO_TO_OPENIPMI_ERRNUM (ctx, errno);
      return (-1);
    }

  return (0);
}

static int
_openipmi_read (ipmi_openipmi_ctx_t ctx,
                fiid_obj_t obj_cmd_rs)
{
  uint8_t rs_buf_temp[IPMI_OPENIPMI_BUFLEN];
  uint8_t rs_buf[IPMI_OPENIPMI_BUFLEN];
  struct ipmi_system_interface_addr rs_addr;
  struct ipmi_recv rs_packet;
  fd_set read_fds;
  struct timeval tv, tv_orig, start, end, delta;
  int n;

  assert (ctx);
  assert (ctx->magic == IPMI_OPENIPMI_CTX_MAGIC);
  assert (fiid_obj_valid (obj_cmd_rs));

  rs_packet.addr = (unsigned char *)&rs_addr;
  rs_packet.addr_len = sizeof (struct ipmi_system_interface_addr);
  rs_packet.msg.data = rs_buf_temp;
  rs_packet.msg.data_len = IPMI_OPENIPMI_BUFLEN;

  FD_ZERO (&read_fds);
  FD_SET (ctx->device_fd, &read_fds);

  tv.tv_sec = IPMI_OPENIPMI_TIMEOUT;
  tv.tv_usec = 0;

  tv_orig.tv_sec = tv.tv_sec;
  tv_orig.tv_usec = tv.tv_usec;

  if (gettimeofday (&start, NULL) < 0)
    {
      OPENIPMI_ERRNO_TO_OPENIPMI_ERRNUM (ctx, errno);
      return (-1);
    }

  do {
    if ((n = select (ctx->device_fd + 1,
		     &read_fds,
		     NULL,
		     NULL,
		     &tv)) < 0)
      {
	if (errno != EINTR)
	  {
	    OPENIPMI_ERRNO_TO_OPENIPMI_ERRNUM (ctx, errno);
	    return (-1);
	  }

	if (gettimeofday (&end, NULL) < 0)
	  {
	    OPENIPMI_ERRNO_TO_OPENIPMI_ERRNUM (ctx, errno);
	    return (-1);
	  }
	
	/* delta = end - start */
	timersub (&end, &start, &delta);
	/* tv = tv_orig - delta */
	timersub (&tv_orig, &delta, &tv);
      }
  } while (n < 0);

  if (!n)
    {
      /* Could be due to a different error, but we assume a timeout */
      OPENIPMI_SET_ERRNUM (ctx, IPMI_OPENIPMI_ERR_DRIVER_TIMEOUT);
      return (-1);
    }

  if (ioctl (ctx->device_fd,
             IPMICTL_RECEIVE_MSG_TRUNC,
             &rs_packet) < 0)
    {
      OPENIPMI_ERRNO_TO_OPENIPMI_ERRNUM (ctx, errno);
      return (-1);
    }

  /* achu: atleast the completion code should be returned */
  if (!rs_packet.msg.data_len)
    {
      OPENIPMI_SET_ERRNUM (ctx, IPMI_OPENIPMI_ERR_SYSTEM_ERROR);
      return (-1);
    }

  rs_buf[0] = rs_packet.msg.cmd;
  /* -1 b/c of cmd */
  if (rs_packet.msg.data_len >= (IPMI_OPENIPMI_BUFLEN - 1))
    rs_packet.msg.data_len = IPMI_OPENIPMI_BUFLEN - 1;
  memcpy (rs_buf + 1, rs_buf_temp, rs_packet.msg.data_len);

  if (fiid_obj_set_all (obj_cmd_rs,
                        rs_buf,
                        rs_packet.msg.data_len + 1) < 0)
    {
      OPENIPMI_SET_ERRNUM (ctx, IPMI_OPENIPMI_ERR_INTERNAL_ERROR);
      return (-1);
    }

  return (0);
}

int
ipmi_openipmi_cmd (ipmi_openipmi_ctx_t ctx,
                   uint8_t lun,
                   uint8_t net_fn,
                   fiid_obj_t obj_cmd_rq,
                   fiid_obj_t obj_cmd_rs)
{
  if (!ctx || ctx->magic != IPMI_OPENIPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_openipmi_ctx_errormsg (ctx), ipmi_openipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_BMC_LUN_VALID (lun)
      || !IPMI_NET_FN_RQ_VALID (net_fn)
      || !fiid_obj_valid (obj_cmd_rq)
      || !fiid_obj_valid (obj_cmd_rs)
      || fiid_obj_packet_valid (obj_cmd_rq) <= 0)
    {
      OPENIPMI_SET_ERRNUM (ctx, IPMI_OPENIPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (!ctx->io_init)
    {
      OPENIPMI_SET_ERRNUM (ctx, IPMI_OPENIPMI_ERR_IO_NOT_INITIALIZED);
      return (-1);
    }

  if (_openipmi_write (ctx,
                       0,
                       0,
                       lun,
                       net_fn,
                       obj_cmd_rq,
                       0) < 0)
    return (-1);

  if (_openipmi_read (ctx,
                      obj_cmd_rs) < 0)
    return (-1);

  return (0);
}

int
ipmi_openipmi_cmd_ipmb (ipmi_openipmi_ctx_t ctx,
                        uint8_t channel_number,
                        uint8_t rs_addr,
                        uint8_t lun,
                        uint8_t net_fn,
                        fiid_obj_t obj_cmd_rq,
                        fiid_obj_t obj_cmd_rs)
{
  if (!ctx || ctx->magic != IPMI_OPENIPMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_openipmi_ctx_errormsg (ctx), ipmi_openipmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_BMC_LUN_VALID (lun)
      || !IPMI_NET_FN_RQ_VALID (net_fn)
      || !fiid_obj_valid (obj_cmd_rq)
      || !fiid_obj_valid (obj_cmd_rs)
      || fiid_obj_packet_valid (obj_cmd_rq) <= 0)
    {
      OPENIPMI_SET_ERRNUM (ctx, IPMI_OPENIPMI_ERR_PARAMETERS);
      return (-1);
    }

  if (!ctx->io_init)
    {
      OPENIPMI_SET_ERRNUM (ctx, IPMI_OPENIPMI_ERR_IO_NOT_INITIALIZED);
      return (-1);
    }

  if (_openipmi_write (ctx,
                       channel_number,
                       rs_addr,
                       lun,
                       net_fn,
                       obj_cmd_rq,
                       1) < 0)
    return (-1);

  if (_openipmi_read (ctx,
                      obj_cmd_rs) < 0)
    return (-1);

  return (0);
}
