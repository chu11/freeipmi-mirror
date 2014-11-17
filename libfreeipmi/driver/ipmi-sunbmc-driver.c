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
#include <stdint.h>
#ifdef STDC_HEADERS
#include <string.h>
#include <stddef.h>
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
#if HAVE_STROPTS_H
#include <stropts.h>            /* putmsg, getmsg on Solaris */
#endif /* HAVE_STROPTS_H */
#if HAVE_SYS_STROPTS_H
#include <sys/stropts.h>        /* for I_STR */
#endif  /* !HAVE_SYS_STROPTS_H */
#if HAVE_SYS_INT_TYPES_H
#include <sys/int_types.h>      /* for uint8_t on Solaris?? */
#endif /* !HAVE_SYS_INT_TYPES_H */
#if HAVE_BMC_INTF_H
#include <bmc_intf.h>
#else  /* !HAVE_BMC_INTF_H */
#include "freeipmi_bmc_intf.h"  /* from ipmitool - is BSD license, not CDDL */
#endif /* !HAVE_BMC_INTF_H */
#include <sys/select.h>
#include <sys/ioctl.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/driver/ipmi-sunbmc-driver.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"

#include "ipmi-driver-trace.h"

#include "libcommon/ipmi-fiid-util.h"

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
    "device not supported",
    "io not initialized",
    "out of memory",
    "driver timeout",
    "internal IPMI error",
    "internal system error",
    "internal error",
    "errnum out of range",
    NULL,
  };

#define IPMI_SUNBMC_CTX_MAGIC 0xd0bbd0bb

#define IPMI_SUNBMC_FLAGS_MASK IPMI_SUNBMC_FLAGS_DEFAULT

struct ipmi_sunbmc_ctx {
  uint32_t magic;
  int errnum;
  unsigned int flags;
  char *driver_device;
  int device_fd;
  int io_init;
  int putmsg_intf;
  uint8_t putmsg_intf_msg_id;
};

static void
_set_sunbmc_ctx_errnum_by_errno (ipmi_sunbmc_ctx_t ctx, int _errno)
{
  if (!ctx || ctx->magic != IPMI_SUNBMC_CTX_MAGIC)
    return;

  if (_errno == 0)
    ctx->errnum = IPMI_SUNBMC_ERR_SUCCESS;
  else if (_errno == EPERM)
    ctx->errnum = IPMI_SUNBMC_ERR_PERMISSION;
  else if (_errno == EACCES)
    ctx->errnum = IPMI_SUNBMC_ERR_PERMISSION;
  else if (_errno == ENOENT)
    ctx->errnum = IPMI_SUNBMC_ERR_DEVICE_NOT_FOUND;
  else if (_errno == ENOTDIR)
    ctx->errnum = IPMI_SUNBMC_ERR_DEVICE_NOT_FOUND;
  else if (_errno == ENAMETOOLONG)
    ctx->errnum = IPMI_SUNBMC_ERR_DEVICE_NOT_FOUND;
  else if (_errno == ENOMEM)
    ctx->errnum = IPMI_SUNBMC_ERR_OUT_OF_MEMORY;
  else if (_errno == ETIMEDOUT)
    ctx->errnum = IPMI_SUNBMC_ERR_DRIVER_TIMEOUT;
  else
    ctx->errnum = IPMI_SUNBMC_ERR_SYSTEM_ERROR;
}

ipmi_sunbmc_ctx_t
ipmi_sunbmc_ctx_create (void)
{
  ipmi_sunbmc_ctx_t ctx = NULL;

  if (!(ctx = (ipmi_sunbmc_ctx_t)malloc (sizeof (struct ipmi_sunbmc_ctx))))
    {
      ERRNO_TRACE (errno);
      return (NULL);
    }
  memset (ctx, '\0', sizeof (struct ipmi_sunbmc_ctx));

  ctx->magic = IPMI_SUNBMC_CTX_MAGIC;
  ctx->flags = IPMI_SUNBMC_FLAGS_DEFAULT;
  ctx->driver_device = NULL;
  ctx->device_fd = -1;
  ctx->io_init = 0;
  ctx->putmsg_intf = 0;
  ctx->putmsg_intf_msg_id = 0;  /* XXX: randomize? */

  ctx->errnum = IPMI_SUNBMC_ERR_SUCCESS;
  return (ctx);
}

void
ipmi_sunbmc_ctx_destroy (ipmi_sunbmc_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_SUNBMC_CTX_MAGIC)
    return;

  ctx->magic = ~IPMI_SUNBMC_CTX_MAGIC;
  ctx->errnum = IPMI_SUNBMC_ERR_SUCCESS;
  free (ctx->driver_device);
  /* ignore potential error, destroy path */
  close (ctx->device_fd);
  free (ctx);
}

int
ipmi_sunbmc_ctx_errnum (ipmi_sunbmc_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_SUNBMC_ERR_NULL);
  else if (ctx->magic != IPMI_SUNBMC_CTX_MAGIC)
    return (IPMI_SUNBMC_ERR_INVALID);
  else
    return (ctx->errnum);
}

char *
ipmi_sunbmc_ctx_strerror (int errnum)
{
  if (errnum >= IPMI_SUNBMC_ERR_SUCCESS && errnum <= IPMI_SUNBMC_ERR_ERRNUMRANGE)
    return (ipmi_sunbmc_ctx_errmsg[errnum]);
  else
    return (ipmi_sunbmc_ctx_errmsg[IPMI_SUNBMC_ERR_ERRNUMRANGE]);
}

char *
ipmi_sunbmc_ctx_errormsg (ipmi_sunbmc_ctx_t ctx)
{
  return (ipmi_sunbmc_ctx_strerror (ipmi_sunbmc_ctx_errnum (ctx)));
}

int
ipmi_sunbmc_ctx_get_driver_device (ipmi_sunbmc_ctx_t ctx, char **driver_device)
{
  if (!ctx || ctx->magic != IPMI_SUNBMC_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sunbmc_ctx_errormsg (ctx), ipmi_sunbmc_ctx_errnum (ctx));
      return (-1);
    }

  if (!driver_device)
    {
      SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_PARAMETERS);
      return (-1);
    }

  *driver_device = ctx->driver_device;
  ctx->errnum = IPMI_SUNBMC_ERR_SUCCESS;
  return (0);
}

int
ipmi_sunbmc_ctx_get_flags (ipmi_sunbmc_ctx_t ctx, unsigned int *flags)
{
  if (!ctx || ctx->magic != IPMI_SUNBMC_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sunbmc_ctx_errormsg (ctx), ipmi_sunbmc_ctx_errnum (ctx));
      return (-1);
    }

  if (!flags)
    {
      SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_PARAMETERS);
      return (-1);
    }

  *flags = ctx->flags;
  ctx->errnum = IPMI_SUNBMC_ERR_SUCCESS;
  return (0);
}

int
ipmi_sunbmc_ctx_set_driver_device (ipmi_sunbmc_ctx_t ctx, const char *driver_device)
{
  if (!ctx || ctx->magic != IPMI_SUNBMC_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sunbmc_ctx_errormsg (ctx), ipmi_sunbmc_ctx_errnum (ctx));
      return (-1);
    }

  if (!driver_device)
    {
      SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_PARAMETERS);
      return (-1);
    }

  free (ctx->driver_device);
  ctx->driver_device = NULL;

  if (!(ctx->driver_device = strdup (driver_device)))
    {
      SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_OUT_OF_MEMORY);
      return (-1);
    }

  ctx->errnum = IPMI_SUNBMC_ERR_SUCCESS;
  return (0);
}

int
ipmi_sunbmc_ctx_set_flags (ipmi_sunbmc_ctx_t ctx, unsigned int flags)
{
  if (!ctx || ctx->magic != IPMI_SUNBMC_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sunbmc_ctx_errormsg (ctx), ipmi_sunbmc_ctx_errnum (ctx));
      return (-1);
    }

  if (flags & ~IPMI_SUNBMC_FLAGS_MASK)
    {
      SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_PARAMETERS);
      return (-1);
    }

  ctx->flags = flags;
  ctx->errnum = IPMI_SUNBMC_ERR_SUCCESS;
  return (0);
}

int
ipmi_sunbmc_ctx_io_init (ipmi_sunbmc_ctx_t ctx)
{
#if defined(HAVE_SYS_STROPTS_H) && defined(IOCTL_IPMI_INTERFACE_METHOD)
  struct strioctl istr;
  uint8_t method;
#endif /* !(defined(HAVE_SYS_STROPTS_H) && defined(IOCTL_IPMI_INTERFACE_METHOD)) */
  char *driver_device;

  if (!ctx || ctx->magic != IPMI_SUNBMC_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sunbmc_ctx_errormsg (ctx), ipmi_sunbmc_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->io_init)
    goto out;

  if (ctx->driver_device)
    driver_device = ctx->driver_device;
  else
    driver_device = IPMI_SUNBMC_DRIVER_DEVICE_DEFAULT;

  if ((ctx->device_fd = open (driver_device,
                              O_RDWR)) < 0)
    {
      SUNBMC_ERRNO_TO_SUNBMC_ERRNUM (ctx, errno);
      goto cleanup;
    }

#if defined(HAVE_SYS_STROPTS_H)

#ifdef IOCTL_IPMI_INTERFACE_METHOD

#ifdef BMC_PUTMSG_METHOD
  method = BMC_PUTMSG_METHOD;
#else
  method = 1;
#endif

  istr.ic_cmd = IOCTL_IPMI_INTERFACE_METHOD;
  istr.ic_timout = 0;           /* spelled 'timout', not a typo */
  istr.ic_len = 1;
  istr.ic_dp = (char *)&method;

  if (ioctl (ctx->device_fd, I_STR, &istr) < 0)
    {
      if (errno != EINVAL)
        {
          SUNBMC_ERRNO_TO_SUNBMC_ERRNUM (ctx, errno);
          goto cleanup;
        }
      /* achu: assume ioctl method */
      ctx->putmsg_intf = 0;
      goto out_io_init;
    }
  ctx->putmsg_intf = method;
#else /* !IOCTL_IPMI_INTERFACE_METHOD */
  /* achu: assume ioctl method */
  ctx->putmsg_intf = 0;
#endif /* !IOCTL_IPMI_INTERFACE_METHOD */

#else /* !defined(HAVE_SYS_STROPTS_H) */

  /* otherwise, device is not supported */
  SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_DEVICE_NOT_SUPPORTED);
  return (-1);

#endif /* !defined(HAVE_SYS_STROPTS_H) */

 out_io_init:
  ctx->io_init = 1;
 out:
  ctx->errnum = IPMI_SUNBMC_ERR_SUCCESS;
  return (0);

 cleanup:
  /* ignore potential error, error path */
  close (ctx->device_fd);
  ctx->device_fd = -1;
  return (-1);
}

static int
_sunbmc_write (ipmi_sunbmc_ctx_t ctx,
               uint8_t lun,
               uint8_t net_fn,
               fiid_obj_t obj_cmd_rq)
{
#if defined(HAVE_SYS_STROPTS_H)
  struct strbuf sbuf;
  bmc_msg_t *msg = NULL;
  bmc_req_t *req = NULL;
  unsigned int msg_len;
  uint8_t rq_buf_temp[IPMI_SUNBMC_BUFLEN];
  uint8_t rq_buf[IPMI_SUNBMC_BUFLEN];
  uint8_t rq_cmd;
  unsigned int rq_buf_len;
  int len;
#endif /* !defined(HAVE_SYS_STROPTS_H) */
  int rv = -1;

  assert (ctx);
  assert (ctx->magic == IPMI_SUNBMC_CTX_MAGIC);
  assert (IPMI_BMC_LUN_VALID (lun));
  assert (IPMI_NET_FN_RQ_VALID (net_fn));
  assert (fiid_obj_valid (obj_cmd_rq));
  assert (fiid_obj_packet_valid (obj_cmd_rq) == 1);
  assert (ctx->io_init);
  assert (ctx->putmsg_intf);

#if defined(HAVE_SYS_STROPTS_H)
  memset (&sbuf, '\0', sizeof (struct strbuf));

  /* Due to API differences, we need to extract the cmd out of the
   * request.
   */
  memset (rq_buf_temp, '\0', IPMI_SUNBMC_BUFLEN);

  if ((len = fiid_obj_get_all (obj_cmd_rq,
                               rq_buf_temp,
                               IPMI_SUNBMC_BUFLEN)) <= 0)
    {
      SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  rq_cmd = rq_buf_temp[0];
  if (len > 1)
    {
      memcpy (rq_buf, &rq_buf_temp[1], len - 1);
      rq_buf_len = len - 1;
    }
  else
    rq_buf_len = 0;

  /* achu: see header for for how this is calculated */
  msg_len = offsetof (bmc_msg_t, msg);
  msg_len += sizeof (bmc_req_t);
  msg_len += (rq_buf_len > SEND_MAX_PAYLOAD_SIZE) ? (rq_buf_len - SEND_MAX_PAYLOAD_SIZE) : 0;

  if (!(msg = (bmc_msg_t *)malloc (msg_len)))
    {
      SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_OUT_OF_MEMORY);
      goto cleanup;
    }

  msg->m_type = BMC_MSG_REQUEST;
  msg->m_id = ctx->putmsg_intf_msg_id;

  req = (bmc_req_t *)&(msg->msg[0]);
  req->fn = net_fn;
  req->lun = lun;
  req->cmd = rq_cmd;
  req->datalength = rq_buf_len;
  memcpy (req->data, rq_buf, rq_buf_len);

  sbuf.len = msg_len;
  sbuf.buf = (char *)msg;

  if (putmsg (ctx->device_fd, NULL, &sbuf, 0) < 0)
    {
      SUNBMC_ERRNO_TO_SUNBMC_ERRNUM (ctx, errno);
      goto cleanup;
    }

#else /* !defined(HAVE_SYS_STROPTS_H) */
  /* otherwise, device is not supported */
  SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_DEVICE_NOT_SUPPORTED);
  goto cleanup;
#endif /* !(defined(HAVE_BMC_INTF_H) && defined(HAVE_SYS_STROPTS_H)) */

  rv = 0;
 cleanup:
#if defined(HAVE_SYS_STROPTS_H)
  free (msg);
#endif /* !defined(HAVE_SYS_STROPTS_H) */
  return (rv);
}

static int
_sunbmc_read (ipmi_sunbmc_ctx_t ctx,
              fiid_obj_t obj_cmd_rs)
{
#if defined(HAVE_SYS_STROPTS_H)
  struct strbuf sbuf;
  bmc_msg_t *msg = NULL;
  bmc_rsp_t *rsp = NULL;
  int flags = 0;
  uint8_t rs_buf_temp[IPMI_SUNBMC_BUFLEN];
  uint8_t rs_buf[IPMI_SUNBMC_BUFLEN];
  unsigned int rs_buf_len = 0;
#endif /* !defined(HAVE_SYS_STROPTS_H) */
  fd_set read_fds;
  struct timeval tv, tv_orig, start, end, delta;
  int n;

  assert (ctx);
  assert (ctx->magic == IPMI_SUNBMC_CTX_MAGIC);
  assert (ctx->io_init);
  assert (ctx->putmsg_intf);
  assert (fiid_obj_valid (obj_cmd_rs));

#if defined(HAVE_SYS_STROPTS_H)
  memset (&sbuf, '\0', sizeof (struct strbuf));

  sbuf.maxlen = IPMI_SUNBMC_BUFLEN;
  sbuf.buf = (char *)rs_buf_temp;
#else /* !defined(HAVE_SYS_STROPTS_H) */
  /* otherwise, device is not supported */
  SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_DEVICE_NOT_SUPPORTED);
  return (-1);
#endif /* !defined(HAVE_SYS_STROPTS_H) */

  FD_ZERO (&read_fds);
  FD_SET (ctx->device_fd, &read_fds);

  tv.tv_sec = IPMI_SUNBMC_TIMEOUT;
  tv.tv_usec = 0;

  tv_orig.tv_sec = tv.tv_sec;
  tv_orig.tv_usec = tv.tv_usec;

  if (gettimeofday (&start, NULL) < 0)
    {
      SUNBMC_ERRNO_TO_SUNBMC_ERRNUM (ctx, errno);
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
	    SUNBMC_ERRNO_TO_SUNBMC_ERRNUM (ctx, errno);
	    return (-1);
	  }

	if (gettimeofday (&end, NULL) < 0)
	  {
	    SUNBMC_ERRNO_TO_SUNBMC_ERRNUM (ctx, errno);
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
      SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_DRIVER_TIMEOUT);
      return (-1);
    }

#if defined(HAVE_SYS_STROPTS_H)
  if (getmsg (ctx->device_fd, NULL, &sbuf, &flags) < 0)
    {
      SUNBMC_ERRNO_TO_SUNBMC_ERRNUM (ctx, errno);
      return (-1);
    }
  msg = (bmc_msg_t *)&(sbuf.buf[0]);

  if (msg->m_type == BMC_MSG_ERROR)
    {
      errno = msg->msg[0];
      SUNBMC_ERRNO_TO_SUNBMC_ERRNUM (ctx, errno);
      return (-1);
    }
  if (msg->m_type != BMC_MSG_RESPONSE)
    {
      SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_SYSTEM_ERROR);
      return (-1);
    }
  if (msg->m_id != ctx->putmsg_intf_msg_id)
    {
      SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_SYSTEM_ERROR);
      return (-1);
    }

  rsp = (bmc_rsp_t *)&(msg->msg[0]);

  /* Due to API differences, we need to put the cmd/ccode back into
   * the buffer.
   */
  rs_buf[0] = rsp->cmd;
  rs_buf_len++;
  rs_buf[1] = rsp->ccode;
  rs_buf_len++;
  memcpy (&(rs_buf[2]), rsp->data, rsp->datalength);
  rs_buf_len += rsp->datalength;

  if (fiid_obj_set_all (obj_cmd_rs,
                        rs_buf,
                        rs_buf_len) < 0)
    {
      SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_INTERNAL_ERROR);
      return (-1);
    }

#else /* !defined(HAVE_SYS_STROPTS_H) */
  /* otherwise, device is not supported */
  SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_DEVICE_NOT_SUPPORTED);
  return (-1);
#endif /* !defined(HAVE_SYS_STROPTS_H) */

  return (0);
}

int
ipmi_sunbmc_cmd (ipmi_sunbmc_ctx_t ctx,
                 uint8_t lun,
                 uint8_t net_fn,
                 fiid_obj_t obj_cmd_rq,
                 fiid_obj_t obj_cmd_rs)
{
  if (!ctx || ctx->magic != IPMI_SUNBMC_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_sunbmc_ctx_errormsg (ctx), ipmi_sunbmc_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_BMC_LUN_VALID (lun)
      || !IPMI_NET_FN_RQ_VALID (net_fn)
      || !fiid_obj_valid (obj_cmd_rq)
      || !fiid_obj_valid (obj_cmd_rs)
      || fiid_obj_packet_valid (obj_cmd_rq) <= 0)
    {
      SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_PARAMETERS);
      return (-1);
    }

  if (!ctx->io_init)
    {
      SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_IO_NOT_INITIALIZED);
      return (-1);
    }

  if (ctx->putmsg_intf)
    {
      if (_sunbmc_write (ctx,
                         lun,
                         net_fn,
                         obj_cmd_rq) < 0)
        return (-1);

      if (_sunbmc_read (ctx,
                        obj_cmd_rs) < 0)
        return (-1);
    }
  else
    {
#if defined(HAVE_SYS_STROPTS_H)
      struct strioctl istr;
      bmc_reqrsp_t reqrsp;
      uint8_t rq_buf_temp[IPMI_SUNBMC_BUFLEN];
      uint8_t rq_buf[IPMI_SUNBMC_BUFLEN];
      uint8_t rq_cmd;
      unsigned int rq_buf_len;
      uint8_t rs_buf[IPMI_SUNBMC_BUFLEN];
      int len;

      memset (&istr, '\0', sizeof (struct strioctl));
      memset (&reqrsp, '\0', sizeof (bmc_reqrsp_t));

      /* Due to API differences, we need to extract the cmd out of the
       * request.
       */
      memset (rq_buf_temp, '\0', IPMI_SUNBMC_BUFLEN);

      if ((len = fiid_obj_get_all (obj_cmd_rq,
                                   rq_buf_temp,
                                   IPMI_SUNBMC_BUFLEN)) <= 0)
        {
          SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_INTERNAL_ERROR);
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

      reqrsp.req.fn = net_fn;
      reqrsp.req.lun = lun;
      reqrsp.req.cmd = rq_cmd;

      istr.ic_cmd = IOCTL_IPMI_KCS_ACTION;
      istr.ic_timout = 0;       /* spelled 'timout', not a typo */
      istr.ic_len = sizeof (struct bmc_reqrsp);
      istr.ic_dp = (char *)&reqrsp;

      if (ioctl (ctx->device_fd,
                 I_STR,
                 &istr) < 0)
        {
          SUNBMC_ERRNO_TO_SUNBMC_ERRNUM (ctx, errno);
          return (-1);
        }

      rs_buf[0] = reqrsp.rsp.cmd;
      rs_buf[1] = reqrsp.rsp.ccode;
      /* -2 b/c of cmd and ccode */
#if 0
      /* achu: to remove warnings, IPMI_SUNBMC_BUFLEN > amount uint8_t can hold */
      if (reqrsp.rsp.datalength >= (IPMI_SUNBMC_BUFLEN - 2))
        reqrsp.rsp.datalength = IPMI_SUNBMC_BUFLEN - 2;
#endif
      /* remove header data stuff in front we don't care about */
      if (reqrsp.rsp.datalength > 3)
        reqrsp.rsp.datalength -= 3;
      else
        reqrsp.rsp.datalength = 0;
      memcpy (rs_buf + 2, reqrsp.rsp.data, reqrsp.rsp.datalength);

      if (fiid_obj_set_all (obj_cmd_rs,
                            rs_buf,
                            reqrsp.rsp.datalength + 2) < 0)
        {
          SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_INTERNAL_ERROR);
          return (-1);
        }
#else /* !defined(HAVE_SYS_STROPTS_H) */
      /* otherwise, device is not supported */
      SUNBMC_SET_ERRNUM (ctx, IPMI_SUNBMC_ERR_DEVICE_NOT_SUPPORTED);
      return (-1);
#endif /* !defined(HAVE_SYS_STROPTS_H) */
    }

  return (0);
}
