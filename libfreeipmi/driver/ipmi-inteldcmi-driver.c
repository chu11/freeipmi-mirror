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
/*----------------------------------------------------------------------* 
The BSD License 
Copyright (c) 2002, Intel Corporation
All rights reserved.
Redistribution and use in source and binary forms, with or without 
modification, are permitted provided that the following conditions are met:
  a.. Redistributions of source code must retain the above copyright notice, 
      this list of conditions and the following disclaimer. 
  b.. Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation 
      and/or other materials provided with the distribution. 
  c.. Neither the name of Intel Corporation nor the names of its contributors 
      may be used to endorse or promote products derived from this software 
      without specific prior written permission. 
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR 
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*----------------------------------------------------------------------*/

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

#include "freeipmi/driver/ipmi-inteldcmi-driver.h"
#include "freeipmi/spec/ipmi-channel-spec.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"
#include "freeipmi/spec/ipmi-slave-address-spec.h"

#include "ipmi-driver-trace.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"

#define IPMI_INTELDCMI_BUFLEN       1024

#define IPMI_INTELDCMI_TIMEOUT      (60 * 1000 * 1000)

#define IPMI_INTELDCMI_MIN_RQ_BUFLEN 41

/* XXX need to autoconf for inteldcmi headers*/
#if HAVE_LINUX_IPMI_H
#include <linux/ipmi.h>
#elif HAVE_SYS_IPMI_H
#include <sys/ipmi.h>
#else  /* !HAVE_LINUX_IPMI_H && !HAVE_SYS_IPMI_H */
#endif /* !HAVE_LINUX_IPMI_H && !HAVE_SYS_IPMI_H */

static char * ipmi_inteldcmi_ctx_errmsg[] =
  {
    "success",
    "inteldcmi context null",
    "inteldcmi context invalid",
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

#define IPMI_INTELDCMI_CTX_MAGIC 0xe00fe00f

#define IPMI_INTELDCMI_FLAGS_MASK IPMI_INTELDCMI_FLAGS_DEFAULT

struct ipmi_inteldcmi_ctx {
  uint32_t magic;
  int errnum;
  unsigned int flags;
  char *driver_device;
  int device_fd;
  int io_init;
};

fiid_template_t tmpl_inteldcmi_request =
  {
    { 32, "flags", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 32, "timeout", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, /* in u-sec */
    { 8, "rs_addr", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "rs_lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "data_len", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Max bytes = 256, 256 * 8 = 2048 */
    { 2048, "data",  FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

fiid_template_t tmpl_inteldcmi_response =
  {
    { 8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    /* Max bytes = 256, 256 * 8 = 2048 */
    { 2048, "data",  FIID_FIELD_OPTIONAL | FIID_FIELD_LENGTH_VARIABLE},
    { 0, "", 0}
  };

/*
 * Begin code from Intel Code
 */

struct inteldcmi_smi {
  unsigned int smi_VersionNo;
  unsigned int smi_Reserved1;
  unsigned int smi_Reserved2;
  void *ntstatus;		    /* address of NT status block*/
  void *lpvInBuffer;		    /* address of buffer for input data*/
  unsigned int cbInBuffer;	    /* size of input buffer*/
  void *lpvOutBuffer;		    /* address of output buffer*/
  unsigned int cbOutBuffer;	    /* size of output buffer*/
  unsigned int *lpcbBytesReturned; /* address of actual bytes of output */
  void *lpoOverlapped;		    /* address of overlapped structure - set to NULL for Linux */
};

struct io_status_block {
  unsigned long status;
  unsigned long information;
};

/*
 * Linux drivers expect ioctls defined using macros defined in ioctl.h.
 * So, instead of using the CTL_CODE defined for NT and UW, I define CTL_CODE
 * using these macros. That way imb_if.h, where the ioctls are defined get
 * to use the correct ioctl command we expect.
 * Notes: I am using the generic _IO macro instead of the more specific
 * ones. The macros expect 8bit entities, so I am cleaning what is sent to
 * us from imb_if.h  - Mahendra
 */
#define IPMI_INTELDCMI_CTL_CODE(DeviceType, Function, Method, Access) \
  _IO(DeviceType & 0x00FF, Function & 0x00FF)

/*
 Define the method codes for how buffers are passed for I/O and FS controls
*/
#define IPMI_INTELDCMI_METHOD_BUFFERED                 0
/*
 Define the access check value for any access
 The FILE_READ_ACCESS and FILE_WRITE_ACCESS constants are also defined in
 ntioapi.h as FILE_READ_DATA and FILE_WRITE_DATA. The values for these
 constants *MUST* always be in sync.
*/
#define IPMI_INTELDCMI_FILE_ANY_ACCESS                 0

#define IPMI_INTELDCMI_FILE_DEVICE_IMB                 0x00008010
#define IPMI_INTELDCMI_IOCTL_IMB_BASE                  0x00000880
#define IPMI_INTELDCMI_IOCTL_IMB_SEND_MESSAGE          IPMI_INTELDCMI_CTL_CODE(IPMI_INTELDCMI_FILE_DEVICE_IMB, (IPMI_INTELDCMI_IOCTL_IMB_BASE + 2),  IPMI_INTELDCMI_METHOD_BUFFERED, IPMI_INTELDCMI_FILE_ANY_ACCESS)
#define IPMI_INTELDCMI_IOCTL_IMB_GET_ASYNC_MSG         IPMI_INTELDCMI_CTL_CODE(IPMI_INTELDCMI_FILE_DEVICE_IMB, (IPMI_INTELDCMI_IOCTL_IMB_BASE + 8),  IPMI_INTELDCMI_METHOD_BUFFERED, IPMI_INTELDCMI_FILE_ANY_ACCESS)
#define IPMI_INTELDCMI_IOCTL_IMB_MAP_MEMORY            IPMI_INTELDCMI_CTL_CODE(IPMI_INTELDCMI_FILE_DEVICE_IMB, (IPMI_INTELDCMI_IOCTL_IMB_BASE + 14), IPMI_INTELDCMI_METHOD_BUFFERED, IPMI_INTELDCMI_FILE_ANY_ACCESS)
#define IPMI_INTELDCMI_IOCTL_IMB_UNMAP_MEMORY          IPMI_INTELDCMI_CTL_CODE(IPMI_INTELDCMI_FILE_DEVICE_IMB, (IPMI_INTELDCMI_IOCTL_IMB_BASE + 16), IPMI_INTELDCMI_METHOD_BUFFERED, IPMI_INTELDCMI_FILE_ANY_ACCESS)
#define IPMI_INTELDCMI_IOCTL_IMB_SHUTDOWN_CODE         IPMI_INTELDCMI_CTL_CODE(IPMI_INTELDCMI_FILE_DEVICE_IMB, (IPMI_INTELDCMI_IOCTL_IMB_BASE + 18), IPMI_INTELDCMI_METHOD_BUFFERED, IPMI_INTELDCMI_FILE_ANY_ACCESS)
#define IPMI_INTELDCMI_IOCTL_IMB_REGISTER_ASYNC_OBJ    IPMI_INTELDCMI_CTL_CODE(IPMI_INTELDCMI_FILE_DEVICE_IMB, (IPMI_INTELDCMI_IOCTL_IMB_BASE + 24), IPMI_INTELDCMI_METHOD_BUFFERED, IPMI_INTELDCMI_FILE_ANY_ACCESS)
#define IPMI_INTELDCMI_IOCTL_IMB_DEREGISTER_ASYNC_OBJ  IPMI_INTELDCMI_CTL_CODE(IPMI_INTELDCMI_FILE_DEVICE_IMB, (IPMI_INTELDCMI_IOCTL_IMB_BASE + 26), IPMI_INTELDCMI_METHOD_BUFFERED, IPMI_INTELDCMI_FILE_ANY_ACCESS)
#define IPMI_INTELDCMI_IOCTL_IMB_CHECK_EVENT           IPMI_INTELDCMI_CTL_CODE(IPMI_INTELDCMI_FILE_DEVICE_IMB, (IPMI_INTELDCMI_IOCTL_IMB_BASE + 28), IPMI_INTELDCMI_METHOD_BUFFERED, IPMI_INTELDCMI_FILE_ANY_ACCESS)
#define IPMI_INTELDCMI_IOCTL_IMB_POLL_ASYNC            IPMI_INTELDCMI_CTL_CODE(IPMI_INTELDCMI_FILE_DEVICE_IMB, (IPMI_INTELDCMI_IOCTL_IMB_BASE + 20), IPMI_INTELDCMI_METHOD_BUFFERED, IPMI_INTELDCMI_FILE_ANY_ACCESS)

/*
 * I2C ioctl's return NTStatus codes
 */
#define IPMI_INTELDCMI_STATUS_SUCCESS                   (0x00000000U)
#define IPMI_INTELDCMI_STATUS_UNSUCCESSFUL              (0xC0000001U)
#define IPMI_INTELDCMI_STATUS_DEVICE_BUSY               (0x80000011U)
#define IPMI_INTELDCMI_STATUS_PENDING                   (0x00000103U)
#define IPMI_INTELDCMI_STATUS_INVALID_PARAMETER         (0xC000000DU)
#define IPMI_INTELDCMI_STATUS_INVALID_DEVICE_REQUEST    (0xC0000010U)
#define IPMI_INTELDCMI_STATUS_BUFFER_TOO_SMALL          (0xC0000023U)
#define IPMI_INTELDCMI_STATUS_FILE_CLOSED               (0xC0000128U)
#define IPMI_INTELDCMI_STATUS_INSUFFICIENT_RESOURCES    (0xC000009AU)
#define IPMI_INTELDCMI_STATUS_NO_DATA_DETECTED          (0x80000022U)
#define IPMI_INTELDCMI_STATUS_NO_SUCH_DEVICE            (0xC000000EU)
#define IPMI_INTELDCMI_STATUS_ALLOTTED_EXCEEDED         (0xC000000FU)
#define IPMI_INTELDCMI_STATUS_IO_DEVICE_ERROR           (0xC0000185U)
#define IPMI_INTELDCMI_STATUS_TOO_MANY_OPEN_FILES       (0xC000011FU)
#define IPMI_INTELDCMI_STATUS_ACCESS_DENIED             (0xC0000022U)
#define IPMI_INTELDCMI_STATUS_BUFFER_OVERFLOW           (0x80000005U)
#define IPMI_INTELDCMI_STATUS_CANCELLED                 (0xC0000120U)

/*
 * End code from Intel Code
 */

static void
_set_inteldcmi_ctx_errnum_by_errno (ipmi_inteldcmi_ctx_t ctx, int _errno)
{
  if (!ctx || ctx->magic != IPMI_INTELDCMI_CTX_MAGIC)
    return;

  if (_errno == 0)
    ctx->errnum = IPMI_INTELDCMI_ERR_SUCCESS;
  else if (_errno == EPERM)
    ctx->errnum = IPMI_INTELDCMI_ERR_PERMISSION;
  else if (_errno == EACCES)
    ctx->errnum = IPMI_INTELDCMI_ERR_PERMISSION;
  else if (_errno == ENOENT)
    ctx->errnum = IPMI_INTELDCMI_ERR_DEVICE_NOT_FOUND;
  else if (_errno == ENOTDIR)
    ctx->errnum = IPMI_INTELDCMI_ERR_DEVICE_NOT_FOUND;
  else if (_errno == ENAMETOOLONG)
    ctx->errnum = IPMI_INTELDCMI_ERR_DEVICE_NOT_FOUND;
  else if (_errno == ENOMEM)
    ctx->errnum = IPMI_INTELDCMI_ERR_OUT_OF_MEMORY;
  else if (_errno == EINVAL)
    ctx->errnum = IPMI_INTELDCMI_ERR_INTERNAL_ERROR;
  else if (_errno == ETIMEDOUT)
    ctx->errnum = IPMI_INTELDCMI_ERR_DRIVER_TIMEOUT;
  else
    ctx->errnum = IPMI_INTELDCMI_ERR_SYSTEM_ERROR;
}

ipmi_inteldcmi_ctx_t
ipmi_inteldcmi_ctx_create (void)
{
  ipmi_inteldcmi_ctx_t ctx = NULL;

  if (!(ctx = (ipmi_inteldcmi_ctx_t)malloc (sizeof (struct ipmi_inteldcmi_ctx))))
    {
      ERRNO_TRACE (errno);
      return (NULL);
    }
  memset (ctx, '\0', sizeof (struct ipmi_inteldcmi_ctx));

  ctx->magic = IPMI_INTELDCMI_CTX_MAGIC;
  ctx->flags = IPMI_INTELDCMI_FLAGS_DEFAULT;
  ctx->driver_device = NULL;
  ctx->device_fd = -1;
  ctx->io_init = 0;

  ctx->errnum = IPMI_INTELDCMI_ERR_SUCCESS;
  return (ctx);
}

void
ipmi_inteldcmi_ctx_destroy (ipmi_inteldcmi_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_INTELDCMI_CTX_MAGIC)
    return;

  ctx->magic = ~IPMI_INTELDCMI_CTX_MAGIC;
  ctx->errnum = IPMI_INTELDCMI_ERR_SUCCESS;
  free (ctx->driver_device);
  /* ignore potential error, destroy path */
  close (ctx->device_fd);
  free (ctx);
}

int
ipmi_inteldcmi_ctx_errnum (ipmi_inteldcmi_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_INTELDCMI_ERR_NULL);
  else if (ctx->magic != IPMI_INTELDCMI_CTX_MAGIC)
    return (IPMI_INTELDCMI_ERR_INVALID);
  else
    return (ctx->errnum);
}

char *
ipmi_inteldcmi_ctx_strerror (int errnum)
{
  if (errnum >= IPMI_INTELDCMI_ERR_SUCCESS && errnum <= IPMI_INTELDCMI_ERR_ERRNUMRANGE)
    return (ipmi_inteldcmi_ctx_errmsg[errnum]);
  else
    return (ipmi_inteldcmi_ctx_errmsg[IPMI_INTELDCMI_ERR_ERRNUMRANGE]);
}

char *
ipmi_inteldcmi_ctx_errormsg (ipmi_inteldcmi_ctx_t ctx)
{
  return (ipmi_inteldcmi_ctx_strerror (ipmi_inteldcmi_ctx_errnum (ctx)));
}

int
ipmi_inteldcmi_ctx_get_driver_device (ipmi_inteldcmi_ctx_t ctx, char **driver_device)
{
  if (!ctx || ctx->magic != IPMI_INTELDCMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_inteldcmi_ctx_errormsg (ctx), ipmi_inteldcmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!driver_device)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_PARAMETERS);
      return (-1);
    }

  *driver_device = ctx->driver_device;
  ctx->errnum = IPMI_INTELDCMI_ERR_SUCCESS;
  return (0);
}

int
ipmi_inteldcmi_ctx_get_flags (ipmi_inteldcmi_ctx_t ctx, unsigned int *flags)
{
  if (!ctx || ctx->magic != IPMI_INTELDCMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_inteldcmi_ctx_errormsg (ctx), ipmi_inteldcmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!flags)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_PARAMETERS);
      return (-1);
    }

  *flags = ctx->flags;
  ctx->errnum = IPMI_INTELDCMI_ERR_SUCCESS;
  return (0);
}

int
ipmi_inteldcmi_ctx_set_driver_device (ipmi_inteldcmi_ctx_t ctx, const char *driver_device)
{
  if (!ctx || ctx->magic != IPMI_INTELDCMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_inteldcmi_ctx_errormsg (ctx), ipmi_inteldcmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!driver_device)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_PARAMETERS);
      return (-1);
    }

  free (ctx->driver_device);
  ctx->driver_device = NULL;

  if (!(ctx->driver_device = strdup (driver_device)))
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_OUT_OF_MEMORY);
      return (-1);
    }

  ctx->errnum = IPMI_INTELDCMI_ERR_SUCCESS;
  return (0);
}

int
ipmi_inteldcmi_ctx_set_flags (ipmi_inteldcmi_ctx_t ctx, unsigned int flags)
{
  if (!ctx || ctx->magic != IPMI_INTELDCMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_inteldcmi_ctx_errormsg (ctx), ipmi_inteldcmi_ctx_errnum (ctx));
      return (-1);
    }

  if (flags & ~IPMI_INTELDCMI_FLAGS_MASK)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_PARAMETERS);
      return (-1);
    }

  ctx->flags = flags;
  ctx->errnum = IPMI_INTELDCMI_ERR_SUCCESS;
  return (0);
}

int
ipmi_inteldcmi_ctx_io_init (ipmi_inteldcmi_ctx_t ctx)
{
  char *driver_device;

  if (!ctx || ctx->magic != IPMI_INTELDCMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_inteldcmi_ctx_errormsg (ctx), ipmi_inteldcmi_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->io_init)
    goto out;

  if (ctx->driver_device)
    driver_device = ctx->driver_device;
  else
    driver_device = IPMI_INTELDCMI_DRIVER_DEVICE_DEFAULT;

  if ((ctx->device_fd = open (driver_device, O_RDWR)) < 0)
    {
      INTELDCMI_ERRNO_TO_INTELDCMI_ERRNUM (ctx, errno);
      goto cleanup;
    }

  ctx->io_init = 1;
 out:
  ctx->errnum = IPMI_INTELDCMI_ERR_SUCCESS;
  return (0);

 cleanup:
  /* ignore potential error, error path */
  close (ctx->device_fd);
  ctx->device_fd = -1;
  return (-1);
}

static int
_inteldcmi_write_read (ipmi_inteldcmi_ctx_t ctx,
		       uint8_t channel_number,
		       uint8_t rs_addr,
		       uint8_t lun,
		       uint8_t net_fn,
		       fiid_obj_t obj_cmd_rq,
		       fiid_obj_t obj_cmd_rs)
{
  uint8_t rq_temp[IPMI_INTELDCMI_BUFLEN];
  uint8_t *rq_data = NULL;
  uint8_t rq_buf[IPMI_INTELDCMI_BUFLEN];
  uint8_t rs_buf[IPMI_INTELDCMI_BUFLEN];
  uint8_t rs_data[IPMI_INTELDCMI_BUFLEN];
  uint8_t rq_cmd;
  unsigned int rq_data_len = 0;
  int len;
  unsigned int rs_len;
  fiid_obj_t obj_inteldcmi_rq = NULL;
  struct inteldcmi_smi smi_msg;
  struct io_status_block ntstatusdummy;
  int rv = -1;
  int ret;

  assert (ctx);
  assert (ctx->magic == IPMI_INTELDCMI_CTX_MAGIC);
  assert (IPMI_CHANNEL_NUMBER_VALID (channel_number));
  assert (IPMI_BMC_LUN_VALID (lun));
  assert (IPMI_NET_FN_RQ_VALID (net_fn));
  assert (fiid_obj_valid (obj_cmd_rq));
  assert (fiid_obj_packet_valid (obj_cmd_rq) == 1);
  assert (fiid_obj_valid (obj_cmd_rs));

  if (!(obj_inteldcmi_rq = fiid_obj_create (tmpl_inteldcmi_request)))
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  memset (&ntstatusdummy, '\0', sizeof (struct io_status_block));

  /* Due to API differences, we need to extract the cmd out of the
   * request.
   */
  memset (rq_temp, '\0', IPMI_INTELDCMI_BUFLEN);

  if ((len = fiid_obj_get_all (obj_cmd_rq,
                               rq_temp,
                               IPMI_INTELDCMI_BUFLEN)) <= 0)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  rq_cmd = rq_temp[0];
  if (len > 1)
    {
      /* -1 b/c of cmd */
      memcpy (rq_buf, &rq_temp[1], len - 1);
      rq_data = &rq_temp[1];
      rq_data_len = len - 1;
    }

  if (fiid_obj_set (obj_inteldcmi_rq, "flags", 0) < 0)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (fiid_obj_set (obj_inteldcmi_rq, "timeout", IPMI_INTELDCMI_TIMEOUT) < 0)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (fiid_obj_set (obj_inteldcmi_rq, "rs_addr", rs_addr) < 0)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (fiid_obj_set (obj_inteldcmi_rq, "cmd", rq_cmd) < 0)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (fiid_obj_set (obj_inteldcmi_rq, "net_fn", net_fn) < 0)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (fiid_obj_set (obj_inteldcmi_rq, "rs_lun", lun) < 0)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (fiid_obj_set (obj_inteldcmi_rq, "data_len", rq_data_len) < 0)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (rq_data && rq_data_len)
    {
      if (fiid_obj_set_data (obj_inteldcmi_rq, "data", rq_data, rq_data_len) < 0)
	{
	  INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_INTERNAL_ERROR);
	  goto cleanup;
	}
    }

  if ((len = fiid_obj_get_all (obj_inteldcmi_rq,
			       rq_buf,
			       IPMI_INTELDCMI_BUFLEN)) < 0)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  /* achu:
   * 
   * In Intel example code, buffer is always 41.  I can't understand
   * why, but unless I do this, it won't work.
   */
  assert (IPMI_INTELDCMI_MIN_RQ_BUFLEN <= IPMI_INTELDCMI_BUFLEN);
  if (len != IPMI_INTELDCMI_MIN_RQ_BUFLEN)
    len = IPMI_INTELDCMI_MIN_RQ_BUFLEN;

  memset (&smi_msg, '\0', sizeof (struct inteldcmi_smi));
  smi_msg.ntstatus = &ntstatusdummy;
  smi_msg.lpvInBuffer = rq_buf;
  smi_msg.cbInBuffer = len;
  smi_msg.lpvOutBuffer = rs_buf;
  smi_msg.cbOutBuffer = IPMI_INTELDCMI_BUFLEN;
  smi_msg.lpcbBytesReturned = (void *)&rs_len;
  smi_msg.lpoOverlapped = NULL;

  if ((ret = ioctl (ctx->device_fd,
		    IPMI_INTELDCMI_IOCTL_IMB_SEND_MESSAGE,
		    &smi_msg)) < 0)
    {
      INTELDCMI_ERRNO_TO_INTELDCMI_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (ret != IPMI_INTELDCMI_STATUS_SUCCESS)
    {
      TRACE_MSG_OUT ("Intel DCMI ioctl status", ret);

      if (ret == IPMI_INTELDCMI_STATUS_DEVICE_BUSY)
	INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_DRIVER_TIMEOUT);
      else
	INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_SYSTEM_ERROR);

      goto cleanup;
    }

  /* achu: atleast the completion code should be returned */
  if (!rs_len)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_SYSTEM_ERROR);
      return (-1);
    }

  rs_data[0] = rq_cmd;
  /* -1 b/c of cmd */
  if (rs_len >= (IPMI_INTELDCMI_BUFLEN - 1))
    rs_len = IPMI_INTELDCMI_BUFLEN - 1;
  memcpy (rs_data + 1, rs_buf, rs_len);

  if (fiid_obj_set_all (obj_cmd_rs,
                        rs_data,
                        rs_len + 1) < 0)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_INTERNAL_ERROR);
      return (-1);
    }

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_inteldcmi_rq);
  return (rv);
}

int
ipmi_inteldcmi_cmd (ipmi_inteldcmi_ctx_t ctx,
		    uint8_t lun,
		    uint8_t net_fn,
		    fiid_obj_t obj_cmd_rq,
		    fiid_obj_t obj_cmd_rs)
{
  if (!ctx || ctx->magic != IPMI_INTELDCMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_inteldcmi_ctx_errormsg (ctx), ipmi_inteldcmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_BMC_LUN_VALID (lun)
      || !IPMI_NET_FN_RQ_VALID (net_fn)
      || !fiid_obj_valid (obj_cmd_rq)
      || !fiid_obj_valid (obj_cmd_rs)
      || fiid_obj_packet_valid (obj_cmd_rq) <= 0)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_PARAMETERS);
      return (-1);
    }

  if (!ctx->io_init)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_IO_NOT_INITIALIZED);
      return (-1);
    }

  if (_inteldcmi_write_read (ctx,
			     IPMI_CHANNEL_NUMBER_PRIMARY_IPMB,
			     IPMI_SLAVE_ADDRESS_BMC,
			     lun,
			     net_fn,
			     obj_cmd_rq,
			     obj_cmd_rs) < 0)
    return (-1);

  return (0);
}

int
ipmi_inteldcmi_cmd_ipmb (ipmi_inteldcmi_ctx_t ctx,
			 uint8_t channel_number,
			 uint8_t rs_addr,
			 uint8_t lun,
			 uint8_t net_fn,
			 fiid_obj_t obj_cmd_rq,
			 fiid_obj_t obj_cmd_rs)
{
  if (!ctx || ctx->magic != IPMI_INTELDCMI_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_inteldcmi_ctx_errormsg (ctx), ipmi_inteldcmi_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_CHANNEL_NUMBER_VALID (channel_number)
      || !IPMI_BMC_LUN_VALID (lun)
      || !IPMI_NET_FN_RQ_VALID (net_fn)
      || !fiid_obj_valid (obj_cmd_rq)
      || !fiid_obj_valid (obj_cmd_rs)
      || fiid_obj_packet_valid (obj_cmd_rq) <= 0)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_PARAMETERS);
      return (-1);
    }

  if (!ctx->io_init)
    {
      INTELDCMI_SET_ERRNUM (ctx, IPMI_INTELDCMI_ERR_IO_NOT_INITIALIZED);
      return (-1);
    }

  if (_inteldcmi_write_read (ctx,
			     channel_number,
			     rs_addr,
			     lun,
			     net_fn,
			     obj_cmd_rq,
			     obj_cmd_rs) < 0)
    return (-1);

  return (0);
}
