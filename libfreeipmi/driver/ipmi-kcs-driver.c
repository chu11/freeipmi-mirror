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
#include <limits.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/driver/ipmi-kcs-driver.h"
#include "freeipmi/interface/ipmi-interface.h"
#include "freeipmi/interface/ipmi-kcs-interface.h"
#include "freeipmi/spec/ipmi-ipmb-lun-spec.h"
#include "freeipmi/spec/ipmi-netfn-spec.h"

#include "ipmi-driver-trace.h"
#include "ipmi-semaphores.h"

#include "libcommon/ipmi-fiid-util.h"

#include "freeipmi-portability.h"

#define IPMI_KCS_SLEEP_USECS                  0x01

/* timeout after 60 seconds */
#define IPMI_KCS_TIMEOUT_USECS                60000000

#define IPMI_KCS_SMS_REGISTER_SPACING_DEFAULT 1
/* KCS Interface Status Register Bits */
/* Scheme BIT Calculator Example
   To BIN:
   (format #f "[~8,'0b]" #x80) => "[10000000]"
   To HEX:
   (format #f "[0x~2,'0x]" #b10000000) => "[0x80]"
*/
#define IPMI_KCS_STATUS_REG_S1          0x80
#define IPMI_KCS_STATUS_REG_S0          0x40
#define IPMI_KCS_STATUS_REG_STATE       (IPMI_KCS_STATUS_REG_S0 | IPMI_KCS_STATUS_REG_S1)
#define IPMI_KCS_STATUS_REG_OEM2        0x20
#define IPMI_KCS_STATUS_REG_OEM1        0x10
#define IPMI_KCS_STATUS_REG_CD          0x08 /* last-written (command or data) */
#define IPMI_KCS_STATUS_REG_SMS_ATN     0x04
#define IPMI_KCS_STATUS_REG_IBF         0x02
#define IPMI_KCS_STATUS_REG_OBF         0x01

/* IPMI KCS Interface State Bits */
#define IPMI_KCS_STATE_IDLE   0x00
#define IPMI_KCS_STATE_READ   IPMI_KCS_STATUS_REG_S0
#define IPMI_KCS_STATE_WRITE  IPMI_KCS_STATUS_REG_S1
#define IPMI_KCS_STATE_ERROR  (IPMI_KCS_STATUS_REG_S0 & IPMI_KCS_STATUS_REG_S1)

/* IPMI KCS Interface Status Codes */
#define IPMI_KCS_STATUS_NO_ERROR             0x00
#define IPMI_KCS_STATUS_SUCCESS              IPMI_KCS_STATUS_NO_ERR
#define IPMI_KCS_STATUS_OK                   IPMI_KCS_STATUS_NO_ERR

#define IPMI_KCS_STATUS_NO_ERROR_STR            \
  "No error"

#define IPMI_KCS_STATUS_ABORTED_BY_CMD         0x01
#define IPMI_KCS_STATUS_ABORTED_BY_CMD_STR                      \
  "Aborted by command (Transfer in progress was "               \
  "aborted by SMS issuing the Abort/Status control code)"

#define IPMI_KCS_STATUS_ILLEGAL_CTRL_CODE      0x02
#define IPMI_KCS_STATUS_ILLEGAL_CTRL_CODE_STR   \
  "Illegal control code"

#define IPMI_KCS_STATUS_LEN_ERROR              0x06
#define IPMI_KCS_STATUS_LEN_ERROR_STR           \
  "Length error (e.g.overrun)"

#define IPMI_KCS_STATUS_OEM_ERROR_BEGIN        0xC0
#define IPMI_KCS_STATUS_OEM_ERROR_END          0xFE

#define IPMI_KCS_STATUS_UNSPECIFIED_ERROR      0xFF
#define IPMI_KCS_STATUS_UNSPECIFIED_ERROR_STR   \
  "Unspecified error"

/* Reserved - all others */

/* IPMI KCS SMS Interface Registers */
#define IPMI_KCS_REG_DATAIN(sms_io_base)   (sms_io_base)
#define IPMI_KCS_REG_DATAOUT(sms_io_base)  (sms_io_base)
#define IPMI_KCS_REG_CMD(sms_io_base, register_spacing) \
  (sms_io_base + register_spacing)
#define IPMI_KCS_REG_STATUS(sms_io_base, register_spacing)      \
  (sms_io_base + register_spacing)

/* IPMI KCS Control Codes */
#define IPMI_KCS_CTRL_GET_STATUS       0x60 /* Request Interface Status /
                                               Abort Current operation */
#define IPMI_KCS_CTRL_GET_ABORT        IPMI_KCS_CTRL_GET_STATUS
#define IPMI_KCS_CTRL_WRITE_START      0x61 /* Write the First byte of an Write Transfer */
#define IPMI_KCS_CTRL_WRITE_END        0x62 /* Write the Last byte of an Write Transfer */
/* reserved      0x63 - 0x67 */
#define IPMI_KCS_CTRL_READ             0x68 /* Request the next data byte */
/* reserved      0x69 - 0x6F */

#define IPMI_KCS_CTX_MAGIC 0xabbaadda

#define IPMI_KCS_FLAGS_MASK \
  (IPMI_KCS_FLAGS_NONBLOCKING \
   | IPMI_KCS_FLAGS_SPIN_POLL)

#define IPMI_KCS_MICROSECONDS_IN_SECOND 1000000

#if defined(__FreeBSD__)
# include <machine/cpufunc.h>
# include <machine/sysarch.h>
#elif defined(__NetBSD__) || defined(__OpenBSD__)
# include <machine/pio.h>               /* inb/outb */
# include <machine/sysarch.h>   /* sysarch call */
#elif defined(HAVE_SYS_IO_H)
/* Linux, _AXP_ */
# include <sys/io.h>
#elif defined(HAVE_ASM_IO_H)
/* PPC */
# include <asm/io.h>
#endif

#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
# define _INB(port)  inb (port)
# define _OUTB(data, port)  outb (port, data)
#elif defined(HAVE_IOPL)
# define _INB(port)  inb (port)
# define _OUTB(data, port)  outb (data, port)
#else
# define _INB(port)  0
# define _OUTB(data, port)
#endif

static char * ipmi_kcs_ctx_errmsg[] =
  {
    "success",
    "kcs context null",
    "kcs context invalid",
    "invalid parameter",
    "permission denied",
    "io not initialized",
    "buffer too small to hold result",
    "BMC busy",
    "out of memory",
    "device not found",
    "driver timeout",
    "internal IPMI error",
    "internal system error",
    "internal error",
    "errnum out of range",
    NULL,
  };

struct ipmi_kcs_ctx {
  uint32_t magic;
  int errnum;
  uint16_t driver_address;
  uint8_t register_spacing;
  unsigned int flags;
  unsigned int poll_interval;
#ifdef __FreeBSD__
#ifndef USE_IOPERM
  int dev_fd;
#endif
#endif /* __FreeBSD__ */
  int io_init;
  int semid;
};

static void
_set_kcs_ctx_errnum_by_errno (ipmi_kcs_ctx_t ctx, int _errno)
{
  if (!ctx || ctx->magic != IPMI_KCS_CTX_MAGIC)
    return;

  if (_errno == 0)
    ctx->errnum = IPMI_KCS_ERR_SUCCESS;
  else if (_errno == EINTR)
    ctx->errnum = IPMI_KCS_ERR_BUSY;
  else if (_errno == EAGAIN)
    ctx->errnum = IPMI_KCS_ERR_BUSY;
  else if (_errno == EPERM)
    ctx->errnum = IPMI_KCS_ERR_PERMISSION;
  else if (_errno == EACCES)
    ctx->errnum = IPMI_KCS_ERR_PERMISSION;
  else if (_errno == ENOENT)
    ctx->errnum = IPMI_KCS_ERR_DEVICE_NOT_FOUND;
  else if (_errno == ENOMEM)
    ctx->errnum = IPMI_KCS_ERR_OUT_OF_MEMORY;
  else if (_errno == EINVAL)
    ctx->errnum = IPMI_KCS_ERR_INTERNAL_ERROR;
  else if (_errno == ETIMEDOUT)
    ctx->errnum = IPMI_KCS_ERR_DRIVER_TIMEOUT;
  else
    ctx->errnum = IPMI_KCS_ERR_SYSTEM_ERROR;
}

static void
_set_kcs_errnum_by_fiid_object (ipmi_kcs_ctx_t ctx, fiid_obj_t obj)
{
  if (!ctx || ctx->magic != IPMI_KCS_CTX_MAGIC)
    return;

  if (!fiid_obj_valid (obj))
    {
      KCS_SET_ERRNUM (ctx, IPMI_ERR_INTERNAL_ERROR);
      return;
    }

  if (fiid_obj_errnum (obj) == FIID_ERR_SUCCESS)
    ctx->errnum = IPMI_KCS_ERR_SUCCESS;
  else if (fiid_obj_errnum (obj) == FIID_ERR_OUT_OF_MEMORY)
    ctx->errnum = IPMI_KCS_ERR_OUT_OF_MEMORY;
  else if (fiid_obj_errnum (obj) == FIID_ERR_DATA_NOT_AVAILABLE)
    ctx->errnum = IPMI_KCS_ERR_SYSTEM_ERROR;
  else if (fiid_obj_errnum (obj) == FIID_ERR_NOT_IDENTICAL
           || fiid_obj_errnum (obj) == FIID_ERR_FIELD_NOT_FOUND)
    ctx->errnum = IPMI_KCS_ERR_PARAMETERS;
  else
    ctx->errnum = IPMI_KCS_ERR_INTERNAL_ERROR;
}

ipmi_kcs_ctx_t
ipmi_kcs_ctx_create (void)
{
  ipmi_kcs_ctx_t ctx = NULL;

  if (!(ctx = (ipmi_kcs_ctx_t)malloc (sizeof (struct ipmi_kcs_ctx))))
    {
      ERRNO_TRACE (errno);
      return (NULL);
    }
  memset (ctx, '\0', sizeof (struct ipmi_kcs_ctx));

  ctx->magic = IPMI_KCS_CTX_MAGIC;
  ctx->driver_address = IPMI_KCS_SMS_IO_BASE_DEFAULT;
  ctx->register_spacing = IPMI_KCS_SMS_REGISTER_SPACING_DEFAULT;
  ctx->flags = IPMI_KCS_FLAGS_DEFAULT;
  ctx->poll_interval = IPMI_KCS_SLEEP_USECS;
  ctx->io_init = 0;

  if ((ctx->semid = driver_mutex_init ()) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }
  ctx->errnum = IPMI_KCS_ERR_SUCCESS;
  return (ctx);

 cleanup:
  free (ctx);
  return (NULL);
}

void
ipmi_kcs_ctx_destroy (ipmi_kcs_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_KCS_CTX_MAGIC)
    return;

  ctx->magic = ~IPMI_KCS_CTX_MAGIC;
  ctx->errnum = IPMI_KCS_ERR_SUCCESS;
#ifdef __FreeBSD__
#ifndef USE_IOPERM
  /* ignore potential error, destroy path */
  close (ctx->dev_fd);
#endif
#endif /* __FreeBSD__ */
  free (ctx);
}

int
ipmi_kcs_ctx_errnum (ipmi_kcs_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_KCS_ERR_NULL);
  else if (ctx->magic != IPMI_KCS_CTX_MAGIC)
    return (IPMI_KCS_ERR_INVALID);
  else
    return (ctx->errnum);
}

char *
ipmi_kcs_ctx_strerror (int errnum)
{
  if (errnum >= IPMI_KCS_ERR_SUCCESS && errnum <= IPMI_KCS_ERR_ERRNUMRANGE)
    return (ipmi_kcs_ctx_errmsg[errnum]);
  else
    return (ipmi_kcs_ctx_errmsg[IPMI_KCS_ERR_ERRNUMRANGE]);
}

char *
ipmi_kcs_ctx_errormsg (ipmi_kcs_ctx_t ctx)
{
  return (ipmi_kcs_ctx_strerror (ipmi_kcs_ctx_errnum (ctx)));
}

int
ipmi_kcs_ctx_get_driver_address (ipmi_kcs_ctx_t ctx, uint16_t *driver_address)
{
  if (!ctx || ctx->magic != IPMI_KCS_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_kcs_ctx_errormsg (ctx), ipmi_kcs_ctx_errnum (ctx));
      return (-1);
    }

  if (!driver_address)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_PARAMETERS);
      return (-1);
    }

  *driver_address = ctx->driver_address;
  ctx->errnum = IPMI_KCS_ERR_SUCCESS;
  return (0);
}

int
ipmi_kcs_ctx_get_register_spacing (ipmi_kcs_ctx_t ctx, uint8_t *register_spacing)
{
  if (!ctx || ctx->magic != IPMI_KCS_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_kcs_ctx_errormsg (ctx), ipmi_kcs_ctx_errnum (ctx));
      return (-1);
    }

  if (!register_spacing)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_PARAMETERS);
      return (-1);
    }

  *register_spacing = ctx->register_spacing;
  ctx->errnum = IPMI_KCS_ERR_SUCCESS;
  return (0);
}

int
ipmi_kcs_ctx_get_poll_interval (ipmi_kcs_ctx_t ctx, uint8_t *poll_interval)
{
  if (!ctx || ctx->magic != IPMI_KCS_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_kcs_ctx_errormsg (ctx), ipmi_kcs_ctx_errnum (ctx));
      return (-1);
    }

  if (!poll_interval)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_PARAMETERS);
      return (-1);
    }

  *poll_interval = ctx->poll_interval;
  ctx->errnum = IPMI_KCS_ERR_SUCCESS;
  return (0);
}

int
ipmi_kcs_ctx_get_flags (ipmi_kcs_ctx_t ctx, unsigned int *flags)
{
  if (!ctx || ctx->magic != IPMI_KCS_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_kcs_ctx_errormsg (ctx), ipmi_kcs_ctx_errnum (ctx));
      return (-1);
    }

  if (!flags)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_PARAMETERS);
      return (-1);
    }

  *flags = ctx->flags;
  ctx->errnum = IPMI_KCS_ERR_SUCCESS;
  return (0);
}

int
ipmi_kcs_ctx_set_driver_address (ipmi_kcs_ctx_t ctx, uint16_t driver_address)
{
  if (!ctx || ctx->magic != IPMI_KCS_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_kcs_ctx_errormsg (ctx), ipmi_kcs_ctx_errnum (ctx));
      return (-1);
    }

  ctx->driver_address = driver_address;
  ctx->errnum = IPMI_KCS_ERR_SUCCESS;
  return (0);
}

int
ipmi_kcs_ctx_set_register_spacing (ipmi_kcs_ctx_t ctx, uint8_t register_spacing)
{
  if (!ctx || ctx->magic != IPMI_KCS_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_kcs_ctx_errormsg (ctx), ipmi_kcs_ctx_errnum (ctx));
      return (-1);
    }

  ctx->register_spacing = register_spacing;
  ctx->errnum = IPMI_KCS_ERR_SUCCESS;
  return (0);
}

int
ipmi_kcs_ctx_set_poll_interval (ipmi_kcs_ctx_t ctx, uint8_t poll_interval)
{
  if (!ctx || ctx->magic != IPMI_KCS_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_kcs_ctx_errormsg (ctx), ipmi_kcs_ctx_errnum (ctx));
      return (-1);
    }

  ctx->poll_interval = poll_interval;
  ctx->errnum = IPMI_KCS_ERR_SUCCESS;
  return (0);
}

int
ipmi_kcs_ctx_set_flags (ipmi_kcs_ctx_t ctx, unsigned int flags)
{
  if (!ctx || ctx->magic != IPMI_KCS_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_kcs_ctx_errormsg (ctx), ipmi_kcs_ctx_errnum (ctx));
      return (-1);
    }

  if (flags & ~IPMI_KCS_FLAGS_MASK)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_PARAMETERS);
      return (-1);
    }

  ctx->flags = flags;
  ctx->errnum = IPMI_KCS_ERR_SUCCESS;
  return (0);
}

int
ipmi_kcs_ctx_io_init (ipmi_kcs_ctx_t ctx)
{
  if (!ctx || ctx->magic != IPMI_KCS_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_kcs_ctx_errormsg (ctx), ipmi_kcs_ctx_errnum (ctx));
      return (-1);
    }

  if (ctx->io_init)
    goto out;

#ifdef __FreeBSD__
#ifdef USE_IOPERM
  /* i386_set_ioperm has known problems on FBSD 5.x (bus errors). */
  if (i386_set_ioperm (ctx->driver_address, 0x02, 0x01))
    {
      KCS_ERRNO_TO_KCS_ERRNUM (ctx, errno);
      return (-1);
    }
#else  /* !USE_IOPERM */
  /* Opening /dev/io raises IOPL bits for current process. */
  if ((ctx->dev_fd = open ("/dev/io", O_RDONLY)) < 0)
    {
      KCS_ERRNO_TO_KCS_ERRNUM (ctx, errno);
      return (-1);
    }
#endif /* !USE_IOPERM */
#else  /* !__FreeBSD__ */
#if HAVE_IOPL
  if (iopl (3) < 0)
    {
      KCS_ERRNO_TO_KCS_ERRNUM (ctx, errno);
      return (-1);
    }
#else /* !HAVE_IOPL */
  /* otherwise, we always return a system error */
  KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_SYSTEM_ERROR);
  return (-1);
#endif /* !HAVE_IOPL */
#endif /* !__FreeBSD__ */

  ctx->io_init = 1;
 out:
  ctx->errnum = IPMI_KCS_ERR_SUCCESS;
  return (0);
}

static int8_t
_ipmi_kcs_get_status (ipmi_kcs_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_KCS_CTX_MAGIC);

  return (_INB (IPMI_KCS_REG_STATUS (ctx->driver_address, ctx->register_spacing)));
}

static unsigned long
_ipmi_kcs_timeval_diff (struct timeval *start, struct timeval *end)
{
  unsigned long t;

  assert (start);
  assert (end);

  if (end->tv_sec == start->tv_sec)
    t = end->tv_usec - start->tv_usec;
  else
    {
      t = (end->tv_sec - start->tv_sec - 1) * IPMI_KCS_MICROSECONDS_IN_SECOND;
      t += (IPMI_KCS_MICROSECONDS_IN_SECOND - start->tv_usec);
      t += end->tv_usec;
    }
  
  return (t);
}

static int
_ipmi_kcs_spin_sleep (ipmi_kcs_ctx_t ctx)
{
  struct timeval spinstart;
  struct timeval spinend;

  assert (ctx);
  assert (ctx->magic == IPMI_KCS_CTX_MAGIC);

  /* achu: There is probably something better than this, but
   * this is cheap and simple and I can't find any research
   * that there is something substantially superior to this.
   */

  if (gettimeofday (&spinstart, NULL) < 0)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_SYSTEM_ERROR);
      return (-1);
    }
  
  while (1)
    {
      unsigned long t;
      
      if (gettimeofday (&spinend, NULL) < 0)
	{
	  KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_SYSTEM_ERROR);
	  return (-1);
	}

      t = _ipmi_kcs_timeval_diff (&spinstart, &spinend);

      if (t > ctx->poll_interval)
	break;
    }

  return (0);
}

static int
_ipmi_kcs_sleep (ipmi_kcs_ctx_t ctx, struct timeval *start)
{
  struct timeval end;
  unsigned long t;

  assert (ctx);
  assert (ctx->magic == IPMI_KCS_CTX_MAGIC);
  assert (start);

  /* achu: Why calculate a timeout this way?  Why not via
   * timeout/poll_interval, and count the loops?  OS timer
   * granularity is the issue.  Another way is via posix realtime
   * timers, but handling signals in this part of the code is sort
   * of sketchy.  This is perhaps not the most efficient, but is
   * safe and portable.
   */

  if (gettimeofday (&end, NULL) < 0)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_SYSTEM_ERROR);
      return (-1);
    }

  t = _ipmi_kcs_timeval_diff (start, &end);

  if (t > IPMI_KCS_TIMEOUT_USECS)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_DRIVER_TIMEOUT);
      return (-1);
    }

  if (!(ctx->flags & IPMI_KCS_FLAGS_SPIN_POLL))
    usleep (ctx->poll_interval);
  else
    {
      if (_ipmi_kcs_spin_sleep (ctx) < 0)
	return (-1);
    }
  
  return (0);
}

/*
 * Wait for IBF (In-Bound Flag) to clear, signalling BMC has
 * read the command.
 */
static int
_ipmi_kcs_wait_for_ibf_clear (ipmi_kcs_ctx_t ctx)
{
  struct timeval start;

  assert (ctx);
  assert (ctx->magic == IPMI_KCS_CTX_MAGIC);

  if (gettimeofday (&start, NULL) < 0)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_SYSTEM_ERROR);
      return (-1);
    }

  while (_ipmi_kcs_get_status (ctx) & IPMI_KCS_STATUS_REG_IBF)
    {
      if (_ipmi_kcs_sleep (ctx, &start) < 0)
	return (-1);
    }

  return (0);
}

/*
 * Wait for OBF to raise, signalling data is pending to read
 * or no command is pending.
 */

static int
_ipmi_kcs_wait_for_obf_set (ipmi_kcs_ctx_t ctx)
{
  struct timeval start;

  assert (ctx);
  assert (ctx->magic == IPMI_KCS_CTX_MAGIC);

  if (gettimeofday (&start, NULL) < 0)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_SYSTEM_ERROR);
      return (-1);
    }

  while (!(_ipmi_kcs_get_status (ctx) & IPMI_KCS_STATUS_REG_OBF))
    {
      if (_ipmi_kcs_sleep (ctx, &start) < 0)
	return (-1);
    }

  return (0);
}

/*
 * Read byte from outbound data port.
 */
static int8_t
_ipmi_kcs_read_byte (ipmi_kcs_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_KCS_CTX_MAGIC);

  return (_INB (IPMI_KCS_REG_DATAOUT (ctx->driver_address)));
}

/*
 * Bump channel into sending next byte.
 */
static void
_ipmi_kcs_read_next (ipmi_kcs_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_KCS_CTX_MAGIC);

  _OUTB (IPMI_KCS_CTRL_READ,
         IPMI_KCS_REG_DATAIN (ctx->driver_address));
}

/*
 * Set up channel for writing.
 */
static void
_ipmi_kcs_start_write (ipmi_kcs_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_KCS_CTX_MAGIC);

  _OUTB (IPMI_KCS_CTRL_WRITE_START,
         IPMI_KCS_REG_CMD (ctx->driver_address, ctx->register_spacing));
}

/*
 * Write byte to inound data port.
 */
static void
_ipmi_kcs_write_byte (ipmi_kcs_ctx_t ctx, uint8_t byte)
{
  assert (ctx);
  assert (ctx->magic == IPMI_KCS_CTX_MAGIC);

  _OUTB (byte,
         IPMI_KCS_REG_DATAIN (ctx->driver_address));
}

/*
 * Set up channel to end write.
 */
static void
_ipmi_kcs_end_write (ipmi_kcs_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_KCS_CTX_MAGIC);

  _OUTB (IPMI_KCS_CTRL_WRITE_END,
         IPMI_KCS_REG_CMD (ctx->driver_address, ctx->register_spacing));
}

#if 0
/* Not used at all right now */

/*
 * Send Abort current processing command.
 */
static void
_ipmi_kcs_get_abort (ipmi_kcs_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_KCS_CTX_MAGIC);

  _OUTB (IPMI_KCS_CTRL_GET_ABORT,
         IPMI_KCS_REG_CMD (ctx->driver_address, ctx->register_spacing));
}
#endif

static int
_ipmi_kcs_test_if_state (ipmi_kcs_ctx_t ctx, uint8_t status)
{
  assert (ctx);
  assert (ctx->magic == IPMI_KCS_CTX_MAGIC);

  if ((_ipmi_kcs_get_status (ctx) & IPMI_KCS_STATUS_REG_STATE) ==
      (status & IPMI_KCS_STATUS_REG_STATE))
    return (1);
  else
    return (0);
}

/*
 * Read dummy byte to clear OBF if set.
 */
static void
_ipmi_kcs_clear_obf (ipmi_kcs_ctx_t ctx)
{
  assert (ctx);
  assert (ctx->magic == IPMI_KCS_CTX_MAGIC);

  if (_ipmi_kcs_get_status (ctx) & IPMI_KCS_STATUS_REG_OBF)
    _ipmi_kcs_read_byte (ctx);
}

int
ipmi_kcs_write (ipmi_kcs_ctx_t ctx,
                const void *buf,
                unsigned int buf_len)
{
  const uint8_t *p = buf;
  unsigned int count = 0;
  int lock_flag = 0;

  if (!ctx || ctx->magic != IPMI_KCS_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_kcs_ctx_errormsg (ctx), ipmi_kcs_ctx_errnum (ctx));
      return (-1);
    }

  if (!buf || !buf_len)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_PARAMETERS);
      return (-1);
    }

  if (!ctx->io_init)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_IO_NOT_INITIALIZED);
      return (-1);
    }

  if (!(ctx->flags & IPMI_KCS_FLAGS_NONBLOCKING))
    {
      if (driver_mutex_lock (ctx->semid) < 0)
        {
          KCS_ERRNO_TO_KCS_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }
  else
    {
      if (driver_mutex_lock_interruptible (ctx->semid) < 0)
        {
          KCS_ERRNO_TO_KCS_ERRNUM (ctx, errno);
          goto cleanup;
        }
    }
  lock_flag++;

  if (_ipmi_kcs_wait_for_ibf_clear (ctx) < 0)
    goto cleanup;

  _ipmi_kcs_clear_obf (ctx);

  _ipmi_kcs_start_write (ctx);

  if (_ipmi_kcs_wait_for_ibf_clear (ctx) < 0)
    goto cleanup;

  if (!_ipmi_kcs_test_if_state (ctx, IPMI_KCS_STATE_WRITE))
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_BUSY);
      goto cleanup;
    }

  _ipmi_kcs_clear_obf (ctx);

  /* note we have to save last byte. */
  /* for (buf=data; data+len-1 < buf; buf++) */
  for (; buf_len > 1; buf_len--)
    {
      _ipmi_kcs_write_byte (ctx, *p);

      if (_ipmi_kcs_wait_for_ibf_clear (ctx) < 0)
	goto cleanup;

      if (!_ipmi_kcs_test_if_state (ctx, IPMI_KCS_STATE_WRITE))
        {
          KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_BUSY);
          goto cleanup;
        }

      _ipmi_kcs_clear_obf (ctx);
      p++;
      count++;
    }
  _ipmi_kcs_end_write (ctx);

  if (_ipmi_kcs_wait_for_ibf_clear (ctx) < 0)
    goto cleanup;

  if (!_ipmi_kcs_test_if_state (ctx, IPMI_KCS_STATE_WRITE))
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_BUSY);
      goto cleanup;
    }

  _ipmi_kcs_clear_obf (ctx);

  _ipmi_kcs_write_byte (ctx, *p);

  count++;

#if 0
  if (!_ipmi_kcs_test_if_state (IPMI_KCS_STATE_READ))
    {
      printf ("Not in READ state after writing last byte?\n");
      ipmi_kcs_print_state (ipmi_kcs_get_state ());
      exit (EXIT_FAILURE);
    }
#endif
  
  if (count > INT_MAX)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_OVERFLOW);
      goto cleanup;
    }

  ctx->errnum = IPMI_KCS_ERR_SUCCESS;
  return (count);

 cleanup:
  if (lock_flag)
    driver_mutex_unlock (ctx->semid);
  return (-1);
}

/*
 * Main read loop.
 */
int
ipmi_kcs_read (ipmi_kcs_ctx_t ctx,
               void *buf,
               unsigned int buf_len)
{
  uint8_t *p = buf;
  unsigned int count = 0;
  int rv = -1;

  if (!ctx || ctx->magic != IPMI_KCS_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_kcs_ctx_errormsg (ctx), ipmi_kcs_ctx_errnum (ctx));
      goto cleanup;
    }

  if (!buf || !buf_len)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_PARAMETERS);
      goto cleanup;
    }

  if (!ctx->io_init)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_IO_NOT_INITIALIZED);
      goto cleanup;
    }

  if (_ipmi_kcs_wait_for_ibf_clear (ctx) < 0)
    goto cleanup;

  if (!_ipmi_kcs_test_if_state (ctx, IPMI_KCS_STATE_READ))
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_BUSY);
      goto cleanup;
    }

  while (_ipmi_kcs_test_if_state (ctx, IPMI_KCS_STATE_READ))
    {
      char c;
      if (_ipmi_kcs_wait_for_obf_set (ctx) < 0)
	goto cleanup;
      c = _ipmi_kcs_read_byte (ctx);
      if (count < buf_len)
        {
          *(p++) = c;
          count++;
        }
      _ipmi_kcs_read_next (ctx);
      if (_ipmi_kcs_wait_for_ibf_clear (ctx) < 0)
	goto cleanup;
    }

  if (_ipmi_kcs_test_if_state (ctx, IPMI_KCS_STATE_IDLE))
    {
      /* Clean up */
      if (_ipmi_kcs_wait_for_obf_set (ctx) < 0)
	goto cleanup;
      _ipmi_kcs_read_byte (ctx); /* toss it, ACK */
    }
  else
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_DRIVER_TIMEOUT);
      goto cleanup;
    }

  if (count > buf_len)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_OVERFLOW);
      goto cleanup;
    }

  if (count > INT_MAX)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_OVERFLOW);
      goto cleanup;
    }

  ctx->errnum = IPMI_KCS_ERR_SUCCESS;
  rv = count;
 cleanup:
  if (ctx && ctx->magic == IPMI_KCS_CTX_MAGIC)
    driver_mutex_unlock (ctx->semid);
  return (rv);
}

static int
_ipmi_kcs_cmd_write (ipmi_kcs_ctx_t ctx,
                     uint8_t lun,
                     uint8_t net_fn,
                     fiid_obj_t obj_cmd_rq)
{
  uint8_t *pkt = NULL;
  unsigned int pkt_len;
  int hdr_len, cmd_len, rv = -1;
  fiid_obj_t obj_hdr = NULL;

  assert (ctx);
  assert (ctx->magic == IPMI_KCS_CTX_MAGIC);
  assert (IPMI_BMC_LUN_VALID (lun));
  assert (IPMI_NET_FN_RQ_VALID (net_fn));
  assert (fiid_obj_valid (obj_cmd_rq));
  assert (fiid_obj_packet_valid (obj_cmd_rq) == 1);

  if ((hdr_len = fiid_template_len_bytes (tmpl_hdr_kcs)) < 0)
    {
      KCS_ERRNO_TO_KCS_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if ((cmd_len = fiid_obj_len_bytes (obj_cmd_rq)) < 0)
    {
      KCS_FIID_OBJECT_ERROR_TO_KCS_ERRNUM (ctx, obj_cmd_rq);
      goto cleanup;
    }

  if (!(obj_hdr = fiid_obj_create (tmpl_hdr_kcs)))
    {
      KCS_ERRNO_TO_KCS_ERRNUM (ctx, errno);
      goto cleanup;
    }

  pkt_len = hdr_len + cmd_len;

  if (!(pkt = (uint8_t *)malloc (pkt_len)))
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_OUT_OF_MEMORY);
      goto cleanup;
    }

  memset (pkt, 0, pkt_len);

  if (fill_hdr_ipmi_kcs (lun,
                         net_fn,
                         obj_hdr) < 0)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (assemble_ipmi_kcs_pkt (obj_hdr,
                             obj_cmd_rq,
                             pkt,
                             pkt_len,
			     IPMI_INTERFACE_FLAGS_DEFAULT) < 0)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  if (ipmi_kcs_write (ctx, pkt, pkt_len) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_hdr);
  free (pkt);
  return (rv);
}

static int
_ipmi_kcs_cmd_read (ipmi_kcs_ctx_t ctx,
                    fiid_obj_t obj_cmd_rs)
{
  uint8_t *pkt = NULL;
  unsigned int pkt_len;
  int hdr_len, cmd_len, read_len, ret, rv = -1;
  fiid_obj_t obj_hdr = NULL;
  fiid_field_t *tmpl = NULL;

  assert (ctx);
  assert (ctx->magic == IPMI_KCS_CTX_MAGIC);
  assert (fiid_obj_valid (obj_cmd_rs));

  if ((hdr_len = fiid_template_len_bytes (tmpl_hdr_kcs)) < 0)
    {
      KCS_ERRNO_TO_KCS_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (!(tmpl = fiid_obj_template (obj_cmd_rs)))
    {
      KCS_FIID_OBJECT_ERROR_TO_KCS_ERRNUM (ctx, obj_cmd_rs);
      goto cleanup;
    }

  if ((cmd_len = fiid_template_len_bytes (tmpl)) < 0)
    {
      KCS_ERRNO_TO_KCS_ERRNUM (ctx, errno);
      goto cleanup;
    }

  if (!(obj_hdr = fiid_obj_create (tmpl_hdr_kcs)))
    {
      KCS_ERRNO_TO_KCS_ERRNUM (ctx, errno);
      goto cleanup;
    }

  pkt_len = hdr_len + cmd_len;

  if (!(pkt = (uint8_t *)malloc (pkt_len)))
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_OUT_OF_MEMORY);
      goto cleanup;
    }

  memset (pkt, 0, pkt_len);

  if ((read_len = ipmi_kcs_read (ctx,
                                 pkt,
                                 pkt_len)) < 0)
    goto cleanup;

  if (!read_len)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_SYSTEM_ERROR);
      goto cleanup;
    }

  if ((ret = unassemble_ipmi_kcs_pkt (pkt,
                                      read_len,
                                      obj_hdr,
                                      obj_cmd_rs,
				      IPMI_INTERFACE_FLAGS_DEFAULT)) < 0)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_INTERNAL_ERROR);
      goto cleanup;
    }

  /* IPMI didn't return enough data back to you */
  if (!ret)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_IPMI_ERROR);
      goto cleanup;
    }

  rv = 0;
 cleanup:
  fiid_template_free (tmpl);
  fiid_obj_destroy (obj_hdr);
  free (pkt);
  return (rv);
}

int
ipmi_kcs_cmd (ipmi_kcs_ctx_t ctx,
              uint8_t lun,
              uint8_t net_fn,
              fiid_obj_t obj_cmd_rq,
              fiid_obj_t obj_cmd_rs)
{
  if (!ctx || ctx->magic != IPMI_KCS_CTX_MAGIC)
    {
      ERR_TRACE (ipmi_kcs_ctx_errormsg (ctx), ipmi_kcs_ctx_errnum (ctx));
      return (-1);
    }

  if (!IPMI_BMC_LUN_VALID (lun)
      || !IPMI_NET_FN_RQ_VALID (net_fn)
      || !fiid_obj_valid (obj_cmd_rq)
      || !fiid_obj_valid (obj_cmd_rs)
      || fiid_obj_packet_valid (obj_cmd_rq) <= 0)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_PARAMETERS);
      return (-1);
    }

  if (!ctx->io_init)
    {
      KCS_SET_ERRNUM (ctx, IPMI_KCS_ERR_IO_NOT_INITIALIZED);
      return (-1);
    }

  if (_ipmi_kcs_cmd_write (ctx, lun, net_fn, obj_cmd_rq) < 0)
    return (-1);

  if (_ipmi_kcs_cmd_read (ctx, obj_cmd_rs) < 0)
    return (-1);

  return (0);
}
