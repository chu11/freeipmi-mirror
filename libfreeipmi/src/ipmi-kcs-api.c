/* 
   ipmi-kcs-api.c: IPMI - Keyboard Controller Style - SMS Api

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
#include <assert.h>
#include <errno.h>

#include "freeipmi/ipmi-kcs-api.h"
#include "freeipmi/ipmi-kcs.h"

#include "ipmi-inband.h"
#include "ipmi-semaphores.h"

#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"
#include "ipmi-common.h"
#include "xmalloc.h"

#define IPMI_KCS_SLEEP_USECS                0x01

#define IPMI_KCS_SMS_REGISTER_SPACE_DEFAULT 1
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

#define IPMI_KCS_STATUS_NO_ERROR_STR \
"No error"
          
#define IPMI_KCS_STATUS_ABORTED_BY_CMD         0x01
#define IPMI_KCS_STATUS_ABORTED_BY_CMD_STR \
"Aborted by command (Transfer in progress was " \
"aborted by SMS issuing the Abort/Status control code)"

#define IPMI_KCS_STATUS_ILLEGAL_CTRL_CODE      0x02
#define IPMI_KCS_STATUS_ILLEGAL_CTRL_CODE_STR \
"Illegal control code"

#define IPMI_KCS_STATUS_LEN_ERROR              0x06
#define IPMI_KCS_STATUS_LEN_ERROR_STR \
"Length error (e.g.overrun)"

#define IPMI_KCS_STATUS_OEM_ERROR_BEGIN        0xC0
#define IPMI_KCS_STATUS_OEM_ERROR_END          0xFE

#define IPMI_KCS_STATUS_UNSPECIFIED_ERROR      0xFF
#define IPMI_KCS_STATUS_UNSPECIFIED_ERROR_STR \
"Unspecified error"

/* Reserved - all others */

/* IPMI KCS SMS Interface Registers */
#define IPMI_KCS_REG_DATAIN(sms_io_base)   (sms_io_base)
#define IPMI_KCS_REG_DATAOUT(sms_io_base)  (sms_io_base)
#define IPMI_KCS_REG_CMD(sms_io_base, reg_space)     \
       (sms_io_base + reg_space)
#define IPMI_KCS_REG_STATUS(sms_io_base, reg_space)  \
       (sms_io_base + reg_space)

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

static char * ipmi_kcs_ctx_errmsg[] =
  {
    "success",
    "kcs context is null",
    "kcs context is invalid",
    "invalid parameter",
    "permission denied",
    "invalid io parameter",
    "io not initialized",
    "buffer too small to hold result",
    "BMC busy",
    "out of memory",
    "internal error",
    "error number out of range",
    NULL,
  };

struct ipmi_kcs_ctx {
  uint32_t magic;
  int32_t errnum;
  uint16_t bmc_iobase_address;
  uint8_t reg_space;
  uint8_t mode;
  uint32_t poll_interval;
#ifdef __FreeBSD__
#ifndef USE_IOPERM
  int dev_fd;
#endif
#endif /* __FreeBSD__ */
  int io_init;
  int semid;
};

ipmi_kcs_ctx_t
ipmi_kcs_ctx_create(void)
{
  ipmi_kcs_ctx_t ctx = NULL;

  ERR_CLEANUP ((ctx = (ipmi_kcs_ctx_t)xmalloc(sizeof(struct ipmi_kcs_ctx))));

  ctx->magic = IPMI_KCS_CTX_MAGIC;
  ctx->bmc_iobase_address = IPMI_KCS_SMS_IO_BASE_DEFAULT;
  ctx->reg_space = IPMI_KCS_SMS_REGISTER_SPACE_DEFAULT;
  ctx->poll_interval = IPMI_KCS_SLEEP_USECS;
  ctx->mode = IPMI_KCS_MODE_DEFAULT;
  ctx->io_init = 0;

  ERR_CLEANUP (!((ctx->semid = ipmi_mutex_init (IPMI_INBAND_IPCKEY())) < 0));
  
  ctx->errnum = IPMI_KCS_CTX_ERR_SUCCESS;
  return ctx;

 cleanup:
  if (ctx)
    xfree(ctx);
  return (NULL);
}

int8_t
ipmi_kcs_ctx_destroy(ipmi_kcs_ctx_t ctx)
{
  if (!(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC))
    return (-1);

  ctx->magic = ~IPMI_KCS_CTX_MAGIC;
  ctx->errnum = IPMI_KCS_CTX_ERR_SUCCESS;
#ifdef __FreeBSD__
#ifndef USE_IOPERM
  close(ctx->dev_fd);
#endif
#endif /* __FreeBSD__ */
  xfree(ctx);
  return (0);
}

char *
ipmi_kcs_ctx_strerror(int32_t errnum)
{
  if (errnum >= IPMI_KCS_CTX_ERR_SUCCESS && errnum <= IPMI_KCS_CTX_ERR_ERRNUMRANGE)
    return ipmi_kcs_ctx_errmsg[errnum];
  else
    return ipmi_kcs_ctx_errmsg[IPMI_KCS_CTX_ERR_ERRNUMRANGE];
}

int32_t
ipmi_kcs_ctx_errnum(ipmi_kcs_ctx_t ctx)
{
  if (!ctx)
    return (IPMI_KCS_CTX_ERR_NULL);
  else if (ctx->magic != IPMI_KCS_CTX_MAGIC)
    return (IPMI_KCS_CTX_ERR_INVALID);
  else
    return (ctx->errnum);
}

int8_t 
ipmi_kcs_ctx_get_bmc_iobase_address(ipmi_kcs_ctx_t ctx, uint16_t *bmc_iobase_address)
{
  if (!(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC))
    return (-1);

  if (!bmc_iobase_address)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_PARAMETERS;
      return (-1);
    }
  
  *bmc_iobase_address = ctx->bmc_iobase_address;
  ctx->errnum = IPMI_KCS_CTX_ERR_SUCCESS;
  return (0);
}

int8_t 
ipmi_kcs_ctx_get_register_space(ipmi_kcs_ctx_t ctx, uint8_t *reg_space)
{
  if (!(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC))
    return (-1);

  if (!reg_space)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_PARAMETERS;
      return (-1);
    }
  
  *reg_space = ctx->reg_space;
  ctx->errnum = IPMI_KCS_CTX_ERR_SUCCESS;
  return (0);
}

int8_t 
ipmi_kcs_ctx_get_poll_interval(ipmi_kcs_ctx_t ctx, uint8_t *poll_interval)
{
  if (!(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC))
    return (-1);

  if (!poll_interval)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_PARAMETERS;
      return (-1);
    }

  *poll_interval = ctx->poll_interval;
  ctx->errnum = IPMI_KCS_CTX_ERR_SUCCESS;
  return (0);
}

int8_t 
ipmi_kcs_ctx_get_mode(ipmi_kcs_ctx_t ctx, uint8_t *mode)
{
  if (!(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC))
    return (-1);

  if (!mode)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_PARAMETERS;
      return (-1);
    }

  *mode = ctx->mode;
  ctx->errnum = IPMI_KCS_CTX_ERR_SUCCESS;
  return (0);
}

int8_t 
ipmi_kcs_ctx_set_bmc_iobase_address(ipmi_kcs_ctx_t ctx, uint16_t bmc_iobase_address)
{
  if (!(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC))
    return (-1);

  ctx->bmc_iobase_address = bmc_iobase_address;
  ctx->errnum = IPMI_KCS_CTX_ERR_SUCCESS;
  return (0);
}

int8_t 
ipmi_kcs_ctx_set_register_space(ipmi_kcs_ctx_t ctx, uint8_t reg_space)
{
  if (!(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC))
    return (-1);

  ctx->reg_space = reg_space;
  ctx->errnum = IPMI_KCS_CTX_ERR_SUCCESS;
  return (0);
}

int8_t 
ipmi_kcs_ctx_set_poll_interval(ipmi_kcs_ctx_t ctx, uint8_t poll_interval)
{
  if (!(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC))
    return (-1);

  ctx->poll_interval = poll_interval;
  ctx->errnum = IPMI_KCS_CTX_ERR_SUCCESS;
  return (0);
}

int8_t 
ipmi_kcs_ctx_set_mode(ipmi_kcs_ctx_t ctx, uint8_t mode)
{
  if (!(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC))
    return (-1);

  if (!(mode == IPMI_KCS_MODE_BLOCKING
        || mode == IPMI_KCS_MODE_NONBLOCKING))
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_PARAMETERS;
      return (-1);
    }
  
  ctx->mode = mode;
  ctx->errnum = IPMI_KCS_CTX_ERR_SUCCESS;
  return (0);
}

int8_t
ipmi_kcs_ctx_io_init(ipmi_kcs_ctx_t ctx)
{
  if (!(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC))
    return (-1);

#ifdef __FreeBSD__
#ifdef USE_IOPERM
  /* i386_set_ioperm has known problems on FBSD 5.x (bus errors). */
  if (i386_set_ioperm (ctx->bmc_iobase_address, 0x02, 0x01) != 0)
    {
      if (errno == EPERM || errno == EACCES)
        ctx->errnum = IPMI_KCS_CTX_ERR_PERMISSION;
      else
        ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL;
      return (-1);
    }
#else  /* !USE_IOPERM */
  /* Opening /dev/io raises IOPL bits for current process. */
  ctx->dev_fd = open ("/dev/io", O_RDONLY);
  if (ctx->dev_fd < 0)
    {
      if (errno == EPERM || errno == EACCES)
        ctx->errnum = IPMI_KCS_CTX_ERR_PERMISSION;
      else
        ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL;
      return (-1);
    }
#endif /* !USE_IOPERM */
#else  /* !__FreeBSD__ */
  if (iopl (3) < 0)
    {
      if (errno == EPERM || errno == EACCES)
        ctx->errnum = IPMI_KCS_CTX_ERR_PERMISSION;
      else
        ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL;
      return (-1);
    }
#endif/* !__FreeBSD__ */

  ctx->io_init = 1;
  ctx->errnum = IPMI_KCS_CTX_ERR_SUCCESS;
  return (0);
}

static int8_t
ipmi_kcs_get_status (ipmi_kcs_ctx_t ctx)
{
  assert(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC);

  return _INB (IPMI_KCS_REG_STATUS (ctx->bmc_iobase_address, ctx->reg_space));
}

/*
 * Wait for IBF (In-Bound Flag) to clear, signalling BMC has
 * read the command. 
 */
static void
ipmi_kcs_wait_for_ibf_clear (ipmi_kcs_ctx_t ctx)
{
  assert(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC);

  while (ipmi_kcs_get_status (ctx) & IPMI_KCS_STATUS_REG_IBF)
    usleep (ctx->poll_interval);
}

/* 
 * Wait for OBF to raise, signalling data is pending to read
 * or no command is pending.
 */

static void
ipmi_kcs_wait_for_obf_set (ipmi_kcs_ctx_t ctx)
{
  assert(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC);

  while (!(ipmi_kcs_get_status (ctx) & IPMI_KCS_STATUS_REG_OBF))
    usleep (ctx->poll_interval);
}

/*
 * Read byte from outbound data port. 
 */
static int8_t
ipmi_kcs_read_byte (ipmi_kcs_ctx_t ctx)
{
  assert(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC);

  return _INB (IPMI_KCS_REG_DATAOUT (ctx->bmc_iobase_address));
}

/*
 * Bump channel into sending next byte.
 */
static void
ipmi_kcs_read_next (ipmi_kcs_ctx_t ctx) 
{
  assert(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC);

  _OUTB (IPMI_KCS_CTRL_READ, IPMI_KCS_REG_DATAIN (ctx->bmc_iobase_address));
}
/*
 * Set up channel for writing.
 */
static void
ipmi_kcs_start_write (ipmi_kcs_ctx_t ctx)
{
  assert(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC);

  _OUTB (IPMI_KCS_CTRL_WRITE_START, IPMI_KCS_REG_CMD (ctx->bmc_iobase_address, ctx->reg_space));
}

/*
 * Write byte to inound data port.
 */
static void
ipmi_kcs_write_byte (ipmi_kcs_ctx_t ctx, uint8_t byte)
{
  assert(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC);

  _OUTB (byte, IPMI_KCS_REG_DATAIN (ctx->bmc_iobase_address));
}

/* 
 * Set up channel to end write.
 */
static void
ipmi_kcs_end_write (ipmi_kcs_ctx_t ctx)
{
  assert(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC);

  _OUTB (IPMI_KCS_CTRL_WRITE_END, IPMI_KCS_REG_CMD (ctx->bmc_iobase_address, ctx->reg_space));
}

#if 0
/* 
 * Send Abort current processing command.
 */
static void
ipmi_kcs_get_abort (ipmi_kcs_ctx_t ctx)
{
  assert(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC);

  _OUTB (IPMI_KCS_CTRL_GET_ABORT, IPMI_KCS_REG_CMD (ctx->bmc_iobase_address, ctx->reg_space));
}
#endif

static int8_t
ipmi_kcs_test_if_state (ipmi_kcs_ctx_t ctx, uint8_t status)
{
  assert(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC);

  if ((ipmi_kcs_get_status (ctx) & IPMI_KCS_STATUS_REG_STATE) == 
      (status & IPMI_KCS_STATUS_REG_STATE))
    return 1;
  else
    return 0;
}

/*
 * Read dummy byte to clear OBF if set.
 */
static void
ipmi_kcs_clear_obf (ipmi_kcs_ctx_t ctx)
{
  assert(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC);

  if (ipmi_kcs_get_status (ctx) & IPMI_KCS_STATUS_REG_OBF) 
    ipmi_kcs_read_byte (ctx);
}

#if 0
static uint8_t
ipmi_kcs_print_state (int fd, uint8_t state)
{
  /* we assume we have already ioperm'd the space */
  ipmi_dprintf (fd, "Current KCS state: 0x%x : ", state);
  if ((state & IPMI_KCS_STATUS_REG_STATE) == IPMI_KCS_STATE_IDLE) {
    ipmi_dprintf (fd, "IDLE_STATE ");
  } else if ((state & IPMI_KCS_STATUS_REG_STATE) == IPMI_KCS_STATE_READ) {
    ipmi_dprintf (fd, "READ_STATE ");
  } else if ((state & IPMI_KCS_STATUS_REG_STATE) == IPMI_KCS_STATE_WRITE) {
    ipmi_dprintf (fd, "WRITE_STATE ");
  } else if ((state & IPMI_KCS_STATUS_REG_STATE) == IPMI_KCS_STATE_ERROR) {
    ipmi_dprintf (fd, "ERROR_STATE ");
  } else {
    ipmi_dprintf (fd, "UNKNOWN_STATE "); /* cannot happen */
  }
  if (state & IPMI_KCS_STATUS_REG_IBF) {
    ipmi_dprintf (fd, "IBF ");
  }
  if (state & IPMI_KCS_STATUS_REG_OBF) {
    ipmi_dprintf (fd, "OBF ");
  }
  if (state & IPMI_KCS_STATUS_REG_OEM1) {
    ipmi_dprintf (fd, "OEM1 ");
  }
  if (state & IPMI_KCS_STATUS_REG_OEM2) {
    ipmi_dprintf (fd, "OEM2 ");
  }
  ipmi_dprintf (fd, "\n");
  return (0);
}

int8_t 
ipmi_kcs_strstatus_r (uint8_t status_code, 
		      char *errstr, 
		      size_t len)
{
  ERR_EINVAL (errstr);
  
  switch (status_code)
    {
    case IPMI_KCS_STATUS_NO_ERROR:
      SNPRINTF_RETURN (IPMI_KCS_STATUS_NO_ERROR_STR);
      
    case IPMI_KCS_STATUS_ABORTED_BY_CMD:
      SNPRINTF_RETURN (IPMI_KCS_STATUS_ABORTED_BY_CMD_STR);
      
    case IPMI_KCS_STATUS_ILLEGAL_CTRL_CODE:
      SNPRINTF_RETURN (IPMI_KCS_STATUS_ILLEGAL_CTRL_CODE_STR);
      
    case IPMI_KCS_STATUS_LEN_ERROR:
      SNPRINTF_RETURN (IPMI_KCS_STATUS_LEN_ERROR_STR); 
      
    case IPMI_KCS_STATUS_UNSPECIFIED_ERROR:
      SNPRINTF_RETURN (IPMI_KCS_STATUS_UNSPECIFIED_ERROR_STR); 
    }
  
  if ((status_code >= IPMI_KCS_STATUS_OEM_ERROR_BEGIN) &&
      (status_code <= IPMI_KCS_STATUS_OEM_ERROR_END))
    {
      SNPRINTF_RETURN ("OEM status code %02Xh.", status_code);
    }
  
  SNPRINTF_RETURN ("Unknown KCS interface status code %02Xh.", status_code);
};

#endif /* 0 */

int32_t
ipmi_kcs_write (ipmi_kcs_ctx_t ctx, 
		uint8_t *buf, 
		uint32_t buf_len)
{
  uint8_t *p=buf;
  int32_t count = 0;

  if (!(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC))
    return (-1); 
 
  if (!buf || !buf_len)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_PARAMETERS;
      return (-1); 
    }
  
  if (!ctx->io_init)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_IO_INIT;
      return (-1); 
    }

  if (ctx->mode == IPMI_KCS_MODE_BLOCKING)
    IPMI_MUTEX_LOCK(ctx->semid);
  else
    {
      int ret;
      
      if ((ret = IPMI_MUTEX_LOCK_INTERRUPTIBLE(ctx->semid)) < 0)
        {
          if (errno == EINTR || errno == EAGAIN)
            ctx->errnum = IPMI_KCS_CTX_ERR_BUSY;
          else
            ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL;
          goto cleanup;
        }
    }
  
  ipmi_kcs_wait_for_ibf_clear (ctx);
  ipmi_kcs_clear_obf (ctx);
  ipmi_kcs_start_write (ctx);
  ipmi_kcs_wait_for_ibf_clear (ctx);
  if (!ipmi_kcs_test_if_state (ctx, IPMI_KCS_STATE_WRITE))
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_BUSY;
      goto cleanup_unlock;
    }
  ipmi_kcs_clear_obf (ctx);

  /* note we have to save last byte. */
  /* for (buf=data; data+len-1 < buf; buf++) */
  for (; buf_len > 1; buf_len--)
    {
      ipmi_kcs_write_byte (ctx, *p);
      ipmi_kcs_wait_for_ibf_clear (ctx);
      if (!ipmi_kcs_test_if_state (ctx, IPMI_KCS_STATE_WRITE))
        {
          ctx->errnum = IPMI_KCS_CTX_ERR_BUSY;
          goto cleanup_unlock;
        }
      ipmi_kcs_clear_obf (ctx);
      p++;
      count++;
    }
  ipmi_kcs_end_write (ctx);
  ipmi_kcs_wait_for_ibf_clear (ctx);
  if (!ipmi_kcs_test_if_state (ctx, IPMI_KCS_STATE_WRITE))
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_BUSY;
      goto cleanup_unlock;
    }
  ipmi_kcs_clear_obf (ctx);
  ipmi_kcs_write_byte (ctx, *p);
  count++;

#if 0
  if (!ipmi_kcs_test_if_state (IPMI_KCS_STATE_READ)) {
    printf ("Not in READ state after writing last byte?\n");
    ipmi_kcs_print_state (ipmi_kcs_get_state ());
    exit (1);
  }
#endif

  ctx->errnum = IPMI_KCS_CTX_ERR_SUCCESS;
  return (count);

 cleanup_unlock:
  IPMI_MUTEX_UNLOCK (ctx->semid);
 cleanup:
  return (-1);
}

/* 
 * Main read loop.
 */
int32_t
ipmi_kcs_read (ipmi_kcs_ctx_t ctx, 
	       uint8_t* buf, 
	       uint32_t buf_len)
{
  uint8_t *p = buf;
  int32_t count = 0;
  int32_t rv = -1;

  if (!(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC))
    goto cleanup_unlock;
  
  if (!buf || !buf_len)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_PARAMETERS;
      goto cleanup_unlock;
    }
  
  if (!ctx->io_init)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_IO_INIT;
      goto cleanup_unlock;
    }

  ipmi_kcs_wait_for_ibf_clear (ctx);
  if (!ipmi_kcs_test_if_state (ctx, IPMI_KCS_STATE_READ)) 
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_BUSY;
      goto cleanup_unlock;
    }
  while (ipmi_kcs_test_if_state (ctx, IPMI_KCS_STATE_READ))
    {
      char c;
      ipmi_kcs_wait_for_obf_set (ctx);
      c = ipmi_kcs_read_byte (ctx);
      if (count < buf_len)
	{
	  *(p++) = c;
	  count++;
	}
      ipmi_kcs_read_next (ctx);
      ipmi_kcs_wait_for_ibf_clear (ctx);
    }
  if (ipmi_kcs_test_if_state (ctx, IPMI_KCS_STATE_IDLE))
    {
      /* Clean up */
      ipmi_kcs_wait_for_obf_set (ctx);
      ipmi_kcs_read_byte (ctx); /* toss it, ACK */
    }
  else
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_BUSY;
      goto cleanup_unlock;
    }

  if (count >= buf_len)
    ctx->errnum = IPMI_KCS_CTX_ERR_OVERFLOW;
  else
    ctx->errnum = IPMI_KCS_CTX_ERR_SUCCESS;
  rv = count;
 cleanup_unlock:
  IPMI_MUTEX_UNLOCK (ctx->semid);
  return (rv);
}

static int8_t
_ipmi_kcs_cmd_write(ipmi_kcs_ctx_t ctx, 
		    uint8_t lun,
		    uint8_t net_fn,
		    fiid_obj_t obj_cmd_rq)
{
  uint8_t *pkt = NULL;
  uint32_t pkt_len;
  int32_t hdr_len, cmd_len;
  fiid_obj_t obj_hdr = NULL;
  int rv = -1;

  assert(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC);
  assert(IPMI_BMC_LUN_VALID(lun));
  assert(IPMI_NET_FN_RQ_VALID(net_fn));
  assert(fiid_obj_valid(obj_cmd_rq));
  assert(fiid_obj_packet_valid(obj_cmd_rq));

  if ((hdr_len = fiid_template_len_bytes(tmpl_hdr_kcs)) < 0)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL;
      return (-1);
    }
  
  if ((cmd_len = fiid_obj_len_bytes(obj_cmd_rq)) < 0)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL;
      return (-1);
    }
  
  if (!(obj_hdr = fiid_obj_create(tmpl_hdr_kcs)))
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_OUTMEM;
      goto cleanup;
    }
  
  pkt_len = hdr_len + cmd_len;
  if (!(pkt = (uint8_t *)malloc (pkt_len)))
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_OUTMEM;
      goto cleanup;
    }
  memset (pkt, 0, pkt_len);
    
  if (fill_hdr_ipmi_kcs (lun,
			 net_fn,
			 obj_hdr) < 0)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL;
      return (-1);
    }
  
  if (assemble_ipmi_kcs_pkt (obj_hdr,
			     obj_cmd_rq,
			     pkt,
			     pkt_len) < 0)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL;
      return (-1);
    }
  
  if (ipmi_kcs_write (ctx, pkt, pkt_len) < 0)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL;
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
_ipmi_kcs_cmd_read(ipmi_kcs_ctx_t ctx, 
		   fiid_obj_t obj_cmd_rs)
{
  uint8_t *pkt;
  uint32_t pkt_len;
  int32_t hdr_len, cmd_len;
  int32_t read_len;
  fiid_obj_t obj_hdr = NULL;
  fiid_field_t *tmpl = NULL;
  int8_t rv = -1;

  assert(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC);
  assert(fiid_obj_valid(obj_cmd_rs));

  if ((hdr_len = fiid_template_len_bytes(tmpl_hdr_kcs)) < 0)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL;
      return -1;
    }
  
  if (!(tmpl = fiid_obj_template(obj_cmd_rs)) < 0)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL;
      goto cleanup;
    }

  if ((cmd_len = fiid_template_len_bytes(tmpl)) < 0)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL;
      goto cleanup;
    }

  if (!(obj_hdr = fiid_obj_create(tmpl_hdr_kcs)))
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_OUTMEM;
      goto cleanup;
    }

  pkt_len = hdr_len + cmd_len;
  
  if (!(pkt = (uint8_t *)malloc(pkt_len)))
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_OUTMEM;
      goto cleanup;
    }
  memset (pkt, 0, pkt_len);

  if ((read_len = ipmi_kcs_read (ctx, 
				 pkt,
				 pkt_len)) < 0)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL;
      goto cleanup;
    }
  
  if (unassemble_ipmi_kcs_pkt (pkt,
			       read_len,
			       obj_hdr,
			       obj_cmd_rs) < 0)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_INTERNAL;
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
ipmi_kcs_cmd (ipmi_kcs_ctx_t ctx, 
	      uint8_t lun,
	      uint8_t net_fn,
              fiid_obj_t obj_cmd_rq,
              fiid_obj_t obj_cmd_rs)
{
  if (!(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC))
    return (-1); 
 
  if (!IPMI_BMC_LUN_VALID(lun)
      || !IPMI_NET_FN_RQ_VALID(net_fn)
      || !fiid_obj_valid(obj_cmd_rq)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_PARAMETERS;
      return (-1); 
    }
  
  if (!ctx->io_init)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_IO_INIT;
      return (-1); 
    }

  if (fiid_obj_packet_valid(obj_cmd_rq))
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_PARAMETERS;
      return (-1); 
    }
 
  if (_ipmi_kcs_cmd_write(ctx, lun, net_fn, obj_cmd_rq) < 0)
    return -1;

  if (_ipmi_kcs_cmd_read(ctx, obj_cmd_rs) < 0)
    return -1;

  return (0);
}
