/* 
   ipmi-kcs-interface.c: IPMI - Keyboard Controller Style - SMS Interface

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

#include "freeipmi.h"

#define IPMI_KCS_SLEEP_USECS            0x01

#define IPMI_KCS_HDR_LEN                0x01

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

fiid_template_t tmpl_hdr_kcs =
  {
    {2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

#define IPMI_KCS_CTX_MAGIC 0xabbaadda

static char * ipmi_kcs_ctx_errmsg[] =
  {
    "success",
    "fiid object is null",
    "fiid object is invalid",
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
  uint16_t bmc_iobase_addr;
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

  if (!(ctx = (ipmi_kcs_ctx_t)ipmi_xmalloc(sizeof(struct ipmi_kcs_ctx))))
    {
      errno = ENOMEM;
      goto cleanup;
    }
  ctx->magic = IPMI_KCS_CTX_MAGIC;
  ctx->bmc_iobase_addr = IPMI_KCS_SMS_IO_BASE_DEFAULT;
  ctx->reg_space = 1;
  ctx->poll_interval = IPMI_KCS_SLEEP_USECS;
  ctx->mode = IPMI_KCS_MODE_DEFAULT;
  ctx->io_init = 0;

  if ((ctx->semid = ipmi_mutex_init (IPMI_INBAND_IPCKEY())) < 0)
    goto cleanup;
  
  ctx->errnum = IPMI_KCS_CTX_ERR_SUCCESS;
  return ctx;

 cleanup:
  if (ctx)
    ipmi_xfree(ctx);
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
  ipmi_xfree(ctx);
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
ipmi_kcs_ctx_get_bmc_iobase_addr(ipmi_kcs_ctx_t ctx, uint16_t *bmc_iobase_addr)
{
  if (!(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC))
    return (-1);

  if (!bmc_iobase_addr)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_PARAMETERS;
      return (-1);
    }
  
  *bmc_iobase_addr = ctx->bmc_iobase_addr;
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
ipmi_kcs_ctx_set_bmc_iobase_addr(ipmi_kcs_ctx_t ctx, uint16_t bmc_iobase_addr)
{
  if (!(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC))
    return (-1);

  ctx->bmc_iobase_addr = bmc_iobase_addr;
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
  if (i386_set_ioperm (ctx->bmc_iobase_addr, 0x02, 0x01) != 0)
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

  return _INB (IPMI_KCS_REG_STATUS (ctx->bmc_iobase_addr, ctx->reg_space));
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

  return _INB (IPMI_KCS_REG_DATAOUT (ctx->bmc_iobase_addr));
}

/*
 * Bump channel into sending next byte.
 */
static void
ipmi_kcs_read_next (ipmi_kcs_ctx_t ctx) 
{
  assert(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC);

  _OUTB (IPMI_KCS_CTRL_READ, IPMI_KCS_REG_DATAIN (ctx->bmc_iobase_addr));
}
/*
 * Set up channel for writing.
 */
static void
ipmi_kcs_start_write (ipmi_kcs_ctx_t ctx)
{
  assert(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC);

  _OUTB (IPMI_KCS_CTRL_WRITE_START, IPMI_KCS_REG_CMD (ctx->bmc_iobase_addr, ctx->reg_space));
}

/*
 * Write byte to inound data port.
 */
static void
ipmi_kcs_write_byte (ipmi_kcs_ctx_t ctx, uint8_t byte)
{
  assert(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC);

  _OUTB (byte, IPMI_KCS_REG_DATAIN (ctx->bmc_iobase_addr));
}

/* 
 * Set up channel to end write.
 */
static void
ipmi_kcs_end_write (ipmi_kcs_ctx_t ctx)
{
  assert(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC);

  _OUTB (IPMI_KCS_CTRL_WRITE_END, IPMI_KCS_REG_CMD (ctx->bmc_iobase_addr, ctx->reg_space));
}

#if 0
/* 
 * Send Abort current processing command.
 */
static void
ipmi_kcs_get_abort (ipmi_kcs_ctx_t ctx)
{
  assert(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC);

  _OUTB (IPMI_KCS_CTRL_GET_ABORT, IPMI_KCS_REG_CMD (ctx->bmc_iobase_addr, ctx->reg_space));
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

uint8_t
ipmi_kcs_print_state (int fd, uint8_t state)
{
  /* we assume we have already ioperm'd the space */
  _dprintf (fd, "Current KCS state: 0x%x : ", state);
  if ((state & IPMI_KCS_STATUS_REG_STATE) == IPMI_KCS_STATE_IDLE) {
    _dprintf (fd, "IDLE_STATE ");
  } else if ((state & IPMI_KCS_STATUS_REG_STATE) == IPMI_KCS_STATE_READ) {
    _dprintf (fd, "READ_STATE ");
  } else if ((state & IPMI_KCS_STATUS_REG_STATE) == IPMI_KCS_STATE_WRITE) {
    _dprintf (fd, "WRITE_STATE ");
  } else if ((state & IPMI_KCS_STATUS_REG_STATE) == IPMI_KCS_STATE_ERROR) {
    _dprintf (fd, "ERROR_STATE ");
  } else {
    _dprintf (fd, "UNKNOWN_STATE "); /* cannot happen */
  }
  if (state & IPMI_KCS_STATUS_REG_IBF) {
    _dprintf (fd, "IBF ");
  }
  if (state & IPMI_KCS_STATUS_REG_OBF) {
    _dprintf (fd, "OBF ");
  }
  if (state & IPMI_KCS_STATUS_REG_OEM1) {
    _dprintf (fd, "OEM1 ");
  }
  if (state & IPMI_KCS_STATUS_REG_OEM2) {
    _dprintf (fd, "OEM2 ");
  }
  _dprintf (fd, "\n");
  return (0);
}

/*
 * Standard write loop. 
 */
int32_t
ipmi_kcs_write (ipmi_kcs_ctx_t ctx, 
		uint8_t *buf, 
		uint32_t buf_len)
{
  uint8_t *p=buf;
  int32_t count = 0;

  if (!(ctx && ctx->magic == IPMI_KCS_CTX_MAGIC))
    goto cleanup;
  
  if (!buf || !buf_len)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_PARAMETERS;
      goto cleanup;
    }
  
  if (!ctx->io_init)
    {
      ctx->errnum = IPMI_KCS_CTX_ERR_IO_INIT;
      goto cleanup;
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

int8_t
fill_hdr_ipmi_kcs (uint8_t lun, 
		   uint8_t fn, 
		   fiid_obj_t obj_hdr)
{
  int8_t rv;

  if (!IPMI_BMC_LUN_VALID(lun)
      || !IPMI_NET_FN_VALID(fn)
      || !fiid_obj_valid(obj_hdr))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_hdr, tmpl_hdr_kcs)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_hdr, (uint8_t *)"lun", lun);
  FIID_OBJ_SET (obj_hdr, (uint8_t *)"net_fn", fn);
  return 0;
}

int32_t 
assemble_ipmi_kcs_pkt (fiid_obj_t obj_hdr, 
		       fiid_obj_t obj_cmd, 
		       uint8_t *pkt, 
		       uint32_t pkt_len)
{
  int32_t obj_cmd_len, obj_hdr_len;
  int8_t rv;

  if (!(fiid_obj_valid(obj_hdr)
        && fiid_obj_valid(obj_cmd)
        && pkt))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_hdr, tmpl_hdr_kcs)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_packet_valid(obj_hdr)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_packet_valid(obj_cmd)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  obj_hdr_len = fiid_obj_len_bytes (obj_hdr);
  ERR(obj_hdr_len != -1);
  obj_cmd_len = fiid_obj_len_bytes (obj_cmd);
  ERR(obj_cmd_len != -1);

  if (pkt_len < (obj_hdr_len + obj_cmd_len))
    {
      errno = EMSGSIZE;
      return (-1);
    }

  memset (pkt, 0, pkt_len);
  ERR((obj_hdr_len = fiid_obj_get_all(obj_hdr, pkt, pkt_len)) != -1);
  ERR((obj_cmd_len = fiid_obj_get_all(obj_cmd, pkt + obj_hdr_len, pkt_len - obj_hdr_len)) != -1);
  return (obj_hdr_len + obj_cmd_len);
}

int32_t 
unassemble_ipmi_kcs_pkt (uint8_t *pkt, 
			 uint32_t pkt_len, 
			 fiid_obj_t obj_hdr, 
			 fiid_obj_t obj_cmd)
{
  uint32_t indx = 0;
  int32_t len;
  int8_t rv;

  if (!(pkt
        && fiid_obj_valid(obj_hdr)
        && fiid_obj_valid(obj_cmd)))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_hdr, tmpl_hdr_kcs)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

  ERR(!((len = fiid_obj_set_all(obj_hdr, pkt + indx, pkt_len - indx)) < 0));
  indx += len;

  if (pkt_len <= indx)
    return 0;

  ERR(!((len = fiid_obj_set_all(obj_cmd, pkt + indx, pkt_len - indx)) < 0));
  indx += len;

  return 0;
}
