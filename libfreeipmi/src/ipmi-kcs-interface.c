/* 
   ipmi-kcs-interface.c: IPMI - Keyboard Controller Style - SMS Interface

   Copyright (C) 2003-2004 FreeIPMI Core Team

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>

#ifdef STDC_HEADERS
#include <string.h>
#else
# include <sys/types.h>
# ifndef HAVE_MEMCPY
static void*
memcpy (void *dest, const void *src, size_t n)
{
  while (0 <= --n) ((unsigned char*)dest) [n] = ((unsigned char*)src) [n];
  return dest;
}
# endif
# ifndef HAVE_MEMSET
static void*
memset (void *s, int c, size_t n)
{
  while (0 <= --n) ((unsigned char*)s) [n] = (unsigned char) c;
  return s;
}
# endif
#endif

#include <errno.h>
#include "freeipmi.h"

#if defined(__FreeBSD__)
#define _INB(port)  inb (port)
#define _OUTB(data, port)  outb (port, data)
#else
#define _INB(port)  inb (port)
#define _OUTB(data, port)  outb (data, port)
#endif

static u_int64_t kcs_poll_count;
static unsigned long kcs_sleep_usecs = IPMI_KCS_SLEEP_USECS;
static int kcs_mutex_semid;

fiid_template_t tmpl_hdr_kcs =
  {
    {2, "lun"},
    {6, "net_fn"},
    {0, ""}
  };

u_int64_t 
ipmi_kcs_get_poll_count (void)
{
  return (kcs_poll_count);
}

int
ipmi_kcs_get_mutex_semid (void)
{
  return (kcs_mutex_semid);
}

int
ipmi_kcs_io_init (u_int16_t sms_io_base, unsigned long sleep_usecs)
{
  kcs_sleep_usecs = sleep_usecs;
  
  kcs_mutex_semid = ipmi_mutex_init (IPMI_KCS_IPCKEY ());
  ERR (kcs_mutex_semid != -1);

#if defined(__FreeBSD__)
  return (ioperm (sms_io_base, 0x02, 0x01));
#else
  return (iopl (3));
#endif
}

int8_t
fill_hdr_ipmi_kcs (u_int8_t lun, u_int8_t fn, fiid_obj_t obj_hdr)
{
  if ((lun > IPMI_BMC_IPMB_LUN_OEM_LUN2) ||
      (fn > IPMI_NET_FN_TRANSPORT_RS)    ||
      (obj_hdr == NULL))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_hdr, tmpl_hdr_kcs, "lun", lun);
  FIID_OBJ_SET (obj_hdr, tmpl_hdr_kcs, "net_fn", fn);
  return 0;
}

int8_t
assemble_ipmi_kcs_pkt (fiid_obj_t obj_hdr, fiid_obj_t obj_cmd, fiid_template_t tmpl_cmd, u_int8_t *pkt, u_int32_t pkt_len)
{
  u_int32_t obj_cmd_len, obj_hdr_len;
  if (!(obj_hdr && obj_cmd && tmpl_cmd && pkt))
    {
      errno = EINVAL;
      return -1;
    }
  obj_hdr_len = fiid_obj_len_bytes (tmpl_hdr_kcs);
  obj_cmd_len = fiid_obj_len_bytes (tmpl_cmd);
  if (pkt_len < (obj_hdr_len + obj_cmd_len))
    {
      errno = EMSGSIZE;
      return -1;
    }
  
  memset (pkt, 0, obj_hdr_len + obj_cmd_len);
  memcpy (pkt, obj_hdr, obj_hdr_len);
  memcpy (pkt + obj_hdr_len, obj_cmd, obj_cmd_len);
  return (obj_hdr_len + obj_cmd_len);
}

int8_t
unassemble_ipmi_kcs_pkt (u_int8_t *pkt, u_int32_t pkt_len, fiid_obj_t obj_hdr, fiid_obj_t obj_cmd, fiid_template_t tmpl_cmd)
{
  u_int32_t indx = 0;

  if (pkt == NULL)
    {
      errno = EINVAL;
      return -1;
    }

  indx = 0;
  if (obj_hdr)
    memcpy (obj_hdr, pkt + indx, FREEIPMI_MIN (pkt_len - indx, fiid_obj_len_bytes (tmpl_hdr_kcs)));
  indx += fiid_obj_len_bytes (tmpl_hdr_kcs);

  if (pkt_len <= indx)
    return 0;

  if (obj_cmd)
    {
      if (tmpl_cmd)
        {
          memcpy (obj_cmd, pkt + indx, FREEIPMI_MIN (pkt_len - indx, fiid_obj_len_bytes (tmpl_cmd)));
          indx += fiid_obj_len_bytes (tmpl_cmd);
        }
      else
	{
	  errno = EINVAL;
	  return -1;
 	}
    }

  return 0;
}

int8_t
ipmi_kcs_get_status (u_int16_t sms_io_base)
{
  return _INB (IPMI_KCS_REG_STATUS (sms_io_base));
}

/*
 * Wait for IBF (In-Bound Flag) to clear, signalling BMC has
 * read the command. 
 */
void
ipmi_kcs_wait_for_ibf_clear (u_int16_t sms_io_base)
{
  while (ipmi_kcs_get_status (sms_io_base) & IPMI_KCS_STATUS_REG_IBF)
    {
      usleep (kcs_sleep_usecs);
      kcs_poll_count++;
    }
}

/* 
 * Wait for OBF to raise, signalling data is pending to read
 * or no command is pending.
 */

void
ipmi_kcs_wait_for_obf_set (u_int16_t sms_io_base)
{
  while (!(ipmi_kcs_get_status (sms_io_base) & IPMI_KCS_STATUS_REG_OBF))
    {
      usleep (kcs_sleep_usecs);
      kcs_poll_count++;
    }
}

/*
 * Read byte from outbound data port. 
 */
int8_t
ipmi_kcs_read_byte (u_int16_t sms_io_base)
{
  return _INB (IPMI_KCS_REG_DATAOUT (sms_io_base));
}

/*
 * Bump channel into sending next byte.
 */
void
ipmi_kcs_read_next (u_int16_t sms_io_base) 
{
  _OUTB (IPMI_KCS_CTRL_READ, IPMI_KCS_REG_DATAIN (sms_io_base));
}
/*
 * Set up channel for writing.
 */
void
ipmi_kcs_start_write (u_int16_t sms_io_base)
{
  _OUTB (IPMI_KCS_CTRL_WRITE_START, IPMI_KCS_REG_CMD (sms_io_base));
}

/*
 * Write byte to inound data port.
 */
void
ipmi_kcs_write_byte (u_int16_t sms_io_base, u_int8_t byte)
{
  _OUTB (byte, IPMI_KCS_REG_DATAIN (sms_io_base));
}

/* 
 * Set up channel to end write.
 */
void
ipmi_kcs_end_write (u_int16_t sms_io_base)
{
  _OUTB (IPMI_KCS_CTRL_WRITE_END, IPMI_KCS_REG_CMD (sms_io_base));
}

/* 
 * Send Abort current processing command.
 */
void
ipmi_kcs_get_abort (u_int16_t sms_io_base)
{
  _OUTB (IPMI_KCS_CTRL_GET_ABORT, IPMI_KCS_REG_CMD (sms_io_base));
}

int8_t
ipmi_kcs_test_if_state (u_int16_t sms_io_base, u_int8_t status)
{
  if ((ipmi_kcs_get_status (sms_io_base) & IPMI_KCS_STATUS_REG_STATE) == 
      (status & IPMI_KCS_STATUS_REG_STATE))
    return 1;
  else
    return 0;
}

/*
 * Read dummy byte to clear OBF if set.
 */
void
ipmi_kcs_clear_obf (u_int16_t sms_io_base)
{
  if (ipmi_kcs_get_status (sms_io_base) & IPMI_KCS_STATUS_REG_OBF) 
    {
      ipmi_kcs_read_byte (sms_io_base);
    }
}

/* 
 * Main read loop.
 */
ssize_t
ipmi_kcs_read (u_int16_t sms_io_base, u_int8_t* bytes, u_int32_t bytes_len)
{
  u_int8_t *p = bytes;
  int len = 0;

  if ((bytes == NULL) || (bytes_len == 0))
    {
      errno = EINVAL;
      len = -1;
      goto finish;
    }

  ipmi_kcs_wait_for_ibf_clear (sms_io_base);
  if (!ipmi_kcs_test_if_state (sms_io_base, IPMI_KCS_STATE_READ)) 
    {
      errno = EBUSY;
      len = -1;
      goto finish;
    }
  while (ipmi_kcs_test_if_state (sms_io_base, IPMI_KCS_STATE_READ) && len < bytes_len)
    {
      ipmi_kcs_wait_for_obf_set (sms_io_base);
      *(p++) = ipmi_kcs_read_byte (sms_io_base);
      len++;
      ipmi_kcs_read_next (sms_io_base);
      ipmi_kcs_wait_for_ibf_clear (sms_io_base);
    }
  if (ipmi_kcs_test_if_state (sms_io_base, IPMI_KCS_STATE_IDLE))
    {
      /* Clean up */
      ipmi_kcs_wait_for_obf_set (sms_io_base);
      ipmi_kcs_read_byte (sms_io_base); /* toss it, ACK */
      goto finish;
    }
  else
    {
      /* error! */
      errno = EBUSY;
      len = -1;
      goto finish;
    }

 finish:
/*   fprintf (stderr, "__DEBUG__: PID [%d] Leaving Lock [%d]\n", getpid (), ipmi_kcs_get_mutex_semid ()); */
  IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ());
  return (len);
}

/*
 * Standard write loop. 
 */
ssize_t
ipmi_kcs_write (u_int16_t sms_io_base, u_int8_t *bytes, u_int32_t  bytes_len)
{
  u_int8_t *buf=bytes;
  u_int32_t bytes_count = 0;

  IPMI_MUTEX_LOCK (ipmi_kcs_get_mutex_semid ()); 
/*   fprintf (stderr, "__DEBUG__: PID [%d] Entered Lock [%d]\n", getpid (), ipmi_kcs_get_mutex_semid ()); */

  if ((bytes == NULL) || (bytes_len == 0))
    {
      errno = EINVAL;
      bytes_count = -1;
      goto failure;
    }

  ipmi_kcs_wait_for_ibf_clear (sms_io_base);
  ipmi_kcs_clear_obf (sms_io_base);
  ipmi_kcs_start_write (sms_io_base);
  ipmi_kcs_wait_for_ibf_clear (sms_io_base);
  if (!ipmi_kcs_test_if_state (sms_io_base, IPMI_KCS_STATE_WRITE))
    {
      errno = EBUSY;
      bytes_count = -1;
      goto failure;
    }
  ipmi_kcs_clear_obf (sms_io_base);

  /* note we have to save last byte. */
  /* for (buf=data; data+len-1 < buf; buf++) */
  for (; bytes_len > 1; bytes_len--)
    {
      ipmi_kcs_write_byte (sms_io_base, *buf);
      ipmi_kcs_wait_for_ibf_clear (sms_io_base);
      if (!ipmi_kcs_test_if_state (sms_io_base, IPMI_KCS_STATE_WRITE))
        {
	  errno = EBUSY;
	  goto failure;
        }
      ipmi_kcs_clear_obf (sms_io_base);
      buf++;
      bytes_count++;
    }
  ipmi_kcs_end_write (sms_io_base);
  ipmi_kcs_wait_for_ibf_clear (sms_io_base);
  if (!ipmi_kcs_test_if_state (sms_io_base, IPMI_KCS_STATE_WRITE))
    {
      errno = EBUSY;
      bytes_count = -1;
      goto failure;
    }
  ipmi_kcs_clear_obf (sms_io_base);
  ipmi_kcs_write_byte (sms_io_base, *buf);
  bytes_count++;
  /*    if (!ipmi_kcs_test_if_state (IPMI_KCS_STATE_READ)) {
	printf ("Not in READ state after writing last byte?\n");
	ipmi_kcs_print_state (ipmi_kcs_get_state ());
	exit (1);
	}
  */
 failure:
  return (bytes_count);
}


/*  Standard write loop. Returns EAGAIN when queue is full, You may
    want to try again some time later.
 */
ssize_t
ipmi_kcs_write_interruptible (u_int16_t sms_io_base, u_int8_t *bytes, u_int32_t  bytes_len)
{
  u_int8_t *buf=bytes;
  u_int32_t bytes_count = 0;

  ERR (IPMI_MUTEX_LOCK_INTERRUPTIBLE (ipmi_kcs_get_mutex_semid ()) != -1);
/*   fprintf (stderr, "__DEBUG__: PID [%d] Entered Lock [%d]\n", getpid (), ipmi_kcs_get_mutex_semid ()); */

  if ((bytes == NULL) || (bytes_len == 0))
    {
      errno = EINVAL;
      bytes_count = -1;
      goto failure;
    }

  ipmi_kcs_wait_for_ibf_clear (sms_io_base);
  ipmi_kcs_clear_obf (sms_io_base);
  ipmi_kcs_start_write (sms_io_base);
  ipmi_kcs_wait_for_ibf_clear (sms_io_base);
  if (!ipmi_kcs_test_if_state (sms_io_base, IPMI_KCS_STATE_WRITE))
    {
      errno = EBUSY;
      bytes_count = -1;
      goto failure;
    }
  ipmi_kcs_clear_obf (sms_io_base);

  /* note we have to save last byte. */
  /* for (buf=data; data+len-1 < buf; buf++) */
  for (; bytes_len > 1; bytes_len--)
    {
      ipmi_kcs_write_byte (sms_io_base, *buf);
      ipmi_kcs_wait_for_ibf_clear (sms_io_base);
      if (!ipmi_kcs_test_if_state (sms_io_base, IPMI_KCS_STATE_WRITE))
        {
	  errno = EBUSY;
	  goto failure;
        }
      ipmi_kcs_clear_obf (sms_io_base);
      buf++;
      bytes_count++;
    }
  ipmi_kcs_end_write (sms_io_base);
  ipmi_kcs_wait_for_ibf_clear (sms_io_base);
  if (!ipmi_kcs_test_if_state (sms_io_base, IPMI_KCS_STATE_WRITE))
    {
      errno = EBUSY;
      bytes_count = -1;
      goto failure;
    }
  ipmi_kcs_clear_obf (sms_io_base);
  ipmi_kcs_write_byte (sms_io_base, *buf);
  bytes_count++;
  /*    if (!ipmi_kcs_test_if_state (IPMI_KCS_STATE_READ)) {
	printf ("Not in READ state after writing last byte?\n");
	ipmi_kcs_print_state (ipmi_kcs_get_state ());
	exit (1);
	}
  */
 failure:
  return (bytes_count);
}

int8_t
ipmi_kcs_cmd (u_int16_t sms_io_base, u_int8_t lun, u_int8_t fn, fiid_obj_t obj_cmd_rq, fiid_template_t tmpl_cmd_rq, fiid_obj_t obj_cmd_rs, fiid_template_t tmpl_cmd_rs)
{
  if (!(sms_io_base && obj_cmd_rq && tmpl_cmd_rq && obj_cmd_rs && tmpl_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  { /* Request Block */
    fiid_obj_t obj_hdr_rq = NULL;
    u_int8_t *bytes = NULL; 
    u_int32_t obj_hdr_rq_len, obj_cmd_rq_len, bytes_len;
    
    obj_hdr_rq_len = fiid_obj_len_bytes (tmpl_hdr_kcs);
    ERR (obj_hdr_rq_len > 0);
    obj_hdr_rq = alloca (obj_hdr_rq_len);
    memset (obj_hdr_rq, 0, obj_hdr_rq_len);
    ERR (obj_hdr_rq);
    
    obj_cmd_rq_len = fiid_obj_len_bytes (tmpl_cmd_rq);
    bytes_len = obj_hdr_rq_len + obj_cmd_rq_len;
    bytes = alloca (bytes_len);
    memset (bytes, 0, bytes_len);
    ERR (bytes);
    
    ERR (fill_hdr_ipmi_kcs (lun, fn, obj_hdr_rq) != -1);
    ERR (assemble_ipmi_kcs_pkt (obj_hdr_rq, obj_cmd_rq, 
				  tmpl_cmd_rq, bytes, bytes_len) > 0);

    ERR (ipmi_kcs_write (sms_io_base, bytes, bytes_len) != -1);
  }
  { /* Response Block */
    fiid_obj_t obj_hdr_rs = NULL;
    u_int8_t *bytes = NULL; 
    u_int32_t obj_hdr_rs_len, obj_cmd_rs_len, bytes_len;
    
    obj_hdr_rs_len = fiid_obj_len_bytes (tmpl_hdr_kcs);
    ERR (obj_hdr_rs_len != -1);
    obj_hdr_rs = alloca (obj_hdr_rs_len);
    memset (obj_hdr_rs, 0, obj_hdr_rs_len);
    ERR (obj_hdr_rs);
    
    obj_cmd_rs_len = fiid_obj_len_bytes (tmpl_cmd_rs);
    
    bytes_len = obj_hdr_rs_len + obj_cmd_rs_len;
    bytes = alloca (bytes_len);
    memset (bytes, 0, bytes_len);
    ERR (bytes);
    
    ERR (ipmi_kcs_read (sms_io_base, bytes, bytes_len) != -1);

    ERR (unassemble_ipmi_kcs_pkt (bytes, bytes_len,
				  obj_hdr_rs, obj_cmd_rs, 
				  tmpl_cmd_rs) != -1);
  }
  return (0);
}


int8_t
ipmi_kcs_cmd_interruptible (u_int16_t sms_io_base, u_int8_t lun, u_int8_t fn, fiid_obj_t obj_cmd_rq, fiid_template_t tmpl_cmd_rq, fiid_obj_t obj_cmd_rs, fiid_template_t tmpl_cmd_rs)
{
  if (!(sms_io_base && obj_cmd_rq && tmpl_cmd_rq && obj_cmd_rs && tmpl_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  { /* Request Block */
    fiid_obj_t obj_hdr_rq = NULL;
    u_int8_t *bytes = NULL; 
    u_int32_t obj_hdr_rq_len, obj_cmd_rq_len, bytes_len;
    
    obj_hdr_rq_len = fiid_obj_len_bytes (tmpl_hdr_kcs);
    ERR (obj_hdr_rq_len > 0);
    obj_hdr_rq = alloca (obj_hdr_rq_len);
    memset (obj_hdr_rq, 0, obj_hdr_rq_len);
    ERR (obj_hdr_rq);
    
    obj_cmd_rq_len = fiid_obj_len_bytes (tmpl_cmd_rq);
    bytes_len = obj_hdr_rq_len + obj_cmd_rq_len;
    bytes = alloca (bytes_len);
    memset (bytes, 0, bytes_len);
    ERR (bytes);
    
    ERR (fill_hdr_ipmi_kcs (lun, fn, obj_hdr_rq) != -1);
    ERR (assemble_ipmi_kcs_pkt (obj_hdr_rq, obj_cmd_rq, 
				  tmpl_cmd_rq, bytes, bytes_len) > 0);

    ERR (ipmi_kcs_write_interruptible (sms_io_base, bytes, bytes_len) != -1);
  }
  { /* Response Block */
    fiid_obj_t obj_hdr_rs = NULL;
    u_int8_t *bytes = NULL; 
    u_int32_t obj_hdr_rs_len, obj_cmd_rs_len, bytes_len;
    
    obj_hdr_rs_len = fiid_obj_len_bytes (tmpl_hdr_kcs);
    ERR (obj_hdr_rs_len != -1);
    obj_hdr_rs = alloca (obj_hdr_rs_len);
    memset (obj_hdr_rs, 0, obj_hdr_rs_len);
    ERR (obj_hdr_rs);
    
    obj_cmd_rs_len = fiid_obj_len_bytes (tmpl_cmd_rs);
    
    bytes_len = obj_hdr_rs_len + obj_cmd_rs_len;
    bytes = alloca (bytes_len);
    memset (bytes, 0, bytes_len);
    ERR (bytes);
    
    ERR (ipmi_kcs_read (sms_io_base, bytes, bytes_len) != -1);

    ERR (unassemble_ipmi_kcs_pkt (bytes, bytes_len,
				  obj_hdr_rs, obj_cmd_rs, 
				  tmpl_cmd_rs) != -1);
  }
  return (0);
}
