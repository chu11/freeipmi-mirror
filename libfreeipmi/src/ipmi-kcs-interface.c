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

/* AIX requires this to be the first thing in the file.  */
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
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

#if defined(__FreeBSD__) && !defined(USE_IOPERM)
#include <fcntl.h>
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

/* FIXME: This whole set of global variables should go. It is replaced
          by ipmi_device_t structure. -- Anand Babu */
/* static u_int64_t     kcs_poll_count; */
/* static u_int16_t     kcs_sms_io_base = IPMI_KCS_SMS_IO_BASE_DEFAULT; */
/* static int           kcs_reg_space   = IPMI_REG_SPACE_DEFAULT; */
/* static unsigned long kcs_sleep_usecs = IPMI_KCS_SLEEP_USECS; */
static int           kcs_mutex_semid = 0;
ipmi_device_t _dev;

#if defined(__FreeBSD__) && !defined(USE_IOPERM)
static int ipmi_ksc_dev_io_fd = -1;
#endif

fiid_template_t tmpl_hdr_kcs =
  {
    {2, "lun"},
    {6, "net_fn"},
    {0, ""}
  };

int
ipmi_kcs_get_mutex_semid (void)
{
  return (kcs_mutex_semid);
}

/* FIXME: This function has to go. It is replaced by ipmi_open_inband 
            -- Anand Babu */
int
ipmi_kcs_io_init (u_int16_t sms_io_base, 
		  u_int8_t reg_space, 
		  unsigned long sleep_usecs)
{
  return (ipmi_open_inband (&_dev, IPMI_DEVICE_KCS, IPMI_MODE_DEFAULT));

#if 0
  if (sms_io_base < 1)
    {
      errno = EINVAL;
      return (-1);
    }

  kcs_sms_io_base = sms_io_base;
  kcs_reg_space   = reg_space;
  kcs_sleep_usecs = sleep_usecs;
  
  ERR ((kcs_mutex_semid = ipmi_mutex_init (IPMI_KCS_IPCKEY ())) != -1);

#ifdef __FreeBSD__
#ifdef USE_IOPERM
  /* i386_set_ioperm has known problems on FBSD 5.x (bus errors). */
  return (i386_set_ioperm (kcs_sms_io_base, 0x02, 0x01));
#else
  /* Opening /dev/io raises IOPL bits for current process. */
  /* XXX This fd will remain open until exit as there is no
   * uninitialization routine. */
  ipmi_ksc_dev_io_fd = open ("/dev/io", O_RDONLY);
  return (ipmi_ksc_dev_io_fd == -1 ? -1 : 0);
#endif
#else
  return (iopl (3));
#endif
#endif /* #if 0 */
}

/* FIXME: this function has to go --Anand Babu */
#if 0
int
ipmi_kcs_open (u_int16_t sms_io_base, 
	       u_int8_t reg_space, 
	       unsigned long sleep_usecs)
{
  if (sms_io_base < 1)
    {
      errno = EINVAL;
      return (-1);
    }

  kcs_sms_io_base = sms_io_base;
  kcs_reg_space   = reg_space;
  kcs_sleep_usecs = sleep_usecs;
  return 0;
}
#endif

int8_t
fill_hdr_ipmi_kcs (u_int8_t lun, 
		   u_int8_t fn, 
		   fiid_obj_t obj_hdr)
{
  if ((lun > IPMI_BMC_IPMB_LUN_OEM_LUN2) ||
      (fn > IPMI_NET_FN_TRANSPORT_RS)    ||
      (obj_hdr == NULL))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_SET (obj_hdr, tmpl_hdr_kcs, "lun", lun);
  FIID_OBJ_SET (obj_hdr, tmpl_hdr_kcs, "net_fn", fn);
  return 0;
}

int8_t 
assemble_ipmi_kcs_pkt (fiid_obj_t obj_hdr, 
		       fiid_obj_t obj_cmd, 
		       fiid_template_t tmpl_cmd, 
		       u_int8_t *pkt, 
		       u_int32_t pkt_len)
{
  u_int32_t obj_cmd_len, obj_hdr_len;
  if (!(obj_hdr && obj_cmd && tmpl_cmd && pkt))
    {
      errno = EINVAL;
      return (-1);
    }
  obj_hdr_len = fiid_obj_len_bytes (tmpl_hdr_kcs);
  obj_cmd_len = fiid_obj_len_bytes (tmpl_cmd);
  if (pkt_len < (obj_hdr_len + obj_cmd_len))
    {
      errno = EMSGSIZE;
      return (-1);
    }
  
  memset (pkt, 0, obj_hdr_len + obj_cmd_len);
  memcpy (pkt, obj_hdr, obj_hdr_len);
  memcpy (pkt + obj_hdr_len, obj_cmd, obj_cmd_len);
  return (obj_hdr_len + obj_cmd_len);
}

int8_t 
unassemble_ipmi_kcs_pkt (u_int8_t *pkt, 
			 u_int32_t pkt_len, 
			 fiid_obj_t obj_hdr, 
			 fiid_obj_t obj_cmd, 
			 fiid_template_t tmpl_cmd)
{
  u_int32_t indx = 0;

  if (pkt == NULL)
    {
      errno = EINVAL;
      return (-1);
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
	  return (-1);
 	}
    }

  return 0;
}

int8_t
ipmi_kcs_get_status (ipmi_device_t *dev)
{
  return _INB (IPMI_KCS_REG_STATUS (dev->private.locate_info.base_addr.bmc_iobase_addr, dev->private.locate_info.reg_space));
}

/*
 * Wait for IBF (In-Bound Flag) to clear, signalling BMC has
 * read the command. 
 */
void
ipmi_kcs_wait_for_ibf_clear (ipmi_device_t *dev)
{
  while (ipmi_kcs_get_status (dev) & IPMI_KCS_STATUS_REG_IBF)
    usleep (dev->poll_interval_usecs);
}

/* 
 * Wait for OBF to raise, signalling data is pending to read
 * or no command is pending.
 */

void
ipmi_kcs_wait_for_obf_set (ipmi_device_t *dev)
{
  while (!(ipmi_kcs_get_status (dev) & IPMI_KCS_STATUS_REG_OBF))
    usleep (dev->poll_interval_usecs);
}

/*
 * Read byte from outbound data port. 
 */
int8_t
ipmi_kcs_read_byte (ipmi_device_t *dev)
{
  return _INB (IPMI_KCS_REG_DATAOUT (dev->private.locate_info.base_addr.bmc_iobase_addr));
}

/*
 * Bump channel into sending next byte.
 */
void
ipmi_kcs_read_next (ipmi_device_t *dev) 
{
  _OUTB (IPMI_KCS_CTRL_READ, IPMI_KCS_REG_DATAIN (dev->private.locate_info.base_addr.bmc_iobase_addr));
}
/*
 * Set up channel for writing.
 */
void
ipmi_kcs_start_write (ipmi_device_t *dev)
{
  _OUTB (IPMI_KCS_CTRL_WRITE_START, IPMI_KCS_REG_CMD (dev->private.locate_info.base_addr.bmc_iobase_addr, dev->private.locate_info.reg_space));
}

/*
 * Write byte to inound data port.
 */
void
ipmi_kcs_write_byte (ipmi_device_t *dev, u_int8_t byte)
{
  _OUTB (byte, IPMI_KCS_REG_DATAIN (dev->private.locate_info.base_addr.bmc_iobase_addr));
}

/* 
 * Set up channel to end write.
 */
void
ipmi_kcs_end_write (ipmi_device_t *dev)
{
  _OUTB (IPMI_KCS_CTRL_WRITE_END, IPMI_KCS_REG_CMD (dev->private.locate_info.base_addr.bmc_iobase_addr, dev->private.locate_info.reg_space));
}

/* 
 * Send Abort current processing command.
 */
void
ipmi_kcs_get_abort (ipmi_device_t *dev)
{
  _OUTB (IPMI_KCS_CTRL_GET_ABORT, IPMI_KCS_REG_CMD (dev->private.locate_info.base_addr.bmc_iobase_addr, dev->private.locate_info.reg_space));
}

int8_t
ipmi_kcs_test_if_state (ipmi_device_t *dev, u_int8_t status)
{
  if ((ipmi_kcs_get_status (dev) & IPMI_KCS_STATUS_REG_STATE) == 
      (status & IPMI_KCS_STATUS_REG_STATE))
    return 1;
  else
    return 0;
}

/*
 * Read dummy byte to clear OBF if set.
 */
void
ipmi_kcs_clear_obf (ipmi_device_t *dev)
{
  if (ipmi_kcs_get_status (dev) & IPMI_KCS_STATUS_REG_OBF) 
    {
      ipmi_kcs_read_byte (dev);
    }
}

/* 
 * Main read loop.
 */
ssize_t
ipmi_kcs_read (ipmi_device_t *dev, 
	       u_int8_t* bytes, 
	       u_int32_t bytes_len)
{
  u_int8_t *p = bytes;
  int len = 0;

  if ((bytes == NULL) || (bytes_len == 0))
    {
      errno = EINVAL;
      len = -1;
      goto finish;
    }

  ipmi_kcs_wait_for_ibf_clear (dev);
  if (!ipmi_kcs_test_if_state (dev, IPMI_KCS_STATE_READ)) 
    {
      errno = EBUSY;
      len = -1;
      goto finish;
    }
  while (ipmi_kcs_test_if_state (dev, IPMI_KCS_STATE_READ) && len < bytes_len)
    {
      ipmi_kcs_wait_for_obf_set (dev);
      *(p++) = ipmi_kcs_read_byte (dev);
      len++;
      ipmi_kcs_read_next (dev);
      ipmi_kcs_wait_for_ibf_clear (dev);
    }
  if (ipmi_kcs_test_if_state (dev, IPMI_KCS_STATE_IDLE))
    {
      /* Clean up */
      ipmi_kcs_wait_for_obf_set (dev);
      ipmi_kcs_read_byte (dev); /* toss it, ACK */
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
ipmi_kcs_write (ipmi_device_t *dev, 
		u_int8_t *bytes, 
		u_int32_t  bytes_len)
{
  u_int8_t *buf=bytes;
  u_int32_t bytes_count = 0;

  IPMI_MUTEX_LOCK (ipmi_kcs_get_mutex_semid ());


/*   fprintf (stderr, "__DEBUG__: PID [%d] Entered Lock [%d]\n", getpid (), ipmi_kcs_get_mutex_semid ()); */

  if ((bytes == NULL) || (bytes_len == 0))
    {
      IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ());
      errno = EINVAL;
      bytes_count = -1;
      goto failure;
    }

  ipmi_kcs_wait_for_ibf_clear (dev);
  ipmi_kcs_clear_obf (dev);
  ipmi_kcs_start_write (dev);
  ipmi_kcs_wait_for_ibf_clear (dev);
  if (!ipmi_kcs_test_if_state (dev, IPMI_KCS_STATE_WRITE))
    {
      IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ());
      errno = EBUSY;
      bytes_count = -1;
      goto failure;
    }
  ipmi_kcs_clear_obf (dev);

  /* note we have to save last byte. */
  /* for (buf=data; data+len-1 < buf; buf++) */
  for (; bytes_len > 1; bytes_len--)
    {
      ipmi_kcs_write_byte (dev, *buf);
      ipmi_kcs_wait_for_ibf_clear (dev);
      if (!ipmi_kcs_test_if_state (dev, IPMI_KCS_STATE_WRITE))
        {
          IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ());
	  errno = EBUSY;
          bytes_count = -1;
	  goto failure;
        }
      ipmi_kcs_clear_obf (dev);
      buf++;
      bytes_count++;
    }
  ipmi_kcs_end_write (dev);
  ipmi_kcs_wait_for_ibf_clear (dev);
  if (!ipmi_kcs_test_if_state (dev, IPMI_KCS_STATE_WRITE))
    {
      IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ());
      errno = EBUSY;
      bytes_count = -1;
      goto failure;
    }
  ipmi_kcs_clear_obf (dev);
  ipmi_kcs_write_byte (dev, *buf);
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
ipmi_kcs_write_interruptible (ipmi_device_t *dev, 
			      u_int8_t *bytes, 
			      u_int32_t  bytes_len)
{
  u_int8_t *buf=bytes;
  u_int32_t bytes_count = 0;
  int ret;
  
  ret = IPMI_MUTEX_LOCK_INTERRUPTIBLE (ipmi_kcs_get_mutex_semid ());
  if (ret == -1 && errno == EAGAIN)
    return (-1);
  ERR ((!(ret == -1 && errno != EAGAIN)));

/*   fprintf (stderr, "__DEBUG__: PID [%d] Entered Lock [%d]\n", getpid (), ipmi_kcs_get_mutex_semid ()); */

  if ((bytes == NULL) || (bytes_len == 0))
    {
      IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ());
      errno = EINVAL;
      bytes_count = -1;
      goto failure;
    }

  ipmi_kcs_wait_for_ibf_clear (dev);
  ipmi_kcs_clear_obf (dev);
  ipmi_kcs_start_write (dev);
  ipmi_kcs_wait_for_ibf_clear (dev);
  if (!ipmi_kcs_test_if_state (dev, IPMI_KCS_STATE_WRITE))
    {
      IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ());
      errno = EBUSY;
      bytes_count = -1;
      goto failure;
    }
  ipmi_kcs_clear_obf (dev);

  /* note we have to save last byte. */
  /* for (buf=data; data+len-1 < buf; buf++) */
  for (; bytes_len > 1; bytes_len--)
    {
      ipmi_kcs_write_byte (dev, *buf);
      ipmi_kcs_wait_for_ibf_clear (dev);
      if (!ipmi_kcs_test_if_state (dev, IPMI_KCS_STATE_WRITE))
        {
          IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ());
	  errno = EBUSY;
          bytes_count = -1;
	  goto failure;
        }
      ipmi_kcs_clear_obf (dev);
      buf++;
      bytes_count++;
    }
  ipmi_kcs_end_write (dev);
  ipmi_kcs_wait_for_ibf_clear (dev);
  if (!ipmi_kcs_test_if_state (dev, IPMI_KCS_STATE_WRITE))
    {
      IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ());
      errno = EBUSY;
      bytes_count = -1;
      goto failure;
    }
  ipmi_kcs_clear_obf (dev);
  ipmi_kcs_write_byte (dev, *buf);
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
ipmi_kcs_cmd (u_int8_t lun, 
	      u_int8_t fn, 
	      fiid_obj_t obj_cmd_rq, 
	      fiid_template_t tmpl_cmd_rq, 
	      fiid_obj_t obj_cmd_rs, 
	      fiid_template_t tmpl_cmd_rs)
{
  ipmi_device_t *dev = &_dev;

  if (!(obj_cmd_rq && tmpl_cmd_rq && obj_cmd_rs && tmpl_cmd_rs))
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

    ERR (ipmi_kcs_write (dev, bytes, bytes_len) != -1);
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
    
    ERR (ipmi_kcs_read (dev, bytes, bytes_len) != -1);

    ERR (unassemble_ipmi_kcs_pkt (bytes, bytes_len,
				  obj_hdr_rs, obj_cmd_rs, 
				  tmpl_cmd_rs) != -1);
  }
  return (0);
}

int8_t 
ipmi_kcs_cmd2 (ipmi_device_t *dev, 
	       fiid_obj_t obj_cmd_rq, 
	       fiid_template_t tmpl_cmd_rq, 
	       fiid_obj_t obj_cmd_rs, 
	       fiid_template_t tmpl_cmd_rs)
{
  if (!(dev && tmpl_cmd_rq && obj_cmd_rq && tmpl_cmd_rs && obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  { 
    u_int8_t *pkt;
    u_int32_t pkt_len;
    
    pkt_len = fiid_obj_len_bytes (*(dev->io.inband.rq.tmpl_hdr_ptr)) + 
      fiid_obj_len_bytes (tmpl_cmd_rq);
    pkt = alloca (pkt_len);
    memset (pkt, 0, pkt_len);
    ERR (pkt);
    
    ERR (fill_hdr_ipmi_kcs (dev->private.lun, 
			    dev->private.net_fn, 
			    dev->io.inband.rq.obj_hdr) == 0);
    ERR (assemble_ipmi_kcs_pkt (dev->io.inband.rq.obj_hdr, 
				obj_cmd_rq, 
				tmpl_cmd_rq, 
				pkt, 
				pkt_len) > 0);
    ERR (ipmi_kcs_write (dev, pkt, pkt_len) != -1);
  }
  
  { 
    u_int8_t *pkt;
    u_int32_t pkt_len;
    
    pkt_len = fiid_obj_len_bytes (*(dev->io.inband.rs.tmpl_hdr_ptr)) + 
      fiid_obj_len_bytes (tmpl_cmd_rs);
    pkt = alloca (pkt_len);
    memset (pkt, 0, pkt_len);
    ERR (pkt);
    
    ERR (fill_hdr_ipmi_kcs (dev->private.lun, 
			    dev->private.net_fn, 
			    dev->io.inband.rs.obj_hdr) == 0);
    ERR (ipmi_kcs_read (dev, pkt, pkt_len) != -1);
    ERR (unassemble_ipmi_kcs_pkt (pkt, 
				  pkt_len, 
				  dev->io.inband.rs.obj_hdr, 
				  obj_cmd_rs, 
				  tmpl_cmd_rs) != -1);
  }
  
  return (0);
}


int8_t
ipmi_kcs_cmd_interruptible (u_int8_t lun, 
			    u_int8_t fn, 
			    fiid_obj_t obj_cmd_rq, 
			    fiid_template_t tmpl_cmd_rq, 
			    fiid_obj_t obj_cmd_rs, 
			    fiid_template_t tmpl_cmd_rs)
{
  int ret;
  ipmi_device_t *dev = &_dev;

  if (!(obj_cmd_rq && tmpl_cmd_rq && obj_cmd_rs && tmpl_cmd_rs))
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

    ret = ipmi_kcs_write_interruptible (dev, bytes, bytes_len);
    if (ret == -1 && errno == EAGAIN)
      return (-1);
    ERR ((!(ret == -1 && errno != EAGAIN)));
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
    
    ERR (ipmi_kcs_read (dev, bytes, bytes_len) != -1);

    ERR (unassemble_ipmi_kcs_pkt (bytes, bytes_len,
				  obj_hdr_rs, obj_cmd_rs, 
				  tmpl_cmd_rs) != -1);
  }
  return (0);
}

int8_t
ipmi_kcs_cmd_raw (u_int8_t lun, 
		  u_int8_t fn, 
		  u_int8_t *buf_rq, 
		  size_t buf_rq_len, 
		  u_int8_t *buf_rs, 
		  size_t *buf_rs_len)
{
  ipmi_device_t *dev = &_dev;

  if (!(buf_rq && buf_rq_len > 0 
        && buf_rs && buf_rs_len && *buf_rs_len > 0))
    {
      errno = EINVAL;
      return (-1);
    }

  { /* Request Block */
    u_int8_t *bytes = NULL; 
    u_int32_t obj_hdr_rq_len, bytes_len;
    
    obj_hdr_rq_len = fiid_obj_len_bytes (tmpl_hdr_kcs);
    ERR (obj_hdr_rq_len > 0);

    bytes_len = obj_hdr_rq_len + buf_rq_len;
    bytes = alloca (bytes_len);
    memset (bytes, 0, bytes_len);
    ERR (bytes);

    ERR (fill_hdr_ipmi_kcs (lun, fn, bytes) != -1);
    memcpy(bytes + obj_hdr_rq_len, buf_rq, buf_rq_len);

    ERR (ipmi_kcs_write (dev, bytes, bytes_len) != -1);
  }
  { /* Response Block */
    u_int8_t *bytes = NULL; 
    u_int32_t obj_hdr_rs_len, bytes_read, bytes_len;
    
    obj_hdr_rs_len = fiid_obj_len_bytes (tmpl_hdr_kcs);
    ERR (obj_hdr_rs_len != -1);

    bytes_len = obj_hdr_rs_len + *buf_rs_len;
    bytes = alloca (bytes_len);
    memset (bytes, 0, bytes_len);
    ERR (bytes);
    
    ERR ((bytes_read = ipmi_kcs_read (dev, bytes, bytes_len)) != -1);
    if (bytes_read > obj_hdr_rs_len)
      {
        u_int32_t rs_len = bytes_read - obj_hdr_rs_len;
        if (rs_len <= *buf_rs_len)
          *buf_rs_len = rs_len;
        
        memcpy(buf_rs, bytes + obj_hdr_rs_len, *buf_rs_len);
      }
    else
      /* achu: the cmd and comp_code should always be returned, so
       * hopefully we never ever reach this point */
      *buf_rs_len = 0;
  }
  return (0);
}

int8_t
ipmi_kcs_cmd_raw2 (ipmi_device_t *dev, 
		   u_int8_t *buf_rq, 
		   size_t buf_rq_len, 
		   u_int8_t *buf_rs, 
		   size_t *buf_rs_len)
{
  if (!(dev && buf_rq && buf_rq_len > 0 
        && buf_rs && buf_rs_len && *buf_rs_len > 0))
    {
      errno = EINVAL;
      return (-1);
    }

  { /* Request Block */
    u_int8_t *bytes = NULL; 
    u_int32_t obj_hdr_rq_len, bytes_len;
    
    obj_hdr_rq_len = fiid_obj_len_bytes (*dev->io.inband.rq.tmpl_hdr_ptr);
    ERR (obj_hdr_rq_len > 0);

    bytes_len = obj_hdr_rq_len + buf_rq_len;
    bytes = alloca (bytes_len);
    memset (bytes, 0, bytes_len);
    ERR (bytes);

/*     ERR (fill_hdr_ipmi_kcs (lun, fn, bytes) != -1); */
    memcpy(bytes, dev->io.inband.rq.obj_hdr, obj_hdr_rq_len);
    memcpy(bytes + obj_hdr_rq_len, buf_rq, buf_rq_len);

    ERR (ipmi_kcs_write (dev, bytes, bytes_len) != -1);
  }
  { /* Response Block */
    u_int8_t *bytes = NULL; 
    u_int32_t obj_hdr_rs_len, bytes_read, bytes_len;
    
    obj_hdr_rs_len = fiid_obj_len_bytes (*dev->io.inband.rs.tmpl_hdr_ptr);
    ERR (obj_hdr_rs_len != -1);

    bytes_len = obj_hdr_rs_len + *buf_rs_len;
    bytes = alloca (bytes_len);
    memset (bytes, 0, bytes_len);
    ERR (bytes);
    
    ERR ((bytes_read = ipmi_kcs_read (dev, bytes, bytes_len)) != -1);
    if (bytes_read > obj_hdr_rs_len)
      {
        u_int32_t rs_len = bytes_read - obj_hdr_rs_len;
        if (rs_len <= *buf_rs_len)
          *buf_rs_len = rs_len;
        
        memcpy(buf_rs, bytes + obj_hdr_rs_len, *buf_rs_len);
      }
    else
      /* achu: the cmd and comp_code should always be returned, so
       * hopefully we never ever reach this point */
      *buf_rs_len = 0;
  }
  return (0);
}

int8_t
ipmi_kcs_cmd_raw_interruptible (u_int8_t lun, 
				u_int8_t fn, 
				u_int8_t *buf_rq, 
				u_int32_t buf_rq_len, 
				u_int8_t *buf_rs, 
				u_int32_t *buf_rs_len)
{
  int ret;
  ipmi_device_t *dev = &_dev;

  if (!(buf_rq && buf_rq_len > 0 && buf_rs && buf_rs_len && *buf_rs_len > 0))
    {
      errno = EINVAL;
      return (-1);
    }

  { /* Request Block */
    u_int8_t *bytes = NULL; 
    u_int32_t obj_hdr_rq_len, bytes_len;
    
    obj_hdr_rq_len = fiid_obj_len_bytes (tmpl_hdr_kcs);
    ERR (obj_hdr_rq_len > 0);

    bytes_len = obj_hdr_rq_len + buf_rq_len;
    bytes = alloca (bytes_len);
    memset (bytes, 0, bytes_len);
    ERR (bytes);

    ERR (fill_hdr_ipmi_kcs (lun, fn, bytes) != -1);
    memcpy(bytes + obj_hdr_rq_len, buf_rq, buf_rq_len);

    ret = ipmi_kcs_write_interruptible (dev, bytes, bytes_len);
    if (ret == -1 && errno == EAGAIN)
      return (-1);
    ERR ((!(ret == -1 && errno != EAGAIN)));
  }
  { /* Response Block */
    u_int8_t *bytes = NULL; 
    u_int32_t obj_hdr_rs_len, bytes_read, bytes_len;
    
    obj_hdr_rs_len = fiid_obj_len_bytes (tmpl_hdr_kcs);
    ERR (obj_hdr_rs_len != -1);

    bytes_len = obj_hdr_rs_len + *buf_rs_len;
    bytes = alloca (bytes_len);
    memset (bytes, 0, bytes_len);
    ERR (bytes);
    
    ERR ((bytes_read = ipmi_kcs_read (dev, bytes, bytes_len)) != -1);
    if (bytes_read > obj_hdr_rs_len)
      {
        u_int32_t rs_len = bytes_read - obj_hdr_rs_len;
        if (rs_len <= *buf_rs_len)
          *buf_rs_len = rs_len;
        
        memcpy(buf_rs, bytes + obj_hdr_rs_len, *buf_rs_len);
      }
    else
      /* achu: the cmd and comp_code should always be returned, so
       * hopefully we never ever reach this point */
      *buf_rs_len = 0;
  }
  return (0);
}
