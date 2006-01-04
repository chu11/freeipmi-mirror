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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

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

static int
ipmi_kcs_get_mutex_semid (void)
{
  return (_dev.io.inband.mutex_semid);
}

int
ipmi_kcs_io_init (uint16_t sms_io_base, 
		  uint8_t reg_space, 
		  unsigned long sleep_usecs)
{
  memset (&_dev, 0, sizeof (ipmi_device_t));
  return (ipmi_open_inband (&_dev, 0, IPMI_DEVICE_KCS, sms_io_base, reg_space, NULL, IPMI_MODE_DEFAULT));

#if 0
  if (sms_io_base < 1)
    {
      errno = EINVAL;
      return (-1);
    }

  kcs_sms_io_base = sms_io_base;
  kcs_reg_space   = reg_space;
  kcs_sleep_usecs = sleep_usecs;
  
  ERR ((inband_mutex_semid = ipmi_mutex_init (IPMI_INBAND_IPCKEY ())) != -1);

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
ipmi_kcs_open (uint16_t sms_io_base, 
	       uint8_t reg_space, 
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
fill_hdr_ipmi_kcs (uint8_t lun, 
		   uint8_t fn, 
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
		       uint8_t *pkt, 
		       uint32_t pkt_len)
{
  uint32_t obj_cmd_len, obj_hdr_len;
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
unassemble_ipmi_kcs_pkt (uint8_t *pkt, 
			 uint32_t pkt_len, 
			 fiid_obj_t obj_hdr, 
			 fiid_obj_t obj_cmd, 
			 fiid_template_t tmpl_cmd)
{
  uint32_t indx = 0;

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

static int8_t
ipmi_kcs_get_status (ipmi_device_t *dev)
{
  return _INB (IPMI_KCS_REG_STATUS (dev->io.inband.locate_info.base_addr.bmc_iobase_addr, dev->io.inband.locate_info.reg_space));
}

/*
 * Wait for IBF (In-Bound Flag) to clear, signalling BMC has
 * read the command. 
 */
static void
ipmi_kcs_wait_for_ibf_clear (ipmi_device_t *dev)
{
  while (ipmi_kcs_get_status (dev) & IPMI_KCS_STATUS_REG_IBF)
    usleep (dev->io.inband.poll_interval_usecs);
}

/* 
 * Wait for OBF to raise, signalling data is pending to read
 * or no command is pending.
 */

static void
ipmi_kcs_wait_for_obf_set (ipmi_device_t *dev)
{
  while (!(ipmi_kcs_get_status (dev) & IPMI_KCS_STATUS_REG_OBF))
    usleep (dev->io.inband.poll_interval_usecs);
}

/*
 * Read byte from outbound data port. 
 */
static int8_t
ipmi_kcs_read_byte (ipmi_device_t *dev)
{
  return _INB (IPMI_KCS_REG_DATAOUT (dev->io.inband.locate_info.base_addr.bmc_iobase_addr));
}

/*
 * Bump channel into sending next byte.
 */
static void
ipmi_kcs_read_next (ipmi_device_t *dev) 
{
  _OUTB (IPMI_KCS_CTRL_READ, IPMI_KCS_REG_DATAIN (dev->io.inband.locate_info.base_addr.bmc_iobase_addr));
}
/*
 * Set up channel for writing.
 */
static void
ipmi_kcs_start_write (ipmi_device_t *dev)
{
  _OUTB (IPMI_KCS_CTRL_WRITE_START, IPMI_KCS_REG_CMD (dev->io.inband.locate_info.base_addr.bmc_iobase_addr, dev->io.inband.locate_info.reg_space));
}

/*
 * Write byte to inound data port.
 */
static void
ipmi_kcs_write_byte (ipmi_device_t *dev, uint8_t byte)
{
  _OUTB (byte, IPMI_KCS_REG_DATAIN (dev->io.inband.locate_info.base_addr.bmc_iobase_addr));
}

/* 
 * Set up channel to end write.
 */
static void
ipmi_kcs_end_write (ipmi_device_t *dev)
{
  _OUTB (IPMI_KCS_CTRL_WRITE_END, IPMI_KCS_REG_CMD (dev->io.inband.locate_info.base_addr.bmc_iobase_addr, dev->io.inband.locate_info.reg_space));
}

#if 0
/* 
 * Send Abort current processing command.
 */
static void
ipmi_kcs_get_abort (ipmi_device_t *dev)
{
  _OUTB (IPMI_KCS_CTRL_GET_ABORT, IPMI_KCS_REG_CMD (dev->io.inband.locate_info.base_addr.bmc_iobase_addr, dev->io.inband.locate_info.reg_space));
}
#endif

static int8_t
ipmi_kcs_test_if_state (ipmi_device_t *dev, uint8_t status)
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
static void
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
	       uint8_t* bytes, 
	       uint32_t bytes_len)
{
  uint8_t *p = bytes;
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

/*   IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ()); */
  return (len);
}

/*
 * Standard write loop. 
 */
ssize_t
ipmi_kcs_write (ipmi_device_t *dev, 
		uint8_t *bytes, 
		uint32_t  bytes_len)
{
  uint8_t *buf=bytes;
  uint32_t bytes_count = 0;

/*   IPMI_MUTEX_LOCK (ipmi_kcs_get_mutex_semid ()); */


/*   fprintf (stderr, "__DEBUG__: PID [%d] Entered Lock [%d]\n", getpid (), ipmi_kcs_get_mutex_semid ()); */

  if ((bytes == NULL) || (bytes_len == 0))
    {
/*       IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ()); */
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
/*       IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ()); */
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
/*           IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ()); */
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
/*       IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ()); */
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
/* __DEPRECATED__: now locking has moved to cmd level --Anand Babu */
ssize_t
ipmi_kcs_write_interruptible (ipmi_device_t *dev, 
			      uint8_t *bytes, 
			      uint32_t  bytes_len)
{
  uint8_t *buf=bytes;
  uint32_t bytes_count = 0;
/*   int ret; */
  
/*   ret = IPMI_MUTEX_LOCK_INTERRUPTIBLE (ipmi_kcs_get_mutex_semid ()); */
/*   if (ret == -1 && errno == EAGAIN) */
/*     return (-1); */
/*   ERR ((!(ret == -1 && errno != EAGAIN))); */

/*   fprintf (stderr, "__DEBUG__: PID [%d] Entered Lock [%d]\n", getpid (), ipmi_kcs_get_mutex_semid ()); */

  if ((bytes == NULL) || (bytes_len == 0))
    {
/*       IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ()); */
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
/*       IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ()); */
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
/*           IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ()); */
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
/*       IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ()); */
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
ipmi_kcs_cmd (uint8_t lun, 
	      uint8_t fn, 
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
    uint8_t *bytes = NULL; 
    uint32_t obj_hdr_rq_len, obj_cmd_rq_len, bytes_len;
    
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

    IPMI_MUTEX_LOCK (ipmi_kcs_get_mutex_semid ());
    ERR_UNLOCK (ipmi_kcs_write (dev, bytes, bytes_len) != -1);
  }
  { /* Response Block */
    fiid_obj_t obj_hdr_rs = NULL;
    uint8_t *bytes = NULL; 
    uint32_t obj_hdr_rs_len, obj_cmd_rs_len, bytes_len;
    
    obj_hdr_rs_len = fiid_obj_len_bytes (tmpl_hdr_kcs);
    ERR_UNLOCK (obj_hdr_rs_len != -1);
    obj_hdr_rs = alloca (obj_hdr_rs_len);
    memset (obj_hdr_rs, 0, obj_hdr_rs_len);
    ERR_UNLOCK (obj_hdr_rs);
    
    obj_cmd_rs_len = fiid_obj_len_bytes (tmpl_cmd_rs);
    
    bytes_len = obj_hdr_rs_len + obj_cmd_rs_len;
    bytes = alloca (bytes_len);
    memset (bytes, 0, bytes_len);
    ERR_UNLOCK (bytes);
    
    ERR_UNLOCK (ipmi_kcs_read (dev, bytes, bytes_len) != -1);
    IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ());

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
    uint8_t *pkt;
    uint32_t pkt_len;
    
    pkt_len = fiid_obj_len_bytes (*(dev->io.inband.rq.tmpl_hdr_ptr)) + 
      fiid_obj_len_bytes (tmpl_cmd_rq);
    pkt = alloca (pkt_len);
    memset (pkt, 0, pkt_len);
    ERR (pkt);
    
    ERR (fill_hdr_ipmi_kcs (dev->lun, 
			    dev->net_fn, 
			    dev->io.inband.rq.obj_hdr) == 0);
    ERR (assemble_ipmi_kcs_pkt (dev->io.inband.rq.obj_hdr, 
				obj_cmd_rq, 
				tmpl_cmd_rq, 
				pkt, 
				pkt_len) > 0);
    ERR (ipmi_kcs_write (dev, pkt, pkt_len) != -1);
  }
  
  { 
    uint8_t *pkt;
    uint32_t pkt_len;
    
    pkt_len = fiid_obj_len_bytes (*(dev->io.inband.rs.tmpl_hdr_ptr)) + 
      fiid_obj_len_bytes (tmpl_cmd_rs);
    pkt = alloca (pkt_len);
    memset (pkt, 0, pkt_len);
    ERR (pkt);
    
    ERR (fill_hdr_ipmi_kcs (dev->lun, 
			    dev->net_fn, 
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
ipmi_kcs_cmd_interruptible (uint8_t lun, 
			    uint8_t fn, 
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
    uint8_t *bytes = NULL; 
    uint32_t obj_hdr_rq_len, obj_cmd_rq_len, bytes_len;
    
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

    ret = IPMI_MUTEX_LOCK_INTERRUPTIBLE (ipmi_kcs_get_mutex_semid ());
    if (ret == -1 && errno == EAGAIN)
      return (-1);
    ERR ((!(ret == -1 && errno != EAGAIN)));

    ERR_UNLOCK (ipmi_kcs_write (dev, bytes, bytes_len) != -1);
  }
  { /* Response Block */
    fiid_obj_t obj_hdr_rs = NULL;
    uint8_t *bytes = NULL; 
    uint32_t obj_hdr_rs_len, obj_cmd_rs_len, bytes_len;
    
    obj_hdr_rs_len = fiid_obj_len_bytes (tmpl_hdr_kcs);
    ERR_UNLOCK (obj_hdr_rs_len != -1);
    obj_hdr_rs = alloca (obj_hdr_rs_len);
    memset (obj_hdr_rs, 0, obj_hdr_rs_len);
    ERR_UNLOCK (obj_hdr_rs);
    
    obj_cmd_rs_len = fiid_obj_len_bytes (tmpl_cmd_rs);
    
    bytes_len = obj_hdr_rs_len + obj_cmd_rs_len;
    bytes = alloca (bytes_len);
    memset (bytes, 0, bytes_len);
    ERR_UNLOCK (bytes);
    
    ERR_UNLOCK (ipmi_kcs_read (dev, bytes, bytes_len) != -1);
    IPMI_MUTEX_UNLOCK (ipmi_kcs_get_mutex_semid ());

    ERR (unassemble_ipmi_kcs_pkt (bytes, bytes_len,
				  obj_hdr_rs, obj_cmd_rs, 
				  tmpl_cmd_rs) != -1);
  }
  return (0);
}

int8_t
ipmi_kcs_cmd_raw2 (ipmi_device_t *dev, 
		   uint8_t *buf_rq, 
		   size_t buf_rq_len, 
		   uint8_t *buf_rs, 
		   size_t *buf_rs_len)
{
  if (!(dev && buf_rq && buf_rq_len > 0 
        && buf_rs && buf_rs_len && *buf_rs_len > 0))
    {
      errno = EINVAL;
      return (-1);
    }
  
  { 
    /* Request Block */
    ERR (ipmi_kcs_write (dev, buf_rq, buf_rq_len) != -1);
  }
  
  { 
    /* Response Block */
    uint32_t bytes_read = 0;
    
    ERR ((bytes_read = ipmi_kcs_read (dev, buf_rs, *buf_rs_len)) != -1);
    *buf_rs_len = bytes_read;
  }
  
  return (0);
}



