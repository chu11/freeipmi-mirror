/* 
   ipmi-interface.c: IPMI Unified Driver Model (API interface for all IPMI drivers)

   Copyright (C) 2005 FreeIPMI Core Team

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

#include <stdlib.h>
#include <time.h>
#include <fcntl.h>
#include <errno.h>
#include "freeipmi.h"

fiid_template_t tmpl_inband_hdr =
  {
    {2, "lun"},
    {6, "net_fn"},
    {0, ""}
  };

int 
ipmi_open_outofband (ipmi_device_t *dev, 
		     ipmi_driver_type_t driver_type, 
		     ipmi_mode_t mode, 
		     struct sockaddr *remote_host, 
		     size_t remote_host_len, 
		     u_int8_t auth_type, 
		     char *username, 
		     char *password, 
		     u_int8_t priv_level)
{
  int status;
  
  if (dev == NULL)
    {
      errno = EINVAL;
      return (-1);
    }
  
  memset (dev, 0, sizeof (ipmi_device_t));
  
  if (!(remote_host && remote_host_len))
    {
      errno = EINVAL;
      return (-1);
    }
  
  switch (driver_type)
    {
    case IPMI_DEVICE_LAN:
      break;
    default:
      errno = EINVAL;
      return (-1);
    }
  
  dev->type = driver_type;
  dev->mode = mode;
  /* No probing for out-of-band driver */
  dev->io.outofband.remote_host     = remote_host;
  dev->io.outofband.remote_host_len = remote_host_len;
  dev->io.outofband.auth_type = auth_type;
  { 
    /* Random number generation */
    unsigned int seedp;
    seedp = (int) clock () % (int) time (NULL);
    srand (seedp);
    dev->io.outofband.initial_outbound_seq_num = rand_r (&seedp);
  }
  dev->io.outofband.session_id = 0;
  dev->io.outofband.session_seq_num = 0;
  dev->io.outofband.rq_seq = 0;
  dev->io.outofband.username = username;
  dev->io.outofband.password = password;
  dev->io.outofband.password_len = ((password) ? strlen (password) : 0);
  dev->io.outofband.priv_level = priv_level;
  
  /* Prepare out-of-band headers */
  dev->io.outofband.rq.tmpl_hdr_rmcp_ptr = &tmpl_hdr_rmcp;
  dev->io.outofband.rs.tmpl_hdr_rmcp_ptr = &tmpl_hdr_rmcp;
  
  switch (auth_type)
    {
    case IPMI_SESSION_AUTH_TYPE_NONE:
      dev->io.outofband.rq.tmpl_hdr_session_ptr = 
	dev->io.outofband.rs.tmpl_hdr_session_ptr = &tmpl_hdr_session;
      break;
    case IPMI_SESSION_AUTH_TYPE_OEM_PROP:
      dev->io.outofband.rq.tmpl_hdr_session_ptr = 
	dev->io.outofband.rs.tmpl_hdr_session_ptr = &tmpl_hdr_session_auth;
      break;
    case IPMI_SESSION_AUTH_TYPE_MD2:
    case IPMI_SESSION_AUTH_TYPE_MD5:
    case IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY:
      dev->io.outofband.rq.tmpl_hdr_session_ptr = 
	dev->io.outofband.rs.tmpl_hdr_session_ptr = &tmpl_hdr_session_auth_calc;
      break;
    default:
      memset (dev, 0, sizeof (ipmi_device_t));
      errno = EINVAL;
      return (-1);
    }
  dev->io.outofband.rq.tmpl_msg_hdr_ptr = &tmpl_lan_msg_hdr_rq;
  dev->io.outofband.rs.tmpl_msg_hdr_ptr = &tmpl_lan_msg_hdr_rs;
  
  FIID_OBJ_ALLOC (dev->io.outofband.rq.obj_hdr_rmcp,
		  *dev->io.outofband.rq.tmpl_hdr_rmcp_ptr);
  FIID_OBJ_ALLOC (dev->io.outofband.rs.obj_hdr_rmcp,
		  *dev->io.outofband.rs.tmpl_hdr_rmcp_ptr);
  FIID_OBJ_ALLOC (dev->io.outofband.rq.obj_hdr_session,
		  *dev->io.outofband.rq.tmpl_hdr_session_ptr);
  FIID_OBJ_ALLOC (dev->io.outofband.rs.obj_hdr_session,
		  *dev->io.outofband.rs.tmpl_hdr_session_ptr);
  FIID_OBJ_ALLOC (dev->io.outofband.rq.obj_msg_hdr,
		  *dev->io.outofband.rq.tmpl_msg_hdr_ptr);
  FIID_OBJ_ALLOC (dev->io.outofband.rs.obj_msg_hdr,
		  *dev->io.outofband.rs.tmpl_msg_hdr_ptr);
  
  /* Open client (local) UDP socket */
  if ((dev->io.outofband.local_sockfd = ipmi_open_free_udp_port ()) == -1)
    {
      fiid_obj_free (dev->io.outofband.rq.obj_hdr_rmcp);
      fiid_obj_free (dev->io.outofband.rs.obj_hdr_rmcp);
      fiid_obj_free (dev->io.outofband.rq.obj_hdr_session);
      fiid_obj_free (dev->io.outofband.rs.obj_hdr_session);
      fiid_obj_free (dev->io.outofband.rq.obj_msg_hdr);
      fiid_obj_free (dev->io.outofband.rs.obj_msg_hdr);
      memset (dev, 0, sizeof (ipmi_device_t));
      return (-1);
    }
  /* Note that ipmi_lan_open_session itself calls ipmi_lan_cmd many
     times internally, at this point everything must be set to go
     -- Anand Babu */
  if ((status = ipmi_lan_open_session2 (dev)) == -1)
    {
      /* close the opened socket */
      fiid_obj_free (dev->io.outofband.rq.obj_hdr_rmcp);
      fiid_obj_free (dev->io.outofband.rs.obj_hdr_rmcp);
      fiid_obj_free (dev->io.outofband.rq.obj_hdr_session);
      fiid_obj_free (dev->io.outofband.rs.obj_hdr_session);
      fiid_obj_free (dev->io.outofband.rq.obj_msg_hdr);
      fiid_obj_free (dev->io.outofband.rs.obj_msg_hdr);
      memset (dev, 0, sizeof (ipmi_device_t));
      return (-1);
    }
  
  return (0);
}

int 
ipmi_open_inband (ipmi_device_t *dev, 
		  ipmi_driver_type_t driver_type, 
		  ipmi_mode_t mode)
{
  if (dev == NULL)
    {
      errno = EINVAL;
      return (-1);
    }
  
  memset (dev, 0, sizeof (ipmi_device_t));
  
  switch (driver_type)
    {
    case IPMI_DEVICE_KCS:
      if (ipmi_locate (IPMI_INTERFACE_KCS, &(dev->io.inband.locate_info)) == NULL)
	return (-1);
      break;
    case IPMI_DEVICE_SMIC:
      if (ipmi_locate (IPMI_INTERFACE_SMIC, &(dev->io.inband.locate_info)) == NULL)
	return (-1);
      break;
    case IPMI_DEVICE_BT:
      errno = ENOTSUP;
      return (-1);
    case IPMI_DEVICE_SSIF:
      if (ipmi_locate (IPMI_INTERFACE_SSIF, &(dev->io.inband.locate_info)) == NULL)
	return (-1);
      break;
    default:
      errno = EINVAL;
      return (-1);
    }
  
  dev->type = driver_type;
  dev->mode = mode;
  dev->io.inband.poll_interval_usecs = IPMI_POLL_INTERVAL_USECS;
  
  /* At this point we only support SYSTEM_IO, i.e. inb/outb style IO. 
     If we cant find the bass address, we better exit. -- Anand Babu */
  if (dev->io.inband.locate_info.addr_space_id != IPMI_ADDRESS_SPACE_ID_SYSTEM_IO)
    {
      memset (dev, 0, sizeof (ipmi_device_t));
      errno = ENODEV;
      return (-1);
    }
  
#ifdef __FreeBSD__
#ifdef USE_IOPERM
  /* i386_set_ioperm has known problems on FBSD 5.x (bus errors). */
  if (i386_set_ioperm (dev->private.locate_info.base_addr.bmc_iobase_addr, 
		       0x02, 0x01) != 0)
    {
      memset (dev, 0, sizeof (ipmi_device_t));
      return (-1);
    }
#else
  /* Opening /dev/io raises IOPL bits for current process. */
  /* XXX This fd will remain open until exit as there is no
   * uninitialization routine. */
  dev->private.dev_fd = open ("/dev/io", O_RDONLY);
  if (dev->private.dev_fd < 0)
    {
      memset (dev, 0, sizeof (ipmi_device_t));
      return (-1);
    }
#endif
#else
  if (iopl (3) < 0)
    {
      memset (dev, 0, sizeof (ipmi_device_t));
      return (-1);
    }
#endif
  
  /* Prepare in-band headers */
  dev->io.inband.rq.tmpl_hdr_ptr = &tmpl_inband_hdr;
  dev->io.inband.rs.tmpl_hdr_ptr = &tmpl_inband_hdr;
  
  FIID_OBJ_ALLOC (dev->io.inband.rq.obj_hdr, 
		  *dev->io.inband.rq.tmpl_hdr_ptr);
  FIID_OBJ_ALLOC (dev->io.inband.rs.obj_hdr, 
		  *dev->io.inband.rs.tmpl_hdr_ptr);
  
  return (0);
}

int 
ipmi_cmd (ipmi_device_t *dev, 
	  fiid_obj_t obj_cmd_rq, 
	  fiid_template_t tmpl_cmd_rq, 
	  fiid_obj_t obj_cmd_rs, 
	  fiid_template_t tmpl_cmd_rs)
{
  if (dev == NULL)
    {
      errno = EINVAL;
      return (-1);
    }
  
  switch (dev->type)
    {
    case IPMI_DEVICE_LAN:
      return ipmi_lan_cmd2 (dev, 
			    obj_cmd_rq, 
			    tmpl_cmd_rq, 
			    obj_cmd_rs, 
			    tmpl_cmd_rs);
    case IPMI_DEVICE_KCS:
      return ipmi_kcs_cmd2 (dev, 
			    obj_cmd_rq, 
			    tmpl_cmd_rq, 
			    obj_cmd_rs, 
			    tmpl_cmd_rs);
    case IPMI_DEVICE_SMIC:
    case IPMI_DEVICE_BT:
    case IPMI_DEVICE_SSIF:
    default:
      errno = EINVAL;
      return (-1);
    }
  
  return (0);
}

int 
ipmi_cmd_raw (ipmi_device_t *dev, 
	      u_int8_t *in, 
	      size_t in_len, 
	      u_int8_t *out, 
	      size_t *out_len)
{
  if (dev == NULL)
    {
      errno = EINVAL;
      return (-1);
    }
  
  switch (dev->type)
    {
    case IPMI_DEVICE_LAN:
	break;
    case IPMI_DEVICE_KCS:
      return ipmi_kcs_cmd_raw2 (dev, in, in_len, out, out_len);
    case IPMI_DEVICE_SMIC:
    case IPMI_DEVICE_BT:
    case IPMI_DEVICE_SSIF:
    default:
	errno = EINVAL;
	return (-1);
    }

  return (0);
}

static int
ipmi_outofband_close (ipmi_device_t *dev)
{
  int retval = 0;
  fiid_obj_t obj_cmd_rs = NULL;
  
  FIID_OBJ_ALLOCA (obj_cmd_rs, tmpl_cmd_close_session_rs);
  
  if (ipmi_lan_close_session2 (dev, obj_cmd_rs) != 0)
    {
      retval = -1;
    }
  
  fiid_obj_free (dev->io.outofband.rq.obj_hdr_rmcp);
  fiid_obj_free (dev->io.outofband.rs.obj_hdr_rmcp);
  fiid_obj_free (dev->io.outofband.rq.obj_hdr_session);
  fiid_obj_free (dev->io.outofband.rs.obj_hdr_session);
  fiid_obj_free (dev->io.outofband.rq.obj_msg_hdr);
  fiid_obj_free (dev->io.outofband.rs.obj_msg_hdr);
  
  return (retval);
}

static int 
ipmi_inband_close (ipmi_device_t *dev)
{
#ifdef __FreeBSD__
#ifndef USE_IOPERM
  close (dev->private.dev_fd);
#endif
#endif
  
  fiid_obj_free (dev->io.inband.rq.obj_hdr);
  fiid_obj_free (dev->io.inband.rs.obj_hdr);
  
  ipmi_xfree (dev->io.inband.dev_name);
  
  if (dev->io.inband.mutex_semid)
    IPMI_MUTEX_UNLOCK (dev->io.inband.mutex_semid);
  
  return (0);
}

int
ipmi_close (ipmi_device_t *dev)
{
  if (dev == NULL)
    {
      errno = EINVAL;
      return (-1);
    }
  
  switch (dev->type)
    {
    case IPMI_DEVICE_LAN:
      ipmi_outofband_close (dev);
      break;
    case IPMI_DEVICE_KCS:
    case IPMI_DEVICE_SMIC:
    case IPMI_DEVICE_BT:
    case IPMI_DEVICE_SSIF:
      ipmi_inband_close (dev);
      break;
    default:
      errno = EINVAL;
      return (-1);
    }
  
  return (0);
}

