/* 
   ipmi-udm.c: IPMI Unified Driver Model (API interface for all
   IPMI drivers)

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  

*/

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>

#include "ipmi-udm.h"

#include "freeipmi-portability.h"
#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "ipmi-locate.h"
#include "ipmi-utils.h"
#include "ipmi-kcs-interface.h"
#include "ipmi-ssif-interface.h"

#include "ipmi-common.h"

fiid_template_t tmpl_inband_hdr =
  {
    {2, "lun", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "net_fn", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

static void 
ipmi_outofband_free (ipmi_device_t *dev)
{
  if (!dev)
    {
      errno = EINVAL;
      return;
    }

  FIID_OBJ_DESTROY_NO_RETURN (dev->io.outofband.rq.obj_rmcp_hdr);
  FIID_OBJ_DESTROY_NO_RETURN (dev->io.outofband.rs.obj_rmcp_hdr);
  FIID_OBJ_DESTROY_NO_RETURN (dev->io.outofband.rq.obj_lan_session_hdr);
  FIID_OBJ_DESTROY_NO_RETURN (dev->io.outofband.rs.obj_lan_session_hdr);
  FIID_OBJ_DESTROY_NO_RETURN (dev->io.outofband.rq.obj_lan_msg_hdr);
  FIID_OBJ_DESTROY_NO_RETURN (dev->io.outofband.rs.obj_lan_msg_hdr);
  FIID_OBJ_DESTROY_NO_RETURN (dev->io.outofband.rq.obj_lan_msg_trlr);
  FIID_OBJ_DESTROY_NO_RETURN (dev->io.outofband.rs.obj_lan_msg_trlr);
}

static void 
ipmi_inband_free (ipmi_device_t *dev)
{
  if (!dev)
    {
      errno = EINVAL;
      return;
    }
  
  if (dev->type == IPMI_DEVICE_KCS && dev->io.inband.kcs_ctx)
    ipmi_kcs_ctx_destroy(dev->io.inband.kcs_ctx);
  if (dev->type == IPMI_DEVICE_SSIF && dev->io.inband.ssif_ctx)
    ipmi_ssif_ctx_destroy(dev->io.inband.ssif_ctx);

  FIID_OBJ_DESTROY_NO_RETURN (dev->io.inband.rq.obj_hdr);
  FIID_OBJ_DESTROY_NO_RETURN (dev->io.inband.rs.obj_hdr);
}

int 
ipmi_open_outofband (ipmi_device_t *dev, 
		     ipmi_driver_type_t driver_type, 
		     ipmi_mode_t mode, 
		     struct sockaddr *remote_host, 
		     size_t remote_host_len, 
		     uint8_t authentication_type, 
		     char *username, 
		     char *password, 
		     uint8_t privilege_level)
{
  int status;

  if (!dev
      || !(remote_host && remote_host_len)
      || driver_type != IPMI_DEVICE_LAN
      || !IPMI_AUTHENTICATION_TYPE_VALID (authentication_type)
      || (username && strlen (username) > IPMI_MAX_USER_NAME_LENGTH)
      || (password && strlen (password) > IPMI_MAX_AUTHENTICATION_CODE_LENGTH)
      || !IPMI_PRIVILEGE_LEVEL_VALID (privilege_level))
    {
      errno = EINVAL;
      return (-1);
    }
  
  dev->type = driver_type;
  dev->mode = mode;
  dev->io.outofband.remote_host = *remote_host;
  dev->io.outofband.remote_host_len = remote_host_len;
  dev->io.outofband.authentication_type = authentication_type;
  memset(dev->io.outofband.username, '\0', IPMI_MAX_USER_NAME_LENGTH);
  if (username != NULL)
    {
      memcpy (dev->io.outofband.username, 
	      username, 
	      strlen (username));
    }
  memset(dev->io.outofband.password, '\0', IPMI_MAX_AUTHENTICATION_CODE_LENGTH);
  if (password != NULL)
    {
      memcpy (dev->io.outofband.password, 
	      password, 
	      strlen (password));
    }
  dev->io.outofband.privilege_level = privilege_level;
  
  dev->io.outofband.rq.tmpl_rmcp_hdr_ptr = &tmpl_rmcp_hdr;
  dev->io.outofband.rs.tmpl_rmcp_hdr_ptr = &tmpl_rmcp_hdr;
  dev->io.outofband.rq.tmpl_lan_session_hdr_ptr = &tmpl_lan_session_hdr;
  dev->io.outofband.rs.tmpl_lan_session_hdr_ptr = &tmpl_lan_session_hdr;
  dev->io.outofband.rq.tmpl_lan_msg_hdr_ptr = &tmpl_lan_msg_hdr_rq;
  dev->io.outofband.rs.tmpl_lan_msg_hdr_ptr = &tmpl_lan_msg_hdr_rs;
  dev->io.outofband.rq.tmpl_lan_msg_trlr_ptr = &tmpl_lan_msg_trlr;
  dev->io.outofband.rs.tmpl_lan_msg_trlr_ptr = &tmpl_lan_msg_trlr;
  
  FIID_OBJ_CREATE_CLEANUP (dev->io.outofband.rq.obj_rmcp_hdr, *(dev->io.outofband.rq.tmpl_rmcp_hdr_ptr));
  
  FIID_OBJ_CREATE_CLEANUP (dev->io.outofband.rs.obj_rmcp_hdr, *(dev->io.outofband.rs.tmpl_rmcp_hdr_ptr));
  
  FIID_OBJ_CREATE_CLEANUP (dev->io.outofband.rq.obj_lan_session_hdr, *(dev->io.outofband.rq.tmpl_lan_session_hdr_ptr));
  
  FIID_OBJ_CREATE_CLEANUP (dev->io.outofband.rs.obj_lan_session_hdr, *(dev->io.outofband.rs.tmpl_lan_session_hdr_ptr));
  
  FIID_OBJ_CREATE_CLEANUP (dev->io.outofband.rq.obj_lan_msg_hdr, *(dev->io.outofband.rq.tmpl_lan_msg_hdr_ptr));
  
  FIID_OBJ_CREATE_CLEANUP (dev->io.outofband.rs.obj_lan_msg_hdr, *(dev->io.outofband.rs.tmpl_lan_msg_hdr_ptr));
  
  FIID_OBJ_CREATE_CLEANUP (dev->io.outofband.rq.obj_lan_msg_trlr, *(dev->io.outofband.rq.tmpl_lan_msg_trlr_ptr));
  
  FIID_OBJ_CREATE_CLEANUP (dev->io.outofband.rs.obj_lan_msg_trlr, *(dev->io.outofband.rs.tmpl_lan_msg_trlr_ptr));
  
  /* Open client (local) UDP socket */
  ERR_CLEANUP (!((dev->io.outofband.local_sockfd = ipmi_open_free_udp_port ()) == -1));

  /* Note that ipmi_lan_open_session itself calls ipmi_lan_cmd many
     times internally, at this point everything must be set to go
     -- Anand Babu */
  if ((status = ipmi_lan_open_session2 (dev)) == -1)
    {
      if (dev->io.outofband.local_sockfd)
	close (dev->io.outofband.local_sockfd);
      goto cleanup;
    }

  return (0);

 cleanup:
  if (dev)
    ipmi_outofband_free (dev);
  return (-1);
}

int 
ipmi_open_inband (ipmi_device_t *dev, 
		  int disable_auto_probe, 
		  ipmi_driver_type_t driver_type, 
		  uint16_t driver_address, 
		  uint8_t reg_space,
		  char *driver_device, 
		  ipmi_mode_t mode)
{
  uint8_t temp_mode;

  if (dev == NULL)
    {
      errno = EINVAL;
      return (-1);
    }

  dev->io.inband.kcs_ctx = NULL;
  dev->io.inband.ssif_ctx = NULL;

  switch (driver_type)
    {
    case IPMI_DEVICE_KCS:
      if (disable_auto_probe)
	{
	  ipmi_locate_info_t *locate_info;
	  
	  if (!driver_device)
	    {
	      errno = EINVAL;
	      return (-1);
	    }
	  
	  if (!(locate_info = (ipmi_locate_info_t *)malloc(sizeof(struct ipmi_locate_info))))
	    goto cleanup;
	  memset(locate_info, '\0', sizeof(struct ipmi_locate_info));

	  locate_info->ipmi_ver_major = 1;
	  locate_info->ipmi_ver_minor = 5;
	  locate_info->locate_driver_type = IPMI_LOCATE_DRIVER_NONE;
	  locate_info->locate_driver = 0;
	  locate_info->interface_type = IPMI_INTERFACE_KCS;
	  ERR ((locate_info->bmc_i2c_dev_name = strdup(driver_device)));
	  locate_info->addr_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_IO;
	  locate_info->base_addr.bmc_iobase_addr = driver_address;
	  locate_info->reg_space = reg_space;
	}
      else 
	{
	  ERR ((dev->io.inband.locate_info = ipmi_locate (IPMI_INTERFACE_KCS)));
	  if (driver_device)
	    {
	      if (dev->io.inband.locate_info->bmc_i2c_dev_name)
		free (dev->io.inband.locate_info->bmc_i2c_dev_name);
	      ERR ((dev->io.inband.locate_info->bmc_i2c_dev_name = strdup(driver_device)));
	    }
	  if (driver_address)
	    dev->io.inband.locate_info->base_addr.bmc_iobase_addr = driver_address;
	  if (reg_space)
	    dev->io.inband.locate_info->reg_space = reg_space;
	}
      dev->type = driver_type;
      dev->mode = mode;
      
      /* At this point we only support SYSTEM_IO, i.e. inb/outb style IO. 
	 If we cant find the bass address, we better exit. -- Anand Babu */
      if (dev->io.inband.locate_info->addr_space_id != IPMI_ADDRESS_SPACE_ID_SYSTEM_IO)
	{
	  errno = ENODEV;
	  goto cleanup;
	}

      ERR_CLEANUP ((dev->io.inband.kcs_ctx = ipmi_kcs_ctx_create()));
      
      ERR_CLEANUP (!(ipmi_kcs_ctx_set_bmc_iobase_addr(dev->io.inband.kcs_ctx, 
						      dev->io.inband.locate_info->base_addr.bmc_iobase_addr) < 0));

      ERR_CLEANUP (!(ipmi_kcs_ctx_set_register_space(dev->io.inband.kcs_ctx, 
						     dev->io.inband.locate_info->reg_space) < 0));

      ERR_CLEANUP (!(ipmi_kcs_ctx_set_poll_interval(dev->io.inband.kcs_ctx, 
						    IPMI_POLL_INTERVAL_USECS) < 0));

      if (dev->mode == IPMI_MODE_DEFAULT)
        temp_mode = IPMI_KCS_MODE_BLOCKING;
      else if (dev->mode == IPMI_MODE_NONBLOCK)
        temp_mode = IPMI_KCS_MODE_NONBLOCKING;
      else
        temp_mode = IPMI_KCS_MODE_DEFAULT;
      
      ERR_CLEANUP (!(ipmi_kcs_ctx_set_mode(dev->io.inband.kcs_ctx, temp_mode) < 0));

      ERR_CLEANUP (!(ipmi_kcs_ctx_io_init(dev->io.inband.kcs_ctx) < 0));

      break;
    case IPMI_DEVICE_SMIC:
      ERR ((dev->io.inband.locate_info = ipmi_locate (IPMI_INTERFACE_SMIC)));
      break;
    case IPMI_DEVICE_BT:
      errno = ENOTSUP;
      return (-1);
    case IPMI_DEVICE_SSIF:
      if (disable_auto_probe)
	{
	  ipmi_locate_info_t *locate_info;
	  
	  if (!driver_device)
	    {
	      errno = EINVAL;
	      return (-1);
	    }

	  if (!(locate_info = (ipmi_locate_info_t *)malloc(sizeof(struct ipmi_locate_info))))
	    goto cleanup;
	  memset(locate_info, '\0', sizeof(struct ipmi_locate_info));

	  locate_info->ipmi_ver_major = 1;
	  locate_info->ipmi_ver_minor = 5;
	  locate_info->locate_driver_type = IPMI_LOCATE_DRIVER_NONE;
	  locate_info->locate_driver = 0;
	  locate_info->interface_type = IPMI_INTERFACE_SSIF;
	  ERR ((locate_info->bmc_i2c_dev_name = strdup(driver_device)));
	  locate_info->addr_space_id = IPMI_ADDRESS_SPACE_ID_SMBUS;
	  locate_info->base_addr.bmc_smbus_slave_addr = driver_address;
	  locate_info->reg_space = reg_space;
	}
      else 
	{
	  ERR ((dev->io.inband.locate_info = ipmi_locate (IPMI_INTERFACE_SSIF)));
	  if (driver_device)
	    {
	      if (dev->io.inband.locate_info->bmc_i2c_dev_name)
		free (dev->io.inband.locate_info->bmc_i2c_dev_name);
	      ERR ((dev->io.inband.locate_info->bmc_i2c_dev_name = strdup(driver_device)));
	    }
	  if (driver_address)
	    dev->io.inband.locate_info->base_addr.bmc_smbus_slave_addr = driver_address;
	  if (reg_space)
	    dev->io.inband.locate_info->reg_space = reg_space;
	}
      dev->type = driver_type;
      dev->mode = mode;

      ERR_CLEANUP ((dev->io.inband.ssif_ctx = ipmi_ssif_ctx_create()));
      
      ERR_CLEANUP (!(ipmi_ssif_ctx_set_i2c_device(dev->io.inband.ssif_ctx, 
						  dev->io.inband.locate_info->bmc_i2c_dev_name) < 0));
 
      ERR_CLEANUP (!(ipmi_ssif_ctx_set_ipmb_addr(dev->io.inband.ssif_ctx, 
						 dev->io.inband.locate_info->base_addr.bmc_smbus_slave_addr) < 0));

      if (dev->mode == IPMI_MODE_DEFAULT)
        temp_mode = IPMI_SSIF_MODE_BLOCKING;
      else if (dev->mode == IPMI_MODE_NONBLOCK)
        temp_mode = IPMI_SSIF_MODE_NONBLOCKING;
      else
        temp_mode = IPMI_SSIF_MODE_DEFAULT;
      
      ERR_CLEANUP (!(ipmi_ssif_ctx_set_mode(dev->io.inband.ssif_ctx, temp_mode) < 0));

      ERR_CLEANUP (!(ipmi_ssif_ctx_io_init(dev->io.inband.ssif_ctx) < 0));

      break;
    default:
      errno = EINVAL;
      return (-1);
    }
  
  /* Prepare in-band headers */
  dev->io.inband.rq.tmpl_hdr_ptr = &tmpl_inband_hdr;
  dev->io.inband.rs.tmpl_hdr_ptr = &tmpl_inband_hdr;
  
  FIID_OBJ_CREATE_CLEANUP (dev->io.inband.rq.obj_hdr, *(dev->io.inband.rq.tmpl_hdr_ptr));
  FIID_OBJ_CREATE_CLEANUP (dev->io.inband.rs.obj_hdr, *(dev->io.inband.rs.tmpl_hdr_ptr));
  
  return (0);

 cleanup:
  if (dev)
    ipmi_inband_free (dev);
  return (-1);
}

int 
ipmi_cmd (ipmi_device_t *dev, 
	  uint8_t lun, 
	  uint8_t net_fn, 
	  fiid_obj_t obj_cmd_rq, 
	  fiid_obj_t obj_cmd_rs)
{
  int8_t status;

  if (!dev)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_PACKET_VALID(obj_cmd_rq);

  dev->lun = lun;
  dev->net_fn = net_fn;

  status = 0;
  switch (dev->type)
    {
    case IPMI_DEVICE_LAN:
      status = ipmi_lan_cmd2 (dev, obj_cmd_rq, obj_cmd_rs);
      break;
    case IPMI_DEVICE_KCS:
      status = ipmi_kcs_cmd2 (dev, obj_cmd_rq, obj_cmd_rs);
      break;
    case IPMI_DEVICE_SSIF:
      status = ipmi_ssif_cmd2 (dev, obj_cmd_rq, obj_cmd_rs);
      break;
    case IPMI_DEVICE_SMIC:
    case IPMI_DEVICE_BT:
    default:
      errno = EINVAL;
      status = -1;
    }
  
  return (status);
}

int 
ipmi_cmd_raw (ipmi_device_t *dev, 
	      uint8_t *in, 
	      size_t in_len, 
	      uint8_t *out, 
	      size_t *out_len)
{
  int8_t status;

  if (dev == NULL || in_len < 2)
    {
      errno = EINVAL;
      return (-1);
    }
  
  status = 0;
  switch (dev->type)
    {
    case IPMI_DEVICE_LAN:
      status = ipmi_lan_cmd_raw2 (dev, in, in_len, out, out_len);
      break;
    case IPMI_DEVICE_KCS:
      status = ipmi_kcs_cmd_raw2 (dev, in, in_len, out, out_len);
      break;
    case IPMI_DEVICE_SSIF:
      status = ipmi_ssif_cmd_raw2 (dev, in, in_len, out, out_len);
      break;
    case IPMI_DEVICE_SMIC:
    case IPMI_DEVICE_BT:
    default:
      errno = EINVAL;
      status = -1;
    }
  return (status);
}

static int
ipmi_outofband_close (ipmi_device_t *dev)
{
  fiid_obj_t obj_cmd_rs = NULL;
  int retval = -1;
  
  if (!dev)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_CREATE_CLEANUP (obj_cmd_rs, tmpl_cmd_close_session_rs);

  ERR_CLEANUP (!((retval = ipmi_lan_close_session2 (dev, obj_cmd_rs)) < 0));

  if (dev->io.outofband.local_sockfd)
    close (dev->io.outofband.local_sockfd);
  
  ipmi_outofband_free (dev);
cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rs);
  return (retval);
}

static int 
ipmi_inband_close (ipmi_device_t *dev)
{
  if (dev == NULL)
    {
      errno = EINVAL;
      return (-1);
    }
  
  switch (dev->type)
    {
    case IPMI_DEVICE_KCS:
      break;
    case IPMI_DEVICE_SMIC:
      break;
    case IPMI_DEVICE_BT:
      break;
    case IPMI_DEVICE_SSIF:
      break;
    default:
      errno = EINVAL;
      return (-1);
    }

  ipmi_locate_destroy (dev->io.inband.locate_info);
  ipmi_inband_free (dev);
  
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
