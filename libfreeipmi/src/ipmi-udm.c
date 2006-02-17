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

#include "freeipmi.h"

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

  fiid_obj_destroy (dev->io.outofband.rq.obj_rmcp_hdr);
  fiid_obj_destroy (dev->io.outofband.rs.obj_rmcp_hdr);
  fiid_obj_destroy (dev->io.outofband.rq.obj_lan_session_hdr);
  fiid_obj_destroy (dev->io.outofband.rs.obj_lan_session_hdr);
  fiid_obj_destroy (dev->io.outofband.rq.obj_lan_msg_hdr);
  fiid_obj_destroy (dev->io.outofband.rs.obj_lan_msg_hdr);
  fiid_obj_destroy (dev->io.outofband.rq.obj_lan_msg_trlr);
  fiid_obj_destroy (dev->io.outofband.rs.obj_lan_msg_trlr);
}

static void 
ipmi_inband_free (ipmi_device_t *dev)
{
  if (!dev)
    {
      errno = EINVAL;
      return;
    }
  
  fiid_obj_destroy (dev->io.inband.rq.obj_hdr);
  fiid_obj_destroy (dev->io.inband.rs.obj_hdr);
  ipmi_xfree (dev->io.inband.driver_device);
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
  
  if (dev == NULL)
    {
      errno = EINVAL;
      return (-1);
    }
  
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
  
  if (IPMI_1_5_AUTHENTICATION_TYPE_VALID (authentication_type) == 0)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if (username != NULL && 
      strlen (username) > IPMI_MAX_USER_NAME_LENGTH)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if (password != NULL && 
      strlen (password) > IPMI_MAX_AUTHENTICATION_CODE_LENGTH)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if (IPMI_1_5_PRIVILEGE_LEVEL_VALID (privilege_level) == 0)
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
  
  dev->io.outofband.rq.obj_rmcp_hdr =
    fiid_obj_create (*(dev->io.outofband.rq.tmpl_rmcp_hdr_ptr));
  if (dev->io.outofband.rq.obj_rmcp_hdr == NULL)
    {
      ipmi_outofband_free (dev);
      return (-1);
    }
  dev->io.outofband.rs.obj_rmcp_hdr =
    fiid_obj_create (*(dev->io.outofband.rs.tmpl_rmcp_hdr_ptr));
  if (dev->io.outofband.rs.obj_rmcp_hdr == NULL)
    {
      ipmi_outofband_free (dev);
      return (-1);
    }

  dev->io.outofband.rq.obj_lan_session_hdr =
    fiid_obj_create (*(dev->io.outofband.rq.tmpl_lan_session_hdr_ptr));
  if (dev->io.outofband.rq.obj_lan_session_hdr == NULL)
    {
      ipmi_outofband_free (dev);
      return (-1);
    }
  dev->io.outofband.rs.obj_lan_session_hdr =
    fiid_obj_create (*(dev->io.outofband.rs.tmpl_lan_session_hdr_ptr));
  if (dev->io.outofband.rs.obj_lan_session_hdr == NULL)
    {
      ipmi_outofband_free (dev);
      return (-1);
    }

  dev->io.outofband.rq.obj_lan_msg_hdr =
    fiid_obj_create (*(dev->io.outofband.rq.tmpl_lan_msg_hdr_ptr));
  if (dev->io.outofband.rq.obj_lan_msg_hdr == NULL)
    {
      ipmi_outofband_free (dev);
      return (-1);
    }
  dev->io.outofband.rs.obj_lan_msg_hdr =
    fiid_obj_create (*(dev->io.outofband.rs.tmpl_lan_msg_hdr_ptr));
  if (dev->io.outofband.rs.obj_lan_msg_hdr == NULL)
    {
      ipmi_outofband_free (dev);
      return (-1);
    }

  dev->io.outofband.rq.obj_lan_msg_trlr =
    fiid_obj_create (*(dev->io.outofband.rq.tmpl_lan_msg_trlr_ptr));
  if (dev->io.outofband.rq.obj_lan_msg_trlr == NULL)
    {
      ipmi_outofband_free (dev);
      return (-1);
    }
  dev->io.outofband.rs.obj_lan_msg_trlr =
    fiid_obj_create (*(dev->io.outofband.rs.tmpl_lan_msg_trlr_ptr));
  if (dev->io.outofband.rs.obj_lan_msg_trlr == NULL)
    {
      ipmi_outofband_free (dev);
      return (-1);
    }
  
  /* Open client (local) UDP socket */
  if ((dev->io.outofband.local_sockfd = ipmi_open_free_udp_port ()) == -1)
    {
      ipmi_outofband_free (dev);
      return (-1);
    }
  /* Note that ipmi_lan_open_session itself calls ipmi_lan_cmd many
     times internally, at this point everything must be set to go
     -- Anand Babu */
  if ((status = ipmi_lan_open_session2 (dev)) == -1)
    {
      if (dev->io.outofband.local_sockfd)
	close (dev->io.outofband.local_sockfd);
      ipmi_outofband_free (dev);
      return (-1);
    }
  
  return (0);
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
 
  switch (driver_type)
    {
    case IPMI_DEVICE_KCS:
      dev->io.inband.disable_auto_probe = disable_auto_probe;
      if (dev->io.inband.disable_auto_probe)
	{
	  ipmi_locate_info_t *locate_info = &(dev->io.inband.locate_info);
	  
	  locate_info->ipmi_ver_major = 1;
	  locate_info->ipmi_ver_minor = 5;
	  locate_info->locate_driver_type = IPMI_LOCATE_DRIVER_NONE;
	  locate_info->locate_driver = 0;
	  locate_info->interface_type = IPMI_INTERFACE_KCS;
	  locate_info->bmc_i2c_dev_name = driver_device;
	  locate_info->addr_space_id = IPMI_ADDRESS_SPACE_ID_SYSTEM_IO;
	  locate_info->base_addr.bmc_iobase_addr = driver_address;
	  locate_info->reg_space = reg_space;
	}
      else 
	{
	  ERR (ipmi_locate (IPMI_INTERFACE_KCS, &(dev->io.inband.locate_info)) != NULL);
	  if (driver_device)
	    dev->io.inband.locate_info.bmc_i2c_dev_name = driver_device;
	  if (driver_address)
	    dev->io.inband.locate_info.base_addr.bmc_iobase_addr = driver_address;
	  if (reg_space)
	    dev->io.inband.locate_info.reg_space = reg_space;
	}
      dev->type = driver_type;
      dev->mode = mode;
      
      /* At this point we only support SYSTEM_IO, i.e. inb/outb style IO. 
	 If we cant find the bass address, we better exit. -- Anand Babu */
      if (dev->io.inband.locate_info.addr_space_id != IPMI_ADDRESS_SPACE_ID_SYSTEM_IO)
	{
	  errno = ENODEV;
	  return (-1);
	}

      if (!(dev->io.inband.kcs_ctx = ipmi_kcs_ctx_create()))
        return (-1);
      
      if (ipmi_kcs_ctx_set_bmc_iobase_addr(dev->io.inband.kcs_ctx, 
                                           dev->io.inband.locate_info.base_addr.bmc_iobase_addr) < 0)
        {
	  ipmi_kcs_ctx_destroy(dev->io.inband.kcs_ctx);
          ipmi_inband_free (dev);
          return (-1);
        }

      if (ipmi_kcs_ctx_set_register_space(dev->io.inband.kcs_ctx, 
                                          dev->io.inband.locate_info.reg_space) < 0)
        {
	  ipmi_kcs_ctx_destroy(dev->io.inband.kcs_ctx);
          ipmi_inband_free (dev);
          return (-1);
        }

      if (ipmi_kcs_ctx_set_poll_interval(dev->io.inband.kcs_ctx, 
                                         IPMI_POLL_INTERVAL_USECS) < 0)
        {
	  ipmi_kcs_ctx_destroy(dev->io.inband.kcs_ctx);
          ipmi_inband_free (dev);
          return (-1);
        }

      if (dev->mode == IPMI_MODE_DEFAULT)
        temp_mode = IPMI_KCS_MODE_BLOCKING;
      else if (dev->mode == IPMI_MODE_NONBLOCK)
        temp_mode = IPMI_KCS_MODE_NONBLOCKING;
      else
        temp_mode = IPMI_KCS_MODE_DEFAULT;
      
      if (ipmi_kcs_ctx_set_mode(dev->io.inband.kcs_ctx, 
                                temp_mode) < 0)
        {
	  ipmi_kcs_ctx_destroy(dev->io.inband.kcs_ctx);
          ipmi_inband_free (dev);
          return (-1);
        }

      if (ipmi_kcs_ctx_io_init(dev->io.inband.kcs_ctx) < 0)
        {
	  ipmi_kcs_ctx_destroy(dev->io.inband.kcs_ctx);
          ipmi_inband_free (dev);
          return (-1);
        }

      break;
    case IPMI_DEVICE_SMIC:
      ERR (ipmi_locate (IPMI_INTERFACE_SMIC, &(dev->io.inband.locate_info)) != NULL);
      break;
    case IPMI_DEVICE_BT:
      errno = ENOTSUP;
      return (-1);
    case IPMI_DEVICE_SSIF:
      dev->io.inband.disable_auto_probe = disable_auto_probe;
      if (dev->io.inband.disable_auto_probe)
	{
	  ipmi_locate_info_t *locate_info = &(dev->io.inband.locate_info);
	  
	  locate_info->ipmi_ver_major = 1;
	  locate_info->ipmi_ver_minor = 5;
	  locate_info->locate_driver_type = IPMI_LOCATE_DRIVER_NONE;
	  locate_info->locate_driver = 0;
	  locate_info->interface_type = IPMI_INTERFACE_SSIF;
	  locate_info->bmc_i2c_dev_name = driver_device;
	  locate_info->addr_space_id = IPMI_ADDRESS_SPACE_ID_SMBUS;
	  locate_info->base_addr.bmc_smbus_slave_addr = driver_address;
	  locate_info->reg_space = reg_space;
	}
      else 
	{
	  ERR (ipmi_locate (IPMI_INTERFACE_SSIF, &(dev->io.inband.locate_info)) != NULL);
	  if (driver_device)
	    dev->io.inband.locate_info.bmc_i2c_dev_name = driver_device;
	  if (driver_address)
	    dev->io.inband.locate_info.base_addr.bmc_smbus_slave_addr = driver_address;
	  if (reg_space)
	    dev->io.inband.locate_info.reg_space = reg_space;
	}
      dev->io.inband.driver_device = dev->io.inband.locate_info.bmc_i2c_dev_name;
      dev->io.inband.driver_address = dev->io.inband.locate_info.base_addr.bmc_smbus_slave_addr;
      /* dev->io.inband.driver_address = 0x341A; */
      dev->type = driver_type;
      dev->mode = mode;

      if (!(dev->io.inband.ssif_ctx = ipmi_ssif_ctx_create()))
        return (-1);
      
      if (ipmi_ssif_ctx_set_i2c_device(dev->io.inband.ssif_ctx, 
				       dev->io.inband.driver_device) < 0)
        {
	  ipmi_ssif_ctx_destroy(dev->io.inband.ssif_ctx);
          ipmi_inband_free (dev);
          return (-1);
        }
 
      if (ipmi_ssif_ctx_set_ipmb_addr(dev->io.inband.ssif_ctx, 
				      dev->io.inband.driver_address) < 0)
        {
	  ipmi_ssif_ctx_destroy(dev->io.inband.ssif_ctx);
          ipmi_inband_free (dev);
          return (-1);
        }

      if (dev->mode == IPMI_MODE_DEFAULT)
        temp_mode = IPMI_SSIF_MODE_BLOCKING;
      else if (dev->mode == IPMI_MODE_NONBLOCK)
        temp_mode = IPMI_SSIF_MODE_NONBLOCKING;
      else
        temp_mode = IPMI_SSIF_MODE_DEFAULT;
      
      if (ipmi_ssif_ctx_set_mode(dev->io.inband.ssif_ctx, 
                                temp_mode) < 0)
        {
	  ipmi_ssif_ctx_destroy(dev->io.inband.ssif_ctx);
          ipmi_inband_free (dev);
          return (-1);
        }

      if (ipmi_ssif_ctx_io_init(dev->io.inband.ssif_ctx) < 0)
        {
	  ipmi_ssif_ctx_destroy(dev->io.inband.ssif_ctx);
          ipmi_inband_free (dev);
          return (-1);
        }

      break;
    default:
      errno = EINVAL;
      return (-1);
    }
  
  /* Prepare in-band headers */
  dev->io.inband.rq.tmpl_hdr_ptr = &tmpl_inband_hdr;
  dev->io.inband.rs.tmpl_hdr_ptr = &tmpl_inband_hdr;
  
  dev->io.inband.rq.obj_hdr =
    fiid_obj_create (*(dev->io.inband.rq.tmpl_hdr_ptr));
  if (dev->io.inband.rq.obj_hdr == NULL)
    {
      ipmi_inband_free (dev);
      return (-1);
    }
  dev->io.inband.rs.obj_hdr =
    fiid_obj_create (*(dev->io.inband.rs.tmpl_hdr_ptr));
  if (dev->io.inband.rs.obj_hdr == NULL)
    {
      ipmi_inband_free (dev);
      return (-1);
    }
  
  return (0);
}

int 
ipmi_cmd (ipmi_device_t *dev, 
	  uint8_t lun, 
	  uint8_t net_fn, 
	  fiid_obj_t obj_cmd_rq, 
	  fiid_obj_t obj_cmd_rs)
{
  int8_t status, rv;

  if ((rv = fiid_obj_packet_valid(obj_cmd_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return (-1);
    }

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
  int retval = 0;
  fiid_obj_t obj_cmd_rs = NULL;
  
  if (!(obj_cmd_rs = fiid_obj_create (tmpl_cmd_close_session_rs)))
    return (-1);

  if ((retval = ipmi_lan_close_session2 (dev, obj_cmd_rs)) < 0)
    {
      fiid_obj_destroy(obj_cmd_rs);
      return (-1);
    }

  if (dev->io.outofband.local_sockfd)
    close (dev->io.outofband.local_sockfd);
  
  ipmi_outofband_free (dev);
  fiid_obj_destroy(obj_cmd_rs);
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
      ipmi_kcs_ctx_destroy(dev->io.inband.kcs_ctx);
      break;
    case IPMI_DEVICE_SMIC:
      break;
    case IPMI_DEVICE_BT:
      break;
    case IPMI_DEVICE_SSIF:
      ipmi_ssif_ctx_destroy(dev->io.inband.ssif_ctx);
      break;
    default:
      errno = EINVAL;
      return (-1);
    }
  ipmi_locate_free (&(dev->io.inband.locate_info));
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

