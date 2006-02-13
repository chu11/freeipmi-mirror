/* 
   ipmi-msg-support-cmds-udm.c - IPMI UDM Message Support Commands

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

int8_t 
ipmi_get_channel_number2 (ipmi_device_t *dev, uint8_t channel_medium_type)
{
  if (!dev)
    {
      errno = EINVAL;
      return -1;
    }

  if (channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)
    {
      fiid_obj_t obj_data_rs;
      uint64_t manf_id, prod_id;
      
      FIID_OBJ_ALLOCA (obj_data_rs, tmpl_cmd_get_dev_id_rs);
      
      ERR (ipmi_cmd_get_dev_id (dev, obj_data_rs) == 0);
      
      FIID_OBJ_GET (obj_data_rs, tmpl_cmd_get_dev_id_rs, (uint8_t *)"manf_id.id", &manf_id);
      FIID_OBJ_GET (obj_data_rs, tmpl_cmd_get_dev_id_rs, (uint8_t *)"prod_id", &prod_id);
      
      switch (manf_id)
	{
	case IPMI_MANF_ID_INTEL:
	case 0xB000157: // Intel 
	  switch (prod_id)
	    {
	    case IPMI_PROD_ID_SE7501WV2:
	      return 7;
	    }
	}
    }
  
  {
    fiid_obj_t data_rs;
    uint64_t val;
    int i;
    
    FIID_OBJ_ALLOCA (data_rs, tmpl_get_channel_info_rs);
    
    /* Channel numbers range from 0 - 7 */
    for (i = 0; i < 8; i++)
      {
	if (ipmi_cmd_get_channel_info2 (dev, i, data_rs) != 0)
	  continue;
	
	FIID_OBJ_GET (data_rs, 
		      tmpl_get_channel_info_rs, 
		      (uint8_t *)"channel_medium_type", 
		      &val);
	if ((uint8_t) val == channel_medium_type)
	  {
	    FIID_OBJ_GET (data_rs, 
			  tmpl_get_channel_info_rs, 
			  (uint8_t *)"actual_channel_number", 
			  &val);
	    return (int8_t) val;
	  }
      }
  }
  
  return (-1);
}

int8_t 
ipmi_cmd_get_channel_auth_caps2 (ipmi_device_t *dev, 
				 fiid_obj_t obj_cmd_rs)
{
  ipmi_device_t local_dev;
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  ERR (dev->type == IPMI_DEVICE_LAN);
  local_dev = *dev;
  local_dev.io.outofband.auth_type = IPMI_SESSION_AUTH_TYPE_NONE;
  local_dev.io.outofband.rq.tmpl_hdr_session_ptr = 
    local_dev.io.outofband.rs.tmpl_hdr_session_ptr = &tmpl_hdr_session;
  FIID_OBJ_ALLOCA (local_dev.io.outofband.rq.obj_hdr_session,
		   *(local_dev.io.outofband.rq.tmpl_hdr_session_ptr));
  FIID_OBJ_ALLOCA (local_dev.io.outofband.rs.obj_hdr_session,
		   *(local_dev.io.outofband.rs.tmpl_hdr_session_ptr));
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_cmd_get_channel_auth_caps_rq);
  ERR (fill_cmd_get_channel_auth_caps (IPMI_CHANNEL_CURRENT_CHANNEL,
                                       IPMI_PRIV_LEVEL_USER, 
                                       obj_cmd_rq) == 0);
  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_APP_RQ;
  ERR (ipmi_cmd (&local_dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_cmd_get_channel_auth_caps_rq, 
		 obj_cmd_rs, 
		 tmpl_cmd_get_channel_auth_caps_rs) == 0);
  
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_session_challenge2 (ipmi_device_t *dev, 
				 fiid_obj_t obj_cmd_rs)
{
  ipmi_device_t local_dev;
  fiid_obj_t obj_cmd_rq = NULL;

  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  ERR (dev->type == IPMI_DEVICE_LAN);
  local_dev = *dev;
  local_dev.io.outofband.auth_type = IPMI_SESSION_AUTH_TYPE_NONE;
  local_dev.io.outofband.rq.tmpl_hdr_session_ptr = 
    local_dev.io.outofband.rs.tmpl_hdr_session_ptr = &tmpl_hdr_session;
  FIID_OBJ_ALLOCA (local_dev.io.outofband.rq.obj_hdr_session,
		   *(local_dev.io.outofband.rq.tmpl_hdr_session_ptr));
  FIID_OBJ_ALLOCA (local_dev.io.outofband.rs.obj_hdr_session,
		   *(local_dev.io.outofband.rs.tmpl_hdr_session_ptr));
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_cmd_get_session_challenge_rq);
  ERR (fill_cmd_get_session_challenge (dev->io.outofband.auth_type, 
				       (char *)dev->io.outofband.username,
				       IPMI_SESSION_MAX_USERNAME_LEN,
				       obj_cmd_rq) == 0);

  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_APP_RQ;
  ERR (ipmi_cmd (&local_dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_cmd_get_session_challenge_rq, 
		 obj_cmd_rs, 
		 tmpl_cmd_get_session_challenge_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_activate_session2 (ipmi_device_t *dev, 
			    fiid_obj_t obj_cmd_rs)
{
  uint32_t initial_outbound_seq_num = 0;
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  ERR (dev->type == IPMI_DEVICE_LAN);
  { 
    /* Random number generation */
    unsigned int seedp;
    seedp = (unsigned int) clock () + (unsigned int) time (NULL);
    srand (seedp);
    initial_outbound_seq_num = rand_r (&seedp);
  }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_cmd_activate_session_rq);
  ERR (fill_cmd_activate_session (dev->io.outofband.auth_type, 
				  dev->io.outofband.priv_level, 
				  dev->io.outofband.challenge_string, 
				  IPMI_SESSION_CHALLENGE_STR_LEN, 
				  initial_outbound_seq_num, 
				  obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_cmd_activate_session_rq, 
		 obj_cmd_rs, 
		 tmpl_cmd_activate_session_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_session_priv_level2 (ipmi_device_t *dev, 
				  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  ERR (dev->type == IPMI_DEVICE_LAN);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_cmd_set_session_priv_level_rq);
  ERR (fill_cmd_set_session_priv_level (dev->io.outofband.priv_level, 
					obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_cmd_set_session_priv_level_rq, 
		 obj_cmd_rs, 
		 tmpl_cmd_set_session_priv_level_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_lan_open_session2 (ipmi_device_t *dev)
{
  fiid_obj_t obj_cmd_rs = NULL;
  
  uint64_t supported_auth_type = 0;
  uint64_t temp_session_id = 0;
  uint8_t challenge_str[IPMI_SESSION_CHALLENGE_STR_LEN];
  uint64_t temp_session_seq_num = 0;
  
  if (!dev)
    {
      errno = EINVAL;
      return (-1);
    }

  dev->io.outofband.rq_seq = 0;
  
  FIID_OBJ_ALLOCA (obj_cmd_rs, tmpl_cmd_get_channel_auth_caps_rs);
  ERR (ipmi_cmd_get_channel_auth_caps2 (dev, obj_cmd_rs) == 0);
  switch (dev->io.outofband.auth_type)
    {
    case IPMI_SESSION_AUTH_TYPE_NONE:
      FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_get_channel_auth_caps_rs, 
		    (uint8_t *)"auth_type.none", &supported_auth_type);
      break;
    case IPMI_SESSION_AUTH_TYPE_MD2:
      FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_get_channel_auth_caps_rs, 
		    (uint8_t *)"auth_type.md2", &supported_auth_type);
      break;
    case IPMI_SESSION_AUTH_TYPE_MD5:
      FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_get_channel_auth_caps_rs, 
		    (uint8_t *)"auth_type.md5", &supported_auth_type);
      break;
    case IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY:
      FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_get_channel_auth_caps_rs, 
		    (uint8_t *)"auth_type.straight_passwd_key", &supported_auth_type);
      break;
    case IPMI_SESSION_AUTH_TYPE_OEM_PROP:
      FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_get_channel_auth_caps_rs, 
		    (uint8_t *)"auth_type.oem_prop", &supported_auth_type);
      break;
    default:
      errno = EINVAL;
      return (-1);
    }
  if (supported_auth_type == 0)
    {
      errno = ENOTSUP;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rs, tmpl_cmd_get_session_challenge_rs);
  ERR (ipmi_cmd_get_session_challenge2 (dev, obj_cmd_rs) == 0);
  FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_get_session_challenge_rs, 
		(uint8_t *)"tmp_session_id", &temp_session_id);
  dev->io.outofband.session_id = temp_session_id;
  ERR (fiid_obj_get_data (obj_cmd_rs, 
			  tmpl_cmd_get_session_challenge_rs, 
			  (uint8_t *)"challenge_str", 
			  challenge_str,
                          IPMI_SESSION_CHALLENGE_STR_LEN) == 0);
  memcpy (dev->io.outofband.challenge_string, 
	  challenge_str, 
	  IPMI_SESSION_CHALLENGE_STR_LEN);

  FIID_OBJ_ALLOCA (obj_cmd_rs, tmpl_cmd_activate_session_rs);
  ERR (ipmi_cmd_activate_session2 (dev, obj_cmd_rs) == 0);
  FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_activate_session_rs, 
		(uint8_t *)"session_id", &temp_session_id);
  dev->io.outofband.session_id = temp_session_id;
  FIID_OBJ_GET (obj_cmd_rs, tmpl_cmd_activate_session_rs, 
		(uint8_t *)"initial_inbound_seq_num", &temp_session_seq_num);
  dev->io.outofband.session_seq_num = temp_session_seq_num;
  
  FIID_OBJ_ALLOCA (obj_cmd_rs, tmpl_cmd_set_session_priv_level_rs);
  ERR (ipmi_cmd_set_session_priv_level2 (dev, obj_cmd_rs) == 0);
  
  return (0);
}

int8_t 
ipmi_lan_close_session2 (ipmi_device_t *dev, 
			 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  ERR (dev != NULL);
  ERR (obj_cmd_rs != NULL);
  
  ERR (dev->type == IPMI_DEVICE_LAN);
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_cmd_close_session_rq);
  ERR (fill_cmd_close_session (dev->io.outofband.session_id, 
			       obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_cmd_close_session_rq, 
		 obj_cmd_rs, 
		 tmpl_cmd_close_session_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_channel_access2 (ipmi_device_t *dev, 
			      uint8_t channel_number, 
			      uint8_t ipmi_messaging_access_mode, 
			      uint8_t user_level_authentication, 
			      uint8_t per_message_authentication, 
			      uint8_t pef_alerting, 
			      uint8_t channel_access_set_flag, 
			      uint8_t channel_privilege_level_limit, 
			      uint8_t channel_privilege_level_limit_set_flag, 
			      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_channel_access_rq);
  ERR (fill_kcs_set_channel_access (channel_number, 
				    ipmi_messaging_access_mode, 
				    user_level_authentication, 
				    per_message_authentication, 
				    pef_alerting, 
				    channel_access_set_flag, 
				    channel_privilege_level_limit, 
				    channel_privilege_level_limit_set_flag,
                                    obj_cmd_rq) == 0); 
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_channel_access_rq, 
		 obj_cmd_rs, 
		 tmpl_set_channel_access_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_user_name2 (ipmi_device_t *dev, 
			 uint8_t user_id, 
			 char *user_name, 
			 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_user_name_rq);
  ERR (fill_kcs_set_user_name (user_id, 
			       user_name, 
			       ((user_name) ? strlen (user_name) : 0),
                               obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_user_name_rq, 
		 obj_cmd_rs, 
		 tmpl_set_user_name_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_user_name2 (ipmi_device_t *dev, 
			 uint8_t user_id, 
			 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_user_name_rq);
  ERR (fill_kcs_get_user_name (user_id, obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_user_name_rq, 
		 obj_cmd_rs, 
		 tmpl_get_user_name_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_user_password2 (ipmi_device_t *dev, 
			     uint8_t user_id, 
			     uint8_t operation, 
			     char *user_password,
			     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_user_password_rq);
  ERR (fill_kcs_set_user_password (user_id, 
				   operation, 
				   user_password, 
				   ((user_password) ? strlen(user_password) : 0),
                                   obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_user_password_rq, 
		 obj_cmd_rs, 
		 tmpl_set_user_password_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_set_user_access2 (ipmi_device_t *dev, 
			   uint8_t channel_number,
			   uint8_t user_id,
			   uint8_t restrict_to_callback,
			   uint8_t enable_link_auth,
			   uint8_t enable_ipmi_msgs,
			   uint8_t user_privilege_level_limit,
			   uint8_t user_session_number_limit, 
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_set_user_access_rq);
  ERR (fill_kcs_set_user_access (channel_number,
				 user_id,
				 restrict_to_callback,
				 enable_link_auth,
				 enable_ipmi_msgs,
				 user_privilege_level_limit,
				 user_session_number_limit,
                                 obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_set_user_access_rq, 
		 obj_cmd_rs, 
		 tmpl_set_user_access_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_user_access2 (ipmi_device_t *dev, 
			   uint8_t channel_number,
			   uint8_t user_id,
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_user_access_rq);
  ERR (fill_kcs_get_user_access (channel_number, user_id, obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_user_access_rq, 
		 obj_cmd_rs, 
		 tmpl_get_user_access_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_channel_access2 (ipmi_device_t *dev, 
			      uint8_t channel_number,
			      uint8_t channel_access_set_flag,
			      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_channel_access_rq);
  ERR (fill_kcs_get_channel_access (channel_number, 
				    channel_access_set_flag,
                                    obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_channel_access_rq, 
		 obj_cmd_rs, 
		 tmpl_get_channel_access_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_channel_info2 (ipmi_device_t *dev, 
			    uint8_t channel_number,
			    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_channel_info_rq);
  ERR (fill_kcs_get_channel_info (channel_number, obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_APP_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_channel_info_rq, 
		 obj_cmd_rs, 
		 tmpl_get_channel_info_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

