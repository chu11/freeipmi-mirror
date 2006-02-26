/* 
   ipmi-messaging-support-cmds-udm.c - IPMI UDM Message Support Commands

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
#include <errno.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#ifdef __FreeBSD__
#include <sys/time.h>
#else  /* !__FreeBSD */
#include <time.h>
#endif /* !__FreeBSD */
#endif /* !HAVE_SYS_TIME_H */
#endif	/* !TIME_WITH_SYS_TIME */

#include "freeipmi/ipmi-messaging-support-cmds-udm.h"
#include "freeipmi/ipmi-messaging-support-cmds.h"
#include "freeipmi/ipmi-device-global-cmds.h"
#include "freeipmi/ipmi-device-global-cmds-udm.h"
#include "freeipmi/ipmi-ipmb-interface.h"
#include "freeipmi/ipmi-netfn-spec.h"

#include "freeipmi-portability.h"
#include "err-wrappers.h"
#include "fiid-wrappers.h"

int8_t 
ipmi_cmd_get_channel_authentication_capabilities2 (ipmi_device_t *dev, 
                                                   fiid_obj_t obj_cmd_rs)
{
  ipmi_device_t local_dev;
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev 
      || dev->type != IPMI_DEVICE_LAN
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_channel_authentication_capabilities_rs);
  
  local_dev = *dev;
  local_dev.io.outofband.authentication_type = IPMI_AUTHENTICATION_TYPE_NONE;

  FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_get_channel_authentication_capabilities_rq);

  ERR_CLEANUP (!(fill_cmd_get_channel_authentication_capabilities (IPMI_CHANNEL_CURRENT_CHANNEL,
								   IPMI_PRIVILEGE_LEVEL_USER, 
								   obj_cmd_rq) < 0));

  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_APP_RQ;
  ERR_IPMI_CMD_CLEANUP (&local_dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_APP_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_session_challenge2 (ipmi_device_t *dev, 
				 fiid_obj_t obj_cmd_rs)
{
  ipmi_device_t local_dev;
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev 
      || dev->type != IPMI_DEVICE_LAN
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_get_session_challenge_rs);
  
  local_dev = *dev;
  local_dev.io.outofband.authentication_type = IPMI_AUTHENTICATION_TYPE_NONE;

  FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_get_session_challenge_rq);

  ERR_CLEANUP (!(fill_cmd_get_session_challenge (dev->io.outofband.authentication_type, 
						 (char *)dev->io.outofband.username,
						 IPMI_MAX_USER_NAME_LENGTH,
						 obj_cmd_rq) < 0));

  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_APP_RQ;
  ERR_IPMI_CMD_CLEANUP (&local_dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_APP_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_activate_session2 (ipmi_device_t *dev, 
			    fiid_obj_t obj_cmd_rs)
{
  uint32_t initial_outbound_sequence_number = 0;
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev 
      || dev->type != IPMI_DEVICE_LAN
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_activate_session_rs);
  
  { 
    /* Random number generation */
    unsigned int seedp;
    seedp = (unsigned int) clock () + (unsigned int) time (NULL);
    srand (seedp);
    initial_outbound_sequence_number = rand_r (&seedp);
  }

  FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_activate_session_rq);

  ERR_CLEANUP (!(fill_cmd_activate_session (dev->io.outofband.authentication_type, 
					    dev->io.outofband.privilege_level, 
					    dev->io.outofband.challenge_string, 
					    IPMI_CHALLENGE_STRING_LENGTH, 
					    initial_outbound_sequence_number, 
					    obj_cmd_rq) < 0));

  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_APP_RQ;
  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_APP_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_session_privilege_level2 (ipmi_device_t *dev, 
                                       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev 
      || dev->type != IPMI_DEVICE_LAN
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_set_session_privilege_level_rs);
  
  FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_set_session_privilege_level_rq);

  ERR_CLEANUP (!(fill_cmd_set_session_privilege_level (dev->io.outofband.privilege_level, 
						       obj_cmd_rq) < 0));

  dev->lun = IPMI_BMC_IPMB_LUN_BMC;
  dev->net_fn = IPMI_NET_FN_APP_RQ;
  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_APP_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_lan_close_session2 (ipmi_device_t *dev, 
			 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev
      || dev->type != IPMI_DEVICE_LAN
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_cmd_close_session_rs);
  
  FIID_OBJ_CREATE (obj_cmd_rq, tmpl_cmd_close_session_rq);

  ERR_CLEANUP (!(fill_cmd_close_session (dev->io.outofband.session_id, obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev,
			IPMI_BMC_IPMB_LUN_BMC,
			IPMI_NET_FN_APP_RQ,
			obj_cmd_rq,
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_channel_access2 (ipmi_device_t *dev, 
			      uint8_t channel_number, 
			      uint8_t ipmi_messaging_access_mode, 
			      uint8_t user_level_authentication, 
			      uint8_t per_message_authentication, 
			      uint8_t pef_alerting, 
			      uint8_t channel_access_set, 
			      uint8_t channel_privilege_level_limit, 
			      uint8_t channel_privilege_level_limit_set, 
			      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_MESSAGING_ACCESS_MODE_VALID(ipmi_messaging_access_mode)
      || !IPMI_USER_LEVEL_AUTHENTICATION_VALID(user_level_authentication)
      || !IPMI_PER_MESSAGE_AUTHENTICATION_VALID(per_message_authentication)
      || !IPMI_PEF_ALERTING_VALID(pef_alerting)
      || !IPMI_CHANNEL_ACCESS_VALID(channel_access_set)
      || !IPMI_PRIVILEGE_LEVEL_VALID(channel_privilege_level_limit)
      || !IPMI_PRIVILEGE_LEVEL_LIMIT_SET_VALID(channel_privilege_level_limit_set)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_channel_access_rs);
  
  FIID_OBJ_CREATE (obj_cmd_rq, tmpl_set_channel_access_rq);

  ERR_CLEANUP (!(fill_cmd_set_channel_access (channel_number, 
					      ipmi_messaging_access_mode, 
					      user_level_authentication, 
					      per_message_authentication, 
					      pef_alerting, 
					      channel_access_set, 
					      channel_privilege_level_limit, 
					      channel_privilege_level_limit_set,
					      obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_APP_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_channel_access2 (ipmi_device_t *dev, 
			      uint8_t channel_number,
			      uint8_t channel_access_get,
			      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_CHANNEL_ACCESS_GET_VALID(channel_access_get)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_channel_access_rs);
  
  FIID_OBJ_CREATE (obj_cmd_rq, tmpl_get_channel_access_rq);

  ERR_CLEANUP (!(fill_cmd_get_channel_access (channel_number, 
					      channel_access_get,
					      obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_APP_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_channel_info2 (ipmi_device_t *dev, 
			    uint8_t channel_number,
			    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_channel_info_rs);
  
  FIID_OBJ_CREATE (obj_cmd_rq, tmpl_get_channel_info_rq);

  ERR_CLEANUP (!(fill_cmd_get_channel_info (channel_number, obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev,
			IPMI_BMC_IPMB_LUN_BMC,
			IPMI_NET_FN_APP_RQ,
			obj_cmd_rq,
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_user_access2 (ipmi_device_t *dev, 
			   uint8_t channel_number,
                           uint8_t user_ipmi_messaging,
                           uint8_t user_link_authentication,
                           uint8_t user_restricted_to_callback,
			   uint8_t user_id,
			   uint8_t user_privilege_level_limit,
			   uint8_t user_session_number_limit, 
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_USER_IPMI_MESSAGING_VALID(user_ipmi_messaging)
      || !IPMI_USER_LINK_AUTHENTICATION_VALID(user_link_authentication)
      || !IPMI_USER_RESTRICTED_TO_CALLBACK_VALID(user_restricted_to_callback)
      || !IPMI_PRIVILEGE_LEVEL_LIMIT_VALID(user_privilege_level_limit)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_user_access_rs);
  
  FIID_OBJ_CREATE (obj_cmd_rq, tmpl_set_user_access_rq);

  ERR_CLEANUP (!(fill_cmd_set_user_access (channel_number,
					   user_ipmi_messaging,
					   user_link_authentication,
					   user_restricted_to_callback,
					   user_id,
					   user_privilege_level_limit,
					   user_session_number_limit,
					   obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_APP_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_user_access2 (ipmi_device_t *dev, 
			   uint8_t channel_number,
			   uint8_t user_id,
			   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_user_access_rs);
  
  FIID_OBJ_CREATE (obj_cmd_rq, tmpl_get_user_access_rq);

  ERR_CLEANUP (!(fill_cmd_get_user_access (channel_number, user_id, obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_APP_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_user_name2 (ipmi_device_t *dev, 
			 uint8_t user_id, 
			 char *user_name, 
                         unsigned int user_name_len,
			 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev
      || (user_name && user_name_len > IPMI_MAX_USER_NAME_LENGTH)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_user_name_rs);
  
  FIID_OBJ_CREATE (obj_cmd_rq, tmpl_set_user_name_rq);

  ERR_CLEANUP (!(fill_cmd_set_user_name (user_id, 
					 user_name, 
					 user_name_len,
					 obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_APP_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_user_name2 (ipmi_device_t *dev, 
			 uint8_t user_id, 
			 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_user_name_rs);
  
  FIID_OBJ_CREATE (obj_cmd_rq, tmpl_get_user_name_rq);

  ERR_CLEANUP (!(fill_cmd_get_user_name (user_id, obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_APP_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_user_password2 (ipmi_device_t *dev, 
			     uint8_t user_id, 
			     uint8_t operation, 
			     char *password,
                             unsigned int password_len,
			     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev
      || !IPMI_PASSWORD_OPERATION_VALID(operation)
      || (password && password_len > IPMI_MAX_AUTHENTICATION_CODE_LENGTH)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_set_user_password_rs);
  
  FIID_OBJ_CREATE (obj_cmd_rq, tmpl_set_user_password_rq);

  ERR_CLEANUP (!(fill_cmd_set_user_password (user_id, 
					     operation, 
					     password, 
					     password_len,
					     obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_APP_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t
ipmi_get_channel_number2 (ipmi_device_t *dev, uint8_t channel_medium_type)
{
  fiid_obj_t obj_data_rs = NULL;
  uint64_t manufacturer_id, product_id;
  int8_t rv = -1;
  uint64_t val;
  int i;
    
  if (channel_medium_type == IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3)
    {
      FIID_OBJ_CREATE_CLEANUP(obj_data_rs, tmpl_cmd_get_device_id_rs);
      
      ERR_CLEANUP (!(ipmi_cmd_get_device_id (dev, obj_data_rs) < 0));
      
      FIID_OBJ_GET_CLEANUP (obj_data_rs, (uint8_t *)"manufacturer_id.id", &manufacturer_id);

      FIID_OBJ_GET_CLEANUP (obj_data_rs, (uint8_t *)"product_id", &product_id);
      
      switch (manufacturer_id)
	{
	case IPMI_MANUFACTURER_ID_INTEL:
	case 0xB000157: // Intel 
	  switch (product_id)
	    {
	    case IPMI_PRODUCT_ID_SE7501WV2:
	      rv = 7;
	      goto cleanup;
	    }
	}

      FIID_OBJ_DESTROY_NO_RETURN(obj_data_rs);
    }
  
  FIID_OBJ_CREATE_CLEANUP(obj_data_rs, tmpl_get_channel_info_rs);
  
  /* Channel numbers range from 0 - 7 */
  for (i = 0; i < 8; i++)
    {
      if (ipmi_cmd_get_channel_info2 (dev, i, obj_data_rs) != 0)
	continue;
	
      FIID_OBJ_GET_CLEANUP (obj_data_rs, (uint8_t *)"channel_medium_type", &val);
      
      if ((uint8_t) val == channel_medium_type)
	{
	  FIID_OBJ_GET_CLEANUP (obj_data_rs, (uint8_t *)"actual_channel_number", &val);
	  
	  rv = (int8_t) val;
	  break;
	}
    }

 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_data_rs);
  return (rv);
}

int8_t 
ipmi_lan_open_session2 (ipmi_device_t *dev)
{
  fiid_obj_t obj_cmd_rs = NULL;
  
  uint64_t supported_authentication_type = 0;
  uint64_t temp_session_id = 0;
  uint8_t challenge_string[IPMI_CHALLENGE_STRING_LENGTH];
  uint64_t temp_session_sequence_number = 0;
  
  if (!dev)
    {
      errno = EINVAL;
      return (-1);
    }

  dev->io.outofband.rq_seq = 0;
  
  FIID_OBJ_CREATE_CLEANUP(obj_cmd_rs, tmpl_cmd_get_channel_authentication_capabilities_rs);
  
  ERR_CLEANUP (!(ipmi_cmd_get_channel_authentication_capabilities2 (dev, obj_cmd_rs) < 0));

  switch (dev->io.outofband.authentication_type)
    {
    case IPMI_AUTHENTICATION_TYPE_NONE:
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    (uint8_t *)"authentication_type.none", 
			    &supported_authentication_type);
      break;
    case IPMI_AUTHENTICATION_TYPE_MD2:
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    (uint8_t *)"authentication_type.md2", 
			    &supported_authentication_type);
      break;
    case IPMI_AUTHENTICATION_TYPE_MD5:
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    (uint8_t *)"authentication_type.md5", 
			    &supported_authentication_type);
      break;
    case IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY:
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    (uint8_t *)"authentication_type.straight_password_key", 
			    &supported_authentication_type);
      break;
    case IPMI_AUTHENTICATION_TYPE_OEM_PROP:
      FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			    (uint8_t *)"authentication_type.oem_prop", 
			    &supported_authentication_type);
      break;
    default:
      errno = EINVAL;
      return (-1);
    }

  if (supported_authentication_type == 0)
    {
      errno = ENOTSUP;
      goto cleanup;
    }
  
  FIID_OBJ_DESTROY(obj_cmd_rs);
  FIID_OBJ_CREATE_CLEANUP (obj_cmd_rs, tmpl_cmd_get_session_challenge_rs);
  ERR_CLEANUP (!(ipmi_cmd_get_session_challenge2 (dev, obj_cmd_rs) < 0));

  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			(uint8_t *)"temp_session_id", 
			&temp_session_id);

  dev->io.outofband.session_id = temp_session_id;
  FIID_OBJ_GET_DATA_CLEANUP (obj_cmd_rs, 
			     (uint8_t *)"challenge_string", 
			     challenge_string,
			     IPMI_CHALLENGE_STRING_LENGTH);

  memcpy (dev->io.outofband.challenge_string, 
	  challenge_string, 
	  IPMI_CHALLENGE_STRING_LENGTH);

  FIID_OBJ_DESTROY(obj_cmd_rs);
  FIID_OBJ_CREATE_CLEANUP (obj_cmd_rs, tmpl_cmd_activate_session_rs);
  ERR_CLEANUP (!(ipmi_cmd_activate_session2 (dev, obj_cmd_rs) < 0));

  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			(uint8_t *)"session_id", 
			&temp_session_id);

  dev->io.outofband.session_id = temp_session_id;
  FIID_OBJ_GET_CLEANUP (obj_cmd_rs, 
			(uint8_t *)"initial_inbound_sequence_number", 
			&temp_session_sequence_number);
  dev->io.outofband.session_sequence_number = temp_session_sequence_number;
  
  FIID_OBJ_DESTROY(obj_cmd_rs);
  FIID_OBJ_CREATE_CLEANUP (obj_cmd_rs, tmpl_cmd_set_session_privilege_level_rs);
  ERR_CLEANUP (!(ipmi_cmd_set_session_privilege_level2 (dev, obj_cmd_rs) < 0));

  FIID_OBJ_DESTROY(obj_cmd_rs);
  return (0);
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rs);
  return (-1);
}
