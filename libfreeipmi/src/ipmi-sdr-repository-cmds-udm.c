/* 
   ipmi-sdr-repository-cmds-udm.c - IPMI UDM SDR Repository commands

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

#include "freeipmi-build.h"
#include "err-wrappers.h"
#include "fiid-wrappers.h"

int8_t 
ipmi_cmd_get_sdr_repository_info2 (ipmi_device_t *dev, 
                                   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_sdr_repository_info_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_sdr_repository_info_rq);

  ERR_CLEANUP (!(fill_cmd_get_repository_info (obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_STORAGE_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_sdr_repository_allocation_info2 (ipmi_device_t *dev, 
                                              fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return -1;
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_sdr_repository_allocation_info_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_sdr_repository_allocation_info_rq);

  ERR_CLEANUP (!(fill_cmd_get_repository_allocation_info (obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_STORAGE_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_reserve_sdr_repository2 (ipmi_device_t *dev, 
                                  fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return -1;
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_reserve_sdr_repository_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_reserve_sdr_repository_rq);

  ERR_CLEANUP (!(fill_cmd_reserve_sdr_repository (obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_STORAGE_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);

  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_sdr2 (ipmi_device_t *dev, 
		   uint16_t reservation_id, 
		   uint16_t record_id, 
		   uint8_t offset_into_record, 
		   uint8_t bytes_to_read, 
		   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev 
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return -1;
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_sdr_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_sdr_rq);

  ERR_CLEANUP (!(fill_cmd_get_sdr (reservation_id, 
				   record_id, 
				   offset_into_record, 
				   bytes_to_read,
				   obj_cmd_rq) < 0));

  ERR_IPMI_CMD_CLEANUP (dev, 
			IPMI_BMC_IPMB_LUN_BMC, 
			IPMI_NET_FN_STORAGE_RQ, 
			obj_cmd_rq, 
			obj_cmd_rs);
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj_cmd_rq);
  return (rv);
}

