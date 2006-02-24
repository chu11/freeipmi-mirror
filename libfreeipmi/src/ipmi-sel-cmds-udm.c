/* 
   ipmi-sel-cmds-udm.c - IPMI UDM System Event Log Commands
   
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

#include "ipmi-sel-cmds-udm.h"
#include "ipmi-sel-cmds.h"

#include "freeipmi-portability.h"
#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "ipmi-netfn-spec.h"
#include "ipmi-ipmb-interface.h"

int8_t 
ipmi_cmd_get_sel_info2 (ipmi_device_t *dev, 
			fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;

  if (!dev || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_sel_info_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_sel_info_rq);

  ERR_CLEANUP (!(fill_cmd_get_sel_info (obj_cmd_rq) < 0));

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
ipmi_cmd_get_sel_allocation_info2 (ipmi_device_t *dev, 
                                   fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_sel_allocation_info_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_sel_allocation_info_rq);

  ERR_CLEANUP (!(fill_cmd_get_sel_allocation_info (obj_cmd_rq) < 0));

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
ipmi_cmd_reserve_sel2 (ipmi_device_t *dev, 
		       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_reserve_sel_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_reserve_sel_rq);

  ERR_CLEANUP (!(fill_cmd_reserve_sel (obj_cmd_rq) < 0));

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
ipmi_cmd_get_sel_entry2 (ipmi_device_t *dev, 
                         uint16_t reservation_id,
                         uint16_t record_id,
                         uint8_t offset_into_record,
                         uint8_t bytes_to_read,
			 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_get_sel_entry_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_get_sel_entry_rq);

  ERR_CLEANUP (!(fill_cmd_get_sel_entry (reservation_id,
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

int8_t 
ipmi_cmd_delete_sel_entry2 (ipmi_device_t *dev, 
			    uint16_t reservation_id, 
			    uint16_t record_id, 
			    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_delete_sel_entry_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_delete_sel_entry_rq);

  ERR_CLEANUP (!(fill_cmd_delete_sel_entry (
					    reservation_id, 
					    record_id,
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

int8_t 
ipmi_cmd_clear_sel2 (ipmi_device_t *dev, 
		     uint16_t reservation_id, 
		     uint8_t operation, 
		     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t rv = -1;
  
  if (!dev 
      || !IPMI_SEL_CLEAR_OPERATION_VALID(operation)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_cmd_rs, tmpl_clear_sel_rs);

  FIID_OBJ_CREATE(obj_cmd_rq, tmpl_clear_sel_rq);

  ERR_CLEANUP (!(fill_cmd_clear_sel (reservation_id, 
				     operation,
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

