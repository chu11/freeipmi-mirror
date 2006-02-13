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

#include "freeipmi.h"

int8_t 
ipmi_cmd_get_sel_info2 (ipmi_device_t *dev, 
			fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_sel_info_rq);
  ERR (fill_kcs_get_sel_info (obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_STORAGE_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_sel_info_rq, 
		 obj_cmd_rs, 
		 tmpl_get_sel_info_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_sel_alloc_info2 (ipmi_device_t *dev, 
			      fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_sel_alloc_info_rq);
  ERR (fill_kcs_get_sel_alloc_info (obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_STORAGE_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_sel_alloc_info_rq, 
		 obj_cmd_rs, 
		 tmpl_get_sel_alloc_info_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_reserve_sel2 (ipmi_device_t *dev, 
		       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_reserve_sel_rq);
  ERR (fill_kcs_reserve_sel (obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_STORAGE_RQ, 
		 obj_cmd_rq, 
		 tmpl_reserve_sel_rq, 
		 obj_cmd_rs, 
		 tmpl_reserve_sel_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_sel_entry2 (ipmi_device_t *dev, 
			 uint16_t record_id, 
			 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_sel_entry_rq);
  ERR (fill_kcs_get_sel_entry (record_id, obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_STORAGE_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_sel_entry_rq, 
		 obj_cmd_rs, 
		 tmpl_get_sel_entry_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_delete_sel_entry2 (ipmi_device_t *dev, 
			    uint16_t reservation_id, 
			    uint16_t record_id, 
			    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_delete_sel_entry_rq);
  ERR (fill_kcs_delete_sel_entry (reservation_id, 
				  record_id,
                                  obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_STORAGE_RQ, 
		 obj_cmd_rq, 
		 tmpl_delete_sel_entry_rq, 
		 obj_cmd_rs, 
		 tmpl_delete_sel_entry_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_clear_sel2 (ipmi_device_t *dev, 
		     uint16_t reservation_id, 
		     uint8_t opcode, 
		     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_clear_sel_rq);
  ERR (fill_kcs_clear_sel (reservation_id, 
			   opcode,
                           obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_STORAGE_RQ, 
		 obj_cmd_rq, 
		 tmpl_clear_sel_rq, 
		 obj_cmd_rs, 
		 tmpl_clear_sel_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

