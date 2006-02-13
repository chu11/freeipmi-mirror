/* 
   ipmi-sensor-cmds-udm.c - IPMI UDM Sensor commands

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
ipmi_cmd_get_threshold_reading2 (ipmi_device_t *dev, 
				 uint8_t sensor_number, 
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_sensor_threshold_reading_rq);
  ERR (fill_kcs_get_threshold_reading (sensor_number,
                                       obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_SENSOR_EVENT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_sensor_threshold_reading_rq, 
		 obj_cmd_rs, 
		 tmpl_get_sensor_threshold_reading_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_discrete_reading2 (ipmi_device_t *dev, 
				uint8_t sensor_number, 
				fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_sensor_discrete_reading_rq);
  ERR (fill_kcs_get_discrete_reading (sensor_number,
                                      obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_SENSOR_EVENT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_sensor_discrete_reading_rq, 
		 obj_cmd_rs, 
		 tmpl_get_sensor_discrete_reading_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

int8_t 
ipmi_cmd_get_sensor_thresholds2 (ipmi_device_t *dev, 
				 uint8_t sensor_number, 
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  
  if (!dev || !obj_cmd_rs)
    {
      errno = EINVAL;
      return (-1);
    }
  
  FIID_OBJ_ALLOCA (obj_cmd_rq, tmpl_get_sensor_thresholds_rq);
  ERR (fill_kcs_get_sensor_thresholds (sensor_number,
                                       obj_cmd_rq) == 0);
  ERR (ipmi_cmd (dev, 
		 IPMI_BMC_IPMB_LUN_BMC, 
		 IPMI_NET_FN_SENSOR_EVENT_RQ, 
		 obj_cmd_rq, 
		 tmpl_get_sensor_thresholds_rq, 
		 obj_cmd_rs, 
		 tmpl_get_sensor_thresholds_rs) == 0);
  ERR (ipmi_comp_test (obj_cmd_rs) == 1);
  
  return (0);
}

