/* 
   ipmi-sol-cmds.c - IPMI SOL Commands

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

fiid_template_t tmpl_set_sol_configuration_parameters_rq =
  {
    {8,    "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,    "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,    "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,    "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

fiid_template_t tmpl_set_sol_configuration_parameters_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_set_sol_sol_enable_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "sol_payload", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {7, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_get_sol_configuration_parameters_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "get_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0, "", 0}
  };

fiid_template_t tmpl_get_sol_configuration_parameters_rs =
  {
    {8,    "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,    "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,    "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,    "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

fiid_template_t tmpl_get_sol_enable_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "sol_payload", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {7, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {0,  "", 0}
  };

int8_t
fill_cmd_set_sol_configuration_parameters (fiid_obj_t obj_data_rq,
					   uint8_t channel_number,
					   uint8_t parameter_selector,
					   uint8_t *configuration_parameter_data,
					   uint8_t configuration_parameter_data_len)
{
  int8_t rv;

  if (!IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !configuration_parameter_data
      || !configuration_parameter_data_len
      || !fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_sol_configuration_parameters_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"cmd",
                IPMI_CMD_SET_SOL_CONFIGURATION_PARAMETERS);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"channel_number",
                channel_number);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved",
                0);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"parameter_selector",
                parameter_selector);
  
  FIID_OBJ_SET_DATA (obj_data_rq,
                     (uint8_t *)"configuration_parameter_data",
                     configuration_parameter_data,
                     configuration_parameter_data_len);
  
  return 0;
}


int8_t 
fill_cmd_set_sol_sol_enable (uint8_t channel_number, 
			     uint8_t sol_payload,
			     fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_SOL_PAYLOAD_VALID(sol_payload)
      || !fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_sol_sol_enable_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_SOL_CONFIGURATION_PARAMETERS);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved1",
                0);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_SOL_PARAM_SOL_ENABLE);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"sol_payload", 
		sol_payload);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved2",
                0);

  return 0;
}

int8_t 
fill_cmd_get_sol_configuration_parameters (uint8_t channel_number,
					   uint8_t get_parameter,
					   uint8_t parameter_selector, 
					   uint8_t set_selector,
					   uint8_t block_selector,
					   fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_GET_SOL_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_get_sol_configuration_parameters_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_SOL_CONFIGURATION_PARAMETERS);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
    
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved",
                0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"get_parameter", 
		get_parameter);
    
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		parameter_selector);
    
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"set_selector", 
		set_selector);
    
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"block_selector", 
		block_selector);
  
  return 0;
}

