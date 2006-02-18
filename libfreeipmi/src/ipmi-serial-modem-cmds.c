/* 
   ipmi-serial-modem-cmds.c - IPMI serial port settings commands

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
#include "fiid-wrappers.h"

fiid_template_t tmpl_set_serial_modem_configuration_rq =
  {
    {8,    "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,    "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,    "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,    "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

fiid_template_t tmpl_set_serial_modem_configuration_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_set_serial_modem_configuration_connection_mode_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {1, "basic_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "ppp_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "terminal_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "connect_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_set_serial_modem_configuration_ipmi_messaging_comm_settings_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {5, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "dtr_hangup", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "flow_control", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "bit_rate", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_set_serial_modem_configuration_page_blackout_interval_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "page_blackout_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_set_serial_modem_configuration_call_retry_interval_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    {8, "call_retry_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };
   
fiid_template_t tmpl_get_serial_modem_configuration_rq =
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

fiid_template_t tmpl_get_serial_modem_configuration_rs =
  {
    {8,    "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,    "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,    "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4,    "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1024, "configuration_parameter_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_VARIABLE},
    {0, "", 0}
  };

fiid_template_t tmpl_get_serial_modem_configuration_connection_mode_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "basic_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "ppp_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "terminal_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "connect_mode", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_get_serial_modem_configuration_ipmi_messaging_comm_settings_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {5, "reserved1", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "dtr_hangup", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "flow_control", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "bit_rate", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_get_serial_modem_configuration_page_blackout_interval_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "page_blackout_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_get_serial_modem_configuration_call_retry_interval_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "call_retry_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

int8_t
fill_cmd_set_serial_modem_configuration (fiid_obj_t obj_data_rq,
                                         uint8_t channel_number,
                                         uint8_t parameter_selector,
                                         uint8_t *configuration_parameter_data,
                                         uint8_t configuration_parameter_data_len)
{
  if (!IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !configuration_parameter_data
      || !configuration_parameter_data_len
      || !fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return (-1);
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_data_rq, tmpl_set_serial_modem_configuration_rq);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"cmd",
                IPMI_CMD_SET_SERIAL_MODEM_CONFIGURATION);

  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"channel_number", channel_number);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"reserved", 0);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"parameter_selector", parameter_selector);

  FIID_OBJ_SET_DATA (obj_data_rq,
                     (uint8_t *)"configuration_parameter_data",
                     configuration_parameter_data,
                     configuration_parameter_data_len);

  return 0;
}


int8_t 
fill_cmd_set_serial_modem_configuration_connection_mode (uint8_t channel_number, 
                                                         uint8_t basic_mode,
                                                         uint8_t ppp_mode,
                                                         uint8_t terminal_mode,
                                                         uint8_t connect_mode,
                                                         fiid_obj_t obj_data_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_BASIC_MODE_VALID(basic_mode)
      || !IPMI_PPP_MODE_VALID(ppp_mode)
      || !IPMI_TERMINAL_MODE_VALID(terminal_mode)
      || !IPMI_CONNECT_MODE_VALID(connect_mode)
      || !fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_data_rq, tmpl_set_serial_modem_configuration_connection_mode_rq);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_SERIAL_MODEM_CONFIGURATION);
    
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"channel_number", channel_number);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"reserved1", 0);
  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"parameter_selector", 
		IPMI_SERIAL_MODEM_PARAM_CONNECTION_MODE);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"basic_mode", basic_mode);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"ppp_mode", ppp_mode);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"terminal_mode", terminal_mode);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"reserved2", 0);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"connect_mode", connect_mode);
  
  return 0;
}

int8_t 
fill_cmd_set_serial_modem_configuration_ipmi_messaging_comm_settings (uint8_t channel_number, 
                                                                      uint8_t dtr_hangup,
                                                                      uint8_t flow_control,
                                                                      uint8_t bit_rate,
                                                                      fiid_obj_t obj_data_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_DTR_HANGUP_VALID(dtr_hangup)
      || !IPMI_FLOW_CONTROL_VALID(flow_control)
      || !IPMI_BIT_RATE_VALID(bit_rate)
      || !fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_data_rq, tmpl_set_serial_modem_configuration_ipmi_messaging_comm_settings_rq);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_SERIAL_MODEM_CONFIGURATION);
  
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"channel_number", channel_number); 
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"reserved1", 0);
  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"parameter_selector", 
		IPMI_SERIAL_MODEM_PARAM_IPMI_MESSAGING_COMM_SETTINGS);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"reserved2", 0);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"dtr_hangup", dtr_hangup);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"flow_control", flow_control);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"bit_rate", bit_rate);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"reserved3", 0);

  return 0;
}

int8_t 
fill_cmd_set_serial_modem_configuration_page_blackout_interval (uint8_t channel_number, 
                                                                uint8_t page_blackout_interval,
                                                                fiid_obj_t obj_data_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return -1;
    }
  
  FIID_OBJ_TEMPLATE_COMPARE(obj_data_rq, tmpl_set_serial_modem_configuration_page_blackout_interval_rq);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_SERIAL_MODEM_CONFIGURATION);
  
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"channel_number", channel_number);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"reserved", 0);
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_SERIAL_MODEM_PARAM_PAGE_BLACKOUT_INTERVAL);
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"page_blackout_interval", 
		page_blackout_interval);
  
  return 0;
}

int8_t 
fill_cmd_set_serial_modem_configuration_call_retry_interval (uint8_t channel_number, 
                                                             uint8_t call_retry_interval,
                                                             fiid_obj_t obj_data_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_data_rq, tmpl_set_serial_modem_configuration_call_retry_interval_rq);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_SERIAL_MODEM_CONFIGURATION);
    
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"channel_number", channel_number);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"reserved", 0);
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_SERIAL_MODEM_PARAM_CALL_RETRY_INTERVAL);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"call_retry_interval", call_retry_interval);
  
  return 0;
}	    

int8_t 
fill_cmd_get_serial_modem_configuration (uint8_t channel_number,
                                         uint8_t get_parameter,
                                         uint8_t parameter_selector, 
                                         uint8_t set_selector,
                                         uint8_t block_selector,
                                         fiid_obj_t obj_data_rq)
{
  if (!IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !IPMI_GET_SERIAL_MODEM_PARAMETER_VALID(get_parameter)
      || !fiid_obj_valid(obj_data_rq))
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_TEMPLATE_COMPARE(obj_data_rq, tmpl_get_serial_modem_configuration_rq);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_SERIAL_MODEM_CONFIGURATION);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"channel_number", channel_number);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"reserved", 0);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"get_parameter", get_parameter);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"parameter_selector", parameter_selector);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"set_selector", set_selector);
  FIID_OBJ_SET (obj_data_rq, (uint8_t *)"block_selector", block_selector);
  
  return 0;
}

