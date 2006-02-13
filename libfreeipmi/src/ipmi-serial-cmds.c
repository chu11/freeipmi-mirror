/* 
   ipmi-serial-cmds.c - IPMI serial port settings commands

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

fiid_template_t tmpl_set_serial_conf_param_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_get_serial_conf_param_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {3, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "parameter_type", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "set_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "block_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_serial_conf_param_connmode_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {1, "basic_mode_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "ppp_mode_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "terminal_mode_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "direct", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {0, "", 0}
  };

fiid_template_t tmpl_get_serial_conf_param_connmode_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {1, "basic_mode_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "ppp_mode_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "terminal_mode_enable", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "direct", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_serial_conf_param_pageblackout_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {8, "page_blackout_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_get_serial_conf_param_pageblackout_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "page_blackout_interval", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_serial_conf_param_retry_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {8, "retry_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {0, "", 0}
  };

fiid_template_t tmpl_get_serial_conf_param_retry_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "retry_time", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

fiid_template_t tmpl_set_serial_conf_param_commbits_rq =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {4, "channel_number", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {8, "parameter_selector", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED}, 
    
    {5, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "dtr_hangup", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "flow_control", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {4, "bit_rate", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    
    {0, "", 0}
  };
    
fiid_template_t tmpl_get_serial_conf_param_commbits_rs =
  {
    {8, "cmd", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {8, "comp_code", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "present_revision", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "oldest_revision_parameter", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {5, "reserved2", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {1, "dtr_hangup", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "flow_control", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {4, "bit_rate", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {4, "reserved3", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},

    {0, "", 0}
  };

int 
fill_set_serial_connmode (uint8_t channel_number, 
			  uint8_t basic_mode_enable,
			  uint8_t ppp_mode_enable,
			  uint8_t terminal_mode_enable,
			  uint8_t direct,
                          fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_serial_conf_param_connmode_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_SERIAL_MODEM_CONF_PARAM);
    
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);

  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved",
                0);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_SERIAL_PARAM_CONNECTION_MODE);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"basic_mode_enable", 
		basic_mode_enable);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"ppp_mode_enable", 
		ppp_mode_enable);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"terminal_mode_enable", 
		terminal_mode_enable);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved2",
                0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"direct", 
		direct);
  
  return 0;
}

int8_t 
fill_set_serial_page_blackout_interval (uint8_t channel_number, 
					uint8_t page_blackout_interval,
                                        fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_serial_conf_param_pageblackout_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_SERIAL_MODEM_CONF_PARAM);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved",
                0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_SERIAL_PARAM_PAGE_BLACKOUT_INTERVAL);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"page_blackout_interval", 
		page_blackout_interval);
  
  return 0;
}

int8_t 
fill_set_serial_retry_time (uint8_t channel_number, 
			    uint8_t retry_time,
                            fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_serial_conf_param_retry_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_SERIAL_MODEM_CONF_PARAM);
    
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
    
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved",
                0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_SERIAL_PARAM_RETRY_TIME);
    
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"retry_time", 
		retry_time);
  
  return 0;
}	    

int8_t 
fill_set_serial_comm_bits (uint8_t channel_number, 
                           uint8_t dtr_hangup,
                           uint8_t flow_control,
                           uint8_t bit_rate,
                           fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_set_serial_conf_param_commbits_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_SET_SERIAL_MODEM_CONF_PARAM);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved",
                0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_selector", 
		IPMI_SERIAL_PARAM_COMM_BITS);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved2",
                0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"dtr_hangup", 
		dtr_hangup);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"flow_control", 
		flow_control);
  
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"bit_rate", 
		bit_rate);
  
  FIID_OBJ_SET (obj_data_rq,
                (uint8_t *)"reserved3",
                0);

  return 0;
}

int8_t 
fill_get_serial_conf_param (uint8_t parameter_selector, 
			    uint8_t channel_number,
			    uint8_t parameter_type,
			    uint8_t set_selector,
			    uint8_t block_selector,
                            fiid_obj_t obj_data_rq)
{
  int8_t rv;

  if (!fiid_obj_valid(obj_data_rq)
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(obj_data_rq, tmpl_get_serial_conf_param_rq)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"cmd", 
		IPMI_CMD_GET_SERIAL_MODEM_CONF_PARAM);
    
  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"channel_number", 
		channel_number);
    
  FIID_OBJ_SET (obj_data_rq,
		(uint8_t *)"reserved",
		0);

  FIID_OBJ_SET (obj_data_rq, 
		(uint8_t *)"parameter_type", 
		parameter_type);
    
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

int8_t 
ipmi_cmd_set_serial_connmode2 (ipmi_device_t *dev, 
			       uint8_t channel_number, 
			       uint8_t basic_mode_enable,
			       uint8_t ppp_mode_enable,
			       uint8_t terminal_mode_enable,
			       uint8_t direct,
			       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t ret, rv = -1;

  if (!dev 
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_set_serial_conf_param_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_set_serial_conf_param_connmode_rq)))
    goto cleanup;
  
  if (fill_set_serial_connmode (channel_number, 
                                basic_mode_enable, 
                                ppp_mode_enable, 
                                terminal_mode_enable, 
                                direct,
                                obj_cmd_rq) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
                IPMI_BMC_IPMB_LUN_BMC, 
                IPMI_NET_FN_TRANSPORT_RQ, 
                obj_cmd_rq, 
                obj_cmd_rs) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_serial_page_blackout_interval2 (ipmi_device_t *dev, 
					     uint8_t channel_number, 
					     uint8_t page_blackout_interval, 
					     fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t ret, rv = -1;
  
  if (!dev 
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_set_serial_conf_param_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_set_serial_conf_param_pageblackout_rq)))
    goto cleanup;

  if (fill_set_serial_page_blackout_interval (channel_number, 
                                              page_blackout_interval,
                                              obj_cmd_rq) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
                IPMI_BMC_IPMB_LUN_BMC, 
                IPMI_NET_FN_TRANSPORT_RQ, 
                obj_cmd_rq, 
                obj_cmd_rs) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_serial_retry_time2 (ipmi_device_t *dev, 
				 uint8_t channel_number, 
				 uint8_t retry_time, 
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t ret, rv = -1;
  
  if (!dev 
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_set_serial_conf_param_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_set_serial_conf_param_retry_rq)))
    goto cleanup;

  if (fill_set_serial_retry_time (channel_number, 
                                  retry_time,
                                  obj_cmd_rq) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
                IPMI_BMC_IPMB_LUN_BMC, 
                IPMI_NET_FN_TRANSPORT_RQ, 
                obj_cmd_rq, 
                obj_cmd_rs) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_set_serial_comm_bits2 (ipmi_device_t *dev, 
				uint8_t channel_number, 
				uint8_t dtr_hangup,
				uint8_t flow_control,
				uint8_t bit_rate,
				fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t ret, rv = -1;
  
  if (!dev 
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_set_serial_conf_param_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_set_serial_conf_param_commbits_rq)))
    goto cleanup;

  if (fill_set_serial_comm_bits (channel_number, 
                                 dtr_hangup,
                                 flow_control,
                                 bit_rate,
                                 obj_cmd_rq) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
                IPMI_BMC_IPMB_LUN_BMC, 
                IPMI_NET_FN_TRANSPORT_RQ, 
                obj_cmd_rq, 
                obj_cmd_rs) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_serial_connmode2 (ipmi_device_t *dev, 
			       uint8_t channel_number,
			       uint8_t parameter_type,
			       uint8_t set_selector,
			       uint8_t block_selector,
			       fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t ret, rv = -1;
  
  if (!dev 
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_serial_conf_param_connmode_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_serial_conf_param_rq)))
    goto cleanup;

  if (fill_get_serial_conf_param (IPMI_SERIAL_PARAM_CONNECTION_MODE, 
                                  channel_number, 
                                  parameter_type, 
                                  set_selector, 
                                  block_selector,
                                  obj_cmd_rq) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
                IPMI_BMC_IPMB_LUN_BMC, 
                IPMI_NET_FN_TRANSPORT_RQ, 
                obj_cmd_rq, 
                obj_cmd_rs) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_serial_page_blackout2 (ipmi_device_t *dev, 
				    uint8_t channel_number,
				    uint8_t parameter_type,
				    uint8_t set_selector,
				    uint8_t block_selector,
				    fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t ret, rv = -1;
  
  if (!dev 
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_serial_conf_param_pageblackout_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_serial_conf_param_rq)))
    goto cleanup;

  if (fill_get_serial_conf_param (IPMI_SERIAL_PARAM_PAGE_BLACKOUT_INTERVAL, 
                                  channel_number, 
                                  parameter_type, 
                                  set_selector, 
                                  block_selector,
                                  obj_cmd_rq) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
                IPMI_BMC_IPMB_LUN_BMC, 
                IPMI_NET_FN_TRANSPORT_RQ, 
                obj_cmd_rq, 
                obj_cmd_rs) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_serial_retry_time2 (ipmi_device_t *dev, 
				 uint8_t channel_number,
				 uint8_t parameter_type,
				 uint8_t set_selector,
				 uint8_t block_selector,
				 fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t ret, rv = -1;
  
  if (!dev 
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_serial_conf_param_retry_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_serial_conf_param_rq)))
    goto cleanup;

  if (fill_get_serial_conf_param (IPMI_SERIAL_PARAM_RETRY_TIME, 
                                  channel_number, 
                                  parameter_type, 
                                  set_selector, 
                                  block_selector,
                                  obj_cmd_rq) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
                IPMI_BMC_IPMB_LUN_BMC, 
                IPMI_NET_FN_TRANSPORT_RQ, 
                obj_cmd_rq, 
                obj_cmd_rs) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  return (rv);
}

int8_t 
ipmi_cmd_get_serial_comm_bits2 (ipmi_device_t *dev, 
				uint8_t channel_number,
				uint8_t parameter_type,
				uint8_t set_selector,
				uint8_t block_selector,
				fiid_obj_t obj_cmd_rs)
{
  fiid_obj_t obj_cmd_rq = NULL;
  int8_t ret, rv = -1;
  
  if (!dev 
      || !IPMI_CHANNEL_NUMBER_VALID(channel_number)
      || !fiid_obj_valid(obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((ret = fiid_obj_template_compare(obj_cmd_rs, tmpl_get_serial_conf_param_commbits_rs)) < 0)
    goto cleanup;

  if (!ret)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj_cmd_rq = fiid_obj_create(tmpl_get_serial_conf_param_rq)))
    goto cleanup;

  if (fill_get_serial_conf_param (IPMI_SERIAL_PARAM_COMM_BITS, 
                                  channel_number, 
                                  parameter_type, 
                                  set_selector, 
                                  block_selector,
                                  obj_cmd_rq) < 0)
    goto cleanup;

  if (ipmi_cmd (dev, 
                IPMI_BMC_IPMB_LUN_BMC, 
                IPMI_NET_FN_TRANSPORT_RQ, 
                obj_cmd_rq, 
                obj_cmd_rs) < 0)
    goto cleanup;

  rv = 0;
 cleanup:
  if (obj_cmd_rq)
    fiid_obj_destroy(obj_cmd_rq);
  return (rv);
}

