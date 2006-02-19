/* 
   ipmi-sensor-utils.c - IPMI Sensor utility procedures

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

double 
ipmi_sensor_decode_value (char r_exponent, 
			  char b_exponent, 
			  short m, 
			  short b, 
			  char linear, 
			  uint8_t analog_data_format, 
			  uint8_t raw_data)
{
  double dval = 0.0;
  
  if (analog_data_format == 0x00)
    dval = (double) raw_data;
  else if (analog_data_format == 0x01)
    {
      if (raw_data & 0x80)
        raw_data++;
      dval = (double) ((char) raw_data);
    }
  else if (analog_data_format == 0x02)
    dval = (double) ((char) raw_data);
  else
    {
#if defined (IPMI_SYSLOG)
      syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), 
              "ipmi_sensor_decode_value: Invalid numeric data format 0x%X",
              analog_data_format);
      return (0.0);
#endif /* IPMI_SYSLOG */
    }
    
  dval *= (double) m;
  dval += (b * pow (10, b_exponent));
  dval *= pow (10, r_exponent);
  
  return (dval);
}

void 
ipmi_sensor_get_decode_params (uint8_t *sensor_record, 
			       uint32_t sensor_record_len,
			       uint8_t *analog_data_format, 
			       char *r_exponent, 
			       char *b_exponent, 
			       char *linear, 
			       short *b, 
			       short *m)
{
  uint64_t val;
  
  uint64_t m_ls;
  uint64_t m_ms;
  
  uint64_t b_ls;
  uint64_t b_ms;
  
  fiid_obj_t obj = NULL;

  if (!sensor_record 
      || !analog_data_format
      || !r_exponent
      || !b_exponent
      || !linear
      || !b
      || !m)
    {
      errno = EINVAL;
      return;
    }

  FIID_OBJ_CREATE_CLEANUP (obj, tmpl_sdr_full_sensor_record);
  
  FIID_OBJ_SET_ALL_CLEANUP (obj, sensor_record, sensor_record_len);

  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"r_exponent", &val);
  *r_exponent = (char) val;
  if (*r_exponent & 0x08)
    *r_exponent |= 0xF0;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"b_exponent", &val);
  *b_exponent = (char) val;
  if (*b_exponent & 0x08)
    *b_exponent |= 0xF0;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"m_ls", &m_ls);
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"m_ms", &m_ms);
  ERR_CLEANUP (!(bits_merge (m_ls, 8, 10, m_ms, &val) < 0));
  *m = (short) val;
  if (*m & 0x200)
    *m |= 0xFE00;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"b_ls", &b_ls);
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"b_ms", &b_ms);
  ERR_CLEANUP (!(bits_merge (b_ls, 8, 10, b_ms, &val) < 0));
  *b = (short) val;
  if (*b & 0x200)
    *b |= 0xFE00;
  
  FIID_OBJ_GET_CLEANUP (obj, (uint8_t *)"sensor_unit1.analog_data_format", &val);
  *analog_data_format = (uint8_t) val;

 cleanup:
  FIID_OBJ_DESTROY_NO_RETURN(obj);
  return;
}

