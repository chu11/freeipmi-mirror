/* 
   ipmi-sensor-utils.c - IPMI Sensor utility procedures

   Copyright (C) 2003-2004 FreeIPMI Core Team

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "freeipmi.h"

double 
ipmi_sensor_decode_value_old (char r_exponent, 
			      char b_exponent, 
			      int m, 
			      int b, 
			      int linear, 
			      int is_signed, 
			      u_int64_t raw_data)
{
  double fval = 0.0;
  
  if (is_signed)
    fval = (double) ((char) raw_data);
  else 
    fval = (double) raw_data;
  fval *= (double) m;
  fval += (b * pow (10, b_exponent));
  fval *= pow (10, r_exponent);
  if (raw_data != 0) 
    if (linear == 7) 
      fval = 1.0 / fval;
  return fval;
}

double 
ipmi_sensor_decode_value (char r_exponent, 
			  char b_exponent, 
			  short m, 
			  short b, 
			  char linear, 
			  u_int8_t analog_data_format, 
			  u_int8_t raw_data)
{
  double dval = 0.0;
  
/*   printf ("r_exponent: %d\n", r_exponent); */
/*   printf ("b_exponent: %d\n", b_exponent); */
/*   printf ("m: %d\n", m); */
/*   printf ("b: %d\n", b); */
/*   printf ("linear: %d\n", linear); */
/*   printf ("is_signed: %d\n", is_signed); */
/*   printf ("raw_data: %d\n", raw_data); */
  
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
ipmi_sensor_get_decode_params_old (u_int8_t *sensor_record, 
				   int *is_signed, char *r_exponent, char *b_exponent, 
				   u_int64_t *linear, int *b, int *m)
{
  u_int64_t val;
  
  u_int64_t m_ls;
  u_int64_t m_ms;
  
  u_int64_t b_ls;
  u_int64_t b_ms;
  
/*   ipmi_sensor_get_decode_params_own (sensor_record); */
  
  if ((sensor_record[20] & 0xC0) == 0)
    *is_signed = 0;
  else 
    *is_signed = 1;
  
  fiid_obj_get (sensor_record, 
		tmpl_sdr_full_sensor_record, 
		"r_exponent", 
		&val);
  *r_exponent = (char) val;
  if (*r_exponent & 0x08)
    {
      *r_exponent = (char) val;
      *r_exponent += 0xF0;
    }
  
  fiid_obj_get (sensor_record, 
		tmpl_sdr_full_sensor_record, 
		"b_exponent", 
		&val);
  *b_exponent = (char) val;
  
  fiid_obj_get (sensor_record, 
		tmpl_sdr_full_sensor_record, 
		"linearization_enum", 
		&val);
  *linear = val;
  
  fiid_obj_get (sensor_record, 
		tmpl_sdr_full_sensor_record, 
		"m_ls", 
		&m_ls); 
  fiid_obj_get (sensor_record, 
		tmpl_sdr_full_sensor_record, 
		"m_ms", 
		&m_ms); 
  val = bits_merge (m_ls, 8, 10, m_ms);
  
  *m = (int) val;
  
  fiid_obj_get (sensor_record, 
		tmpl_sdr_full_sensor_record, 
		"b_ls", 
		&b_ls); 
  fiid_obj_get (sensor_record, 
		tmpl_sdr_full_sensor_record, 
		"b_ms", 
		&b_ms); 
  val = bits_merge (b_ls, 8, 10, b_ms);
  
  *b = (int) val;
}

void 
ipmi_sensor_get_decode_params (u_int8_t *sensor_record, 
			       u_int8_t *analog_data_format, 
			       char *r_exponent, 
			       char *b_exponent, 
			       char *linear, 
			       short *b, 
			       short *m)
{
  u_int64_t val;
  
  u_int64_t m_ls;
  u_int64_t m_ms;
  
  u_int64_t b_ls;
  u_int64_t b_ms;
  
  fiid_obj_get (sensor_record, 
		tmpl_sdr_full_sensor_record, 
		"r_exponent", 
		&val);
  *r_exponent = (char) val;
  if (*r_exponent & 0x08)
    *r_exponent |= 0xF0;
  
  fiid_obj_get (sensor_record, 
		tmpl_sdr_full_sensor_record, 
		"b_exponent", 
		&val);
  *b_exponent = (char) val;
  if (*b_exponent & 0x08)
    *b_exponent |= 0xF0;
  
  fiid_obj_get (sensor_record, 
		tmpl_sdr_full_sensor_record, 
		"m_ls", 
		&m_ls); 
  fiid_obj_get (sensor_record, 
		tmpl_sdr_full_sensor_record, 
		"m_ms", 
		&m_ms); 
  val = bits_merge (m_ls, 8, 10, m_ms);
  *m = (short) val;
  if (*m & 0x200)
    *m |= 0xFE00;
  
  fiid_obj_get (sensor_record, 
		tmpl_sdr_full_sensor_record, 
		"b_ls", 
		&b_ls); 
  fiid_obj_get (sensor_record, 
		tmpl_sdr_full_sensor_record, 
		"b_ms", 
		&b_ms); 
  val = bits_merge (b_ls, 8, 10, b_ms);
  *b = (short) val;
  if (*b & 0x200)
    *b |= 0xFE00;
  
  fiid_obj_get (sensor_record,
                tmpl_sdr_full_sensor_record,
                "sensor_unit_analog_data_format",
                &val);
  *analog_data_format = (u_int8_t) val;

  return;
}

int 
convert_sensor_state_to_offset (u_int16_t sensor_state, u_int16_t *offset)
{
  if (sensor_state == 0)
    {
      // fprintf (stderr, "sensor_state is none\n");
      return -1;
    }
  
  switch (sensor_state)
    {
    case 0x1:    /* BIT  0 is set */
      *offset = 0;
      break;
    case 0x2:    /* BIT  1 is set */
      *offset = 1;
      break;
    case 0x4:    /* BIT  2 is set */
      *offset = 2;
      break;
    case 0x8:    /* BIT  3 is set */
      *offset = 3;
      break;
    case 0x10:   /* BIT  4 is set */
      *offset = 4;
      break;
    case 0x20:   /* BIT  5 is set */
      *offset = 5;
      break;
    case 0x40:   /* BIT  6 is set */
      *offset = 6;
      break;
    case 0x80:   /* BIT  7 is set */
      *offset = 7;
      break;
    case 0x100:  /* BIT  8 is set */
      *offset = 8;
      break;
    case 0x200:  /* BIT  9 is set */
      *offset = 9;
      break;
    case 0x400:  /* BIT 10 is set */
      *offset = 10;
      break;
    case 0x800:  /* BIT 11 is set */
      *offset = 11;
      break;
    case 0x1000: /* BIT 12 is set */
      *offset = 12;
      break;
    case 0x2000: /* BIT 13 is set */
      *offset = 13;
      break;
    case 0x4000: /* BIT 14 is set */
      *offset = 14;
      break;
    case 0x8000: /* BIT 15 is set */
      *offset = 15;
      break;
    default:
      {
	// fprintf (stderr, "sensor_state is out-of-range. state=%X\n", sensor_state);
	return -1;
      }
    }
  
  return 0;
}
