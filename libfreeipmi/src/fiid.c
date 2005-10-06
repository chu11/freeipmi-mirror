/* 
   fiid.c - FreeIPMI Interface Definition

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

*/

#include "freeipmi.h"

int32_t
fiid_obj_len (fiid_template_t tmpl)
{
  int i, len=0;
  if (tmpl == NULL)
    {
      errno = EINVAL;
      return (-1);
    }

  for (i=0; tmpl[i].len != 0; i++)
    len += tmpl[i].len;
  return (len);
}

int32_t
fiid_obj_len_bytes (fiid_template_t tmpl)
{
  int32_t len;
  
  len = fiid_obj_len (tmpl);
  ERR (len != -1);
  return (BITS_ROUND_BYTES (len));
}

int32_t
fiid_obj_field_start_end (fiid_template_t tmpl, 
			  u_int8_t *field, 
			  u_int32_t *start, 
			  u_int32_t *end)
{
  int i = 0;
  int _start = 0;
  int _end = 0; //excluded always
  int key_index = -1;
  
  if ((tmpl == NULL)   ||
      (field == NULL) ||
      (start == NULL) ||
      (end == NULL))
    {
      errno = EINVAL;      
      return (-1);
    }
  
  for (i = 0; tmpl[i].len != 0; i++)
    {
      if (strcmp (tmpl[i].key, field) == 0)
	{
	  _end = _start + tmpl[i].len;
	  key_index = i;
	  break;
	}
      _start += tmpl[i].len;
    }
  
  if (key_index == -1)
    {
      errno = ESPIPE; 		/* Invalid seek */
      return (-1);
    }
  
  *start = _start;
  *end   = _end;
  
  return (tmpl[i].len);
}

int8_t
fiid_obj_field_lookup (fiid_template_t tmpl, 
		       u_int8_t *field)
{
  int start = 0;
  int end = 0; //excluded always
  
  if (fiid_obj_field_start_end (tmpl, field, &start, &end) != -1)
    return (1);
  else
    return (0);
}

int32_t
fiid_obj_field_start (fiid_template_t tmpl, 
		      u_int8_t *field)
{
  int start = 0;
  int end = 0; //excluded always
  
  ERR (fiid_obj_field_start_end (tmpl, field, &start, &end) != -1);
  return (start);
}

int32_t
fiid_obj_field_start_bytes (fiid_template_t tmpl, 
			    u_int8_t *field)
{
  int start = 0;
  
  start = fiid_obj_field_start (tmpl, field);
  ERR (start != -1);
  return (BITS_ROUND_BYTES (start));
}

int32_t
fiid_obj_field_end (fiid_template_t tmpl, 
		    u_int8_t *field)
{
  int start = 0;
  int end = 0; //excluded always
  
  ERR (fiid_obj_field_start_end (tmpl, field, &start, &end) != -1);
  return (end);
}

int32_t
fiid_obj_field_end_bytes (fiid_template_t tmpl, 
			  u_int8_t *field)
{
  int end = 0;
  
  end = fiid_obj_field_end (tmpl, field);
  ERR (end != -1);
  return (BITS_ROUND_BYTES (end));
}

int32_t
fiid_obj_field_len (fiid_template_t tmpl, 
		    u_int8_t *field)
{
  int i;
  if ((tmpl == NULL) ||
      (field == NULL))
    {
      errno = EINVAL;      
      return (-1);
    }
  
  for (i=0; tmpl[i].len != 0; i++)
    {
      if (strcmp (tmpl[i].key, field) == 0)
	return (tmpl[i].len);
    }
  
  errno = ESPIPE; 		/* Invalid seek */
  return (-1);
}

int32_t
fiid_obj_field_len_bytes (fiid_template_t tmpl, 
			  u_int8_t *field)
{
  int32_t len;
  
  len = fiid_obj_field_len (tmpl, field);
  ERR (len != -1);
  return (BITS_ROUND_BYTES (len));
}

int32_t
fiid_obj_block_len (fiid_template_t tmpl, 
		    u_int8_t *field_start, 
		    u_int8_t *field_end)
{
  int end;
  int start;
  
  end = fiid_obj_field_end (tmpl, field_end);
  ERR (end != -1);
  start = fiid_obj_field_start (tmpl, field_start);
  ERR (start != -1);
  return (abs (end - start));
}

int32_t
fiid_obj_block_len_bytes (fiid_template_t tmpl, 
			  u_int8_t *field_start, 
			  u_int8_t *field_end)
{
  int len;
  
  len = fiid_obj_block_len (tmpl, field_start, field_end);
  ERR (len != -1);
  return (BITS_ROUND_BYTES (len));
}

fiid_obj_t 
fiid_obj_alloc (fiid_template_t tmpl)
{
  int len;
  
  len = fiid_obj_len_bytes (tmpl);
  if (len == -1)
    {
      errno = EINVAL;
      return NULL;
    }
  
  return (ipmi_xcalloc (1, len));
}

fiid_obj_t 
fiid_obj_memset (fiid_obj_t obj, 
		 int c, 
		 fiid_template_t tmpl)
{
  int len;
  
  if (obj == NULL)
    {
      errno = EINVAL;
      return NULL;
    }
  
  len = fiid_obj_len_bytes (tmpl);
  if (len == -1)
    {
      errno = EINVAL;
      return NULL;
    }
  
  return (memset (obj, c, len));
}

int8_t 
fiid_obj_memset_field (fiid_obj_t obj, 
		       int c, 
		       fiid_template_t tmpl, 
		       u_int8_t *field)
{
  int field_index; 
  int32_t len; 
  
  if (!(obj && tmpl))
    {
      errno = EINVAL;
      return (-1);
    }
  
  field_index = fiid_obj_field_start_bytes (tmpl, field);
  ERR (field_index != -1);
  len = fiid_obj_field_len_bytes (tmpl, field);
  ERR (len != -1);
  
  memset ((obj + field_index), c, len);
  
  return 0;
}


void
fiid_obj_free (fiid_obj_t obj)
{
  ipmi_xfree (obj);
}

int8_t
fiid_obj_set (fiid_obj_t obj, 
	      fiid_template_t tmpl, 
	      u_int8_t *field, 
	      u_int64_t val)
{
  int start_bit_pos = 0;
  int end_bit_pos = 0; //excluded always
  int byte_pos = 0;
  int start_bit_in_byte_pos = 0;
  int end_bit_in_byte_pos = 0;
  int field_len = 0;
  int bytes_used = 0;
  
  u_int8_t byte_data;
  
  if (!(obj && tmpl && field))
    {
      errno = EINVAL;
      return (-1);
    }
  
  field_len = fiid_obj_field_start_end (tmpl, field, &start_bit_pos, &end_bit_pos);
  ERR (field_len != -1);
  
  byte_pos = start_bit_pos / 8;
  
  //in byte_pos, start_bit_pos is
  start_bit_in_byte_pos = start_bit_pos - (byte_pos * 8);
  bytes_used = 1;
  
  //and it spans into...
  if (start_bit_in_byte_pos + field_len > 8)
    {
      field_len -= start_bit_in_byte_pos;
      bytes_used += (field_len / 8);
      field_len %= 8;
      if (field_len != 0)
	{
	  bytes_used++;
	}
    }
  else 
    end_bit_in_byte_pos = start_bit_in_byte_pos + field_len;
  
  if (bytes_used > 1)
    {
      int start_val_pos = 0;
      int end_val_pos = 0;
      u_int64_t range_val = 0;
      int i;
      
      for (i = 0; i < bytes_used; i++)
	{
	  if (i == 0)
	    end_val_pos = 8 - start_bit_in_byte_pos;
	  else 
	    if (i == (bytes_used - 1))
	      end_val_pos += field_len;
	    else 
	      end_val_pos += 8;
	  
	  if (i == (bytes_used - 1))
	    end_bit_in_byte_pos = field_len;
	  else 
	    end_bit_in_byte_pos = 8;
	  
	  range_val = bits_extract (val, start_val_pos, end_val_pos);
	  byte_data = obj[byte_pos + i];
	  obj[byte_pos + i] = bits_merge (byte_data, 
					  start_bit_in_byte_pos, 
					  end_bit_in_byte_pos, range_val);
	  start_bit_in_byte_pos = 0;
	  start_val_pos = end_val_pos;
	}
      return (0);
    }
  byte_data = obj[byte_pos];
  obj[byte_pos] = bits_merge (byte_data, 
			      start_bit_in_byte_pos, 
			      end_bit_in_byte_pos, val);
  return (0);
}

int8_t
fiid_obj_get (fiid_obj_t obj, 
	      fiid_template_t tmpl, 
	      u_int8_t *field, 
	      u_int64_t *val)
{
  int start_bit_pos = 0;
  int end_bit_pos = 0; //excluded always
  int byte_pos = 0;
  int start_bit_in_byte_pos = 0;
  int end_bit_in_byte_pos = 0;
  int field_len = 0;
  int bytes_used = 0;
  u_int8_t byte_data;
  
  if (!(obj && tmpl && field))
    {
      errno = EINVAL;
      return (-1);
    }

  field_len = fiid_obj_field_start_end (tmpl, field, &start_bit_pos, &end_bit_pos);
  ERR (field_len != -1);
  
  byte_pos = start_bit_pos / 8;

  /* in byte_pos, start_bit_pos is  */
  start_bit_in_byte_pos = start_bit_pos - (byte_pos * 8);
  bytes_used = 1;
  
  /*   and it spans into... */
  if (start_bit_in_byte_pos + field_len > 8)
    {
      field_len -= start_bit_in_byte_pos;
      bytes_used += (field_len / 8);
      field_len %= 8;
      if (field_len != 0)
	{
	  bytes_used++;
	}
    }
  else 
    end_bit_in_byte_pos = start_bit_in_byte_pos + field_len;
  
  if (bytes_used > 1)
    {
      int start_val_pos = 0;
      int end_val_pos = 0;
      u_int64_t range_val = 0;
      int i;
      u_int64_t mval = 0x0;
      
      for (i = 0; i < bytes_used; i++)
	{
	  if (i == 0)
	    end_val_pos = 8 - start_bit_in_byte_pos;
	  else
	    if (i == (bytes_used - 1))
	      end_val_pos += field_len;
	    else
	      end_val_pos += 8;
	  
	  if (i == (bytes_used - 1))
	    end_bit_in_byte_pos = field_len;
	  else
	    end_bit_in_byte_pos = 8;
	  
	  byte_data = obj[byte_pos + i];
	  range_val = bits_extract (byte_data,
				    start_bit_in_byte_pos,
				    end_bit_in_byte_pos);
	  
	  mval = bits_merge (mval, start_val_pos, end_val_pos, range_val);
	  
	  start_bit_in_byte_pos = 0;
	  
	  start_val_pos = end_val_pos;
	}
      
      *val = mval;
      return (0);
    }
  byte_data = obj[byte_pos];
  *val = bits_extract (byte_data, 
		       start_bit_in_byte_pos, 
		       end_bit_in_byte_pos);
  return (0);
}

fiid_field_t * 
__fiid_template_make (u_int8_t dummy, ...)
{
  va_list ap;
  
  int len = 0;
  char *key = NULL;
  
  fiid_field_t *tmpl_dynamic = NULL;
  int element_count = 0;
  
  int i;
  
  
  {
    va_list ap;
    
    va_start (ap, dummy);
    
    while (1)
      {
	if ((len = va_arg (ap, int)) == 0)
	  break;
	
	if ((key = va_arg (ap, char *)) == NULL)
	  break;
	
	element_count++;
      }
    
    //printf ("total field count: %d\n", element_count);
    va_end (ap);
  }
  
  va_start (ap, dummy);
  
  tmpl_dynamic = (fiid_field_t *) calloc ((element_count + 1), 
					 sizeof (fiid_field_t));
  
  for (i = 0; i < element_count; i++)
    {
      if ((len = va_arg (ap, int)) == 0)
	break;
      
      if ((key = va_arg (ap, char *)) == NULL)
	{
	  free (tmpl_dynamic);
	  return NULL;
	}
      
      tmpl_dynamic[i].len = len;
      strcpy (tmpl_dynamic[i].key, key);
    }
  
  va_end (ap);
  
  return tmpl_dynamic;
}

void 
fiid_template_free (fiid_field_t *tmpl_dynamic)
{
  if (tmpl_dynamic != NULL)
    free (tmpl_dynamic);
}

int8_t 
fiid_obj_get_data (fiid_obj_t obj, 
		   fiid_template_t tmpl, 
		   u_int8_t *field, 
		   u_int8_t *data)
{
  int field_index; 
  int len;
  
  if (obj == NULL || data == NULL)
    {
      errno = EINVAL;
      return (-1);
    }
  
  field_index = fiid_obj_field_start_bytes (tmpl, field);
  ERR (field_index != -1);
  len = fiid_obj_field_len_bytes (tmpl, field);
  ERR (len != -1);
  memcpy (data, (obj + field_index), len);
  
  return 0;
}

int8_t 
fiid_obj_set_data (fiid_obj_t obj, 
		   fiid_template_t tmpl, 
		   u_int8_t *field, 
		   u_int8_t *data, 
		   u_int32_t data_len)
{
  int field_index; 
  int32_t len; 
  
  if (obj == NULL || data == NULL)
    {
      errno = EINVAL;
      return (-1);
    }
  
  field_index = fiid_obj_field_start_bytes (tmpl, field);
  ERR (field_index != -1);
  len = fiid_obj_field_len_bytes (tmpl, field);
  ERR (len != -1);
  
  if (data_len > len)
    data_len = len;
  
  memcpy ((obj + field_index), data, data_len);
  
  return 0;
}

fiid_obj_t 
fiid_obj_dup (fiid_obj_t src_obj, fiid_template_t tmpl)
{
  fiid_obj_t dest_obj = NULL;
  
  if (src_obj == NULL || tmpl == NULL)
    {
      errno = EINVAL;
      return NULL;
    }
  
  dest_obj = fiid_obj_alloc (tmpl);
  if (dest_obj != NULL)
    {
      int len;
      
      len = fiid_obj_len_bytes (tmpl);
      if (len == -1)
	return NULL;
      memcpy (dest_obj, src_obj, len);
    }
  
  return dest_obj;
}
