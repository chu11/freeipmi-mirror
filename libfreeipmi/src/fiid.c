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

#define FIID_OBJ_MAGIC 0xf00fd00d
#define FIID_ITERATOR_MAGIC 0xd00df00f

struct fiid_field_data
{
  uint32_t max_field_len;
  char key[FIID_FIELD_MAX];
  uint32_t set_field_len;
};

struct fiid_obj
{
  uint32_t magic;
  uint8_t *data;
  unsigned int data_len;
  struct fiid_field_data *field_data;
  unsigned int field_data_len;
};

struct fiid_iterator
{
  uint32_t magic;
  int current_index;
  int last_index;
  struct fiid_obj *obj;
};

static int32_t
_fiid_template_len (fiid_template_t tmpl, unsigned int *tmpl_len)
{
  int32_t len = 0;
  int i;

  assert(tmpl && tmpl_len);

  for (i = 0; tmpl[i].max_field_len != 0; i++)
    len += tmpl[i].max_field_len;

#ifndef NDEBUG
  if (len % 8 != 0)
    {
      errno = EINVAL;
      return (-1);
    }
#endif /* !NDEBUG */

  *tmpl_len = (i + 1);
  return (len);
}

static int32_t
_fiid_template_len_bytes (fiid_template_t tmpl, unsigned int *tmpl_len)
{
  int32_t len;

  assert(tmpl && tmpl_len);
  
  if ((len = _fiid_template_len (tmpl, tmpl_len)) < 0)
    return (-1);

  if (len % 8 != 0)
    {
      errno = EINVAL;
      return (-1);
    }

  return (BITS_ROUND_BYTES (len));
}

int8_t
fiid_template_field_lookup (fiid_template_t tmpl, uint8_t *field)
{
  int i;
  
  for (i = 0; tmpl[i].max_field_len != 0; i++)
    {
      if (!strcmp (tmpl[i].key, field))
        return (1);
    }

  return (0);
}

int32_t
fiid_template_len (fiid_template_t tmpl)
{
  unsigned int temp;
  int32_t len = 0;

  if (!tmpl)
    {
      errno = EINVAL;
      return (-1);
    }

  if ((len = _fiid_template_len(tmpl, &temp)) < 0)
    return (-1);

  return (len);
}

int32_t
fiid_template_len_bytes (fiid_template_t tmpl)
{
  int32_t len;

  if (!tmpl)
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((len = fiid_template_len (tmpl)) < 0)
    return (-1);

  if (len % 8 != 0)
    {
      errno = EINVAL;
      return (-1);
    }

  return (BITS_ROUND_BYTES (len));
}

static int32_t
_fiid_template_field_start_end (fiid_template_t tmpl, 
				uint8_t *field, 
				uint32_t *start, 
				uint32_t *end)
{
  int i = 0;
  int _start = 0;
  int _end = 0;
  
  assert(tmpl && field && start && end);
  
  for (i = 0; tmpl[i].max_field_len != 0; i++)
    {
      if (strcmp (tmpl[i].key, (char *)field) == 0)
	{
	  _end = _start + tmpl[i].max_field_len;
	  *start = _start;
	  *end = _end;
	  return (tmpl[i].max_field_len);
	}
      _start += tmpl[i].max_field_len;
    }
  
  errno = ESPIPE; 		/* Invalid seek */
  return (-1);
}

int32_t
fiid_template_field_start (fiid_template_t tmpl, uint8_t *field)
{
  uint32_t start = 0;
  uint32_t end = 0;
  
  if (!(tmpl && field))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if (_fiid_template_field_start_end (tmpl, field, &start, &end) < 0)
    return (-1);

  return (start);
}

int32_t
fiid_template_field_start_bytes (fiid_template_t tmpl, uint8_t *field)
{
  int32_t start = 0;
  
  if (!(tmpl && field))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((start = fiid_template_field_start (tmpl, field)) < 0)
    return (-1);

  if (start % 8 != 0)
    {
      errno = EINVAL;
      return (-1);
    }

  return (BITS_ROUND_BYTES (start));
}

int32_t
fiid_template_field_end (fiid_template_t tmpl, uint8_t *field)
{
  uint32_t start = 0;
  uint32_t end = 0;
  
  if (!(tmpl && field))
    {
      errno = EINVAL;
      return (-1);
    }

  if (_fiid_template_field_start_end (tmpl, field, &start, &end) < 0)
    return (-1);

  return (end);
}

int32_t
fiid_template_field_end_bytes (fiid_template_t tmpl, uint8_t *field)
{
  int32_t end = 0;
  
  if (!(tmpl && field))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((end = fiid_template_field_end (tmpl, field)) < 0)
    return (-1);

  if (end % 8 != 0)
    {
      errno = EINVAL;
      return (-1);
    }

  return (BITS_ROUND_BYTES (end));
}

int32_t
fiid_template_field_len (fiid_template_t tmpl, uint8_t *field)
{
  int i;

  if (!(tmpl && field))
    {
      errno = EINVAL;
      return (-1);
    }
  
  for (i=0; tmpl[i].max_field_len != 0; i++)
    {
      if (!strcmp (tmpl[i].key, (char *)field))
	return (tmpl[i].max_field_len);
    }
  
  errno = ESPIPE; 		/* Invalid seek */
  return (-1);
}

int32_t
fiid_template_field_len_bytes (fiid_template_t tmpl, uint8_t *field)
{
  int32_t len;
  
  if (!(tmpl && field))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((len = fiid_template_field_len (tmpl, field)) < 0)
    return (-1);

  if (len % 8 != 0)
    {
      errno = EINVAL;
      return (-1);
    }

  return (BITS_ROUND_BYTES (len));
}

int32_t
fiid_template_block_len (fiid_template_t tmpl, 
			 uint8_t *field_start, 
			 uint8_t *field_end)
{
  int32_t start;
  int32_t end;
  
  if (!(tmpl && field_start && field_end))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((end = fiid_template_field_end (tmpl, field_end)) < 0)
    return (-1);

  if ((start = fiid_template_field_start (tmpl, field_start)) < 0)
    return (-1);

  if (start > end)
    {
      errno = EINVAL;
      return (-1);
    }

  return (end - start);
}

int32_t
fiid_template_block_len_bytes (fiid_template_t tmpl, 
			       uint8_t *field_start, 
			       uint8_t *field_end)
{
  int32_t len;
  
  if (!(tmpl && field_start && field_end))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((len = fiid_template_block_len (tmpl, field_start, field_end)) < 0)
    return (-1);

  if (len % 8 != 0)
    {
      errno = EINVAL;
      return (-1);
    }

  return (BITS_ROUND_BYTES (len));
}

static int32_t
_fiid_obj_field_start_end (fiid_obj_t obj, 
                           uint8_t *field, 
                           uint32_t *start, 
                           uint32_t *end)
{
  int i = 0;
  int _start = 0;
  int _end = 0; 
  
  assert(obj && obj->magic == FIID_OBJ_MAGIC && field && start && end);

  for (i = 0; obj->field_data[i].max_field_len != 0; i++)
    {
      if (!strcmp (obj->field_data[i].key, field))
	{
	  _end = _start + obj->field_data[i].max_field_len;
          *start = _start;
          *end = _end;
          return (obj->field_data[i].max_field_len);
	}
      _start += obj->field_data[i].max_field_len;
    }
  
  errno = ESPIPE; 		/* Invalid seek */
  return (-1);
}

static int32_t
_fiid_obj_field_start (fiid_obj_t obj, uint8_t *field)
{
  uint32_t start = 0;
  uint32_t end = 0; //excluded always
  
  assert(obj && obj->magic == FIID_OBJ_MAGIC && field);

  ERR (_fiid_obj_field_start_end (obj, field, &start, &end) != -1);
  return (start);
}

static int32_t
_fiid_obj_field_end (fiid_obj_t obj, uint8_t *field)
{
  uint32_t start = 0;
  uint32_t end = 0; //excluded always
  
  assert(obj && obj->magic == FIID_OBJ_MAGIC && field);

  ERR (_fiid_obj_field_start_end (obj, field, &start, &end) != -1);
  return (end);
}

static int32_t
_fiid_obj_field_len (fiid_obj_t obj, uint8_t *field)
{
  int i;

  assert(obj && obj->magic == FIID_OBJ_MAGIC && field);
  
  for (i = 0; obj->field_data[i].max_field_len != 0; i++)
    {
      if (!strcmp (obj->field_data[i].key, field))
	return (obj->field_data[i].max_field_len);
    }
  
  errno = ESPIPE; 		/* Invalid seek */
  return (-1);
}

fiid_obj_t 
fiid_obj_create (fiid_template_t tmpl)
{
  fiid_obj_t obj = NULL;
  uint32_t max_pkt_len = 0;
  int i;
  
  if (!tmpl)
    {
      errno = EINVAL;
      goto cleanup;
    }
 
  if (!(obj = (fiid_obj_t)ipmi_xmalloc(sizeof(struct fiid_obj))))
    {
      errno = ENOMEM;
      goto cleanup;
    }
  obj->magic = FIID_OBJ_MAGIC;
  
  if ((obj->data_len = _fiid_template_len_bytes (tmpl, &obj->field_data_len)) < 0)
    goto cleanup;

  if (!obj->field_data_len)
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj->data = ipmi_xmalloc(obj->data_len)))
    {
      errno = ENOMEM;
      goto cleanup;
    }
  
  if (!(obj->field_data = ipmi_xmalloc(obj->field_data_len * sizeof(struct fiid_field_data))))
    {
      errno = ENOMEM;
      goto cleanup;
    }
  memset(obj->field_data, '\0', obj->field_data_len * sizeof(struct fiid_field_data));

  for (i = 0; i < obj->field_data_len; i++)
    {
#ifndef NDEBUG
      int j = 0;
      for (j = 0; j < i; j++)
	{
	  if (!strncmp(obj->field_data[j].key, tmpl[i].key, FIID_FIELD_MAX))
	    {
	      errno = EINVAL;
	      goto cleanup;
	    }
	}
#endif /* !NDEBUG */
      obj->field_data[i].max_field_len = tmpl[i].max_field_len;
      strncpy(obj->field_data[i].key, tmpl[i].key, FIID_FIELD_MAX);
      obj->field_data[i].key[FIID_FIELD_MAX - 1] = '\0';
      obj->field_data[i].set_field_len = 0;
      max_pkt_len += tmpl[i].max_field_len;
    }

  if (max_pkt_len % 8)
    {
      errno = EINVAL;
      goto cleanup;
    }

  return (obj);
  
 cleanup:
  if (obj)
    {
      if (obj->data)
        ipmi_xfree(obj->data);
      if (obj->field_data)
        ipmi_xfree(obj->field_data);
      ipmi_xfree(obj);
    }

  return (NULL);
}

int8_t
fiid_obj_destroy (fiid_obj_t obj)
{
  if (!(obj && obj->magic == FIID_OBJ_MAGIC))
    {
      errno = EINVAL;
      return (-1);
    }

  ipmi_xfree(obj->data);
  ipmi_xfree(obj->field_data);
  ipmi_xfree(obj);
  
  return (0);
}

fiid_obj_t 
fiid_obj_dup (fiid_obj_t src_obj)
{
  fiid_obj_t dest_obj = NULL;
  
  if (!(src_obj && src_obj->magic == FIID_OBJ_MAGIC))
    {
      errno = EINVAL;
      goto cleanup;
    }
 
  if (!(dest_obj = ipmi_xmalloc(sizeof(struct fiid_obj))))
    {
      errno = ENOMEM;
      goto cleanup;
    }
  dest_obj->magic = src_obj->magic;
  dest_obj->data_len = src_obj->data_len;
  dest_obj->field_data_len = src_obj->field_data_len;

  if (!(dest_obj->data = ipmi_xmalloc(src_obj->data_len)))
    {
      errno = ENOMEM;
      goto cleanup;
    }
  memcpy(dest_obj->data, src_obj->data, src_obj->data_len);

  if (!(dest_obj->field_data = ipmi_xmalloc(dest_obj->field_data_len * sizeof(struct fiid_field_data))))
    {
      errno = ENOMEM;
      goto cleanup;
    }
  memcpy(dest_obj->field_data, 
         src_obj->field_data, 
         src_obj->field_data_len * sizeof(struct fiid_field_data));

  return dest_obj;

 cleanup:
  if (dest_obj)
    {
      if (dest_obj->data)
        ipmi_xfree(dest_obj->data);
      if (dest_obj->field_data)
        ipmi_xfree(dest_obj->field_data);
      ipmi_xfree(dest_obj);
    }
  return NULL;
}

int8_t 
fiid_obj_valid(fiid_obj_t obj)
{
  if (!(obj && obj->magic == FIID_OBJ_MAGIC))
    return (0);
  return (1);
}

int32_t
fiid_obj_max_len(fiid_obj_t obj)
{
  int32_t counter = 0;
  int i;

  if (!(obj && obj->magic == FIID_OBJ_MAGIC))
    {
      errno = EINVAL;
      return (-1);
    }

  for (i = 0; obj->field_data[i].max_field_len != 0; i++)
    counter += obj->field_data[i].max_field_len;

  return (counter);
}

int32_t
fiid_obj_max_len_bytes(fiid_obj_t obj)
{
  int32_t len;

  if (!(obj && obj->magic == FIID_OBJ_MAGIC))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((len = fiid_obj_max_len (obj)) < 0)
    return (-1);

  return (BITS_ROUND_BYTES (len));
}

int32_t
fiid_obj_len(fiid_obj_t obj)
{
  int32_t counter = 0;
  int i;

  if (!(obj && obj->magic == FIID_OBJ_MAGIC))
    {
      errno = EINVAL;
      return (-1);
    }

  for (i = 0; obj->field_data[i].max_field_len != 0; i++)
    counter += obj->field_data[i].set_field_len;

  return (counter);
}

int32_t
fiid_obj_len_bytes(fiid_obj_t obj)
{
  int32_t len;

  if (!(obj && obj->magic == FIID_OBJ_MAGIC))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((len = fiid_obj_len (obj)) < 0)
    return (-1);

  return (BITS_ROUND_BYTES (len));
}

static int32_t 
_fiid_obj_lookup_field_index(fiid_obj_t obj, uint8_t *field)
{
  int i;

  assert(obj && obj->magic == FIID_OBJ_MAGIC && field);

  for (i = 0; obj->field_data[i].max_field_len != 0; i++)
    {
      if (!strcmp (obj->field_data[i].key, field))
        return (i);
    }

  errno = ESPIPE; 		/* Invalid seek */
  return (-1);
}

int32_t
fiid_obj_max_field_len(fiid_obj_t obj, uint8_t *field)
{
  int key_index = -1;

  if (!(obj && obj->magic == FIID_OBJ_MAGIC && field))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((key_index = _fiid_obj_lookup_field_index(obj, field)) < 0)
    return (-1);
  
  return (obj->field_data[key_index].max_field_len);
}

int32_t
fiid_obj_max_field_len_bytes(fiid_obj_t obj, uint8_t *field)
{
  int32_t len;

  if (!(obj && obj->magic == FIID_OBJ_MAGIC && field))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((len = fiid_obj_max_field_len (obj, field)) < 0)
    return (-1);

  return (BITS_ROUND_BYTES (len));
}

int32_t
fiid_obj_field_len(fiid_obj_t obj, uint8_t *field)
{
  int key_index = -1;

  if (!(obj && obj->magic == FIID_OBJ_MAGIC && field))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((key_index = _fiid_obj_lookup_field_index(obj, field)) < 0)
    return (-1);
  
  return (obj->field_data[key_index].set_field_len);
}

int32_t
fiid_obj_field_len_bytes(fiid_obj_t obj, uint8_t *field)
{
  int32_t len;

  if (!(obj && obj->magic == FIID_OBJ_MAGIC && field))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((len = fiid_obj_field_len (obj, field)) < 0)
    return (-1);

  return (BITS_ROUND_BYTES (len));
}

int8_t
fiid_obj_clear (fiid_obj_t obj)
{
  int i;
  
  if (!(obj && obj->magic == FIID_OBJ_MAGIC))
    {
      errno = EINVAL;
      return (-1);
    }

  memset(obj->data, '\0', obj->data_len);
  
  for (i =0; i < obj->field_data_len; i++)
    obj->field_data[i].set_field_len = 0;

  return (0);
}

int8_t 
fiid_obj_clear_field (fiid_obj_t obj, uint8_t *field)
{
  int32_t bits_len, bytes_len;
  int key_index = -1;
  
  if (!(obj && obj->magic == FIID_OBJ_MAGIC && field))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((key_index = _fiid_obj_lookup_field_index(obj, field)) < 0)
    return (-1);

  if (!obj->field_data[key_index].set_field_len)
    return (0);

  bits_len = _fiid_obj_field_len (obj, field);
  ERR (bits_len != -1);

  if (bits_len <= 64)
    {
      uint64_t val = 0;
      
      if (fiid_obj_set(obj, field, val) < 0)
	return (-1);
    }
  else
    {
      int32_t field_start, field_offset;

      /* achu: We assume the field must start on a byte boundary and end
       * on a byte boundary.
       */

      if (bits_len % 8 != 0)
	{
	  errno = EINVAL;
	  return (-1);
	}

      bytes_len = BITS_ROUND_BYTES (bytes_len);

      field_start = _fiid_obj_field_start (obj, field);
      ERR (field_start != -1);

      if (field_start % 8 != 0)
	{
	  errno = EINVAL;
	  return (-1);
	}

      field_offset = BITS_ROUND_BYTES(field_start);
      memset ((obj->data + field_offset), '\0', bytes_len);
    }

  obj->field_data[key_index].set_field_len = 0;
  return (0);
}

int8_t
fiid_obj_field_lookup (fiid_obj_t obj, uint8_t *field)
{
  int start = 0;
  int end = 0; //excluded always
  
  if (_fiid_obj_field_start_end (obj, field, &start, &end) != -1)
    return (1);
  else
    return (0);
}

int8_t
fiid_obj_set (fiid_obj_t obj, 
	      uint8_t *field, 
	      uint64_t val)
{
  uint32_t start_bit_pos = 0;
  uint32_t end_bit_pos = 0; //excluded always
  int byte_pos = 0;
  int start_bit_in_byte_pos = 0;
  int end_bit_in_byte_pos = 0;
  int field_len = 0;
  int bytes_used = 0;
  int key_index = -1;
  uint64_t merged_val = 0;
  uint8_t *temp_data = NULL;

  if (!(obj && obj->magic == FIID_OBJ_MAGIC && field))
    {
      errno = EINVAL;
      goto cleanup;
    }

  if ((key_index = _fiid_obj_lookup_field_index(obj, field)) < 0)
    return (-1);

  field_len = _fiid_obj_field_start_end (obj, field, &start_bit_pos, &end_bit_pos);
  ERR(field_len != -1);

  if (field_len > 64)
    field_len = 64;

  byte_pos = start_bit_pos / 8;

  /* in byte_pos, start_bit_pos is  */
  start_bit_in_byte_pos = start_bit_pos - (byte_pos * 8);
  
  /* and it spans into... */
  if (start_bit_in_byte_pos + field_len > 8)
    {
      int field_len_temp = field_len;

      if (start_bit_in_byte_pos)
        bytes_used++;

      field_len_temp -= start_bit_in_byte_pos;

      bytes_used += (field_len_temp / 8);

      field_len_temp -= (bytes_used * 8);

      if (field_len_temp)
        bytes_used++;
    }
  else 
    {
      end_bit_in_byte_pos = start_bit_in_byte_pos + field_len;
      bytes_used++;
    }

  if (bytes_used > 1)
    {
      int start_val_pos = 0;
      int end_val_pos = 0;
      uint64_t extracted_val = 0;
      int field_len_left = field_len;
      int i;
      
      if (!(temp_data = ipmi_xmalloc(obj->data_len)))
        {
          errno = ENOMEM;
          goto cleanup;
        }
      memcpy(temp_data, obj->data, obj->data_len);

      for (i = 0; i < bytes_used; i++)
	{
	  if (i == 0)
            {
              end_val_pos = 8 - start_bit_in_byte_pos;
              field_len_left -= end_val_pos;
            }
	  else if (i != (bytes_used - 1))
            {
              end_val_pos += 8;
              field_len_left -= 8;
            }
          else
            end_val_pos += field_len_left;
	  
	  if (i != (bytes_used - 1))
	    end_bit_in_byte_pos = 8;
	  else
	    end_bit_in_byte_pos = field_len_left;

	  if (bits_extract (val, 
                            start_val_pos, 
                            end_val_pos, 
                            &extracted_val) < 0)
            goto cleanup;

          if (bits_merge (temp_data[byte_pos + i], 
                          start_bit_in_byte_pos, 
                          end_bit_in_byte_pos, 
                          extracted_val,
                          &merged_val) < 0)
            goto cleanup;

          temp_data[byte_pos + i] = merged_val;
	  start_bit_in_byte_pos = 0;
	  start_val_pos = end_val_pos;
	}

      memcpy(obj->data, temp_data, obj->data_len);
      obj->field_data[key_index].set_field_len = field_len;
    }
  else
    {
      if (bits_merge (obj->data[byte_pos], 
                      start_bit_in_byte_pos, 
                      end_bit_in_byte_pos, 
                      val,
                      &merged_val) < 0)
        goto cleanup;
      obj->data[byte_pos] = merged_val;
      obj->field_data[key_index].set_field_len = field_len;
    }

  ipmi_xfree(temp_data);
  return (0);

 cleanup:
  if (temp_data)
    ipmi_xfree(temp_data);
  return (-1);
}

int8_t
fiid_obj_get (fiid_obj_t obj, 
	      uint8_t *field, 
	      uint64_t *val)
{
  uint32_t start_bit_pos = 0;
  uint32_t end_bit_pos = 0; //excluded always
  int byte_pos = 0;
  int start_bit_in_byte_pos = 0;
  int end_bit_in_byte_pos = 0;
  int field_len = 0;
  int bytes_used = 0;
  int key_index = -1;
  uint64_t merged_val = 0;
  
  if (!(obj && obj->magic == FIID_OBJ_MAGIC && field && val))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((key_index = _fiid_obj_lookup_field_index(obj, field)) < 0)
    return (-1);

  if (!obj->field_data[key_index].set_field_len)
    return (0);

  field_len = _fiid_obj_field_start_end (obj, field, &start_bit_pos, &end_bit_pos);
  ERR(field_len != -1);
  
  if (field_len > 64)
    field_len = 64;

  byte_pos = start_bit_pos / 8;

  /* in byte_pos, start_bit_pos is  */
  start_bit_in_byte_pos = start_bit_pos - (byte_pos * 8);
  
  /* and it spans into... */
  if (start_bit_in_byte_pos + field_len > 8)
    {
      int field_len_temp = field_len;

      if (start_bit_in_byte_pos)
        bytes_used++;

      field_len_temp -= start_bit_in_byte_pos;

      bytes_used += (field_len_temp / 8);

      field_len_temp -= (bytes_used * 8);

      if (field_len_temp)
        bytes_used++;
    }
  else 
    {
      end_bit_in_byte_pos = start_bit_in_byte_pos + field_len;
      bytes_used++;
    }
  
  if (bytes_used > 1)
    {
      int start_val_pos = 0;
      int end_val_pos = 0;
      uint64_t extracted_val = 0;
      uint64_t final_val = 0x0;
      int field_len_left = field_len;
      int i;
      
      for (i = 0; i < bytes_used; i++)
	{
	  if (i == 0)
            {
              end_val_pos = 8 - start_bit_in_byte_pos;
              field_len_left -= end_val_pos;
            }
	  else if (i != (bytes_used - 1))
            {
              end_val_pos += 8;
              field_len_left -= 8;
            }
          else
            end_val_pos += field_len_left;
	  
	  if (i != (bytes_used - 1))
	    end_bit_in_byte_pos = 8;
	  else
	    end_bit_in_byte_pos = field_len_left;
	  
	  if (bits_extract (obj->data[byte_pos + i],
                            start_bit_in_byte_pos,
                            end_bit_in_byte_pos,
                            &extracted_val) < 0)
            return (-1);
	  
	  if (bits_merge (final_val, 
                          start_val_pos, 
                          end_val_pos, 
                          extracted_val, 
                          &merged_val) < 0)
            return (-1);
	  
          final_val = merged_val;
	  start_bit_in_byte_pos = 0;
	  start_val_pos = end_val_pos;
	}

      *val = 0;
      *val = final_val;
    }
  else
    {
      if (bits_extract (obj->data[byte_pos], 
                        start_bit_in_byte_pos, 
                        end_bit_in_byte_pos,
                        &merged_val) < 0)
        return (-1);

      *val = 0;
      *val = merged_val;
    }

  return (1);
}

int32_t 
fiid_obj_set_data (fiid_obj_t obj, 
		   uint8_t *field, 
		   uint8_t *data, 
		   uint32_t data_len)
{
  int32_t bits_len, bytes_len, field_start;
  int field_offset; 
  int key_index = -1;
  
  if (!(obj && obj->magic == FIID_OBJ_MAGIC && field && data))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((key_index = _fiid_obj_lookup_field_index(obj, field)) < 0)
    return (-1);

  /* achu: We assume the field must start on a byte boundary and end
   * on a byte boundary.
   */

  field_start = _fiid_obj_field_start (obj, field);
  ERR (field_start != -1);

  if (field_start % 8 != 0)
    {
      errno = EINVAL;
      return (-1);
    }

  bits_len = _fiid_obj_field_len (obj, field);
  ERR (bits_len != -1);

  if (bits_len % 8 != 0)
    {
      errno = EINVAL;
      return (-1);
    }

  bytes_len = BITS_ROUND_BYTES (bits_len);
  if (data_len > bytes_len)
    data_len = bytes_len;
  
  field_offset = BITS_ROUND_BYTES(field_start);
  memcpy ((obj->data + field_offset), data, data_len);
  obj->field_data[key_index].set_field_len = (data_len * 8);
  
  return (data_len);
}

int32_t 
fiid_obj_get_data (fiid_obj_t obj, 
		   uint8_t *field, 
		   uint8_t *data,
                   uint32_t data_len)
{
  int32_t bits_len, bytes_len, field_start;
  int field_offset;
  int key_index = -1;

  if (!(obj && obj->magic == FIID_OBJ_MAGIC && field && data))
    {
      errno = EINVAL;
      return (-1);
    }

  if ((key_index = _fiid_obj_lookup_field_index(obj, field)) < 0)
    return (-1);

  if (!obj->field_data[key_index].set_field_len)
    return (0);

  /* achu: We assume the field must start on a byte boundary and end
   * on a byte boundary.
   */

  field_start = _fiid_obj_field_start (obj, field);
  ERR (field_start != -1);

  if (field_start % 8 != 0)
    {
      errno = EINVAL;
      return (-1);
    }

  bits_len = _fiid_obj_field_len (obj, field);
  ERR (bits_len != -1);

  if (obj->field_data[key_index].set_field_len < bits_len)
    bits_len = obj->field_data[key_index].set_field_len;

  if (bits_len % 8 != 0)
    {
      errno = EINVAL;
      return (-1);
    }

  bytes_len = BITS_ROUND_BYTES (bits_len);

  if (bytes_len > data_len)
    {
      errno = EMSGSIZE;
      return (-1);
    }

  field_offset = BITS_ROUND_BYTES(field_start);

  memset (data, '\0', data_len);
  memcpy (data, (obj->data + field_offset), bytes_len);

  return (bytes_len);
}

int32_t 
fiid_obj_set_all (fiid_obj_t obj, 
                  uint8_t *data, 
                  uint32_t data_len)
{
  int key_index_end = -1;
  int bits_counter, data_bits_len;
  int i;
  
  if (!(obj && obj->magic == FIID_OBJ_MAGIC && data))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if (data_len > obj->data_len)
    data_len = obj->data_len;

  /* achu: Find index of last field */
  data_bits_len = data_len * 8;
  if (data_len < obj->data_len)
    {
      bits_counter = 0;
      for (i = 0; obj->field_data[i].max_field_len != 0; i++)
        {
          bits_counter += obj->field_data[i].max_field_len;
          if (bits_counter >= data_bits_len)
            {
              /* achu: We assume the data must end on a byte boundary. */
              if (bits_counter % 8 != 0)
                {
                  errno = EINVAL;
                  return (-1);
                }
              else
                break;
            }

        }
      key_index_end = i;
    }
  else
    key_index_end = (obj->field_data_len - 1);

  memcpy (obj->data, data, data_len);

  bits_counter = 0;
  for (i = 0; i < key_index_end; i++)
    {
      obj->field_data[i].set_field_len = obj->field_data[i].max_field_len;
      bits_counter += obj->field_data[i].set_field_len;
    }
  if (data_bits_len < bits_counter + obj->field_data[key_index_end].max_field_len)
    {
      int data_bits_left = data_bits_len - bits_counter;
      obj->field_data[i].set_field_len = data_bits_left;
    }
  else
    obj->field_data[i].set_field_len = obj->field_data[i].max_field_len;

  return (data_len);
}

int32_t 
fiid_obj_get_all (fiid_obj_t obj, 
                  uint8_t *data, 
                  uint32_t data_len)
{
  int32_t bits_len, bytes_len;

  if (!(obj && obj->magic == FIID_OBJ_MAGIC && data))
    {
      errno = EINVAL;
      return -1;
    }

  if ((bits_len = fiid_obj_len(obj)) < 0)
    return -1;
  
  if (bits_len == (obj->data_len * 8))
    bytes_len = obj->data_len;
  else
    bytes_len = BITS_ROUND_BYTES (bits_len);

  if (data_len < bytes_len)
    {
      errno = EMSGSIZE;
      return -1;
    }

  memset(data, '\0', data_len);

  if (bytes_len == obj->data_len)
    memcpy (data, obj->data, bytes_len);
  else
    {
      int i, bytes_written = 0, max_bits_counter = 0, set_bits_counter = 0, 
        optional_bits_counter = 0, data_index = 0, obj_data_index = 0;

      for (i = 0; i < obj->field_data_len; i++)
        {
          int32_t field_bits_max = obj->field_data[i].max_field_len;
          int32_t field_bits_set = obj->field_data[i].set_field_len;

	  max_bits_counter += field_bits_max;

          if (field_bits_set)
            {
              if (optional_bits_counter)
                {
                  errno = EINVAL;
                  goto cleanup;
                }
	      
	      if (field_bits_set != field_bits_max)
		{
		  /* If there is an optional or variable length
		   * field, it cannot have only partial data.
		   */
		  if ((set_bits_counter + field_bits_set) % 8 != 0)
		    {
		      errno = EINVAL;
		      goto cleanup;
		    }
		}

              set_bits_counter += field_bits_set;
              if (!(set_bits_counter % 8))
                {
                  int32_t max_bytes_count = BITS_ROUND_BYTES(max_bits_counter);
                  int32_t set_bytes_count = BITS_ROUND_BYTES(set_bits_counter);

                  memcpy(data + data_index,
                         obj->data + obj_data_index,
                         set_bytes_count);
                  
                  bytes_written += set_bytes_count;
                  data_index += set_bytes_count;
                  obj_data_index += max_bytes_count;
                  set_bits_counter = 0;
		  max_bits_counter = 0;
                }
            }
          else
            {
              /* All information must be collected in byte sized
               * chunks
               */
              if (set_bits_counter)
                {
                  errno = EINVAL;
                  goto cleanup;
                }

              /* Likewise, optional data should be aligned across
               * bytes
               */
              optional_bits_counter += field_bits_max;
              if (optional_bits_counter && !(optional_bits_counter % 8))
                {
		  /* an "assert" */
		  if (optional_bits_counter != max_bits_counter)
		    {
		      errno = EINVAL;
		      goto cleanup;
		    }

                  obj_data_index += BITS_ROUND_BYTES(optional_bits_counter);
                  optional_bits_counter = 0;
		  max_bits_counter = 0;
                }
            }
        }
      
      /* There shouldn't be anything left over */
      if (set_bits_counter)
	{
	  errno = EINVAL;
	  goto cleanup;
	}

      if (bytes_written != bytes_len)
        {
          errno = EINVAL;
          goto cleanup;
        }
    }

  return (bytes_len);

 cleanup:
  if (data)
    memset(data, '\0', data_len);
  return (-1);
}

static int32_t
_fiid_obj_max_block_len (fiid_obj_t obj,
                         uint8_t *field_start,
                         uint8_t *field_end)
{
  int end;
  int start;

  assert(obj && obj->magic == FIID_OBJ_MAGIC && field_start && field_end);

  start = _fiid_obj_field_start (obj, field_start);
  ERR (start != -1);
  end = _fiid_obj_field_end (obj, field_end);
  ERR (end != -1);
  ERR (!(start > end));

  return (end - start);
}

static int32_t
_fiid_obj_block_len (fiid_obj_t obj,
                     uint8_t *field_start,
                     uint8_t *field_end)
{
  int key_index_start = -1, key_index_end = -1;
  int32_t counter = 0;
  int i;

  assert(obj && obj->magic == FIID_OBJ_MAGIC && field_start && field_end);

  if ((key_index_start = _fiid_obj_lookup_field_index(obj, field_start)) < 0)
    return (-1);

  if ((key_index_end = _fiid_obj_lookup_field_index(obj, field_end)) < 0)
    return (-1);

  if (key_index_end > key_index_start)
    {
      errno = EINVAL;
      return (-1);
    }

  if (!(obj && obj->magic == FIID_OBJ_MAGIC))
    {
      errno = EINVAL;
      return (-1);
    }

  for (i = key_index_start; i <= key_index_end; i++)
    counter += obj->field_data[i].set_field_len;
  
  return (counter);
}

int8_t 
fiid_obj_set_block (fiid_obj_t obj, 
                    uint8_t *field_start, 
                    uint8_t *field_end,
                    uint8_t *data, 
                    uint32_t data_len)
{
  int32_t block_bits_start, block_bits_len, block_bytes_len; 
  int key_index_start = -1, key_index_end = -1;
  int bits_counter, data_bits_len;
  int i, field_offset; 
  
  if (!(obj && obj->magic == FIID_OBJ_MAGIC && field_start && field_end && data))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((key_index_start = _fiid_obj_lookup_field_index(obj, field_start)) < 0)
    return (-1);

  if ((key_index_end = _fiid_obj_lookup_field_index(obj, field_end)) < 0)
    return (-1);

  if (key_index_end > key_index_start)
    {
      errno = EINVAL;
      return (-1);
    }

  /* achu: We assume the field must start on a byte boundary and end
   * on a byte boundary.
   */

  block_bits_start = _fiid_obj_field_start (obj, field_start);
  ERR (block_bits_start != -1);
 
  if (block_bits_start % 8 != 0)
    {
      errno = EINVAL;
      return (-1);
    }

  block_bits_len = _fiid_obj_max_block_len (obj, field_start, field_end);
  ERR (block_bits_len != -1);
    
  if (block_bits_len % 8 != 0)
    {
      errno = EINVAL;
      return (-1);
    }

  block_bytes_len = BITS_ROUND_BYTES (block_bits_len);

  if (data_len > block_bytes_len)
    data_len = block_bytes_len;

  /* achu: Potentially adjust index of last field */
  data_bits_len = data_len * 8;
  if (data_len < block_bits_len)
    {
      bits_counter = 0;
      for (i = key_index_start; i <= key_index_end; i++)
        {
          bits_counter += obj->field_data[i].max_field_len;
          if (bits_counter >= data_bits_len)
            {
              /* achu: We assume the data must end on a byte boundary. */
              if (bits_counter % 8 != 0)
                {
                  errno = EINVAL;
                  return (-1);
                }
              else
                break;
            }

        }
      key_index_end = i;
    }

  field_offset = BITS_ROUND_BYTES(block_bits_start);
  memcpy ((obj->data + field_offset), data, data_len);
  
  bits_counter = 0;
  for (i = key_index_start; i < key_index_end; i++)
    {
      obj->field_data[i].set_field_len = obj->field_data[i].max_field_len;
      bits_counter += obj->field_data[i].set_field_len;
    }
  if (data_bits_len < bits_counter + obj->field_data[key_index_end].max_field_len)
    {
      int data_bits_left = data_bits_len - bits_counter;
      obj->field_data[i].set_field_len = data_bits_left;
    }
  else
    obj->field_data[i].set_field_len = obj->field_data[i].max_field_len;

  return (data_len);
}

int8_t 
fiid_obj_get_block (fiid_obj_t obj, 
                    uint8_t *field_start, 
                    uint8_t *field_end,
                    uint8_t *data, 
                    uint32_t data_len)
{
  int32_t block_bits_start, block_bits_max_len, block_bits_set_len, block_bytes_max_len, block_bytes_set_len;
  int key_index_start = -1, key_index_end = -1;
  int field_offset; 
  
  if (!(obj && obj->magic == FIID_OBJ_MAGIC && field_start && field_end && data))
    {
      errno = EINVAL;
      return (-1);
    }
  
  if ((key_index_start = _fiid_obj_lookup_field_index(obj, field_start)) < 0)
    return (-1);

  if ((key_index_end = _fiid_obj_lookup_field_index(obj, field_end)) < 0)
    return (-1);

  if (key_index_start > key_index_end)
    {
      errno = EINVAL;
      return (-1);
    }

  /* achu: We assume the field must start on a byte boundary and ends
   * on a byte boundary.
   */

  block_bits_start = _fiid_obj_field_start (obj, field_start);
  ERR (block_bits_start != -1);
 
  if (block_bits_start % 8 != 0)
    {
      errno = EINVAL;
      return (-1);
    }

  block_bits_max_len = _fiid_obj_max_block_len (obj, field_start, field_end);
  ERR (block_bits_max_len != -1);

  if (block_bits_max_len % 8 != 0)
    {
      errno = EINVAL;
      return (-1);
    }

  block_bits_set_len = _fiid_obj_block_len (obj, field_start, field_end);
  ERR (block_bits_set_len != -1);
    
  if (block_bits_set_len % 8 != 0)
    {
      errno = EINVAL;
      return (-1);
    }

  block_bytes_max_len = BITS_ROUND_BYTES (block_bits_max_len);
  block_bytes_set_len = BITS_ROUND_BYTES (block_bits_set_len);

  if (data_len < block_bytes_set_len)
    {
      errno = EMSGSIZE;
      return (-1);
    }

  field_offset = BITS_ROUND_BYTES(block_bits_start);
 
  memset(data, '\0', data_len);

  if (block_bytes_set_len == block_bytes_max_len)
    memcpy (data, (obj->data + field_offset), block_bytes_set_len);
  else
    {
      int i, bytes_written = 0, max_bits_counter = 0, set_bits_counter = 0, 
        optional_bits_counter = 0, data_index = 0, obj_data_index = field_offset;

      for (i = key_index_start; i <= key_index_end; i++)
        {
          int32_t field_bits_max = obj->field_data[i].max_field_len;
          int32_t field_bits_set = obj->field_data[i].set_field_len;

	  max_bits_counter += field_bits_max;

          if (field_bits_set)
            {
              if (optional_bits_counter)
                {
                  errno = EINVAL;
                  goto cleanup;
                }
	      
	      if (field_bits_set != field_bits_max)
		{
		  /* If there is an optional or variable length
		   * field, it cannot have only partial data.
		   */
		  if ((set_bits_counter + field_bits_set) % 8 != 0)
		    {
		      errno = EINVAL;
		      goto cleanup;
		    }
		}

              set_bits_counter += field_bits_set;
              if (!(set_bits_counter % 8))
                {
                  int32_t max_bytes_count = BITS_ROUND_BYTES(max_bits_counter);
                  int32_t set_bytes_count = BITS_ROUND_BYTES(set_bits_counter);

                  memcpy(data + data_index,
                         obj->data + obj_data_index,
                         set_bytes_count);
                  
                  bytes_written += set_bytes_count;
                  data_index += set_bytes_count;
                  obj_data_index += max_bytes_count;
                  set_bits_counter = 0;
		  max_bits_counter = 0;
                }
            }
          else
            {
              /* All information must be collected in byte sized
               * chunks
               */
              if (set_bits_counter)
                {
                  errno = EINVAL;
                  goto cleanup;
                }

              /* Likewise, optional data should be aligned across
               * bytes
               */
              optional_bits_counter += field_bits_max;
              if (optional_bits_counter && !(optional_bits_counter % 8))
                {
		  /* an "assert" */
		  if (optional_bits_counter != max_bits_counter)
		    {
		      errno = EINVAL;
		      goto cleanup;
		    }

                  obj_data_index += BITS_ROUND_BYTES(optional_bits_counter);
                  optional_bits_counter = 0;
		  max_bits_counter = 0;
                }
            }

        }
      
      /* There shouldn't be anything left over */
      if (set_bits_counter)
	{
	  errno = EINVAL;
	  goto cleanup;
	}

      if (bytes_written != block_bytes_set_len)
        {
          errno = EINVAL;
          goto cleanup;
        }
    }
  
  return (block_bytes_set_len);

 cleanup:
  if (data)
    memset(data, '\0', data_len);
  return (-1);
}

fiid_iterator_t
fiid_iterator_create(fiid_obj_t obj)
{
  fiid_iterator_t iter = NULL;
  
  if (!(obj && obj->magic == FIID_OBJ_MAGIC))
    {
      errno = EINVAL;
      goto cleanup;
    }
 
  if (!(iter = (fiid_iterator_t)ipmi_xmalloc(sizeof(struct fiid_iterator))))
    {
      errno = ENOMEM;
      goto cleanup;
    }
  iter->magic = FIID_ITERATOR_MAGIC;
  iter->current_index = 0;
  /* The -1 below is because field_data_len is length of the array.  The iterator
   * is concerned about array indexes.
   */
  iter->last_index = obj->field_data_len - 1;


  if (!(iter->obj = fiid_obj_dup(obj)))
    goto cleanup;

  return (iter);

 cleanup:
  if (iter)
    {
      if (iter->obj)
        fiid_obj_destroy(iter->obj);
      ipmi_xfree(iter);
    }
  
  return (NULL);
}

int8_t
fiid_iterator_destroy(fiid_iterator_t iter)
{
  if (!(iter && iter->magic == FIID_ITERATOR_MAGIC))
    {
      errno = EINVAL;
      return (-1);
    }
  
  fiid_obj_destroy(iter->obj);
  ipmi_xfree(iter);
  
  return (0);
}

int8_t
fiid_iterator_reset(fiid_iterator_t iter)
{
  if (!(iter && iter->magic == FIID_ITERATOR_MAGIC))
    {
      errno = EINVAL;
      return (-1);
    }

  iter->current_index = 0;
  return (0);
}

int8_t
fiid_iterator_next(fiid_iterator_t iter)
{
  if (!(iter && iter->magic == FIID_ITERATOR_MAGIC))
    {
      errno = EINVAL;
      return (-1);
    }

  if (iter->current_index != iter->last_index)
    iter->current_index++;
  return (0);
}

int8_t 
fiid_iterator_end(fiid_iterator_t iter)
{
  if (!(iter && iter->magic == FIID_ITERATOR_MAGIC))
    {
      errno = EINVAL;
      return (-1);
    }

  return ((iter->current_index == iter->last_index) ? 1 : 0);
}

int32_t
fiid_iterator_max_field_len(fiid_iterator_t iter)
{
  if (!(iter && iter->magic == FIID_ITERATOR_MAGIC))
    {
      errno = EINVAL;
      return (-1);
    }

  return (iter->obj->field_data[iter->current_index].max_field_len);
}

int32_t
fiid_iterator_field_len(fiid_iterator_t iter)
{
  if (!(iter && iter->magic == FIID_ITERATOR_MAGIC))
    {
      errno = EINVAL;
      return (-1);
    }

  return (iter->obj->field_data[iter->current_index].set_field_len);
}

uint8_t *
fiid_iterator_key(fiid_iterator_t iter)
{
  if (!(iter && iter->magic == FIID_ITERATOR_MAGIC))
    {
      errno = EINVAL;
      return (NULL);
    }

  return (iter->obj->field_data[iter->current_index].key);
}

int32_t
fiid_iterator_get(fiid_iterator_t iter, uint64_t *val)
{
  uint8_t *key;

  if (!(iter && iter->magic == FIID_ITERATOR_MAGIC))
    {
      errno = EINVAL;
      return (-1);
    }
  
  key = iter->obj->field_data[iter->current_index].key;
  return (fiid_obj_get(iter->obj, key, val));
}

int32_t
fiid_iterator_get_data(fiid_iterator_t iter, uint8_t *data, uint32_t data_len)
{
  uint8_t *key;

  if (!(iter && iter->magic == FIID_ITERATOR_MAGIC))
    {
      errno = EINVAL;
      return (-1);
    }

  key = iter->obj->field_data[iter->current_index].key;
  return (fiid_obj_get_data(iter->obj, key, data, data_len));
}

fiid_field_t * 
__fiid_template_make (uint8_t dummy, ...)
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
      
      tmpl_dynamic[i].max_field_len = len;
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

