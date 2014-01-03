/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#include <limits.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/fiid/fiid.h"

#include "libcommon/ipmi-bit-ops.h"

#include "freeipmi-portability.h"
#include "secure.h"

#define FIID_OBJ_MAGIC 0xf00fd00d
#define FIID_ITERATOR_MAGIC 0xd00df00f

struct fiid_field_data
{
  unsigned int max_field_len;
  char key[FIID_FIELD_MAX_KEY_LEN + 1];
  unsigned int set_field_len;
  unsigned int flags;
};

struct fiid_obj
{
  uint32_t magic;
  fiid_err_t errnum;
  uint8_t *data;
  unsigned int data_len;
  struct fiid_field_data *field_data;
  unsigned int field_data_len;
  int makes_packet_sufficient;
};

struct fiid_iterator
{
  uint32_t magic;
  fiid_err_t errnum;
  unsigned int current_index;
  unsigned int last_index;
  struct fiid_obj *obj;
};

static char * fiid_errmsg[] =
  {
    "success",
    "fiid object null",
    "fiid object invalid",
    "fiid iterator null",
    "fiid iterator invalid",
    "invalid parameter",
    "invalid template specified",
    "field not found",
    "template key invalid",
    "template flags invalid",
    "template not byte aligned",
    "field not byte aligned",
    "block not byte aligned",
    "buffer too small to hold result",
    "template max field length mismatch",
    "template key mismatch",
    "template flags mismatch",
    "template length mismatch",
    "data not byte aligned",
    "required field missing",
    "fixed length field invalid",
    "data not available",
    "not identical",
    "out of memory",
    "internal error",
    "errnum out of range",
  };

static int
_fiid_template_check_valid_keys (fiid_template_t tmpl)
{
  unsigned int i;

  assert (tmpl);

  for (i = 0; tmpl[i].max_field_len; i++)
    {
      unsigned int len;

      len = strlen (tmpl[i].key);
      if (!len || len > FIID_FIELD_MAX_KEY_LEN)
        return (-1);
    }

  return (0);
}

static int
_fiid_template_check_valid_flags (fiid_template_t tmpl)
{
  unsigned int i;

  assert (tmpl);

  for (i = 0; tmpl[i].max_field_len; i++)
    {
      if (!FIID_FIELD_REQUIRED_FLAG_VALID (tmpl[i].flags)
          || !FIID_FIELD_LENGTH_FLAG_VALID (tmpl[i].flags))
        return (-1);
    }

  return (0);
}

static int
_fiid_template_len (fiid_template_t tmpl,
                    unsigned int *tmpl_field_count)
{
  unsigned int i, len = 0;

  assert (tmpl);
  assert (tmpl_field_count);

  for (i = 0; tmpl[i].max_field_len; i++)
    {
      if (tmpl[i].max_field_len > INT_MAX)
        {
          /* FIID_ERR_OVERFLOW */
          errno = EINVAL;
          return (-1);
        }

      len += tmpl[i].max_field_len;

      /* check for integer overflow */
      if (len < tmpl[i].max_field_len)
        {
          /* FIID_ERR_OVERFLOW */
          errno = EINVAL;
          return (-1);
        }
    }

  if (len > INT_MAX)
    {
      /* FIID_ERR_OVERFLOW */
      errno = EINVAL;
      return (-1);
    }

  if (len % 8)
    {
      /* FIID_ERR_TEMPLATE_NOT_BYTE_ALIGNED */
      errno = EINVAL;
      return (-1);
    }

  *tmpl_field_count = (i + 1);
  return (len);
}

static int
_fiid_template_len_bytes (fiid_template_t tmpl,
                          unsigned int *tmpl_field_count)
{
  int len;

  assert (tmpl);
  assert (tmpl_field_count);

  if ((len = _fiid_template_len (tmpl,
                                 tmpl_field_count)) < 0)
    return (-1);

  return (BITS_ROUND_BYTES (len));
}

int
fiid_template_field_lookup (fiid_template_t tmpl,
                            const char *field)
{
  unsigned int i;

  if (!(tmpl && field))
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if (_fiid_template_check_valid_keys (tmpl) < 0)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  for (i = 0; tmpl[i].max_field_len; i++)
    {
      if (!strcmp (tmpl[i].key, field))
        return (1);
    }

  return (0);
}

int
FIID_TEMPLATE_FIELD_LOOKUP (fiid_template_t tmpl,
                            const char *field)
{
  int ret;

  if ((ret = fiid_template_field_lookup (tmpl, field)) < 0)
    return (ret);

  if (!ret)
    {
      errno = EINVAL;
      return (-1);
    }

  return (ret);
}

int
fiid_template_len (fiid_template_t tmpl)
{
  unsigned int temp;
  int len = 0;

  if (!tmpl)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if ((len = _fiid_template_len (tmpl,
                                 &temp)) < 0)
    return (-1);

  return (len);
}

int
fiid_template_len_bytes (fiid_template_t tmpl)
{
  int len;

  if (!tmpl)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if ((len = fiid_template_len (tmpl)) < 0)
    return (-1);

  if (len % 8)
    {
      /* FIID_ERR_TEMPLATE_NOT_BYTE_ALIGNED */
      errno = EINVAL;
      return (-1);
    }

  return (BITS_ROUND_BYTES (len));
}

static int
_fiid_template_field_start_end (fiid_template_t tmpl,
                                const char *field,
                                unsigned int *start,
                                unsigned int *end)
{
  unsigned int i = 0;
  unsigned int _start = 0;
  unsigned int _end = 0;

  assert (tmpl);
  assert (field);
  assert (start);
  assert (end);

  for (i = 0; tmpl[i].max_field_len; i++)
    {
      if (strcmp (tmpl[i].key, field) == 0)
        {
          if (tmpl[i].max_field_len > INT_MAX)
            {
              /* FIID_ERR_OVERFLOW */
              errno = EINVAL;
              return (-1);
            }
          
          _end = _start + tmpl[i].max_field_len;

          /* check for integer overflow */
          if (_end < tmpl[i].max_field_len)
            {
              /* FIID_ERR_OVERFLOW */
              errno = EINVAL;
              return (-1);
            }

          *start = _start;
          *end = _end;
          return (tmpl[i].max_field_len);
        }

      _start += tmpl[i].max_field_len;

      /* check for integer overflow */
      if (_start < tmpl[i].max_field_len)
        {
          /* FIID_ERR_OVERFLOW */
          errno = EINVAL;
          return (-1);
        }
    }

  /* FIID_ERR_FIELD_NOT_FOUND */
  errno = EINVAL;
  return (-1);
}

int
fiid_template_field_start (fiid_template_t tmpl,
                           const char *field)
{
  unsigned int start = 0;
  unsigned int end = 0;

  if (!tmpl || !field)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if (_fiid_template_check_valid_keys (tmpl) < 0)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if (_fiid_template_field_start_end (tmpl,
                                      field,
                                      &start,
                                      &end) < 0)
    return (-1);

  if (start > INT_MAX)
    {
      /* FIID_ERR_OVERFLOW */
      errno = EINVAL;
      return (-1);
    }
  
  return (start);
}

int
fiid_template_field_start_bytes (fiid_template_t tmpl,
                                 const char *field)
{
  int start = 0;

  if (!tmpl || !field)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if (_fiid_template_check_valid_keys (tmpl) < 0)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if ((start = fiid_template_field_start (tmpl,
                                          field)) < 0)
    return (-1);

  if (start % 8)
    {
      /* FIID_ERR_FIELD_NOT_BYTE_ALIGNED */
      errno = EINVAL;
      return (-1);
    }

  return (BITS_ROUND_BYTES (start));
}

int
fiid_template_field_end (fiid_template_t tmpl,
                         const char *field)
{
  unsigned int start = 0;
  unsigned int end = 0;

  if (!tmpl || !field)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if (_fiid_template_check_valid_keys (tmpl) < 0)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if (_fiid_template_field_start_end (tmpl,
                                      field,
                                      &start,
                                      &end) < 0)
    return (-1);

  if (end > INT_MAX)
    {
      /* FIID_ERR_OVERFLOW */
      errno = EINVAL;
      return (-1);
    }

  return (end);
}

int
fiid_template_field_end_bytes (fiid_template_t tmpl,
                               const char *field)
{
  int end = 0;

  if (!tmpl || !field)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if (_fiid_template_check_valid_keys (tmpl) < 0)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if ((end = fiid_template_field_end (tmpl,
                                      field)) < 0)
    return (-1);

  if (end % 8)
    {
      /* FIID_ERR_FIELD_NOT_BYTE_ALIGNED */
      errno = EINVAL;
      return (-1);
    }

  return (BITS_ROUND_BYTES (end));
}

int
fiid_template_field_len (fiid_template_t tmpl,
                         const char *field)
{
  unsigned int i;

  if (!tmpl || !field)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if (_fiid_template_check_valid_keys (tmpl) < 0)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  for (i = 0; tmpl[i].max_field_len; i++)
    {     
      if (!strcmp (tmpl[i].key, field))
        {
          if (tmpl[i].max_field_len > INT_MAX)
            {
              /* FIID_ERR_OVERFLOW */
              errno = EINVAL;
              return (-1);
            }
          
          return (tmpl[i].max_field_len);
        }
    }

  /* FIID_ERR_FIELD_NOT_FOUND */
  errno = EINVAL;
  return (-1);
}

int
fiid_template_field_len_bytes (fiid_template_t tmpl,
                               const char *field)
{
  int len;

  if (!tmpl || !field)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if (_fiid_template_check_valid_keys (tmpl) < 0)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if ((len = fiid_template_field_len (tmpl,
                                      field)) < 0)
    return (-1);

  if (len % 8)
    {
      /* FIID_ERR_FIELD_NOT_BYTE_ALIGNED */
      errno = EINVAL;
      return (-1);
    }

  return (BITS_ROUND_BYTES (len));
}

int
fiid_template_block_len (fiid_template_t tmpl,
                         const char *field_start,
                         const char *field_end)
{
  int start;
  int end;

  if (!tmpl || !field_start || !field_end)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if (_fiid_template_check_valid_keys (tmpl) < 0)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if ((end = fiid_template_field_end (tmpl,
                                      field_end)) < 0)
    return (-1);

  if ((start = fiid_template_field_start (tmpl,
                                          field_start)) < 0)
    return (-1);

  if (start > end)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  return (end - start);
}

int
fiid_template_block_len_bytes (fiid_template_t tmpl,
                               const char *field_start,
                               const char *field_end)
{
  int len;

  if (!tmpl || !field_start || !field_end)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if (_fiid_template_check_valid_keys (tmpl) < 0)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if ((len = fiid_template_block_len (tmpl,
                                      field_start,
                                      field_end)) < 0)
    return (-1);

  if (len % 8)
    {
      /* FIID_ERR_BLOCK_NOT_BYTE_ALIGNED */
      errno = EINVAL;
      return (-1);
    }

  return (BITS_ROUND_BYTES (len));
}

int
fiid_template_compare (fiid_template_t tmpl1,
                       fiid_template_t tmpl2)
{
  unsigned int i;

  if (!tmpl1 || !tmpl2)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if (_fiid_template_check_valid_keys (tmpl1) < 0)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  if (_fiid_template_check_valid_keys (tmpl2) < 0)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      return (-1);
    }

  for (i = 0; tmpl1[i].max_field_len; i++)
    {
      if (tmpl1[i].max_field_len != tmpl2[i].max_field_len)
        return (0);

      if (strcmp (tmpl1[i].key, tmpl2[i].key))
        return (0);

      if (tmpl1[i].flags != tmpl2[i].flags)
        return (0);
    }

  if (tmpl2[i].max_field_len)
    return (0);

  return (1);
}

int
FIID_TEMPLATE_COMPARE (fiid_template_t tmpl1,
                       fiid_template_t tmpl2)
{
  int ret;

  if ((ret = fiid_template_compare (tmpl1, tmpl2)) < 0)
    return (ret);

  if (!ret)
    {
      errno = EINVAL;
      return (-1);
    }

  return (ret);
}

void
fiid_template_free (fiid_field_t *tmpl_dynamic)
{
  free (tmpl_dynamic);
}

static int
_fiid_obj_field_start_end (fiid_obj_t obj,
                           const char *field,
                           unsigned int *start,
                           unsigned int *end)
{
  unsigned int i = 0;
  unsigned int _start = 0;
  unsigned int _end = 0;

  assert (obj);
  assert (obj->magic == FIID_OBJ_MAGIC);
  assert (field);
  assert (start);
  assert (end);

  /* integer overflow conditions checked during object creation */
  for (i = 0; obj->field_data[i].max_field_len; i++)
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

  obj->errnum = FIID_ERR_FIELD_NOT_FOUND;
  return (-1);
}

static int
_fiid_obj_field_start (fiid_obj_t obj, const char *field)
{
  unsigned int start = 0;
  unsigned int end = 0; /* excluded always */

  assert (obj);
  assert (obj->magic == FIID_OBJ_MAGIC);
  assert (field);

  if (_fiid_obj_field_start_end (obj, field, &start, &end) < 0)
    return (-1);

  return (start);
}

static int
_fiid_obj_field_end (fiid_obj_t obj, const char *field)
{
  unsigned int start = 0;
  unsigned int end = 0; /* excluded always */

  assert (obj);
  assert (obj->magic == FIID_OBJ_MAGIC);
  assert (field);

  if (_fiid_obj_field_start_end (obj, field, &start, &end) < 0)
    return (-1);

  return (end);
}

static int
_fiid_obj_field_len (fiid_obj_t obj, const char *field)
{
  unsigned int i;

  assert (obj);
  assert (obj->magic == FIID_OBJ_MAGIC);
  assert (field);

  for (i = 0; obj->field_data[i].max_field_len; i++)
    {
      if (!strcmp (obj->field_data[i].key, field))
        return (obj->field_data[i].max_field_len);
    }

  obj->errnum = FIID_ERR_FIELD_NOT_FOUND;
  return (-1);
}

char *
fiid_strerror (fiid_err_t errnum)
{
  if (errnum >= FIID_ERR_SUCCESS && errnum <= FIID_ERR_ERRNUMRANGE)
    return (fiid_errmsg[errnum]);
  else
    return (fiid_errmsg[FIID_ERR_ERRNUMRANGE]);
}

fiid_obj_t
fiid_obj_create (fiid_template_t tmpl)
{
  fiid_obj_t obj = NULL;
  unsigned int max_pkt_len = 0;
  unsigned int i;
  int data_len;

  if (!tmpl)
    {
      /* FIID_ERR_PARAMETERS */
      errno = EINVAL;
      goto cleanup;
    }

  if (_fiid_template_check_valid_keys (tmpl) < 0)
    {
      /* FIID_ERR_TEMPLATE_INVALID */
      errno = EINVAL;
      goto cleanup;
    }

  if (_fiid_template_check_valid_flags (tmpl) < 0)
    {
      /* FIID_ERR_TEMPLATE_INVALID */
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj = (fiid_obj_t)malloc (sizeof (struct fiid_obj))))
    {
      /* FIID_ERR_OUT_OF_MEMORY */
      errno = ENOMEM;
      goto cleanup;
    }
  memset (obj, '\0', sizeof (struct fiid_obj));
  obj->magic = FIID_OBJ_MAGIC;

  /* after call to _fiid_template_len_bytes, we know each field length
   * and total field length won't overflow an int.
   */
  if ((data_len = _fiid_template_len_bytes (tmpl,
                                            &obj->field_data_len)) < 0)
    goto cleanup;
  obj->data_len = data_len;

  if (!obj->field_data_len)
    {
      /* FIID_ERR_TEMPLATE_INVALID */
      errno = EINVAL;
      goto cleanup;
    }

  if (!(obj->data = malloc (obj->data_len)))
    {
      /* FIID_ERR_OUT_OF_MEMORY */
      errno = ENOMEM;
      goto cleanup;
    }
  memset (obj->data, '\0', obj->data_len);

  if (!(obj->field_data = malloc (obj->field_data_len * sizeof (struct fiid_field_data))))
    {
      /* FIID_ERR_OUT_OF_MEMORY */
      errno = ENOMEM;
      goto cleanup;
    }
  memset (obj->field_data, '\0', obj->field_data_len * sizeof (struct fiid_field_data));

  for (i = 0; i < obj->field_data_len; i++)
    {
#ifndef NDEBUG
      if (tmpl[i].max_field_len)
        {
          unsigned int j;

          for (j = 0; j < i; j++)
            {
              if (!strncmp (obj->field_data[j].key, tmpl[i].key, FIID_FIELD_MAX_KEY_LEN))
                {
                  /* FIID_ERR_TEMPLATE_INVALID */
                  errno = EINVAL;
                  goto cleanup;
                }
            }
        }
#endif /* !NDEBUG */
      obj->field_data[i].max_field_len = tmpl[i].max_field_len;
      memset (obj->field_data[i].key, '\0', FIID_FIELD_MAX_KEY_LEN + 1);
      strncpy (obj->field_data[i].key, tmpl[i].key, FIID_FIELD_MAX_KEY_LEN);
      obj->field_data[i].set_field_len = 0;
      obj->field_data[i].flags = tmpl[i].flags;
      max_pkt_len += tmpl[i].max_field_len;

      if (obj->field_data[i].flags & FIID_FIELD_MAKES_PACKET_SUFFICIENT)
        obj->makes_packet_sufficient = 1;
    }

  if (max_pkt_len % 8)
    {
      /* FIID_ERR_TEMPLATE_NOT_BYTE_ALIGNED */
      errno = EINVAL;
      goto cleanup;
    }

  obj->errnum = FIID_ERR_SUCCESS;
  return (obj);

 cleanup:
  if (obj)
    {
      free (obj->data);
      free (obj->field_data);
      free (obj);
    }

  return (NULL);
}

void
fiid_obj_destroy (fiid_obj_t obj)
{
  if (!(obj && obj->magic == FIID_OBJ_MAGIC))
    return;

  obj->magic = ~FIID_OBJ_MAGIC;
  obj->errnum = FIID_ERR_SUCCESS;
  free (obj->data);
  free (obj->field_data);
  free (obj);
}

fiid_obj_t
fiid_obj_dup (fiid_obj_t src_obj)
{
  fiid_obj_t dest_obj = NULL;

  if (!src_obj || src_obj->magic != FIID_OBJ_MAGIC)
    goto cleanup;

  if (!(dest_obj = malloc (sizeof (struct fiid_obj))))
    {
      src_obj->errnum = FIID_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }
  memset (dest_obj, '\0', sizeof (struct fiid_obj));
  dest_obj->magic = src_obj->magic;
  dest_obj->data_len = src_obj->data_len;
  dest_obj->field_data_len = src_obj->field_data_len;

  if (!(dest_obj->data = malloc (src_obj->data_len)))
    {
      src_obj->errnum = FIID_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }
  memcpy (dest_obj->data, src_obj->data, src_obj->data_len);

  if (!(dest_obj->field_data = malloc (dest_obj->field_data_len * sizeof (struct fiid_field_data))))
    {
      src_obj->errnum = FIID_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }

  memcpy (dest_obj->field_data,
          src_obj->field_data,
          src_obj->field_data_len * sizeof (struct fiid_field_data));

  src_obj->errnum = FIID_ERR_SUCCESS;
  dest_obj->errnum = FIID_ERR_SUCCESS;
  return (dest_obj);

 cleanup:
  if (dest_obj)
    {
      free (dest_obj->data);
      free (dest_obj->field_data);
      free (dest_obj);
    }
  return (NULL);
}

fiid_obj_t
fiid_obj_copy (fiid_obj_t src_obj, fiid_template_t alt_tmpl)
{
  fiid_obj_t dest_obj = NULL;
  int data_len;
  unsigned int field_data_len = 0;
  uint8_t *databuf = NULL;

  if (!src_obj || src_obj->magic != FIID_OBJ_MAGIC)
    goto cleanup;

  if ((data_len = _fiid_template_len_bytes (alt_tmpl, &field_data_len)) < 0)
    goto cleanup;

  if (src_obj->data_len != data_len)
    {
      src_obj->errnum = FIID_ERR_PARAMETERS;
      goto cleanup;
    }

  if (!(dest_obj = fiid_obj_create (alt_tmpl)))
    goto cleanup;

  if (!(databuf = (uint8_t *)malloc (src_obj->data_len)))
    {
      src_obj->errnum = FIID_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }

  if ((data_len = fiid_obj_get_all (src_obj, databuf, src_obj->data_len)) < 0)
    goto cleanup;

  if (fiid_obj_set_all (dest_obj, databuf, data_len) < 0)
    {
      src_obj->errnum = dest_obj->errnum;
      goto cleanup;
    }

  free (databuf);
  return (dest_obj);

 cleanup:
  if (dest_obj)
    fiid_obj_destroy (dest_obj);
  free (databuf);
  return (NULL);
}

int
fiid_obj_valid (fiid_obj_t obj)
{
  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (0);
  return (1);
}

static int
_fiid_obj_packet_valid (fiid_obj_t obj, int makes_packet_sufficient_checks)
{
  unsigned int total_set_bits_counter = 0, max_bits_counter = 0,
    set_bits_counter = 0, optional_bits_counter = 0;
  unsigned int i;

  assert (obj);
  assert (obj->magic == FIID_OBJ_MAGIC);
  assert (!makes_packet_sufficient_checks || obj->makes_packet_sufficient);

  for (i = 0; i < obj->field_data_len; i++)
    {
      unsigned int required_flag = FIID_FIELD_REQUIRED_FLAG (obj->field_data[i].flags);
      unsigned int length_flag = FIID_FIELD_LENGTH_FLAG (obj->field_data[i].flags);
      unsigned int max_field_len = obj->field_data[i].max_field_len;
      unsigned int set_field_len = obj->field_data[i].set_field_len;
      unsigned int makes_packet_sufficient_flag = obj->field_data[i].flags & FIID_FIELD_MAKES_PACKET_SUFFICIENT;

      if (makes_packet_sufficient_checks)
        {
          /* Each field w/ MAKES_PACKET_SUFFICIENT must have the field set. */
          if (makes_packet_sufficient_flag)
            {
              if (!set_field_len)
                return (0);
              
              if (length_flag == FIID_FIELD_LENGTH_FIXED && max_field_len != set_field_len)
                return (0);
            }
        }
      else
        {
          if (required_flag == FIID_FIELD_REQUIRED && !set_field_len)
            {
              obj->errnum = FIID_ERR_REQUIRED_FIELD_MISSING;
              return (0);
            }
          
          if ((length_flag == FIID_FIELD_LENGTH_FIXED && max_field_len != set_field_len)
              && (required_flag == FIID_FIELD_REQUIRED
                  || (required_flag == FIID_FIELD_OPTIONAL && set_field_len)))
            
            {
              obj->errnum = FIID_ERR_FIXED_LENGTH_FIELD_INVALID;
              return (0);
            }
        }

      max_bits_counter += max_field_len;
      total_set_bits_counter += set_field_len;

      if (set_field_len)
        {
          if (optional_bits_counter)
            {
              obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
              return (0);
            }

          if (set_field_len != max_field_len)
            {
              /* If there is an optional or variable length
               * field, it cannot have only partial data.
               */
              if ((set_bits_counter + set_field_len) % 8)
                {
                  obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
                  return (0);
                }
            }

          set_bits_counter += set_field_len;
          if (!(set_bits_counter % 8))
            {
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
              obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
              return (0);
            }

          /* Likewise, optional data should be aligned across
           * bytes
           */
          optional_bits_counter += max_field_len;
          if (optional_bits_counter && !(optional_bits_counter % 8))
            {
              /* an "assert" */
              if (optional_bits_counter != max_bits_counter)
                {
                  obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
                  return (0);
                }

              optional_bits_counter = 0;
              max_bits_counter = 0;
            }
        }
    }

  /* There shouldn't be anything left over */
  if (set_bits_counter)
    {
      obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
      return (0);
    }

  /* And the bits set should align across a byte */
  if (total_set_bits_counter % 8)
    {
      obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
      return (0);
    }

  obj->errnum = FIID_ERR_SUCCESS;
  return (1);
}

int
fiid_obj_packet_valid (fiid_obj_t obj)
{
  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  return _fiid_obj_packet_valid (obj, 0);
}

int
FIID_OBJ_PACKET_VALID (fiid_obj_t obj)
{
  int ret;

  if ((ret = fiid_obj_packet_valid (obj)) < 0)
    return (ret);

  /* errnum set in fiid_obj_packet_valid() */
  if (!ret)
    return (-1);

  return (ret);
}

int
fiid_obj_packet_sufficient (fiid_obj_t obj)
{
  int ret;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  if (!obj->makes_packet_sufficient)
    return _fiid_obj_packet_valid (obj, 0);

  if (!(ret = _fiid_obj_packet_valid (obj, 0)))
    {
      fiid_err_t save_errnum = obj->errnum;

      if (_fiid_obj_packet_valid (obj, 1) == 1)
        return (1);
      
      obj->errnum = save_errnum;
    }
  return (ret);
}

int
FIID_OBJ_PACKET_SUFFICIENT (fiid_obj_t obj)
{
  int ret;

  if ((ret = fiid_obj_packet_sufficient (obj)) < 0)
    return (ret);

  /* errnum set in fiid_obj_packet_sufficient() */
  if (!ret)
    return (-1);

  return (ret);
}

fiid_field_t *
fiid_obj_template (fiid_obj_t obj)
{
  fiid_field_t *tmpl;
  unsigned int i;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (NULL);

  if (!(tmpl = (fiid_field_t *)malloc (sizeof (fiid_field_t) * obj->field_data_len)))
    {
      obj->errnum = FIID_ERR_OUT_OF_MEMORY;
      return (NULL);
    }
  memset (tmpl, '\0', sizeof (fiid_field_t) * obj->field_data_len);

  for (i = 0; i < obj->field_data_len; i++)
    {
      tmpl[i].max_field_len = obj->field_data[i].max_field_len;
      /* not FIID_FIELD_MAX_KEY_LEN + 1, template does not have + 1 */
      memcpy (tmpl[i].key, obj->field_data[i].key, FIID_FIELD_MAX_KEY_LEN);
      tmpl[i].flags = obj->field_data[i].flags;
    }

  obj->errnum = FIID_ERR_SUCCESS;
  return (tmpl);
}

int
fiid_obj_template_compare (fiid_obj_t obj, fiid_template_t tmpl)
{
  unsigned int i;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  if (!tmpl)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      return (-1);
    }

  if (_fiid_template_check_valid_keys (tmpl) < 0)
    {
      obj->errnum = FIID_ERR_TEMPLATE_INVALID;
      return (-1);
    }

  if (_fiid_template_check_valid_flags (tmpl) < 0)
    {
      obj->errnum = FIID_ERR_TEMPLATE_INVALID;
      return (-1);
    }

  for (i = 0; obj->field_data[i].max_field_len; i++)
    {
      if (obj->field_data[i].max_field_len != tmpl[i].max_field_len)
        {
          obj->errnum = FIID_ERR_MAX_FIELD_LEN_MISMATCH;
          return (0);
        }

      if (strcmp (obj->field_data[i].key, tmpl[i].key))
        {
          obj->errnum = FIID_ERR_KEY_FIELD_MISMATCH;
          return (0);
        }

      if (obj->field_data[i].flags != tmpl[i].flags)
        {
          obj->errnum = FIID_ERR_FLAGS_FIELD_MISMATCH;
          return (0);
        }
    }

  if (tmpl[i].max_field_len)
    {
      obj->errnum = FIID_ERR_TEMPLATE_LENGTH_MISMATCH;
      return (0);
    }

  obj->errnum = FIID_ERR_SUCCESS;
  return (1);
}

int
FIID_OBJ_TEMPLATE_COMPARE (fiid_obj_t obj, fiid_template_t tmpl)
{
  int ret;

  if ((ret = fiid_obj_template_compare (obj, tmpl)) < 0)
    return (ret);

  if (!ret)
    {
      obj->errnum = FIID_ERR_NOT_IDENTICAL;
      return (-1);
    }

  return (ret);
}

fiid_err_t
fiid_obj_errnum (fiid_obj_t obj)
{
  if (!obj)
    return (FIID_ERR_OBJ_NULL);
  else if (obj->magic != FIID_OBJ_MAGIC)
    return (FIID_ERR_OBJ_INVALID);
  else
    return (obj->errnum);
}

char *
fiid_obj_errormsg (fiid_obj_t obj)
{
  return (fiid_strerror (fiid_obj_errnum (obj)));
}

static int
_fiid_obj_lookup_field_index (fiid_obj_t obj, const char *field, unsigned int *index)
{
  unsigned int i;

  assert (obj);
  assert (obj->magic == FIID_OBJ_MAGIC);
  assert (field);
  assert (index);

  for (i = 0; obj->field_data[i].max_field_len; i++)
    {
      if (!strcmp (obj->field_data[i].key, field))
        {
          (*index) = i;
          return (0);
        }
    }

  obj->errnum = FIID_ERR_FIELD_NOT_FOUND;
  return (-1);
}

int
fiid_obj_len (fiid_obj_t obj)
{
  unsigned int counter = 0;
  unsigned int i;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  /* integer overflow conditions checked during object creation */
  for (i = 0; obj->field_data[i].max_field_len; i++)
    counter += obj->field_data[i].set_field_len;

  obj->errnum = FIID_ERR_SUCCESS;
  return (counter);
}

int
fiid_obj_len_bytes (fiid_obj_t obj)
{
  int len;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  if ((len = fiid_obj_len (obj)) < 0)
    return (-1);

  if (len % 8)
    {
      obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
      return (-1);
    }

  obj->errnum = FIID_ERR_SUCCESS;
  return (BITS_ROUND_BYTES (len));
}

int
fiid_obj_field_len (fiid_obj_t obj, const char *field)
{
  unsigned int key_index;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  if (!field)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      return (-1);
    }

  if (_fiid_obj_lookup_field_index (obj, field, &key_index) < 0)
    return (-1);

  obj->errnum = FIID_ERR_SUCCESS;
  return (obj->field_data[key_index].set_field_len);
}

int
fiid_obj_field_len_bytes (fiid_obj_t obj, const char *field)
{
  int len;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  if (!field)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      return (-1);
    }

  if ((len = fiid_obj_field_len (obj, field)) < 0)
    return (-1);

  if (len % 8)
    {
      obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
      return (-1);
    }

  obj->errnum = FIID_ERR_SUCCESS;
  return (BITS_ROUND_BYTES (len));
}

int
fiid_obj_block_len (fiid_obj_t obj, const char *field_start, const char *field_end)
{
  unsigned int key_index_start, key_index_end;
  unsigned int counter = 0;
  unsigned int i;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  if (!field_start || !field_end)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      return (-1);
    }

  if (_fiid_obj_lookup_field_index (obj, field_start, &key_index_start) < 0)
    return (-1);

  if (_fiid_obj_lookup_field_index (obj, field_end, &key_index_end) < 0)
    return (-1);

  if (key_index_start > key_index_end)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      return (-1);
    }

  /* integer overflow conditions checked during object creation */
  for (i = key_index_start; i <= key_index_end; i++)
    counter += obj->field_data[i].set_field_len;

  obj->errnum = FIID_ERR_SUCCESS;
  return (counter);
}

int
fiid_obj_block_len_bytes (fiid_obj_t obj, const char *field_start, const char *field_end)
{
  int len;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  if (!field_start || !field_end)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      return (-1);
    }

  if ((len = fiid_obj_block_len (obj, field_start, field_end)) < 0)
    return (-1);

  if (len % 8)
    {
      obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
      return (-1);
    }

  obj->errnum = FIID_ERR_SUCCESS;
  return (BITS_ROUND_BYTES (len));
}

int
fiid_obj_clear (fiid_obj_t obj)
{
  unsigned int i;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  secure_memset (obj->data, '\0', obj->data_len);
  for (i =0; i < obj->field_data_len; i++)
    obj->field_data[i].set_field_len = 0;

  obj->errnum = FIID_ERR_SUCCESS;
  return (0);
}

int
fiid_obj_clear_field (fiid_obj_t obj, const char *field)
{
  unsigned int key_index;
  int bits_len;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  if (!field)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      return (-1);
    }

  if (_fiid_obj_lookup_field_index (obj, field, &key_index) < 0)
    return (-1);

  if (!obj->field_data[key_index].set_field_len)
    return (0);

  if ((bits_len = _fiid_obj_field_len (obj, field)) < 0)
    return (-1);

  if (bits_len <= 64)
    {
      uint64_t val = 0;

      if (fiid_obj_set (obj, field, val) < 0)
        return (-1);
    }
  else
    {
      int field_start;
      unsigned int field_offset, bytes_len;

      /* achu: We assume the field must start on a byte boundary and end
       * on a byte boundary.
       */

      if (bits_len % 8)
        {
          obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
          return (-1);
        }

      bytes_len = BITS_ROUND_BYTES (bits_len);

      if ((field_start = _fiid_obj_field_start (obj, field)) < 0)
        return (-1);

      if (field_start % 8)
        {
          obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
          return (-1);
        }

      field_offset = BITS_ROUND_BYTES (field_start);
      secure_memset (obj->data + field_offset, '\0', bytes_len);
    }

  obj->field_data[key_index].set_field_len = 0;
  obj->errnum = FIID_ERR_SUCCESS;
  return (0);
}

int
fiid_obj_field_lookup (fiid_obj_t obj, const char *field)
{
  unsigned int start = 0;
  unsigned int end = 0; /* excluded always */

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  if (!field)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      return (-1);
    }

  if (_fiid_obj_field_start_end (obj, field, &start, &end) != -1)
    {
      obj->errnum = FIID_ERR_SUCCESS;
      return (1);
    }

  obj->errnum = FIID_ERR_FIELD_NOT_FOUND;
  return (0);
}

int
FIID_OBJ_FIELD_LOOKUP (fiid_obj_t obj, const char *field)
{
  int ret;

  if ((ret = fiid_obj_field_lookup (obj, field)) < 0)
    return (ret);

  if (!ret)
    {
      obj->errnum = FIID_ERR_FIELD_NOT_FOUND;
      return (-1);
    }

  return (ret);
}

int
fiid_obj_set (fiid_obj_t obj,
              const char *field,
              uint64_t val)
{
  unsigned int start_bit_pos = 0;
  unsigned int end_bit_pos = 0; /* excluded always */
  int byte_pos = 0;
  int start_bit_in_byte_pos = 0;
  int end_bit_in_byte_pos = 0;
  int field_len = 0;
  int bytes_used = 0;
  unsigned int key_index;
  uint64_t merged_val = 0;
  uint8_t *temp_data = NULL;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    goto cleanup;

  if (!field)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      goto cleanup;
    }

  if (_fiid_obj_lookup_field_index (obj, field, &key_index) < 0)
    return (-1);

  if ((field_len = _fiid_obj_field_start_end (obj,
                                              field,
                                              &start_bit_pos,
                                              &end_bit_pos)) < 0)
    return (-1);

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
      unsigned int i;

      if (!(temp_data = malloc (obj->data_len)))
        {
          obj->errnum = FIID_ERR_OUT_OF_MEMORY;
          goto cleanup;
        }
      memcpy (temp_data, obj->data, obj->data_len);

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
            {
              obj->errnum = FIID_ERR_INTERNAL_ERROR;
              goto cleanup;
            }

          if (bits_merge (temp_data[byte_pos + i],
                          start_bit_in_byte_pos,
                          end_bit_in_byte_pos,
                          extracted_val,
                          &merged_val) < 0)
            {
              obj->errnum = FIID_ERR_INTERNAL_ERROR;
              goto cleanup;
            }

          temp_data[byte_pos + i] = merged_val;
          start_bit_in_byte_pos = 0;
          start_val_pos = end_val_pos;
        }

      memcpy (obj->data, temp_data, obj->data_len);
      obj->field_data[key_index].set_field_len = field_len;
    }
  else
    {
      if (bits_merge (obj->data[byte_pos],
                      start_bit_in_byte_pos,
                      end_bit_in_byte_pos,
                      val,
                      &merged_val) < 0)
        {
          obj->errnum = FIID_ERR_INTERNAL_ERROR;
          goto cleanup;
        }
      obj->data[byte_pos] = merged_val;
      obj->field_data[key_index].set_field_len = field_len;
    }

  free (temp_data);
  obj->errnum = FIID_ERR_SUCCESS;
  return (0);

 cleanup:
  free (temp_data);
  return (-1);
}

int
fiid_obj_get (fiid_obj_t obj,
              const char *field,
              uint64_t *val)
{
  unsigned int start_bit_pos = 0;
  unsigned int end_bit_pos = 0; /* excluded always */
  int byte_pos = 0;
  int start_bit_in_byte_pos = 0;
  int end_bit_in_byte_pos = 0;
  int field_len = 0;
  int bytes_used = 0;
  unsigned int key_index;
  uint64_t merged_val = 0;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  if (!field || !val)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      return (-1);
    }

  if (_fiid_obj_lookup_field_index (obj, field, &key_index) < 0)
    return (-1);

  if (!obj->field_data[key_index].set_field_len)
    {
      obj->errnum = FIID_ERR_SUCCESS;
      return (0);
    }

  if ((field_len = _fiid_obj_field_start_end (obj,
                                              field,
                                              &start_bit_pos,
                                              &end_bit_pos)) < 0)
    return (-1);

  if (field_len > 64)
    field_len = 64;

  if (field_len > obj->field_data[key_index].set_field_len)
    field_len = obj->field_data[key_index].set_field_len;

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
      unsigned int i;

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
            {
              obj->errnum = FIID_ERR_INTERNAL_ERROR;
              return (-1);
            }

          if (bits_merge (final_val,
                          start_val_pos,
                          end_val_pos,
                          extracted_val,
                          &merged_val) < 0)
            {
              obj->errnum = FIID_ERR_INTERNAL_ERROR;
              return (-1);
            }

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
        {
          obj->errnum = FIID_ERR_INTERNAL_ERROR;
          return (-1);
        }

      *val = 0;
      *val = merged_val;
    }

  obj->errnum = FIID_ERR_SUCCESS;
  return (1);
}

int
FIID_OBJ_GET (fiid_obj_t obj,
              const char *field,
              uint64_t *val)
{
  uint64_t lval;
  int ret;

  if ((ret = fiid_obj_get (obj, field, &lval)) < 0)
    return (ret);

  if (!ret)
    {
      obj->errnum = FIID_ERR_DATA_NOT_AVAILABLE;
      return (-1);
    }

  *val = lval;
  return (ret);
}

int
fiid_obj_set_data (fiid_obj_t obj,
                   const char *field,
                   const void *data,
                   unsigned int data_len)
{
  unsigned int field_offset, bytes_len, key_index;
  int bits_len, field_start;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  if (!field || !data)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      return (-1);
    }

  if (_fiid_obj_lookup_field_index (obj, field, &key_index) < 0)
    return (-1);

  /* achu: We assume the field must start on a byte boundary and end
   * on a byte boundary.
   */

  if ((field_start = _fiid_obj_field_start (obj, field)) < 0)
    return (-1);

  if (field_start % 8)
    {
      obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
      return (-1);
    }

  if ((bits_len = _fiid_obj_field_len (obj, field)) < 0)
    return (-1);

  if (bits_len % 8)
    {
      obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
      return (-1);
    }

  bytes_len = BITS_ROUND_BYTES (bits_len);
  if (data_len > bytes_len)
    data_len = bytes_len;

  field_offset = BITS_ROUND_BYTES (field_start);
  memcpy ((obj->data + field_offset), data, data_len);
  obj->field_data[key_index].set_field_len = (data_len * 8);

  obj->errnum = FIID_ERR_SUCCESS;
  return (data_len);
}

int
fiid_obj_get_data (fiid_obj_t obj,
                   const char *field,
                   void *data,
                   unsigned int data_len)
{
  unsigned int field_offset, bytes_len, key_index;
  int bits_len, field_start;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  if (!field || !data)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      return (-1);
    }

  if (_fiid_obj_lookup_field_index (obj, field, &key_index) < 0)
    return (-1);

  if (!obj->field_data[key_index].set_field_len)
    return (0);

  /* achu: We assume the field must start on a byte boundary and end
   * on a byte boundary.
   */

  if ((field_start = _fiid_obj_field_start (obj, field)) < 0)
    return (-1);

  if (field_start % 8)
    {
      obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
      return (-1);
    }

  if ((bits_len = _fiid_obj_field_len (obj, field)) < 0)
    return (-1);

  if (obj->field_data[key_index].set_field_len < bits_len)
    bits_len = obj->field_data[key_index].set_field_len;

  if (bits_len % 8)
    {
      obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
      return (-1);
    }

  bytes_len = BITS_ROUND_BYTES (bits_len);

  if (bytes_len > data_len)
    {
      obj->errnum = FIID_ERR_OVERFLOW;
      return (-1);
    }

  field_offset = BITS_ROUND_BYTES (field_start);

  memset (data, '\0', data_len);
  memcpy (data, (obj->data + field_offset), bytes_len);

  obj->errnum = FIID_ERR_SUCCESS;
  return (bytes_len);
}

int
fiid_obj_set_all (fiid_obj_t obj,
                  const void *data,
                  unsigned int data_len)
{
  unsigned int bits_counter, data_bits_len;
  unsigned int key_index_end;
  unsigned int i;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  if (!data)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      return (-1);
    }

  if (data_len > obj->data_len)
    data_len = obj->data_len;

  /* achu: Find index of last field */
  data_bits_len = data_len * 8;
  if (data_len < obj->data_len)
    {
      /* integer overflow conditions checked during object creation */
      bits_counter = 0;
      for (i = 0; obj->field_data[i].max_field_len; i++)
        {
          bits_counter += obj->field_data[i].max_field_len;
          if (bits_counter >= data_bits_len)
            {
              /* achu: We assume the data must end on a byte boundary. */
              if (bits_counter % 8)
                {
                  obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
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

  /* integer overflow conditions checked during object creation */
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

  obj->errnum = FIID_ERR_SUCCESS;
  return (data_len);
}

int
fiid_obj_get_all (fiid_obj_t obj,
                  void *data,
                  unsigned int data_len)
{
  unsigned int bytes_len;
  int bits_len;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  if (!data)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      return (-1);
    }

  if ((bits_len = fiid_obj_len (obj)) < 0)
    return (-1);

  if (bits_len == (obj->data_len * 8))
    bytes_len = obj->data_len;
  else
    bytes_len = BITS_ROUND_BYTES (bits_len);

  if (data_len < bytes_len)
    {
      obj->errnum = FIID_ERR_OVERFLOW;
      return (-1);
    }

  memset (data, '\0', data_len);

  if (bytes_len == obj->data_len)
    memcpy (data, obj->data, bytes_len);
  else
    {
      unsigned int bytes_written = 0, max_bits_counter = 0, set_bits_counter = 0,
        optional_bits_counter = 0, data_index = 0, obj_data_index = 0;
      unsigned int i;

      /* integer overflow conditions checked during object creation */
      for (i = 0; i < obj->field_data_len; i++)
        {
          unsigned int max_field_len = obj->field_data[i].max_field_len;
          unsigned int set_field_len = obj->field_data[i].set_field_len;

          max_bits_counter += max_field_len;

          if (set_field_len)
            {
              if (optional_bits_counter)
                {
                  obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
                  goto cleanup;
                }

              if (set_field_len != max_field_len)
                {
                  /* If there is an optional or variable length
                   * field, it cannot have only partial data.
                   */
                  if ((set_bits_counter + set_field_len) % 8)
                    {
                      obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
                      goto cleanup;
                    }
                }

              set_bits_counter += set_field_len;
              if (!(set_bits_counter % 8))
                {
                  unsigned int max_bytes_count = BITS_ROUND_BYTES (max_bits_counter);
                  unsigned int set_bytes_count = BITS_ROUND_BYTES (set_bits_counter);

                  memcpy (data + data_index,
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
                  obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
                  goto cleanup;
                }

              /* Likewise, optional data should be aligned across
               * bytes
               */
              optional_bits_counter += max_field_len;
              if (optional_bits_counter && !(optional_bits_counter % 8))
                {
                  /* an "assert" */
                  if (optional_bits_counter != max_bits_counter)
                    {
                      obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
                      goto cleanup;
                    }

                  obj_data_index += BITS_ROUND_BYTES (optional_bits_counter);
                  optional_bits_counter = 0;
                  max_bits_counter = 0;
                }
            }
        }

      /* There shouldn't be anything left over */
      if (set_bits_counter)
        {
          obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
          goto cleanup;
        }

      if (bytes_written != bytes_len)
        {
          obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
          goto cleanup;
        }
    }

  obj->errnum = FIID_ERR_SUCCESS;
  return (bytes_len);

 cleanup:
  if (data)
    memset (data, '\0', data_len);
  return (-1);
}

static int
_fiid_obj_max_block_len (fiid_obj_t obj,
                         const char *field_start,
                         const char *field_end)
{
  int end;
  int start;

  assert (obj);
  assert (obj->magic == FIID_OBJ_MAGIC);
  assert (field_start);
  assert (field_end);

  if ((start = _fiid_obj_field_start (obj, field_start)) < 0)
    return (-1);

  if ((end = _fiid_obj_field_end (obj, field_end)) < 0)
    return (-1);

  if (start > end)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      return (-1);
    }

  return (end - start);
}

int
fiid_obj_set_block (fiid_obj_t obj,
                    const char *field_start,
                    const char *field_end,
                    const void *data,
                    unsigned int data_len)
{
  int block_bits_start, block_bits_len;
  unsigned int key_index_start, key_index_end;
  unsigned int block_bytes_len, bits_counter, data_bits_len, field_offset;
  unsigned int i;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  if (!field_start || !field_end || !data)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      return (-1);
    }

  if (_fiid_obj_lookup_field_index (obj, field_start, &key_index_start) < 0)
    return (-1);

  if (_fiid_obj_lookup_field_index (obj, field_end, &key_index_end) < 0)
    return (-1);

  if (key_index_start > key_index_end)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      return (-1);
    }

  /* achu: We assume the field must start on a byte boundary and end
   * on a byte boundary.
   */

  if ((block_bits_start = _fiid_obj_field_start (obj, field_start)) < 0)
    return (-1);

  if (block_bits_start % 8)
    {
      obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
      return (-1);
    }

  if ((block_bits_len = _fiid_obj_max_block_len (obj,
                                                 field_start,
                                                 field_end)) < 0)
    return (-1);

  if (block_bits_len % 8)
    {
      obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
      return (-1);
    }

  block_bytes_len = BITS_ROUND_BYTES (block_bits_len);

  if (data_len > block_bytes_len)
    data_len = block_bytes_len;

  /* achu: Potentially adjust index of last field */
  /* integer overflow conditions checked during object creation */
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
              if (bits_counter % 8)
                {
                  obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
                  return (-1);
                }
              else
                break;
            }

        }
      key_index_end = i;
    }

  field_offset = BITS_ROUND_BYTES (block_bits_start);
  memcpy ((obj->data + field_offset), data, data_len);

  /* integer overflow conditions checked during object creation */
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

  obj->errnum = FIID_ERR_SUCCESS;
  return (data_len);
}

int
fiid_obj_get_block (fiid_obj_t obj,
                    const char *field_start,
                    const char *field_end,
                    void *data,
                    unsigned int data_len)
{
  int block_bits_start, block_bits_max_len, block_bits_set_len;
  unsigned int block_bytes_max_len, block_bytes_set_len, field_offset;
  unsigned int key_index_start, key_index_end;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    return (-1);

  if (!field_start || !field_end || !data)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      return (-1);
    }

  if (_fiid_obj_lookup_field_index (obj, field_start, &key_index_start) < 0)
    return (-1);

  if (_fiid_obj_lookup_field_index (obj, field_end, &key_index_end) < 0)
    return (-1);

  if (key_index_start > key_index_end)
    {
      obj->errnum = FIID_ERR_PARAMETERS;
      return (-1);
    }

  /* achu: We assume the field must start on a byte boundary and ends
   * on a byte boundary.
   */

  if ((block_bits_start = _fiid_obj_field_start (obj, field_start)) < 0)
    return (-1);

  if (block_bits_start % 8)
    {
      obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
      return (-1);
    }

  if ((block_bits_max_len = _fiid_obj_max_block_len (obj, field_start, field_end)) < 0)
    return (-1);

  if (block_bits_max_len % 8)
    {
      obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
      return (-1);
    }

  if ((block_bits_set_len = fiid_obj_block_len (obj, field_start, field_end)) < 0)
    return (-1);

  if (block_bits_set_len % 8)
    {
      obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
      return (-1);
    }

  block_bytes_max_len = BITS_ROUND_BYTES (block_bits_max_len);
  block_bytes_set_len = BITS_ROUND_BYTES (block_bits_set_len);

  if (data_len < block_bytes_set_len)
    {
      obj->errnum = FIID_ERR_OVERFLOW;
      return (-1);
    }

  field_offset = BITS_ROUND_BYTES (block_bits_start);

  memset (data, '\0', data_len);

  if (block_bytes_set_len == block_bytes_max_len)
    memcpy (data, (obj->data + field_offset), block_bytes_set_len);
  else
    {
      unsigned int bytes_written = 0, max_bits_counter = 0, set_bits_counter = 0,
        optional_bits_counter = 0, data_index = 0, obj_data_index = field_offset;
      unsigned int i;

      /* integer overflow conditions checked during object creation */
      for (i = key_index_start; i <= key_index_end; i++)
        {
          unsigned int max_field_len = obj->field_data[i].max_field_len;
          unsigned int set_field_len = obj->field_data[i].set_field_len;

          max_bits_counter += max_field_len;

          if (set_field_len)
            {
              if (optional_bits_counter)
                {
                  obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
                  goto cleanup;
                }

              if (set_field_len != max_field_len)
                {
                  /* If there is an optional or variable length
                   * field, it cannot have only partial data.
                   */
                  if ((set_bits_counter + set_field_len) % 8)
                    {
                      obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
                      goto cleanup;
                    }
                }

              set_bits_counter += set_field_len;
              if (!(set_bits_counter % 8))
                {
                  unsigned int max_bytes_count = BITS_ROUND_BYTES (max_bits_counter);
                  unsigned int set_bytes_count = BITS_ROUND_BYTES (set_bits_counter);

                  memcpy (data + data_index,
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
                  obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
                  goto cleanup;
                }

              /* Likewise, optional data should be aligned across
               * bytes
               */
              optional_bits_counter += max_field_len;
              if (optional_bits_counter && !(optional_bits_counter % 8))
                {
                  /* an "assert" */
                  if (optional_bits_counter != max_bits_counter)
                    {
                      obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
                      goto cleanup;
                    }

                  obj_data_index += BITS_ROUND_BYTES (optional_bits_counter);
                  optional_bits_counter = 0;
                  max_bits_counter = 0;
                }
            }

        }

      /* There shouldn't be anything left over */
      if (set_bits_counter)
        {
          obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
          goto cleanup;
        }

      if (bytes_written != block_bytes_set_len)
        {
          obj->errnum = FIID_ERR_DATA_NOT_BYTE_ALIGNED;
          goto cleanup;
        }
    }

  obj->errnum = FIID_ERR_SUCCESS;
  return (block_bytes_set_len);

 cleanup:
  if (data)
    memset (data, '\0', data_len);
  return (-1);
}

fiid_iterator_t
fiid_iterator_create (fiid_obj_t obj)
{
  fiid_iterator_t iter = NULL;

  if (!obj || obj->magic != FIID_OBJ_MAGIC)
    goto cleanup;

  if (!(iter = (fiid_iterator_t)malloc (sizeof (struct fiid_iterator))))
    {
      obj->errnum = FIID_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }
  memset (iter, '\0', sizeof (struct fiid_iterator));
  iter->magic = FIID_ITERATOR_MAGIC;
  iter->current_index = 0;

  /* The - 1 below is because field_data_len is length of the array.
   * The iterator is concerned about array indexes.
   */
  iter->last_index = obj->field_data_len - 1;

  if (!(iter->obj = fiid_obj_dup (obj)))
    goto cleanup;

  obj->errnum = FIID_ERR_SUCCESS;
  return (iter);

 cleanup:
  if (iter)
    {
      if (iter->obj)
        fiid_obj_destroy (iter->obj);
      free (iter);
    }

  return (NULL);
}

void
fiid_iterator_destroy (fiid_iterator_t iter)
{
  if (!(iter && iter->magic == FIID_ITERATOR_MAGIC))
    return;

  iter->magic = ~FIID_ITERATOR_MAGIC;
  iter->errnum = FIID_ERR_SUCCESS;
  fiid_obj_destroy (iter->obj);
  free (iter);
}

fiid_err_t
fiid_iterator_errnum (fiid_iterator_t iter)
{
  if (!iter)
    return (FIID_ERR_ITERATOR_NULL);
  else if (iter->magic != FIID_ITERATOR_MAGIC)
    return (FIID_ERR_ITERATOR_INVALID);
  else
    return (iter->errnum);
}

char *
fiid_iterator_errormsg (fiid_iterator_t iter)
{
  return (fiid_strerror (fiid_iterator_errnum (iter)));
}

int
fiid_iterator_reset (fiid_iterator_t iter)
{
  if (!(iter && iter->magic == FIID_ITERATOR_MAGIC))
    return (-1);

  iter->current_index = 0;
  iter->errnum = FIID_ERR_SUCCESS;
  return (0);
}

int
fiid_iterator_next (fiid_iterator_t iter)
{
  if (!(iter && iter->magic == FIID_ITERATOR_MAGIC))
    return (-1);

  if (iter->current_index != iter->last_index)
    iter->current_index++;

  iter->errnum = FIID_ERR_SUCCESS;
  return (0);
}

int
fiid_iterator_end (fiid_iterator_t iter)
{
  if (!(iter && iter->magic == FIID_ITERATOR_MAGIC))
    return (-1);

  iter->errnum = FIID_ERR_SUCCESS;
  return ((iter->current_index == iter->last_index) ? 1 : 0);
}

int
fiid_iterator_field_len (fiid_iterator_t iter)
{
  if (!(iter && iter->magic == FIID_ITERATOR_MAGIC))
    return (-1);

  iter->errnum = FIID_ERR_SUCCESS;
  /* integer overflow conditions checked during object creation */
  return (iter->obj->field_data[iter->current_index].set_field_len);
}

char *
fiid_iterator_key (fiid_iterator_t iter)
{
  if (!(iter && iter->magic == FIID_ITERATOR_MAGIC))
    return (NULL);

  iter->errnum = FIID_ERR_SUCCESS;
  return (iter->obj->field_data[iter->current_index].key);
}

int
fiid_iterator_get (fiid_iterator_t iter, uint64_t *val)
{
  char *key;
  int rv;

  if (!(iter && iter->magic == FIID_ITERATOR_MAGIC))
    return (-1);

  key = iter->obj->field_data[iter->current_index].key;
  rv = fiid_obj_get (iter->obj, key, val);
  iter->errnum = (iter->obj->errnum);
  return (rv);
}

int
fiid_iterator_get_data (fiid_iterator_t iter,
                        void *data,
                        unsigned int data_len)
{
  char *key;
  int rv;

  if (!(iter && iter->magic == FIID_ITERATOR_MAGIC))
    return (-1);

  key = iter->obj->field_data[iter->current_index].key;
  rv = fiid_obj_get_data (iter->obj, key, data, data_len);
  iter->errnum = (iter->obj->errnum);
  return (rv);
}
