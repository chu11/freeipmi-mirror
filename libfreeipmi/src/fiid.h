/* 
   fiid.h - FreeIPMI Interface Definition

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

#ifndef _FIID_H
#define	_FIID_H	1

#ifdef __cplusplus
extern "C" {
#endif

#define __LFI_FIID_OBJ_SET(bytes, field, val)           \
do {                                                    \
    if (fiid_obj_set (bytes, field, val) == -1)         \
      return (-1);                                      \
} while (0)

#define __FI_FIID_OBJ_SET(bytes, field, val)            \
do {                                                    \
    if (fiid_obj_set (bytes, field, val) == -1)         \
    {                                                   \
      err (1, "fiid_obj_set (%p, \"%s\", %X) error",    \
	   bytes, field, val);                          \
    }                                                   \
} while (0)

#if defined (FREEIPMI_LIBRARY)                                
#define FIID_OBJ_SET(bytes, field, val)   \
  __LFI_FIID_OBJ_SET (bytes, field, val)
#else
#define FIID_OBJ_SET(bytes, field, val)   \
  __FI_FIID_OBJ_SET (bytes, field, val)
#endif

#define __LFI_FIID_OBJ_SET_DATA(bytes, field, val, val_len)   \
do {                                                          \
    if (fiid_obj_set_data (bytes, field, val, val_len) == -1) \
      return (-1);                                            \
} while (0)

#define __FI_FIID_OBJ_SET_DATA(bytes, field, val, val_len)    \
do {                                                          \
    if (fiid_obj_set (bytes, field, val, val_len) == -1)      \
    {                                                         \
      err (1, "fiid_obj_set_data (%p, \"%s\", %p, %u) error", \
	   bytes, field, val, val_len);                       \
    }                                                         \
} while (0)

#if defined (FREEIPMI_LIBRARY)                                
#define FIID_OBJ_SET_DATA(bytes, field, val, val_len)    \
  __LFI_FIID_OBJ_SET_DATA (bytes, field, val, val_len)
#else
#define FIID_OBJ_SET_DATA(bytes, field, val, val_len)    \
  __FI_FIID_OBJ_SET_DATA (bytes, field, val, val_len)
#endif

#define __LFI_FIID_OBJ_GET(bytes, field, val)            \
do {                                                     \
    uint64_t _val = 0, *_val_ptr;                        \
    _val_ptr = val;                                      \
    if (fiid_obj_get (bytes, field, &_val) == -1)        \
      return (-1);                                       \
    *_val_ptr = _val;                                    \
} while (0)

#define __FI_FIID_OBJ_GET(bytes, field, val)             \
do {                                                     \
    uint64_t _val = 0, *_val_ptr;                        \
    _val_ptr = val;                                      \
    if (fiid_obj_get (bytes, field, &_val) == -1)        \
    {                                                    \
      err (1, "fiid_obj_get (%p, %p, \"%s\", %p) error", \
	   bytes, field, val);                           \
    }                                                    \
    *_val_ptr = _val;                                    \
} while (0)

#if defined (FREEIPMI_LIBRARY)                                
#define FIID_OBJ_GET(bytes, field, val)  \
  __LFI_FIID_OBJ_GET (bytes, field, val)
#else
#define FIID_OBJ_GET(bytes, field, val)  \
  __FI_FIID_OBJ_GET (bytes, field, val)
#endif

#define __LFI_FIID_OBJ_GET_DATA(bytes, field, val, val_len)    \
do {                                                           \
    if (fiid_obj_get_data (bytes, field, val, val_len) == -1)  \
      return (-1);                                             \
} while (0)

#define __FI_FIID_OBJ_GET_DATA(bytes, field, val, val_len)     \
do {                                                           \
    if (fiid_obj_get_data (bytes, field, &val, val_len) == -1) \
    {                                                          \
      err (1, "fiid_obj_get_data (%p, \"%s\", %p) error",      \
	   bytes, field, val);                                 \
    }                                                          \
} while (0)

#if defined (FREEIPMI_LIBRARY)                                
#define FIID_OBJ_GET_DATA(bytes, tmpl, field, val, val_len)  \
  __LFI_FIID_OBJ_GET_DATA (bytes, tmpl, field, val, val_len)
#else
#define FIID_OBJ_GET_DATA(bytes, tmpl, field, val, val_len)  \
  __FI_FIID_OBJ_GET_DATA (bytes, tmpl, field, val, val_len)
#endif

#define fiid_template_make(arg...) __fiid_template_make (1, arg, 0)

#define FIID_FIELD_MAX 256

typedef struct fiid_field
{
  uint32_t max_field_len;
  char key[FIID_FIELD_MAX];
} fiid_field_t;

typedef fiid_field_t fiid_template_t[];

typedef struct fiid_obj *fiid_obj_t;

typedef struct fiid_iterator *fiid_iterator_t;

int8_t fiid_template_field_lookup (fiid_template_t tmpl, uint8_t *field);

fiid_obj_t fiid_obj_create (fiid_template_t tmpl);
int8_t fiid_obj_destroy (fiid_obj_t obj);
fiid_obj_t fiid_obj_dup (fiid_obj_t src_obj);
int8_t fiid_obj_verify(fiid_obj_t obj);

int8_t fiid_obj_clear (fiid_obj_t obj);
int8_t fiid_obj_clear_field (fiid_obj_t obj, uint8_t *field);
int8_t fiid_obj_field_lookup (fiid_obj_t obj, uint8_t *field);
int8_t fiid_obj_set (fiid_obj_t obj, uint8_t *field, uint64_t val);
int8_t fiid_obj_get (fiid_obj_t obj, uint8_t *field, uint64_t *val);
int8_t fiid_obj_set_data (fiid_obj_t obj, uint8_t *field, uint8_t *data, uint32_t data_len);
int8_t fiid_obj_get_data (fiid_obj_t obj, uint8_t *field, uint8_t *data, uint32_t data_len);
int8_t fiid_obj_set_block (fiid_obj_t obj, uint8_t *field_start, uint8_t *field_end, uint8_t *data, uint32_t data_len);
int8_t fiid_obj_get_block (fiid_obj_t obj, uint8_t *field_start, uint8_t *field_end, uint8_t *data, uint32_t data_len);

fiid_iterator_t fiid_iterator_create(fiid_obj_t obj);
int8_t fiid_iterator_destroy(fiid_iterator_t iter);
int8_t fiid_iterator_reset(fiid_iterator_t iter);
int8_t fiid_iterator_next(fiid_iterator_t iter);
int8_t fiid_iterator_end(fiid_iterator_t iter);
int32_t fiid_iterator_max_field_len(fiid_iterator_t iter);
uint8_t *fiid_iterator_key(fiid_iterator_t iter);
int32_t fiid_iterator_get(fiid_iterator_t iter, uint64_t *val);
int32_t fiid_iterator_get_data(fiid_iterator_t iter, uint8_t *data, uint32_t data_len);

fiid_field_t *__fiid_template_make (uint8_t dummy, ...);
void fiid_template_free (fiid_field_t *tmpl_dynamic);

#ifdef __cplusplus
}
#endif

#endif /* fiid.h */


