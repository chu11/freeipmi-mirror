/* 
   fiid.h - FreeIPMI Interface Definition

   Copyright (C) 2003 FreeIPMI Core Team

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

#define FIID_FIELD_MAX 256

#define FIID_OBJ_ALLOCA(obj, tmpl)                            \
    obj = alloca (fiid_obj_len_bytes (tmpl));		      \
    ERR (obj);						      \
    memset (obj, 0, fiid_obj_len_bytes (tmpl))                \

#define FIID_OBJ_ALLOC(obj, tmpl)                             \
do {                                                          \
    obj = fiid_obj_alloc (fiid_obj_len_bytes (tmpl));	      \
    ERR (obj);						      \
} while (0)	

#define __LFI_FIID_OBJ_SET(bytes, tmpl, field, val)           \
do {                                                          \
    if (fiid_obj_set (bytes, tmpl, field, val) == -1)         \
      return (-1);                                            \
} while (0)

#define __FI_FIID_OBJ_SET(bytes, tmpl, field, val)            \
do {                                                          \
    if (fiid_obj_set (bytes, tmpl, field, val) == -1)         \
    {                                                         \
      err (1, "fiid_obj_set (%p, %p, \"%s\", %X) error",      \
	   bytes, tmpl, field, val);                          \
    }                                                         \
} while (0)

#if defined (FREEIPMI_LIBRARY)                                
#define FIID_OBJ_SET(bytes, tmpl, field, val)   \
  __LFI_FIID_OBJ_SET (bytes, tmpl, field, val)
#else
#define FIID_OBJ_SET(bytes, tmpl, field, val)   \
  __FI_FIID_OBJ_SET (bytes, tmpl, field, val)
#endif

#define __LFI_FIID_OBJ_GET(bytes, tmpl, field, val)           \
do {                                                          \
    u_int64_t _val = 0, *_val_ptr;                            \
    _val_ptr = val;                                           \
    if (fiid_obj_get (bytes, tmpl, field, &_val) == -1)       \
      return (-1);                                            \
    *_val_ptr = _val;                                         \
} while (0)

#define __FI_FIID_OBJ_GET(bytes, tmpl, field, val)            \
do {                                                          \
    u_int64_t _val = 0, *_val_ptr;                            \
    _val_ptr = val;                                           \
    if (fiid_obj_get (bytes, tmpl, field, &_val) == -1)       \
    {                                                         \
      err (1, "fiid_obj_get (%p, %p, \"%s\", %p) error",      \
	   bytes, tmpl, field, val);                          \
    }                                                         \
    *_val_ptr = _val;                                         \
} while (0)

#if defined (FREEIPMI_LIBRARY)                                
#define FIID_OBJ_GET(bytes, tmpl, field, val)  \
  __LFI_FIID_OBJ_GET (bytes, tmpl, field, val)
#else
#define FIID_OBJ_GET(bytes, tmpl, field, val)  \
  __FI_FIID_OBJ_GET (bytes, tmpl, field, val)
#endif

#define fiid_template_make(arg...) __fiid_template_make (1, arg, 0)

typedef struct fiid_field
{
  u_int32_t len;
  char key[FIID_FIELD_MAX];
} fiid_field_t;

/* typedef fiid_field_t fiid_template_t[]; */
typedef fiid_field_t const fiid_template_t[];
typedef fiid_field_t const fiid_tmpl_t[];
typedef u_int8_t *fiid_obj_t;
/* FIID Template for testing. 
fiid_template_t tmpl_test =
  {
    {2,   "netfn.lun"},
    {6,   "netfn.fn"},
    {128, "user_name"},
    {4,   "reserved1"},
    {17,  "manfid"},
    {2,   "reserved2"},
    {1,   "dev_available"}
  };
*/

int32_t fiid_obj_len (fiid_template_t tmpl);
int32_t fiid_obj_len_bytes (fiid_template_t tmpl);
int32_t fiid_obj_field_start_end (fiid_template_t tmpl, u_int8_t *field, u_int32_t *start, u_int32_t *end);
int8_t fiid_obj_field_lookup (fiid_template_t tmpl, u_int8_t *field);
int32_t fiid_obj_field_start (fiid_template_t tmpl, u_int8_t *field);
int32_t fiid_obj_field_start_bytes (fiid_template_t tmpl, u_int8_t *field);
int32_t fiid_obj_field_end (fiid_template_t tmpl, u_int8_t *field);
int32_t fiid_obj_field_end_bytes (fiid_template_t tmpl, u_int8_t *field);
int32_t fiid_obj_field_len (fiid_template_t tmpl, u_int8_t *field);
int32_t fiid_obj_field_len_bytes (fiid_template_t tmpl, u_int8_t *field);
int32_t fiid_obj_block_len (fiid_template_t tmpl, u_int8_t *field_start, u_int8_t *field_end);
int32_t fiid_obj_block_len_bytes (fiid_template_t tmpl, u_int8_t *field_start, u_int8_t *field_end);
void * fiid_obj_alloc (fiid_template_t tmpl);
void * fiid_obj_memset (fiid_obj_t obj, int c, fiid_template_t tmpl);
void fiid_obj_free (fiid_obj_t obj);
int8_t fiid_obj_set (fiid_obj_t obj, fiid_template_t tmpl, u_int8_t *field, u_int64_t val);
int8_t fiid_obj_get (fiid_obj_t obj, fiid_template_t tmpl, u_int8_t *field, u_int64_t *val);
fiid_field_t *__fiid_template_make (u_int8_t dummy, ...);
void fiid_template_free (fiid_field_t *tmpl_dynamic);
int8_t fiid_obj_get_data (fiid_obj_t obj, fiid_template_t tmpl, u_int8_t *field, u_int8_t *data);
int8_t fiid_obj_set_data (fiid_obj_t obj, fiid_template_t tmpl, u_int8_t *field, u_int8_t *data);


#ifdef __cplusplus
}
#endif

#endif /* fiid.h */


