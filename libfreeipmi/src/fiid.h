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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  

*/

#ifndef _FIID_H
#define	_FIID_H	1

#ifdef __cplusplus
extern "C" {
#endif

#define __FIID_OBJ_SET_ERRNO(__obj)            \
do {                                           \
  int32_t __errnum = fiid_obj_errnum((__obj)); \
  if (__errnum == FIID_ERR_SUCCESS)            \
    errno = 0;                                 \
  else if (__errnum == FIID_ERR_OUTMEM)        \
    errno = ENOMEM;                            \
  else if (__errnum == FIID_ERR_OVERFLOW)      \
    errno = ENOSPC;                            \
  else                                         \
    errno = EINVAL;                            \
} while (0)

#define FIID_OBJ_SET(__obj, __field, __val)              \
do {                                                     \
    if (fiid_obj_set ((__obj), (__field), (__val)) < 0)  \
      {                                                  \
          __FIID_OBJ_SET_ERRNO((__obj));                 \
         return (-1);                                    \
      }                                                  \
} while (0)

#define FIID_OBJ_SET_DATA(__obj, __field, __val, __val_len)               \
do {                                                                      \
    if (fiid_obj_set_data ((__obj), (__field), (__val), (__val_len)) < 0) \
      {                                                                   \
          __FIID_OBJ_SET_ERRNO((__obj));                                  \
         return (-1);                                                     \
      }                                                                   \
} while (0)

#define FIID_OBJ_GET(__obj, __field, __val)                   \
do {                                                          \
    uint64_t __localval = 0, *__localval_ptr;                 \
    __localval_ptr = (__val);                                 \
    if (fiid_obj_get ((__obj), (__field), &__localval) < 0)   \
      {                                                       \
         __FIID_OBJ_SET_ERRNO((__obj));                       \
         return (-1);                                         \
      }                                                       \
    *__localval_ptr = __localval;                             \
} while (0)

#define FIID_OBJ_GET_DATA(__obj, __field, __val, __val_len)                \
do {                                                                       \
    if (fiid_obj_get_data ((__obj), (__field), (__val), (__val_len)) < 0)  \
      {                                                                    \
          __FIID_OBJ_SET_ERRNO((__obj));                                   \
         return (-1);                                                      \
      }                                                                    \
} while (0)

#define FIID_OBJ_TEMPLATE_COMPARE(__obj, __tmpl)           \
do {                                                       \
    if (fiid_obj_template_compare ((__obj), (__tmpl)) < 0) \
      {                                                    \
         __FIID_OBJ_SET_ERRNO((__obj));                    \
         return (-1);                                      \
      }                                                    \
} while (0)

#define FIID_ERR_SUCCESS                         0
#define FIID_ERR_OBJ_NULL                        1 
#define FIID_ERR_OBJ_INVALID                     2                   
#define FIID_ERR_ITERATOR_NULL                   3
#define FIID_ERR_ITERATOR_INVALID                4
#define FIID_ERR_PARAMETERS                      5
#define FIID_ERR_FIELD_NOT_FOUND                 6
#define FIID_ERR_KEY_INVALID                     7
#define FIID_ERR_FLAGS_INVALID                   8
#define FIID_ERR_TEMPLATE_NOT_BYTE_ALIGNED       9
#define FIID_ERR_OVERFLOW                       10
#define FIID_ERR_MAX_FIELD_LEN_MISMATCH         11
#define FIID_ERR_KEY_FIELD_MISMATCH             12
#define FIID_ERR_FLAGS_FIELD_MISMATCH           13
#define FIID_ERR_TEMPLATE_LENGTH_MISMATCH       14
#define FIID_ERR_DATA_NOT_BYTE_ALIGNED          15

#define FIID_ERR_REQUIRED_FIELD_MISSING         16
#define FIID_ERR_FIXED_LENGTH_FIELD_INVALID     17
#define FIID_ERR_OUTMEM                         18
#define FIID_ERR_INTERNAL                       19
#define FIID_ERR_ERRNUMRANGE                    20

#define fiid_template_make(arg...) __fiid_template_make (1, arg, 0)

#define FIID_FIELD_MAX 256

#define FIID_FIELD_REQUIRED         0x00000001
#define FIID_FIELD_OPTIONAL         0x00000002
#define FIID_FIELD_REQUIRED_MASK    0x0000000F

#define FIID_FIELD_REQUIRED_FLAG(__flags) \
        ((__flags) & FIID_FIELD_REQUIRED_MASK)

#define FIID_FIELD_REQUIRED_FLAG_VALID(__flags) \
        ((FIID_FIELD_REQUIRED_FLAG(__flags) ==  FIID_FIELD_REQUIRED \
	  || FIID_FIELD_REQUIRED_FLAG(__flags) ==  FIID_FIELD_OPTIONAL) ? 1 : 0)
  
#define FIID_FIELD_LENGTH_FIXED     0x00000010
#define FIID_FIELD_LENGTH_VARIABLE  0x00000020
#define FIID_FIELD_LENGTH_MASK      0x000000F0

#define FIID_FIELD_LENGTH_FLAG(__flags) \
        ((__flags) & FIID_FIELD_LENGTH_MASK)

#define FIID_FIELD_LENGTH_FLAG_VALID(__flags) \
        ((FIID_FIELD_LENGTH_FLAG(__flags) ==  FIID_FIELD_LENGTH_FIXED \
	  || FIID_FIELD_LENGTH_FLAG(__flags) ==  FIID_FIELD_LENGTH_VARIABLE) ? 1 : 0)

#define FIID_FIELD_FLAGS_DEFAULT    (FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED)

typedef struct fiid_field
{
  uint32_t max_field_len;
  char key[FIID_FIELD_MAX];
  uint32_t flags;
} fiid_field_t;

typedef fiid_field_t fiid_template_t[];

typedef struct fiid_obj *fiid_obj_t;

typedef struct fiid_iterator *fiid_iterator_t;

int8_t fiid_template_field_lookup (fiid_template_t tmpl, char *field);
int32_t fiid_template_len (fiid_template_t tmpl);
int32_t fiid_template_len_bytes (fiid_template_t tmpl);
int32_t fiid_template_field_start (fiid_template_t tmpl, char *field);
int32_t fiid_template_field_start_bytes (fiid_template_t tmpl, char *field);
int32_t fiid_template_field_end (fiid_template_t tmpl, char *field);
int32_t fiid_template_field_end_bytes (fiid_template_t tmpl, char *field);
int32_t fiid_template_field_len (fiid_template_t tmpl, char *field);
int32_t fiid_template_field_len_bytes (fiid_template_t tmpl, 
				       char *field);
int32_t fiid_template_block_len (fiid_template_t tmpl, 
				 char *field_start, 
				 char *field_end);
int32_t fiid_template_block_len_bytes (fiid_template_t tmpl, 
				       char *field_start, 
				       char *field_end);
fiid_field_t *__fiid_template_make (uint8_t dummy, ...);
void fiid_template_free (fiid_field_t *tmpl_dynamic);

char *fiid_strerror(int32_t errnum);

fiid_obj_t fiid_obj_create (fiid_template_t tmpl);
int8_t fiid_obj_destroy (fiid_obj_t obj);
fiid_obj_t fiid_obj_dup (fiid_obj_t src_obj);
int8_t fiid_obj_valid(fiid_obj_t obj);
int8_t fiid_obj_packet_valid(fiid_obj_t obj);
fiid_field_t *fiid_obj_template(fiid_obj_t obj);
int8_t fiid_obj_template_compare(fiid_obj_t obj, fiid_template_t tmpl);
int32_t fiid_obj_errnum(fiid_obj_t obj);

int32_t fiid_obj_len(fiid_obj_t obj);
int32_t fiid_obj_len_bytes(fiid_obj_t obj);
int32_t fiid_obj_field_len(fiid_obj_t obj, char *field);
int32_t fiid_obj_field_len_bytes(fiid_obj_t obj, char *field);

int8_t fiid_obj_clear (fiid_obj_t obj);
int8_t fiid_obj_clear_field (fiid_obj_t obj, char *field);
int8_t fiid_obj_field_lookup (fiid_obj_t obj, char *field);
int8_t fiid_obj_set (fiid_obj_t obj, char *field, uint64_t val);
int8_t fiid_obj_get (fiid_obj_t obj, char *field, uint64_t *val);
int32_t fiid_obj_set_data (fiid_obj_t obj, char *field, uint8_t *data, uint32_t data_len);
int32_t fiid_obj_get_data (fiid_obj_t obj, char *field, uint8_t *data, uint32_t data_len);
int32_t fiid_obj_get_all (fiid_obj_t obj, uint8_t *data, uint32_t data_len);
int32_t fiid_obj_set_all (fiid_obj_t obj, uint8_t *data, uint32_t data_len);

int8_t fiid_obj_set_block (fiid_obj_t obj, char *field_start, char *field_end, uint8_t *data, uint32_t data_len);
int8_t fiid_obj_get_block (fiid_obj_t obj, char *field_start, char *field_end, uint8_t *data, uint32_t data_len);

fiid_iterator_t fiid_iterator_create(fiid_obj_t obj);
int8_t fiid_iterator_destroy(fiid_iterator_t iter);
int32_t fiid_iterator_errnum(fiid_iterator_t iter);
int8_t fiid_iterator_reset(fiid_iterator_t iter);
int8_t fiid_iterator_next(fiid_iterator_t iter);
int8_t fiid_iterator_end(fiid_iterator_t iter);
int32_t fiid_iterator_field_len(fiid_iterator_t iter);
uint8_t *fiid_iterator_key(fiid_iterator_t iter);
int32_t fiid_iterator_get(fiid_iterator_t iter, uint64_t *val);
int32_t fiid_iterator_get_data(fiid_iterator_t iter, uint8_t *data, uint32_t data_len);

#ifdef __cplusplus
}
#endif

#endif /* fiid.h */


