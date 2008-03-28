/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  

*/

#ifndef _FIID_H
#define	_FIID_H	1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

/* 
 * FIID Error Codes
 */

enum fiid_err 
  {
    FIID_ERR_SUCCESS                         =  0,
    FIID_ERR_OBJ_NULL                        =  1,
    FIID_ERR_OBJ_INVALID                     =  2,                
    FIID_ERR_ITERATOR_NULL                   =  3,
    FIID_ERR_ITERATOR_INVALID                =  4,
    FIID_ERR_PARAMETERS                      =  5,
    FIID_ERR_TEMPLATE_INVALID                =  6,
    FIID_ERR_FIELD_NOT_FOUND                 =  7,
    FIID_ERR_KEY_INVALID                     =  8,
    FIID_ERR_FLAGS_INVALID                   =  9,
    FIID_ERR_TEMPLATE_NOT_BYTE_ALIGNED       = 10,
    FIID_ERR_FIELD_NOT_BYTE_ALIGNED          = 11,
    FIID_ERR_BLOCK_NOT_BYTE_ALIGNED          = 12,
    FIID_ERR_OVERFLOW                        = 13,
    FIID_ERR_MAX_FIELD_LEN_MISMATCH          = 14,
    FIID_ERR_KEY_FIELD_MISMATCH              = 15,
    FIID_ERR_FLAGS_FIELD_MISMATCH            = 16,
    FIID_ERR_TEMPLATE_LENGTH_MISMATCH        = 17,
    FIID_ERR_DATA_NOT_BYTE_ALIGNED           = 18,
    FIID_ERR_REQUIRED_FIELD_MISSING          = 19,
    FIID_ERR_FIXED_LENGTH_FIELD_INVALID      = 20,
    FIID_ERR_OUT_OF_MEMORY                   = 21,
    FIID_ERR_INTERNAL_ERROR                  = 22,
    FIID_ERR_ERRNUMRANGE                     = 23
  };

typedef enum fiid_err fiid_err_t;

/*  
 * FIID Field Maximum Key Length
 */

#define FIID_FIELD_MAX_KEY_LEN      256

/* 
 * FIID Field Flags
 *
 * REQUIRED
 *
 * The field in the template is required.  If not set, a packet cannot
 * be created.
 *
 * OPTIONAL
 *
 * The field in the template is not required.  If not set, a packet
 * can still be created.
 *
 * LENGTH_FIXED
 *
 * The number of bits that must be set in the field is fixed.  It
 * cannot be any other length.
 *
 * LENGTH_VARIABLE
 *
 * The number of bits that must be set in the field is variable in
 * length.
 *
 * In a template field, either the REQUIRED or OPTIONAL flag must be
 * specified.
 *
 * In a template field, either the LENGTH_FIXED or LENGTH_VARIABLE
 * flag must be specified.
 * 
 */

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

/* 
 * fiid_field_t
 *
 * Defines a FIID field:
 *
 * max_field_len - maximum length of a field in bits
 * key - field name
 * flags - indicating field requirements
 *
 * An array of field's makes up a FIID template.
 */
typedef struct fiid_field
{
  uint32_t max_field_len;
  char key[FIID_FIELD_MAX_KEY_LEN];
  uint32_t flags;
} fiid_field_t;

/*  
 * FIID Template
 *
 * An array of fiid_field_t's make up a fiid template.  The array should be
 * terminated with a field with a maximum field length of 0.
 *
 * The FIID template should be a multiple of 8 (i.e. byte aligned) otherwise
 * most of the FIID API will return errors.
 */
typedef fiid_field_t fiid_template_t[];

typedef struct fiid_obj *fiid_obj_t;

typedef struct fiid_iterator *fiid_iterator_t;

/*****************************
 * FIID Template API         *
 *****************************/

/* 
 * fiid_template_field_lookup
 *
 * Returns 1 if the field is found in the template, 0 if not, -1 on
 * error.
 */
int8_t fiid_template_field_lookup (fiid_template_t tmpl, 
                                   char *field);

/* 
 * fiid_template_len
 *
 * Returns the total length (in bits) of the all the fields in the
 * template, -1 on error.
 */
int32_t fiid_template_len (fiid_template_t tmpl);

/* 
 * fiid_template_len_bytes
 *
 * Returns the total length (in bytes) of the all the fields in the
 * template, -1 on error.  Will return an error if template bit length
 * is not a multiple of 8.
 */
int32_t fiid_template_len_bytes (fiid_template_t tmpl);

/* 
 * fiid_template_field_start
 *
 * Returns the offset (in bits) of the beginning of the field within
 * this template, -1 on error.
 */
int32_t fiid_template_field_start (fiid_template_t tmpl, 
                                   char *field);

/* 
 * fiid_template_field_start_bytes
 *
 * Returns the offset (in bytes) of the beginning of the field within
 * this template, -1 on error.  Will return an error if field bit
 * offset is not a multiple of 8.
 */
int32_t fiid_template_field_start_bytes (fiid_template_t tmpl, 
                                         char *field);

/* 
 * fiid_template_field_end
 *
 * Returns the offset (in bits) of the ending of the field within this
 * template, -1 on error.
 */
int32_t fiid_template_field_end (fiid_template_t tmpl, 
                                 char *field);

/* 
 * fiid_template_field_end_bytes
 *
 * Returns the offset (in bytes) of the ending of the field within
 * this template, -1 on error.  Will return an error if field bit
 * offset is not a multiple of 8.
 */
int32_t fiid_template_field_end_bytes (fiid_template_t tmpl, 
                                       char *field);

/* 
 * fiid_template_field_len
 *
 * Returns the maximum length (in bits) of the specified field, -1 on
 * error.
 */
int32_t fiid_template_field_len (fiid_template_t tmpl, 
                                 char *field);

/* 
 * fiid_template_field_len_bytes
 *
 * Returns the maximum length (in bytes) of the specified field, -1 on
 * error.  Will return an error if the field maximum bit length is not
 * a multiple of 8.
 */
int32_t fiid_template_field_len_bytes (fiid_template_t tmpl, 
                                       char *field);

/* 
 * fiid_template_block_len
 *
 * Returns the maximum length (in bits) of the block of fields
 * beginning at 'field_start' and ending at 'field_end'.  Returns -1
 * on error.
 */
int32_t fiid_template_block_len (fiid_template_t tmpl, 
				 char *field_start, 
				 char *field_end);

/* 
 * fiid_template_block_len_bytes
 *
 * Returns the maximum length (in bytes) of the block of fields
 * beginning at 'field_start' and ending at 'field_end'.  Returns -1
 * on error.  Will return an error if the calculated bit length is not
 * a multiple of 8.
 */
int32_t fiid_template_block_len_bytes (fiid_template_t tmpl, 
				       char *field_start, 
				       char *field_end);

/*  
 * fiid_template_compare
 *
 * Returns 1 if the two specified templates are identical, 0 if not,
 * -1 on error.
 */
int8_t fiid_template_compare(fiid_template_t tmpl1, 
                             fiid_template_t tmpl2);

/* 
 * fiid_template_free
 *
 * Free's a template created by fiid_obj_template.
 */
void fiid_template_free (fiid_field_t *tmpl_dynamic);

/*****************************
 * FIID Object API           *
 *****************************/

/*  
 * fiid_strerror
 *
 * Return statically allocated string describing the specified error.
 */
char *fiid_strerror(fiid_err_t errnum);

/* 
 * fiid_obj_create
 *
 * Return a fiid object based on the specified template.  Returns NULL
 * on error.
 */
fiid_obj_t fiid_obj_create (fiid_template_t tmpl);

/* 
 * fiid_obj_destroy
 *
 * Destroy and free memory from a fiid object.  
 */
void fiid_obj_destroy (fiid_obj_t obj);

/* 
 * fiid_obj_dup
 *
 * Create and return a duplicate object from the one specified.
 * Returns NULL on error.
 */
fiid_obj_t fiid_obj_dup (fiid_obj_t src_obj);

/*
 * fiid_obj_copy
 *
 * Create and return a duplicate object from the one specified, but
 * base the new object on the alternate template specified.  Template
 * length of the original and alternate template must be the same.
 * Returns NULL on error.
 */
fiid_obj_t fiid_obj_copy (fiid_obj_t src_obj, fiid_template_t alt_tmpl);

/* 
 * fiid_obj_valid
 *
 * Returns 1 if the object passed in is a valid fiid object, 0 if not.
 */
int8_t fiid_obj_valid(fiid_obj_t obj);

/* 
 * fiid_obj_packet_valid
 *
 * Returns 1 if the object contains a all the data for a valid packet,
 * 0 if not, -1 on error.  A valid packet is based on the field flags
 * specified in the original fiid template.  For example, this
 * function will check if all required fields have been set with the
 * correct number of bytes.  It will also check that data set within
 * the object is byte aligned.
 */
int8_t fiid_obj_packet_valid(fiid_obj_t obj);

/* 
 * fiid_obj_template
 *
 * Create a template based on what is stored internally within the
 * object.  Returns NULL on error.  Free the resulting template using
 * fiid_template_free().
 */
fiid_field_t *fiid_obj_template(fiid_obj_t obj);

/* 
 * fiid_obj_template_compare
 *
 * Returns 1 if the template specified is the one used to create the
 * object, 0 if not, -1 one rror.
 */
int8_t fiid_obj_template_compare(fiid_obj_t obj, fiid_template_t tmpl);

/* 
 * fiid_obj_errnum
 *
 * Returns the error code for the most recently occurring error.
 */
fiid_err_t fiid_obj_errnum(fiid_obj_t obj);

/*  
 * fiid_obj_len
 * 
 * Returns the total length (in bits) of data stored within the
 * object, -1 on error.
 */
int32_t fiid_obj_len(fiid_obj_t obj);

/*  
 * fiid_obj_len_bytes
 * 
 * Returns the total length (in bytes) of data stored within the
 * object, -1 on error.  Will return an error if the total bit length
 * of data is not a multiple of 8.
 */
int32_t fiid_obj_len_bytes(fiid_obj_t obj);

/*  
 * fiid_obj_field_len
 * 
 * Returns the length (in bits) of data stored within the
 * specified field, -1 on error.
 */
int32_t fiid_obj_field_len(fiid_obj_t obj, char *field);

/*  
 * fiid_obj_field_len_bytes
 * 
 * Returns the length (in bytes) of data stored within the specified
 * field, -1 on error.  Will return an error if the bit length of data
 * is not a multiple of 8.
 */
int32_t fiid_obj_field_len_bytes(fiid_obj_t obj, char *field);

/*
 * fiid_obj_block_len
 *
 * Returns the length (in bits) of data stored within the block of
 * fields beginning at 'field_start' and ending at 'field_end'.
 * Returns -1 on error.
 */
int32_t fiid_obj_block_len(fiid_obj_t obj, 
                           char *field_start, 
                           char *field_end);

/* 
 * fiid_obj_block_len_bytes
 *
 * Returns the length (in bytes) of data stored within the block of
 * fields beginning at 'field_start' and ending at 'field_end'.
 * Returns -1 on error.  Will return an error if the calculated bit
 * length is not a multiple of 8.
 */
int32_t fiid_obj_block_len_bytes(fiid_obj_t obj, 
                                 char *field_start, 
                                 char *field_end);

/*  
 * fiid_obj_clear
 * 
 * Clear all data stored in the object.  Return 0 on success, -1 on
 * error.
 */
int8_t fiid_obj_clear (fiid_obj_t obj);

/*  
 * fiid_obj_clear_field
 * 
 * Clear all data stored in a specified field in the object.  Return 0
 * on success, -1 on error.
 */
int8_t fiid_obj_clear_field (fiid_obj_t obj, char *field);

/*  
 * fiid_obj_field_lookup
 * 
 * Returns 1 if the field is found in the object, 0 if not, -1 on
 * error.
 */
int8_t fiid_obj_field_lookup (fiid_obj_t obj, char *field);

/* 
 * fiid_obj_set
 *
 * Set data in the object for the specified field.  Returns 0 on
 * success, -1 on error.
 */
int8_t fiid_obj_set (fiid_obj_t obj, char *field, uint64_t val);

/* 
 * fiid_obj_get
 *
 * Get data stored in the object for the specified field.  Returns 1
 * if data was available and returned, 0 if no data was available, -1
 * one error.
 */
int8_t fiid_obj_get (fiid_obj_t obj, char *field, uint64_t *val);

/* 
 * fiid_obj_set_data
 *
 * Set an array of data in the object for the specified field.
 * Returns length of data set on success, -1 on error.  The field
 * specified must begin on a byte boundary and have a maximum bit
 * length that is a multiple of 8.  Will truncate the data written
 * if the field maximum length is smaller than the data given.
 */
int32_t fiid_obj_set_data (fiid_obj_t obj, 
                           char *field, 
                           uint8_t *data, 
                           uint32_t data_len);

/* 
 * fiid_obj_get_data
 *
 * Get an array of data in the object for the specified field.
 * Returns length of data read on success, -1 on error.  The field
 * specified must begin on a byte boundary and have a data bit length
 * that is a multiple of 8.
 */
int32_t fiid_obj_get_data (fiid_obj_t obj, 
                           char *field, 
                           uint8_t *data, 
                           uint32_t data_len);

/* 
 * fiid_obj_set_all
 *
 * Set all fields in the object with the specified array of data.
 * Returns length of data set on success, -1 on error.  The given data
 * must fall on a byte boundary of the object.  Will truncate the data
 * written if the total object maximum length is smaller than the data
 * given.  Will write as much as possible if data is not large enough
 * to fill the entire object.
 */
int32_t fiid_obj_set_all (fiid_obj_t obj, uint8_t *data, uint32_t data_len);

/* 
 * fiid_obj_get_all
 *
 * Get an array of all data in the object.  Returns length of data
 * read on success, -1 on error.  
 */
int32_t fiid_obj_get_all (fiid_obj_t obj, uint8_t *data, uint32_t data_len);

/* 
 * fiid_obj_set_block
 *
 * Set a block of fields in the object, beginning with 'field_start'
 * and ending with 'field_end'.  Returns length of data set on
 * success, -1 on error.  The fields given must fall on a byte
 * boundary of the object.  Will truncate the data written if the
 * total block maximum length is smaller than the data given.  Will
 * write as much as possible if data is not large enough to fill the
 * entire block.
 */
int8_t fiid_obj_set_block (fiid_obj_t obj, 
                           char *field_start, 
                           char *field_end, 
                           uint8_t *data, 
                           uint32_t data_len);

/* 
 * fiid_obj_get_block
 *
 * Get a block of data in the object, beginning with 'field_start' and
 * ending with 'field_end'.  Returns length of data read on success,
 * -1 on error.  Data being read must fall on a byte boundary.
 */
int8_t fiid_obj_get_block (fiid_obj_t obj, 
                           char *field_start, 
                           char *field_end, 
                           uint8_t *data, 
                           uint32_t data_len);

/*****************************
 * FIID Iterator API         *
 *****************************/

/*  
 * fiid_iterator_create
 *
 * Create a fiid iterator to iteratate through each field in an
 * object.  Returns iterator on success, NULL on error.
 */
fiid_iterator_t fiid_iterator_create(fiid_obj_t obj);

/*  
 * fiid_iterator_destroy
 *
 * Destroy and free memory from a fiid iterator.  
 */
void fiid_iterator_destroy(fiid_iterator_t iter);

/*
 * fiid_iterator_errnum
 *
 * Returns the error code for the most recently occurring error.
 */
fiid_err_t fiid_iterator_errnum(fiid_iterator_t iter);

/*  
 * fiid_iterator_reset
 *
 * Reset the iterator back to the beginning.  Returns 0 on success, -1
 * on error.
 */
int8_t fiid_iterator_reset(fiid_iterator_t iter);

/* 
 * fiid_iterator_next
 *
 * Move the iterator to the next field.  Returns 0 on success, -1 on
 * error.
 */
int8_t fiid_iterator_next(fiid_iterator_t iter);

/*  
 * fiid_iterator_end
 *
 * Returns 1 if you are at the end of the iterator, 0 if not, -1
 * error.
 */
int8_t fiid_iterator_end(fiid_iterator_t iter);

/*  
 * fiid_iterator_field_len
 *
 * Returns the number of bits set for the current field.  Returns -1
 * on error.
 */
int32_t fiid_iterator_field_len(fiid_iterator_t iter);

/*  
 * fiid_iterator_key
 *
 * Returns the key name for the current field.  Returns NULL on error.
 */
char *fiid_iterator_key(fiid_iterator_t iter);

/*  
 * fiid_iterator_get
 *
 * Get data stored in the object for the current field.  Returns 1
 * if data was available and returned, 0 if no data was available, -1
 * one error.
 */
int32_t fiid_iterator_get(fiid_iterator_t iter, uint64_t *val);

/*  
 * fiid_iterator_get_data
 *
 * Get an array of data in the object for the current field.  Returns
 * length of data read on success, -1 on error.  The current field
 * must begin on a byte boundary and have a data bit length that is a
 * multiple of 8.
 */
int32_t fiid_iterator_get_data(fiid_iterator_t iter, 
                               uint8_t *data, 
                               uint32_t data_len);

#ifdef __cplusplus
}
#endif

#endif /* fiid.h */


