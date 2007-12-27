#ifndef _IPMI_SEL_FIID_H
#define _IPMI_SEL_FIID_H

#include "freeipmi/fiid/fiid.h"

#define _FIID_TEMPLATE_LEN_BYTES(__len, __tmpl)                        \
  do {                                                                 \
    if (((__len) = fiid_template_len_bytes((__tmpl))) < 0)             \
      {                                                                \
        pstdout_fprintf(state_data->pstate,                            \
                        stderr,                                        \
                        "fiid_template_len_bytes: %s\n",               \
                        strerror(errno));                              \
        goto cleanup;                                                  \
      }                                                                \
  } while (0)

#define _FIID_TEMPLATE_FIELD_START_BYTES(__len, __tmpl, __field)                      \
  do {                                                                                \
    if (((__len) = fiid_template_field_start_bytes((__tmpl), (__field))) < 0)         \
      {                                                                               \
        pstdout_fprintf(state_data->pstate,                                           \
                        stderr,                                                       \
                        "fiid_template_field_start_bytes: %s: %s\n",                  \
                        (__field),                                                    \
                        strerror(errno));                                             \
        goto cleanup;                                                                 \
      }                                                                               \
  } while (0)

#define _FIID_OBJ_CLEAR(__obj)                                        \
  do {                                                                \
    if (fiid_obj_clear ((__obj)) < 0)                                 \
      {                                                               \
        pstdout_perror(state_data->pstate, "fiid_obj_clear");         \
        goto cleanup;                                                 \
      }                                                               \
  } while (0)

#define _FIID_OBJ_CREATE(__obj, __tmpl)                   \
do {                                                      \
  if (!((__obj) = fiid_obj_create(__tmpl)))               \
    {                                                     \
      pstdout_fprintf(state_data->pstate,                 \
                      stderr,                             \
                      "fiid_obj_create: %s\n",            \
                      strerror(errno));                   \
      goto cleanup;                                       \
    }                                                     \
} while (0)

#define _FIID_OBJ_GET(__obj, __field, __val)                    \
do {                                                            \
    uint64_t __localval = 0, *__localval_ptr;                   \
    __localval_ptr = (__val);                                   \
    if (fiid_obj_get ((__obj), (__field), &__localval) < 0)     \
      {                                                         \
        pstdout_fprintf(state_data->pstate,                     \
                        stderr,                                 \
                        "fiid_obj_get: %s: %s\n",               \
                        __field,                                \
                        fiid_strerror(fiid_obj_errnum(__obj))); \
         goto cleanup;                                          \
      }                                                         \
    *__localval_ptr = __localval;                               \
} while (0)

#define _FIID_OBJ_GET_WITH_RETURN_VALUE(obj, field, val, rv)            \
  do {                                                                  \
    uint64_t _val = 0, *_val_ptr;                                       \
    _val_ptr = val;                                                     \
    if ((rv = fiid_obj_get (obj, field, &_val)) < 0)                    \
      {                                                                 \
        pstdout_fprintf(state_data->pstate,                             \
                        stderr,                                         \
                        "fiid_obj_get: %s: %s\n",                       \
                        field,                                          \
                        fiid_strerror(fiid_obj_errnum(obj)));           \
        goto cleanup;                                                   \
      }                                                                 \
    *_val_ptr = _val;                                                   \
  } while (0)

#define _FIID_OBJ_GET_DATA(__len, __obj, __field, __data, __datalen) \
  do {                                                               \
    if (((__len) = fiid_obj_get_data ((__obj),                       \
                                      (__field),                     \
                                      (__data),                      \
                                      (__datalen))) < 0)             \
      {                                                              \
        pstdout_fprintf(state_data->pstate,                          \
                        stderr,                                      \
                        "fiid_obj_get_data: %s: %s\n",               \
                        (__field),                                   \
                        fiid_strerror(fiid_obj_errnum((__obj))));    \
        goto cleanup;                                                \
      }                                                              \
  } while (0)

#define _FIID_OBJ_GET_DATA_LEN(__len, __obj, __field, __data, __data_len)               \
do {                                                                                    \
    if (((__len) = fiid_obj_get_data ((__obj), (__field), (__data), (__data_len))) < 0) \
      {                                                                                 \
         pstdout_fprintf(state_data->pstate,                                            \
                        stderr,                                                         \
                        "fiid_obj_get_data: %s: %s\n",                                  \
                        __field,                                                        \
                        fiid_strerror(fiid_obj_errnum(__obj)));                         \
         goto cleanup;                                                                  \
      }                                                                                 \
} while (0)

#define _FIID_OBJ_SET_ALL(__obj, __data, __data_len)                        \
do {                                                                        \
    if (fiid_obj_set_all ((__obj), (__data), (__data_len)) < 0)             \
      {                                                                     \
         pstdout_fprintf(state_data->pstate,                                \
                        stderr,                                             \
                        "fiid_obj_set_all: %s\n",                           \
                        fiid_strerror(fiid_obj_errnum(__obj)));             \
         goto cleanup;                                                      \
      }                                                                     \
} while (0)

#define _FIID_OBJ_SET_ALL_LEN(__len, __obj, __data, __datalen)       \
  do {                                                               \
    if (((__len) = fiid_obj_set_all ((__obj),                        \
                                     (__data),                       \
                                     (__datalen))) < 0)              \
      {                                                              \
        pstdout_fprintf(state_data->pstate,                          \
                        stderr,                                      \
                        "fiid_obj_set_all: %s\n",                    \
                        fiid_strerror(fiid_obj_errnum((__obj))));    \
        goto cleanup;                                                \
      }                                                              \
  } while (0)

#define _FIID_OBJ_DESTROY(__obj)                 \
  do {                                           \
    if ((__obj))                                 \
      {                                          \
        fiid_obj_destroy((__obj));               \
        (__obj) = NULL;                          \
      }                                          \
  } while (0)

#endif
