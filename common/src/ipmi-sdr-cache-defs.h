/*
  Copyright (C) 2006 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#ifndef _IPMI_SDR_CACHE_DEFS_H
#define _IPMI_SDR_CACHE_DEFS_H

#define _SDR_FIID_TEMPLATE_COMPARE(__tmpl1, __tmpl2)                        \
do {                                                                        \
    int __ret;                                                              \
    if ((__ret = fiid_template_compare ((__tmpl1), (__tmpl2))) < 0)         \
      {                                                                     \
       ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;                            \
       goto cleanup;                                                        \
      }                                                                     \
    if (!__ret)                                                             \
      {                                                                     \
       ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;                            \
       goto cleanup;                                                        \
      }                                                                     \
} while (0)

#define _SDR_FIID_TEMPLATE_LEN_BYTES(__len, __tmpl)                         \
do {                                                                        \
  if (((__len) = fiid_template_len_bytes ((__tmpl))) < 0)                   \
    {                                                                       \
      ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;                             \
      goto cleanup;                                                         \
    }                                                                       \
} while (0)

#define _SDR_FIID_OBJ_CLEAR(__obj)                                  \
do {                                                                \
  if (fiid_obj_clear ((__obj)) < 0)                                 \
    {                                                               \
      ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;                     \
      goto cleanup;                                                 \
    }                                                               \
} while (0)

#define _SDR_FIID_OBJ_CREATE(__obj, __tmpl)                         \
do {                                                                \
  if (!((__obj) = fiid_obj_create ((__tmpl))))                      \
    {                                                               \
      ctx->errnum = SDR_CACHE_CTX_ERR_OUTMEM;                       \
      goto cleanup;                                                 \
    }                                                               \
} while (0)

#define _SDR_FIID_OBJ_TEMPLATE_COMPARE(__obj, __tmpl)                \
do {                                                                 \
    int __ret;                                                       \
    if ((__ret = fiid_obj_template_compare ((__obj), (__tmpl))) < 0) \
      {                                                              \
       ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;                     \
       goto cleanup;                                                 \
      }                                                              \
    if (!__ret)                                                      \
      {                                                              \
       ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;                     \
       goto cleanup;                                                 \
      }                                                              \
} while (0)

#define _SDR_FIID_OBJ_GET(__obj, __field, __val)                    \
do {                                                                \
    uint64_t __tmp_val = 0, *__val_ptr;                             \
    __val_ptr = (__val);                                            \
    if (fiid_obj_get ((__obj), (__field), &__tmp_val) < 0)          \
      {                                                             \
        ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;                   \
        goto cleanup;                                               \
      }                                                             \
    *__val_ptr = __tmp_val;                                         \
} while (0)

#define _SDR_FIID_OBJ_GET_DATA(__len, __obj, __field, __data, __datalen) \
do {                                                                     \
    if (((__len) = fiid_obj_get_data ((__obj),                           \
                                      (__field),                         \
                                      (__data),                          \
                                      (__datalen))) < 0)                 \
      {                                                                  \
        ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;                        \
        goto cleanup;                                                    \
      }                                                                  \
} while (0)

#define _SDR_FIID_OBJ_SET_ALL(__len, __obj, __data, __datalen)       \
do {                                                                 \
    if (((__len) = fiid_obj_set_all ((__obj),                        \
                                     (__data),                       \
                                     (__datalen))) < 0)              \
      {                                                              \
        ctx->errnum = SDR_CACHE_CTX_ERR_INTERNAL;                    \
        goto cleanup;                                                \
      }                                                              \
} while (0)

#define SDR_CACHE_CTX_MAGIC 0xabf00f00

struct sdr_cache_ctx {
  uint32_t magic;
  unsigned int errnum;
};

#endif
