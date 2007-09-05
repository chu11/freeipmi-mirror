/*****************************************************************************\
 *  $Id: ipmi-fru-fiid.h,v 1.3 2007-09-05 20:13:25 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-232183
 *
 *  This file is part of Ipmi-fru, a tool used for retrieving
 *  motherboard field replaceable unit (FRU) information. For details,
 *  see http://www.llnl.gov/linux/.
 *
 *  Ipmi-fru is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-fru is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-fru; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
\*****************************************************************************/

#ifndef _IPMI_FRU_FIID_H
#define _IPMI_FRU_FIID_H

#include <freeipmi/freeipmi.h>

#define _FIID_TEMPLATE_LEN_BYTES(__len, __tmpl)                      \
do {                                                                 \
  if (((__len) = fiid_template_len_bytes((__tmpl))) < 0)             \
    {                                                                \
      pstdout_perror(state_data->pstate, "fiid_template_len_bytes"); \
      goto cleanup;                                                  \
    }                                                                \
} while (0)

#define _FIID_TEMPLATE_FIELD_START_BYTES(__len, __tmpl, __field)             \
do {                                                                         \
  if (((__len) = fiid_template_field_start_bytes((__tmpl), (__field))) < 0)  \
    {                                                                        \
      pstdout_perror(state_data->pstate, "fiid_template_field_start_bytes"); \
      goto cleanup;                                                          \
    }                                                                        \
} while (0)

#define _FIID_OBJ_CLEAR(__obj)                                      \
do {                                                                \
  if (fiid_obj_clear ((__obj)) < 0)                                 \
    {                                                               \
      pstdout_perror(state_data->pstate, "fiid_obj_clear");         \
      goto cleanup;                                                 \
    }                                                               \
} while (0)

#define _FIID_OBJ_CREATE(__obj, __tmpl)                             \
do {                                                                \
  if (!((__obj) = fiid_obj_create ((__tmpl))))                      \
    {                                                               \
      pstdout_perror(state_data->pstate, "fiid_obj_create");        \
      goto cleanup;                                                 \
    }                                                               \
} while (0)

#define _FIID_OBJ_GET(__obj, __field, __val)                        \
do {                                                                \
    uint64_t __tmp_val = 0, *__val_ptr;                             \
    __val_ptr = (__val);                                            \
    if (fiid_obj_get ((__obj), (__field), &__tmp_val) < 0)          \
      {                                                             \
        pstdout_fprintf(state_data->pstate,                         \
                        stderr,                                     \
                        "fiid_obj_get: %s: %s\n",                   \
                        (__field),                                  \
                        fiid_strerror(fiid_obj_errnum((__obj))));   \
        goto cleanup;                                               \
      }                                                             \
    *__val_ptr = __tmp_val;                                         \
} while (0)

#define _FIID_OBJ_GET_DATA(__len, __obj, __field, __data, __datalen) \
do {                                                                 \
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

#define _FIID_OBJ_SET_ALL(__len, __obj, __data, __datalen)           \
do {                                                                 \
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

#endif
