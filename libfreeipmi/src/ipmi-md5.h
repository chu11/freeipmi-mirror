/* 
   ipmi-md5.h - IPMI Command Specification

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

#ifndef _IPMI_MD5_H
#define _IPMI_MD5_H

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/types.h>

#define IPMI_MD5_BLOCK_LEN       64
#define IPMI_MD5_BLOCK_WORDS_LEN (IPMI_MD5_BLOCK_LEN/4)
#define IPMI_MD5_DIGEST_LEN      16

typedef struct __md5 {
  u_int32_t magic;
  unsigned int mlen;
  unsigned int bytes_mod_64;
  u_int32_t bit_count[2];
  u_int32_t a;
  u_int32_t b;
  u_int32_t c;
  u_int32_t d;
  u_int8_t m[IPMI_MD5_BLOCK_LEN];
} ipmi_md5_t;

int ipmi_md5_init(ipmi_md5_t *ctx);

int ipmi_md5_update_data(ipmi_md5_t *ctx, u_int8_t *buf, unsigned int buflen);

int ipmi_md5_finish(ipmi_md5_t *ctx, u_int8_t *digest, unsigned int digestlen);

#ifdef __cplusplus
}
#endif

#endif /* _IPMI_MD5_H */
