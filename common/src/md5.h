/*****************************************************************************\
 *  $Id: md5.h,v 1.2.2.1 2006-03-02 04:52:27 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2003 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmipower is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmipower is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmipower; if not, write to the Free Software Foundation, Inc.,
 *  59 Temple Place, Suite 330, Boston, MA  02110-1301  USA.
\*****************************************************************************/

#ifndef _MD5_H
#define _MD5_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define MD5_BLOCK_LEN       64
#define MD5_BLOCK_WORDS_LEN (MD5_BLOCK_LEN/4)
#define MD5_DIGEST_LEN      16

typedef struct __md5 {
  uint32_t magic;
  unsigned int mlen;
  unsigned int bytes_mod_64;
  uint32_t bit_count[2];
  uint32_t a;
  uint32_t b;
  uint32_t c;
  uint32_t d;
  uint8_t m[MD5_BLOCK_LEN];
} md5_t;

int md5_init(md5_t *ctx);

int md5_update_data(md5_t *ctx, uint8_t *buf, unsigned int buflen);

int md5_finish(md5_t *ctx, uint8_t *digest, unsigned int digestlen);

#ifdef __cplusplus
}
#endif

#endif /* _MD5_H */
