/*****************************************************************************\
 *  $Id: ipmi-sha1.c,v 1.1 2005-02-05 01:22:48 chu11 Exp $
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
 *  59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>

#ifdef STDC_HEADERS
#include <string.h>

#else
# include <sys/types.h>
# ifndef HAVE_MEMCPY
static void*
memcpy (void *dest, const void *src, size_t n)
{
  while (0 <= --n) ((unsigned char*)dest) [n] = ((unsigned char*)src) [n];
  return dest;
}
# endif
# ifndef HAVE_MEMSET
static void*
memset (void *s, int c, size_t n)
{
  while (0 <= --n) ((unsigned char*)s) [n] = (unsigned char) c;
  return s;
}
# endif
#endif

#include <sys/types.h>
#include <errno.h>
#include "freeipmi.h"

static u_int8_t padding[64] =
  {
    0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  };

#define IPMI_SHA1_MAGIC  0x01dead01

#define CSHIFTL(x, n) (((x) << (n)) | ((x) >> (32 - (n))))

int 
ipmi_sha1_init(ipmi_sha1_t *ctx) 
{
  if (ctx == NULL) 
    {
      errno = EINVAL;
      return -1;
    }

  ctx->magic = IPMI_SHA1_MAGIC;
  ctx->len = 0;
  ctx->mlen = 0;
  
  ctx->H[0] = 0x67452301;
  ctx->H[1] = 0xEFCDAB89;
  ctx->H[2] = 0x98BADCFE;
  ctx->H[3] = 0x10325476;
  ctx->H[4] = 0xC3D2E1F0;

  return 0;
}

static int32_t
_f(unsigned int t, int32_t B, int32_t C, int32_t D)
{
  if (t >= 0 && t <= 19)
    return ((B & C) | ((~B) & D));
  else if (t >= 20 && t <= 39)
    return (B ^ C ^ D);
  else if (t >= 40 && t <= 59)
    return ((B & C) | (B & D) | (C & D));
  else if (t >= 60 && t <= 79)
    return (B ^ C ^ D);
}

static int32_t
_K(unsigned int t)
{
  
  if (t >= 0 && t <= 19)
    return 0x5A827999;
  else if (t >= 20 && t <= 39)
    return 0x6ED9EBA1;
  else if (t >= 40 && t <= 59)
    return 0x8F1BBCDC;
  else if (t >= 60 && t <= 79)
    return 0xCA62C1D6;
}

static void 
_ipmi_sha1_update_digest(ipmi_sha1_t *ctx) 
{
  u_int32_t W[IPMI_SHA1_ROUNDS_LEN];
  u_int32_t A, B, C, D, E, TEMP;
  int t;

  /* Note there are no endian issues here, compiler is required to
   * handle shifts correctly
   */
  for (t = 0; t <= 15; t++)
    W[t] = ((ctx->M[t*4] << 24)
            | (ctx->M[t*4+1] << 16)
            | (ctx->M[t*4+2] << 8)
            | (ctx->M[t*4+3]));

  for (t = 16; t <= 79; t++)
    W[t] = CSHIFTL(W[t-3] ^ W[t-8] ^ W[t-14] ^ W[t-16], 1);

  A = ctx->H[0];
  B = ctx->H[1];
  C = ctx->H[2];
  D = ctx->H[3];
  E = ctx->H[4];

  for (t = 0; t <= 79; t++)
    {
      TEMP = CSHIFTL(A, 5) + _f(t, B, C, D) + E + W[t] + _K(t);
      E = D; 
      D = C;
      C = CSHIFTL(B, 30);
      B = A;
      A = TEMP;
    }
  
  ctx->H[0] = ctx->H[0] + A;
  ctx->H[1] = ctx->H[1] + B;
  ctx->H[2] = ctx->H[2] + C;
  ctx->H[3] = ctx->H[3] + D;
  ctx->H[4] = ctx->H[4] + E;
}

int 
ipmi_sha1_update_data(ipmi_sha1_t *ctx, u_int8_t *buf, unsigned int buflen) 
{

  if (ctx == NULL || ctx->magic != IPMI_SHA1_MAGIC || buf == NULL) 
    {
      errno = EINVAL;
      return -1;
    }

  if (buflen == 0)
    return 0;

  if ((ctx->mlen + buflen) >= IPMI_SHA1_BLOCK_LEN) 
    {
      unsigned int bufcount;
      
      bufcount = (IPMI_SHA1_BLOCK_LEN - ctx->mlen);
      memcpy(ctx->M + ctx->mlen, buf, bufcount);
      _ipmi_sha1_update_digest(ctx);
    
      while ((buflen - bufcount) >= IPMI_SHA1_BLOCK_LEN) 
        {
          memcpy(ctx->M, buf + bufcount, IPMI_SHA1_BLOCK_LEN);
          bufcount += IPMI_SHA1_BLOCK_LEN;
          _ipmi_sha1_update_digest(ctx);
        }
      
      ctx->mlen = buflen - bufcount;
      if (ctx->mlen > 0)
        memcpy(ctx->M, buf + bufcount, ctx->mlen);
      ctx->len += buflen;
    }
  else 
    {
      /* Not enough data to update, just copy in data */ 
      memcpy(ctx->M + ctx->mlen, buf, buflen); 
      ctx->mlen += buflen;
      ctx->len += buflen;
    }

  return buflen;
}

static void 
_ipmi_sha1_append_padding(ipmi_sha1_t *ctx) 
{
  u_int8_t lenpad[IPMI_SHA1_PADDING_LEN];
  u_int64_t len = ctx->len * 8;

  /* Must grab length buffer before update with pad data, we don't
   * want to update length with pad data.  Length is appended low
   * order byte first.  Note there are no endian issues here,
   * compiler is required to handle bitmasks and shifts correctly.
   */

  lenpad[0] = (len >> 56) & 0x00000000000000ff;
  lenpad[1] = (len >> 48) & 0x00000000000000ff;
  lenpad[2] = (len >> 40) & 0x00000000000000ff;
  lenpad[3] = (len >> 32) & 0x00000000000000ff;
  lenpad[4] = (len >> 24) & 0x00000000000000ff;
  lenpad[5] = (len >> 16) & 0x00000000000000ff;
  lenpad[6] = (len >> 8) & 0x00000000000000ff;
  lenpad[7] = (len) & 0x00000000000000ff;

  if (ctx->mlen >= 56)
    {
      /* Not enough space to hold 64 bit length of message */
      u_int8_t zeroes[IPMI_SHA1_BLOCK_LEN - IPMI_SHA1_PADDING_LEN];

      ipmi_sha1_update_data(ctx, padding, IPMI_SHA1_BLOCK_LEN - ctx->mlen);
      memset(zeroes, '\0', IPMI_SHA1_BLOCK_LEN - IPMI_SHA1_PADDING_LEN);
      ipmi_sha1_update_data(ctx, zeroes, 
                            IPMI_SHA1_BLOCK_LEN - IPMI_SHA1_PADDING_LEN);
    }
  else
    ipmi_sha1_update_data(ctx, padding, 
                          IPMI_SHA1_BLOCK_LEN - ctx->mlen - IPMI_SHA1_PADDING_LEN);
  ipmi_sha1_update_data(ctx, lenpad, IPMI_SHA1_PADDING_LEN);
}

int 
ipmi_sha1_finish(ipmi_sha1_t *ctx, u_int8_t *digest, unsigned int digestlen) 
{
  
  if (ctx == NULL || ctx->magic != IPMI_SHA1_MAGIC 
      || digest == NULL || digestlen < IPMI_SHA1_DIGEST_LEN) 
    {
      errno = EINVAL;
      return -1;
    }
  
  _ipmi_sha1_append_padding(ctx);

  /* Note there are no endian issues here, compiler is required to
   * handle bitmasks and shifts correctly
   */

  digest[0]  = (ctx->H[0] & 0xff000000) >> 24;
  digest[1]  = (ctx->H[0] & 0x00ff0000) >> 16;
  digest[2]  = (ctx->H[0] & 0x0000ff00) >> 8;
  digest[3]  = (ctx->H[0] & 0x000000ff);
  digest[4]  = (ctx->H[1] & 0xff000000) >> 24;
  digest[5]  = (ctx->H[1] & 0x00ff0000) >> 16;
  digest[6]  = (ctx->H[1] & 0x0000ff00) >> 8;
  digest[7]  = (ctx->H[1] & 0x000000ff);
  digest[8]  = (ctx->H[2] & 0xff000000) >> 24;
  digest[9]  = (ctx->H[2] & 0x00ff0000) >> 16;
  digest[10] = (ctx->H[2] & 0x0000ff00) >> 8;
  digest[11] = (ctx->H[2] & 0x000000ff);
  digest[12] = (ctx->H[3] & 0xff000000) >> 24;
  digest[13] = (ctx->H[3] & 0x00ff0000) >> 16;
  digest[14] = (ctx->H[3] & 0x0000ff00) >> 8;
  digest[15] = (ctx->H[3] & 0x000000ff);
  digest[16] = (ctx->H[4] & 0xff000000) >> 24;
  digest[17] = (ctx->H[4] & 0x00ff0000) >> 16;
  digest[18] = (ctx->H[4] & 0x0000ff00) >> 8;
  digest[19] = (ctx->H[4] & 0x000000ff); 

  ctx->magic = ~IPMI_SHA1_MAGIC;
  return IPMI_SHA1_DIGEST_LEN;
}

