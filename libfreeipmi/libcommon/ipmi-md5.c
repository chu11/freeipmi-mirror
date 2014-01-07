/*****************************************************************************\
 *  $Id: ipmi-md5.c,v 1.17 2010-02-08 22:09:40 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2003-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmipower is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmipower is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmipower.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <sys/types.h>
#include <errno.h>

#include "ipmi-md5.h"

#include "freeipmi-portability.h"

static uint8_t padding[64] =
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

/*
  achu: You can generate this table using the following code snippet.

  for (i = 1; i <= 64; i++)
  {
    double n = 4294967296 * ((sin(i) > 0) ? sin(i) : (sin(i) * -1));
    printf("0x%08X\n", (uint32_t)n);
  }
*/
static uint32_t T[64] =
  {
    0xD76AA478, 0xE8C7B756, 0x242070DB, 0xC1BDCEEE,
    0xF57C0FAF, 0x4787C62A, 0xA8304613, 0xFD469501,
    0x698098D8, 0x8B44F7AF, 0xFFFF5BB1, 0x895CD7BE,
    0x6B901122, 0xFD987193, 0xA679438E, 0x49B40821,
    0xF61E2562, 0xC040B340, 0x265E5A51, 0xE9B6C7AA,
    0xD62F105D, 0x02441453, 0xD8A1E681, 0xE7D3FBC8,
    0x21E1CDE6, 0xC33707D6, 0xF4D50D87, 0x455A14ED,
    0xA9E3E905, 0xFCEFA3F8, 0x676F02D9, 0x8D2A4C8A,
    0xFFFA3942, 0x8771F681, 0x6D9D6122, 0xFDE5380C,
    0xA4BEEA44, 0x4BDECFA9, 0xF6BB4B60, 0xBEBFBC70,
    0x289B7EC6, 0xEAA127FA, 0xD4EF3085, 0x04881D05,
    0xD9D4D039, 0xE6DB99E5, 0x1FA27CF8, 0xC4AC5665,
    0xF4292244, 0x432AFF97, 0xAB9423A7, 0xFC93A039,
    0x655B59C3, 0x8F0CCC92, 0xFFEFF47D, 0x85845DD1,
    0x6FA87E4F, 0xFE2CE6E0, 0xA3014314, 0x4E0811A1,
    0xF7537E82, 0xBD3AF235, 0x2AD7D2BB, 0xEB86D391
  };

#define A               ctx->a
#define B               ctx->b
#define C               ctx->c
#define D               ctx->d
#define M               ctx->m
#define Mlen            ctx->mlen
#define MD5_MAGIC       0xfb0fdb0d

#define F(x,y,z)  (((x) & (y)) | ((~(x)) & (z)))
#define G(x,y,z)  (((x) & (z)) | ((y) & (~(z))))
#define H(x,y,z)  ((x) ^ (y) ^ (z))
#define I(x,y,z)  ((y) ^ ((x) | (~(z))))

#define CSHIFTL(x, n) (((x) << (n)) | ((x) >> (32 - (n))))

/* achu: The RFC specification accesses elements of T from 1 to 64,
 * but we index from 0 to 63.  So we need to subtract 1 from the T
 * array index.
 */
#define R1(a, b, c, d, k, s, i)                                              \
  do {                                                                       \
    (a) = ((b) + CSHIFTL (((a) + F ((b),(c),(d)) + X[(k)] + T[(i-1)]),(s))); \
  } while (0)

#define R2(a, b, c, d, k, s, i)                                              \
  do {                                                                       \
    (a) = ((b) + CSHIFTL (((a) + G ((b),(c),(d)) + X[(k)] + T[(i-1)]),(s))); \
  } while (0)

#define R3(a, b, c, d, k, s, i)                                              \
  do {                                                                       \
    (a) = ((b) + CSHIFTL (((a) + H ((b),(c),(d)) + X[(k)] + T[(i-1)]),(s))); \
  } while (0)

#define R4(a, b, c, d, k, s, i)                                              \
  do {                                                                       \
    (a) = ((b) + CSHIFTL (((a) + I ((b),(c),(d)) + X[(k)] + T[(i-1)]),(s))); \
  } while (0)

int
md5_init (md5_t *ctx)
{
  if (ctx == NULL)
    {
      errno = EINVAL;
      return (-1);
    }

  ctx->magic = MD5_MAGIC;

  Mlen = 0;
  ctx->bytes_mod_64 = 0;
  ctx->bit_count[0] = 0;
  ctx->bit_count[1] = 0;
  memset (M, '\0', MD5_BLOCK_LENGTH);

  /* initial values are listed low-order byte first */
  A = 0x67452301;
  B = 0xEFCDAB89;
  C = 0x98BADCFE;
  D = 0x10325476;

  return (0);
}

static void
_md5_update_digest (md5_t *ctx)
{
  uint32_t AA, BB, CC, DD;
  uint32_t X[MD5_BLOCK_WORDS_LENGTH];
  unsigned int j;

  /* Note there are no endian issues here, compiler is required to
   * handle shifts correctly
   */
  for (j = 0; j < MD5_BLOCK_WORDS_LENGTH; j++)
    X[j] = ((uint32_t)M[j*4]
            | ((uint32_t)M[j*4+1] << 8)
            | ((uint32_t)M[j*4+2] << 16)
            | ((uint32_t)M[j*4+3] << 24));

  AA = A;
  BB = B;
  CC = C;
  DD = D;

  /* Round 1 */
  R1 (A, B, C, D,  0,  7,  1);
  R1 (D, A, B, C,  1, 12,  2);
  R1 (C, D, A, B,  2, 17,  3);
  R1 (B, C, D, A,  3, 22,  4);
  R1 (A, B, C, D,  4,  7,  5);
  R1 (D, A, B, C,  5, 12,  6);
  R1 (C, D, A, B,  6, 17,  7);
  R1 (B, C, D, A,  7, 22,  8);
  R1 (A, B, C, D,  8,  7,  9);
  R1 (D, A, B, C,  9, 12, 10);
  R1 (C, D, A, B, 10, 17, 11);
  R1 (B, C, D, A, 11, 22, 12);
  R1 (A, B, C, D, 12,  7, 13);
  R1 (D, A, B, C, 13, 12, 14);
  R1 (C, D, A, B, 14, 17, 15);
  R1 (B, C, D, A, 15, 22, 16);

  /* Round 2 */
  R2 (A, B, C, D,  1,  5, 17);
  R2 (D, A, B, C,  6,  9, 18);
  R2 (C, D, A, B, 11, 14, 19);
  R2 (B, C, D, A,  0, 20, 20);
  R2 (A, B, C, D,  5,  5, 21);
  R2 (D, A, B, C, 10,  9, 22);
  R2 (C, D, A, B, 15, 14, 23);
  R2 (B, C, D, A,  4, 20, 24);
  R2 (A, B, C, D,  9,  5, 25);
  R2 (D, A, B, C, 14,  9, 26);
  R2 (C, D, A, B,  3, 14, 27);
  R2 (B, C, D, A,  8, 20, 28);
  R2 (A, B, C, D, 13,  5, 29);
  R2 (D, A, B, C,  2,  9, 30);
  R2 (C, D, A, B,  7, 14, 31);
  R2 (B, C, D, A, 12, 20, 32);

  /* Round 3 */
  R3 (A, B, C, D,  5,  4, 33);
  R3 (D, A, B, C,  8, 11, 34);
  R3 (C, D, A, B, 11, 16, 35);
  R3 (B, C, D, A, 14, 23, 36);
  R3 (A, B, C, D,  1,  4, 37);
  R3 (D, A, B, C,  4, 11, 38);
  R3 (C, D, A, B,  7, 16, 39);
  R3 (B, C, D, A, 10, 23, 40);
  R3 (A, B, C, D, 13,  4, 41);
  R3 (D, A, B, C,  0, 11, 42);
  R3 (C, D, A, B,  3, 16, 43);
  R3 (B, C, D, A,  6, 23, 44);
  R3 (A, B, C, D,  9,  4, 45);
  R3 (D, A, B, C, 12, 11, 46);
  R3 (C, D, A, B, 15, 16, 47);
  R3 (B, C, D, A,  2, 23, 48);

  /* Round 4 */
  R4 (A, B, C, D,  0,  6, 49);
  R4 (D, A, B, C,  7, 10, 50);
  R4 (C, D, A, B, 14, 15, 51);
  R4 (B, C, D, A,  5, 21, 52);
  R4 (A, B, C, D, 12,  6, 53);
  R4 (D, A, B, C,  3, 10, 54);
  R4 (C, D, A, B, 10, 15, 55);
  R4 (B, C, D, A,  1, 21, 56);
  R4 (A, B, C, D,  8,  6, 57);
  R4 (D, A, B, C, 15, 10, 58);
  R4 (C, D, A, B,  6, 15, 59);
  R4 (B, C, D, A, 13, 21, 60);
  R4 (A, B, C, D,  4,  6, 61);
  R4 (D, A, B, C, 11, 10, 62);
  R4 (C, D, A, B,  2, 15, 63);
  R4 (B, C, D, A,  9, 21, 64);

  A = A + AA;
  B = B + BB;
  C = C + CC;
  D = D + DD;
}

static void
_md5_update_count (md5_t *ctx, unsigned int buflen)
{

  /* Use two uint32_t integers to hold our 64 bit count.
   * bit_count[1] holds the 4 lower order bytes.  bit_count[0] holds
   * the 4 higher order bytes.
   */

  ctx->bit_count[1] += buflen * 8;

  /* Account for bit overflow */
  if (ctx->bit_count[1] < (buflen * 8))
    ctx->bit_count[0]++;

  /* Account for overflow due to multiplication by 8 */
  ctx->bit_count[0] += (buflen >> 29);

  ctx->bytes_mod_64 = ((buflen % 64) + ctx->bytes_mod_64) % 64;
}

int
md5_update_data (md5_t *ctx, const void *buf, unsigned int buflen)
{

  if (ctx == NULL || ctx->magic != MD5_MAGIC || buf == NULL)
    {
      errno = EINVAL;
      return (-1);
    }

  if (buflen == 0)
    return (0);

  if ((Mlen + buflen) >= MD5_BLOCK_LENGTH)
    {
      unsigned int bufcount;

      bufcount = (MD5_BLOCK_LENGTH - Mlen);
      memcpy (M + Mlen, buf, bufcount);
      _md5_update_digest (ctx);
      _md5_update_count (ctx, bufcount);

      while ((buflen - bufcount) >= MD5_BLOCK_LENGTH)
        {
          memcpy (M, buf + bufcount, MD5_BLOCK_LENGTH);
          bufcount += MD5_BLOCK_LENGTH;
          _md5_update_digest (ctx);
          _md5_update_count (ctx, MD5_BLOCK_LENGTH);
        }

      Mlen = buflen - bufcount;
      if (Mlen > 0)
        {
          memcpy (M, buf + bufcount, Mlen);
          _md5_update_count (ctx, Mlen);
        }
    }
  else
    {
      /* Not enough data to update digest, just copy in data */
      memcpy (M + Mlen, buf, buflen);
      Mlen += buflen;
      _md5_update_count (ctx, buflen);
    }

  return (buflen);
}

static void
_md5_append_padding_and_length (md5_t *ctx)
{
  unsigned int padlen;
  char length[8];

  if (ctx->bytes_mod_64 == 56)
    padlen = 64;
  else if (ctx->bytes_mod_64 < 56)
    padlen = 56 - ctx->bytes_mod_64;
  else
    padlen = 56 + (64 - ctx->bytes_mod_64);

  /* Must grab length buffer before update with pad data, we don't
   * want to update length with pad data.  Length is appended low
   * order byte first.  Note there are no endian issues here,
   * compiler is required to handle bitmasks and shifts correctly.
   */

  length[0] = (ctx->bit_count[1] & 0x000000ff);
  length[1] = (ctx->bit_count[1] & 0x0000ff00) >> 8;
  length[2] = (ctx->bit_count[1] & 0x00ff0000) >> 16;
  length[3] = (ctx->bit_count[1] & 0xff000000) >> 24;

  length[4] = (ctx->bit_count[0] & 0x000000ff);
  length[5] = (ctx->bit_count[0] & 0x0000ff00) >> 8;
  length[6] = (ctx->bit_count[0] & 0x00ff0000) >> 16;
  length[7] = (ctx->bit_count[0] & 0xff000000) >> 24;

  md5_update_data (ctx, padding, padlen);
  md5_update_data (ctx, length, 8);
}

int
md5_finish (md5_t *ctx, void *digest, unsigned int digestlen)
{
  uint8_t buf[MD5_DIGEST_LENGTH];

  if (ctx == NULL || ctx->magic != MD5_MAGIC
      || digest == NULL || digestlen < MD5_DIGEST_LENGTH)
    {
      errno = EINVAL;
      return (-1);
    }

  _md5_append_padding_and_length (ctx);

  /* Note there are no endian issues here, compiler is required to
   * handle bitmasks and shifts correctly
   */
  
  buf[0]  = (A & 0x000000ff);
  buf[1]  = (A & 0x0000ff00) >> 8;
  buf[2]  = (A & 0x00ff0000) >> 16;
  buf[3]  = (A & 0xff000000) >> 24;
  buf[4]  = (B & 0x000000ff);
  buf[5]  = (B & 0x0000ff00) >> 8;
  buf[6]  = (B & 0x00ff0000) >> 16;
  buf[7]  = (B & 0xff000000) >> 24;
  buf[8]  = (C & 0x000000ff);
  buf[9]  = (C & 0x0000ff00) >> 8;
  buf[10] = (C & 0x00ff0000) >> 16;
  buf[11] = (C & 0xff000000) >> 24;
  buf[12] = (D & 0x000000ff);
  buf[13] = (D & 0x0000ff00) >> 8;
  buf[14] = (D & 0x00ff0000) >> 16;
  buf[15] = (D & 0xff000000) >> 24;

  memcpy (digest, buf, MD5_DIGEST_LENGTH);
  ctx->magic = ~MD5_MAGIC;
  return (MD5_DIGEST_LENGTH);
}
