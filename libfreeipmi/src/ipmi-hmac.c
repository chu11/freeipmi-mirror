/*****************************************************************************\
 *  $Id: ipmi-hmac.c,v 1.3 2005-10-06 10:41:09 balamurugan Exp $
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

#include "freeipmi.h"

#define IPMI_HMAC_MAGIC  0xabba00ba

typedef int (*H_INIT)(void *ctx);
typedef int (*H_UPDATE_DATA)(void *ctx, u_int8_t *buf, unsigned int buflen);
typedef int (*H_FINISH)(void *ctx, u_int8_t *digest, unsigned int digestlen);

typedef struct __hmac_h_info {
  H_INIT h_init;
  H_UPDATE_DATA h_update_data;
  H_FINISH h_finish;
  u_int32_t block_len;
  u_int32_t digest_len;
  void *ipad_ctx;
  void *opad_ctx;
} ipmi_hmac_h_info_t;

int 
ipmi_hmac_init(ipmi_hmac_t *ctx, ipmi_hmac_type_t type, u_int8_t *key, unsigned int keylen)
{
  u_int8_t *ipad = NULL;
  u_int8_t *opad = NULL;
  u_int8_t *hashed_key = NULL;
  int i, save_errno;
  ipmi_hmac_h_info_t *h_info = NULL;

  if (!ctx || !IPMI_HMAC_TYPE_VALID(type))
    {
      errno = EINVAL;
      return -1;
    }

  ctx->magic = IPMI_HMAC_MAGIC;
  
  ctx->h_info = (void *)malloc(sizeof(ipmi_hmac_h_info_t));
  if (!(ctx->h_info))
    goto cleanup;
  memset(ctx->h_info, '\0', sizeof(ipmi_hmac_h_info_t));
  h_info = (ipmi_hmac_h_info_t *)ctx->h_info;

  switch (type)
    {
    case IPMI_HMAC_SHA1:
      h_info->h_init = (H_INIT)ipmi_sha1_init;
      h_info->h_update_data = (H_UPDATE_DATA)ipmi_sha1_update_data;
      h_info->h_finish = (H_FINISH)ipmi_sha1_finish;
      h_info->block_len = IPMI_SHA1_BLOCK_LEN;
      h_info->digest_len = IPMI_SHA1_DIGEST_LEN;
      
      h_info->ipad_ctx = (void *)malloc(sizeof(ipmi_sha1_t));
      if (!(h_info->ipad_ctx))
        goto cleanup;
      memset(h_info->ipad_ctx, '\0', sizeof(ipmi_sha1_t));

      h_info->opad_ctx = (void *)malloc(sizeof(ipmi_sha1_t));
      if (!(h_info->opad_ctx))
        goto cleanup;
      memset(h_info->opad_ctx, '\0', sizeof(ipmi_sha1_t));

      break;
    case IPMI_HMAC_MD5:
      h_info->h_init = (H_INIT)ipmi_md5_init;
      h_info->h_update_data = (H_UPDATE_DATA)ipmi_md5_update_data;
      h_info->h_finish = (H_FINISH)ipmi_md5_finish;
      h_info->block_len = IPMI_MD5_BLOCK_LEN;
      h_info->digest_len = IPMI_MD5_DIGEST_LEN;

      h_info->ipad_ctx = (void *)malloc(sizeof(ipmi_md5_t));
      if (!(h_info->ipad_ctx))
        goto cleanup;
      memset(h_info->ipad_ctx, '\0', sizeof(ipmi_md5_t));

      h_info->opad_ctx = (void *)malloc(sizeof(ipmi_md5_t));
      if (!(h_info->opad_ctx))
        goto cleanup;
      memset(h_info->opad_ctx, '\0', sizeof(ipmi_md5_t));

      break;
    }

  ipad = (u_int8_t *)malloc(h_info->block_len + 1);
  if (!ipad)
    goto cleanup;
  memset(ipad, '\0', h_info->block_len + 1);

  opad = (u_int8_t *)malloc(h_info->block_len + 1);
  if (!opad)
    goto cleanup;
  memset(opad, '\0', h_info->block_len + 1);

  if ((h_info->h_init)(h_info->ipad_ctx) < 0)
    goto cleanup;

  if ((h_info->h_init)(h_info->opad_ctx) < 0)
    goto cleanup;
  
  if (keylen > h_info->block_len)
    {
      hashed_key = (u_int8_t *)malloc(h_info->digest_len + 1);
      if (!hashed_key)
        goto cleanup;
      memset(hashed_key, '\0', h_info->digest_len + 1);
      
      if ((h_info->h_update_data)(h_info->ipad_ctx,
                                  key,
                                  keylen) < 0)
        goto cleanup;
      
      if ((h_info->h_finish)(h_info->ipad_ctx,
                             hashed_key,
                             h_info->digest_len) < 0)
        goto cleanup;

      key = hashed_key;
      keylen = h_info->digest_len;

      /* re-init ipad context */
      if ((h_info->h_init)(h_info->ipad_ctx) < 0)
        goto cleanup;
    }

  for (i = 0; i < h_info->block_len; i++)
    {
      if (i < keylen) 
        {
          ipad[i] = key[i] ^ 0x36;
          opad[i] = key[i] ^ 0x5c;
        }
      else
        {
          ipad[i] ^= 0x36;
          opad[i] ^= 0x5c;
        }
    }

  if ((h_info->h_update_data)(h_info->ipad_ctx,
                              ipad,
                              h_info->block_len) < 0)
    goto cleanup;

  if ((h_info->h_update_data)(h_info->opad_ctx,
                              opad,
                              h_info->block_len) < 0)
    goto cleanup;
               
  free(ipad);
  free(opad);
  return 0;

 cleanup:
  save_errno = errno;
  if (ctx->h_info)
    {
      if (h_info->ipad_ctx)
        free(h_info->ipad_ctx);
      if (h_info->opad_ctx)
        free(h_info->opad_ctx);
      free(ctx->h_info);
    }
  if (ipad)
    free(ipad);
  if (opad)
    free(opad);
  if (hashed_key)
    free(hashed_key);
  errno = save_errno;
  return -1;
}

int 
ipmi_hmac_update_data(ipmi_hmac_t *ctx, u_int8_t *buf, unsigned int buflen) 
{
  ipmi_hmac_h_info_t *h_info;

  if (ctx == NULL || ctx->magic != IPMI_HMAC_MAGIC || buf == NULL) 
    {
      errno = EINVAL;
      return -1;
    }
  h_info = (ipmi_hmac_h_info_t *)ctx->h_info;

  if (buflen == 0)
    return 0;

  if ((h_info->h_update_data)(h_info->ipad_ctx,
                              buf,
                              buflen) < 0)
    return -1;

  return buflen;
}

int 
ipmi_hmac_finish(ipmi_hmac_t *ctx, u_int8_t *digest, unsigned int digestlen) 
{
  u_int8_t *idigest = NULL;
  int ret = -1;
  ipmi_hmac_h_info_t *h_info;

  if (ctx == NULL || ctx->magic != IPMI_HMAC_MAGIC 
      || digest == NULL
      || digestlen < ((ipmi_hmac_h_info_t *)ctx->h_info)->digest_len)
    {
      errno = EINVAL;
      return -1;
    }
  h_info = ctx->h_info;

  idigest = (u_int8_t *)malloc(h_info->digest_len + 1);
  if (!idigest)
    goto cleanup;
  memset(idigest, '\0', h_info->digest_len + 1);
      
  if ((h_info->h_finish)(h_info->ipad_ctx,
                         idigest,
                         h_info->digest_len) < 0)
    goto cleanup;
  
  if ((h_info->h_update_data)(h_info->opad_ctx,
                              idigest,
                              h_info->digest_len) < 0)
    goto cleanup_full;

  if ((h_info->h_finish)(h_info->opad_ctx,
                         digest,
                         digestlen) < 0)
    goto cleanup_full;

  ret = h_info->digest_len;
 cleanup_full:
  /* If cleanup_full is called, some context state was destroyed, so
   * the caller will have to start over 
   */
  ctx->magic = ~IPMI_HMAC_MAGIC;
  free(h_info->ipad_ctx);
  free(h_info->opad_ctx);
  free(ctx->h_info);
  ctx->h_info = 0;
 cleanup:
  if (idigest)
    free(idigest);
  return -1;
}
