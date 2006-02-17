/*****************************************************************************\
 *  $Id: ipmi-md5.h,v 1.6 2006-02-17 19:34:34 chu11 Exp $
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

#ifndef _IPMI_MD5_H
#define _IPMI_MD5_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_MD5_BLOCK_LEN       64
#define IPMI_MD5_BLOCK_WORDS_LEN (IPMI_MD5_BLOCK_LEN/4)
#define IPMI_MD5_DIGEST_LEN      16

typedef struct __md5 {
  uint32_t magic;
  unsigned int mlen;
  unsigned int bytes_mod_64;
  uint32_t bit_count[2];
  uint32_t a;
  uint32_t b;
  uint32_t c;
  uint32_t d;
  uint8_t m[IPMI_MD5_BLOCK_LEN];
} ipmi_md5_t;

int ipmi_md5_init(ipmi_md5_t *ctx);

int ipmi_md5_update_data(ipmi_md5_t *ctx, uint8_t *buf, unsigned int buflen);

int ipmi_md5_finish(ipmi_md5_t *ctx, uint8_t *digest, unsigned int digestlen);

#ifdef __cplusplus
}
#endif

#endif /* _IPMI_MD5_H */
