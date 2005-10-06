/*****************************************************************************\
 *  $Id: ipmi-sha1.h,v 1.2 2005-10-06 10:41:10 balamurugan Exp $
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

#ifndef _IPMI_SHA1_H
#define _IPMI_SHA1_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_SHA1_BLOCK_LEN    64
#define IPMI_SHA1_DIGEST_LEN   20
#define IPMI_SHA1_DIGEST_WORDS  5
#define IPMI_SHA1_PADDING_LEN   8
#define IPMI_SHA1_ROUNDS_LEN   80

typedef struct __sha1 {
  u_int32_t magic;
  u_int64_t len;
  unsigned int mlen;
  u_int32_t H[IPMI_SHA1_DIGEST_WORDS];
  u_int8_t M[IPMI_SHA1_BLOCK_LEN];
} ipmi_sha1_t;

int ipmi_sha1_init(ipmi_sha1_t *ctx);

int ipmi_sha1_update_data(ipmi_sha1_t *ctx, u_int8_t *buf, unsigned int buflen);

int ipmi_sha1_finish(ipmi_sha1_t *ctx, u_int8_t *digest, unsigned int digestlen);

#ifdef __cplusplus
}
#endif

#endif /* _IPMI_SHA1_H */
