/*****************************************************************************\
 *  $Id: ipmi-md5.h,v 1.9 2010-02-08 22:09:40 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2015 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2003-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see https://savannah.gnu.org/projects/freeipmi/.
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

/* achu: Concern/question over license of openssl.  Other
 * implementations found online questionable.  End result was
 * re-implementation from scratch.
 */

#ifndef IPMI_MD5_H
#define IPMI_MD5_H

#include <stdint.h>

#define IPMI_MD5_BLOCK_LENGTH       64
#define IPMI_MD5_BLOCK_WORDS_LENGTH (IPMI_MD5_BLOCK_LENGTH/4)
#define IPMI_MD5_DIGEST_LENGTH      16

typedef struct __ipmi_md5 {
  uint32_t magic;
  unsigned int mlen;
  unsigned int bytes_mod_64;
  uint32_t bit_count[2];
  uint32_t a;
  uint32_t b;
  uint32_t c;
  uint32_t d;
  uint8_t m[IPMI_MD5_BLOCK_LENGTH];
} ipmi_md5_t;

int ipmi_md5_init (ipmi_md5_t *ctx);

int ipmi_md5_update_data (ipmi_md5_t *ctx, const void *buf, unsigned int buflen);

int ipmi_md5_finish (ipmi_md5_t *ctx, void *digest, unsigned int digestlen);

#endif /* IPMI_MD5_H */
