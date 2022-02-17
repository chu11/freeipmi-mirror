/*****************************************************************************\
 *  $Id: ipmi-md2.h,v 1.9 2010-02-08 22:09:40 chu11 Exp $
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

#ifndef IPMI_MD2_H
#define IPMI_MD2_H

#include <stdint.h>

#define IPMI_MD2_BLOCK_LENGTH   16
#define IPMI_MD2_BUFFER_LENGTH  48
#define IPMI_MD2_CHKSUM_LENGTH  16
#define IPMI_MD2_DIGEST_LENGTH  16
#define IPMI_MD2_PADDING_LENGTH 16
#define IPMI_MD2_ROUNDS_LENGTH  18

typedef struct __ipmi_md2 {
  uint32_t magic;
  uint8_t l;
  unsigned int mlen;
  uint8_t x[IPMI_MD2_BUFFER_LENGTH];
  uint8_t c[IPMI_MD2_CHKSUM_LENGTH];
  uint8_t m[IPMI_MD2_BLOCK_LENGTH];
} ipmi_md2_t;

int ipmi_md2_init (ipmi_md2_t *ctx);

int ipmi_md2_update_data (ipmi_md2_t *ctx, const void *buf, unsigned int buflen);

int ipmi_md2_finish (ipmi_md2_t *ctx, void *digest, unsigned int digestlen);

#endif /* IPMI_MD2_H */
