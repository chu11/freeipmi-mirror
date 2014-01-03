/*****************************************************************************\
 *  $Id: ipmi-md2.h,v 1.9 2010-02-08 22:09:40 chu11 Exp $
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

#ifndef IPMI_MD2_H
#define IPMI_MD2_H

#include <stdint.h>

#define MD2_BLOCK_LENGTH   16
#define MD2_BUFFER_LENGTH  48
#define MD2_CHKSUM_LENGTH  16
#define MD2_DIGEST_LENGTH  16
#define MD2_PADDING_LENGTH 16
#define MD2_ROUNDS_LENGTH  18

typedef struct __md2 {
  uint32_t magic;
  uint8_t l;
  unsigned int mlen;
  uint8_t x[MD2_BUFFER_LENGTH];
  uint8_t c[MD2_CHKSUM_LENGTH];
  uint8_t m[MD2_BLOCK_LENGTH];
} md2_t;

int md2_init (md2_t *ctx);

int md2_update_data (md2_t *ctx, const void *buf, unsigned int buflen);

int md2_finish (md2_t *ctx, void *digest, unsigned int digestlen);

#endif /* IPMI_MD2_H */
