/*****************************************************************************\
 *  $Id: md2.h,v 1.2 2006-02-25 02:44:00 chu11 Exp $
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

#ifndef _MD2_H
#define _MD2_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define MD2_BLOCK_LEN   16
#define MD2_BUFFER_LEN  48
#define MD2_CHKSUM_LEN  16
#define MD2_DIGEST_LEN  16
#define MD2_PADDING_LEN 16
#define MD2_ROUNDS_LEN  18

typedef struct __md2 {
  uint32_t magic;
  uint8_t l;
  unsigned int mlen;
  uint8_t x[MD2_BUFFER_LEN];
  uint8_t c[MD2_CHKSUM_LEN];
  uint8_t m[MD2_BLOCK_LEN];
} md2_t;

int md2_init(md2_t *ctx);

int md2_update_data(md2_t *ctx, uint8_t *buf, unsigned int buflen);

int md2_finish(md2_t *ctx, uint8_t *digest, unsigned int digestlen);

#ifdef __cplusplus
}
#endif

#endif /* _MD2_H */
