/*****************************************************************************\
 *  $Id: ipmi-md2.h,v 1.4 2005-12-16 08:48:40 ab Exp $
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

#ifndef _IPMI_MD2_H
#define _IPMI_MD2_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_MD2_BLOCK_LEN   16
#define IPMI_MD2_BUFFER_LEN  48
#define IPMI_MD2_CHKSUM_LEN  16
#define IPMI_MD2_DIGEST_LEN  16
#define IPMI_MD2_PADDING_LEN 16
#define IPMI_MD2_ROUNDS_LEN  18

typedef struct __md2 {
  uint32_t magic;
  uint8_t l;
  unsigned int mlen;
  uint8_t x[IPMI_MD2_BUFFER_LEN];
  uint8_t c[IPMI_MD2_CHKSUM_LEN];
  uint8_t m[IPMI_MD2_BLOCK_LEN];
} ipmi_md2_t;

int ipmi_md2_init(ipmi_md2_t *ctx);

int ipmi_md2_update_data(ipmi_md2_t *ctx, uint8_t *buf, unsigned int buflen);

int ipmi_md2_finish(ipmi_md2_t *ctx, uint8_t *digest, unsigned int digestlen);

#ifdef __cplusplus
}
#endif

#endif /* _IPMI_MD2_H */
