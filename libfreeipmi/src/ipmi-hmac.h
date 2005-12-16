/*****************************************************************************\
 *  $Id: ipmi-hmac.h,v 1.4 2005-12-16 08:48:40 ab Exp $
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

#ifndef _IPMI_HMAC_H
#define _IPMI_HMAC_H

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
  IPMI_HMAC_SHA1 = 0,
  IPMI_HMAC_MD5 = 1
} ipmi_hmac_type_t;
#define IPMI_HMAC_TYPE_VALID(__t)    ((__t) >= IPMI_HMAC_SHA1 && \
                                      (__t) <= IPMI_HMAC_MD5)

typedef struct __hmac {
  uint32_t magic;
  void *h_info;
} ipmi_hmac_t;


int ipmi_hmac_init(ipmi_hmac_t *ctx, ipmi_hmac_type_t type, uint8_t *key, unsigned int keylen);

int ipmi_hmac_update_data(ipmi_hmac_t *ctx, uint8_t *buf, unsigned int buflen);

int ipmi_hmac_finish(ipmi_hmac_t *ctx, uint8_t *digest, unsigned int digestlen);

#ifdef __cplusplus
}
#endif

#endif /* _IPMI_HMAC_H */
