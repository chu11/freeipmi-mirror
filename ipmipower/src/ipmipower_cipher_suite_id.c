/*****************************************************************************\
 *  $Id: ipmipower_cipher_suite_id.c,v 1.1.2.1 2007-07-24 00:59:44 chu11 Exp $
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
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>

#include "ipmipower_cipher_suite_id.h"
#include "ipmipower_wrappers.h"

cipher_suite_id_t 
ipmipower_cipher_suite_id_index(char *str)
{
  assert(str != NULL);

  if (!strcasecmp(str, "auto"))
    return CIPHER_SUITE_ID_AUTO;
  else if (!strcasecmp(str, "0"))
    return CIPHER_SUITE_ID_0;
  else if (!strcasecmp(str, "1"))
    return CIPHER_SUITE_ID_1;
  else if (!strcasecmp(str, "2"))
    return CIPHER_SUITE_ID_2;
  else if (!strcasecmp(str, "3"))
    return CIPHER_SUITE_ID_3;
  else if (!strcasecmp(str, "6"))
    return CIPHER_SUITE_ID_6;
  else if (!strcasecmp(str, "7"))
    return CIPHER_SUITE_ID_7;
  else if (!strcasecmp(str, "8"))
    return CIPHER_SUITE_ID_8;
  else if (!strcasecmp(str, "11"))
    return CIPHER_SUITE_ID_11;
  else if (!strcasecmp(str, "12"))
    return CIPHER_SUITE_ID_12;
  else 
    return CIPHER_SUITE_ID_INVALID;
}

char *
ipmipower_cipher_suite_id_string(cipher_suite_id_t id)
{
  assert(CIPHER_SUITE_ID_VALID_OR_AUTO(id));

  switch(id) 
    {
    case CIPHER_SUITE_ID_AUTO:
      return "auto";
      break;
    case CIPHER_SUITE_ID_0:
      return "0";
      break;
      /* XXX */
    case CIPHER_SUITE_ID_1:
      return "1";
      break;
    case CIPHER_SUITE_ID_2:
      return "2";
      break;
    case CIPHER_SUITE_ID_3:
      return "3";
      break;
    case CIPHER_SUITE_ID_6:
      return "6";
      break;
    case CIPHER_SUITE_ID_7:
      return "7";
      break;
    case CIPHER_SUITE_ID_8:
      return "8";
      break;
    case CIPHER_SUITE_ID_11:
      return "11";
      break;
    case CIPHER_SUITE_ID_12:
      return "12";
      break;
    default:
      err_exit("ipmipower_cipher_suite_id_string: Invalid cipher_suite_id: %d\n", id);
    }
  
  return NULL;                  /* NOT_REACHED */
}

char *
ipmipower_cipher_suite_id_description(cipher_suite_id_t id)
{
  assert(CIPHER_SUITE_ID_VALID_OR_AUTO(id));

  switch(id) 
    {
    case CIPHER_SUITE_ID_AUTO:
      return "auto";
      break;
    case CIPHER_SUITE_ID_0:
      return "Authentication Algorithm = None; Integrity Algorithm = None; Confidentiality Algorithm = None";
      break;
    case CIPHER_SUITE_ID_1:
      return "Authentication Algorithm = HMAC-SHA1; Integrity Algorithm = None; Confidentiality Algorithm = None";
      break;
    case CIPHER_SUITE_ID_2:
      return "Authentication Algorithm = HMAC-SHA1; Integrity Algorithm = HMAC-SHA1-96; Confidentiality Algorithm = None";
      break;
    case CIPHER_SUITE_ID_3:
      return "Authentication Algorithm = HMAC-SHA1; Integrity Algorithm = HMAC-SHA1-96; Confidentiality Algorithm = AES-CBC-128";
      break;
    case CIPHER_SUITE_ID_6:
      return "Authentication Algorithm = HMAC-MD5; Integrity Algorithm = None; Confidentiality Algorithm = None";
      break;
    case CIPHER_SUITE_ID_7:
      return "Authentication Algorithm = HMAC-MD5; Integrity Algorithm = HMAC-MD5-128; Confidentiality Algorithm = None";
      break;
    case CIPHER_SUITE_ID_8:
      return "Authentication Algorithm = HMAC-MD5; Integrity Algorithm = HMAC-MD5-128; Confidentiality Algorithm = AES-CBC-128";
      break;
    case CIPHER_SUITE_ID_11:
      return "Authentication Algorithm = HMAC-MD5; Integrity Algorithm = MD5-128; Confidentiality Algorithm = None";
      break;
    case CIPHER_SUITE_ID_12:
      return "Authentication Algorithm = HMAC-MD5; Integrity Algorithm = MD5-128; Confidentiality Algorithm = AES-CBC-128";
      break;
    default:
      err_exit("ipmipower_cipher_suite_id_description: Invalid cipher_suite_id: %d\n", id);
    }
  
  return NULL;                  /* NOT_REACHED */
}

char *
ipmipower_cipher_suite_id_list(void)
{
  return "auto, 0, 1, 2, 3, 6, 7, 8, 11, 12";
}

uint8_t
ipmipower_ipmi_cipher_suite_id(cipher_suite_id_t id)
{
  assert(CIPHER_SUITE_ID_VALID(id));

  switch(id) 
    {
    case CIPHER_SUITE_ID_0:
      return 0;
      break;
    case CIPHER_SUITE_ID_1:
      return 1;
      break;
      /* XXX */
    case CIPHER_SUITE_ID_2:
      return 2;
      break;
    case CIPHER_SUITE_ID_3:
      return 3;
      break;
    case CIPHER_SUITE_ID_6:
      return 6;
      break;
    case CIPHER_SUITE_ID_7:
      return 7;
      break;
    case CIPHER_SUITE_ID_8:
      return 8;
      break;
    case CIPHER_SUITE_ID_11:
      return 11;
      break;
    case CIPHER_SUITE_ID_12:
      return 12;
      break;
    default:
      err_exit("ipmipower_ipmi_cipher_suite_id: Invalid cipher_suite_id: %d\n", id);
    }
  
  return 0;                  /* NOT_REACHED */
}
