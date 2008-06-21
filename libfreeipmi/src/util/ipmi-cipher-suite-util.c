/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>

#include "freeipmi/util/ipmi-cipher-suite-util.h"
#include "freeipmi/record-format/ipmi-cipher-suite-record-format.h"

#include "libcommon/ipmi-err-wrappers.h"

#include "freeipmi-portability.h"

int8_t 
ipmi_cipher_suite_id_to_algorithms(uint8_t cipher_suite_id,
				   uint8_t *authentication_algorithm,
				   uint8_t *integrity_algorithm,
				   uint8_t *confidentiality_algorithm)
{
  uint8_t a, i, c;

  /* To avoid gcc warnings, add +1 to comparison */
  ERR_EINVAL((cipher_suite_id + 1) >= 1
	     && (cipher_suite_id + 1) <= 15);

  if (cipher_suite_id == 0)
    a = IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE;
  else if (cipher_suite_id >= 1 && cipher_suite_id <= 5)
    a = IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1;
  else /* cipher_suite_id >= 6 && cipher_suite_id <= 14 */
    a = IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5;

  if (cipher_suite_id == 0
      || cipher_suite_id == 1
      || cipher_suite_id == 6)
    i = IPMI_INTEGRITY_ALGORITHM_NONE;
  else if (cipher_suite_id >= 2 && cipher_suite_id <= 5)
    i = IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96;
  else if (cipher_suite_id >= 7 && cipher_suite_id <= 10)
    i = IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128;
  else /* cipher_suite_id >= 11 && cipher_suite_id <= 14 */
    i = IPMI_INTEGRITY_ALGORITHM_MD5_128;

  if (cipher_suite_id == 0
      || cipher_suite_id == 1
      || cipher_suite_id == 2
      || cipher_suite_id == 6
      || cipher_suite_id == 7
      || cipher_suite_id == 11)
    c = IPMI_CONFIDENTIALITY_ALGORITHM_NONE;
  else if (cipher_suite_id == 3
	   || cipher_suite_id == 8
	   || cipher_suite_id == 12)
    c = IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128;
  else if (cipher_suite_id == 4
	   || cipher_suite_id == 9
	   || cipher_suite_id == 13)
    c = IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_128;
  else /* cipher_suite_id == 5
	  || cipher_suite_id == 10
	  || cipher_suite_id == 14 */
    c = IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_40;

  if (authentication_algorithm)
    *authentication_algorithm = a;
  if (integrity_algorithm)
    *integrity_algorithm = i;
  if (confidentiality_algorithm)
    *confidentiality_algorithm = c;

  return (0);
}

int8_t 
ipmi_algorithms_to_cipher_suite_id(uint8_t authentication_algorithm,
				   uint8_t integrity_algorithm,
				   uint8_t confidentiality_algorithm,
				   uint8_t *cipher_suite_id)
{
  ERR_EINVAL(IPMI_AUTHENTICATION_ALGORITHM_VALID(authentication_algorithm)
	     && IPMI_INTEGRITY_ALGORITHM_VALID(integrity_algorithm)
	     && IPMI_CONFIDENTIALITY_ALGORITHM_VALID(confidentiality_algorithm)
	     && IPMI_CIPHER_SUITE_COMBINATION_VALID(authentication_algorithm,
						    integrity_algorithm,
						    confidentiality_algorithm)
	     && cipher_suite_id);

  if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE
      && integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE
      && confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
    *cipher_suite_id = 0;
  else if (authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1)
    {
      if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE
	  && confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
	*cipher_suite_id = 1;
      else /* integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96) */
	{
	  if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
	    *cipher_suite_id = 2;
	  else if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
	    *cipher_suite_id = 3;
	  else if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_128)
	    *cipher_suite_id = 4;
	  else /* confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_40 */
	    *cipher_suite_id = 5;
	}
    }
  else /* authentication_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128 */
    {
      if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_NONE
	  && confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
	*cipher_suite_id = 6;
      else if (integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128)
	{
	  if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
	    *cipher_suite_id = 7;
	  else if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
	    *cipher_suite_id = 8;
	  else if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_128)
	    *cipher_suite_id = 9;
	  else /* confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_40 */
	    *cipher_suite_id = 10;
	}
      else /* integrity_algorithm == IPMI_INTEGRITY_ALGORITHM_MD5_128 */
	{
	  if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)
	    *cipher_suite_id = 11;
	  else if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)
	    *cipher_suite_id = 12;
	  else if (confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_128)
	    *cipher_suite_id = 13;
	  else /* confidentiality_algorithm == IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_40 */
	    *cipher_suite_id = 14;
	}
    }
  
  return (0);
}
