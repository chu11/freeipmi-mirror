/* 
   ipmi-cipher-suite-spec.h - IPMI Network Function Specification

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#ifndef _IPMI_CIPHER_SUITE_SPEC_H
#define	_IPMI_CIPHER_SUITE_SPEC_H

#include <freeipmi/fiid.h>
#include <freeipmi/ipmi-rmcpplus.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Notes:
   Refer to IPMI 2.0 spec Table 22-18 and Table 22-19. 
*/

#define IPMI_CIPHER_SUITE_TAG_BITS_AUTHENTICATION_ALGORITHM   0x0
#define IPMI_CIPHER_SUITE_TAG_BITS_INTEGRITY_ALGORITHM        0x1
#define IPMI_CIPHER_SUITE_TAG_BITS_CONFIDENTIALITY_ALGORITHM  0x2
#define IPMI_CIPHER_SUITE_TAG_BITS_RECORD                     0x3

#define IPMI_CIPHER_SUITE_RECORD_FORMAT_STANDARD              0x00
#define IPMI_CIPHER_SUITE_RECORD_FORMAT_OEM                   0x01

#define IPMI_CIPHER_SUITE_RECORD_FORMAT_VALID(__val) \
  (((__val) == IPMI_CIPHER_SUITE_RECORD_FORMAT_STANDARD \
    || (__val) == IPMI_CIPHER_SUITE_RECORD_FORMAT_OEM) ? 1 : 0)

#define IPMI_CIPHER_SUITE_TAG_BITS_VALID(__val) \
  (((__val) == IPMI_CIPHER_SUITE_TAG_BITS_AUTHENTICATION_ALGORITHM \
    || (__val) == IPMI_CIPHER_SUITE_TAG_BITS_INTEGRITY_ALGORITHM \
    || (__val) == IPMI_CIPHER_SUITE_TAG_BITS_CONFIDENTIALITY_ALGORITHM \
    || (__val) == IPMI_CIPHER_SUITE_TAG_BITS_RECORD) ? 1 : 0)

extern fiid_template_t tmpl_cipher_suite_record_header;
extern fiid_template_t tmpl_cipher_suite_record;
extern fiid_template_t tmpl_oem_cipher_suite_record;

#define IPMI_CIPHER_SUITE_COMBINATION_VALID(__a, __i, __c) \
  ((((__a) == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE \
     && (__i) == IPMI_INTEGRITY_ALGORITHM_NONE \
     && (__c) == IPMI_CONFIDENTIALITY_ALGORITHM_NONE) \
    || ((__a) == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1 \
	&& ((__i) == IPMI_INTEGRITY_ALGORITHM_NONE \
	    && (__c) == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)) \
    || ((__a) == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1 \
	&& ((__i) == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96 \
	    && ((__c) == IPMI_CONFIDENTIALITY_ALGORITHM_NONE \
		|| (__c) == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128 \
		|| (__c) == IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_128 \
		|| (__c) == IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_40))) \
    || ((__a) == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5 \
	&& ((__i) == IPMI_INTEGRITY_ALGORITHM_NONE \
	    && (__c) == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)) \
    || ((__a) == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5 \
	&& ((__i) == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128 \
	    && ((__c) == IPMI_CONFIDENTIALITY_ALGORITHM_NONE \
		|| (__c) == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128 \
		|| (__c) == IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_128 \
		|| (__c) == IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_40))) \
    || ((__a) == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5 \
	&& ((__i) == IPMI_INTEGRITY_ALGORITHM_MD5_128 \
	    && ((__c) == IPMI_CONFIDENTIALITY_ALGORITHM_NONE \
		|| (__c) == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128 \
		|| (__c) == IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_128 \
		|| (__c) == IPMI_CONFIDENTIALITY_ALGORITHM_XRC4_40)))) ? 1 : 0)

#define IPMI_CIPHER_SUITE_COMBINATION_SUPPORTED(__a, __i, __c) \
  ((((__a) == IPMI_AUTHENTICATION_ALGORITHM_RAKP_NONE \
     && (__i) == IPMI_INTEGRITY_ALGORITHM_NONE \
     && (__c) == IPMI_CONFIDENTIALITY_ALGORITHM_NONE) \
    || ((__a) == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1 \
	&& ((__i) == IPMI_INTEGRITY_ALGORITHM_NONE \
	    && (__c) == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)) \
    || ((__a) == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_SHA1 \
	&& ((__i) == IPMI_INTEGRITY_ALGORITHM_HMAC_SHA1_96 \
	    && ((__c) == IPMI_CONFIDENTIALITY_ALGORITHM_NONE \
		|| (__c) == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128))) \
    || ((__a) == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5 \
	&& ((__i) == IPMI_INTEGRITY_ALGORITHM_NONE \
	    && (__c) == IPMI_CONFIDENTIALITY_ALGORITHM_NONE)) \
    || ((__a) == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5 \
	&& ((__i) == IPMI_INTEGRITY_ALGORITHM_HMAC_MD5_128 \
	    && ((__c) == IPMI_CONFIDENTIALITY_ALGORITHM_NONE \
		|| (__c) == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128))) \
    || ((__a) == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5 \
	&& ((__i) == IPMI_INTEGRITY_ALGORITHM_MD5_128 \
	    && ((__c) == IPMI_CONFIDENTIALITY_ALGORITHM_NONE \
		|| (__c) == IPMI_CONFIDENTIALITY_ALGORITHM_AES_CBC_128)))) ? 1 : 0)

#define IPMI_CIPHER_SUITE_ID_SUPPORTED(__id) \
   ((((__id) >= 0 && (__id) <= 3) \
     || ((__id) >= 6 && (__id) <= 8) \
     || ((__id) >= 11 && (__id) <= 12)) ? 1 : 0)

int8_t ipmi_cipher_suite_id_to_algorithms(uint8_t cipher_suite_id,
					  uint8_t *authentication_algorithm,
					  uint8_t *integrity_algorithm,
					  uint8_t *confidentiality_algorithm);

int8_t ipmi_algorithms_to_cipher_suite_id(uint8_t authentication_algorithm,
					  uint8_t integrity_algorithm,
					  uint8_t confidentiality_algorithm,
					  uint8_t *cipher_suite_id);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-cipher_suite-spec.h */
