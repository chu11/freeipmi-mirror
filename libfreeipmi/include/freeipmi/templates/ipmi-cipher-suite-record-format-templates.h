/*
 * Copyright (C) 2003-2011 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef _IPMI_CIPHER_SUITE_RECORD_FORMAT_TEMPLATES_H
#define _IPMI_CIPHER_SUITE_RECORD_FORMAT_TEMPLATES_H

#ifdef __cplusplus
extern "C" {
#endif

/* This header file is for documentation only */

#if 0

Format = { bits, "field name", field flags }

FIID_FIELD_REQUIRED - field is required for the payload
FIID_FIELD_OPTIONAL - field is optional for the payload

FIID_FIELD_LENGTH_FIXED - field length is fixed at the number of bits listed
FIID_FIELD_LENGTH_VARIABLE - field length is variable for the number of bits listed

Cipher Suite Record Header
--------------------------

fiid_template_t tmpl_cipher_suite_record_header =
  {
    { 6, "record_format", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "tag_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

Cipher Suite Record
-------------------

fiid_template_t tmpl_cipher_suite_record =
  {
    { 6, "record_format", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "tag_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "cipher_suite_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "authentication_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "authentication_algorithm_tag_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "integrity_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "integrity_algorithm_tag_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "confidentiality_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "confidentiality_algorithm_tag_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

OEM Cipher Suite Record
-----------------------

fiid_template_t tmpl_oem_cipher_suite_record =
  {
    { 6, "record_format", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "tag_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 8, "oem_cipher_suite_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 24, "oem_iana", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "authentication_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "authentication_algorithm_tag_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "integrity_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "integrity_algorithm_tag_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 6, "confidentiality_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 2, "confidentiality_algorithm_tag_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    { 0, "", 0}
  };

#endif  /* 0 */

#ifdef __cplusplus
}
#endif

#endif  /* _IPMI_CIPHER_SUITE_RECORD_FORMAT_TEMPLATES_H */
