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

#include "freeipmi/record-format/ipmi-cipher-suite-record-format.h"

#include "libcommon/ipmi-err-wrappers.h"

#include "freeipmi-portability.h"

fiid_template_t tmpl_cipher_suite_record_header =
  {
    {6, "record_format", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "tag_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_cipher_suite_record = 
  {
    {6, "record_format", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "tag_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8, "cipher_suite_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "authentication_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "authentication_algorithm_tag_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "integrity_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "integrity_algorithm_tag_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "confidentiality_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "confidentiality_algorithm_tag_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

fiid_template_t tmpl_oem_cipher_suite_record = 
  {
    {6,  "record_format", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2,  "tag_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {8,  "oem_cipher_suite_id", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {24, "oem_iana", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "authentication_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "authentication_algorithm_tag_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "integrity_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "integrity_algorithm_tag_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {6, "confidentiality_algorithm", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {2, "confidentiality_algorithm_tag_bits", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };
