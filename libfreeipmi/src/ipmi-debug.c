/*
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
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <errno.h>

#include "freeipmi/ipmi-debug.h"

#include "ipmi-debug-common.h"

#include "ipmi-common.h"
#include "ipmi-err-wrappers.h"
#include "ipmi-fiid-wrappers.h"

#include "bit-ops.h"
#include "freeipmi-portability.h"

#define IPMI_DEBUG_MAX_BUF_LEN        65536
#define IPMI_DEBUG_MAX_PKT_LEN        65536
#define IPMI_DEBUG_CHAR_PER_LINE          8
#define IPMI_DEBUG_DEFAULT_FD STDERR_FILENO

#define IPMI_DEBUG_MAX_UNEXPECTED_BYTES 65536
#define IPMI_DEBUG_MAX_UNEXPECTED_BITS  (IPMI_DEBUG_MAX_UNEXPECTED_BYTES*8)

fiid_template_t tmpl_unexpected_data =
  {
    {IPMI_DEBUG_MAX_UNEXPECTED_BITS, "unexpected_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

int8_t
ipmi_obj_dump_perror (int fd, char *prefix, char *hdr, char *trlr, fiid_obj_t obj)
{
  char prefix_buf[IPMI_DEBUG_MAX_PREFIX_LEN];
  fiid_iterator_t iter = NULL;

  ERR_EINVAL (fiid_obj_valid(obj));
  
  ERR(!(ipmi_debug_set_prefix (prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN, prefix) < 0));

  ERR(!(ipmi_debug_output_str (fd, prefix_buf, hdr) < 0));

  FIID_ITERATOR_CREATE (iter, obj);

  while (!fiid_iterator_end(iter))
    {
      int32_t field_len;
      char *key;

      FIID_ITERATOR_KEY_CLEANUP(key, iter);

      FIID_ITERATOR_FIELD_LEN_CLEANUP(field_len, iter);
    
      if (!field_len)
        {
          fiid_iterator_next(iter);
          continue;
        }

      if (prefix)
        IPMI_DEBUG_DPRINTF_CLEANUP ((fd, "%s", prefix));

      if (field_len <= 64)
        {
          uint64_t val = 0;

	  FIID_ITERATOR_GET_CLEANUP (iter, &val);

          IPMI_DEBUG_DPRINTF_CLEANUP ((fd, "[%16LXh] = %s[%2db]\n", (uint64_t) val, key, field_len));
        }
      else
        {
          uint8_t buf[IPMI_DEBUG_MAX_BUF_LEN];
          int len;

          IPMI_DEBUG_DPRINTF_CLEANUP ((fd, "[  BYTE ARRAY ... ] = %s[%2dB]\n", key, BITS_ROUND_BYTES(field_len)));

	  FIID_ITERATOR_GET_DATA_LEN_CLEANUP(len, iter, buf, IPMI_DEBUG_MAX_BUF_LEN);
       
          ERR_CLEANUP (!(ipmi_debug_output_byte_array(fd, prefix, buf, len) < 0));
        }

      fiid_iterator_next(iter);
    }

  ERR_CLEANUP (!(ipmi_debug_output_str(fd, prefix, trlr) < 0));

  fiid_iterator_destroy(iter);
  return (0);

 cleanup:
  if (iter)
    fiid_iterator_destroy(iter);
  return (-1);
}

int8_t
ipmi_obj_dump (int fd, fiid_obj_t obj)
{
  char *hdr = 
    "================================================================\n"
    "[ VALUE               TAG NAME:LENGTH                          ]\n"
    "================================================================";
  char *trlr = 
    "================================================================";

  return ipmi_obj_dump_perror(fd, NULL, hdr, trlr, obj);
}

