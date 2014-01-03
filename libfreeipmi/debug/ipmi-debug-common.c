/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "freeipmi/debug/ipmi-debug.h"
#include "freeipmi/fiid/fiid.h"
#include "freeipmi/interface/ipmi-ipmb-interface.h"

#include "ipmi-debug-common.h"

#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

#define IPMI_DEBUG_MAX_BUF_LEN        65536
#define IPMI_DEBUG_MAX_PKT_LEN        65536
#define IPMI_DEBUG_CHAR_PER_LINE          8
#define IPMI_DEBUG_DEFAULT_FD STDERR_FILENO

static int
_write (int fd, const void *buf, size_t n)
{
  /* chu: by Chris Dunlap <dunlap6 at llnl dot gov> */
  size_t nleft;
  ssize_t nwritten;
  unsigned char *p;

  p = (unsigned char *)buf;
  nleft = n;
  while (nleft > 0)
    {
      if ((nwritten = write (fd, p, nleft)) < 0)
        {
          if (errno == EINTR)
            continue;
          else
            return (-1);
        }
      nleft -= nwritten;
      p += nwritten;
    }
  return (n);
}

int
debug_dprintf (int fd, const char *fmt, ...)
{
  va_list ap;
  int len, rv;
  char buf[IPMI_DEBUG_MAX_BUF_LEN];

  va_start (ap, fmt);
  len = vsnprintf (buf, IPMI_DEBUG_MAX_BUF_LEN, fmt, ap);
  rv = _write (fd, buf, len);
  va_end (ap);

  return (rv);
}

int
debug_set_prefix (char *buf, unsigned int buflen, const char *prefix)
{
  assert (buf && buflen > 3);

  memset (buf, '\0', buflen);
  if (prefix)
    {
      strncpy (buf, prefix, buflen);
      buf[buflen - 1] = '\0'; /* strncpy may not null terminate */
      buf[buflen - 2] = '\0'; /* guaranteed space for ' ' */
      buf[buflen - 3] = '\0'; /* guaranteed space for ':' */
      strcat (buf, ": ");
    }

  return (0);
}

int
debug_output_str (int fd, const char *prefix, const char *str)
{
  /* achu: Yeah, I know this is slow.  Figure out something better
   * later.
   */
  if (str)
    {
      char *ptr = (char *)str;

      if (prefix)
        {
          if (debug_dprintf (fd, "%s", prefix) < 0)
            {
              ERRNO_TRACE (errno);
              return (-1);
            }
        }

      while (*ptr != '\0')
        {
          if (*ptr == '\n')
            {
              if (debug_dprintf (fd, "%c", *ptr++) < 0)
                {
                  ERRNO_TRACE (errno);
                  return (-1);
                }
              if (prefix)
                {
                  if (debug_dprintf (fd, "%s", prefix) < 0)
                    {
                      ERRNO_TRACE (errno);
                      return (-1);
                    }
                }
            }
          else
            {
              if (debug_dprintf (fd, "%c", *ptr++) < 0)
                {
                  ERRNO_TRACE (errno);
                  return (-1);
                }
            }
        }

      if (debug_dprintf (fd, "\n") < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }
    }

  return (0);
}

int
debug_output_byte_array (int fd, const char *prefix, const uint8_t *buf, unsigned int buf_len)
{
  unsigned int count = 0;

  assert (buf);

  while (count < buf_len)
    {
      int i = 0;
      if (prefix)
        {
          if (debug_dprintf (fd, "%s", prefix) < 0)
            {
              ERRNO_TRACE (errno);
              return (-1);
            }
        }

      if (debug_dprintf (fd, "[ ") < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }

      while (count < buf_len && i < IPMI_DEBUG_CHAR_PER_LINE)
        {
          if (debug_dprintf (fd, "%02Xh ", buf[count++]) < 0)
            {
              ERRNO_TRACE (errno);
              return (-1);
            }
          i++;
        }

      if (debug_dprintf (fd, "]\n") < 0)
        {
          ERRNO_TRACE (errno);
          return (-1);
        }
    }

  return (0);
}

int
debug_dump_ipmb (int fd,
		 const char *prefix,
		 const uint8_t *ipmb_buf,
		 unsigned int ipmb_buf_len,
		 fiid_template_t tmpl_ipmb_msg_hdr,
		 fiid_template_t tmpl_ipmb_cmd)
{
  char *ipmb_msg_hdr =
    "IPMB Message Header:\n"
    "--------------------";
  char *ipmb_cmd_hdr =
    "IPMB Message Data:\n"
    "------------------";
  char *ipmb_msg_trlr_hdr =
    "IPMB Message Trailer:\n"
    "---------------------";
  char *ipmb_unexpected_hdr =
    "IPMB Unexpected Data:\n"
    "---------------------";
  fiid_obj_t obj_ipmb_msg_hdr = NULL;
  fiid_obj_t obj_ipmb_cmd = NULL;
  fiid_obj_t obj_ipmb_msg_trlr = NULL;
  fiid_obj_t obj_ipmb_unexpected_data = NULL;
  int obj_ipmb_msg_trlr_len = 0;
  unsigned int obj_ipmb_cmd_len = 0;
  int ipmb_hdr_len = 0;
  int ipmb_cmd_len = 0;
  int ipmb_trlr_len = 0;
  int len;
  unsigned int ipmb_indx = 0;
  int rv = -1;

  assert (ipmb_buf);
  assert (ipmb_buf_len);
  assert (tmpl_ipmb_msg_hdr);
  assert (tmpl_ipmb_cmd);

  if (!(obj_ipmb_msg_hdr = fiid_obj_create (tmpl_ipmb_msg_hdr)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }
  if (!(obj_ipmb_cmd = fiid_obj_create (tmpl_ipmb_cmd)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }
  if (!(obj_ipmb_msg_trlr = fiid_obj_create (tmpl_ipmb_msg_trlr)))
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((obj_ipmb_msg_trlr_len = fiid_template_len_bytes (tmpl_ipmb_msg_trlr)) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if ((ipmb_hdr_len = fiid_obj_set_all (obj_ipmb_msg_hdr,
                                        ipmb_buf,
                                        ipmb_buf_len)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_hdr);
      goto cleanup;
    }
  ipmb_indx += ipmb_hdr_len;

  if (ipmi_obj_dump (fd,
                     prefix,
                     ipmb_msg_hdr,
                     NULL,
                     obj_ipmb_msg_hdr) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (ipmb_buf_len <= ipmb_indx)
    {
      rv = 0;
      goto cleanup;
    }

  if ((ipmb_buf_len - ipmb_hdr_len) <= obj_ipmb_msg_trlr_len)
    goto dump_ipmb_extra;

  obj_ipmb_cmd_len = (ipmb_buf_len - ipmb_hdr_len) - obj_ipmb_msg_trlr_len;

  if ((ipmb_cmd_len = fiid_obj_set_all (obj_ipmb_cmd,
                                        ipmb_buf + ipmb_hdr_len,
                                        obj_ipmb_cmd_len)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_cmd);
      goto cleanup;
    }
  ipmb_indx += ipmb_cmd_len;

  if (ipmi_obj_dump (fd,
                     prefix,
                     ipmb_cmd_hdr,
                     NULL,
                     obj_ipmb_cmd) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }

  if (ipmb_buf_len <= ipmb_indx)
    {
      rv = 0;
      goto cleanup;
    }

  if ((ipmb_trlr_len = fiid_obj_set_all (obj_ipmb_msg_trlr,
                                         ipmb_buf + ipmb_hdr_len + ipmb_cmd_len,
                                         (ipmb_buf_len - ipmb_hdr_len - ipmb_cmd_len))) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_msg_trlr);
      goto cleanup;
    }
  ipmb_indx += ipmb_trlr_len;

  if (ipmi_obj_dump (fd,
                     prefix,
                     ipmb_msg_trlr_hdr,
                     NULL,
                     obj_ipmb_msg_trlr) < 0)
    {
      ERRNO_TRACE (errno);
      goto cleanup;
    }
  
  /* Dump IPMB unexpected stuff */
  
 dump_ipmb_extra:
  
  if ((ipmb_buf_len - ipmb_indx) > 0)
    {
      if (!(obj_ipmb_unexpected_data = fiid_obj_create (tmpl_unexpected_data)))
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
      
      if ((len = fiid_obj_set_all (obj_ipmb_unexpected_data,
                                   ipmb_buf + ipmb_indx,
                                   ipmb_buf_len - ipmb_indx)) < 0)
        {
          FIID_OBJECT_ERROR_TO_ERRNO (obj_ipmb_unexpected_data);
          goto cleanup;
        }
      ipmb_indx += len;
      
      if (ipmi_obj_dump (fd,
                         prefix,
                         ipmb_unexpected_hdr,
                         NULL,
                         obj_ipmb_unexpected_data) < 0)
        {
          ERRNO_TRACE (errno);
          goto cleanup;
        }
    }
  
  rv = 0;
 cleanup:
  fiid_obj_destroy (obj_ipmb_msg_hdr);
  fiid_obj_destroy (obj_ipmb_cmd);
  fiid_obj_destroy (obj_ipmb_msg_trlr);
  fiid_obj_destroy (obj_ipmb_unexpected_data);
  return (rv);

}
