/*
  ipmi-debug.c - IPMI Debugging Functions

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <errno.h>

#include "freeipmi/ipmi-authentication-type-spec.h"
#include "freeipmi/ipmi-debug.h"
#include "freeipmi/ipmi-comp-code-spec.h"
#include "freeipmi/ipmi-lan.h"
#include "freeipmi/ipmi-lan-interface.h"
#include "freeipmi/rmcp.h"

#include "bit-ops.h"
#include "err-wrappers.h"
#include "fiid-wrappers.h"
#include "freeipmi-portability.h"
#include "ipmi-common.h"

#define IPMI_DEBUG_MAX_PREFIX_LEN        32
#define IPMI_DEBUG_MAX_BUF_LEN        65536
#define IPMI_DEBUG_MAX_PKT_LEN        65536
#define IPMI_DEBUG_CHAR_PER_LINE          8
#define IPMI_DEBUG_DEFAULT_FD STDERR_FILENO

#define FREEIPMI_DPRINTF(args) \
        do { \
          ERR (!((freeipmi_dprintf args) < 0)); \
        } while(0) 

#define FREEIPMI_DPRINTF_CLEANUP(args) \
        do { \
          ERR_CLEANUP (!((freeipmi_dprintf args) < 0)); \
        } while(0) 

#define IPMI_DEBUG_MAX_UNEXPECTED_BYTES 65536
#define IPMI_DEBUG_MAX_UNEXPECTED_BITS  (IPMI_DEBUG_MAX_UNEXPECTED_BYTES*8)

fiid_template_t tmpl_unexpected_data =
  {
    {IPMI_DEBUG_MAX_UNEXPECTED_BITS, "unexpected_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
    {0, "", 0}
  };

static int8_t
_set_prefix_str(char *buf, unsigned int buflen, char *prefix)
{
  ERR (buf && buflen > 3);

  memset(buf, '\0', buflen);
  if (prefix)
    {
      strncpy(buf, prefix, buflen);
      buf[buflen - 1] = '\0'; /* strncpy may not null terminate */
      buf[buflen - 2] = '\0'; /* guaranteed space for ' ' */
      buf[buflen - 3] = '\0'; /* guaranteed space for ':' */
      strcat(buf, ": ");
    }

  return (0);
}

static int8_t
_output_str(int fd, char *prefix, char *str)
{
  /* achu: Yeah, I know this is slow.  Figure out something better
   * later.
   */
  if (str)
    {
      char *ptr = str;

      if (prefix)
        FREEIPMI_DPRINTF((fd, "%s", prefix));
      while (*ptr != '\0')
        {
          if (*ptr == '\n')
            {
              FREEIPMI_DPRINTF((fd, "%c", *ptr++));
              if (prefix)
                FREEIPMI_DPRINTF((fd, "%s", prefix));
            }
          else
            FREEIPMI_DPRINTF((fd, "%c", *ptr++));
        }
      FREEIPMI_DPRINTF((fd, "\n"));
    }

  return 0;
} 

static int8_t
_output_byte_array(int fd, char *prefix, uint8_t *buf, uint32_t buf_len)
{
  uint32_t count = 0;

  if (!buf || !buf_len)
    return 0;

  while (count < buf_len)
    {
      int i = 0;
      if (prefix)
        FREEIPMI_DPRINTF ((fd, "%s", prefix));
      FREEIPMI_DPRINTF ((fd, "[ "));
      while (count < buf_len && i < IPMI_DEBUG_CHAR_PER_LINE)
	{
	  FREEIPMI_DPRINTF ((fd, "%02Xh ", buf[count++]));
	  i++;
	}
      FREEIPMI_DPRINTF ((fd, "]\n"));
    }

  return 0;
}

int8_t
ipmi_dump_setup(int fd, char *prefix, char *hdr, char *prefix_buf, uint32_t prefix_buf_len)
{
  ERR (!(_set_prefix_str(prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN, prefix) < 0));

  ERR (!(_output_str(fd, prefix_buf, hdr) < 0));

  return (0);
}

int8_t
ipmi_obj_dump_perror (int fd, char *prefix, char *hdr, char *trlr, fiid_obj_t obj)
{
  char prefix_buf[IPMI_DEBUG_MAX_PREFIX_LEN];
  fiid_iterator_t iter = NULL;

  ERR_EINVAL (fiid_obj_valid(obj));
  
  ERR (!(ipmi_dump_setup(fd, prefix, hdr, prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN) < 0));

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
        FREEIPMI_DPRINTF_CLEANUP ((fd, "%s", prefix));

      if (field_len <= 64)
        {
          uint64_t val = 0;

	  FIID_ITERATOR_GET_CLEANUP (iter, &val);

          FREEIPMI_DPRINTF_CLEANUP ((fd, "[%16LXh] = %s[%2db]\n", (uint64_t) val, key, field_len));
        }
      else
        {
          uint8_t buf[IPMI_DEBUG_MAX_BUF_LEN];
          int len;

          FREEIPMI_DPRINTF_CLEANUP ((fd, "[  BYTE ARRAY ... ] = %s[%2dB]\n", key, BITS_ROUND_BYTES(field_len)));

	  FIID_ITERATOR_GET_DATA_LEN_CLEANUP(len, iter, buf, IPMI_DEBUG_MAX_BUF_LEN);
       
          ERR_CLEANUP (!(_output_byte_array(fd, prefix, buf, len) < 0));
        }

      fiid_iterator_next(iter);
    }

  ERR_CLEANUP (!(_output_str(fd, prefix, trlr) < 0));

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

int8_t
ipmi_dump_lan_packet (int fd, char *prefix, char *hdr, uint8_t *pkt, uint32_t pkt_len, fiid_template_t tmpl_lan_msg_hdr, fiid_template_t tmpl_cmd)
{
  uint32_t indx = 0;
  int32_t obj_cmd_len, obj_lan_msg_trlr_len;
  char prefix_buf[IPMI_DEBUG_MAX_PREFIX_LEN];
  char *rmcp_hdr = 
    "RMCP Header:\n"
    "------------";
  char *session_hdr =
    "IPMI Session Header:\n"
    "--------------------";
  char *msg_hdr =
    "IPMI Message Header:\n"
    "--------------------";
  char *cmd_hdr =
    "IPMI Command Data:\n"
    "------------------";
  char *trlr_hdr =
    "IPMI Trailer:\n"
    "--------------";
  char *unexpected_hdr =
    "Unexpected Data:\n"
    "----------------";
  fiid_obj_t obj_rmcp_hdr = NULL;
  fiid_obj_t obj_session_hdr = NULL;
  fiid_obj_t obj_lan_msg_hdr = NULL;
  fiid_obj_t obj_cmd = NULL;
  fiid_obj_t obj_lan_msg_trlr = NULL;
  fiid_obj_t obj_unexpected_data = NULL;
  int32_t len;
  int8_t rv = -1;
  uint64_t authentication_type;

  ERR_EINVAL (pkt && tmpl_lan_msg_hdr && tmpl_cmd);

  ERR (!(ipmi_dump_setup(fd, prefix, hdr, prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN) < 0));

  /* Dump rmcp header */
  
  FIID_OBJ_CREATE_CLEANUP (obj_rmcp_hdr, tmpl_rmcp_hdr);
  
  FIID_OBJ_SET_ALL_LEN_CLEANUP (len, obj_rmcp_hdr, pkt + indx, pkt_len - indx);
  indx += len;
  
  ERR_CLEANUP (!(ipmi_obj_dump_perror(fd, prefix_buf, rmcp_hdr, NULL, obj_rmcp_hdr) < 0));
  
  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }

  /* Dump session header */
  /* Output of session header depends on the auth code */

  FIID_OBJ_CREATE_CLEANUP (obj_session_hdr, tmpl_lan_session_hdr);
  
  FIID_OBJ_SET_BLOCK_LEN_CLEANUP(len, 
				 obj_session_hdr, 
				 "authentication_type", 
				 "session_id", 
				 pkt + indx, 
				 pkt_len - indx);
  indx += len;
  
  if (pkt_len <= indx)
    {
      ERR_CLEANUP (!(ipmi_obj_dump_perror(fd, 
					  prefix_buf, 
					  session_hdr, 
					  NULL, 
					  obj_session_hdr) < 0));

      rv = 0;
      goto cleanup;
    }

  FIID_OBJ_GET_CLEANUP (obj_session_hdr, "authentication_type", &authentication_type);

  if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
    {
      FIID_OBJ_SET_DATA_LEN_CLEANUP(len, 
				    obj_session_hdr,
				    "authentication_code",
				    pkt + indx,
				    pkt_len - indx);
      indx += len;
    }

  if (pkt_len <= indx)
    {
      ERR_CLEANUP (!(ipmi_obj_dump_perror(fd, 
					  prefix_buf, 
					  session_hdr, 
					  NULL, 
					  obj_session_hdr) < 0));

      rv = 0;
      goto cleanup;
    }

  FIID_OBJ_SET_DATA_LEN_CLEANUP(len,
				obj_session_hdr,
				"ipmi_msg_len",
				pkt + indx,
				pkt_len - indx);
  indx += len;

  ERR_CLEANUP (!(ipmi_obj_dump_perror(fd, 
				      prefix_buf, 
				      session_hdr, 
				      NULL, 
				      obj_session_hdr) < 0));

  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }

  /* Dump message header */

  FIID_OBJ_CREATE_CLEANUP (obj_lan_msg_hdr, tmpl_lan_msg_hdr);
  
  FIID_OBJ_SET_ALL_LEN_CLEANUP (len, obj_lan_msg_hdr, pkt + indx, pkt_len - indx);
  indx += len;
  
  ERR_CLEANUP (!(ipmi_obj_dump_perror(fd, prefix_buf, msg_hdr, NULL, obj_lan_msg_hdr) < 0));

  /* Clear out data */
  FIID_OBJ_CLEAR_CLEANUP(obj_lan_msg_hdr);

  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }

  /* Dump command data */

  FIID_OBJ_CREATE_CLEANUP (obj_cmd, tmpl_cmd);
  FIID_OBJ_CREATE_CLEANUP (obj_lan_msg_trlr, tmpl_lan_msg_trlr);
  
  FIID_TEMPLATE_LEN_BYTES (obj_lan_msg_trlr_len, tmpl_lan_msg_trlr);

  if ((pkt_len - indx) >= obj_lan_msg_trlr_len)
    obj_cmd_len = (pkt_len - indx) - obj_lan_msg_trlr_len;
  else if ((pkt_len - indx) < obj_lan_msg_trlr_len)
    obj_cmd_len = 0;

  if (obj_cmd_len)
    {
      
      FIID_OBJ_SET_ALL_LEN_CLEANUP (len, 
				    obj_cmd,
				    pkt + indx, 
				    obj_cmd_len);
      indx += len;
      
      ERR_CLEANUP (!(ipmi_obj_dump_perror(fd, 
					  prefix_buf, 
					  cmd_hdr, 
					  NULL, 
					  obj_cmd) < 0));

      if (pkt_len <= indx)
	{
	  rv = 0;
	  goto cleanup;
	}
    }

  /* Dump trailer */

  FIID_OBJ_SET_ALL_LEN_CLEANUP (len, obj_lan_msg_trlr, pkt + indx, pkt_len - indx);
  indx += len;
  
  ERR_CLEANUP (!(ipmi_obj_dump_perror(fd, prefix_buf, trlr_hdr, NULL, obj_lan_msg_trlr) < 0));
  
  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }

  /* Dump unexpected stuff */
  
  FIID_OBJ_CREATE_CLEANUP (obj_unexpected_data, tmpl_unexpected_data);
  
  FIID_OBJ_SET_ALL_LEN_CLEANUP (len, obj_unexpected_data, pkt + indx, pkt_len - indx);
  indx += len;
  
  ERR_CLEANUP (!(ipmi_obj_dump_perror(fd, prefix_buf, unexpected_hdr, NULL, obj_unexpected_data) < 0));
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY(obj_rmcp_hdr);
  FIID_OBJ_DESTROY(obj_session_hdr);
  FIID_OBJ_DESTROY(obj_lan_msg_hdr);
  FIID_OBJ_DESTROY(obj_cmd);
  FIID_OBJ_DESTROY(obj_lan_msg_trlr);
  FIID_OBJ_DESTROY(obj_unexpected_data);
  return (rv);
}

int8_t 
ipmi_dump_rmcp_packet (int fd, char *prefix, char *hdr, uint8_t *pkt, uint32_t pkt_len, fiid_template_t tmpl_cmd)
{
  uint32_t indx = 0;
  char prefix_buf[IPMI_DEBUG_MAX_PREFIX_LEN];
  char *rmcp_hdr =
    "RMCP Header:\n"
    "------------";
  char *rmcp_cmd =
    "RMCP Command Data:\n"
    "------------------";
  char *unexpected_hdr =
    "Unexpected Data:\n"
    "----------------";
  fiid_obj_t obj_rmcp_hdr = NULL;
  fiid_obj_t obj_cmd = NULL;
  fiid_obj_t obj_unexpected_data = NULL;
  int32_t len;
  int8_t rv = -1;

  ERR_EINVAL (pkt && tmpl_cmd);

  ERR (!(ipmi_dump_setup(fd, prefix, hdr, prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN) < 0));

  /* Dump rmcp header */
  
  FIID_OBJ_CREATE_CLEANUP (obj_rmcp_hdr, tmpl_rmcp_hdr);
  
  FIID_OBJ_SET_ALL_LEN_CLEANUP (len, obj_rmcp_hdr, pkt + indx, pkt_len - indx);
  indx += len;
  
  ERR_CLEANUP (!(ipmi_obj_dump_perror(fd, prefix_buf, rmcp_hdr, NULL, obj_rmcp_hdr) < 0));
  
  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }
  
  /* Dump command data */
  
  FIID_OBJ_CREATE_CLEANUP (obj_cmd, tmpl_cmd);
  
  FIID_OBJ_SET_ALL_LEN_CLEANUP (len, obj_cmd, pkt + indx, pkt_len - indx);
  indx += len;
  
  ERR_CLEANUP (!(ipmi_obj_dump_perror(fd, prefix_buf, rmcp_cmd, NULL, obj_cmd) < 0));
  
  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }
  
  /* Dump unexpected stuff */
  
  FIID_OBJ_CREATE_CLEANUP (obj_unexpected_data, tmpl_unexpected_data);
  
  FIID_OBJ_SET_ALL_LEN_CLEANUP (len, obj_unexpected_data, pkt + indx, pkt_len - indx);
  indx += len;
  
  ERR_CLEANUP (!(ipmi_obj_dump_perror(fd, prefix_buf, unexpected_hdr, NULL, obj_unexpected_data) < 0));
  
  rv = 0;
 cleanup:
  FIID_OBJ_DESTROY(obj_rmcp_hdr);
  FIID_OBJ_DESTROY(obj_cmd);
  FIID_OBJ_DESTROY(obj_unexpected_data);
  return (rv);
}

