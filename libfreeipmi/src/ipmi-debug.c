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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include "freeipmi.h"

#define IPMI_DEBUG_MAX_PREFIX_LEN        32
#define IPMI_DEBUG_MAX_BUF_LEN        65536
#define IPMI_DEBUG_MAX_PKT_LEN        65536
#define IPMI_DEBUG_CHAR_PER_LINE          8

#define _DPRINTF(args) \
        do { \
          if((_dprintf args) < 0) \
            return -1; \
        } while(0) 

#define _DPRINTF_CLEANUP(args) \
        do { \
          if((_dprintf args) < 0) \
            goto cleanup; \
        } while(0) 

#define IPMI_DEBUG_MAX_UNEXPECTED_BYTES 65536
#define IPMI_DEBUG_MAX_UNEXPECTED_BITS  (IPMI_DEBUG_MAX_UNEXPECTED_BYTES*8)

fiid_template_t tmpl_unexpected =
{
  {IPMI_DEBUG_MAX_UNEXPECTED_BITS, "unexpected_data", FIID_FIELD_REQUIRED | FIID_FIELD_LENGTH_FIXED},
  {0, "", 0}
};

static int
_write(int fd, void *buf, size_t n)
{
  /* chu: by Chris Dunlap <dunlap6 at llnl dot gov> */
  size_t nleft;
  ssize_t nwritten;
  unsigned char *p;

  p = buf;
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

/* Portable version of the extremely unportable Linux dprintf() */
static int
_dprintf(int fd, char *fmt, ...)
{
  va_list ap;
  int len, rv;
  char buf[IPMI_DEBUG_MAX_BUF_LEN];

  va_start(ap, fmt);
  len = vsnprintf(buf, IPMI_DEBUG_MAX_BUF_LEN, fmt, ap);
  rv = _write(fd, buf, len);
  va_end(ap);

  return rv;
}

static int8_t
_set_prefix_str(char *buf, unsigned int buflen, char *prefix)
{
  if (!buf || buflen <= 3)
    return (-1);

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
        _DPRINTF((fd, "%s", prefix));
      while (*ptr != '\0')
        {
          if (*ptr == '\n')
            {
              _DPRINTF((fd, "%c", *ptr++));
              if (prefix)
                _DPRINTF((fd, "%s", prefix));
            }
          else
            _DPRINTF((fd, "%c", *ptr++));
        }
      _DPRINTF((fd, "\n"));
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
        _DPRINTF ((fd, "%s", prefix));
      _DPRINTF ((fd, "[ "));
      while (count < buf_len && i < IPMI_DEBUG_CHAR_PER_LINE)
	{
	  _DPRINTF ((fd, "%02Xh ", buf[count++]));
	  i++;
	}
      _DPRINTF ((fd, "]\n"));
    }

  return 0;
}

int8_t
fiid_obj_dump_setup(int fd, char *prefix, char *hdr, char *prefix_buf, uint32_t prefix_buf_len)
{
  if (_set_prefix_str(prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN, prefix) < 0)
    return (-1);

  if (_output_str(fd, prefix_buf, hdr) < 0)
    return (-1);

  return (0);
}

int8_t
fiid_obj_dump_perror (int fd, char *prefix, char *hdr, char *trlr, fiid_obj_t obj)
{
  char prefix_buf[IPMI_DEBUG_MAX_PREFIX_LEN];
  fiid_iterator_t iter = NULL;

  if (!fiid_obj_valid(obj))
    {
      errno = EINVAL;
      goto cleanup;
    }
  
  if (fiid_obj_dump_setup(fd, prefix, hdr, prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN) < 0)
    goto cleanup;

  if (!(iter = fiid_iterator_create(obj)))
    goto cleanup;

  while (!fiid_iterator_end(iter))
    {
      int32_t field_len;
      uint8_t *key;

      if (!(key = fiid_iterator_key(iter)))
        goto cleanup;

      if ((field_len = fiid_iterator_field_len(iter)) < 0)
        goto cleanup;
    
      if (!field_len)
        {
          fiid_iterator_next(iter);
          continue;
        }

      if (prefix)
        _DPRINTF_CLEANUP ((fd, "%s", prefix));

      if (field_len <= 64)
        {
          uint64_t val = 0;

	  if (fiid_iterator_get (iter, &val) < 0)
            goto cleanup;

          _DPRINTF_CLEANUP ((fd, "[%16LXh] = %s[%2db]\n", (uint64_t) val, key, field_len));
        }
      else
        {
          char buf[IPMI_DEBUG_MAX_BUF_LEN];
          int len;

          _DPRINTF_CLEANUP ((fd, "[  BYTE ARRAY ... ] = %s[%2dB]\n", key, BITS_ROUND_BYTES(field_len)));
     
          if ((len = fiid_iterator_get_data(iter, buf, IPMI_DEBUG_MAX_BUF_LEN)) < 0)
            goto cleanup;
       
          if (_output_byte_array(fd, prefix, buf, len) < 0)
            goto cleanup;
        }

      fiid_iterator_next(iter);
    }

  if (_output_str(fd, prefix, trlr) < 0)
    goto cleanup;

  fiid_iterator_destroy(iter);
  return (0);

 cleanup:
  if (iter)
    fiid_iterator_destroy(iter);
  return (-1);
}

int8_t
fiid_obj_dump (int fd, fiid_obj_t obj)
{
  char *hdr = 
    "================================================================\n"
    "[ VALUE               TAG NAME:LENGTH                          ]\n"
    "================================================================";
  char *trlr = 
    "================================================================";

  return fiid_obj_dump_perror(fd, NULL, hdr, trlr, obj);
}

int8_t
fiid_obj_dump_lan (int fd, char *prefix, char *hdr, uint8_t *pkt, uint32_t pkt_len, fiid_template_t tmpl_msg_hdr, fiid_template_t tmpl_cmd)
{
  uint32_t indx = 0;
  int32_t obj_cmd_len, obj_msg_trlr_len;
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
  fiid_obj_t obj_msg_hdr = NULL;
  fiid_obj_t obj_cmd = NULL;
  fiid_obj_t obj_msg_trlr = NULL;
  fiid_obj_t obj_unexpected = NULL;
  int32_t len;
  int8_t rv = -1;
  uint64_t auth_type;

  if (!(pkt && tmpl_msg_hdr && tmpl_cmd))
    {
      errno = EINVAL;
      goto cleanup;
    }

  if (fiid_obj_dump_setup(fd, prefix, hdr, prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN) < 0)
    goto cleanup;

  /* Dump rmcp header */
  
  if (!(obj_rmcp_hdr = fiid_obj_create(tmpl_hdr_rmcp)))
    goto cleanup;
  
  if ((len = fiid_obj_set_all(obj_rmcp_hdr, pkt + indx, pkt_len - indx)) < 0)
    goto cleanup;
  indx += len;
  
  if (fiid_obj_dump_perror(fd, prefix_buf, rmcp_hdr, NULL, obj_rmcp_hdr) < 0)
    goto cleanup;
  
  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }

  /* Dump session header */
  /* Output of session header depends on the auth code */

  if (!(obj_session_hdr = fiid_obj_create(tmpl_lan_session_hdr)))
    goto cleanup;
  
  if ((len = fiid_obj_set_block(obj_session_hdr, 
                                (uint8_t *)"auth_type", 
                                (uint8_t *)"session_id", 
                                pkt + indx, 
                                pkt_len - indx)) < 0)
    goto cleanup;
  indx += len;
  
  if (pkt_len <= indx)
    {
      if (fiid_obj_dump_perror(fd, 
			       prefix_buf, 
			       session_hdr, 
			       NULL, 
			       obj_session_hdr) < 0)
	goto cleanup;

      rv = 0;
      goto cleanup;
    }

  if (fiid_obj_get(obj_session_hdr, "auth_type", &auth_type) < 0)
    goto cleanup;

  if (auth_type != IPMI_SESSION_AUTH_TYPE_NONE)
    {
      if ((len = fiid_obj_set_data(obj_session_hdr,
				   (uint8_t *)"auth_code",
				   pkt + indx,
				   pkt_len - indx)) < 0)
	goto cleanup;
      indx += len;
    }

  if (pkt_len <= indx)
    {
      if (fiid_obj_dump_perror(fd, 
			       prefix_buf, 
			       session_hdr, 
			       NULL, 
			       obj_session_hdr) < 0)
	goto cleanup;

      rv = 0;
      goto cleanup;
    }

  if ((len = fiid_obj_set_data(obj_session_hdr,
			       "ipmi_msg_len",
			       pkt + indx,
			       pkt_len - indx)) < 0)
    goto cleanup;
  indx += len;

  if (fiid_obj_dump_perror(fd, 
			   prefix_buf, 
			   session_hdr, 
			   NULL, 
			   obj_session_hdr) < 0)
    goto cleanup;

  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }

  /* Dump message header */

  if (!(obj_msg_hdr = fiid_obj_create(tmpl_msg_hdr)))
    goto cleanup;
  
  if ((len = fiid_obj_set_all(obj_msg_hdr, pkt + indx, pkt_len - indx)) < 0)
    goto cleanup;
  indx += len;
  
  if (fiid_obj_dump_perror(fd, prefix_buf, msg_hdr, NULL, obj_msg_hdr) < 0)
    goto cleanup;
  
  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }

  /* Dump command data */

  if (!(obj_cmd = fiid_obj_create(tmpl_cmd)))
    goto cleanup;

  if (!(obj_msg_trlr = fiid_obj_create(tmpl_lan_msg_trlr)))
    goto cleanup;
  
  ERR(!((obj_msg_trlr_len = fiid_template_len_bytes (tmpl_lan_msg_trlr)) < 0));

  if ((pkt_len - indx) >= obj_msg_trlr_len)
    obj_cmd_len = (pkt_len - indx) - obj_msg_trlr_len;
  else if ((pkt_len - indx) < obj_msg_trlr_len)
    obj_cmd_len = 0;

  if (obj_cmd_len)
    {
      if ((len = fiid_obj_set_all(obj_cmd,
				  pkt + indx, 
				  pkt_len - indx)) < 0)
	goto cleanup;
      indx += len;
      
      if (fiid_obj_dump_perror(fd, 
			       prefix_buf, 
			       cmd_hdr, 
			       NULL, 
			       obj_cmd) < 0)
	goto cleanup;

      if (pkt_len <= indx)
	{
	  rv = 0;
	  goto cleanup;
	}
    }

  /* Dump trailer */

  if ((len = fiid_obj_set_all(obj_msg_trlr, pkt + indx, pkt_len - indx)) < 0)
    goto cleanup;
  indx += len;
  
  if (fiid_obj_dump_perror(fd, prefix_buf, trlr_hdr, NULL, obj_msg_trlr) < 0)
    goto cleanup;
  
  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }

  /* Dump unexpected stuff */
  
  if (!(obj_unexpected = fiid_obj_create(tmpl_unexpected)))
    goto cleanup;
  
  if ((len = fiid_obj_set_all(obj_unexpected, pkt + indx, pkt_len - indx)) < 0)
    goto cleanup;
  indx += len;
  
  if (fiid_obj_dump_perror(fd, prefix_buf, unexpected_hdr, NULL, obj_unexpected) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_rmcp_hdr)
    fiid_obj_destroy(obj_rmcp_hdr);
  if (obj_session_hdr)
    fiid_obj_destroy(obj_session_hdr);
  if (obj_msg_hdr)
    fiid_obj_destroy(obj_msg_hdr);
  if (obj_cmd)
    fiid_obj_destroy(obj_cmd);
  if (obj_msg_trlr)
    fiid_obj_destroy(obj_msg_trlr);
  if (obj_unexpected)
    fiid_obj_destroy(obj_unexpected);
  return (rv);
}

int8_t 
fiid_obj_dump_rmcp (int fd, char *prefix, char *hdr, uint8_t *pkt, uint32_t pkt_len, fiid_template_t tmpl_cmd)
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
  fiid_obj_t obj_unexpected = NULL;
  int32_t len;
  int8_t rv = -1;

  if (!(pkt && tmpl_cmd))
    {
      errno = EINVAL;
      return (-1);
    }

  if (fiid_obj_dump_setup(fd, prefix, hdr, prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN) < 0)
    return (-1);

  /* Dump rmcp header */
  
  if (!(obj_rmcp_hdr = fiid_obj_create(tmpl_hdr_rmcp)))
    goto cleanup;
  
  if ((len = fiid_obj_set_all(obj_rmcp_hdr, pkt + indx, pkt_len - indx)) < 0)
    goto cleanup;
  indx += len;
  
  if (fiid_obj_dump_perror(fd, prefix_buf, rmcp_hdr, NULL, obj_rmcp_hdr) < 0)
    goto cleanup;
  
  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }
  
  /* Dump command data */
  
  if (!(obj_cmd = fiid_obj_create(tmpl_cmd)))
    goto cleanup;
  
  if ((len = fiid_obj_set_all(obj_cmd, pkt + indx, pkt_len - indx)) < 0)
    goto cleanup;
  indx += len;
  
  if (fiid_obj_dump_perror(fd, prefix_buf, rmcp_cmd, NULL, obj_cmd) < 0)
    goto cleanup;
  
  if (pkt_len <= indx)
    {
      rv = 0;
      goto cleanup;
    }
  
  /* Dump unexpected stuff */
  
  if (!(obj_unexpected = fiid_obj_create(tmpl_unexpected)))
    goto cleanup;
  
  if ((len = fiid_obj_set_all(obj_unexpected, pkt + indx, pkt_len - indx)) < 0)
    goto cleanup;
  indx += len;
  
  if (fiid_obj_dump_perror(fd, prefix_buf, unexpected_hdr, NULL, obj_unexpected) < 0)
    goto cleanup;
  
  rv = 0;
 cleanup:
  if (obj_rmcp_hdr)
    fiid_obj_destroy(obj_rmcp_hdr);
  if (obj_cmd)
    fiid_obj_destroy(obj_cmd);
  if (obj_unexpected)
    fiid_obj_destroy(obj_unexpected);
  return (rv);
}

uint8_t
ipmi_kcs_print_state (int fd, uint8_t state)
{
    /* we assume we have already ioperm'd the space */
    _dprintf (fd, "Current KCS state: 0x%x : ", state);
    if ((state & IPMI_KCS_STATUS_REG_STATE) == IPMI_KCS_STATE_IDLE) {
            _dprintf (fd, "IDLE_STATE ");
    } else if ((state & IPMI_KCS_STATUS_REG_STATE) == IPMI_KCS_STATE_READ) {
            _dprintf (fd, "READ_STATE ");
    } else if ((state & IPMI_KCS_STATUS_REG_STATE) == IPMI_KCS_STATE_WRITE) {
            _dprintf (fd, "WRITE_STATE ");
    } else if ((state & IPMI_KCS_STATUS_REG_STATE) == IPMI_KCS_STATE_ERROR) {
            _dprintf (fd, "ERROR_STATE ");
    } else {
      _dprintf (fd, "UNKNOWN_STATE "); /* cannot happen */
    }
    if (state & IPMI_KCS_STATUS_REG_IBF) {
            _dprintf (fd, "IBF ");
    }
    if (state & IPMI_KCS_STATUS_REG_OBF) {
            _dprintf (fd, "OBF ");
    }
    if (state & IPMI_KCS_STATUS_REG_OEM1) {
            _dprintf (fd, "OEM1 ");
    }
    if (state & IPMI_KCS_STATUS_REG_OEM2) {
            _dprintf (fd, "OEM2 ");
    }
    _dprintf (fd, "\n");
    return (0);
}

int
ipmi_smic_print_flags (int fd, uint8_t state)
{
    _dprintf (fd, "Current SMIC flags: %#x : ", state);
    if(state & IPMI_SMIC_RX_DATA_RDY) 
        _dprintf (fd, "RX_DATA_RDY ");
    if(state & IPMI_SMIC_TX_DATA_RDY)
        _dprintf (fd, "TX_DATA_RDY ");
    if(state & IPMI_SMIC_SMI)
        _dprintf (fd, "SMI ");
    if(state & IPMI_SMIC_EVT_ATN) 
        _dprintf (fd, "EVT_ATN ");
    if(state & IPMI_SMIC_SMS_ATN)
        _dprintf (fd, "SMS_ATN ");
    if(state & IPMI_SMIC_BUSY)
        _dprintf (fd, "BUSY ");
    _dprintf (fd, "\n");
    return (0);
}

void
ipmi_debug(const char *fmt, ...)
{
#if defined (IPMI_SYSLOG) || defined (IPMI_TRACE)
  va_list ap;
#endif
#if defined (IPMI_SYSLOG)
  char dbgstr[IPMI_ERR_STR_MAX_LEN];
  char errstr[IPMI_ERR_STR_MAX_LEN];
  int dbglen, errlen;
#endif

#if defined (IPMI_SYSLOG)
  va_start(ap, fmt);
  dbglen = snprintf (dbgstr, IPMI_ERR_STR_MAX_LEN, 
                     "%s: %d: %s: ", __FILE__, __LINE__, __PRETTY_FUNCTION__);
  errlen = vsnprintf (errstr, IPMI_ERR_STR_MAX_LEN, fmt, ap);
  strncat(dbgstr, errstr, IPMI_ERR_STR_MAX_LEN - dbglen - 1);
  syslog (LOG_MAKEPRI (LOG_FAC (LOG_LOCAL1), LOG_ERR), dbgstr);
#endif /* !IPMI_SYSLOG */

#if defined (IPMI_TRACE)
  fprintf (stderr, 
           "%s: %d: %s: ", __FILE__, __LINE__, __PRETTY_FUNCTION__);
  vfprintf (stderr, fmt, ap);
  fprintf (stderr, "\n");
  fflush (stderr);
#endif /* !IPMI_TRACE */ 
}
