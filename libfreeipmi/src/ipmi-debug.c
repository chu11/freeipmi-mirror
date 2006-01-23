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

  if (!fiid_obj_verify(obj))
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

      if (prefix)
        _DPRINTF_CLEANUP ((fd, "%s", prefix));
      
      if (!field_len)
        {
          fiid_iterator_next(iter);
          continue;
        }

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

#if 0 /* TEST */

int8_t
fiid_obj_dump_lan (int fd, char *prefix, char *hdr, uint8_t *pkt, uint32_t pkt_len, fiid_template_t tmpl_session, fiid_template_t tmpl_msg_hdr, fiid_template_t tmpl_cmd)
{
  uint32_t indx = 0;
  int32_t obj_cmd_len, obj_msg_trlr_len;
  uint8_t buf[IPMI_DEBUG_MAX_PKT_LEN];
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
  char *extra_hdr =
    "Unexpected Data:\n"
    "----------------";

  if (!(pkt && tmpl_session && tmpl_cmd))
    {
      errno = EINVAL;
      return (-1);
    }

  if (fiid_obj_dump_setup(fd, prefix, hdr, prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN) < 0)
    return (-1);

  /* Dump rmcp header */

  if ((pkt_len - indx) < fiid_obj_len_bytes (tmpl_hdr_rmcp))
    {
      ERR_EXIT(fiid_obj_len_bytes(tmpl_hdr_rmcp) < IPMI_DEBUG_MAX_PKT_LEN);
      memset(buf, '\0', IPMI_DEBUG_MAX_PKT_LEN);
      memcpy(buf, pkt + indx, (pkt_len - indx)); 
      ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, rmcp_hdr, NULL, buf, tmpl_hdr_rmcp) != -1);
    }
  else 
    ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, rmcp_hdr, NULL, pkt + indx, tmpl_hdr_rmcp) != -1);
  indx += fiid_obj_len_bytes (tmpl_hdr_rmcp);

  if (pkt_len <= indx)
    return 0;
  
  /* Dump session header */
  /* Output of session header depends on the auth code */

  if ((pkt_len - indx) < fiid_obj_field_end_bytes (tmpl_session, (uint8_t *)"auth_type"))
    {
      ERR_EXIT(fiid_obj_len_bytes(tmpl_session) < IPMI_DEBUG_MAX_PKT_LEN);
      memset(buf, '\0', IPMI_DEBUG_MAX_PKT_LEN);
      memcpy(buf, pkt + indx, (pkt_len - indx)); 
      ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, session_hdr, NULL, buf, tmpl_session) != -1);
    }
  else 
    {
      uint8_t auth_type;
      uint32_t auth_type_offset;

      auth_type_offset = fiid_obj_len_bytes (tmpl_hdr_rmcp) + fiid_obj_field_start_bytes (tmpl_session, (uint8_t *)"auth_type");
      auth_type = pkt[auth_type_offset];

      if (auth_type == IPMI_SESSION_AUTH_TYPE_NONE)
        tmpl_session = tmpl_hdr_session;
      else if (fiid_obj_field_lookup(tmpl_session, (uint8_t *)"auth_calc_data"))
        tmpl_session = tmpl_hdr_session_auth;

      if ((pkt_len - indx) < fiid_obj_len_bytes (tmpl_session))
        {
          ERR_EXIT(fiid_obj_len_bytes(tmpl_session) < IPMI_DEBUG_MAX_PKT_LEN);
          memset(buf, '\0', IPMI_DEBUG_MAX_PKT_LEN);
          memcpy(buf, pkt + indx, (pkt_len - indx)); 
          ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, session_hdr, NULL, buf, tmpl_session) != -1);
        }
      else 
        ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, session_hdr, NULL, pkt + indx, tmpl_session) != -1);
    }
  indx += fiid_obj_len_bytes (tmpl_session);

  if (pkt_len <= indx)
    return 0;

  /* Dump message header */

  if ((pkt_len - indx) < fiid_obj_len_bytes (tmpl_msg_hdr))
    {
      ERR_EXIT(fiid_obj_len_bytes(tmpl_msg_hdr) < IPMI_DEBUG_MAX_PKT_LEN);
      memset(buf, '\0', IPMI_DEBUG_MAX_PKT_LEN);
      memcpy(buf, pkt + indx, (pkt_len - indx)); 
      ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, msg_hdr, NULL, buf, tmpl_msg_hdr) != -1);
    }
  else 
    ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, msg_hdr, NULL, pkt + indx, tmpl_msg_hdr) != -1);
  indx += fiid_obj_len_bytes (tmpl_msg_hdr);

  if (pkt_len <= indx)
    return 0;

  obj_cmd_len = fiid_obj_len_bytes (tmpl_cmd);
  obj_msg_trlr_len = fiid_obj_len_bytes (tmpl_lan_msg_trlr);

  /* Dump command data */

  if ((pkt_len - indx) <= obj_cmd_len)
    {
      if ((pkt_len - indx) > obj_msg_trlr_len)
        obj_cmd_len = (pkt_len - indx) - obj_msg_trlr_len;
      else
        obj_cmd_len = (pkt_len - indx);
      ERR_EXIT(fiid_obj_len_bytes(tmpl_cmd) < IPMI_DEBUG_MAX_PKT_LEN);
      memset(buf, '\0', IPMI_DEBUG_MAX_PKT_LEN);
      memcpy(buf, pkt + indx, obj_cmd_len); 
      ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, cmd_hdr, NULL, buf, tmpl_cmd) != -1);
    }
  else 
    ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, cmd_hdr, NULL, pkt + indx, tmpl_cmd) != -1);
  indx += obj_cmd_len;

  if (pkt_len <= indx)
    return 0;

  /* Dump trailer */

  if ((pkt_len - indx) < obj_msg_trlr_len)
    {
      ERR_EXIT(fiid_obj_len_bytes(tmpl_lan_msg_trlr) < IPMI_DEBUG_MAX_PKT_LEN);
      memset(buf, '\0', IPMI_DEBUG_MAX_PKT_LEN);
      memcpy(buf, pkt + indx, (pkt_len - indx)); 
      ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, trlr_hdr, NULL, buf, tmpl_lan_msg_trlr) != -1);
    }
  else 
    ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, trlr_hdr, NULL, pkt + indx, tmpl_lan_msg_trlr) != -1);
  indx += obj_msg_trlr_len;

  if (pkt_len <= indx)
    return 0;

  /* Dump extra stuff if packet is extra long */

  if (_output_str(fd, prefix_buf, extra_hdr) < 0)
    return (-1);
  
  if (_output_byte_array(fd, prefix_buf, (uint8_t *)pkt+indx, (pkt_len - indx)) < 0)
    return (-1);

  return 0;
}

int8_t 
fiid_obj_dump_rmcp (int fd, char *prefix, char *hdr, uint8_t *pkt, uint32_t pkt_len, fiid_template_t tmpl_cmd)
{
  uint32_t indx = 0;
  uint8_t buf[IPMI_DEBUG_MAX_PKT_LEN];
  char prefix_buf[IPMI_DEBUG_MAX_PREFIX_LEN];
  char *rmcp_hdr = 
    "RMCP Header:\n"
    "------------";
  char *rmcp_cmd =
    "RMCP Command Data:\n"
    "------------------";
  char *extra_hdr =
    "Unexpected Data:\n"
    "----------------";

  if (!(pkt && tmpl_cmd))
    {
      errno = EINVAL;
      return (-1);
    }

  if (fiid_obj_dump_setup(fd, prefix, hdr, prefix_buf, IPMI_DEBUG_MAX_PREFIX_LEN) < 0)
    return (-1);

  /* Dump rmcp header */

  if ((pkt_len - indx) < fiid_obj_len_bytes (tmpl_hdr_rmcp))
    {
      ERR_EXIT(fiid_obj_len_bytes(tmpl_hdr_rmcp) < IPMI_DEBUG_MAX_PKT_LEN);
      memset(buf, '\0', IPMI_DEBUG_MAX_PKT_LEN);
      memcpy(buf, pkt + indx, (pkt_len - indx)); 
      ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, rmcp_hdr, NULL, buf, tmpl_hdr_rmcp) != -1);
    }
  else 
    ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, rmcp_hdr, NULL, pkt + indx, tmpl_hdr_rmcp) != -1);
  indx += fiid_obj_len_bytes (tmpl_hdr_rmcp);

  if (pkt_len <= indx)
    return 0;
  
  /* Dump command data */

  if ((pkt_len - indx) < fiid_obj_len_bytes (tmpl_cmd))
    {
      ERR_EXIT(fiid_obj_len_bytes(tmpl_cmd) < IPMI_DEBUG_MAX_PKT_LEN);
      memset(buf, '\0', IPMI_DEBUG_MAX_PKT_LEN);
      memcpy(buf, pkt + indx, (pkt_len - indx)); 
      ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, rmcp_cmd, NULL, buf, tmpl_cmd) != -1);
    }
  else 
    ERR_OUT(fiid_obj_dump_perror (fd, prefix_buf, rmcp_cmd, NULL, pkt + indx, tmpl_cmd) != -1);
  indx += fiid_obj_len_bytes (tmpl_cmd);

  if (pkt_len <= indx)
    return 0;

  /* Dump extra stuff if packet is extra long */

  if (_output_str(fd, prefix_buf, extra_hdr) < 0)
    return (-1);
  
  if (_output_byte_array(fd, prefix_buf, (uint8_t *)pkt+indx, (pkt_len - indx)) < 0)
    return (-1);

  return 0;
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

#endif /* TEST */
