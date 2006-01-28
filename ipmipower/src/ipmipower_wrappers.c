/*****************************************************************************\
 *  $Id: ipmipower_wrappers.c,v 1.4.2.1 2006-01-28 20:45:19 chu11 Exp $
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
 *  59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif
#include <errno.h>
#include <assert.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "ipmipower_wrappers.h"

cbuf_t 
Cbuf_create(int minsize, int maxsize) 
{
  cbuf_t c;
  if ((c = cbuf_create(minsize, maxsize)) == NULL)
    err_exit("Cbuf_create: %s", strerror(errno));
  cbuf_opt_set(c, CBUF_OPT_OVERWRITE, CBUF_WRAP_MANY);
  return c;
}

void 
Cbuf_drop_all(cbuf_t buf) 
{ 
  assert(buf != NULL);
  if (cbuf_drop(buf, -1) < 0)
    err_exit("Cbuf_drop: %s", strerror(errno));
}

void 
Cbuf_read_to_fd(cbuf_t buf, int fd) 
{
  assert(buf != NULL);
  if (cbuf_read_to_fd(buf, fd, -1) < 0)
    err_exit("Cbuf_read_to_fd(%d): %s", fd, strerror(errno));
}

void 
Cbuf_write_from_fd(cbuf_t buf, int fd) 
{
  int n, dropped = 0;

  assert(buf != NULL);

  if ((n = cbuf_write_from_fd(buf, fd, -1, &dropped)) < 0)
    err_exit("Cbuf_write_from_fd(%d): %s", fd, strerror(errno));
  
  /* achu: If you are running ipmipower in co-process mode with
   * powerman, this error condition will probably be hit with the file
   * descriptor STDIN_FILENO.  The powerman daemon is usually closed
   * by /etc/init.d/powerman stop, which kills a process through a
   * signal.  Thus, powerman closes stdin and stdout pipes to
   * ipmipower and the call to cbuf_write_from_fd will give us an EOF
   * reading.  We'll consider this EOF an "ok" error.
   */
  if (n == 0)
    {
      if (fd == STDIN_FILENO)
        exit(1);
      else
        err_exit("Cbuf_write_from_fd(%d): EOF", fd);
    }
  if (dropped != 0)
    err_output("Cbuf_write_from_fd: read dropped %d bytes", dropped);
}

void 
Cbuf_write(cbuf_t buf, void *buffer, int len) 
{
  int rv, dropped = 0;
    
  assert(buf != NULL); 
  assert(buffer != NULL);
  assert(len > 0);

  if ((rv = cbuf_write(buf, buffer, len, &dropped)) < 0)
    err_exit("Cbuf_write: %s", strerror(errno));

  if (rv != len)
    err_exit("Cbuf_write: incorrect bytes written %d", rv);

  if (dropped != 0)
    err_output("Cbuf_write: dropped %d bytes", dropped);
}

int 
Cbuf_peek_and_drop(cbuf_t buf, void *buffer, int len) 
{
  int rv, r_len, dropped = 0;

  assert(buf != NULL); 
  assert(buffer != NULL);
  assert(len > 0);

  if ((r_len = cbuf_peek(buf, buffer, len)) < 0)
    err_exit("Cbuf_peek: %s", strerror(errno));
    
  /* Nothing there */
  if (!r_len)
    return 0;

  if ((dropped = cbuf_drop(buf, len)) < 0)
    err_exit("Cbuf_peek: cbuf_drop: %s", strerror(errno));

  if (dropped != r_len)
    err_exit("Cbuf_peek: dropped incorrect bytes: %d", dropped);

  if ((rv = cbuf_drop(buf, -1)) < 0)
    err_exit("Cbuf_peek: cbuf_drop: %s", strerror(errno));

  if (rv > 0)
    err_output("Cbuf_peek: cbuf_drop dropped data: %d", rv);

  return r_len;
}

int
Cbuf_peek_to_fd(cbuf_t src, int dstfd, int len)
{
  int ret;

  assert(src != NULL);
  assert(dstfd > 2);

  if ((ret = cbuf_peek_to_fd(src, dstfd, len)) < 0)
    err_exit("Cbuf_peek_to_fd: %s", strerror(errno));

  return ret;
}

void * 
Fiid_obj_calloc(fiid_template_t tmpl) 
{
  void *ptr;
  assert(tmpl != NULL);

  if ((ptr = fiid_obj_calloc(tmpl)) == NULL)
    err_exit("Fiid_obj_calloc: %s", strerror(errno));
  return ptr;
}

void * 
Fiid_obj_memset(fiid_obj_t obj, int c, fiid_template_t tmpl)
{
  void *ptr;
  assert(obj != NULL && tmpl != NULL);
  
  if ((ptr = fiid_obj_memset(obj, c, tmpl)) == NULL)
    err_exit("Fiid_obj_memset: %s", strerror(errno)); 
  
  return obj;
}

void 
Fiid_obj_free(fiid_obj_t obj) 
{
  fiid_obj_free(obj);
}

void
Fiid_obj_get(fiid_obj_t obj, fiid_template_t tmpl, uint8_t *field, uint64_t *val)
{
  assert(obj !=NULL && tmpl !=NULL && field != NULL && val !=NULL);

  if (fiid_obj_get(obj, tmpl, field, val) < 0)
    err_exit("Fiid_obj_get: %s", strerror(errno));
}

void 
Fiid_obj_dump_lan(int fd, char *prefix, char *hdr, uint8_t *pkt, uint32_t pkt_len, fiid_template_t tmpl_session, fiid_template_t tmpl_msg_hdr, fiid_template_t tmpl_cmd) 
{
  assert(pkt != NULL && tmpl_session != NULL && tmpl_msg_hdr != NULL 
         && tmpl_cmd != NULL);

  if (fiid_obj_dump_lan(fd, prefix, hdr, pkt, pkt_len, tmpl_session,
                        tmpl_msg_hdr, tmpl_cmd) < 0)
    err_exit("Fiid_obj_dump_lan: %s", strerror(errno));
}

void 
Fiid_obj_dump_rmcp(int fd, char *prefix, char *hdr, uint8_t *pkt, uint32_t pkt_len, fiid_template_t tmpl_cmd) 
{
  assert(pkt != NULL && tmpl_cmd != NULL);

  if (fiid_obj_dump_rmcp(fd, prefix, hdr, pkt, pkt_len, tmpl_cmd) < 0)
    err_exit("Fiid_obj_dump_lan: %s", strerror(errno));
}
