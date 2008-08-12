/*****************************************************************************\
 *  $Id: ipmipower_wrappers.c,v 1.34 2008-08-12 18:14:42 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2003-2007 The Regents of the University of California.
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
 *  with Ipmipower.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <assert.h>
#include <errno.h>

#include "ipmipower_wrappers.h"

#include "freeipmi-portability.h"

cbuf_t 
Cbuf_create(int minsize, int maxsize) 
{
  cbuf_t c;
  if (!(c = cbuf_create(minsize, maxsize)))
    ierr_exit("Cbuf_create: %s", strerror(errno));
  cbuf_opt_set(c, CBUF_OPT_OVERWRITE, CBUF_WRAP_MANY);
  return c;
}

void 
Cbuf_drop_all(cbuf_t buf) 
{ 
  assert(buf);
  if (cbuf_drop(buf, -1) < 0)
    ierr_exit("Cbuf_drop: %s", strerror(errno));
}

void 
Cbuf_read_to_fd(cbuf_t buf, int fd) 
{
  assert(buf);
  if (cbuf_read_to_fd(buf, fd, -1) < 0)
    ierr_exit("Cbuf_read_to_fd(%d): %s", fd, strerror(errno));
}

void 
Cbuf_write_from_fd(cbuf_t buf, int fd) 
{
  int n, dropped = 0;

  assert(buf);

  if ((n = cbuf_write_from_fd(buf, fd, -1, &dropped)) < 0)
    ierr_exit("Cbuf_write_from_fd(%d): %s", fd, strerror(errno));
  
  /* achu: If you are running ipmipower in co-process mode with
   * powerman, this error condition will probably be hit with the file
   * descriptor STDIN_FILENO.  The powerman daemon is usually closed
   * by /etc/init.d/powerman stop, which kills a process through a
   * signal.  Thus, powerman closes stdin and stdout pipes to
   * ipmipower and the call to cbuf_write_from_fd will give us an EOF
   * reading.  We'll consider this EOF an "ok" error.
   */
  if (!n)
    {
      if (fd == STDIN_FILENO)
        exit(1);
      else
        ierr_exit("Cbuf_write_from_fd(%d): EOF", fd);
    }
  if (dropped)
    ierr_dbg("Cbuf_write_from_fd: read dropped %d bytes", dropped);
}

void 
Cbuf_write(cbuf_t buf, void *buffer, int len) 
{
  int rv, dropped = 0;
    
  assert(buf); 
  assert(buffer);
  assert(len > 0);

  if ((rv = cbuf_write(buf, buffer, len, &dropped)) < 0)
    ierr_exit("Cbuf_write: %s", strerror(errno));

  if (rv != len)
    ierr_exit("Cbuf_write: incorrect bytes written %d", rv);

  if (dropped)
    ierr_dbg("Cbuf_write: dropped %d bytes", dropped);
}

int 
Cbuf_peek_and_drop(cbuf_t buf, void *buffer, int len) 
{
  int rv, r_len, dropped = 0;

  assert(buf); 
  assert(buffer);
  assert(len > 0);

  if ((r_len = cbuf_peek(buf, buffer, len)) < 0)
    ierr_exit("Cbuf_peek: %s", strerror(errno));
    
  /* Nothing there */
  if (!r_len)
    return 0;

  if ((dropped = cbuf_drop(buf, len)) < 0)
    ierr_exit("Cbuf_peek: cbuf_drop: %s", strerror(errno));

  if (dropped != r_len)
    ierr_exit("Cbuf_peek: dropped incorrect bytes: %d", dropped);

  if ((rv = cbuf_drop(buf, -1)) < 0)
    ierr_exit("Cbuf_peek: cbuf_drop: %s", strerror(errno));

  if (rv > 0)
    ierr_dbg("Cbuf_peek: cbuf_drop dropped data: %d", rv);

  return r_len;
}

int
Cbuf_peek_to_fd(cbuf_t src, int dstfd, int len)
{
  int ret;

  assert(src);
  assert(dstfd > 2);

  if ((ret = cbuf_peek_to_fd(src, dstfd, len)) < 0)
    ierr_exit("Cbuf_peek_to_fd: %s", strerror(errno));

  return ret;
}

fiid_obj_t
Fiid_obj_create(fiid_template_t tmpl) 
{
  fiid_obj_t obj;

  assert(tmpl);

  if (!(obj = fiid_obj_create(tmpl)))
    ierr_exit("Fiid_obj_create: %s", strerror(errno));
  return obj;
}

void 
Fiid_obj_destroy(fiid_obj_t obj) 
{
  assert(fiid_obj_valid(obj));

  fiid_obj_destroy(obj);
}

void
Fiid_obj_clear(fiid_obj_t obj)
{
  int8_t rv;

  assert(fiid_obj_valid(obj));

  if ((rv = fiid_obj_clear(obj)) < 0)
    ierr_exit("Fiid_obj_clear: %s", strerror(errno)); 
}

void
Fiid_obj_clear_field(fiid_obj_t obj, char *field)
{
  int8_t rv;

  assert(fiid_obj_valid(obj) && field);

  if ((rv = fiid_obj_clear_field(obj, field)) < 0)
    ierr_exit("Fiid_obj_clear_field: %s", strerror(errno)); 
}

void
Fiid_obj_get(fiid_obj_t obj, char *field, uint64_t *val)
{
  int8_t rv;

  assert(fiid_obj_valid(obj) && field && val);

  if ((rv = fiid_obj_get(obj, field, val)) < 0)
    ierr_exit("Fiid_obj_get: field=%s: %s", field, fiid_strerror(fiid_obj_errnum(obj)));

  if (!rv)
    ierr_exit("Fiid_obj_get: field=%s: No data set", field);
  
  return;
}

int32_t 
Fiid_obj_get_data(fiid_obj_t obj, char *field, uint8_t *data, uint32_t data_len)
{
  int32_t rv;

  assert(fiid_obj_valid(obj) && field && data && data_len);

  if ((rv = fiid_obj_get_data(obj, field, data, data_len)) < 0)
    ierr_exit("Fiid_obj_get_data: field=%s: %s", field, fiid_strerror(fiid_obj_errnum(obj)));

  return rv;
}

int32_t 
Fiid_obj_set_data(fiid_obj_t obj, char *field, uint8_t *data, uint32_t data_len)
{
  int32_t rv;
  
  assert(fiid_obj_valid(obj) && field && data && data_len);
  
  if ((rv = fiid_obj_set_data(obj, field, data, data_len)) < 0)
    ierr_exit("Fiid_obj_set_data: field=%s: %s", field, fiid_strerror(fiid_obj_errnum(obj)));
  
  return rv;
}

int32_t 
Fiid_obj_set_all(fiid_obj_t obj, uint8_t *data, uint32_t data_len)
{
  int32_t rv;

  assert(fiid_obj_valid(obj) && data && data_len);

  if ((rv = fiid_obj_set_all(obj, data, data_len)) < 0)
    ierr_exit("Fiid_obj_set_all: %s", fiid_strerror(fiid_obj_errnum(obj)));

  return rv;
}
