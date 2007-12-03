/*****************************************************************************\
 *  $Id: ipmi_monitoring_fiid_wrappers.c,v 1.7.2.2 2007-12-03 02:27:35 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-222073
 *
 *  This file is part of Ipmimonitoring, an IPMI sensor monitoring
 *  library.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmimonitoring is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmimonitoring is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmimonitoring.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "ipmi_monitoring.h"
#include "ipmi_monitoring_debug.h"
#include "ipmi_monitoring_defs.h"
#include "ipmi_monitoring_fiid_wrappers.h"

int32_t
Fiid_template_len_bytes(ipmi_monitoring_ctx_t c, fiid_template_t tmpl)
{
  int32_t rv;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(tmpl);

  if ((rv = fiid_template_len_bytes(tmpl)) < 0)
    {
      IPMI_MONITORING_DEBUG(("fiid_template_len_bytes: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return -1;
    }

  return rv;
}

int32_t
Fiid_template_block_len_bytes(ipmi_monitoring_ctx_t c, fiid_template_t tmpl, char *field_start, char *field_end)
{
  int32_t rv;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(tmpl);
  assert(field_start);
  assert(field_end);

  if ((rv = fiid_template_block_len_bytes(tmpl, field_start, field_end)) < 0)
    {
      IPMI_MONITORING_DEBUG(("fiid_template_len_bytes: field_start=%s; field_end=%s; %s", field_start, field_end, strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return -1;
    }

  return rv;
}

fiid_obj_t
Fiid_obj_create(ipmi_monitoring_ctx_t c, fiid_template_t tmpl) 
{
  fiid_obj_t obj;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(tmpl);

  if (!(obj = fiid_obj_create(tmpl)))
    {
      IPMI_MONITORING_DEBUG(("fiid_obj_create: %s", strerror(errno)));
      c->errnum = IPMI_MONITORING_ERR_OUT_OF_MEMORY;
      return NULL;
    }

  return obj;
}

int8_t
Fiid_obj_clear(ipmi_monitoring_ctx_t c, fiid_obj_t obj)
{
  int8_t rv;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(fiid_obj_valid(obj));

  if ((rv = fiid_obj_clear(obj)) < 0)
    {
      IPMI_MONITORING_DEBUG(("fiid_obj_clear: %s", fiid_strerror(fiid_obj_errnum(obj))));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return -1;
    }

  return rv;
}

void
Fiid_obj_destroy(ipmi_monitoring_ctx_t c, fiid_obj_t obj) 
{
  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(fiid_obj_valid(obj));

  fiid_obj_destroy(obj);
}

int8_t
Fiid_obj_get(ipmi_monitoring_ctx_t c, fiid_obj_t obj, char *field, uint64_t *val)
{
  int8_t rv;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(fiid_obj_valid(obj));
  assert(field);
  assert(val);

  if ((rv = fiid_obj_get(obj, field, val)) < 0)
    {
      IPMI_MONITORING_DEBUG(("fiid_obj_get: field=%s; %s", field, fiid_strerror(fiid_obj_errnum(obj))));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return -1;
    }

  if (!rv)
    {
      IPMI_MONITORING_DEBUG(("fiid_obj_get: field=%s; no data set", field));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return -1;
    }
  
  return rv;
}

int32_t 
Fiid_obj_get_data(ipmi_monitoring_ctx_t c, fiid_obj_t obj, char *field, uint8_t *data, uint32_t data_len)
{
  int32_t rv;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(fiid_obj_valid(obj));
  assert(field);
  assert(data);
  assert(data_len);

  if ((rv = fiid_obj_get_data(obj, field, data, data_len)) < 0)
    {
      IPMI_MONITORING_DEBUG(("fiid_obj_get_data: field=%s; %s", field, fiid_strerror(fiid_obj_errnum(obj))));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return -1;
    }

  return rv;
}

int8_t
Fiid_obj_set(ipmi_monitoring_ctx_t c, fiid_obj_t obj, char *field, uint64_t val)
{
  int8_t rv;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(fiid_obj_valid(obj));
  assert(field);

  if ((rv = fiid_obj_set(obj, field, val)) < 0)
    {
      IPMI_MONITORING_DEBUG(("fiid_obj_set: field=%s; %s", field, fiid_strerror(fiid_obj_errnum(obj))));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return -1;
    }

  return rv;
}

int32_t 
Fiid_obj_set_all(ipmi_monitoring_ctx_t c, fiid_obj_t obj, uint8_t *data, uint32_t data_len)
{
  int32_t rv;

  assert(c);
  assert(c->magic == IPMI_MONITORING_MAGIC);
  assert(fiid_obj_valid(obj));
  assert(data);
  assert(data_len);

  if ((rv = fiid_obj_set_all(obj, data, data_len)) < 0)
    {
      IPMI_MONITORING_DEBUG(("fiid_obj_set_all: %s", fiid_strerror(fiid_obj_errnum(obj))));
      c->errnum = IPMI_MONITORING_ERR_INTERNAL_ERROR;
      return -1;
    }

  return rv;
}
