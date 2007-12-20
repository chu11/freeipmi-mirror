/*****************************************************************************\
 *  $Id: ipmi_sdr_cache_delete.c,v 1.1.2.2 2007-12-20 23:25:41 chu11 Exp $
 *****************************************************************************
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
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <errno.h>

#include "ipmi_sdr_cache.h"
#include "ipmi_sdr_cache_defs.h"

int 
ipmi_sdr_cache_delete(ipmi_sdr_cache_ctx_t c, char *filename)
{
  if (!c || c->magic != IPMI_SDR_CACHE_MAGIC)
    return -1;
  
  if (!filename)
    {
      c->errnum = IPMI_SDR_CACHE_ERR_PARAMETERS;
      return -1;
    }

  if (c->operation != IPMI_SDR_CACHE_OPERATION_UNINITIALIZED)
    {
      if (c->operation == IPMI_SDR_CACHE_OPERATION_READ_CACHE)
        c->errnum = IPMI_SDR_CACHE_ERR_CACHE_DELETE_CTX_SET_TO_READ;
      else
        c->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
      return -1;
    }

  if (unlink(filename) < 0)
    {
      if (errno == EPERM 
          || errno == EACCES
          || errno == EROFS)
        c->errnum = IPMI_SDR_CACHE_ERR_PERMISSION;
      else if (errno == EISDIR
               || errno == ENAMETOOLONG
               || errno == ENOENT
               || errno == ELOOP)
        c->errnum = IPMI_SDR_CACHE_ERR_FILENAME_INVALID;
      else
        c->errnum = IPMI_SDR_CACHE_ERR_INTERNAL_ERROR;
      return -1;
    }
  
  c->errnum = IPMI_SDR_CACHE_ERR_SUCCESS;
  return 0;
}
