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
#endif /* STDC_HEADERS */
#include <errno.h>

#include "freeipmi/fiid/fiid.h"

#include "ipmi-fiid-util.h"
#include "ipmi-trace.h"

#include "freeipmi-portability.h"

void
set_errno_by_fiid_object (fiid_obj_t obj)
{
  if (fiid_obj_errnum (obj) == FIID_ERR_SUCCESS)
    errno = 0;
  else if (fiid_obj_errnum (obj) == FIID_ERR_OUT_OF_MEMORY)
    errno = ENOMEM;
  else if (fiid_obj_errnum (obj) == FIID_ERR_OVERFLOW)
    errno = ENOSPC;
#if 0
  else if (fiid_obj_errnum (obj) == FIID_ERR_DATA_NOT_AVAILABLE)
    errno = EINVAL;
  else if (fiid_obj_errnum (obj) == FIID_ERR_FIELD_NOT_FOUND
           || fiid_obj_errnum (obj) == FIID_ERR_DATA_NOT_BYTE_ALIGNED
           || fiid_obj_errnum (obj) == FIID_ERR_REQUIRED_FIELD_MISSING
           || fiid_obj_errnum (obj) == FIID_ERR_FIXED_LENGTH_FIELD_INVALID
           || fiid_obj_errnum (obj) == FIID_ERR_DATA_NOT_AVAILABLE
           || fiid_obj_errnum (obj) == FIID_ERR_NOT_IDENTICAL)
    errno = EINVAL;
#endif
  else
    errno = EINVAL;
}

void
set_errno_by_fiid_iterator (fiid_iterator_t iter)
{
  if (fiid_iterator_errnum (iter) == FIID_ERR_SUCCESS)
    errno = 0;
  else if (fiid_iterator_errnum (iter) == FIID_ERR_OUT_OF_MEMORY)
    errno = ENOMEM;
  else if (fiid_iterator_errnum (iter) == FIID_ERR_OVERFLOW)
    errno = ENOSPC;
  else
    errno = EINVAL;
}
