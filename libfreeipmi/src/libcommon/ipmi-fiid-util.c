/* 
   Copyright (C) 2003-2009 FreeIPMI Core Team

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
set_errno_by_fiid_object(fiid_obj_t obj)
{
  if (!fiid_obj_valid(obj))
    {
      SET_ERRNO(EINVAL);
      return;
    }

  if (fiid_obj_errnum(obj) == FIID_ERR_SUCCESS)
    errno = 0;
  else if (fiid_obj_errnum(obj) == FIID_ERR_OUT_OF_MEMORY)
    errno = ENOMEM;
  else if (fiid_obj_errnum(obj) == FIID_ERR_OVERFLOW)
    errno = ENOSPC;
  else
    errno = EINVAL;
}

void 
set_errno_by_fiid_iterator(fiid_iterator_t iter)
{
  if (!iter)
    {
      SET_ERRNO(EINVAL);
      return;
    }

  if (fiid_iterator_errnum(iter) == FIID_ERR_SUCCESS)
    errno = 0;
  else if (fiid_iterator_errnum(iter) == FIID_ERR_OUT_OF_MEMORY)
    errno = ENOMEM;
  else if (fiid_iterator_errnum(iter) == FIID_ERR_OVERFLOW)
    errno = ENOSPC;
  else
    errno = EINVAL;
}

int
Fiid_obj_template_compare(fiid_obj_t obj, fiid_template_t tmpl)
{
  int ret;
  
  if ((ret = fiid_obj_template_compare(obj, tmpl)) < 0)
    {
      FIID_OBJECT_ERROR_TO_ERRNO(obj);
      return (-1);
    }
  if (!ret)
    {
      errno = EINVAL;
      return (-1);
    }
  return (0);
}
