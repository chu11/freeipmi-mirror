/*
 * Copyright (C) 2003-2010 FreeIPMI Core Team
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
/*****************************************************************************\
 *  Copyright (C) 2007-2010 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
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
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <limits.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/interpret/ipmi-interpret.h"

#include "freeipmi-portability.h"
#include "conffile.h"

int
ipmi_interpret_config_parse_state (conffile_t cf,
                                   char *option_string)
{
  assert (cf);
  assert (option_string);

  if (!strcasecmp (option_string, "Nominal"))
    return (IPMI_INTERPRET_STATE_NOMINAL);
  else if (!strcasecmp (option_string, "Warning"))
    return (IPMI_INTERPRET_STATE_WARNING);
  else if (!strcasecmp (option_string, "Critical"))
    return (IPMI_INTERPRET_STATE_CRITICAL);

  conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_INVALID);
  return (-1);
}

int
ipmi_interpret_config_parse_strtoul (conffile_t cf,
                                     const char *str,
                                     uint32_t max,
                                     uint32_t *value)
{
  char *ptr = NULL;
  
  assert (cf);
  assert (str);
  assert (value);
  
  errno = 0;

  (*value) = strtoul (str, &ptr, 0);

  if (errno
      || (*ptr) != '\0'
      || (*value) > max)
    {
      conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_INVALID);
      return (-1);
    }
  
  return (0);
}
