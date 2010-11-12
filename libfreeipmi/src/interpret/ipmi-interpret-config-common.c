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

int
ipmi_interpret_config_parse_manufactuer_id_product_id (conffile_t cf,
                                                       const char *str,
                                                       uint32_t manufacturer_ids[IPMI_INTERPRET_CONFIG_FILE_ID_MAX],
                                                       uint16_t product_ids[IPMI_INTERPRET_CONFIG_FILE_ID_MAX],
                                                       unsigned int *ids_count)
{
  char *tmpstr = NULL;
  char *manufacturer_id_ptr;
  char *product_id_ptr;
  int rv = -1;
  char *lasts;

  assert (cf);
  assert (str);
  assert (ids_count);

  (*ids_count) = 0;

  if (!(tmpstr = strdup (str)))
    {
      conffile_seterrnum (cf, CONFFILE_ERR_OUTMEM);
      goto cleanup;
    }

  manufacturer_id_ptr = strtok_r (tmpstr, ",", &lasts);
  while (manufacturer_id_ptr && (*ids_count) < IPMI_INTERPRET_CONFIG_FILE_ID_MAX)
    {
      char *ptr;
      uint32_t tmp;

      if (!(ptr = strchr (manufacturer_id_ptr, ':')))
        {
          conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_INVALID);
          goto cleanup;
        }
      
      (*ptr) = '\0';
      product_id_ptr = ptr + 1;
      
      if (ipmi_interpret_config_parse_strtoul (cf,
                                               manufactuer_id_ptr,
                                               0x00FFFFFF,  /* 24 bit manufacturer ID */
                                               &tmp) < 0)
        goto cleanup;
      manufacturer_ids[(*ids_count)] = tmp;
      
      if (ipmi_interpret_config_parse_strtoul (cf,
                                               product_id_ptr,
                                               USHRT_MAX,
                                               &tmp) < 0)
        goto cleanup;
      product_ids[(*ids_count)] = tmp;

      (*ids_count)++;

      tok = strtok_r (NULL, ",", &lasts);
    }

  rv = 0;
 cleanup:
  free (tmpstr);
  return (rv);
}

