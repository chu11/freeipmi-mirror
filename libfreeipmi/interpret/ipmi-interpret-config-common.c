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
/*****************************************************************************\
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
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

#include "ipmi-interpret-config-common.h"

#include "freeipmi-portability.h"
#include "conffile.h"

int
interpret_config_parse_state (conffile_t cf,
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
interpret_config_parse_strtoul (conffile_t cf,
				const char *str,
				uint32_t max,
				uint32_t *value)
{
  char *endptr = NULL;
  
  assert (cf);
  assert (str);
  assert (value);
  
  errno = 0;

  (*value) = strtoul (str, &endptr, 0);

  if (errno
      || endptr[0] != '\0'
      || (*value) > max)
    {
      conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_INVALID);
      return (-1);
    }
  
  return (0);
}

int
interpret_config_parse_manufactuer_id_product_id (conffile_t cf,
						  const char *str,
						  struct ipmi_interpret_config_file_ids ids[IPMI_INTERPRET_CONFIG_FILE_MANUFACTURER_ID_MAX],
						  unsigned int *ids_count)
{
  char *tmpstr = NULL;
  char *manufacturer_id_ptr;
  char *manufacturer_id_lasts;
  unsigned int i;
  int rv = -1;

  assert (cf);
  assert (str);
  assert (ids_count);

  (*ids_count) = 0;

  if (!(tmpstr = strdup (str)))
    {
      conffile_seterrnum (cf, CONFFILE_ERR_OUTMEM);
      goto cleanup;
    }

  manufacturer_id_ptr = strtok_r (tmpstr, ",", &manufacturer_id_lasts);
  while (manufacturer_id_ptr && (*ids_count) < IPMI_INTERPRET_CONFIG_FILE_MANUFACTURER_ID_MAX)
    {
      char *product_ids_ptr;
      char *ptr;
      uint32_t tmp;

      if (!(ptr = strchr (manufacturer_id_ptr, ':')))
        {
          conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_INVALID);
          goto cleanup;
        }
      
      (*ptr) = '\0';
      product_ids_ptr = ptr + 1;
      
      if (interpret_config_parse_strtoul (cf,
					  manufacturer_id_ptr,
					  0x00FFFFFF,  /* 24 bit manufacturer ID */
					  &tmp) < 0)
        goto cleanup;
      ids[(*ids_count)].manufacturer_id = tmp;
      
      if ((ptr = strchr (product_ids_ptr, '-')))
        {
          char *product_id1_ptr;
          char *product_id2_ptr;
          uint16_t product_id1;
          uint16_t product_id2;
          
          product_id1_ptr = product_ids_ptr;
          (*ptr) = '\0';
          product_id2_ptr = ptr + 1;

          if (interpret_config_parse_strtoul (cf,
					      product_id1_ptr,
					      USHRT_MAX,
					      &tmp) < 0)
            goto cleanup;
          product_id1 = tmp;

          if (interpret_config_parse_strtoul (cf,
					      product_id2_ptr,
					      USHRT_MAX,
					      &tmp) < 0)
            goto cleanup;
          product_id2 = tmp;

          if (product_id1 > product_id2)
            {
              conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_INVALID);
              return (-1);
            }
          
          if ((product_id2 - product_id1 + 1) > IPMI_INTERPRET_CONFIG_FILE_PRODUCT_ID_MAX)
            {
              conffile_seterrnum (cf, CONFFILE_ERR_PARSE_ARG_TOOMANY);
              return (-1);
            }

          for (i = 0; i < (product_id2 - product_id1 + 1) ; i++)
            ids[(*ids_count)].product_ids[i] = product_id1 + i;
          ids[(*ids_count)].product_ids_count = product_id2 - product_id1 + 1;
        }
      else if ((ptr = strchr (product_ids_ptr, '+')))
        {  
          unsigned int index = 0;
          uint16_t product_id;

          while ((ptr = strchr (product_ids_ptr, '+'))
                 && index < IPMI_INTERPRET_CONFIG_FILE_PRODUCT_ID_MAX)
            {
              char *product_id_ptr;
              
              product_id_ptr = product_ids_ptr;
              (*ptr) = '\0';
              product_ids_ptr = ptr + 1;

              if (interpret_config_parse_strtoul (cf,
						  product_id_ptr,
						  USHRT_MAX,
						  &tmp) < 0)
                goto cleanup;
              product_id = tmp;
              
              ids[(*ids_count)].product_ids[index] = product_id;
              
              index++;
            }

          if (interpret_config_parse_strtoul (cf,
					      product_ids_ptr,
					      USHRT_MAX,
					      &tmp) < 0)
            goto cleanup;
          product_id = tmp;
          
          ids[(*ids_count)].product_ids[index] = product_id;
          
          index++;

          ids[(*ids_count)].product_ids_count = index;
        }
      else
        {
          if (interpret_config_parse_strtoul (cf,
					      product_ids_ptr,
					      USHRT_MAX,
					      &tmp) < 0)
            goto cleanup;
          ids[(*ids_count)].product_ids[0] = tmp;
          ids[(*ids_count)].product_ids_count = 1;
        }

      (*ids_count)++;

      manufacturer_id_ptr = strtok_r (NULL, ",", &manufacturer_id_lasts);
    }

  rv = 0;
 cleanup:
  free (tmpstr);
  return (rv);
}

