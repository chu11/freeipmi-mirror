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

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include <ipmidetect.h>

#include "freeipmi-portability.h"
#include "hostlist.h"
#include "pstdout.h"
#include "tool-hostrange-common.h"

#define HOSTLIST_BUFLEN 1024

static int
eliminate_nodes (char **hosts)
{
  hostlist_t hl = NULL;
  hostlist_t hlnew = NULL;
  hostlist_iterator_t hitr = NULL;
  ipmidetect_t id = NULL;
  char *host = NULL;
  char hostbuf[HOSTLIST_BUFLEN];
  int rv = -1;

  assert (hosts);
  assert (*hosts);

  if (!(id = ipmidetect_handle_create ()))
    {
      fprintf (stderr,
               "ipmidetect_handle_create\n");
      goto cleanup;
    }

  if (ipmidetect_load_data (id,
                            NULL,
                            0,
                            0) < 0)
    {
      if (ipmidetect_errnum (id) == IPMIDETECT_ERR_CONNECT
          || ipmidetect_errnum (id) == IPMIDETECT_ERR_CONNECT_TIMEOUT)
        fprintf (stderr,
                 "Error connecting to ipmidetect daemon\n");
      else
        fprintf (stderr,
                 "ipmidetect_load_data: %s\n", ipmidetect_errormsg (id));
      goto cleanup;
    }

  if (!(hl = hostlist_create (*hosts)))
    {
      fprintf (stderr,
               "hostlist_create: %s\n",
               strerror (errno));
      goto cleanup;
    }

  if (!(hlnew = hostlist_create (*hosts)))
    {
      fprintf (stderr,
               "hostlist_create: %s\n",
               strerror (errno));
      goto cleanup;
    }

  if (!(hitr = hostlist_iterator_create (hl)))
    {
      fprintf (stderr,
               "hostlist_iterator_create: %s\n",
               strerror (errno));
      goto cleanup;
    }

  while ((host = hostlist_next (hitr)))
    {
      int ret;

      if ((ret = ipmidetect_is_node_detected (id, host)) < 0)
        {
          if (ipmidetect_errnum (id) == IPMIDETECT_ERR_NOTFOUND)
            fprintf (stderr,
                     "Node '%s' unrecognized by ipmidetect\n", host);
          else
            fprintf (stderr,
                     "ipmidetect_is_node_detected: %s\n", ipmidetect_errormsg (id));
          goto cleanup;
        }

      if (!ret)
        hostlist_delete (hlnew, host);

      free (host);
    }
  host = NULL;

  if (!hostlist_count (hlnew))
    {
      rv = 0;
      goto cleanup;
    }
  
  if (hostlist_ranged_string (hlnew, HOSTLIST_BUFLEN, hostbuf) < 0)
    {
      fprintf (stderr,
               "hostlist_ranged_string: truncation\n");
      goto cleanup;
    }

  free (*hosts);
  if (!(*hosts = strdup (hostbuf)))
    {
      fprintf (stderr, "strdup: %s\n", strerror (errno));
      goto cleanup;
    }

  rv = hostlist_count (hlnew);
 cleanup:
  if (id)
    ipmidetect_handle_destroy (id);
  if (hitr)
    hostlist_iterator_destroy (hitr);
  if (hl)
    hostlist_destroy (hl);
  if (hlnew)
    hostlist_destroy (hlnew);
  free (host);
  return (rv);
}

int
pstdout_setup (char **hosts, struct common_cmd_args *common_args)
{
  unsigned int output_flags = 0;
  int hosts_count = 0;

  assert (hosts);
  assert (common_args);

  if (pstdout_init () < 0)
    {
      fprintf (stderr,
               "pstdout_init: %s\n",
               pstdout_strerror (pstdout_errnum));
      goto cleanup;
    }

  if (*hosts)
    {
      if ((hosts_count = pstdout_hostnames_count (*hosts)) < 0)
        {
          fprintf (stderr,
                   "pstdout_hostnames_count: %s\n",
                   pstdout_strerror (pstdout_errnum));
          goto cleanup;
        }

      if (!hosts_count)
        {
          fprintf (stderr,
                   "invalid number of hosts specified\n");
          goto cleanup;
        }
    }
  else /* inband communication, hosts_count = 1 */
    {
      hosts_count = 1;

      /* if always prefix - turn hostname into "localhost" for prefixing */
      if (common_args->always_prefix)
        {
          if (!(*hosts = strdup ("localhost")))
            {
              fprintf (stderr, "strdup: %s\n", strerror (errno));
              goto cleanup;
            }
        }
    }

  /* if hosts_count > 1 it is always prefixed, so ignore always_prefixed flag */
  if (hosts_count > 1)
    {
      if (common_args->buffer_output)
        output_flags = PSTDOUT_OUTPUT_STDOUT_DEFAULT | PSTDOUT_OUTPUT_BUFFER_STDOUT | PSTDOUT_OUTPUT_STDERR_PREPEND_HOSTNAME;
      else if (common_args->consolidate_output)
        output_flags = PSTDOUT_OUTPUT_STDOUT_DEFAULT | PSTDOUT_OUTPUT_STDOUT_CONSOLIDATE | PSTDOUT_OUTPUT_STDERR_PREPEND_HOSTNAME;
      else
        output_flags = PSTDOUT_OUTPUT_STDOUT_PREPEND_HOSTNAME | PSTDOUT_OUTPUT_STDERR_PREPEND_HOSTNAME;

      if (common_args->fanout)
        {
          if (pstdout_set_fanout (common_args->fanout) < 0)
            {
              fprintf (stderr,
                       "pstdout_set_fanout: %s\n",
                       pstdout_strerror (pstdout_errnum));
              goto cleanup;
            }
        }
    }
  else if (hosts_count == 1 && common_args->always_prefix)
    output_flags = PSTDOUT_OUTPUT_STDOUT_PREPEND_HOSTNAME | PSTDOUT_OUTPUT_STDERR_PREPEND_HOSTNAME;

  if (output_flags)
    {
      if (pstdout_set_output_flags (output_flags) < 0)
        {
          fprintf (stderr,
                   "pstdout_set_output_flags: %s\n",
                   pstdout_strerror (pstdout_errnum));
          goto cleanup;
        }
    }

  if (*hosts && common_args->eliminate)
    {
      int hosts_count_new;
      
      if ((hosts_count_new = eliminate_nodes (hosts)) < 0)
        goto cleanup;

      hosts_count = hosts_count_new;
    }

  return (hosts_count);

 cleanup:
  return (-1);
}
