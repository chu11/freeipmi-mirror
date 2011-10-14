/*****************************************************************************\
 *  $Id: ipmipower_output.c,v 1.58 2010-02-08 22:02:31 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2011 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 3 of the License, or (at your
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
#include <assert.h>

#include "ipmipower.h"
#include "ipmipower_error.h"
#include "ipmipower_output.h"
#include "ipmipower_util.h"

#include "freeipmi-portability.h"
#include "cbuf.h"
#include "hostlist.h"

extern cbuf_t ttyout;
extern struct ipmipower_arguments cmd_args;
extern hostlist_t output_hostrange[MSG_TYPE_NUM_ENTRIES];

static char *ipmipower_outputs[] =
  {
    "on",
    "off",
    "ok",
    "unknown",
    "username invalid",
    "password invalid",
    "password length invalid",
    "k_g invalid",
    "privilege level cannot be obtained for this user",
    "operation invalid",
    "authentication type unavailable for attempted privilege level",
    "cipher suite id unavailable",
    "password verification timeout",
    "connection timeout",
    "session timeout",
    "not discovered",
    "bad connection",
    "invalid hostname",
    "unconfigured hostname",
    "out of resources",
    "ipmi 2.0 unavailable",
    "BMC busy",
    "BMC error"
  };

void
ipmipower_output (msg_type_t num, const char *hostname)
{
  assert (MSG_TYPE_VALID (num));
  assert (hostname);

  if (cmd_args.hostrange.consolidate_output)
    {
      if (!hostlist_push_host (output_hostrange[num], hostname))
        {
          IPMIPOWER_ERROR (("hostlist_push_host: %s", strerror(errno)));
          exit (1);
        }
    }
  else
    ipmipower_cbuf_printf (ttyout,
                           "%s: %s\n",
                           hostname,
                           ipmipower_outputs[num]);

  return;
}

void
ipmipower_output_finish (void)
{
  if (cmd_args.hostrange.consolidate_output)
    {
      int i, rv;
      char buf[IPMIPOWER_OUTPUT_BUFLEN];

      for (i = 0; i < MSG_TYPE_NUM_ENTRIES; i++)
        {
          if (hostlist_count (output_hostrange[i]) > 0)
            {
              memset (buf, '\0', IPMIPOWER_OUTPUT_BUFLEN);
              
              hostlist_sort (output_hostrange[i]);
              
              if ((rv = hostlist_ranged_string (output_hostrange[i],
                                                IPMIPOWER_OUTPUT_BUFLEN,
                                                buf)) < 0)
                {
                  IPMIPOWER_ERROR (("hostlist_ranged_string: %s", strerror(errno)));
                  exit (1);
                }
              
              if (rv > 0)
                {
                  ipmipower_cbuf_printf (ttyout,
                                         "----------------\n");
                  ipmipower_cbuf_printf (ttyout,
                                         "%s\n",
                                         buf);
                  ipmipower_cbuf_printf (ttyout,
                                         "----------------\n");
                  ipmipower_cbuf_printf (ttyout,
                                         " %s\n",
                                         ipmipower_outputs[i]);
                  hostlist_delete (output_hostrange[i], buf);
                }
              
              assert (!hostlist_count (output_hostrange[i]));
            }
        }
    }
  
  return;
}

