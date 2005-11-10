/*****************************************************************************\
 *  $Id: ipmipower_output.c,v 1.7 2005-11-10 22:17:03 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2003 The Regents of the University of California.
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
 *  with Ipmipower; if not, write to the Free Software Foundation, Inc.,
 *  59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif
#include <assert.h>

#include "ipmipower.h"
#include "ipmipower_output.h"
#include "ipmipower_wrappers.h"

extern cbuf_t ttyout;
extern struct ipmipower_config *conf;
extern hostlist_t output_hostrange[MSG_TYPE_NUM];

static char *ipmipower_outputs[] = 
  {
    "success",
    "on",
    "off",
    "ok",
    "permission denied",
    "invalid username",
    "invalid password",
    "cannot obtain proper privilege level",
    "invalid operation",
    "invalid authtype",
    "usable authtype/privilege combination not found",
    "cannot authenticate at given privilege",
    "timed out",
    "not discovered",
    "bad connection",
    "unknown node name",
    "out of resources",
    "bmc busy",
    "bmc error"
  };

void
ipmipower_output(msg_type_t num, char *hostname) 
{
  assert(MSG_TYPE_VALID(num));
  assert(hostname != NULL);

  if (conf->outputtype == OUTPUT_TYPE_NEWLINE)
    cbuf_printf(ttyout, "%s: %s\n", hostname, ipmipower_outputs[num]);
  else if (conf->outputtype == OUTPUT_TYPE_HOSTRANGE) 
    {
      if (hostlist_push_host(output_hostrange[num], hostname) == 0)
        err_exit("hostlist_push_host() error\n");
    }

  return;
}

void
ipmipower_output_finish(void)
{
  if (conf->outputtype == OUTPUT_TYPE_HOSTRANGE) 
    {
      int i, rv;
      char buffer[IPMIPOWER_HOSTLIST_BUFLEN];
      
      for (i = 0; i < MSG_TYPE_NUM; i++) 
        {
          if (hostlist_count(output_hostrange[i]) > 0) {
            memset(buffer, '\0', IPMIPOWER_HOSTLIST_BUFLEN); 

            hostlist_sort(output_hostrange[i]);
                
            rv = hostlist_ranged_string(output_hostrange[i], 
                                        IPMIPOWER_HOSTLIST_BUFLEN, 
                                        buffer);
            if (rv < 0) 
              {
                cbuf_printf(ttyout, "OVERFLOWED BUFFER: %s\n", 
                            ipmipower_outputs[i]);
                
                while (hostlist_count(output_hostrange[i]) > 0)
                  hostlist_delete_nth(output_hostrange[i], 0);
              }
                
            if (rv > 0) 
              {
                cbuf_printf(ttyout, "%s: %s\n",
                            buffer, ipmipower_outputs[i]);
                hostlist_delete(output_hostrange[i], buffer);
              }
            
            assert(hostlist_count(output_hostrange[i]) == 0);
          }
        } 
    }
  
  return;
}

output_type_t 
ipmipower_output_index(char *str) 
{
  assert(str != NULL);
    
  if (!strcasecmp(str, "none"))
    return OUTPUT_TYPE_NONE;
  else if (!strcasecmp(str, "newline"))
    return OUTPUT_TYPE_NEWLINE;
  else if (!strcasecmp(str, "hostrange"))
    return OUTPUT_TYPE_HOSTRANGE;
  else 
    return OUTPUT_TYPE_INVALID;
}

char *
ipmipower_output_string(output_type_t ot) 
{
  assert(OUTPUT_TYPE_VALID(ot));

  switch(ot) 
    {
    case OUTPUT_TYPE_NONE:
      return "none";
      break;
    case OUTPUT_TYPE_NEWLINE:
      return "newline";
      break;
    case OUTPUT_TYPE_HOSTRANGE:
      return "hostrange";
      break;
    default:
      err_exit("ipmipower_output_string: Invalid Output Type: %d\n", ot);
    }
    
  return NULL;                  /* NOT_REACHED */
}

char *
ipmipower_output_list(void) 
{
  return "none, newline, hostrange";
}
