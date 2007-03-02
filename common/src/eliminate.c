/*****************************************************************************\
 *  $Id: eliminate.c,v 1.1 2007-03-02 00:56:26 chu11 Exp $
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>

#include "ipmidetect.h"
#include "hostlist.h"

#define HOSTLIST_BUFLEN 1024

int
eliminate_nodes(char **hosts)
{
  hostlist_t hl = NULL;
  hostlist_t hlnew = NULL;
  hostlist_iterator_t hitr = NULL;
  ipmidetect_t id = NULL;
  char *host = NULL;
  char hostbuf[HOSTLIST_BUFLEN];
  int rv = -1;
  
  assert(hosts);
  assert(*hosts);

  if (!(id = ipmidetect_handle_create()))
    {
      fprintf(stderr,
              "ipmidetect_handle_create\n");
      goto cleanup;
    }

  if (ipmidetect_load_data(id,
                           NULL,
                           0,
                           0) < 0)
    {
      if (ipmidetect_errnum(id) == IPMIDETECT_ERR_CONNECT
          || ipmidetect_errnum(id) == IPMIDETECT_ERR_CONNECT_TIMEOUT)
        fprintf(stderr,
                "Error connecting to ipmidetect daemon\n");
      else
        fprintf(stderr,
                "ipmidetect_load_data: %s\n", ipmidetect_errormsg(id));
      goto cleanup;
    }
                       
  if (!(hl = hostlist_create(*hosts)))
    {
      fprintf(stderr,
              "hostlist_create: %s\n", 
              strerror(errno));
      goto cleanup;
    }

  if (!(hlnew = hostlist_create(*hosts)))
    {
      fprintf(stderr,
              "hostlist_create: %s\n", 
              strerror(errno));
      goto cleanup;
    }

  if (!(hitr = hostlist_iterator_create(hl)))
    {
      fprintf(stderr,
              "hostlist_iterator_create: %s\n", 
              strerror(errno));
      goto cleanup;
    }

  while ((host = hostlist_next(hitr)))
    {
      int ret;

      if ((ret = ipmidetect_is_node_detected(id, host)) < 0)
        {
          if (ipmidetect_errnum(id) == IPMIDETECT_ERR_NOTFOUND)
            fprintf(stderr,
                    "Unrecognized node '%s' by ipmidetect\n", host);
          else
            fprintf(stderr,
                    "ipmidetect_is_node_detected: %s\n", ipmidetect_errormsg(id));
          goto cleanup;
        }

      if (!ret)
        hostlist_delete(hlnew, host);

      free(host);
    }
  host = NULL;

  if (hostlist_ranged_string(hlnew, HOSTLIST_BUFLEN, hostbuf) <= 0)
    {
      fprintf(stderr,
              "hostlist_ranged_string: truncation\n");
      goto cleanup;
    }

  free(*hosts);
  if (!(*hosts = strdup(hostbuf)))
    {
      fprintf(stderr, "strdup: %s\n", strerror(errno));
      goto cleanup;
    }
  
  rv = 0;
 cleanup:
  if (id)
    ipmidetect_handle_destroy(id);
  if (hitr)
    hostlist_iterator_destroy(hitr);
  if (hl)
    hostlist_destroy(hl);
  if (host)
    free(host);
  return rv;
}
