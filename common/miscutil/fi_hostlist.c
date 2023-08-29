/*****************************************************************************\
 *  $Id: hostlist.c,v 1.3 2009-12-16 17:49:39 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2015 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2003-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see https://savannah.gnu.org/projects/freeipmi/.
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


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif  /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>
#include <errno.h>
#include <unistd.h>

#include "fi_hostlist.h"
#include "hostlist.h"
#include "network.h"

#define FI_HOSTLIST_MAGIC 57006

#define FI_HOSTLIST_SEPERATOR "\t, "

/* "impossible" legal hostnames to replace brackets with */
#define FI_LEFT_BRACKET  "@l@"
#define FI_RIGHT_BRACKET "@r@"

struct fi_hostlist {
#ifndef NDEBUG
  int magic;
#endif
  hostlist_t hl;
};

struct fi_hostlist_iterator {
#ifndef NDEBUG
  int magic;
#endif
  hostlist_iterator_t itr;
};

/* achu: _advance_past_brackets and _next_tok ripped from hostlist.c
 */

static int _advance_past_brackets (char *tok, char **str)
{
  /* if _single_ opening bracket exists b/w tok and str, push str
   * past first closing bracket to next seperator */
  if (   memchr(tok, '[', *str - tok) != NULL
         && memchr(tok, ']', *str - tok) == NULL ) {
    char *q = strchr(*str, ']');
    if (q && memchr(*str, '[', q - *str) == NULL) {
      *str = q + 1;
      return (1);
    }
  }

  return 0;
}

/*
 * Helper function for host list string parsing routines
 * Returns a pointer to the next token; additionally advance *str
 * to the next separator.
 *
 * next_tok was taken directly from pdsh courtesy of Jim Garlick.
 * (with modifications to support bracketed hostlists, i.e.:
 *  xxx[xx,xx,xx] is a single token)
 *
 */
static char * _next_tok(char *sep, char **str)
{
  char *tok;

  /* push str past any leading separators */
  while (**str != '\0' && strchr(sep, **str))
    (*str)++;

  if (**str == '\0')
    return NULL;

  /* assign token ptr */
  tok = *str;

  /*
   * Advance str past any separators, but if a separator occurs between
   *  brackets, e.g. foo[0-3,5], then advance str past closing brackets and
   *  try again.
   */
  do {
    /* push str past token and leave pointing to first separator */
    while (**str != '\0' && strchr(sep, **str) == NULL)
      (*str)++;
  } while (_advance_past_brackets (tok, str));

  /* nullify consecutive separators and push str beyond them */
  while (**str != '\0' && strchr(sep, **str) != NULL)
    *(*str)++ = '\0';

  return tok;
}

/* achu: Bulk from
 *
 * http://stackoverflow.com/questions/779875/what-is-the-function-to-replace-string-in-c
 *
 * as a starting point.  Adjusting code style and library needs.
 */
static char *
_str_replace (const char *orig, const char *replace, const char *with) {
  int len_replace;
  int len_with;
  int count = 0;
  char *new = NULL;
  char *tmp, *newp, *p;
  char *rv = NULL;

  assert (orig);
  assert (replace);
  assert (strlen (replace) > 0);
  assert (with);
  assert (strlen (with) > 0);

  len_replace = strlen (replace);
  len_with = strlen (with);

  /* count number of replacements needed */
  p = (char *)orig;
  while ((tmp = strstr (p, replace)))
    {
      p = tmp + len_replace;
      count++;
    }

  /* No substitutions special case */
  if (!count)
    {
      if (!(new = strdup (orig)))
        goto cleanup;
      return (new);
    }

  new = newp = (char *) malloc (strlen(orig) + (len_with - len_replace) * count + 1);
  if (!new)
    goto cleanup;

  /* p - points to remainder of orig */
  /* newp - walks the new string buffer */
  p = (char *)orig;
  while (count)
    {
      char *ins;
      int len_front;

      ins = strstr (p, replace);
      len_front = ins - p;
      strncpy (newp, p, len_front);
      newp += len_front;
      memcpy (newp, with, len_with);
      newp += len_with;
      p += len_front + len_replace;
      count--;
    }
  /* copy the rest over */
  strcpy (newp, p);

  rv = new;
 cleanup:
  if (!rv)
    free (new);
  return (rv);
}

static char *
_fi_unparse_string (const char *hosts)
{
  char *str1 = NULL;
  char *str2 = NULL;
  char *rv = NULL;

  if (!(str1 = _str_replace (hosts, FI_LEFT_BRACKET, "[")))
    goto cleanup;

  if (!(str2 = _str_replace (str1, FI_RIGHT_BRACKET, "]")))
    goto cleanup;

  rv = strdup (str2);
 cleanup:
  free (str1);
  free (str2);
  return (rv);
}

static char *
_fi_preparse_host (const char *host)
{
  char *new = NULL;
  char *rv = NULL;

  /* Handle special IPv6 address with port format, i.e.
   * "[Ipv6 address]:port"
   */

  if (host_is_ipv6_with_port (host, NULL, NULL))
    {
      unsigned int len = 0;
      char *pl;
      char *pr;

      /* +4 for extra 2 chars on left & right bracket, +1 for NUL char */
      if (!(new = (char *) malloc (strlen (host) + 5)))
        goto cleanup;

      pl = strchr (host, '[');
      memcpy (new, host, pl - host);
      len += (pl - host);

      memcpy (new + len, FI_LEFT_BRACKET, strlen (FI_LEFT_BRACKET));
      len += strlen (FI_LEFT_BRACKET);

      /* move past left bracket */
      pl++;

      pr = strchr (pl, ']');
      memcpy (new + len, pl, pr - pl);
      len += (pr - pl);

      memcpy (new + len, FI_RIGHT_BRACKET, strlen (FI_RIGHT_BRACKET));
      len += strlen (FI_RIGHT_BRACKET);

      pr++;
      memcpy (new + len, pr, host + strlen (host) - pr);
      len += (host + strlen (host) - pr);

      new[len] = '\0';
    }
  else
    {
      if (!(new = strdup (host)))
        goto cleanup;
    }

  rv = new;
 cleanup:
  if (!rv)
    free (new);
  return (rv);
}

typedef int (HostlistFn)(hostlist_t, const char *);

static int _fi_hosts (const char *hosts,
                      fi_hostlist_t fihl,
                      HostlistFn hfn)
{
  char *orig = NULL;
  char *copy = NULL;
  char *tok;
  int rv = -1;
  int count = 0;

  assert (hosts);
  assert (fihl);
  assert (hfn);

  if (!(orig = copy = strdup (hosts)))
    goto cleanup;

  while ((tok = _next_tok (FI_HOSTLIST_SEPERATOR, &copy)))
    {
      char *tok_parsed;
      int ret;

      tok_parsed = _fi_preparse_host (tok);

      ret = hfn (fihl->hl, tok_parsed);

      free (tok_parsed);

      if (!ret)
        goto cleanup;

      count += ret;
    }

  rv = count;
 cleanup:
  free (orig);
  return (rv);
}


static int
_fi_push_hosts (const char *hosts, fi_hostlist_t fihl)
{
  return (_fi_hosts (hosts, fihl, hostlist_push));
}

static int
_fi_delete_hosts (const char *hosts, fi_hostlist_t fihl)
{
  return (_fi_hosts (hosts, fihl, hostlist_delete));
}

static fi_hostlist_t
_fi_hostlist_new (void)
{
  fi_hostlist_t fihl;

  if (!(fihl = (fi_hostlist_t) malloc (sizeof (struct fi_hostlist))))
    return (NULL);

#ifndef NDEBUG
  fihl->magic = FI_HOSTLIST_MAGIC;
#endif
  fihl->hl = NULL;
  return (fihl);
}

fi_hostlist_t
fi_hostlist_create (const char *hostlist)
{
  fi_hostlist_t fihl = NULL;
  hostlist_t hl = NULL;
  fi_hostlist_t rv = NULL;

  if (!(hl = hostlist_create (NULL)))
    goto cleanup;

  if (!(fihl = _fi_hostlist_new ()))
    goto cleanup;
  fihl->hl = hl;

  if (hostlist)
    {
      if (_fi_push_hosts (hostlist, fihl) < 0)
	goto cleanup;
    }

  rv = fihl;
 cleanup:
  if (!rv)
    {
      if (fihl)
        fi_hostlist_destroy (fihl);
      else if (hl)
        hostlist_destroy (hl);
    }
  return (rv);
}

fi_hostlist_t
fi_hostlist_copy(const fi_hostlist_t fihl)
{
  fi_hostlist_t newfihl;

  assert (fihl);
  assert (fihl->magic == FI_HOSTLIST_MAGIC);

  if (!(newfihl = _fi_hostlist_new()))
    return (NULL);

  if (!(newfihl->hl = hostlist_copy (fihl->hl)))
    {
      fi_hostlist_destroy (newfihl);
      return (NULL);
    }

  return (newfihl);
}

void
fi_hostlist_destroy (fi_hostlist_t fihl)
{
  if (fihl)
    {
      assert (fihl->magic == FI_HOSTLIST_MAGIC);
      hostlist_destroy (fihl->hl);
      free (fihl);
    }
}

int
fi_hostlist_push (fi_hostlist_t fihl, const char *hosts)
{
  int rv;

  assert (fihl);
  assert (fihl->magic == FI_HOSTLIST_MAGIC);

  if ((rv = _fi_push_hosts (hosts, fihl)) < 0)
    return (0);
  return (rv);
}

int
fi_hostlist_push_host (fi_hostlist_t fihl, const char *host)
{
  int rv;

  assert (fihl);
  assert (fihl->magic == FI_HOSTLIST_MAGIC);

  if ((rv = _fi_push_hosts (host, fihl)) < 0)
    return (0);
  return (rv);
}

int
fi_hostlist_push_list (fi_hostlist_t fihl1, fi_hostlist_t fihl2)
{
  assert (fihl1);
  assert (fihl1->magic == FI_HOSTLIST_MAGIC);
  assert (fihl2);
  assert (fihl2->magic == FI_HOSTLIST_MAGIC);

  return hostlist_push_list (fihl1->hl, fihl2->hl);
}

int
fi_hostlist_find (fi_hostlist_t fihl, const char *hostname)
{
  char *p;
  int rv;

  assert (fihl);
  assert (fihl->magic == FI_HOSTLIST_MAGIC);

  p = _fi_preparse_host (hostname);

  rv = hostlist_find (fihl->hl, p);

  free (p);

  return (rv);
}

int
fi_hostlist_delete (fi_hostlist_t fihl, const char *hosts)
{
  int rv;

  assert (fihl);
  assert (fihl->magic == FI_HOSTLIST_MAGIC);

  if ((rv = _fi_delete_hosts (hosts, fihl)) < 0)
    return (0);
  return (rv);
}

int
fi_hostlist_delete_host (fi_hostlist_t fihl, const char *hostname)
{
  int rv;

  assert (fihl);
  assert (fihl->magic == FI_HOSTLIST_MAGIC);

  if ((rv = _fi_delete_hosts (hostname, fihl)) < 0)
    return (0);
  return (rv);
}

int
fi_hostlist_count (fi_hostlist_t fihl)
{
  assert (fihl);
  assert (fihl->magic == FI_HOSTLIST_MAGIC);

  return hostlist_count (fihl->hl);
}

void
fi_hostlist_sort (fi_hostlist_t fihl)
{
  assert (fihl);
  assert (fihl->magic == FI_HOSTLIST_MAGIC);

  return hostlist_sort (fihl->hl);
}

void
fi_hostlist_uniq (fi_hostlist_t fihl)
{
  assert (fihl);
  assert (fihl->magic == FI_HOSTLIST_MAGIC);

  return hostlist_uniq (fihl->hl);
}

typedef ssize_t (HostlistStrFn)(hostlist_t, size_t, char *);

static int _fi_hostlist_string (fi_hostlist_t fihl,
                                size_t n,
                                char *buf,
                                HostlistStrFn hfn)
{
  char *tmpbuf = NULL;
  char *str = NULL;
  ssize_t rv = -1;

  assert (fihl);
  assert (fihl->magic == FI_HOSTLIST_MAGIC);
  assert (hfn);

  /* +1 to guarantee NUL byte */
  if (!(tmpbuf = (char *) malloc (n + 1)))
    return -1;
  memset (tmpbuf, '\0', n + 1);

  if (hfn (fihl->hl, n, tmpbuf) < 0)
    goto cleanup;

  if (!(str = _fi_unparse_string (tmpbuf)))
    goto cleanup;

  if (strlen (str) > n)
    goto cleanup;

  strncpy (buf, str, n);
  rv = strlen (str);
 cleanup:
  free (tmpbuf);
  free (str);
  return (rv);
}

ssize_t
fi_hostlist_ranged_string (fi_hostlist_t fihl, size_t n, char *buf)
{
  return (_fi_hostlist_string (fihl, n, buf, hostlist_ranged_string));
}

ssize_t
fi_hostlist_deranged_string (fi_hostlist_t fihl, size_t n, char *buf)
{
  return (_fi_hostlist_string (fihl, n, buf, hostlist_deranged_string));
}

static fi_hostlist_iterator_t
_fi_hostlist_iterator_new (void)
{
  fi_hostlist_iterator_t fiitr;

  if (!(fiitr = (fi_hostlist_iterator_t) malloc (sizeof (struct fi_hostlist_iterator))))
    return (NULL);

#ifndef NDEBUG
  fiitr->magic = FI_HOSTLIST_MAGIC;
#endif
  fiitr->itr = NULL;
  return (fiitr);
}

fi_hostlist_iterator_t
fi_hostlist_iterator_create (fi_hostlist_t fihl)
{
  hostlist_iterator_t itr;
  fi_hostlist_iterator_t fiitr;

  assert (fihl);
  assert (fihl->magic == FI_HOSTLIST_MAGIC);

  if (!(itr = hostlist_iterator_create (fihl->hl)))
    return (NULL);

  if (!(fiitr = _fi_hostlist_iterator_new ()))
    {
      hostlist_iterator_destroy (itr);
      return (NULL);
    }

  fiitr->itr = itr;
  return (fiitr);
}

void
fi_hostlist_iterator_destroy (fi_hostlist_iterator_t fiitr)
{
  if (fiitr)
    {
      assert (fiitr->magic == FI_HOSTLIST_MAGIC);
      hostlist_iterator_destroy (fiitr->itr);
      free (fiitr);
    }
}

void
fi_hostlist_iterator_reset (fi_hostlist_iterator_t fiitr)
{
  assert (fiitr);
  assert (fiitr->magic == FI_HOSTLIST_MAGIC);

  return hostlist_iterator_reset (fiitr->itr);
}

char *
fi_hostlist_next (fi_hostlist_iterator_t fiitr)
{
  char *str = NULL;
  char *rv = NULL;

  assert (fiitr);
  assert (fiitr->magic == FI_HOSTLIST_MAGIC);

  if ((str = hostlist_next (fiitr->itr)))
    rv = _fi_unparse_string (str);

  free (str);
  return (rv);
}

int
fi_hostlist_remove (fi_hostlist_iterator_t fiitr)
{
  assert (fiitr);
  assert (fiitr->magic == FI_HOSTLIST_MAGIC);

  return hostlist_remove (fiitr->itr);
}
