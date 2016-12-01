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


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif  /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include <unistd.h>

#include "fi_hostlist.h"
#include "hostlist.h"

#define FI_HOSTLIST_MAGIC 57006

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
  hostlist_t hl;
  fi_hostlist_t fihl;

  if (!(hl = hostlist_create (hostlist)))
    return (NULL);

  if (!(fihl = _fi_hostlist_new ()))
    {
      hostlist_destroy (hl);
      return (NULL);
    }

  fihl->hl = hl;
  return (fihl);
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
  assert (fihl);
  assert (fihl->magic == FI_HOSTLIST_MAGIC);

  return hostlist_push (fihl->hl, hosts);
}

int
fi_hostlist_push_host (fi_hostlist_t fihl, const char *host)
{
  assert (fihl);
  assert (fihl->magic == FI_HOSTLIST_MAGIC);

  return hostlist_push_host (fihl->hl, host);
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
  assert (fihl);
  assert (fihl->magic == FI_HOSTLIST_MAGIC);

  return hostlist_find (fihl->hl, hostname);
}

int
fi_hostlist_delete (fi_hostlist_t fihl, const char *hosts)
{
  assert (fihl);
  assert (fihl->magic == FI_HOSTLIST_MAGIC);

  return hostlist_delete (fihl->hl, hosts);
}

int
fi_hostlist_delete_host (fi_hostlist_t fihl, const char *hostname)
{
  assert (fihl);
  assert (fihl->magic == FI_HOSTLIST_MAGIC);

  return hostlist_delete_host (fihl->hl, hostname);
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

ssize_t
fi_hostlist_ranged_string (fi_hostlist_t fihl, size_t n, char *buf)
{
  assert (fihl);
  assert (fihl->magic == FI_HOSTLIST_MAGIC);

  return hostlist_ranged_string (fihl->hl, n, buf);
}

ssize_t
fi_hostlist_deranged_string (fi_hostlist_t fihl, size_t n, char *buf)
{
  assert (fihl);
  assert (fihl->magic == FI_HOSTLIST_MAGIC);

  return hostlist_deranged_string (fihl->hl, n, buf);
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
  assert (fiitr);
  assert (fiitr->magic == FI_HOSTLIST_MAGIC);

  return hostlist_next (fiitr->itr);
}

int
fi_hostlist_remove (fi_hostlist_iterator_t fiitr)
{
  assert (fiitr);
  assert (fiitr->magic == FI_HOSTLIST_MAGIC);

  return hostlist_remove (fiitr->itr);
}

