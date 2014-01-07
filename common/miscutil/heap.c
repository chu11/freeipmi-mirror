/*****************************************************************************
 *  $Id: list.c,v 1.2 2008-08-12 18:14:34 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2012-2014 Lawrence Livermore National Security, LLC.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  LLNL-CODE-559172
 *
 *  This file is part of Ipmiseld, an IPMI SEL syslog logging daemon.
 *  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiseld is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiseld is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiseld.  If not, see <http://www.gnu.org/licenses/>.
 *****************************************************************************
 *  Copyright (C) 2001-2002 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Chris Dunlap <cdunlap@llnl.gov>.
 *  
 *  This file is from LSD-Tools, the LLNL Software Development Toolbox.
 *
 *  LSD-Tools is free software; you can redistribute it and/or modify it under
 *  the terms of the GNU General Public License as published by the Free
 *  Software Foundation; either version 2 of the License, or (at your option)
 *  any later version.
 *
 *  LSD-Tools is distributed in the hope that it will be useful, but WITHOUT
 *  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 *  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 *  more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with LSD-Tools; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
 *****************************************************************************/


#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif /* HAVE_CONFIG_H */

#if HAVE_PTHREAD_H
#  include <pthread.h>
#endif /* HAVE_PTHREAD_H */

#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include "heap.h"

/*********************
 *  lsd_fatal_error  *
 *********************/

#ifdef WITH_LSD_FATAL_ERROR_FUNC
#  undef lsd_fatal_error
   extern void lsd_fatal_error(char *file, int line, char *mesg);
#else /* !WITH_LSD_FATAL_ERROR_FUNC */
#  ifndef lsd_fatal_error
#    include <errno.h>
#    include <stdio.h>
#    include <string.h>
#    define lsd_fatal_error(file, line, mesg)                                 \
       do {                                                                   \
           fprintf(stderr, "ERROR: [%s:%d] %s: %s\n",                         \
                   file, line, mesg, strerror(errno));                        \
       } while (0)
#  endif /* !lsd_fatal_error */
#endif /* !WITH_LSD_FATAL_ERROR_FUNC */


/*********************
 *  lsd_nomem_error  *
 *********************/

#ifdef WITH_LSD_NOMEM_ERROR_FUNC
#  undef lsd_nomem_error
   extern void * lsd_nomem_error(char *file, int line, char *mesg);
#else /* !WITH_LSD_NOMEM_ERROR_FUNC */
#  ifndef lsd_nomem_error
#    define lsd_nomem_error(file, line, mesg) (NULL)
#  endif /* !lsd_nomem_error */
#endif /* !WITH_LSD_NOMEM_ERROR_FUNC */


/***************
 *  Constants  *
 ***************/

#define HEAP_MAGIC        0x12345678
#define HEAP_SIZE_DEFAULT 64
#define HEAP_SIZE_MINIMUM 4


/****************
 *  Data Types  *
 ****************/

struct heapNode {
    void                *data;         /* node's data                       */
};

struct heap
{
  unsigned int          size;         /* size of heap elements array       */
  HeapCmpF              fCmp;         /* function to compare data          */
  HeapDelF              fDel;         /* function to delete data           */
  unsigned int          count;        /* number of items in heap           */
  unsigned int          heap_size;    /* next power of 2 to use for size   */  
  struct heapNode **    heaparray;    /* heap array to store pointers      */
#ifdef WITH_PTHREADS
  pthread_mutex_t       mutex;        /* mutex to protect access to list   */
#endif /* WITH_PTHREADS */
#ifndef NDEBUG
  unsigned int          magic;        /* sentinel for asserting validity   */
#endif /* !NDEBUG */
};

/************
 *  Macros  *
 ************/

#ifdef WITH_PTHREADS

#  define heap_mutex_init(mutex)                                              \
     do {                                                                     \
         int e = pthread_mutex_init(mutex, NULL);                             \
         if (e != 0) {                                                        \
             errno = e;                                                       \
             lsd_fatal_error(__FILE__, __LINE__, "heap mutex init");          \
             abort();                                                         \
         }                                                                    \
     } while (0)

#  define heap_mutex_lock(mutex)                                              \
     do {                                                                     \
         int e = pthread_mutex_lock(mutex);                                   \
         if (e != 0) {                                                        \
             errno = e;                                                       \
             lsd_fatal_error(__FILE__, __LINE__, "heap mutex lock");          \
             abort();                                                         \
         }                                                                    \
     } while (0)

#  define heap_mutex_unlock(mutex)                                            \
     do {                                                                     \
         int e = pthread_mutex_unlock(mutex);                                 \
         if (e != 0) {                                                        \
             errno = e;                                                       \
             lsd_fatal_error(__FILE__, __LINE__, "heap mutex unlock");        \
             abort();                                                         \
         }                                                                    \
     } while (0)

#  define heap_mutex_destroy(mutex)                                           \
     do {                                                                     \
         int e = pthread_mutex_destroy(mutex);                                \
         if (e != 0) {                                                        \
             errno = e;                                                       \
             lsd_fatal_error(__FILE__, __LINE__, "heap mutex destroy");       \
             abort();                                                         \
         }                                                                    \
     } while (0)

#  ifndef NDEBUG
     static int heap_mutex_is_locked (pthread_mutex_t *mutex);
#  endif /* !NDEBUG */

#else /* !WITH_PTHREADS */

#  define heap_mutex_init(mutex)
#  define heap_mutex_lock(mutex)
#  define heap_mutex_unlock(mutex)
#  define heap_mutex_destroy(mutex)
#  define heap_mutex_is_locked(mutex) (1)

#endif /* !WITH_PTHREADS */


/***************
 *  Functions  *
 ***************/

Heap
heap_create (int size, HeapCmpF fCmp, HeapDelF fDel)
{
  Heap h;

  if (!fCmp)
    {
      errno = EINVAL;
      return (NULL);
    }

  if (!(h = (struct heap *)malloc (sizeof (struct heap))))
    {
      errno = ENOMEM;
      return (lsd_nomem_error (__FILE__, __LINE__, "heap create"));
    }

  if (size <= 0)
    h->size = HEAP_SIZE_DEFAULT;
  else
    h->size = size;
  h->fCmp = fCmp;
  h->fDel = fDel;

  h->count = 0;

  /* classic find next power of 2 algorithm */
  h->heap_size = h->size - 1;
  h->heap_size = (h->heap_size >> 1) | h->heap_size;
  h->heap_size = (h->heap_size >> 2) | h->heap_size;
  h->heap_size = (h->heap_size >> 4) | h->heap_size;
  h->heap_size = (h->heap_size >> 8) | h->heap_size;
  h->heap_size = (h->heap_size >> 16) | h->heap_size;
  h->heap_size += 1;

  if (h->heap_size < HEAP_SIZE_MINIMUM)
    h->heap_size = HEAP_SIZE_MINIMUM;

  if (!(h->heaparray = (struct heapNode **)malloc (sizeof (struct heapNode *) * h->heap_size)))
    {
      free (h);
      errno = ENOMEM;
      return (lsd_nomem_error (__FILE__, __LINE__, "heap array create"));
    }
  memset (h->heaparray, '\0', sizeof (void *) * h->heap_size);

  heap_mutex_init (&h->mutex);
  assert(h->magic = HEAP_MAGIC);      /* set magic via assert abuse */
  return (h);
}


void
heap_destroy (Heap h)
{
  unsigned int i;
  assert (h != NULL);
  heap_mutex_lock (&h->mutex);
  assert (h->magic == HEAP_MAGIC);
  for (i = 0; i < h->count; i++)
    {
      if ((h->heaparray[i])->data && h->fDel)
	h->fDel ((h->heaparray[i])->data);
      free (h->heaparray[i]);
    }
  free (h->heaparray);
  heap_mutex_unlock (&h->mutex);
  heap_mutex_destroy (&h->mutex);
  assert (h->magic = ~HEAP_MAGIC);     /* clear magic via assert abuse */
  free (h);
  return;
}

int
heap_is_empty (Heap h)
{
  unsigned int n;

  assert (h != NULL);
  heap_mutex_lock (&h->mutex);
  assert (h->magic == HEAP_MAGIC);
  n = h->count;
  heap_mutex_unlock (&h->mutex);
  return (n == 0);
}

int
heap_is_full (Heap h)
{
  unsigned int n;

  assert (h != NULL);
  heap_mutex_lock (&h->mutex);
  assert (h->magic == HEAP_MAGIC);
  n = h->count;
  heap_mutex_unlock (&h->mutex);
  return (n == h->size);
}

static void *
heap_node_create (Heap h, struct heapNode **pp, void *x)
{
  struct heapNode *p;

  assert (h != NULL);
  assert (h->magic == HEAP_MAGIC);
  assert (heap_mutex_is_locked (&h->mutex));
  assert (pp != NULL);
  assert (x != NULL);
  if (!(p = (struct heapNode *) malloc (sizeof (struct heapNode))))
    {
      errno = ENOMEM;
      return (lsd_nomem_error (__FILE__, __LINE__, "heap node create"));
    }
  p->data = x;
  *pp = p;
  return (x);
}

static void *
heap_node_destroy (Heap h, struct heapNode *p)
{
  void *v;
  assert (h != NULL);
  assert (h->magic == HEAP_MAGIC);
  assert (heap_mutex_is_locked(&l->mutex));
  assert (p != NULL);
  v = p->data;
  free (p);
  return (v);
}

void *
heap_insert (Heap h, void *x)
{
  unsigned int index;
  struct heapNode *p;
  void *v;

  assert (h != NULL);
  assert (x != NULL);

  heap_mutex_lock (&h->mutex);
  assert (h->magic == HEAP_MAGIC);

  if (h->count == h->size)
    {
      errno = ENOSPC;
      heap_mutex_unlock (&h->mutex);
      return (NULL);
    }

  if (!(v = heap_node_create (h, &p, x)))
    {
      heap_mutex_unlock (&h->mutex);
      return (NULL);
    }

  if (!h->count)
    {
      h->heaparray[0] = p;
      h->count++;
      heap_mutex_unlock (&h->mutex);
      return (v);
    }

  index = h->count;
  while (index > 0 && h->fCmp (x, h->heaparray[(index - 1)/ 2]->data) > 0)
    {
      h->heaparray[index] = h->heaparray[(index - 1) / 2];
      index = (index - 1) / 2;
    }

  h->heaparray[index] = p;
  h->count++;
  heap_mutex_unlock (&h->mutex);
  return (v);
}

void *
heap_pop (Heap h)
{
  unsigned int index = 0;
  struct heapNode *p;
  void *v;

  assert (h != NULL);

  heap_mutex_lock (&h->mutex);
  assert (h->magic == HEAP_MAGIC);

  if (!h->count)
    {
      heap_mutex_unlock (&h->mutex);
      return (NULL);
    }

  v = heap_node_destroy (h, h->heaparray[0]);

  if (h->count == 1)
    {
      h->heaparray[0] = NULL;
      h->count--;
      heap_mutex_unlock (&h->mutex);
      return (v);
    }

  p = h->heaparray[h->count - 1];

  while ((((2 * index) + 1) < h->count
	  && h->fCmp (h->heaparray[(2 * index) + 1]->data, p->data) > 0)
	 || (((2 * index) + 2) < h->count
	     && h->fCmp (h->heaparray[(2 * index) + 2]->data, p->data) > 0))
    {
      unsigned int tmp;
      if (((2 * index) + 2) < h->count)
	{
	  if (h->fCmp (h->heaparray[(2 * index) + 1]->data, h->heaparray[(2 * index) + 2]->data) > 0)
	    tmp = (2 * index) + 1;
	  else
	    tmp = (2 * index) + 2;
	}
      else
	tmp = (2 * index) + 1;
      h->heaparray[index] = h->heaparray[tmp];
      index = tmp;
    }
  
  h->heaparray[index] = p;
  h->heaparray[h->count - 1] = NULL;
  h->count--;
  heap_mutex_unlock (&h->mutex);
  return (v);
}

void *
heap_peek (Heap h)
{
    void *v;

    assert (h != NULL);
    heap_mutex_lock (&h->mutex);
    assert (h->magic == HEAP_MAGIC);
    v = (h->count) ? h->heaparray[0]->data : NULL;
    heap_mutex_unlock (&h->mutex);
    return (v);
}
