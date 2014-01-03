/*****************************************************************************\
 *  $Id: ipmiseld.h,v 1.11 2010-02-08 22:02:30 chu11 Exp $
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
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <pthread.h>
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "ipmiseld.h"
#include "ipmiseld-debug.h"
#include "ipmiseld-threadpool.h"

#include "freeipmi-portability.h"
#include "error.h"
#include "list.h"

struct ipmiseld_threadpool_data
{
  pthread_t tid;
  int threadpool_num;
  IpmiSeldThreadPoolCallback callback;
  IpmiSeldThreadPoolPostProcess postprocess;
  int exit_flag;
};

static struct ipmiseld_threadpool_data *threadpool_data_array = NULL;
static unsigned int threadpool_data_array_len = 0; 

static List threadpool_queue = NULL;
static pthread_mutex_t threadpool_queue_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t threadpool_queue_cond = PTHREAD_COND_INITIALIZER;

static unsigned int threadpool_count = 0;
static pthread_mutex_t threadpool_count_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t threadpool_count_cond = PTHREAD_COND_INITIALIZER;

static void *
_threadpool_func (void *arg)
{
  struct ipmiseld_threadpool_data *threadpool_data;

  assert (arg);

  threadpool_data = (struct ipmiseld_threadpool_data *)arg;

  while (!threadpool_data->exit_flag)
    {
      void *queue_arg;

      pthread_mutex_lock (&threadpool_queue_lock);

      while (!list_count (threadpool_queue)
	     && !threadpool_data->exit_flag)
	pthread_cond_wait (&threadpool_queue_cond, &threadpool_queue_lock);
      
      if (threadpool_data->exit_flag)
	{
	  pthread_mutex_unlock (&threadpool_queue_lock);
	  break;
	}

      if (!(queue_arg = list_dequeue (threadpool_queue)))
	err_output ("list_dequeue: %s", strerror (errno));
      
      pthread_mutex_unlock (&threadpool_queue_lock);
      
      if (queue_arg)
	{
	  threadpool_data->callback (queue_arg);
	  
	  if (threadpool_data->postprocess)
	    threadpool_data->postprocess (queue_arg);
	}
    }

  pthread_mutex_lock (&threadpool_count_lock);
  threadpool_count--;
  pthread_cond_signal (&threadpool_count_cond);
  pthread_mutex_unlock (&threadpool_count_lock);

  return (NULL);
}

int
ipmiseld_threadpool_init (struct ipmiseld_prog_data *prog_data,
			  IpmiSeldThreadPoolCallback callback,
			  IpmiSeldThreadPoolPostProcess postprocess)
{
  int i;
  int ret;
  int rv = -1;

  assert (prog_data);
  assert (prog_data->args->threadpool_count);
  assert (callback);
  /* postprocess can be NULL */
  assert (!threadpool_data_array);

  if (!(threadpool_data_array = (struct ipmiseld_threadpool_data *)malloc (sizeof (struct ipmiseld_threadpool_data) * prog_data->args->threadpool_count)))
    {
      err_output ("malloc: %s", strerror (errno));
      goto cleanup;
    }
  memset (threadpool_data_array, '\0', sizeof (struct ipmiseld_threadpool_data) * prog_data->args->threadpool_count);

  if (!(threadpool_queue = list_create (NULL)))
    {
      err_output ("list_create: %s", strerror (errno));
      goto cleanup;
    }

  if ((ret = pthread_mutex_init (&threadpool_queue_lock, NULL)))
    {
      err_output ("pthread_mutex_init: %s", strerror (ret));
      goto cleanup;
    }

  for (i = 0; i < prog_data->args->threadpool_count; i++)
    {
      threadpool_data_array[i].threadpool_num = i;
      threadpool_data_array[i].callback = callback;
      threadpool_data_array[i].postprocess = postprocess;
      threadpool_data_array[i].exit_flag = 0;

      if ((ret = pthread_create (&threadpool_data_array[i].tid,
				 NULL,
				 _threadpool_func,
				 &threadpool_data_array[i])))
	{
	  err_output ("pthread_create: %s", strerror (ret));
	  goto cleanup;
	}
      
      pthread_mutex_lock (&threadpool_count_lock);
      threadpool_count++;
      pthread_mutex_unlock (&threadpool_count_lock);
    }

  threadpool_data_array_len = prog_data->args->threadpool_count;

  rv = 0;
 cleanup:
  return (rv);
}

void
ipmiseld_threadpool_destroy (void)
{
  int i;
  int ret;
  
  /* achu: We want any current SEL poll to complete, so we won't
   * pthread_cancel() here (and likewise won't use
   * pthread_cleanup_push/pthread_cleanup_pop).
   *
   * Instead we set this flag and wait for the threads to finish up.
   */
  pthread_mutex_lock (&threadpool_count_lock);

  for (i = 0; i < threadpool_data_array_len; i++)
    threadpool_data_array[i].exit_flag = 1;

  for (i = 0; i < threadpool_data_array_len; i++)
    {
      if ((ret = pthread_cond_signal (&threadpool_queue_cond)))
	err_output ("pthread_cond_signal: %s", strerror (ret)); 
    }

  while (threadpool_count > 0)
    pthread_cond_wait (&threadpool_count_cond, &threadpool_count_lock);

  pthread_mutex_unlock (&threadpool_count_lock);

  free (threadpool_data_array);

  if (threadpool_queue)
    list_destroy (threadpool_queue);
}  

int
ipmiseld_threadpool_queue (void *arg)
{
  assert (arg);

  pthread_mutex_lock (&threadpool_queue_lock);

  if (!list_enqueue (threadpool_queue, arg))
    {
      pthread_mutex_unlock (&threadpool_queue_lock);
      err_output ("list_enqueue: %s", strerror (errno));
      return (-1);
    }
  
  pthread_cond_signal (&threadpool_queue_cond);

  pthread_mutex_unlock (&threadpool_queue_lock);

  return (0);
}
