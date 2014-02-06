/*****************************************************************************\
 *  $Id: pstdout.c,v 1.10 2010-02-10 01:27:44 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-227589
 *
 *  This file is part of pstdout, a library used to launch and manage
 *  the standard output of multiple threads. For details, see
 *  http://www.llnl.gov/linux/.
 *
 *  Pstdout is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Pstdout is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Pstdout.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

/* 
 * Notes:
 *
 * Needs to be compiled with -D_REENTRANT
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_PTHREAD_H
#include <pthread.h>
#endif /* HAVE_PTHREAD_H */
#include <signal.h>
#include <assert.h>
#include <errno.h>

#include "pstdout.h"
#include "cbuf.h"
#include "hostlist.h"
#include "list.h"

/* max hostrange size is typically 16 bytes
 *
 * counting the comma, a 1000 node non-expanded hostrange list could
 * be (16 + 1) * 1000 = 17000 bytes.
 *
 * So we round up from there to the nearest 16K
 */
#define PSTDOUT_BUFLEN            32768

static char * pstdout_errmsg[] =
  {
    "success",
    "library uninitialized",
    "incorrect parameters passed in",
    "out of memory",
    "unknown internal error",
    "error number out of range",
  };

static uint32_t pstdout_debug_flags = PSTDOUT_DEBUG_NONE;
static uint32_t pstdout_output_flags = PSTDOUT_OUTPUT_STDOUT_DEFAULT | PSTDOUT_OUTPUT_STDERR_DEFAULT;
static unsigned int pstdout_fanout = PSTDOUT_FANOUT_DEFAULT;

static pthread_mutex_t pstdout_threadcount_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t pstdout_threadcount_cond = PTHREAD_COND_INITIALIZER;
static int pstdout_threadcount = 0;

struct pstdout_thread_data {
  char *hostname;
  pthread_t tid;
  pthread_attr_t attr;
  int exit_code;
  Pstdout_Thread pstdout_func;
  void *arg;
};

struct pstdout_state {
  uint32_t magic;
  char *hostname; 
  cbuf_t p_stdout;
  cbuf_t p_stderr;
  char *buffer_stdout;
  char *buffer_stderr;
  unsigned int buffer_stdout_len;
  unsigned int buffer_stderr_len;
  int no_more_external_output;
  pthread_mutex_t mutex;
};

#define PSTDOUT_STATE_MAGIC    0x76309ab3
#define PSTDOUT_STATE_CBUF_MIN 32
#define PSTDOUT_STATE_CBUF_MAX 2048

int pstdout_errnum = PSTDOUT_ERR_SUCCESS;

struct pstdout_consolidated_data {
  hostlist_t h;
  char *output;
};

static List pstdout_consolidated_stdout = NULL;
static List pstdout_consolidated_stderr = NULL;

static pthread_mutex_t pstdout_consolidated_stdout_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t pstdout_consolidated_stderr_mutex = PTHREAD_MUTEX_INITIALIZER;

static int pstdout_initialized = 0;

static pthread_mutex_t pstdout_launch_mutex = PTHREAD_MUTEX_INITIALIZER;

static List pstdout_states = NULL;
static pthread_mutex_t pstdout_states_mutex = PTHREAD_MUTEX_INITIALIZER;

#ifndef HAVE_SIGHANDLER_T
typedef void (*sighandler_t)(int);
#endif /* HAVE_SIGHANDLER_T */

static struct pstdout_consolidated_data *
_pstdout_consolidated_data_create(const char *hostname, const char *output)
{
  struct pstdout_consolidated_data *cdata = NULL;

  assert(hostname);
  assert(output);

  if (!(cdata = (struct pstdout_consolidated_data *)malloc(sizeof(struct pstdout_consolidated_data))))
    {
      pstdout_errnum = PSTDOUT_ERR_OUTMEM;
      goto cleanup;
    }
  memset(cdata, '\0', sizeof(struct pstdout_consolidated_data));

  if (!(cdata->h = hostlist_create(hostname)))
    {
      pstdout_errnum = PSTDOUT_ERR_OUTMEM;
      goto cleanup;
    }

  if (!(cdata->output = strdup(output)))
    {
      pstdout_errnum = PSTDOUT_ERR_OUTMEM;
      goto cleanup;
    }

  return cdata;

 cleanup:
  if (cdata)
    {
      if (cdata->h)
        hostlist_destroy(cdata->h);
      free(cdata->output);
      free(cdata);
    }
  return NULL;
}

static void
_pstdout_consolidated_data_destroy(void *x)
{
  struct pstdout_consolidated_data *cdata;
  
  assert(x);

  cdata = (struct pstdout_consolidated_data *)x;
  if (cdata->h)
    hostlist_destroy(cdata->h);
  free(cdata->output);
  free(cdata);
}

static int
_pstdout_consolidated_data_compare(void *x, void *y)
{
  struct pstdout_consolidated_data *cdataX;
  struct pstdout_consolidated_data *cdataY;
  int h_countX, h_countY;

  assert(x);
  assert(y);

  cdataX = (struct pstdout_consolidated_data *)x;
  cdataY = (struct pstdout_consolidated_data *)x;

  assert(cdataX->h);
  assert(cdataY->h);

  h_countX = hostlist_count(cdataX->h);
  h_countY = hostlist_count(cdataY->h);

  if (h_countX < h_countY)
    return -1;
  if (h_countX > h_countY)
    return 1;
  return 0;
}

static int
_pstdout_consolidated_data_find(void *x, void *key)
{
  struct pstdout_consolidated_data *cdata;

  assert(x);
  assert(key);

  cdata = (struct pstdout_consolidated_data *)x;
  
  assert(cdata->output);
  
  if (!strcmp(cdata->output, (char *)key))
    return 1;
  return 0;
}

static int
_pstdout_consolidated_data_delete_all(void *x, void *key)
{
  return 1;
}

static int
_pstdout_states_delete_pointer(void *x, void *key)
{
  if (x == key)
    return 1;
  return 0;
}

int 
pstdout_init(void)
{
  if (!pstdout_initialized)
    {
      if (!(pstdout_consolidated_stdout = list_create((ListDelF)_pstdout_consolidated_data_destroy)))
        {
          pstdout_errnum = PSTDOUT_ERR_OUTMEM;
          goto cleanup;
        }
      if (!(pstdout_consolidated_stderr = list_create((ListDelF)_pstdout_consolidated_data_destroy)))
        {
          pstdout_errnum = PSTDOUT_ERR_OUTMEM;
          goto cleanup;
        }
      if (!(pstdout_states = list_create((ListDelF)NULL)))
	{
          pstdout_errnum = PSTDOUT_ERR_OUTMEM;
          goto cleanup;
	}
      pstdout_initialized++;
    }

  return 0;

 cleanup:
  if (pstdout_consolidated_stdout)
    list_destroy(pstdout_consolidated_stdout);
  if (pstdout_consolidated_stderr)
    list_destroy(pstdout_consolidated_stderr);
  if (pstdout_states)
    list_destroy(pstdout_states);
  return -1;
}

char *
pstdout_strerror(int errnum)
{
  if (errnum >= PSTDOUT_ERR_SUCCESS && errnum <= PSTDOUT_ERR_ERRNUMRANGE)
    return pstdout_errmsg[errnum];
  else
    return pstdout_errmsg[PSTDOUT_ERR_ERRNUMRANGE];
}

int
pstdout_set_debug_flags(unsigned int debug_flags)
{
  int rc, rv = -1;

  if (debug_flags & ~PSTDOUT_DEBUG_MASK)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }

  if (debug_flags & PSTDOUT_DEBUG_NONE
      && debug_flags && PSTDOUT_DEBUG_STANDARD)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }

  if ((rc = pthread_mutex_lock(&pstdout_launch_mutex)))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "pthread_mutex_lock: %s\n", strerror(rc));
      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
      goto cleanup;
    }

  pstdout_debug_flags = debug_flags;


  pstdout_errnum = PSTDOUT_ERR_SUCCESS;
  rv = 0;
 cleanup:
  if ((rc = pthread_mutex_unlock(&pstdout_launch_mutex)))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "pthread_mutex_unlock: %s\n", strerror(rc));
      /* Don't change error code, just move on */
    }
  return rv;
}

int
pstdout_get_debug_flags(void)
{
  pstdout_errnum = PSTDOUT_ERR_SUCCESS;
  return pstdout_debug_flags;
}

int
pstdout_set_output_flags(unsigned int output_flags)
{
  int rc, rv = -1;

  if (output_flags & ~PSTDOUT_OUTPUT_MASK)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }

  if (((output_flags & PSTDOUT_OUTPUT_STDOUT_DEFAULT)
       && (output_flags & PSTDOUT_OUTPUT_STDOUT_PREPEND_HOSTNAME))
      || ((output_flags & PSTDOUT_OUTPUT_STDERR_DEFAULT)
          && (output_flags & PSTDOUT_OUTPUT_STDERR_PREPEND_HOSTNAME))
      || (!(output_flags & PSTDOUT_OUTPUT_STDOUT_DEFAULT)
          && !(output_flags & PSTDOUT_OUTPUT_STDOUT_PREPEND_HOSTNAME))
      || (!(output_flags & PSTDOUT_OUTPUT_STDERR_DEFAULT)
          && !(output_flags & PSTDOUT_OUTPUT_STDERR_PREPEND_HOSTNAME)))
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }

  if (((output_flags & PSTDOUT_OUTPUT_STDOUT_PREPEND_HOSTNAME)
       && (output_flags & PSTDOUT_OUTPUT_STDOUT_CONSOLIDATE))
      || ((output_flags & PSTDOUT_OUTPUT_STDERR_PREPEND_HOSTNAME)
          && (output_flags & PSTDOUT_OUTPUT_STDERR_CONSOLIDATE)))
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }

  if (((output_flags & PSTDOUT_OUTPUT_BUFFER_STDOUT)
       && (output_flags & PSTDOUT_OUTPUT_STDOUT_CONSOLIDATE))
      || ((output_flags & PSTDOUT_OUTPUT_BUFFER_STDERR)
          && (output_flags & PSTDOUT_OUTPUT_STDERR_CONSOLIDATE)))
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }

  if ((rc = pthread_mutex_lock(&pstdout_launch_mutex)))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "pthread_mutex_lock: %s\n", strerror(rc));
      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
      goto cleanup;
    }

  pstdout_output_flags = output_flags;

  pstdout_errnum = PSTDOUT_ERR_SUCCESS;
  rv = 0;
 cleanup:
  if ((rc = pthread_mutex_unlock(&pstdout_launch_mutex)))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "pthread_mutex_unlock: %s\n", strerror(rc));
      /* Don't change error code, just move on */
    }
  return rv;
}

int
pstdout_get_output_flags(void)
{
  pstdout_errnum = PSTDOUT_ERR_SUCCESS;
  return pstdout_output_flags;
}

int
pstdout_set_fanout(unsigned int fanout)
{
  int rc, rv = -1;

  if (!(fanout >= PSTDOUT_FANOUT_MIN && fanout <= PSTDOUT_FANOUT_MAX))
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }

  if ((rc = pthread_mutex_lock(&pstdout_launch_mutex)))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "pthread_mutex_lock: %s\n", strerror(rc));
      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
      goto cleanup;
    }

  pstdout_fanout = fanout;

  pstdout_errnum = PSTDOUT_ERR_SUCCESS;
  rv = 0;
 cleanup:
  if ((rc = pthread_mutex_unlock(&pstdout_launch_mutex)))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "pthread_mutex_unlock: %s\n", strerror(rc));
      /* Don't change error code, just move on */
    }

  pstdout_errnum = PSTDOUT_ERR_SUCCESS;
  return rv;
}

int
pstdout_get_fanout(void)
{
  pstdout_errnum = PSTDOUT_ERR_SUCCESS;
  return pstdout_fanout;
}

int 
pstdout_hostnames_count(const char *hostnames)
{
  hostlist_t h = NULL;
  int count = 0;
  int rv = -1;

  if (!hostnames)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }

  if (!(h = hostlist_create(hostnames)))
    {
      pstdout_errnum = PSTDOUT_ERR_OUTMEM;
      goto cleanup;
    }

  if (!(count = hostlist_count(h)))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "hostnames count == 0\n");
      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
      goto cleanup;
    }

  rv = count;
 cleanup:
  if (h)
    hostlist_destroy(h);
  return rv;
}

static int
_pstdout_print(pstdout_state_t pstate, 
               int internal_to_pstdout,
               FILE *stream,
               const char *format, 
               va_list ap)
{
  char *buf = NULL;
  char *linebuf = NULL;
  size_t buflen = PSTDOUT_BUFLEN;
  size_t linebuflen = PSTDOUT_BUFLEN;
  cbuf_t whichcbuf;
  uint32_t whichdefaultmask;
  uint32_t whichprependmask;
  uint32_t whichbuffermask;
  uint32_t whichconsolidatemask;
  char **whichbuffer;
  unsigned int *whichbufferlen;
  int wlen;
  int linelen;
  int pstate_mutex_locked = 0;
  int rc, rv = -1;

  assert(pstate);
  assert(pstate->magic == PSTDOUT_STATE_MAGIC);
  assert(pstate->p_stdout);
  assert(pstate->p_stderr);
  assert(stream);
  assert(stream == stdout || stream == stderr);
  assert(format);
  assert(ap);

  if (stream == stdout)
    {
      whichcbuf = pstate->p_stdout;
      whichdefaultmask = PSTDOUT_OUTPUT_STDOUT_DEFAULT;
      whichprependmask = PSTDOUT_OUTPUT_STDOUT_PREPEND_HOSTNAME;
      whichbuffermask = PSTDOUT_OUTPUT_BUFFER_STDOUT;
      whichconsolidatemask = PSTDOUT_OUTPUT_STDOUT_CONSOLIDATE;
      whichbuffer = &(pstate->buffer_stdout);
      whichbufferlen = &(pstate->buffer_stdout_len);
    }
  else
    {
      whichcbuf = pstate->p_stderr;
      whichdefaultmask = PSTDOUT_OUTPUT_STDERR_DEFAULT;
      whichprependmask = PSTDOUT_OUTPUT_STDERR_PREPEND_HOSTNAME;
      whichbuffermask = PSTDOUT_OUTPUT_BUFFER_STDERR;
      whichconsolidatemask = PSTDOUT_OUTPUT_STDERR_CONSOLIDATE;
      whichbuffer = &(pstate->buffer_stderr);
      whichbufferlen = &(pstate->buffer_stderr_len);
    }

  while (1)
    {
      va_list vacpy;

      if (!(buf = (char *)realloc(buf, buflen)))
        {
          pstdout_errnum = PSTDOUT_ERR_OUTMEM;
          goto cleanup;
        }
      memset(buf, '\0', PSTDOUT_BUFLEN);
      va_copy(vacpy, ap);
      wlen = vsnprintf(buf, buflen, format, vacpy);
      va_end(vacpy);
      if (wlen < buflen)
        break;
      buflen += PSTDOUT_BUFLEN;
    }

  if ((rc = pthread_mutex_lock(&(pstate->mutex))))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "pthread_mutex_lock: %s\n", strerror(rc));
      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
      goto cleanup;
    }
  pstate_mutex_locked++;

  /* Protect from racing output when we are in a Ctrl+C flushing
   * buffered output situation
   */
  if (!internal_to_pstdout && pstate->no_more_external_output)
    goto cleanup;

  if (cbuf_write(whichcbuf, buf, wlen, NULL) < 0)
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "cbuf_write: %s\n", strerror(errno));
      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
      goto cleanup;
    }

  while (1)
    {
      if (!(linebuf = (char *)realloc(linebuf, linebuflen)))
        {
          pstdout_errnum = PSTDOUT_ERR_OUTMEM;
          goto cleanup;
        }
      memset(linebuf, '\0', PSTDOUT_BUFLEN);
      
      while ((linelen = cbuf_read_line (whichcbuf, linebuf, linebuflen, 1)) > 0)
        {
          if (linelen >= linebuflen)
            break;

          if (!pstate->hostname 
              || ((pstdout_output_flags & whichdefaultmask)
                  && !(pstdout_output_flags & whichbuffermask)
                  && !(pstdout_output_flags & whichconsolidatemask)))
                      
            {
              rv = fprintf(stream, "%s", linebuf);
              fflush(stream);
            }
          else if (pstdout_output_flags & whichprependmask
                   && !(pstdout_output_flags & whichbuffermask)
                   && !(pstdout_output_flags & whichconsolidatemask))
            {
              rv = fprintf(stream, "%s: %s", pstate->hostname, linebuf);
              fflush(stream);
            }
          else if (((pstdout_output_flags & whichdefaultmask)
                    && (pstdout_output_flags & whichbuffermask))
                   || (pstdout_output_flags & whichconsolidatemask))
            {
              if (!(*whichbuffer = (char *)realloc(*whichbuffer, *whichbufferlen + linelen)))
                {
                  pstdout_errnum = PSTDOUT_ERR_OUTMEM;
                  goto cleanup;
                }
                  
              /* Don't use snprintf, it will truncate b/c "snprintf and
                 vsnprintf do not write more than size bytes (including
                 the trailing '\0'). " */
              memcpy(*whichbuffer + *whichbufferlen, linebuf, linelen);
              *whichbufferlen += linelen;
              rv = linelen;
            }
          else if ((pstdout_output_flags & whichprependmask)
                   && (pstdout_output_flags & whichbuffermask))
            {
              unsigned int hostname_len;
              unsigned int extra_len;
                  
              /* + 2 is for the ": " */
              hostname_len = strlen(pstate->hostname);
              extra_len = hostname_len + 2;
              if (!(*whichbuffer = (char *)realloc(*whichbuffer, *whichbufferlen + linelen + extra_len)))
                {
                  pstdout_errnum = PSTDOUT_ERR_OUTMEM;
                  goto cleanup;
                }
              
              /* Don't use snprintf, it will truncate b/c "snprintf and
                 vsnprintf do not write more than size bytes (including
                 the trailing '\0'). " */
              memcpy(*whichbuffer + *whichbufferlen, 
                     pstate->hostname, 
                     hostname_len);
              memcpy(*whichbuffer + *whichbufferlen + hostname_len, 
                     ": ",
                     2);
              memcpy(*whichbuffer + *whichbufferlen + hostname_len + 2, 
                     linebuf, 
                     linelen);
              *whichbufferlen += linelen + extra_len;
              rv = linelen + extra_len;
            }
          else 
            {
              pstdout_errnum = PSTDOUT_ERR_INTERNAL;
              return -1;
            }
        }
      
      if (linelen < 0)
        {
          if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
            fprintf(stderr, "cbuf_read_line: %s\n", strerror(errno));
          pstdout_errnum = PSTDOUT_ERR_INTERNAL;
          goto cleanup;
        }

      if (!linelen)
        break;

      linebuflen += PSTDOUT_BUFLEN;
    }

  if (rv < 0)
    rv = 0;

  pstdout_errnum = PSTDOUT_ERR_SUCCESS;
 cleanup:
  if (pstate_mutex_locked)
    {
      if ((rc = pthread_mutex_unlock(&(pstate->mutex))))
	{
	  if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
	    fprintf(stderr, "pthread_mutex_unlock: %s\n", strerror(rc));
	  /* Don't change error code, just move on */
	}
    }
  free(buf);
  free(linebuf);
  return rv;
}

static void
_pstdout_print_wrapper(pstdout_state_t pstate, 
                       int internal_to_pstdout,
                       FILE *stream, 
                       const char *format, ...)
{
  va_list ap;
  
  assert(pstate);
  assert(pstate->magic == PSTDOUT_STATE_MAGIC);
  assert(stream);
  assert(stream == stdout || stream == stderr);
  assert(format);

  va_start(ap, format);
  _pstdout_print(pstate, internal_to_pstdout, stderr, format, ap);
  va_end(ap);
}

int
pstdout_printf(pstdout_state_t pstate, const char *format, ...)
{
  va_list ap;
  int rv;

  if (!pstdout_initialized)
    {
      pstdout_errnum = PSTDOUT_ERR_UNINITIALIZED;
      return -1;
    }

  if (!pstate || pstate->magic != PSTDOUT_STATE_MAGIC)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }

  if (!format)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }

  va_start(ap, format);
  rv = _pstdout_print(pstate, 0, stdout, format, ap);
  va_end(ap);
  return rv;
}

int
pstdout_vprintf(pstdout_state_t pstate, const char *format, va_list ap)
{
  int rv;

  if (!pstdout_initialized)
    {
      pstdout_errnum = PSTDOUT_ERR_UNINITIALIZED;
      return -1;
    }

  if (!pstate || pstate->magic != PSTDOUT_STATE_MAGIC)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }

  if (!format)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }

  /* achu: va_list is defined by C standard as an object type, not
   * necessarily pointer type.  So can't do NULL on ap type.  No known
   * portable macro to test for validity of input.
   */

  rv = _pstdout_print(pstate, 0, stdout, format, ap);
  return rv;
}

int 
pstdout_fprintf(pstdout_state_t pstate, FILE *stream, const char *format, ...)
{
  va_list ap;
  int rv;

  if (!pstdout_initialized)
    {
      pstdout_errnum = PSTDOUT_ERR_UNINITIALIZED;
      return -1;
    }

  if (!pstate || pstate->magic != PSTDOUT_STATE_MAGIC)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }

  if (!stream || !format)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }

  if (stream != stdout && stream != stderr)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }
  
  va_start(ap, format);
  rv = _pstdout_print(pstate, 0, stream, format, ap);
  va_end(ap);
  return rv;
}

int 
pstdout_vfprintf(pstdout_state_t pstate, FILE *stream, const char *format,
		 va_list ap)
{
  int rv;

  if (!pstdout_initialized)
    {
      pstdout_errnum = PSTDOUT_ERR_UNINITIALIZED;
      return -1;
    }

  if (!pstate || pstate->magic != PSTDOUT_STATE_MAGIC)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }

  if (!stream || !format)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }

  if (stream != stdout && stream != stderr)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }
  
  rv = _pstdout_print(pstate, 0, stream, format, ap);
  return rv;
}

void 
pstdout_perror(pstdout_state_t pstate, const char *s)
{
  if (!pstdout_initialized)
    {
      pstdout_errnum = PSTDOUT_ERR_UNINITIALIZED;
      return;
    }

  if (!pstate || pstate->magic != PSTDOUT_STATE_MAGIC)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return;
    }

  if (!s)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return;
    }

  _pstdout_print_wrapper(pstate, 0, stderr, "%s: %s\n", s, strerror(errno));
}

static int
_pstdout_state_init(pstdout_state_t pstate, const char *hostname)
{
  int rc;

  assert(pstate);

  memset(pstate, '\0', sizeof(struct pstdout_state));
  pstate->magic = PSTDOUT_STATE_MAGIC;
  pstate->hostname = (char *)hostname;

  if (!(pstate->p_stdout = cbuf_create(PSTDOUT_STATE_CBUF_MIN, PSTDOUT_STATE_CBUF_MAX)))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "cbuf_create: %s\n", strerror(errno));
      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
      return -1;
    }
  if (!(pstate->p_stderr = cbuf_create(PSTDOUT_STATE_CBUF_MIN, PSTDOUT_STATE_CBUF_MAX)))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "cbuf_create: %s\n", strerror(errno));
      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
      return -1;
    }
  pstate->buffer_stdout = NULL;
  pstate->buffer_stdout_len = 0;
  pstate->buffer_stderr = NULL;
  pstate->buffer_stderr_len = 0;
  pstate->no_more_external_output = 0;

  if ((rc = pthread_mutex_init(&(pstate->mutex), NULL)))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "pthread_mutex_lock: %s\n", strerror(rc));
      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
      return -1;
    }
  return 0;
}

static int
_pstdout_output_buffer_data(pstdout_state_t pstate, 
                            FILE *stream,
                            char **whichbuffer,
                            unsigned int *whichbufferlen,
                            uint32_t whichprependmask, 
                            uint32_t whichbuffermask,
                            uint32_t whichconsolidatemask,
                            List whichconsolidatedlist,
			    pthread_mutex_t *whichconsolidatedmutex)
{
  assert(pstate);
  assert(pstate->magic == PSTDOUT_STATE_MAGIC);
  assert(pstate->p_stdout);
  assert(pstate->p_stderr);
  assert(stream);
  assert(stream == stdout || stream == stderr);
  assert(whichbuffer);
  assert(whichbufferlen);
  assert(whichprependmask == PSTDOUT_OUTPUT_STDOUT_PREPEND_HOSTNAME 
         || whichprependmask == PSTDOUT_OUTPUT_STDERR_PREPEND_HOSTNAME);
  assert(whichbuffermask == PSTDOUT_OUTPUT_BUFFER_STDOUT 
         || whichbuffermask == PSTDOUT_OUTPUT_BUFFER_STDERR);
  assert(whichconsolidatemask == PSTDOUT_OUTPUT_STDOUT_CONSOLIDATE
         || whichconsolidatemask == PSTDOUT_OUTPUT_STDERR_CONSOLIDATE);
  assert(whichconsolidatedlist);
  assert(whichconsolidatedmutex);

  if ((*whichbuffer && *whichbufferlen)
      && (pstdout_output_flags & whichbuffermask
          || pstdout_output_flags & whichconsolidatemask))
    {
      /* Need to write a '\0' */
      if (!(*whichbuffer = (char *)realloc(*whichbuffer, *whichbufferlen + 1)))
        {
          pstdout_errnum = PSTDOUT_ERR_OUTMEM;
          goto cleanup;
        }

      (*whichbuffer)[*whichbufferlen] = '\0';
      *whichbufferlen += 1;

      if (pstdout_output_flags & whichbuffermask)
        {
          if (!(pstdout_output_flags & whichprependmask))
            {
              fprintf(stream, "----------------\n");
              fprintf(stream, "%s\n", pstate->hostname);
              fprintf(stream, "----------------\n");
            }
          fprintf(stream, "%s", *whichbuffer);
          fflush(stream);
        }
      else
        {
	  struct pstdout_consolidated_data *cdata;
	  int rc;

	  if ((rc = pthread_mutex_lock(whichconsolidatedmutex)))
	    {
	      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
		fprintf(stderr, "pthread_mutex_lock: %s\n", strerror(rc));
	      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
	      goto cleanup;
	    }

          if (!(cdata = list_find_first(whichconsolidatedlist, _pstdout_consolidated_data_find, *whichbuffer)))
            {
              if (!(cdata = _pstdout_consolidated_data_create(pstate->hostname, *whichbuffer)))
                goto cleanup;

              if (!list_append(whichconsolidatedlist, cdata))
                {
                  if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
                    fprintf(stderr, "list_append: %s\n", strerror(errno));
                  pstdout_errnum = PSTDOUT_ERR_INTERNAL;
                  _pstdout_consolidated_data_destroy(cdata);
                  goto cleanup;
                }
            }
          else
            {
              if (!hostlist_push(cdata->h, pstate->hostname))
                {
                  if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
                    fprintf(stderr, "hostlist_push: %s\n", strerror(errno));
                  pstdout_errnum = PSTDOUT_ERR_INTERNAL;
                  goto cleanup;
                }
            }

	  if ((rc = pthread_mutex_unlock(whichconsolidatedmutex)))
	    {
	      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
		fprintf(stderr, "pthread_mutex_unlock: %s\n", strerror(rc));
	      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
	      goto cleanup;
	    }

        }
    }

  return 0;

 cleanup:
  return -1;
}

static int
_pstdout_output_finish(pstdout_state_t pstate)
{
  int pstate_mutex_locked = 0;
  int rc, rv = -1;

  assert(pstate);
  assert(pstate->magic == PSTDOUT_STATE_MAGIC);
  assert(pstate->p_stdout);
  assert(pstate->p_stderr);

  if ((rc = pthread_mutex_lock(&(pstate->mutex))))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
	fprintf(stderr, "pthread_mutex_lock: %s\n", strerror(rc));
      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
      goto cleanup;
    }
  pstate_mutex_locked++;

  /* If there is remaining junk in the cbufs, write a "\n" to it so we
   * finish off the line and get it flushed out.
   */
  if (!cbuf_is_empty(pstate->p_stdout))
    _pstdout_print_wrapper(pstate, 1, stdout, "\n");
  
  if (!cbuf_is_empty(pstate->p_stderr))
    _pstdout_print_wrapper(pstate, 1, stderr, "\n");
  
  if (_pstdout_output_buffer_data(pstate,
                                  stdout,
                                  &(pstate->buffer_stdout),
                                  &(pstate->buffer_stdout_len),
                                  PSTDOUT_OUTPUT_STDOUT_PREPEND_HOSTNAME,
                                  PSTDOUT_OUTPUT_BUFFER_STDOUT,
                                  PSTDOUT_OUTPUT_STDOUT_CONSOLIDATE,
                                  pstdout_consolidated_stdout,
				  &pstdout_consolidated_stdout_mutex) < 0)
    goto cleanup;

  if (_pstdout_output_buffer_data(pstate,
                                  stderr,
                                  &(pstate->buffer_stderr),
                                  &(pstate->buffer_stderr_len),
                                  PSTDOUT_OUTPUT_STDERR_PREPEND_HOSTNAME,
                                  PSTDOUT_OUTPUT_BUFFER_STDERR,
                                  PSTDOUT_OUTPUT_STDERR_CONSOLIDATE,
                                  pstdout_consolidated_stderr,
				  &pstdout_consolidated_stderr_mutex) < 0)
    goto cleanup;

  /* Only output from internal to pstdout is allowed */
  pstate->no_more_external_output = 1;
  rv = 0;

 cleanup:
  if (pstate_mutex_locked)
    {
      if ((rc = pthread_mutex_unlock(&(pstate->mutex))))
	{
	  if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
	    fprintf(stderr, "pthread_mutex_unlock: %s\n", strerror(rc));
	  /* Don't change error code, just move on */
	}
    }
  return rv;
}

static void
_pstdout_state_cleanup(pstdout_state_t pstate)
{
  assert(pstate);
  assert(pstate->magic == PSTDOUT_STATE_MAGIC);

  if (pstate->p_stdout)
    cbuf_destroy(pstate->p_stdout);
  if (pstate->p_stderr)
    cbuf_destroy(pstate->p_stderr);
  free(pstate->buffer_stdout);
  free(pstate->buffer_stderr);
  memset(pstate, '\0', sizeof(struct pstdout_state));
}

static void *
_pstdout_func_entry(void *arg)
{
  struct pstdout_thread_data *tdata = NULL;
  struct pstdout_state pstate;
  int rc;

  tdata = (struct pstdout_thread_data *)arg;

  if (_pstdout_state_init(&pstate, tdata->hostname) < 0)
    goto cleanup;

  if ((rc = pthread_mutex_lock(&pstdout_states_mutex)))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "pthread_mutex_lock: %s\n", strerror(rc));
      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
      goto cleanup;
    }
  
  if (!list_append(pstdout_states, &pstate))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
	fprintf(stderr, "list_append: %s\n", strerror(errno));
      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
      pthread_mutex_unlock(&pstdout_states_mutex);
      goto cleanup;
    }

  if ((rc = pthread_mutex_unlock(&pstdout_states_mutex)))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "pthread_mutex_lock: %s\n", strerror(rc));
      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
      goto cleanup;
    }

  tdata->exit_code = (tdata->pstdout_func)(&pstate, tdata->hostname, tdata->arg);
  
  if (_pstdout_output_finish(&pstate) < 0)
    goto cleanup;

 cleanup:
  pthread_mutex_lock(&pstdout_states_mutex);
  list_delete_all(pstdout_states, _pstdout_states_delete_pointer, &pstate);
  pthread_mutex_unlock(&pstdout_states_mutex);
  _pstdout_state_cleanup(&pstate);
  pthread_mutex_lock(&pstdout_threadcount_mutex);
  pstdout_threadcount--;
  pthread_cond_signal(&pstdout_threadcount_cond);
  pthread_mutex_unlock(&pstdout_threadcount_mutex);
  return NULL;
}

static int
_pstdout_output_consolidated(FILE *stream,
                             List whichconsolidatedlist,
			     pthread_mutex_t *whichconsolidatedmutex)
{
  struct pstdout_consolidated_data *cdata;
  ListIterator itr = NULL;
  int mutex_locked = 0;
  int rc, rv = -1;

  assert(stream);
  assert(stream == stdout || stream == stderr);
  assert(whichconsolidatedlist);
  assert(whichconsolidatedmutex);

  if ((rc = pthread_mutex_lock(whichconsolidatedmutex)))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "pthread_mutex_lock: %s\n", strerror(rc));
      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
      goto cleanup;
    }
  mutex_locked++;

  list_sort(whichconsolidatedlist, _pstdout_consolidated_data_compare);
  
  if (!(itr = list_iterator_create (whichconsolidatedlist)))
    {
      pstdout_errnum = PSTDOUT_ERR_OUTMEM;
      goto cleanup;
    }

  while ((cdata = list_next(itr)))
    {
      char hbuf[PSTDOUT_BUFLEN];
      
      memset(hbuf, '\0', PSTDOUT_BUFLEN);
      hostlist_sort(cdata->h);
      if (hostlist_ranged_string(cdata->h, PSTDOUT_BUFLEN, hbuf) < 0)
        {
          if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
            fprintf(stderr, "hostlist_ranged_string: %s\n", strerror(errno));
          pstdout_errnum = PSTDOUT_ERR_INTERNAL;
          goto cleanup;
        }
      
      fprintf(stream, "----------------\n");
      fprintf(stream, "%s\n", hbuf);
      fprintf(stream, "----------------\n");
      fprintf(stream, "%s", cdata->output);
    }

  rv = 0;
 cleanup:  
  if (mutex_locked)
    {
      if ((rc = pthread_mutex_unlock(whichconsolidatedmutex)))
	{
	  if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
	    fprintf(stderr, "pthread_mutex_unlock: %s\n", strerror(rc));
	  /* Don't change error code, just move on */
	}
    }
  if (itr)
    list_iterator_destroy(itr);
  return rv;
}

static int
_pstdout_output_consolidated_finish(void)
{
  /* Output consolidated data */
  if (pstdout_output_flags & PSTDOUT_OUTPUT_STDOUT_CONSOLIDATE)
    {
      if (_pstdout_output_consolidated(stdout, 
				       pstdout_consolidated_stdout,
				       &pstdout_consolidated_stdout_mutex) < 0)
        goto cleanup;
    }

  if (pstdout_output_flags & PSTDOUT_OUTPUT_STDERR_CONSOLIDATE)
    {
      if (_pstdout_output_consolidated(stderr,
				       pstdout_consolidated_stderr,
				       &pstdout_consolidated_stderr_mutex) < 0)
        goto cleanup;
    }

  return 0;

 cleanup:
  return -1;
}

static int
_pstdout_sigint_finish_output(void *x, void *arg)
{
  struct pstdout_state *pstate;
  assert(x);

  pstate = (struct pstdout_state *)x;
       
  if (_pstdout_output_finish(pstate) < 0)
    return -1;

  /* The no_more_external_output flag set in _pstdout_output_finish()
   * protects from extraneous extra output from other threads after
   * this output.
   */
  
  if (pstate->hostname
      && (pstdout_output_flags & PSTDOUT_OUTPUT_STDOUT_PREPEND_HOSTNAME)
      && !(pstdout_output_flags & PSTDOUT_OUTPUT_BUFFER_STDOUT)
      && !(pstdout_output_flags & PSTDOUT_OUTPUT_STDOUT_CONSOLIDATE))
    {
      fprintf(stdout, "%s: exiting session\n", pstate->hostname);
      fflush(stdout);
    }

  /* Note: there isn't a race with any remaining threads and the below
   * output, b/c the _pstdout_output_finish() outputs buffered data or
   * puts the remaining data into the consolidated output list.  Any extra
   * output from the user cannot be outputted.
   */

  if (pstate->hostname
      && (pstdout_output_flags & PSTDOUT_OUTPUT_STDOUT_PREPEND_HOSTNAME)
      && (pstdout_output_flags & PSTDOUT_OUTPUT_BUFFER_STDOUT))
    {
      if ((pstate->buffer_stdout && pstate->buffer_stdout_len)
          || (pstate->buffer_stderr && pstate->buffer_stderr_len))
        fprintf(stdout, "%s: exiting session: current output flushed\n", pstate->hostname);
      else
        fprintf(stdout, "%s: exiting session\n", pstate->hostname);
      fflush(stdout);
    }

  if (pstdout_output_flags & PSTDOUT_OUTPUT_STDOUT_CONSOLIDATE)
    {
      if ((pstate->buffer_stdout && pstate->buffer_stdout_len)
          || (pstate->buffer_stderr && pstate->buffer_stderr_len))
        fprintf(stdout, "%s: exiting session: current output consolidated\n", pstate->hostname);
      else
        fprintf(stdout, "%s: exiting session\n", pstate->hostname);
    }

  return 0;
}

void  
_pstdout_sigint(int s)
{
  int rc;
  
  /* This is a last ditch effort, so no need to worry if we don't get
   * a lock or get an error or whatever.
   */
  if ((rc = pthread_mutex_lock(&pstdout_states_mutex)))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
	fprintf(stderr, "hostlist_ranged_string: %s\n", strerror(rc));
    }

  if (list_for_each(pstdout_states, _pstdout_sigint_finish_output, NULL) < 0)
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
	fprintf(stderr, "list_for_each: %s\n", strerror(errno));
    }

  _pstdout_output_consolidated_finish();
  exit (EXIT_FAILURE);
}

int
pstdout_launch(const char *hostnames, Pstdout_Thread pstdout_func, void *arg)
{
  struct pstdout_thread_data **tdata = NULL;
  struct pstdout_state pstate;
  unsigned int pstate_init = 0;
  hostlist_iterator_t hitr = NULL;
  hostlist_t h = NULL;
  int h_count = 0;
  char *host = NULL;
  int exit_code = -1;
  sighandler_t sighandler_save = NULL;
  int sighandler_set = 0;
  int rc;
  int i;

  if (!pstdout_initialized)
    {
      pstdout_errnum = PSTDOUT_ERR_UNINITIALIZED;
      return -1;
    }

  if (!pstdout_func)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }
  
  if ((rc = pthread_mutex_lock(&pstdout_launch_mutex)))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "pthread_mutex_lock: %s\n", strerror(rc));
      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
      goto cleanup;
    }

  /* Special case */
  if (!hostnames)
    {
      if (_pstdout_state_init(&pstate, NULL) < 0)
        goto cleanup;
      pstate_init++;

      exit_code = pstdout_func(&pstate, NULL, arg);
      pstdout_errnum = PSTDOUT_ERR_SUCCESS;
      goto cleanup;
    }
  
  if (!(h = hostlist_create(hostnames)))
    {
      pstdout_errnum = PSTDOUT_ERR_OUTMEM;
      goto cleanup;
    }
  h_count = hostlist_count(h);

  /* Sanity check */
  if (h_count <= 0)
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "h_count = %d\n", h_count);
      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
      goto cleanup;
    }

  /* Special case */
  if (h_count == 1)
    {
      if (_pstdout_state_init(&pstate, hostnames) < 0)
        goto cleanup;
      pstate_init++;

      exit_code = pstdout_func(&pstate, hostnames, arg);
      pstdout_errnum = PSTDOUT_ERR_SUCCESS;
      goto cleanup;
    }

  if ((sighandler_save = signal(SIGINT, _pstdout_sigint)) == SIG_ERR)
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "signal\n");
      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
      goto cleanup;
    }
  sighandler_set++;

  if (!(hitr = hostlist_iterator_create(h)))
    {
      pstdout_errnum = PSTDOUT_ERR_OUTMEM;
      goto cleanup;
    }

  if (!(tdata = (struct pstdout_thread_data **)malloc(sizeof(struct pstdout_thread_data *) * h_count)))
    {
      pstdout_errnum = PSTDOUT_ERR_OUTMEM;
      goto cleanup;
    }
  memset(tdata, '\0', sizeof(struct pstdout_thread_data *) * h_count);

  i = 0;
  while ((host = hostlist_next(hitr)))
    {
      if (!(tdata[i] = (struct pstdout_thread_data *)malloc(sizeof(struct pstdout_thread_data))))
        {
          pstdout_errnum = PSTDOUT_ERR_OUTMEM;
          goto cleanup;
        }
      memset(tdata[i], '\0', sizeof(struct pstdout_thread_data));
      
      if (!(tdata[i]->hostname = strdup(host)))
        {
          pstdout_errnum = PSTDOUT_ERR_OUTMEM;
          goto cleanup;
        }
      tdata[i]->pstdout_func = pstdout_func;
      tdata[i]->arg = arg;
      
      if ((rc = pthread_attr_init(&(tdata[i]->attr))))
        {
          if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
            fprintf(stderr, "pthread_attr_init: %s\n", strerror(rc));
          pstdout_errnum = PSTDOUT_ERR_INTERNAL;
          goto cleanup;
        }
      
      if ((rc = pthread_attr_setdetachstate(&(tdata[i]->attr), PTHREAD_CREATE_DETACHED)))
        {
          if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
            fprintf(stderr, "pthread_attr_setdetachstate: %s\n", strerror(rc));
          pstdout_errnum = PSTDOUT_ERR_INTERNAL;
          goto cleanup;
        }
      
      free(host);
      i++;
    }
  host = NULL;

  hostlist_iterator_destroy(hitr);
  hitr = NULL;

  hostlist_destroy(h);
  h = NULL;

  /* Launch threads up to fanout */
  for (i = 0; i < h_count; i++)
    {
      if ((rc = pthread_mutex_lock(&pstdout_threadcount_mutex)))
        {
          if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
            fprintf(stderr, "pthread_mutex_lock: %s\n", strerror(rc));
          pstdout_errnum = PSTDOUT_ERR_INTERNAL;
          goto cleanup;
        }

      if (pstdout_threadcount == pstdout_fanout)
        {
          if ((rc = pthread_cond_wait(&pstdout_threadcount_cond, &pstdout_threadcount_mutex)))
            {
              if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
                fprintf(stderr, "pthread_cond_wait: %s\n", strerror(rc));
              pstdout_errnum = PSTDOUT_ERR_INTERNAL;
              goto cleanup;
            }
        }

      if ((rc = pthread_create(&(tdata[i]->tid),
                               &(tdata[i]->attr),
                               _pstdout_func_entry,
                               (void *) tdata[i])))
        {
          if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
            fprintf(stderr, "pthread_create: %s\n", strerror(rc));
          pstdout_errnum = PSTDOUT_ERR_INTERNAL;
          goto cleanup;
        }

      pstdout_threadcount++;

      if ((rc = pthread_mutex_unlock(&pstdout_threadcount_mutex)))
        {
          if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
            fprintf(stderr, "pthread_mutex_unlock: %s\n", strerror(rc));
          pstdout_errnum = PSTDOUT_ERR_INTERNAL;
          goto cleanup;
        }
    }

  /* Wait for Threads to finish */

  if ((rc = pthread_mutex_lock(&pstdout_threadcount_mutex)))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "pthread_mutex_lock: %s\n", strerror(rc));
      pstdout_errnum = PSTDOUT_ERR_INTERNAL;
      goto cleanup;
    }

  while (pstdout_threadcount > 0)
    {
      if ((rc = pthread_cond_wait(&pstdout_threadcount_cond, &pstdout_threadcount_mutex)))
        {
          if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
            fprintf(stderr, "pthread_cond_wait: %s\n", strerror(rc));
          pstdout_errnum = PSTDOUT_ERR_INTERNAL;
          goto cleanup;
        }
    }

  if (_pstdout_output_consolidated_finish() < 0)
    goto cleanup;

  /* Determine exit code */
  exit_code = 0;
  for (i = 0; i < h_count; i++)
    {
      if (tdata[i]->exit_code > exit_code)
        exit_code = tdata[i]->exit_code;
    }

 cleanup:
  /* Cannot pass NULL for key, so just pass dummy key */
  list_delete_all(pstdout_consolidated_stdout, _pstdout_consolidated_data_delete_all, "");
  list_delete_all(pstdout_consolidated_stderr, _pstdout_consolidated_data_delete_all, "");
  if (pstate_init)
    _pstdout_state_cleanup(&pstate);
  if (tdata)
    {
      for (i = 0; i < h_count; i++)
        {
          if (tdata[i])
            {
	      free(tdata[i]->hostname);
              pthread_attr_destroy(&(tdata[i]->attr));
              free(tdata[i]);
            }
        }
      free(tdata);
    }
  if (hitr)
    hostlist_iterator_destroy(hitr);
  if (h)
    hostlist_destroy(h);
  free(host);
  if ((rc = pthread_mutex_unlock(&pstdout_launch_mutex)))
    {
      if (pstdout_debug_flags & PSTDOUT_DEBUG_STANDARD)
        fprintf(stderr, "pthread_mutex_unlock: %s\n", strerror(rc));
      /* Don't change error code, just move on */
    }
  if (sighandler_set)
    signal(SIGINT, sighandler_save);
  return exit_code;
}

int 
PSTDOUT_PRINTF(pstdout_state_t pstate, const char *format, ...)
{
  va_list ap;
  int rv;

  if (!format)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }

  va_start(ap, format);
  if (!pstate
      || pstate->magic != PSTDOUT_STATE_MAGIC
      || !pstdout_initialized)
    rv = vprintf(format, ap);
  else
    rv = _pstdout_print(pstate, 0, stdout, format, ap);
  va_end(ap);
  return rv;
}

int 
PSTDOUT_FPRINTF(pstdout_state_t pstate, FILE *stream, const char *format, ...)
{
  va_list ap;
  int rv;

  if (!stream || !format)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return -1;
    }

  va_start(ap, format);
  if (!pstate 
      || pstate->magic != PSTDOUT_STATE_MAGIC
      || (stream != stdout && stream != stderr)
      || !pstdout_initialized)
    rv = vfprintf(stream, format, ap);
  else
    rv = _pstdout_print(pstate, 0, stream, format, ap);
  va_end(ap);
  return rv;
}

void 
PSTDOUT_PERROR(pstdout_state_t pstate, const char *s)
{
  if (!s)
    {
      pstdout_errnum = PSTDOUT_ERR_PARAMETERS;
      return;
    }

  if (!pstate
      || pstate->magic != PSTDOUT_STATE_MAGIC
      || !pstdout_initialized)
    fprintf(stderr, "%s: %s\n", s, strerror(errno));
  else
    _pstdout_print_wrapper(pstate, 0, stderr, "%s: %s\n", s, strerror(errno));
}
