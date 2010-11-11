/*****************************************************************************
 *  $Id: scbuf.c,v 1.5 2009-03-03 23:56:52 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2002-2005 The Regents of the University of California.
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
 *****************************************************************************
 *  Refer to "scbuf.h" for documentation on public functions.
 *****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif /* HAVE_PTHREAD_H */

#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include "scbuf.h"

#include "secure.h"

/*****************************************************************************
 *  lsd_fatal_error
 *****************************************************************************/

#ifdef WITH_LSD_FATAL_ERROR_FUNC
#  undef lsd_fatal_error
   extern void lsd_fatal_error (char *file, int line, char *mesg);
#else /* !WITH_LSD_FATAL_ERROR_FUNC */
#  ifndef lsd_fatal_error
#    include <errno.h>
#    include <stdio.h>
#    include <string.h>
#    define lsd_fatal_error(file, line, mesg)                                 \
       do {                                                                   \
           fprintf (stderr, "ERROR: [%s:%d] %s: %s\n",                        \
                   file, line, mesg, strerror (errno));                       \
       } while (0)
#  endif /* !lsd_fatal_error */
#endif /* !WITH_LSD_FATAL_ERROR_FUNC */


/*****************************************************************************
 *  lsd_nomem_error
 *****************************************************************************/

#ifdef WITH_LSD_NOMEM_ERROR_FUNC
#  undef lsd_nomem_error
   extern void * lsd_nomem_error (char *file, int line, char *mesg);
#else /* !WITH_LSD_NOMEM_ERROR_FUNC */
#  ifndef lsd_nomem_error
#    define lsd_nomem_error(file, line, mesg) (NULL)
#  endif /* !lsd_nomem_error */
#endif /* !WITH_LSD_NOMEM_ERROR_FUNC */


/*****************************************************************************
 *  Constants
 *****************************************************************************/

#define SCBUF_CHUNK      1000
#define SCBUF_MAGIC      0xDEADBEEF
#define SCBUF_MAGIC_LEN  (sizeof (unsigned long))


/*****************************************************************************
 *  Data Types
 *****************************************************************************/

struct scbuf {

#ifndef NDEBUG
    unsigned long       magic;          /* cookie for asserting validity     */
#endif /* !NDEBUG */

#ifdef WITH_PTHREADS
    pthread_mutex_t     mutex;          /* mutex to protect access to scbuf   */
#endif /* WITH_PTHREADS */

    int                 alloc;          /* num bytes malloc'd/realloc'd      */
    int                 minsize;        /* min bytes of data to allocate     */
    int                 maxsize;        /* max bytes of data to allocate     */
    int                 size;           /* num bytes of data allocated       */
    int                 used;           /* num bytes of unread data          */
    scbuf_overwrite_t    overwrite;      /* overwrite option behavior         */
    int                 got_wrap;       /* true if data has wrapped          */
    int                 i_in;           /* index to where data is written in */
    int                 i_out;          /* index to where data is read out   */
    int                 i_rep;          /* index to where data is replayable */
    unsigned char      *data;           /* ptr to circular buffer of data    */
};

typedef int (*scbuf_iof) (void *scbuf_data, void *arg, int len);


/*****************************************************************************
 *  Prototypes
 *****************************************************************************/

static int scbuf_find_replay_line (scbuf_t cb, int chars, int *nlines, int *nl);
static int scbuf_find_unread_line (scbuf_t cb, int chars, int *nlines);

static int scbuf_get_fd (void *dstbuf, int *psrcfd, int len);
static int scbuf_get_mem (void *dstbuf, unsigned char **psrscbuf, int len);
static int scbuf_put_fd (void *srscbuf, int *pdstfd, int len);
static int scbuf_put_mem (void *srscbuf, unsigned char **pdstbuf, int len);

static int scbuf_copier (scbuf_t src, scbuf_t dst, int len, int *ndropped, int secure_malloc_flag);
static int scbuf_dropper (scbuf_t cb, int len);
static int scbuf_reader (scbuf_t src, int len, scbuf_iof putf, void *dst);
static int scbuf_replayer (scbuf_t src, int len, scbuf_iof putf, void *dst);
static int scbuf_writer (scbuf_t dst, int len, scbuf_iof getf, void *src,
       int *ndropped, int secure_malloc_flag);

static int scbuf_grow (scbuf_t cb, int n, int secure_malloc_flag);
static int scbuf_shrink (scbuf_t cb);

#ifndef NDEBUG
static int scbuf_is_valid (scbuf_t cb);
#endif /* !NDEBUG */


/*****************************************************************************
 *  Macros
 *****************************************************************************/

#ifndef MAX
#  define MAX(x,y) (((x) >= (y)) ? (x) : (y))
#endif /* !MAX */

#ifndef MIN
#  define MIN(x,y) (((x) <= (y)) ? (x) : (y))
#endif /* !MIN */

#ifdef WITH_PTHREADS

#  define scbuf_mutex_init(cb)                                                 \
     do {                                                                     \
         int e = pthread_mutex_init (&cb->mutex, NULL);                       \
         if (e) {                                                             \
             errno = e;                                                       \
             lsd_fatal_error (__FILE__, __LINE__, "scbuf mutex init");         \
             abort ();                                                        \
         }                                                                    \
     } while (0)

#  define scbuf_mutex_lock(cb)                                                 \
     do {                                                                     \
         int e = pthread_mutex_lock (&cb->mutex);                             \
         if (e) {                                                             \
             errno = e;                                                       \
             lsd_fatal_error (__FILE__, __LINE__, "scbuf mutex lock");         \
             abort ();                                                        \
         }                                                                    \
     } while (0)

#  define scbuf_mutex_unlock(cb)                                               \
     do {                                                                     \
         int e = pthread_mutex_unlock (&cb->mutex);                           \
         if (e) {                                                             \
             errno = e;                                                       \
             lsd_fatal_error (__FILE__, __LINE__, "scbuf mutex unlock");       \
             abort ();                                                        \
         }                                                                    \
     } while (0)

#  define scbuf_mutex_destroy(cb)                                              \
     do {                                                                     \
         int e = pthread_mutex_destroy (&cb->mutex);                          \
         if (e) {                                                             \
             errno = e;                                                       \
             lsd_fatal_error (__FILE__, __LINE__, "scbuf mutex destroy");      \
             abort ();                                                        \
         }                                                                    \
     } while (0)

#  ifndef NDEBUG
     static int scbuf_mutex_is_locked (scbuf_t cb);
#  endif /* !NDEBUG */

#else /* !WITH_PTHREADS */

#  define scbuf_mutex_init(cb)
#  define scbuf_mutex_lock(cb)
#  define scbuf_mutex_unlock(cb)
#  define scbuf_mutex_destroy(cb)
#  define scbuf_mutex_is_locked(cb) (1)

#endif /* !WITH_PTHREADS */


/*****************************************************************************
 *  Functions
 *****************************************************************************/

scbuf_t
scbuf_create (int minsize, int maxsize, int secure_malloc_flag)
{
    scbuf_t cb;

    if (minsize <= 0) {
        errno = EINVAL;
        return (NULL);
    }
    if (secure_malloc_flag) {
        if (!(cb = secure_malloc (sizeof (struct scbuf)))) {
            errno = ENOMEM;
            return (lsd_nomem_error (__FILE__, __LINE__, "scbuf struct"));
        }
    }
    else {
        if (!(cb = malloc (sizeof (struct scbuf)))) {
            errno = ENOMEM;
            return (lsd_nomem_error (__FILE__, __LINE__, "scbuf struct"));
        }
    }
    /*  Circular buffer is empty when (i_in == i_out),
     *    so reserve 1 byte for this sentinel.
     */
    cb->alloc = minsize + 1;
#ifndef NDEBUG
    /*  Reserve space for the magic cookies used to protect the
     *    scbuf data[] array from underflow and overflow.
     */
    cb->alloc += 2 * SCBUF_MAGIC_LEN;
#endif /* !NDEBUG */

    if (secure_malloc_flag) {
        if (!(cb->data = secure_malloc (cb->alloc))) {
            secure_free (cb, sizeof (struct scbuf));
            errno = ENOMEM;
            return (lsd_nomem_error (__FILE__, __LINE__, "scbuf data"));
        }
    }
    else {
        if (!(cb->data = malloc (cb->alloc))) {
            free (cb);
            errno = ENOMEM;
            return (lsd_nomem_error (__FILE__, __LINE__, "scbuf data"));
        }
    }
    scbuf_mutex_init (cb);
    cb->minsize = minsize;
    cb->maxsize = (maxsize > minsize) ? maxsize : minsize;
    cb->size = minsize;
    cb->used = 0;
    cb->overwrite = SCBUF_WRAP_MANY;
    cb->got_wrap = 0;
    cb->i_in = cb->i_out = cb->i_rep = 0;

#ifndef NDEBUG
    /*  C is for cookie, that's good enough for me, yeah!
     *  The magic cookies are only defined during DEBUG code.
     *  The first "magic" cookie is at the top of the structure.
     *  Magic cookies are also placed at the top & bottom of the
     *  scbuf data[] array to catch buffer underflow & overflow errors.
     */
    cb->data += SCBUF_MAGIC_LEN;         /* jump forward past underflow magic */
    cb->magic = SCBUF_MAGIC;
    /*
     *  Must use memcpy since overflow cookie may not be word-aligned.
     */
    memcpy (cb->data - SCBUF_MAGIC_LEN, (void *) &cb->magic, SCBUF_MAGIC_LEN);
    memcpy (cb->data + cb->size + 1, (void *) &cb->magic, SCBUF_MAGIC_LEN);

    scbuf_mutex_lock (cb);
    assert (scbuf_is_valid (cb));
    scbuf_mutex_unlock (cb);
#endif /* !NDEBUG */

    return (cb);
}


void
scbuf_destroy (scbuf_t cb, int secure_malloc_flag)
{
    assert (cb != NULL);
    scbuf_mutex_lock (cb);
    assert (scbuf_is_valid (cb));

#ifndef NDEBUG
    /*  The moon sometimes looks like a C, but you can't eat that.
     *  Munch the magic cookies before freeing memory.
     */
    cb->magic = ~SCBUF_MAGIC;            /* the anti-cookie! */
    memcpy (cb->data - SCBUF_MAGIC_LEN, (void *) &cb->magic, SCBUF_MAGIC_LEN);
    memcpy (cb->data + cb->size + 1, (void *) &cb->magic, SCBUF_MAGIC_LEN);
    cb->data -= SCBUF_MAGIC_LEN;         /* jump back to what malloc returned */
#endif /* !NDEBUG */

    if (secure_malloc_flag)
        secure_free (cb->data, cb->alloc);
    else
        free (cb->data);
    scbuf_mutex_unlock (cb);
    scbuf_mutex_destroy (cb);
    if (secure_malloc_flag)
        secure_free (cb, sizeof (struct scbuf));
    else
        free (cb);
    return;
}


void
scbuf_flush (scbuf_t cb)
{
    assert (cb != NULL);
    scbuf_mutex_lock (cb);
    assert (scbuf_is_valid (cb));
    /*
     *  FIXME: Shrink buffer back to minimum size.
     */
    cb->used = 0;
    cb->got_wrap = 0;
    cb->i_in = cb->i_out = cb->i_rep = 0;
    assert (scbuf_is_valid (cb));
    scbuf_mutex_unlock (cb);
    return;
}


int
scbuf_size (scbuf_t cb)
{
    int size;

    assert (cb != NULL);
    scbuf_mutex_lock (cb);
    assert (scbuf_is_valid (cb));
    size = cb->maxsize;
    scbuf_mutex_unlock (cb);
    return (size);
}


int
scbuf_free (scbuf_t cb)
{
    int nfree;

    assert (cb != NULL);
    scbuf_mutex_lock (cb);
    assert (scbuf_is_valid (cb));
    nfree = cb->maxsize - cb->used;
    scbuf_mutex_unlock (cb);
    return (nfree);
}


int
scbuf_used (scbuf_t cb)
{
    int used;

    assert (cb != NULL);
    scbuf_mutex_lock (cb);
    assert (scbuf_is_valid (cb));
    used = cb->used;
    scbuf_mutex_unlock (cb);
    return (used);
}


int
scbuf_lines_used (scbuf_t cb)
{
    int lines = -1;

    assert (cb != NULL);
    scbuf_mutex_lock (cb);
    assert (scbuf_is_valid (cb));
    scbuf_find_unread_line (cb, cb->size, &lines);
    scbuf_mutex_unlock (cb);
    return (lines);
}


int
scbuf_reused (scbuf_t cb)
{
/*  If (O > R)
 *    n = O - R
 *  else
 *    n = (O - 0) + ((S+1) - R).
 *  (S+1) is used since data[] contains 'size' bytes + a 1-byte sentinel.
 */
    int reused;

    assert (cb != NULL);
    scbuf_mutex_lock (cb);
    assert (scbuf_is_valid (cb));
    reused = (cb->i_out - cb->i_rep + (cb->size + 1)) % (cb->size + 1);
    scbuf_mutex_unlock (cb);
    return (reused);
}


int
scbuf_lines_reused (scbuf_t cb)
{
    int lines = -1;

    assert (cb != NULL);
    scbuf_mutex_lock (cb);
    assert (scbuf_is_valid (cb));
    scbuf_find_replay_line (cb, cb->size, &lines, NULL);
    scbuf_mutex_unlock (cb);
    return (lines);
}


int
scbuf_is_empty (scbuf_t cb)
{
    int used;

    assert (cb != NULL);
    scbuf_mutex_lock (cb);
    assert (scbuf_is_valid (cb));
    used = cb->used;
    scbuf_mutex_unlock (cb);
    return (used == 0);
}


int
scbuf_opt_get (scbuf_t cb, scbuf_opt_t name, int *value)
{
    int rc = 0;

    assert (cb != NULL);

    if (value == NULL) {
        errno = EINVAL;
        return (-1);
    }
    scbuf_mutex_lock (cb);
    assert (scbuf_is_valid (cb));
    if (name == SCBUF_OPT_OVERWRITE) {
        *value = cb->overwrite;
    }
    else {
        errno = EINVAL;
        rc = -1;
    }
    scbuf_mutex_unlock (cb);
    return (rc);
}


int
scbuf_opt_set (scbuf_t cb, scbuf_opt_t name, int value)
{
    int rc = 0;

    assert (cb != NULL);

    scbuf_mutex_lock (cb);
    assert (scbuf_is_valid (cb));
    if (name == SCBUF_OPT_OVERWRITE) {
        if  (  (value == SCBUF_NO_DROP)
            || (value == SCBUF_WRAP_ONCE)
            || (value == SCBUF_WRAP_MANY) ) {
            cb->overwrite = value;
        }
        else {
            errno = EINVAL;
            rc = -1;
        }
    }
    else {
        errno = EINVAL;
        rc = -1;
    }
    assert (scbuf_is_valid (cb));
    scbuf_mutex_unlock (cb);
    return (rc);
}


int
scbuf_drop (scbuf_t src, int len)
{
    assert (src != NULL);

    if (len < -1) {
        errno = EINVAL;
        return (-1);
    }
    if (len == 0) {
        return (0);
    }
    scbuf_mutex_lock (src);
    assert (scbuf_is_valid (src));

    if (len == -1) {
        len = src->used;
    }
    else {
        len = MIN (len, src->used);
    }
    if (len > 0) {
        scbuf_dropper (src, len);
    }
    assert (scbuf_is_valid (src));
    scbuf_mutex_unlock (src);
    return (len);
}


int
scbuf_peek (scbuf_t src, void *dstbuf, int len)
{
    int n;

    assert (src != NULL);

    if ((dstbuf == NULL) || (len < 0)) {
        errno = EINVAL;
        return (-1);
    }
    if (len == 0) {
        return (0);
    }
    scbuf_mutex_lock (src);
    assert (scbuf_is_valid (src));
    n = scbuf_reader (src, len, (scbuf_iof) scbuf_put_mem, &dstbuf);
    assert (scbuf_is_valid (src));
    scbuf_mutex_unlock (src);
    return (n);
}


int
scbuf_read (scbuf_t src, void *dstbuf, int len)
{
    int n;

    assert (src != NULL);

    if ((dstbuf == NULL) || (len < 0)) {
        errno = EINVAL;
        return (-1);
    }
    if (len == 0) {
        return (0);
    }
    scbuf_mutex_lock (src);
    assert (scbuf_is_valid (src));
    n = scbuf_reader (src, len, (scbuf_iof) scbuf_put_mem, &dstbuf);
    if (n > 0) {
        scbuf_dropper (src, n);
    }
    assert (scbuf_is_valid (src));
    scbuf_mutex_unlock (src);
    return (n);
}


int
scbuf_replay (scbuf_t src, void *dstbuf, int len)
{
    int n;

    assert (src != NULL);

    if ((dstbuf == NULL) || (len < 0)) {
        errno = EINVAL;
        return (-1);
    }
    if (len == 0) {
        return (0);
    }
    scbuf_mutex_lock (src);
    assert (scbuf_is_valid (src));
    n = scbuf_replayer (src, len, (scbuf_iof) scbuf_put_mem, &dstbuf);
    assert (scbuf_is_valid (src));
    scbuf_mutex_unlock (src);
    return (n);
}


int
scbuf_rewind (scbuf_t src, int len)
{
    int reused;

    assert (src != NULL);

    if (len < -1) {
        errno = EINVAL;
        return (-1);
    }
    if (len == 0) {
        return (0);
    }
    scbuf_mutex_lock (src);
    assert (scbuf_is_valid (src));

    reused = (src->i_out - src->i_rep + (src->size + 1)) % (src->size + 1);
    if (len == -1) {
        len = reused;
    }
    else {
        len = MIN (len, reused);
    }
    if (len > 0) {
        src->used += len;
        src->i_out = (src->i_out - len + (src->size + 1)) % (src->size + 1);
    }
    assert (scbuf_is_valid (src));
    scbuf_mutex_unlock (src);
    return (len);
}


int
scbuf_write (scbuf_t dst, void *srscbuf, int len, int *ndropped, int secure_malloc_flag)
{
    int n;

    assert (dst != NULL);

    if (ndropped) {
        *ndropped = 0;
    }
    if ((srscbuf == NULL) || (len < 0)) {
        errno = EINVAL;
        return (-1);
    }
    if (len == 0) {
        return (0);
    }
    scbuf_mutex_lock (dst);
    assert (scbuf_is_valid (dst));
    n = scbuf_writer (dst, len, (scbuf_iof) scbuf_get_mem, &srscbuf, ndropped, secure_malloc_flag);
    assert (scbuf_is_valid (dst));
    scbuf_mutex_unlock (dst);
    return (n);
}


int
scbuf_drop_line (scbuf_t src, int len, int lines)
{
    int n;

    assert (src != NULL);

    if ((len < 0) || (lines < -1)) {
        errno = EINVAL;
        return (-1);
    }
    if (lines == 0) {
        return (0);
    }
    scbuf_mutex_lock (src);
    assert (scbuf_is_valid (src));

    n = scbuf_find_unread_line (src, len, &lines);
    if (n > 0) {
        scbuf_dropper (src, n);
    }
    assert (scbuf_is_valid (src));
    scbuf_mutex_unlock (src);
    return (n);
}


int
scbuf_peek_line (scbuf_t src, char *dstbuf, int len, int lines)
{
    int n, m, l;
    char *pdst;

    assert (src != NULL);

    if ((dstbuf == NULL) || (len < 0) || (lines < -1)) {
        errno = EINVAL;
        return (-1);
    }
    if (lines == 0) {
        return (0);
    }
    scbuf_mutex_lock (src);
    assert (scbuf_is_valid (src));
    n = scbuf_find_unread_line (src, len - 1, &lines);
    if (n > 0) {
        if (len > 0) {
            m = MIN (n, len - 1);
            if (m > 0) {
                pdst = dstbuf;
                l = scbuf_reader (src, m, (scbuf_iof) scbuf_put_mem, &pdst);
                assert (l == m);
            }
            assert (m < len);
            dstbuf[m] = '\0';
        }
    }
    assert (scbuf_is_valid (src));
    scbuf_mutex_unlock (src);
    return (n);
}


int
scbuf_read_line (scbuf_t src, char *dstbuf, int len, int lines)
{
    int n, m, l;
    char *pdst;

    assert (src != NULL);

    if ((dstbuf == NULL) || (len < 0) || (lines < -1)) {
        errno = EINVAL;
        return (-1);
    }
    if (lines == 0) {
        return (0);
    }
    scbuf_mutex_lock (src);
    assert (scbuf_is_valid (src));
    n = scbuf_find_unread_line (src, len - 1, &lines);
    if (n > 0) {
        if (len > 0) {
            m = MIN (n, len - 1);
            if (m > 0) {
                pdst = dstbuf;
                l = scbuf_reader (src, m, (scbuf_iof) scbuf_put_mem, &pdst);
                assert (l == m);
            }
            assert (m < len);
            dstbuf[m] = '\0';
        }
        scbuf_dropper (src, n);
    }
    assert (scbuf_is_valid (src));
    scbuf_mutex_unlock (src);
    return (n);
}


int
scbuf_replay_line (scbuf_t src, char *dstbuf, int len, int lines)
{
    int n, m, l;
    int nl;
    char *pdst;

    assert (src != NULL);

    if ((dstbuf == NULL) || (len < 0) || (lines < -1)) {
        errno = EINVAL;
        return (-1);
    }
    if (lines == 0) {
        return (0);
    }
    scbuf_mutex_lock (src);
    assert (scbuf_is_valid (src));
    n = scbuf_find_replay_line (src, len - 1, &lines, &nl);
    if (n > 0) {
        if (len > 0) {
            assert ((nl == 0) || (nl == 1));
            m = MIN (n, len - 1 - nl);
            m = MAX (m, 0);
            if (m > 0) {
                pdst = dstbuf;
                l = scbuf_replayer (src, m, (scbuf_iof) scbuf_put_mem, &pdst);
                assert (l == m);
            }
            /*  Append newline if needed and space allows.
             */
            if ((nl) && (len > 1)) {
                dstbuf[m++] = '\n';
            }
            assert (m < len);
            dstbuf[m] = '\0';
            n += nl;
        }
    }
    assert (scbuf_is_valid (src));
    scbuf_mutex_unlock (src);
    return (n);
}


int
scbuf_rewind_line (scbuf_t src, int len, int lines)
{
    int n;

    assert (src != NULL);

    if ((len < 0) || (lines < -1)) {
        errno = EINVAL;
        return (-1);
    }
    if (lines == 0) {
        return (0);
    }
    scbuf_mutex_lock (src);
    assert (scbuf_is_valid (src));

    n = scbuf_find_replay_line (src, len, &lines, NULL);
    if (n > 0) {
        src->used += n;
        src->i_out = (src->i_out - n + (src->size + 1)) % (src->size + 1);
    }
    assert (scbuf_is_valid (src));
    scbuf_mutex_unlock (src);
    return (n);
}


int
scbuf_write_line (scbuf_t dst, char *srscbuf, int *ndropped, int secure_malloc_flag)
{
    int len;
    int nfree, ncopy, n;
    int ndrop = 0, d;
    char *psrc = srscbuf;
    char *newline = "\n";

    assert (dst != NULL);

    if (ndropped) {
        *ndropped = 0;
    }
    if (srscbuf == NULL) {
        errno = EINVAL;
        return (-1);
    }
    /*  Compute number of bytes to effectively copy to dst scbuf.
     *  Reserve space for the trailing newline if needed.
     */
    len = ncopy = strlen (srscbuf);
    if ((len == 0) || (srscbuf[len - 1] != '\n')) {
        len++;
    }
    scbuf_mutex_lock (dst);
    assert (scbuf_is_valid (dst));
    /*
     *  Attempt to grow dst scbuf if necessary.
     */
    nfree = dst->size - dst->used;
    if ((len > nfree) && (dst->size < dst->maxsize)) {
        nfree += scbuf_grow (dst, len - nfree, secure_malloc_flag);
    }
    /*  Determine if src will fit (or be made to fit) in dst scbuf.
     */
    if (dst->overwrite == SCBUF_NO_DROP) {
        if (len > dst->size - dst->used) {
            errno = ENOSPC;
            len = -1;                   /* cannot return while mutex locked */
        }
    }
    else if (dst->overwrite == SCBUF_WRAP_ONCE) {
        if (len > dst->size) {
            errno = ENOSPC;
            len = -1;                   /* cannot return while mutex locked */
        }
    }
    if (len > 0) {
        /*
         *  Discard data that won't fit in dst scbuf.
         */
        if (len > dst->size) {
            ndrop += len - dst->size;
            ncopy -= ndrop;
            psrc += ndrop;
        }
        /*  Copy data from src string to dst scbuf.
         */
        if (ncopy > 0) {
            n = scbuf_writer (dst, ncopy, (scbuf_iof) scbuf_get_mem, &psrc, &d, secure_malloc_flag);
            assert (n == ncopy);
            ndrop += d;
        }
        /*  Append newline if needed.
         */
        if (srscbuf[len - 1] != '\n') {
            n = scbuf_writer (dst, 1, (scbuf_iof) scbuf_get_mem, &newline, &d, secure_malloc_flag);
            assert (n == 1);
            ndrop += d;
        }
    }
    assert (scbuf_is_valid (dst));
    scbuf_mutex_unlock (dst);
    if (ndropped) {
        *ndropped = ndrop;
    }
    return (len);
}


int
scbuf_peek_to_fd (scbuf_t src, int dstfd, int len)
{
    int n = 0;

    assert (src != NULL);

    if ((dstfd < 0) || (len < -1)) {
        errno = EINVAL;
        return (-1);
    }
    scbuf_mutex_lock (src);
    assert (scbuf_is_valid (src));
    if (len == -1) {
        len = src->used;
    }
    if (len > 0) {
        n = scbuf_reader (src, len, (scbuf_iof) scbuf_put_fd, &dstfd);
    }
    assert (scbuf_is_valid (src));
    scbuf_mutex_unlock (src);
    return (n);
}


int
scbuf_read_to_fd (scbuf_t src, int dstfd, int len)
{
    int n = 0;

    assert (src != NULL);

    if ((dstfd < 0) || (len < -1)) {
        errno = EINVAL;
        return (-1);
    }
    scbuf_mutex_lock (src);
    assert (scbuf_is_valid (src));
    if (len == -1) {
        len = src->used;
    }
    if (len > 0) {
        n = scbuf_reader (src, len, (scbuf_iof) scbuf_put_fd, &dstfd);
        if (n > 0) {
            scbuf_dropper (src, n);
        }
    }
    assert (scbuf_is_valid (src));
    scbuf_mutex_unlock (src);
    return (n);
}


int
scbuf_replay_to_fd (scbuf_t src, int dstfd, int len)
{
    int n = 0;

    assert (src != NULL);

    if ((dstfd < 0) || (len < -1)) {
        errno = EINVAL;
        return (-1);
    }
    scbuf_mutex_lock (src);
    assert (scbuf_is_valid (src));
    if (len == -1) {
        len = src->size - src->used;
    }
    if (len > 0) {
        n = scbuf_replayer (src, len, (scbuf_iof) scbuf_put_fd, &dstfd);
    }
    assert (scbuf_is_valid (src));
    scbuf_mutex_unlock (src);
    return (n);
}


int
scbuf_write_from_fd (scbuf_t dst, int srcfd, int len, int *ndropped, int secure_malloc_flag)
{
    int n = 0;

    assert (dst != NULL);

    if (ndropped) {
        *ndropped = 0;
    }
    if ((srcfd < 0) || (len < -1)) {
        errno = EINVAL;
        return (-1);
    }
    scbuf_mutex_lock (dst);
    assert (scbuf_is_valid (dst));
    if (len == -1) {
        /*
         *  Try to use all of the free buffer space available for writing.
         *    If it is all in use, try to grab another chunk.
         */
        len = dst->size - dst->used;
        if (len == 0) {
            len = SCBUF_CHUNK;
        }
    }
    if (len > 0) {
        n = scbuf_writer (dst, len, (scbuf_iof) scbuf_get_fd, &srcfd, ndropped, secure_malloc_flag);
    }
    assert (scbuf_is_valid (dst));
    scbuf_mutex_unlock (dst);
    return (n);
}


int
scbuf_copy (scbuf_t src, scbuf_t dst, int len, int *ndropped, int secure_malloc_flag)
{
    int n = 0;

    assert (src != NULL);
    assert (dst != NULL);

    if (ndropped) {
        *ndropped = 0;
    }
    if (src == dst) {
        errno = EINVAL;
        return (-1);
    }
    if (len < -1) {
        errno = EINVAL;
        return (-1);
    }
    if (len == 0) {
        return (0);
    }
    /*  Lock scbufs in order of lowest memory address to prevent deadlock.
     */
    if (src < dst) {
        scbuf_mutex_lock (src);
        scbuf_mutex_lock (dst);
    }
    else {
        scbuf_mutex_lock (dst);
        scbuf_mutex_lock (src);
    }
    assert (scbuf_is_valid (src));
    assert (scbuf_is_valid (dst));

    if (len == -1) {
        len = src->used;
    }
    if (len > 0) {
        n = scbuf_copier (src, dst, len, ndropped, secure_malloc_flag);
    }
    assert (scbuf_is_valid (src));
    assert (scbuf_is_valid (dst));
    scbuf_mutex_unlock (src);
    scbuf_mutex_unlock (dst);
    return (n);
}


int
scbuf_move (scbuf_t src, scbuf_t dst, int len, int *ndropped, int secure_malloc_flag)
{
    int n = 0;

    assert (src != NULL);
    assert (dst != NULL);

    if (ndropped) {
        *ndropped = 0;
    }
    if (src == dst) {
        errno = EINVAL;
        return (-1);
    }
    if (len < -1) {
        errno = EINVAL;
        return (-1);
    }
    if (len == 0) {
        return (0);
    }
    /*  Lock scbufs in order of lowest memory address to prevent deadlock.
     */
    if (src < dst) {
        scbuf_mutex_lock (src);
        scbuf_mutex_lock (dst);
    }
    else {
        scbuf_mutex_lock (dst);
        scbuf_mutex_lock (src);
    }
    assert (scbuf_is_valid (src));
    assert (scbuf_is_valid (dst));

    if (len == -1) {
        len = src->used;
    }
    if (len > 0) {
        n = scbuf_copier (src, dst, len, ndropped, secure_malloc_flag);
        if (n > 0) {
            scbuf_dropper (src, n);
        }
    }
    assert (scbuf_is_valid (src));
    assert (scbuf_is_valid (dst));
    scbuf_mutex_unlock (src);
    scbuf_mutex_unlock (dst);
    return (n);
}


static int
scbuf_find_replay_line (scbuf_t cb, int chars, int *nlines, int *nl)
{
/*  Finds the specified number of lines from the replay region of the buffer.
 *  If ([nlines] > 0), returns the number of bytes comprising the line count,
 *    or 0 if this number of lines is not available (ie, all or none).
 *  If ([nlines] == -1), returns the number of bytes comprising the maximum
 *    line count bounded by the number of characters specified by [chars].
 *  Only complete lines (ie, those terminated by a newline) are counted,
 *    with once exception: the most recent line of replay data is treated
 *    as a complete line regardless of the presence of a terminating newline.
 *  Sets the value-result parameter [nlines] to the number of lines found.
 *  Sets [nl] to '1' if a newline is required to terminate the replay data.
 */
    int i, n, m, l;
    int lines;

    assert (cb != NULL);
    assert (nlines != NULL);
    assert (*nlines >= -1);
    assert (scbuf_mutex_is_locked (cb));

    n = m = l = 0;
    lines = *nlines;
    *nlines = 0;

    if (nl) {
        *nl = 0;                        /* init in case of early return */
    }
    if ((lines == 0) || ((lines <= -1) && (chars <= 0))) {
        return (0);
    }
    if (cb->i_out == cb->i_rep) {
        return (0);                     /* no replay data available */
    }
    if (lines > 0) {
        chars = -1;                     /* chars parm not used if lines > 0 */
    }
    else {
        ++chars;                        /* incr to allow for preceding '\n' */
    }
    /*  Since the most recent line of replay data is considered implicitly
     *    terminated, decrement the char count to account for the newline
     *    if one is not present, or increment the line count if one is.
     *  Note: cb->data[(O - 1 + (S+1)) % (S+1)] is the last replayable char.
     */
    if (cb->data[(cb->i_out + cb->size) % (cb->size + 1)] != '\n') {
        if (nl) {
            *nl = 1;
        }
        --chars;
    }
    else {
        if (lines > 0) {
            ++lines;
        }
        --l;
    }
    i = cb->i_out;
    while (i != cb->i_rep) {
        i = (i + cb->size) % (cb->size + 1); /* (i - 1 + (S+1)) % (S+1) */
        ++n;
        if (chars > 0) {
            --chars;
        }
        /*  Complete lines are identified by a preceding newline.
         */
        if (cb->data[i] == '\n') {
            if (lines > 0) {
                --lines;
            }
            m = n - 1;                  /* do not include preceding '\n' */
            ++l;
        }
        if ((chars == 0) || (lines == 0)) {
            break;
        }
    }
    /*  But the first line written in does not need a preceding newline.
     */
    if ((!cb->got_wrap) && ((chars > 0) || (lines > 0))) {
        if (lines > 0) {
            --lines;
        }
        m = n;
        ++l;
    }
    if (lines > 0) {
        return (0);                     /* all or none, and not enough found */
    }
    *nlines = l;
    return (m);
}


static int
scbuf_find_unread_line (scbuf_t cb, int chars, int *nlines)
{
/*  Finds the specified number of lines from the unread region of the buffer.
 *  If ([nlines] > 0), returns the number of bytes comprising the line count,
 *    or 0 if this number of lines is not available (ie, all or none).
 *  If ([nlines] == -1), returns the number of bytes comprising the maximum
 *    line count bounded by the number of characters specified by [chars].
 *  Only complete lines (ie, those terminated by a newline) are counted.
 *  Sets the value-result parameter [nlines] to the number of lines found.
 */
    int i, n, m, l;
    int lines;

    assert (cb != NULL);
    assert (nlines != NULL);
    assert (*nlines >= -1);
    assert (scbuf_mutex_is_locked (cb));

    n = m = l = 0;
    lines = *nlines;
    *nlines = 0;

    if ((lines == 0) || ((lines <= -1) && (chars <= 0))) {
        return (0);
    }
    if (cb->used == 0) {
        return (0);                     /* no unread data available */
    }
    if (lines > 0) {
        chars = -1;                     /* chars parm not used if lines > 0 */
    }
    i = cb->i_out;
    while (i != cb->i_in) {
        ++n;
        if (chars > 0) {
            --chars;
        }
        if (cb->data[i] == '\n') {
            if (lines > 0) {
                --lines;
            }
            m = n;
            ++l;
        }
        if ((chars == 0) || (lines == 0)) {
            break;
        }
        i = (i + 1) % (cb->size + 1);
    }
    if (lines > 0) {
        return (0);                     /* all or none, and not enough found */
    }
    *nlines = l;
    return (m);
}


static int
scbuf_get_fd (void *dstbuf, int *psrcfd, int len)
{
/*  Copies data from the file referenced by the file descriptor
 *    pointed at by [psrcfd] into scbuf's [dstbuf].
 *  Returns the number of bytes read from the fd, 0 on EOF, or -1 on error.
 */
    int n;

    assert (dstbuf != NULL);
    assert (psrcfd != NULL);
    assert (*psrcfd >= 0);
    assert (len > 0);

    do {
        n = read (*psrcfd, dstbuf, len);
    } while ((n < 0) && (errno == EINTR));
    return (n);
}


static int
scbuf_get_mem (void *dstbuf, unsigned char **psrscbuf, int len)
{
/*  Copies data from the buffer pointed at by [psrscbuf] into scbuf's [dstbuf].
 *  Returns the number of bytes copied.
 */
    assert (dstbuf != NULL);
    assert (psrscbuf != NULL);
    assert (*psrscbuf != NULL);
    assert (len > 0);

    memcpy (dstbuf, *psrscbuf, len);
    *psrscbuf += len;
    return (len);
}


static int
scbuf_put_fd (void *srscbuf, int *pdstfd, int len)
{
/*  Copies data from scbuf's [srscbuf] into the file referenced
 *    by the file descriptor pointed at by [pdstfd].
 *  Returns the number of bytes written to the fd, or -1 on error.
 */
    int n;

    assert (srscbuf != NULL);
    assert (pdstfd != NULL);
    assert (*pdstfd >= 0);
    assert (len > 0);

    do {
        n = write (*pdstfd, srscbuf, len);
    } while ((n < 0) && (errno == EINTR));
    return (n);
}


static int
scbuf_put_mem (void *srscbuf, unsigned char **pdstbuf, int len)
{
/*  Copies data from scbuf's [srscbuf] into the buffer pointed at by [pdstbuf].
 *  Returns the number of bytes copied.
 */
    assert (srscbuf != NULL);
    assert (pdstbuf != NULL);
    assert (*pdstbuf != NULL);
    assert (len > 0);

    memcpy (*pdstbuf, srscbuf, len);
    *pdstbuf += len;
    return (len);
}


static int
scbuf_copier (scbuf_t src, scbuf_t dst, int len, int *ndropped, int secure_malloc_flag)
{
/*  Copies up to [len] bytes from the [src] scbuf into the [dst] scbuf.
 *  Returns the number of bytes copied, or -1 on error (with errno set).
 *  Sets [ndropped] (if not NULL) to the number of [dst] bytes overwritten.
 */
    int ncopy, nfree, nleft, nrepl, n;
    int i_src, i_dst;

    assert (src != NULL);
    assert (dst != NULL);
    assert (len > 0);
    assert (scbuf_mutex_is_locked (src));
    assert (scbuf_mutex_is_locked (dst));

    /*  Bound len by the number of bytes available.
     */
    len = MIN (len, src->used);
    if (len == 0) {
        return (0);
    }
    /*  Attempt to grow dst scbuf if necessary.
     */
    nfree = dst->size - dst->used;
    if ((len > nfree) && (dst->size < dst->maxsize)) {
        nfree += scbuf_grow (dst, len - nfree, secure_malloc_flag);
    }
    /*  Compute number of bytes to effectively copy to dst scbuf.
     */
    if (dst->overwrite == SCBUF_NO_DROP) {
        len = MIN (len, dst->size - dst->used);
        if (len == 0) {
            errno = ENOSPC;
            return (-1);
        }
    }
    else if (dst->overwrite == SCBUF_WRAP_ONCE) {
        len = MIN (len, dst->size);
    }
    /*  Compute number of bytes that will be overwritten in dst scbuf.
     */
    if (ndropped) {
        *ndropped = MAX (0, len - dst->size + dst->used);
    }
    /*  Compute number of bytes to physically copy to dst scbuf.  This prevents
     *    copying data that will overwritten if the scbuf wraps multiple times.
     */
    ncopy = len;
    i_src = src->i_out;
    i_dst = dst->i_in;
    if (ncopy > dst->size) {
        n = ncopy - dst->size;
        i_src = (i_src + n) % (src->size + 1);
        ncopy -= n;
    }
    /*  Copy data from src scbuf to dst scbuf.
     */
    nleft = ncopy;
    while (nleft > 0) {
        n = MIN (((src->size + 1) - i_src), ((dst->size + 1) - i_dst));
        n = MIN (n, nleft);
        memcpy (&dst->data[i_dst], &src->data[i_src], n);
        i_src = (i_src + n) % (src->size + 1);
        i_dst = (i_dst + n) % (dst->size + 1);
        nleft -= n;
    }
    /*  Update dst scbuf metadata.
     */
    if (ncopy > 0) {
        nrepl = (dst->i_out - dst->i_rep + (dst->size + 1)) % (dst->size + 1);
        dst->used = MIN (dst->used + ncopy, dst->size);
        assert (i_dst == (dst->i_in + ncopy) % (dst->size + 1));
        dst->i_in = i_dst;
        if (ncopy > nfree - nrepl) {
            dst->got_wrap = 1;
            dst->i_rep = (dst->i_in + 1) % (dst->size + 1);
        }
        if (ncopy > nfree) {
            dst->i_out = dst->i_rep;
        }
    }
    return (len);
}


static int
scbuf_dropper (scbuf_t cb, int len)
{
/*  Discards exactly [len] bytes of unread data from [cb].
 *  Returns the number of bytes dropped.
 */
    assert (cb != NULL);
    assert (len > 0);
    assert (len <= cb->used);
    assert (scbuf_mutex_is_locked (cb));

    cb->used -= len;
    cb->i_out = (cb->i_out + len) % (cb->size + 1);

    /*  Attempt to shrink scbuf if possible.
     */
    if ((cb->size - cb->used > SCBUF_CHUNK) && (cb->size > cb->minsize)) {
        scbuf_shrink (cb);
    }
    /*  Don't call me clumsy, don't call me a fool.
     *  When things fall down on me, I'm following the rule.
     */
    return (len);
}


static int
scbuf_reader (scbuf_t src, int len, scbuf_iof putf, void *dst)
{
/*  Reads up to [len] bytes from [src] into the object pointed at by [dst].
 *    The I/O function [putf] specifies how data is written into [dst].
 *  Returns the number of bytes read, or -1 on error (with errno set).
 *  Note that [dst] is a value-result parameter and will be "moved forward"
 *    by the number of bytes written into it.
 */
    int nleft, n, m;
    int i_src;

    assert (src != NULL);
    assert (len > 0);
    assert (putf != NULL);
    assert (dst != NULL);
    assert (scbuf_mutex_is_locked (src));

    /*  Bound len by the number of bytes available.
     */
    len = MIN (len, src->used);
    if (len == 0) {
        return (0);
    }
    /*  Copy data from src scbuf to dst obj.  Do the scbuf hokey-pokey and
     *    wrap-around the buffer at most once.  Break out if putf() returns
     *    either an ERR or a short count.
     */
    i_src = src->i_out;
    nleft = len;
    m = 0;
    while (nleft > 0) {
        n = MIN (nleft, (src->size + 1) - i_src);
        m = putf (&src->data[i_src], dst, n);
        if (m > 0) {
            nleft -= m;
            i_src = (i_src + m) % (src->size + 1);
        }
        if (n != m) {
            break;                      /* got ERR or "short" putf() */
        }
    }
    /*  Compute number of bytes written to dst obj.
     */
    n = len - nleft;
    assert ((n >= 0) && (n <= len));
    /*
     *  If no data has been written, return the ERR reported by putf().
     */
    if (n == 0) {
        return (m);
    }
    return (n);
}


static int
scbuf_replayer (scbuf_t src, int len, scbuf_iof putf, void *dst)
{
/*  Replays up to [len] bytes from [src] into the object pointed at by [dst].
 *    The I/O function [putf] specifies how data is written into [dst].
 *  Returns the number of bytes replayed, or -1 on error (with errno set).
 *  Note that [dst] is a value-result parameter and will be "moved forward"
 *    by the number of bytes written into it.
 */
    int nleft, n, m;
    int i_src;

    assert (src != NULL);
    assert (len > 0);
    assert (putf != NULL);
    assert (dst != NULL);
    assert (scbuf_mutex_is_locked (src));

    /*  Bound len by the number of bytes available.
     */
    n = (src->i_out - src->i_rep + (src->size + 1)) % (src->size + 1);
    len = MIN (len, n);
    if (len == 0) {
        return (0);
    }
    /*  Copy data from src scbuf to dst obj.  Do the scbuf hokey-pokey and
     *    wrap-around the buffer at most once.  Break out if putf() returns
     *    either an ERR or a short count.
     */
    i_src = (src->i_out - len + (src->size + 1)) % (src->size + 1);
    nleft = len;
    m = 0;
    while (nleft > 0) {
        n = MIN (nleft, (src->size + 1) - i_src);
        m = putf (&src->data[i_src], dst, n);
        if (m > 0) {
            nleft -= m;
            i_src = (i_src + m) % (src->size + 1);
        }
        if (n != m) {
            break;                      /* got ERR or "short" putf() */
        }
    }
    /*  Compute number of bytes written to dst obj.
     */
    n = len - nleft;
    assert ((n >= 0) && (n <= len));
    /*
     *  If no data has been written, return the ERR reported by putf().
     */
    if (n == 0) {
        return (m);
    }
    return (n);
}


static int
scbuf_writer (scbuf_t dst, int len, scbuf_iof getf, void *src, int *ndropped, int secure_malloc_flag)
{
/*  Writes up to [len] bytes from the object pointed at by [src] into [dst].
 *    The I/O function [getf] specifies how data is read from [src].
 *  Returns the number of bytes written, or -1 on error (with errno set).
 *  Sets [ndropped] (if not NULL) to the number of [dst] bytes overwritten.
 *  Note that [src] is a value-result parameter and will be "moved forward"
 *    by the number of bytes read from it.
 */
    int nfree, nleft, nrepl, n, m;
    int i_dst;

    assert (dst != NULL);
    assert (len > 0);
    assert (getf != NULL);
    assert (src != NULL);
    assert (scbuf_mutex_is_locked (dst));

    /*  Attempt to grow dst scbuf if necessary.
     */
    nfree = dst->size - dst->used;
    if ((len > nfree) && (dst->size < dst->maxsize)) {
        nfree += scbuf_grow (dst, len - nfree, secure_malloc_flag);
    }
    /*  Compute number of bytes to write to dst scbuf.
     */
    if (dst->overwrite == SCBUF_NO_DROP) {
        len = MIN (len, dst->size - dst->used);
        if (len == 0) {
            errno = ENOSPC;
            return (-1);
        }
    }
    else if (dst->overwrite == SCBUF_WRAP_ONCE) {
        len = MIN (len, dst->size);
    }
    /*  Copy data from src obj to dst scbuf.  Do the scbuf hokey-pokey and
     *    wrap-around the buffer as needed.  Break out if getf() returns
     *    either an EOF/ERR or a short count.
     */
    i_dst = dst->i_in;
    nleft = len;
    m = 0;
    while (nleft > 0) {
        n = MIN (nleft, (dst->size + 1) - i_dst);
        m = getf (&dst->data[i_dst], src, n);
        if (m > 0) {
            nleft -= m;
            i_dst = (i_dst + m) % (dst->size + 1);
        }
        if (n != m) {
            break;                      /* got EOF/ERR or "short" getf() */
        }
    }
    /*  Compute number of bytes written to dst scbuf.
     */
    n = len - nleft;
    assert ((n >= 0) && (n <= len));
    /*
     *  If no data has been written, return the EOF/ERR reported by getf().
     */
    if (n == 0) {
        return (m);
    }
    /*  Update dst scbuf metadata.
     */
    if (n > 0) {
        nrepl = (dst->i_out - dst->i_rep + (dst->size + 1)) % (dst->size + 1);
        dst->used = MIN (dst->used + n, dst->size);
        assert (i_dst == (dst->i_in + n) % (dst->size + 1));
        dst->i_in = i_dst;
        if (n > nfree - nrepl) {
            dst->got_wrap = 1;
            dst->i_rep = (dst->i_in + 1) % (dst->size + 1);
        }
        if (n > nfree) {
            dst->i_out = dst->i_rep;
        }
    }
    if (ndropped) {
        *ndropped = MAX (0, n - nfree);
    }
    return (n);
}


static int
scbuf_grow (scbuf_t cb, int n, int secure_malloc_flag)
{
/*  Attempts to grow the circular buffer [cb] by at least [n] bytes.
 *  Returns the number of bytes by which the buffer has grown (which may be
 *    less-than, equal-to, or greater-than the number of bytes requested).
 */
    unsigned char *data;
    unsigned char *new_data;
    int size_old, size_meta;
    int m;

    assert (cb != NULL);
    assert (n > 0);
    assert (scbuf_mutex_is_locked (cb));

    if (cb->size == cb->maxsize) {
        return (0);
    }
    size_old = cb->size;
    size_meta = cb->alloc - cb->size;   /* size of sentinel & magic cookies */
    assert (size_meta > 0);

    /*  Attempt to grow data buffer by multiples of the chunk-size.
     */
    m = cb->alloc + n;
    m = m + (SCBUF_CHUNK - (m % SCBUF_CHUNK));
    m = MIN (m, (cb->maxsize + size_meta));
    assert (m > cb->alloc);

    data = cb->data;
#ifndef NDEBUG
    data -= SCBUF_MAGIC_LEN;             /* jump back to what malloc returned */
#endif /* !NDEBUG */

    if (secure_malloc_flag) {
        if (!(new_data = secure_malloc(m))) {
            /*
             *  XXX: Set flag or somesuch to prevent regrowing when out of memory?
             */
          return (0);                     /* unable to grow data buffer */
        }
    }
    else {
        if (!(data = realloc (data, m))) {
            /*
             *  XXX: Set flag or somesuch to prevent regrowing when out of memory?
             */
          return (0);                     /* unable to grow data buffer */
        }
    }
    if (secure_malloc_flag) {
        secure_free(data, cb->alloc);
        cb->data = new_data;
    }
    else
        cb->data = data;
    cb->alloc = m;
    cb->size = m - size_meta;

#ifndef NDEBUG
    /*  A round cookie with one bite out of it looks like a C.
     *  The underflow cookie will have been copied by realloc() if needed.
     *    But the overflow cookie must be rebaked.
     *  Must use memcpy since overflow cookie may not be word-aligned.
     */
    cb->data += SCBUF_MAGIC_LEN;         /* jump forward past underflow magic */
    memcpy (cb->data + cb->size + 1, (void *) &cb->magic, SCBUF_MAGIC_LEN);
#endif /* !NDEBUG */

    /*  The memory containing replay and unread data must be contiguous modulo
     *    the buffer size.  Additional memory must be inserted between where
     *    new data is written in (i_in) and where replay data starts (i_rep).
     *  If replay data wraps-around the old buffer, move it to the new end
     *    of the buffer so it wraps-around in the same manner.
     */
    if (cb->i_rep > cb->i_in) {
        n = (size_old + 1) - cb->i_rep;
        m = (cb->size + 1) - n;
        memmove (cb->data + m, cb->data + cb->i_rep, n);

        if (cb->i_out >= cb->i_rep) {
            cb->i_out += m - cb->i_rep;
        }
        cb->i_rep = m;
    }
    assert (scbuf_is_valid (cb));
    return (cb->size - size_old);
}


static int
scbuf_shrink (scbuf_t cb)
{
/*  XXX: DOCUMENT ME.
 */
    assert (cb != NULL);
    assert (scbuf_mutex_is_locked (cb));
    assert (scbuf_is_valid (cb));

    if (cb->size == cb->minsize) {
        return (0);
    }
    if (cb->size - cb->used <= SCBUF_CHUNK) {
        return (0);
    }
    /*  FIXME: NOT IMPLEMENTED.
     */
    assert (scbuf_is_valid (cb));
    return (0);
}


#ifndef NDEBUG
#ifdef WITH_PTHREADS
static int
scbuf_mutex_is_locked (scbuf_t cb)
{
/*  Returns true if the mutex is locked; o/w, returns false.
 */
    int rc;

    assert (cb != NULL);
    rc = pthread_mutex_trylock (&cb->mutex);
    return (rc == EBUSY ? 1 : 0);
}
#endif /* WITH_PTHREADS */
#endif /* !NDEBUG */


#ifndef NDEBUG
static int
scbuf_is_valid (scbuf_t cb)
{
/*  Validates the data structure.  All invariants should be tested here.
 *  Returns true if everything is valid; o/w, aborts due to assertion failure.
 */
    int nfree;

    assert (cb != NULL);
    assert (scbuf_mutex_is_locked (cb));
    assert (cb->data != NULL);
    assert (cb->magic == SCBUF_MAGIC);
    /*
     *  Must use memcmp since overflow cookie may not be word-aligned.
     */
    assert (memcmp (cb->data - SCBUF_MAGIC_LEN,
        (void *) &cb->magic, SCBUF_MAGIC_LEN) == 0);
    assert (memcmp (cb->data + cb->size + 1,
        (void *) &cb->magic, SCBUF_MAGIC_LEN) == 0);

    assert (cb->alloc > 0);
    assert (cb->alloc > cb->size);
    assert (cb->size > 0);
    assert (cb->size >= cb->minsize);
    assert (cb->size <= cb->maxsize);
    assert (cb->minsize > 0);
    assert (cb->maxsize > 0);
    assert (cb->used >= 0);
    assert (cb->used <= cb->size);
    assert (cb->overwrite == SCBUF_NO_DROP
         || cb->overwrite == SCBUF_WRAP_ONCE
         || cb->overwrite == SCBUF_WRAP_MANY);
    assert (cb->got_wrap || !cb->i_rep);/* i_rep = 0 if data has not wrapped */
    assert (cb->i_in >= 0);
    assert (cb->i_in <= cb->size);
    assert (cb->i_out >= 0);
    assert (cb->i_out <= cb->size);
    assert (cb->i_rep >= 0);
    assert (cb->i_rep <= cb->size);

    if (cb->i_in >= cb->i_out) {
        assert ((cb->i_rep > cb->i_in) || (cb->i_rep <= cb->i_out));
    }
    else /* if (cb->in < cb->i_out) */ {
        assert ((cb->i_rep > cb->i_in) && (cb->i_rep <= cb->i_out));
    }
    nfree = (cb->i_out - cb->i_in - 1 + (cb->size + 1)) % (cb->size + 1);
    assert (cb->size - cb->used == nfree);

    return (1);
}
#endif /* !NDEBUG */
