/*****************************************************************************
 *  $Id: scbuf.h,v 1.3 2009-03-03 23:56:52 chu11 Exp $
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
 *****************************************************************************/

/*
 * achu: This is identical to the original cbuf code but with some
 * secure malloc changes.  Outside of the minor code differences,
 * 'cbuf' has been renamed to 'scbuf' pretty much globally.
 */

#ifndef LSD_SCBUF_H
#define LSD_SCBUF_H

/*****************************************************************************
 *  Notes
 *****************************************************************************/
/*
 *  Scbuf is a circular-buffer capable of dynamically resizing itself.
 *  Unread data in the buffer will be overwritten once the scbuf has
 *  reached its maximum size or is unable to allocate additional memory.
 *
 *  The SCBUF_OPT_OVERWRITE option specifies how unread scbuf data will
 *  be overwritten.  If set to SCBUF_NO_DROP, unread data will never be
 *  overwritten; writes into the scbuf will return -1 with ENOSPC.  If set
 *  to SCBUF_WRAP_ONCE, a single write operation will wrap-around the buffer
 *  at most once, and up to scbuf_used() bytes of data may be overwritten.
 *  If set to SCBUF_WRAP_MANY, a single write operation will wrap-around the
 *  buffer as many times as needed in order to write all of the data.
 *
 *  If NDEBUG is not defined, internal debug code will be enabled.  This is
 *  intended for development use only and production code should define NDEBUG.
 *
 *  If WITH_LSD_FATAL_ERROR_FUNC is defined, the linker will expect to
 *  find an external lsd_fatal_error(file,line,mesg) function.  By default,
 *  lsd_fatal_error(file,line,mesg) is a macro definition that outputs an
 *  error message to stderr.  This macro may be redefined to invoke another
 *  routine instead.
 *
 *  If WITH_LSD_NOMEM_ERROR_FUNC is defined, the linker will expect to
 *  find an external lsd_nomem_error(file,line,mesg) function.  By default,
 *  lsd_nomem_error(file,line,mesg) is a macro definition that returns NULL.
 *  This macro may be redefined to invoke another routine instead.
 *
 *  If WITH_PTHREADS is defined, these routines will be thread-safe.
 */


/*****************************************************************************
 *  Data Types
 *****************************************************************************/

typedef struct scbuf * scbuf_t;           /* circular-buffer opaque data type  */

typedef enum {                          /* scbuf option names                 */
    SCBUF_OPT_OVERWRITE
} scbuf_opt_t;

typedef enum {                          /* SCBUF_OPT_OVERWRITE values:        */
    SCBUF_NO_DROP,                       /* -never drop data, ENOSPC if full  */
    SCBUF_WRAP_ONCE,                     /* -drop data, wrapping at most once */
    SCBUF_WRAP_MANY                      /* -drop data, wrapping as needed    */
} scbuf_overwrite_t;


/*****************************************************************************
 *  Functions
 *****************************************************************************/

scbuf_t scbuf_create (int minsize, int maxsize, int secure_malloc_flag);
/*
 *  Creates and returns a new circular buffer, or lsd_nomem_error() on failure.
 *  The buffer is initially allocated to hold [minsize] bytes of data,
 *    but can attempt to grow up to [maxsize] bytes before overwriting data.
 *  Set minsize = maxsize to prevent scbuf from dynamically resizing itself.
 *  The default overwrite option behavior is SCBUF_WRAP_MANY.
 *  Abandoning a scbuf without calling scbuf_destroy() will cause a memory leak.
 */

void scbuf_destroy (scbuf_t cb, int secure_malloc_flag);
/*
 *  Destroys the circular buffer [cb].
 */

void scbuf_flush (scbuf_t cb);
/*
 *  Flushes all data (including replay data) in [cb].
 */

int scbuf_size (scbuf_t cb);
/*
 *  Returns the maximum size of the buffer allocated to [cb]
 *    (ie, the number of bytes it can currently hold).
 */

int scbuf_free (scbuf_t cb);
/*
 *  Returns the number of bytes in [cb] available for writing before unread
 *    data is overwritten (assuming the scbuf can resize itself if needed).
 */

int scbuf_used (scbuf_t cb);
/*
 *  Returns the number of bytes in [cb] available for reading.
 */

int scbuf_lines_used (scbuf_t cb);
/*
 *  Returns the number of lines in [cb] available for reading.
 */

int scbuf_reused (scbuf_t cb);
/*
 *  Returns the number of bytes in [cb] available for replaying/rewinding.
 */

int scbuf_lines_reused (scbuf_t cb);
/*
 *  Returns the number of lines in [cb] available for replaying/rewinding.
 */

int scbuf_is_empty (scbuf_t cb);
/*
 *  Returns non-zero if [cb] is empty; o/w, returns zero.
 */

int scbuf_opt_get (scbuf_t cb, scbuf_opt_t name, int *value);
/*
 *  Gets the [name] option for [cb] and sets [value] to the result.
 *  Returns 0 on success, or -1 on error (with errno set).
 */

int scbuf_opt_set (scbuf_t cb, scbuf_opt_t name, int value);
/*
 *  Sets the [name] option for [cb] to [value].
 *  Returns 0 on success, or -1 on error (with errno set).
 */

int scbuf_drop (scbuf_t src, int len);
/*
 *  Discards up to [len] bytes of unread data from [src];
 *    if [len] is -1, all unread data will be dropped.
 *  Dropped data is still available via the replay buffer.
 *  Returns the number of bytes dropped, or -1 on error (with errno set).
 */

int scbuf_peek (scbuf_t src, void *dstbuf, int len);
/*
 *  Reads up to [len] bytes of data from the [src] scbuf into [dstbuf],
 *    but does not consume the data read from the scbuf.
 *  The "peek" can be committed to the scbuf via a call to scbuf_drop(),
 *    but the peek+drop combination is not atomic.
 *  Returns the number of bytes read, or -1 on error (with errno set).
 */

int scbuf_read (scbuf_t src, void *dstbuf, int len);
/*
 *  Reads up to [len] bytes of data from the [src] scbuf into [dstbuf].
 *  Returns the number of bytes read, or -1 on error (with errno set).
 */

int scbuf_replay (scbuf_t src, void *dstbuf, int len);
/*
 *  Replays up to [len] bytes of previously read data from the [src] scbuf
 *    into [dstbuf].
 *  Returns the number of bytes replayed, or -1 on error (with errno set).
 */

int scbuf_rewind (scbuf_t src, int len);
/*
 *  Rewinds [src] by up to [len] bytes, placing previously read data back in
 *    the unread data buffer; if [len] is -1, all replay data will be rewound.
 *  Returns the number of bytes rewound, or -1 on error (with errno set).
 */

int scbuf_write (scbuf_t dst, void *srscbuf, int len, int *ndropped, int secure_malloc_flag);
/*
 *  Writes up to [len] bytes of data from [srscbuf] into the [dst] scbuf
 *    according to dst's SCBUF_OPT_OVERWRITE behavior.
 *  Returns the number of bytes written, or -1 on error (with errno set).
 *    Sets [ndropped] (if not NULL) to the number of bytes overwritten.
 */

int scbuf_drop_line (scbuf_t src, int len, int lines);
/*
 *  Discards the specified [lines] of data from [src].  If [lines] is -1,
 *    discards the maximum number of lines comprised of up to [len] characters.
 *  Dropped data is still available via the replay buffer.
 *  Returns the number of bytes dropped, or -1 on error (with errno set).
 *    Returns 0 if the number of lines is not available (ie, all or none).
 */

int scbuf_peek_line (scbuf_t src, char *dstbuf, int len, int lines);
/*
 *  Reads the specified [lines] of data from the [src] scbuf into [dstbuf],
 *    but does not consume the data read from the scbuf.  If [lines] is -1,
 *    reads the maximum number of lines that [dstbuf] can hold.  The buffer
 *    will be NUL-terminated and contain at most ([len] - 1) characters.
 *  The "peek" can be committed to the scbuf via a call to scbuf_drop(),
 *    but the peek+drop combination is not atomic.
 *  Returns strlen of the line(s) on success; truncation occurred if >= [len].
 *    Returns 0 if the number of lines is not available (ie, all or none).
 *    Returns -1 on error (with errno set).
 */

int scbuf_read_line (scbuf_t src, char *dstbuf, int len, int lines);
/*
 *  Reads the specified [lines] of data from the [src] scbuf into [dstbuf].
 *    If [lines] is -1, reads the maximum number of lines that [dstbuf]
 *    can hold.  The buffer will be NUL-terminated and contain at most
 *    ([len] - 1) characters.
 *  Returns strlen of the line(s) on success; truncation occurred if >= [len],
 *    in which case excess line data is discarded.  Returns 0 if the number
 *    of lines is not available (ie, all or none), in which case no data is
 *    consumed.  Returns -1 on error (with errno set).
 */

int scbuf_replay_line (scbuf_t src, char *dstbuf, int len, int lines);
/*
 *  Replays the specified [lines] of data from the [src] scbuf into [dstbuf].
 *    If [lines] is -1, replays the maximum number of lines that [dstbuf]
 *    can hold.  A newline will be appended to [dstbuf] if the last (ie, most
 *    recently read) line does not contain a trailing newline.  The buffer
 *    will be NUL-terminated and contain at most ([len] - 1) characters.
 *  Returns strlen of the line(s) on success; truncation occurred if >= [len].
 *    Returns 0 if the number of lines is not available (ie, all or none).
 *    Returns -1 on error (with errno set).
 */

int scbuf_rewind_line (scbuf_t src, int len, int lines);
/*
 *  Rewinds [src] by the specified [lines] of data, placing previously read
 *    data back in the unread data buffer.  If [lines] is -1, rewinds the
 *    maximum number of lines comprised of up to [len] characters.
 *  Returns the number of bytes rewound, or -1 on error (with errno set).
 *    Returns 0 if the number of lines is not available (ie, all or none).
 */

int scbuf_write_line (scbuf_t dst, char *srscbuf, int *ndropped, int secure_malloc_flag);
/*
 *  Writes the entire NUL-terminated [srscbuf] string into the [dst] scbuf
 *    according to dst's SCBUF_OPT_OVERWRITE behavior.  A newline will be
 *    appended to the scbuf if [srscbuf] does not contain a trailing newline.
 *  Returns the number of bytes written, or -1 or error (with errno set).
 *    Sets [ndropped] (if not NULL) to the number of bytes overwritten.
 */

int scbuf_peek_to_fd (scbuf_t src, int dstfd, int len);
/*
 *  Reads up to [len] bytes of data from the [src] scbuf into the file
 *    referenced by the [dstfd] file descriptor, but does not consume the
 *    data read from the scbuf.  If [len] is -1, it will be set to the number
 *    of [src] bytes available for reading.
 *  The "peek" can be committed to the scbuf via a call to scbuf_drop(),
 *    but the peek+drop combination is not atomic.
 *  Returns the number of bytes read, or -1 on error (with errno set).
 */

int scbuf_read_to_fd (scbuf_t src, int dstfd, int len);
/*
 *  Reads up to [len] bytes of data from the [src] scbuf into the file
 *    referenced by the [dstfd] file descriptor.  If [len] is -1, it will
 *    be set to the number of [src] bytes available for reading.
 *  Returns the number of bytes read, or -1 on error (with errno set).
 */

int scbuf_replay_to_fd (scbuf_t src, int dstfd, int len);
/*
 *  Replays up to [len] bytes of previously read data from the [src] scbuf into
 *    the file referenced by the [dstfd] file descriptor.  If [len] is -1, it
 *    will be set to the maximum number of [src] bytes available for replay.
 *  Returns the number of bytes replayed, or -1 on error (with errno set).
 */

int scbuf_write_from_fd (scbuf_t dst, int srcfd, int len, int *ndropped, int secure_malloc_flag);
/*
 *  Writes up to [len] bytes of data from the file referenced by the
 *    [srcfd] file descriptor into the [dst] scbuf according to dst's
 *    SCBUF_OPT_OVERWRITE behavior.  If [len] is -1, it will be set to
 *    an appropriate chunk size.
 *  Returns the number of bytes written, 0 on EOF, or -1 on error (with errno).
 *    Sets [ndropped] (if not NULL) to the number of bytes overwritten.
 */

int scbuf_copy (scbuf_t src, scbuf_t dst, int len, int *ndropped, int secure_malloc_flag);
/*
 *  Copies up to [len] bytes of data from the [src] scbuf into the [dst] scbuf
 *    according to dst's SCBUF_OPT_OVERWRITE behavior.  If [len] is -1,
 *    it will be set to the number of [src] bytes available for reading.
 *  Returns the number of bytes copied, or -1 on error (with errno set).
 *    Sets [ndropped] (if not NULL) to the number of [dst] bytes overwritten.
 */

int scbuf_move (scbuf_t src, scbuf_t dst, int len, int *ndropped, int secure_malloc_flag);
/*
 *  Moves up to [len] bytes of data from the [src] scbuf into the [dst] scbuf
 *    according to dst's SCBUF_OPT_OVERWRITE behavior.  If [len] is -1,
 *    it will be set to the number of [src] bytes available for reading.
 *  Returns the number of bytes moved, or -1 on error (with errno set).
 *    Sets [ndropped] (if not NULL) to the number of [dst] bytes overwritten.
 */


#endif /* !LSD_SCBUF_H */
