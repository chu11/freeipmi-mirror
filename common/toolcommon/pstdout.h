/*****************************************************************************\
 *  $Id: pstdout.h,v 1.7 2010-02-10 01:27:44 chu11 Exp $
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

#ifndef PSTDOUT_H
#define PSTDOUT_H

/* 
 * Pstdout is a library/tool to launch and manage multiple threads,
 * each dealing with a different host.  It will also manage the
 * "parallel" standard output from the launched threads.
 *
 * The idea for pstdout came from the pdsh and dshbak.  (See
 * http://sourceforge.net/projects/pdsh/ or
 * http://www.llnl.gov/linux/pdsh/).  The tool (or code in pdsh) could
 * not be directly used for all projects, thus this library was
 * developed to emulate its functionality. Some of the same basic
 * code/algorithms from pdsh have been re-used here.
 *
 * The general code structure using this library is as follows:
 *
 * call pstdout_init()
 * call pstdout_set_output_flags() if non-defaults needed
 * call pstdout_set_fanout() if non-defaults needed
 * call pstdout_lauch() to launch parallel threads
 * - within callback functions replace printf/fprintf/perror calls
 *   with pstdout equivalent calls.
 */

#include <stdio.h>
#include <stdarg.h>

/* 
 * Error Codes
 */
#define PSTDOUT_ERR_SUCCESS               0
#define PSTDOUT_ERR_UNINITIALIZED         1
#define PSTDOUT_ERR_PARAMETERS            2
#define PSTDOUT_ERR_OUTMEM                3
#define PSTDOUT_ERR_INTERNAL              4
#define PSTDOUT_ERR_ERRNUMRANGE           5

/* 
 * Debug Flags
 *
 * PSTDOUT_DEBUG_NONE - No debug output
 *
 * PSTDOUT_DEBUG_STANDARD - Output occasional debug info to stderr.
 */
#define PSTDOUT_DEBUG_NONE                0x00000001
#define PSTDOUT_DEBUG_STANDARD            0x00000002
#define PSTDOUT_DEBUG_MASK                0x00000003

/* 
 * Output Flags
 *
 * PSTDOUT_OUTPUT_STDOUT_DEFAULT/PSTDOUT_OUTPUT_STDERR_DEFAULT -
 * Output stdout/stderr like normal
 *
 * PSTDOUT_OUTPUT_STDOUT_PREPEND_HOSTNAME/PSTDOUT_OUTPUT_STDERR_PREPEND_HOSTNAME
 * - Prepend the hostname to each line of output.  For example,
 * "host1: foo output".  Conflicts with PSTDOUT_OUTPUT_STDOUT/STDERR_DEFAULT.
 *
 * One of PSTDOUT_OUTPUT_STDOUT_DEFAULT and
 * PSTDOUT_OUTPUT_STDOUT_PREPEND_HOSTNAME must always be set.  One of
 * PSTDOUT_OUTPUT_STDERR_DEFAULT and
 * PSTDOUT_OUTPUT_STDERR_PREPEND_HOSTNAME must always be set.
 *
 * PSTDOUT_OUTPUT_BUFFER_STDOUT/PSTDOUT_OUTPUT_BUFFER_STDERR - Do not
 * output stdout/stderr as each line is output.  Buffer the output and
 * output it all at once right before the thread terminates.  Can work
 * in conjunction with PSTDOUT_OUTPUT_STDOUT/STDERR_PREPEND_HOSTNAME.
 *
 * PSTDOUT_OUTPUT_STDOUT_CONSOLIDATE/PSTDOUT_OUTPUT_STDERR_CONSOLIDATE
 * - Do not output stdout/stderr until all threads have been executed.
 * Consolidate output so that matching output from different nodes are
 * not output twice.  Conflicts with
 * PSTDOUT_OUTPUT_STDOUT/STDERR_PREPEND_HOSTNAME and
 * PSTDOUT_OUTPUT_BUFFER_STDOUT/STDERR.
 * 
 */
#define PSTDOUT_OUTPUT_STDOUT_DEFAULT          0x00000001
#define PSTDOUT_OUTPUT_STDERR_DEFAULT          0x00000002
#define PSTDOUT_OUTPUT_STDOUT_PREPEND_HOSTNAME 0x00000004
#define PSTDOUT_OUTPUT_STDERR_PREPEND_HOSTNAME 0x00000008
#define PSTDOUT_OUTPUT_BUFFER_STDOUT           0x00000010
#define PSTDOUT_OUTPUT_BUFFER_STDERR           0x00000020
#define PSTDOUT_OUTPUT_STDOUT_CONSOLIDATE      0x00000040
#define PSTDOUT_OUTPUT_STDERR_CONSOLIDATE      0x00000080
#define PSTDOUT_OUTPUT_MASK                    0x000000FF

/* 
 * Fanout default, min, and max
 */
#define PSTDOUT_FANOUT_DEFAULT    64
#define PSTDOUT_FANOUT_MIN        1
#define PSTDOUT_FANOUT_MAX        1024

/* pstdout_errnum
 *
 * Will be set to the most recently set pstdout error
 */
extern int pstdout_errnum;

/* pstdout_state_t
 *
 * Maintains parallel stdout information.  Will be passed to each
 * thread that is launched via 'pstdout_launch' and must be passed to
 * each call of 'pstdout_printf', 'pstdout_fprintf' or
 * 'pstdout_perror'.
 */
typedef struct pstdout_state *pstdout_state_t;

/* Pstdout_Thread 
 *
 * Function prototype of the function that pstdout will launch.
 * Passed to 'pstdout_launch'.
 *
 * Returns the exit code for this thread.
 */
typedef int (*Pstdout_Thread)(pstdout_state_t pstate, const char *hostname, void *arg);

/* pstdout_init
 *
 * Must be called before most pstdout API functions can be called.
 */
int pstdout_init(void);

/* pstdout_strerror
 * 
 * Returns statically defined string describing the error code.
 */
char *pstdout_strerror(int errnum);

/* pstdout_set_debug_flags
 *
 * Set the current debug flags.
 *
 * Returns 0 on success, -1 on error
 */
int pstdout_set_debug_flags(unsigned int flags);

/* pstdout_get_debug_flags
 *
 * Returns current debug flags.
 */
int pstdout_get_debug_flags(void);

/* pstdout_set_output_flags
 *
 * Set the current output flags.
 *
 * Returns 0 on success, -1 on error
 */
int pstdout_set_output_flags(unsigned int flags);

/* pstdout_get_output_flags
 *
 * Returns current output flags.
 */
int pstdout_get_output_flags(void);

/* pstdout_set_fanout
 *
 * Set the current fanout.  The fanout is the largest number of
 * threads that can be launched simultaneously by 'pstdout_launch'.
 *
 * Returns 0 on success, -1 on error
 */
int pstdout_set_fanout(unsigned int fanout);

/* pstdout_get_fanout
 *
 * Returns current fanout.
 */
int pstdout_get_fanout(void);

/* pstdout_hostnames_count
 *
 * Count the number of hosts specified by hostnames.  Primarily a
 * utility function to allow client writers to determine what output
 * flags they wish to set.
 *
 * Returns number of hostnames on success, -1 on error.
 */
int pstdout_hostnames_count(const char *hostnames);

/* pstdout_printf
 *
 * Parallel standard output.  Should only be called by a thread
 * executed by 'pstdout_launch'.
 *
 * Returns number of characters printed, -1 on error.
 *
 * Note that the return value of number of characters printed may be
 * 0, because data is being buffered for output on a later
 * pstdout_printf call.
 */
int pstdout_printf(pstdout_state_t pstate, const char *format, ...);

/* pstdout_vprintf
 *
 * Parallel standard output.  Should only be called by a thread
 * executed by 'pstdout_launch'.
 *
 * Returns number of characters printed, -1 on error.
 *
 * Note that the return value of number of characters printed may be
 * 0, because data is being buffered for output on a later
 * pstdout_printf call.
 */
int pstdout_vprintf(pstdout_state_t pstate, const char *format, va_list ap);

/* pstdout_fprintf
 *
 * Parallel file stream output.  Should only be called by a thread
 * executed by 'pstdout_launch'.  Currently will only work with stdout
 * and stderr.
 *
 * Returns number of characters printed, -1 on error.
 *
 * Note that the return value of number of characters printed may be
 * 0, because data is being buffered for output on a later
 * pstdout_fprintf call.
 */
int pstdout_fprintf(pstdout_state_t pstate, FILE *stream, const char *format, ...);

/* pstdout_vfprintf
 *
 * Parallel file stream output.  Should only be called by a thread
 * executed by 'pstdout_launch'.  Currently will only work with stdout
 * and stderr.
 *
 * Returns number of characters printed, -1 on error.
 *
 * Note that the return value of number of characters printed may be
 * 0, because data is being buffered for output on a later
 * pstdout_fprintf call.
 */
int pstdout_vfprintf(pstdout_state_t pstate, FILE *stream, const char *format,
		     va_list ap);

/* pstdout_perror
 *
 * Parallel perror.  Should only be called by a thread executed by
 * 'pstdout_launch'.
 */
void pstdout_perror(pstdout_state_t pstate, const char *s);

/* pstdout_launch
 *
 * Primary thread launching function of the library.  It will launch
 * no more than 'fanout' threads at the same time, launching new
 * threads after old ones have completed.  Will handle all standard output
 * buffering or consolidation that is required.
 *
 * Returns: Largest exit code returned from all threads launched.
 */
int pstdout_launch(const char *hostnames, Pstdout_Thread pstdout_func, void *arg);

/* PSTDOUT_PRINTF
 *
 * Identical to 'pstdout_printf', but will call standard printf() if an invalid
 * pstate is passed in (e.g. a NULL pstate) or the library is not initialized.
 */
int PSTDOUT_PRINTF(pstdout_state_t pstate, const char *format, ...);

/* PSTDOUT_FPRINTF
 *
 * Identical to 'pstdout_fprintf', but will call standard fprintf() if
 * an invalid pstate or stream is passed in (e.g. a NULL pstate) or
 * the library is not initialized.
 */
int PSTDOUT_FPRINTF(pstdout_state_t pstate, FILE *stream, const char *format, ...);

/* PSTDOUT_PERROR
 *
 * Identical to 'pstdout_perror', but will call standard perror() if
 * an invalid pstate is passed in (e.g. a NULL pstate) or the library
 * is not initialized.
 */
void PSTDOUT_PERROR(pstdout_state_t pstate, const char *s);

#endif /* PSTDOUT_H */
