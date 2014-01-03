/*****************************************************************************
 *  $Id: list.h,v 1.2 2008-08-12 18:14:34 chu11 Exp $
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


#ifndef LSD_HEAP_H
#define LSD_HEAP_H


/***********
 *  Notes  *
 ***********/

/* API emulates Chris Dunlap's (dunlap6 at llnl dot gov) List library.
 * Lots of code was copied.
 */
/*
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


/****************
 *  Data Types  *
 ****************/

typedef struct heap * Heap;
/*
 *  Heap opaque data type.
 */

typedef void (*HeapDelF) (void *x);
/*
 *  Function prototype to deallocate data stored in a heap.
 *    This function is responsible for freeing all memory associated
 *    with an item, including all subordinate items (if applicable).
 */

typedef int (*HeapCmpF) (void *x, void *y);
/*
 *  Function prototype for comparing two items in a heap.
 *  Returns less-than-zero if (x<y), zero if (x==y), and
 *    greather-than-zero if (x>y).
 */

/*******************************
 *  General-Purpose Functions  *
 *******************************/

Heap heap_create (int size, HeapCmpF fCmp, HeapDelF fDel);
/*
 *  Creates and returns a new empty heap, or lsd_nomem_error() on failure.
 *  The [size] is guidance on the number of slots in the heap; If set
 *    <= 0, the default size is used.  Usually, the next power of 2
 *    will be used for size, but with a minimum used internally.
 *  The comparison function [fCmp] is used for comparison of items
 *    added to the heap.  This heap operates as a max heap when utilizing
 *    the HeapCmpF.  To implement a min heap, adjust the HeapCmpF function
 *    appropriately.
 *  The deletion function [fDel] is used to deallocate memory used by items
 *    in the heap; if this is NULL, memory associated with these items
 *    will not be freed when the heap is destroyed.
 *  Note: Abandoning a heap without calling heap_destroy() will result
 *    in a memory leak.
 */

void heap_destroy (Heap h);
/*
 *  Destroys heap [h], freeing memory used for heap iterators and the
 *    heap itself; if a deletion function was specified when the heap
 *    was created, it will be called for each item in the heap.
 */

int heap_is_empty (Heap h);
/*
 *  Returns non-zero if heap [h] is empty; o/w returns zero.
 */

int heap_is_full (Heap h);
/*
 *  Returns non-zero if heap [h] is full; o/w returns zero.
 */

/****************************
 *  Access Functions        *
 ****************************/

void * heap_insert (Heap h, void *x);
/*
 *  Pushes data [x] onto the top of heap [h].
 *  Returns the data's ptr, or lsd_nomem_error() if insertion failed.
 */

void * heap_pop (Heap h);
/*
 *  Pops the data item at the top of the heap [h].
 *  Returns the data's ptr, or NULL if the heap is empty.
 */

void * heap_peek (Heap h);
/*
 *  Peeks at the data item at the top of the heap [h].
 *  Returns the data's ptr, or NULL if the heap is empty.
 *  Note: The item is not removed from the heap.
 */

#endif /* !LSD_HEAP_H */
