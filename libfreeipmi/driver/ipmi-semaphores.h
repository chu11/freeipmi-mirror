/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

/*
  A semaphore is basically a counter indicating the number of
  resources available. If this number is 1 (the most common case), the
  semaphore is often called a `mutex'. You can grab the mutex and
  release it. --Anand Babu
*/

#ifndef IPMI_SEMAPHORES_H
#define IPMI_SEMAPHORES_H

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>

#if defined(__FreeBSD__) || defined(__APPLE__)
/* union semun is defined by including <sys/sem.h> */
#else
/* according to X/OPEN we have to define it ourselves */
union semun {
  int val;                          /* value for SETVAL */
  struct semid_ds *buf;             /* buffer for IPC_STAT & IPC_SET */
  unsigned short *array;            /* array for GETALL & SETALL */
  struct seminfo *__buf;            /* buffer for IPC_INFO */
  void *__pad;
};
#endif

int driver_mutex_init (void);

int driver_mutex_lock (int semid);

int driver_mutex_lock_interruptible (int semid);

int driver_mutex_unlock (int semid);

#endif /* IPMI_SEMAPHORES_H */
