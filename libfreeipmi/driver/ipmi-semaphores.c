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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <errno.h>

#include "ipmi-semaphores.h"

#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

struct sembuf mutex_lock_buf_interruptible   = { 0, -1, IPC_NOWAIT|SEM_UNDO};
struct sembuf mutex_unlock_buf_interruptible = { 0,  1, IPC_NOWAIT|SEM_UNDO};

struct sembuf mutex_lock_buf   = { 0, -1, SEM_UNDO};
struct sembuf mutex_unlock_buf = { 0,  1, SEM_UNDO};

#define IPMI_INBAND_PROJ_ID       0x02
#define IPMI_INBAND_DEBUG_PROJ_ID 0x03

int
driver_mutex_init (void)
{
  int semid = -1;
  key_t key;

  /* Allocate Mutex */
#ifndef NDEBUG
  if ((key = ftok (IPMI_IPCKEY, IPMI_INBAND_PROJ_ID)) == ((key_t)-1))
    /* When doing development out of the source code tree, the
     * IPCKEY file may not yet exist, so we do a hack to get it to
     * work.  This is only when we work in debug mode.
     */
    key = ftok (IPMI_DEBUG_IPCKEY, IPMI_INBAND_DEBUG_PROJ_ID);
#else /* !NDEBUG */
  if ((key = ftok (IPMI_IPCKEY, IPMI_INBAND_PROJ_ID)) == ((key_t)-1))
    {
      ERRNO_TRACE (errno);
      return (-1);
    }
#endif /* NDEBUG */

  if ((semid = semget (key, 1, IPC_CREAT | IPC_EXCL | 0600)) < 0)
    {
      if (errno == EEXIST) /* You are not the first one */
        {
          /* Get the orignial semid */
          if ((semid = semget (key, 1, IPC_CREAT | 0600)) < 0)
            {
              ERRNO_TRACE (errno);
              return (-1);
            }

          /* achu: errno may not get reset, so put it back to 0 */
          errno = 0;

          return (semid);
        }

      ERRNO_TRACE (errno);
      return (-1);
    }

  /* You are the first one. Initialize the mutex now */
  {
    union semun mutex_init;
    unsigned short values[1];
    values[0] = 1;
    mutex_init.array = values;
    if (semctl (semid, 0, SETALL, mutex_init) < 0)
      {
        ERRNO_TRACE (errno);
        return (-1);
      }
  }
  return (semid);
}

int
driver_mutex_lock (int semid)
{
  /* achu: don't check for valid semid - responsibility of calling libs */
  do {
    if (semop (semid, &mutex_lock_buf, 1) < 0)
      {
	/* While blocked in this system call, the process caught a signal */
	if (errno == EINTR)
	  continue;
	ERRNO_TRACE (errno);
	return (-1);
      }
    break;
  } while (1);

  return (0);
}

int
driver_mutex_lock_interruptible (int semid)
{
  /* achu: don't check for valid semid - responsibility of calling libs */
  return (semop (semid, &mutex_lock_buf_interruptible, 1));
}

int
driver_mutex_unlock (int semid)
{
  /* achu: don't check for valid semid - responsibility of calling libs */

  if (semop (semid, &mutex_unlock_buf, 1) < 0)
    {
      ERRNO_TRACE (errno);
      return (-1);
    }

  /* If you are in a loop to go grab LOCK again, Other tasks will
     never get a chance until you break. Give fair chance to other
     tasks as well. This is probably because of scheduler
     optimizations in Linux kernel.  --Anand Babu
  */
  usleep (1);
  return (0);
}
