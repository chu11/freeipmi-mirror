/* 
   ipmi-semaphores.c: Synchronization and Locking functionality.

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "ipmi-semaphores.h"

#include "err-wrappers.h"
#include "freeipmi-portability.h"

struct sembuf mutex_lock_buf_interruptible   = {0, -1, IPC_NOWAIT|SEM_UNDO};
struct sembuf mutex_unlock_buf_interruptible = {0,  1, IPC_NOWAIT|SEM_UNDO};

struct sembuf mutex_lock_buf   = {0, -1, SEM_UNDO};
struct sembuf mutex_unlock_buf = {0,  1, SEM_UNDO};

/* Initialize Mutex and return semid */
int
ipmi_mutex_init (key_t key)
{
  int semid;

  ERR (key != -1);

  /* Allocate Mutex */
  semid = semget (key, 1, IPC_CREAT | IPC_EXCL | 0600);
  if (semid == -1)
    {
      if (errno == EEXIST) /* You are not the first one */
	{ 
	  /* Get the orignial semid */
	  semid = semget (key, 1, IPC_CREAT | 0600);  
	  ERR (semid != -1);
	  return (semid);
	}
      ERR (0); /* FAIL */
    }

  /* You are the first one. Initialize the mutex now */
  {
    union semun mutex_init;  
    unsigned short values[1];
    values[0] = 1;
    mutex_init.array = values;
    ERR (semctl (semid, 0, SETALL, mutex_init) != -1);
  }
  return (semid);
}
