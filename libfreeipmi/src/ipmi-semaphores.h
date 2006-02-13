/* 
   ipmi-semaphores.h: Synchronization and Locking functionality.

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

/*
  A semaphore is basically a counter indicating the number of
  resources available. If this number is 1 (the most common case), the
  semaphore is often called a `mutex'. You can grab the mutex and
  release it. --Anand Babu 
*/

#ifndef _IPMI_SEMAPHORES_H
#define	_IPMI_SEMAPHORES_H	1

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_IPCKEY  "/var/lib/" PACKAGE_NAME "/ipckey"

#define IPMI_OUTOFBAND_PROJ_ID    0x01
#define IPMI_INBAND_PROJ_ID       0x02

#define IPMI_OUTOFBAND_IPCKEY()  ftok (IPMI_IPCKEY, IPMI_OUTOFBAND_PROJ_ID)
#define IPMI_INBAND_IPCKEY()  ftok (IPMI_IPCKEY, IPMI_INBAND_PROJ_ID)

#define IPMI_MUTEX_LOCK(semid)                                      \
do {								    \
  int status;							    \
  status = semop (semid, &mutex_lock_buf, 1);			    \
  if (status == -1)						    \
    {								    \
      if (errno == EINTR)					    \
	continue; /* While blocked in this system call, the process \
		     caught a signal */				    \
      ERR (status != -1);					    \
    }								    \
  break;                                                            \
} while (1)

#define IPMI_MUTEX_DOWN(semid)               IPMI_MUTEX_LOCK (semid)

#define IPMI_MUTEX_LOCK_INTERRUPTIBLE(semid)  \
    semop (semid, &mutex_lock_buf_interruptible, 1)
#define IPMI_MUTEX_LOCK_ASYNC(semid)         IPMI_MUTEX_LOCK_INTERRUPTIBLE (semid)
#define IPMI_MUTEX_DOWN_INTERRUPTIBLE(semid) IPMI_MUTEX_LOCK_INTERRUPTIBLE (semid)
#define IPMI_MUTEX_DOWN_ASYNC(semid)         IPMI_MUTEX_LOCK_INTERRUPTIBLE (semid)

#define IPMI_MUTEX_UNLOCK(semid)                                    \
do {								    \
  ERR (semop (semid, &mutex_unlock_buf, 1) != -1);                  \
  /* If you are in a loop to go grab LOCK again, Other tasks will   \
     never get a chance until you break. Give fair chance to other  \
     tasks as well. This is probably because of scheduler	    \
     optimizations in Linux kernel.  --Anand Babu 		    \
  */								    \
  usleep (1);                                                       \
} while (0)

#define IPMI_MUTEX_UP(semid)                 IPMI_MUTEX_UNLOCK (semid)

#if defined(__FreeBSD__)
  /* union semun is defined by including <sys/sem.h> */
#else
  /* according to X/OPEN we have to define it ourselves */
  union semun {
    int val;                        /* value for SETVAL */
    struct semid_ds *buf;           /* buffer for IPC_STAT & IPC_SET */
    unsigned short *array;          /* array for GETALL & SETALL */
    struct seminfo *__buf;          /* buffer for IPC_INFO */
    void *__pad;
  };
#endif

extern struct sembuf mutex_lock_buf_interruptible;
extern struct sembuf mutex_unlock_buf_interruptible;
extern struct sembuf mutex_lock_buf;
extern struct sembuf mutex_unlock_buf;

int ipmi_mutex_init (key_t key);


#ifdef __cplusplus
}
#endif

#endif /* ipmi-semaphores.h */
