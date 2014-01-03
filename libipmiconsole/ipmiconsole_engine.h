/*****************************************************************************\
 *  $Id: ipmiconsole_engine.h,v 1.20 2010-02-08 22:02:30 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2006-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-221226
 *
 *  This file is part of Ipmiconsole, a set of IPMI 2.0 SOL libraries
 *  and utilities.  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmiconsole is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmiconsole is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmiconsole.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifndef IPMICONSOLE_ENGINE_H
#define IPMICONSOLE_ENGINE_H

#include "ipmiconsole.h"

int ipmiconsole_engine_setup (unsigned int thread_count);

int ipmiconsole_engine_is_setup (void);

int ipmiconsole_engine_thread_count (void);

int ipmiconsole_engine_thread_create (void);

int ipmiconsole_engine_submit_ctx (ipmiconsole_ctx_t c);

int ipmiconsole_engine_cleanup (int cleanup_sol_sessions);

#endif /* IPMICONSOLE_ENGINE_H */
