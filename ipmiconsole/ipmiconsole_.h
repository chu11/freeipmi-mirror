/*****************************************************************************\
 *  $Id: ipmiconsole_.h,v 1.7 2010-02-08 22:02:30 chu11 Exp $
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

/* file is named ipmiconsole_.h to differentiate itself from the
 * library ipmiconsole.h.
 *
 * I am scared of the portability of the #include_next directive, so
 * that's why I'm doing it this way.
 */

/* file is "ipmiconsole_.h", so double underscore */
#ifndef IPMICONSOLE__H
#define IPMICONSOLE__H

#include <freeipmi/freeipmi.h>

#include "tool-cmdline-common.h"

enum ipmiconsole_argp_option_keys
  {
    DONT_STEAL_KEY = 160,
    DEACTIVATE_KEY = 161,
    SERIAL_KEEPALIVE_KEY = 162,
    SERIAL_KEEPALIVE_EMPTY_KEY = 163,
    SOL_PAYLOAD_INSTANCE_KEY = 164,
    DEACTIVATE_ALL_INSTANCES_KEY = 165,
    LOCK_MEMORY_KEY = 166,
    ESCAPE_CHAR_KEY = 'e',
    DEBUG_KEY = 167,
    DEBUGFILE_KEY = 168,
    NORAW_KEY = 169,
  };

struct ipmiconsole_arguments
{
  struct common_cmd_args common_args;
  char escape_char;
  int dont_steal;
  int deactivate;
  int serial_keepalive;
  int serial_keepalive_empty;
  unsigned int sol_payload_instance;
  int deactivate_all_instances;
  int lock_memory;
#ifndef NDEBUG
  int debugfile;
  int noraw;
#endif /* NDEBUG */
};

#endif /* IPMICONSOLE__H */
