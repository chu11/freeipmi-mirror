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

#ifndef TOOL_DAEMON_COMMON_H
#define TOOL_DAEMON_COMMON_H

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdint.h>
#include <signal.h>

#ifndef HAVE_SIGHANDLER_T
typedef void (*sighandler_t)(int);
#endif /* HAVE_SIGHANDLER_T */

int daemonize_common (const char *pidfile);

/* can pass NULL for no callback */
int daemon_signal_handler_setup (sighandler_t cb);

/* signal handlers + sleep(3) is a bad idea */
int daemon_sleep (unsigned int sleep_len);

#endif /* TOOL_DAEMON_COMMON_H */
