/*****************************************************************************\
 *  $Id: ipmiseld.h,v 1.11 2010-02-08 22:02:30 chu11 Exp $
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
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif /* STDC_HEADERS */
#include <syslog.h>
#include <assert.h>
#include <errno.h>

#include <freeipmi/freeipmi.h>

#include "ipmiseld.h"
#include "ipmiseld-common.h"

#include "freeipmi-portability.h"
#include "error.h"

#define IPMISELD_ERR_BUFLEN 1024

int
ipmiseld_event_state_filter_parse (const char *str)
{
  char *strtmp;
  char *tok;
  int rv = -1;
  int filter_mask = 0;

  assert (str);

  if (!(strtmp = strdup (str)))
    err_exit ("strdup: %s", strerror (errno));

  tok = strtok (strtmp, " ,");
  while (tok)
    {
      if (!strcasecmp (tok, "NOMINAL"))
	filter_mask |= IPMISELD_NOMINAL_FILTER;
      else if (!strcasecmp (tok, "WARNING"))
	filter_mask |= IPMISELD_WARNING_FILTER;
      else if (!strcasecmp (tok, "CRITICAL"))
	filter_mask |= IPMISELD_CRITICAL_FILTER;
      else if (!strcasecmp (tok, "NA"))
	filter_mask |= IPMISELD_NA_FILTER;
      else if (!strcasecmp (tok, "none"))
	{
	  filter_mask = 0;
	  break;
	}
      else
	{
	  filter_mask = -1;
	  break;
	}
      tok = strtok (NULL, " ,");
    }

  rv = filter_mask;

  free (strtmp);
  return (rv);
}

int
ipmiseld_log_facility_parse (const char *str)
{
  assert (str);

  if (!strcasecmp (str, "LOG_DAEMON"))
    return (LOG_DAEMON);
  else if (!strcasecmp (str, "LOG_USER"))
    return (LOG_USER);
  else if (!strcasecmp (str, "LOG_LOCAL0"))
    return (LOG_LOCAL0);
  else if (!strcasecmp (str, "LOG_LOCAL1"))
    return (LOG_LOCAL1);
  else if (!strcasecmp (str, "LOG_LOCAL2"))
    return (LOG_LOCAL2);
  else if (!strcasecmp (str, "LOG_LOCAL3"))
    return (LOG_LOCAL3);
  else if (!strcasecmp (str, "LOG_LOCAL4"))
    return (LOG_LOCAL4);
  else if (!strcasecmp (str, "LOG_LOCAL5"))
    return (LOG_LOCAL5);
  else if (!strcasecmp (str, "LOG_LOCAL6"))
    return (LOG_LOCAL6);
  else if (!strcasecmp (str, "LOG_LOCAL7"))
    return (LOG_LOCAL7);
  return (-1);
}

int
ipmiseld_log_priority_parse (const char *str)
{
  assert (str);

  if (!strcasecmp (str, "LOG_EMERG"))
    return (LOG_EMERG);
  else if (!strcasecmp (str, "LOG_ALERT"))
    return (LOG_ALERT);
  else if (!strcasecmp (str, "LOG_CRIT"))
    return (LOG_CRIT);
  else if (!strcasecmp (str, "LOG_ERR"))
    return (LOG_ERR);
  else if (!strcasecmp (str, "LOG_WARNING"))
    return (LOG_WARNING);
  else if (!strcasecmp (str, "LOG_NOTICE"))
    return (LOG_NOTICE);
  else if (!strcasecmp (str, "LOG_INFO"))
    return (LOG_INFO);
  else if (!strcasecmp (str, "LOG_DEBUG"))
    return (LOG_DEBUG);
  return (-1);
}

 
static void
_ipmiseld_syslog (ipmiseld_host_data_t *host_data,
		  const char *message,
		  va_list ap)
{
  char buf[IPMISELD_ERR_BUFLEN + 1];
  
  memset (buf, '\0', IPMISELD_ERR_BUFLEN + 1);
  vsnprintf(buf, IPMISELD_ERR_BUFLEN, message, ap);
  
  if (host_data->prog_data->args->test_run
      || host_data->prog_data->args->foreground)
    printf ("%s\n", buf);
  else
    syslog (host_data->prog_data->log_priority, "%s", buf);
}

void
ipmiseld_syslog (ipmiseld_host_data_t *host_data,
		 const char *message,
		 ...)
{
  va_list ap;

  assert (host_data);
  assert (message);

  va_start (ap, message);
  _ipmiseld_syslog (host_data, message, ap);
  va_end (ap);
}

void
ipmiseld_syslog_host (ipmiseld_host_data_t *host_data,
		      const char *message,
		      ...)
{
  va_list ap;

  assert (host_data);
  assert (message);

  va_start (ap, message);
  if (!host_data->hostname)
    _ipmiseld_syslog (host_data, message, ap);
  else
    {
      char buf[IPMISELD_ERR_BUFLEN + 1];
      memset (buf, '\0', IPMISELD_ERR_BUFLEN + 1);
      vsnprintf(buf, IPMISELD_ERR_BUFLEN, message, ap);
      
      if (host_data->prog_data->args->test_run
	  || host_data->prog_data->args->foreground)
	printf ("%s: %s\n", host_data->hostname, buf);
      else
	syslog (host_data->prog_data->log_priority,
		"%s: %s",
		host_data->hostname,
		buf);
    }
  va_end (ap);
}

void
ipmiseld_err_output (ipmiseld_host_data_t *host_data,
		     const char *message,
		     ...)
{
  char buf[IPMISELD_ERR_BUFLEN + 1];
  va_list ap;

  assert (host_data);
  assert (message);
  memset (buf, '\0', IPMISELD_ERR_BUFLEN + 1);

  va_start (ap, message);
  vsnprintf(buf, IPMISELD_ERR_BUFLEN, message, ap);

  if (!host_data->hostname)
    err_output ("%s", buf);
  else
    err_output ("%s: %s", host_data->hostname, buf);
  va_end (ap);
}
