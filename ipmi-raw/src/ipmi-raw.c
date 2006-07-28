/*
ipmi-raw.c: executes IPMI commands by hex values.
Copyright (C) 2005 FreeIPMI Core Team

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif	/* HAVE_UNISTD_H */
#include <error.h>
#include <sys/resource.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <argp.h>

#include <freeipmi/freeipmi.h>
#include <freeipmi/udm/udm.h>

#include "argp-common.h"
#include "ipmi-raw-argp.h"
#include "ipmi-common.h"

static int 
string2bytes (char *line, unsigned char **buf, int *len)
{
  const char delim[] = " \t\f\v\r\n";
  char *str = NULL;
  char *token = NULL;
  int count = 0;
  int i = 0;
  int l = 0;
  int value = 0;
  
  if (line == NULL || buf == NULL || len == NULL)
    return (-1);
  
  for (i = 0, count = 0; line[i]; i++)
    {
      if (strchr ((const char*)delim, (int) line[i]))
	count++;
    }
  count++;
  
  *buf = calloc ((strlen (line) - count), 1);
  str = (char *) strdupa (line);
  count = 0;
  while (1)
    {
      token = strsep (&str, delim);
      if (token == NULL)
	break;
      if (strcmp (token, "") == 0)
	continue;
      
      l = strlen (token);
      if (l > 2)
	{
	  fprintf (stderr, "invalid input\n");
	  free (*buf);
	  *buf = NULL;
	  *len = 0;
	  return (-1);
	}
      for (i = 0; i < l; i++)
	{
	  if (isxdigit (token[i]) == 0)
	    {
	      fprintf (stderr, "invalid input\n");
	      free (*buf);
	      *buf = NULL;
	      *len = 0;
	      return (-1);
	    }
	}
      
      value = strtol (token, (char **) NULL, 16);
      (*buf)[count++] = (unsigned char) value;
    }
  
  *len = count;
  
  return (0);
}

void
_disable_coredump(void)
{
  /* Disable core dumping when not-debugging.  Do not want username,
   * password or other important stuff to core dump.
   */
#ifdef NDEBUG
  struct rlimit resource_limit;

  if (!getrlimit(RLIMIT_CORE, &resource_limit))
    {
      resource_limit.rlim_cur = 0;
      if (setrlimit (RLIMIT_CORE, &resource_limit) != 0)
        perror ("warning: setrlimit()");
    }
#endif /* NDEBUG */
}

int 
main (int argc, char **argv)
{
  struct arguments *args = NULL;
  ipmi_device_t dev = NULL;
  
  FILE *infile = NULL;
  
  int i;
  
  char *line = NULL;
  unsigned int line_count = 0;
  size_t n = 0;
  
  uint8_t *bytes_rq = NULL;
  int send_len;
  
  uint8_t bytes_rs[ARG_MAX];
  int32_t rs_len;

  uint32_t flags;

  _disable_coredump();
  
  ipmi_raw_argp_parse (argc, argv);
  args = ipmi_raw_get_arguments ();
  
#ifdef NDEBUG
  /* Clear out argv data for security purposes on ps(1). */
  for (i = 1; i < argc; i++)
    memset(argv[i], '\0', strlen(argv[i]));
#endif /* NDEBUG */

#ifndef NDEBUG
  if (args->common.debug)
    flags = IPMI_FLAGS_DEBUG_DUMP;
  else
    flags = IPMI_FLAGS_DEFAULT;
#else  /* NDEBUG */
  flags = IPMI_FLAGS_DEFAULT;
#endif /* NDEBUG */

  if (args->common.host != NULL)
    {
      if (!(dev = ipmi_open_outofband (IPMI_DEVICE_LAN, 
				       args->common.host,
                                       args->common.username, 
                                       args->common.password, 
                                       args->common.authentication_type, 
                                       args->common.privilege_level,
                                       args->common.session_timeout, 
                                       args->common.retry_timeout, 
                                       flags))) 
	{
	  perror ("ipmi_open_outofband()");
	  exit (EXIT_FAILURE);
	}
    }
  else 
    {
      if (!ipmi_is_root())
        {
          fprintf(stderr, "%s: Permission Denied\n", argv[0]);
          exit(EXIT_FAILURE);
        }

      if (args->common.driver_type == IPMI_DEVICE_UNKNOWN)
	{
	  if (!(dev = ipmi_open_inband (IPMI_DEVICE_KCS, 
					args->common.disable_auto_probe, 
					args->common.driver_address, 
                                        args->common.register_spacing,
                                        args->common.driver_device, 
                                        flags)))
	    {
	      if (!(dev = ipmi_open_inband (IPMI_DEVICE_SSIF, 
					    args->common.disable_auto_probe, 
					    args->common.driver_address, 
                                            args->common.register_spacing,
                                            args->common.driver_device, 
                                            flags)))
		{
		  perror ("ipmi_open_inband()");
		  return (-1);
                }
	    }
	}
      else 
	{
	  if (!(dev = ipmi_open_inband (args->common.driver_type, 
					args->common.disable_auto_probe, 
					args->common.driver_address, 
                                        args->common.register_spacing,
                                        args->common.driver_device, 
                                        flags)))
            {
	      perror ("ipmi_open_inband()");
	      return (-1);
	    }
	}
    }
  
  if (args->cmd_length)
    {
      bytes_rq = args->cmd;
      send_len = args->cmd_length;

      if (send_len <= 2)
        {
          fprintf(stderr, "Invalid number of hex bytes\n");
          return (-1);
        }

      if ((rs_len = ipmi_cmd_raw (dev, 
				  bytes_rq[0],
				  bytes_rq[1],
				  &bytes_rq[2],
				  send_len - 2, 
				  bytes_rs, 
				  ARG_MAX)) >= 0)
	{
	  printf ("rcvd: ");
	  for (i = 0; i < rs_len; i++)
	    printf ("%02X ", bytes_rs[i]);
	  printf ("\n");
	}
      else 
        {
          perror ("ipmi_cmd_raw()");
        }
      bytes_rq = NULL;
      send_len = 0;
    }
  
  if (args->cmd_file)
    {
      infile = fopen (args->cmd_file, "r");
      if (infile == NULL)
	{
          perror ("fopen()");
	  ipmi_close_device (dev);
	  exit (EXIT_FAILURE);
	}
    }
  else 
    {
      if (args->cmd_length == 0)
	infile = stdin;
    }
  
  while (infile)
    {
      if (line != NULL)
	{
	  free (line);
	  line = NULL;
	}
      n = 0;
      if (bytes_rq != NULL)
	{
	  free (bytes_rq);
	  bytes_rq = NULL;
	}
      send_len = 0;
      
      if (getline (&line, &n, infile) == -1)
	{
	  /* perror ("getline()"); */
	  break;
	}
      line_count++;
      
      if (string2bytes (line, &bytes_rq, &send_len) != 0)
	break;
      
      if (send_len <= 2)
        {
          fprintf(stderr, "Invalid number of hex bytes on line %d\n", line_count);
          continue;
        }

      if ((rs_len = ipmi_cmd_raw (dev, 
				  bytes_rq[0], 
				  bytes_rq[1], 
				  &bytes_rq[2], 
				  send_len - 2, 
				  bytes_rs, 
				  ARG_MAX)) < 0)
        {
          perror ("ipmi_cmd_raw()");
          continue;
        }
      
      printf ("rcvd: ");
      for (i = 0; i < rs_len; i++)
	printf ("%02X ", bytes_rs[i]);
      printf ("\n");
    }
  
  if ((infile != NULL) && (infile != stdin))
    {
      fclose (infile);
    }
  
  ipmi_close_device (dev);
  
  return (0);
}
