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

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <error.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <argp.h>
#include "freeipmi.h"

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

int 
main (int argc, char **argv)
{
  struct arguments *args = NULL;
  ipmi_device_t dev;
  
  FILE *infile = NULL;
  
  int i;
  
  struct hostent *hostinfo;
  struct sockaddr_in host;
  
  char *line = NULL;
  size_t n = 0;
  
  uint8_t *bytes_rq = NULL;
  int send_len;
  
  uint8_t bytes_rs[ARG_MAX];
  size_t rcvd_len;
  
  struct rlimit resource_limit;
  
  /* generate core dump on seg-fault */
  if (ipmi_is_root ())
    {
      resource_limit.rlim_cur =
	resource_limit.rlim_max = RLIM_INFINITY;
      if (setrlimit (RLIMIT_CORE, &resource_limit) != 0)
	perror ("warning: setrlimit()");
    }
  
  ipmi_raw_argp_parse (argc, argv);
  args = ipmi_raw_get_arguments ();
  
  if (args->common.host != NULL)
    {
      host.sin_family = AF_INET;
      host.sin_port = htons (RMCP_AUX_BUS_SHUNT);
      hostinfo = gethostbyname (args->common.host);
      if (hostinfo == NULL)
	{
	  perror ("gethostbyname()");
	  exit (EXIT_FAILURE);
	}
      host.sin_addr = *(struct in_addr *) hostinfo->h_addr;
      
      memset (&dev, 0, sizeof (ipmi_device_t));
      if (ipmi_open_outofband (&dev, 
			       IPMI_DEVICE_LAN, 
			       IPMI_MODE_DEFAULT, 
			       (struct sockaddr *) &host, 
			       sizeof (struct sockaddr), 
			       args->common.auth_type, 
			       args->common.username, 
			       args->common.password, 
			       args->common.priv_level) != 0)
	{
	  perror ("ipmi_open_outofband()");
	  exit (EXIT_FAILURE);
	}
    }
  else 
    {
      memset (&dev, 0, sizeof (ipmi_device_t));
      if (args->common.driver_type == IPMI_DEVICE_UNKNOWN)
	{
	  if (ipmi_open_inband (&dev, 
				args->common.disable_auto_probe, 
				IPMI_DEVICE_KCS, 
				args->common.driver_address, 
				0,
				args->common.driver_device, 
				IPMI_MODE_DEFAULT) != 0)
	    {
	      if (ipmi_open_inband (&dev, 
				    args->common.disable_auto_probe, 
				    IPMI_DEVICE_SSIF, 
				    args->common.driver_address, 
				    0,
				    args->common.driver_device, 
				    IPMI_MODE_DEFAULT) != 0)
		{
		  perror ("ipmi_open_inband()");
		  return (-1);
		 }
	    }
	}
      else 
	{
	  if (ipmi_open_inband (&dev, 
				args->common.disable_auto_probe, 
				args->common.driver_type, 
				args->common.driver_address, 
				0,
				args->common.driver_device, 
				IPMI_MODE_DEFAULT) != 0)
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
      if (ipmi_cmd_raw (&dev, bytes_rq, send_len, bytes_rs, &rcvd_len) == 0)
	{
	  printf ("rcvd: ");
	  for (i = 0; i < rcvd_len; i++)
	    printf ("%02X ", bytes_rs[i]);
	  printf ("\n");
	}
      else 
        {
          perror (program_invocation_short_name);
        }
      bytes_rq = NULL;
      send_len = 0;
    }
  
  if (args->cmd_file)
    {
      infile = fopen (args->cmd_file, "r");
      if (infile == NULL)
	{
          perror (program_invocation_short_name);
	  if (ipmi_close (&dev) != 0)
	    {
	      perror ("ipmi_close()");
	    }
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
      rcvd_len = ARG_MAX;
      
      if (getline (&line, &n, infile) == -1)
	{
	  /* perror ("getline()"); */
	  break;
	}
      
      if (string2bytes (line, &bytes_rq, &send_len) != 0)
	break;
      
      if (ipmi_cmd_raw (&dev, bytes_rq, send_len, bytes_rs, &rcvd_len) < 0)
        {
          perror (program_invocation_short_name);
          continue;
        }
      
      printf ("rcvd: ");
      for (i = 0; i < rcvd_len; i++)
	printf ("%02X ", bytes_rs[i]);
      printf ("\n");
    }
  
  if ((infile != NULL) && (infile != stdin))
    {
      fclose (infile);
    }
  
  if (ipmi_close (&dev) != 0)
    {
      perror ("ipmi_close()");
      exit (EXIT_FAILURE);
    }
  
  return (0);
}
