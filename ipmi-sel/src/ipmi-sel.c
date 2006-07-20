/*
ipmi-sel.c: System Event Logger utility.
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
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
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

#include "argp-common.h"
#include "ipmi-common.h"
#include "ipmi-sel-argp.h"
#include "ipmi-sel-wrapper.h"

int exit_status = 0;

int 
display_sel_info (ipmi_device_t *dev)
{
  local_sel_info_t sel_info;
  char str[512];
  time_t t;
  struct tm *tmp;
  
  memset (&sel_info, 0, sizeof (local_sel_info_t));
  
  if (get_sel_info (dev, &sel_info) != 0)
    {
      fprintf (stderr, "%s: unable to get SEL information\n", 
	       program_invocation_short_name);
      exit_status = -1;
      return (-1);
    }
  
  printf ("SEL version:                                      %d.%d\n", 
	  sel_info.sel_version_major, 
	  sel_info.sel_version_minor);
  printf ("Number of log entries:                            %d\n", 
	  sel_info.log_entry_count);
  printf ("Free space remaining:                             %d bytes\n", 
	  sel_info.free_space);
  
  t = sel_info.recent_addition_timestamp;
  tmp = localtime (&t);
  strftime (str, sizeof (str), "%m/%d/%Y - %H:%M:%S", tmp);
  printf ("Recent addition timestamp:                        %s\n", str);
  
  t = sel_info.recent_erase_timestamp;
  tmp = localtime (&t);
  strftime (str, sizeof (str), "%m/%d/%Y - %H:%M:%S", tmp);
  printf ("Recent erase timestamp:                           %s\n", str);
  
  printf ("Get SEL Allocation Information Command supported: %s\n", 
	  (sel_info.get_sel_alloc_info_cmd_support ? "Yes" : "No"));
  printf ("Reserve SEL Command supported:                    %s\n", 
	  (sel_info.reserve_sel_cmd_support ? "Yes" : "No"));
  printf ("Partial Add SEL Entry Command supported:          %s\n", 
	  (sel_info.partial_add_sel_entry_cmd_support ? "Yes" : "No"));
  printf ("Delete SEL Command supported:                     %s\n", 
	  (sel_info.delete_sel_cmd_support ? "Yes" : "No"));
  printf ("Events drop due to lack of space in SEL:          %s\n", 
	  (sel_info.overflow_flag ? "Yes" : "No"));
  
  return (0);
}

int 
display_sel_records (ipmi_device_t *dev)
{
  sel_record_t sel_rec;
  uint16_t record_id = 0;
  uint16_t next_record_id = 0;
  
  for (record_id = IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY;
       record_id != IPMI_SEL_GET_RECORD_ID_LAST_ENTRY;
       record_id = next_record_id)
    {
      memset (&sel_rec, 0, sizeof (sel_record_t));
      if (get_sel_record (dev, 
                          record_id, 
                          &sel_rec, 
                          &next_record_id) != 0)
	{
	  fprintf (stderr, "%s: unable to get SEL record\n", 
		   program_invocation_short_name);
	  return (-1);
	}
      
      printf ("%d", sel_rec.record_id);
      if (sel_rec.timestamp)
	printf (":%s", sel_rec.timestamp);
      if (sel_rec.sensor_info)
	printf (":%s", sel_rec.sensor_info);
      if (sel_rec.event_message)
	printf (":%s", sel_rec.event_message);
      if (sel_rec.event_data2_message)
	printf (":%s", sel_rec.event_data2_message);
      if (sel_rec.event_data3_message)
	printf (":%s", sel_rec.event_data3_message);
      printf ("\n");
      
      if (sel_rec.timestamp) free (sel_rec.timestamp);
      if (sel_rec.sensor_info) free (sel_rec.sensor_info);
      if (sel_rec.event_message) free (sel_rec.event_message);
      if (sel_rec.event_data2_message) free (sel_rec.event_data2_message);
      if (sel_rec.event_data3_message) free (sel_rec.event_data3_message);
    }
  
  return (0);
}

int 
hex_display_sel_records (ipmi_device_t *dev, FILE *stream)
{
  uint8_t record_data[SEL_RECORD_SIZE];
  uint32_t record_data_len = SEL_RECORD_SIZE;
  uint16_t record_id = 0;
  uint16_t next_record_id = 0;
  
  for (record_id = IPMI_SEL_GET_RECORD_ID_FIRST_ENTRY;
       record_id != IPMI_SEL_GET_RECORD_ID_LAST_ENTRY;
       record_id = next_record_id)
    {
      memset (record_data, 0, record_data_len);
      if (get_sel_record_raw (dev, 
                              record_id, 
                              record_data, 
                              record_data_len, 
                              &next_record_id) != 0)
	{
	  fprintf (stderr, "%s: unable to get SEL record\n", 
		   program_invocation_short_name);
	  return (-1);
	}
      
      fprintf (stream, 
	       "RID:[%02X][%02X] " 
	       "RT:[%02X] " 
	       "TS:[%02X][%02X][%02X][%02X] " 
	       "GID:[%02X][%02X] " 
	       "ER:[%02X] " 
	       "ST:[%02X] " 
	       "SN:[%02X] " 
	       "EDIR:[%02X] "
	       "ED1: [%02X] "
	       "ED2: [%02X] "
	       "ED3: [%02X]\n",
	       record_data[0], record_data[1], 
	       record_data[2], 
	       record_data[3], record_data[4], record_data[5], record_data[6], 
	       record_data[7], record_data[8], 
	       record_data[9], 
	       record_data[10], 
	       record_data[11], 
	       record_data[12], 
	       record_data[13], 
	       record_data[14], 
	       record_data[15]);
    }
  
  return (0);
}

int 
run_cmd_args (ipmi_device_t *dev, struct arguments *args)
{
  int retval = 0;
  
  if (args == NULL)
    return (-1);
  
  if (args->info_wanted)
    {
      retval = display_sel_info (dev);
      return retval;
    }
  
  if (args->hex_dump_wanted)
    {
      if (args->hex_dump_filename)
	{
	  FILE *stream = NULL;
	  
	  stream = fopen (args->hex_dump_filename, "a+");
	  if (stream != NULL)
	    {
	      retval = hex_display_sel_records (dev, stream);
	      fclose (stream);
	    }
	  else 
	    {
	      fprintf (stderr, "%s: unable to open hex dump file [%s]\n", 
		       program_invocation_short_name, 
		       args->hex_dump_filename);
	    }
	}
      else 
	{
	  retval = hex_display_sel_records (dev, stdout);
	}
      
      return retval;
    }
  
  if (args->delete_wanted)
    {
      int i;
      
      for (i = 0; i < args->delete_record_list_length; i++)
	{
	  int rval;
	  
	  rval = delete_sel_entry (dev, args->delete_record_list[i]);
	  if (rval != 0)
	    {
	      fprintf (stderr, "deletion of record ID %d failed\n", 
		       args->delete_record_list[i]);
	    }
	  
	  if (!retval)
	    retval = rval;
	}
      
      return retval;
    }
  
  if (args->delete_all_wanted)
    {
      retval = clear_sel_entries (dev);
      return retval;
    }
  
  if (args->delete_range_wanted)
    {
      int i = 0;
      
      for (i = args->delete_range1; i <= args->delete_range2; i++)
	{
	  delete_sel_entry (dev, i);
	}
      
      return retval;
    }
  
  retval = display_sel_records (dev);
  
  return retval;
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
  ipmi_device_t dev;
  
  struct hostent *hostinfo;
  struct sockaddr_in host;
  
  int retval = 0;
#ifdef NDEBUG
  int i;
#endif /* NDEBUG */
  
  _disable_coredump();

  ipmi_sel_argp_parse (argc, argv);
  args = ipmi_sel_get_arguments ();

#ifdef NDEBUG
  /* Clear out argv data for security purposes on ps(1). */
  for (i = 1; i < argc; i++)
    memset(argv[i], '\0', strlen(argv[i]));
#endif /* NDEBUG */
  
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
			       args->common.session_timeout, 
			       args->common.retry_timeout, 
			       (struct sockaddr *) &host,
			       sizeof (struct sockaddr),
			       args->common.authentication_type, 
			       args->common.username,
			       args->common.password,
			       args->common.privilege_level) != 0)
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

      memset (&dev, 0, sizeof (ipmi_device_t));
      if (args->common.driver_type == IPMI_DEVICE_UNKNOWN)
	{
	  if (ipmi_open_inband (&dev,
				args->common.disable_auto_probe,
				IPMI_DEVICE_KCS,
				args->common.driver_address,
				args->common.register_spacing,
				args->common.driver_device,
				IPMI_MODE_DEFAULT) != 0)
	    {
	      if (ipmi_open_inband (&dev,
				    args->common.disable_auto_probe,
				    IPMI_DEVICE_SSIF,
				    args->common.driver_address,
				    args->common.register_spacing,
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
				args->common.register_spacing,
				args->common.driver_device,
				IPMI_MODE_DEFAULT) != 0)
	    {
	      perror ("ipmi_open_inband()");
	      return (-1);
	    }
	}
    }
  
  retval = run_cmd_args (&dev, args);
  
  if (ipmi_close (&dev) != 0)
    {
      perror ("ipmi_close()");
      exit (EXIT_FAILURE);
    }
  
  return (retval);
}
