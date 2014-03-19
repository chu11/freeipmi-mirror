/*****************************************************************************\
 *  $Id: ipmipower_prompt.c,v 1.121 2010-08-06 18:38:37 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2003-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmipower is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmipower is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmipower.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <stdint.h>
#include <sys/stat.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <assert.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <errno.h>

#include "ipmipower_prompt.h"
#include "ipmipower_error.h"
#include "ipmipower_connection.h"
#include "ipmipower_oem.h"
#include "ipmipower_ping.h"
#include "ipmipower_powercmd.h"
#include "ipmipower_output.h"
#include "ipmipower_util.h"

#include "argv.h"

#include "freeipmi-portability.h"
#include "cbuf.h"
#include "hostlist.h"
#include "pstdout.h"
#include "tool-cmdline-common.h"
#include "tool-util-common.h"

extern cbuf_t ttyout;
extern cbuf_t ttyin;

extern struct ipmipower_arguments cmd_args;
extern struct ipmipower_connection *ics;

extern unsigned int ics_len;

extern struct oem_power_type_data *oem_power_type_data;

extern unsigned int output_counts[IPMIPOWER_MSG_TYPE_NUM_ENTRIES];

/* eliminate
 *
 * Note: only for non-interactive mode on the command line.  Won't be
 * anywhere in this file.
 */

static void
_cmd_driver_type (char **argv)
{
  assert (argv);

  if (argv[1])
    {
      int tmp;

      if ((tmp = parse_outofband_driver_type (argv[1])) < 0)
        ipmipower_cbuf_printf (ttyout,
                               "invalid driver type '%s' specified\n",
                               argv[1]);
      else
        {
          cmd_args.common_args.driver_type = tmp;
          ipmipower_cbuf_printf (ttyout,
                                 "driver type is now %s\n",
                                 argv[1]);
        }
    }
  else
    ipmipower_cbuf_printf (ttyout,
                           "driver_type must be specified: %s, %s\n",
                           IPMI_PARSE_DEVICE_LAN_STR,
                           IPMI_PARSE_DEVICE_LAN_2_0_STR);
}

static void
_cmd_hostname_clear (void)
{
  free (cmd_args.common_args.hostname);
  cmd_args.common_args.hostname = NULL;
      
  ipmipower_connection_array_destroy (ics, ics_len);
  ics = NULL;
  ics_len = 0;
}

static void
_cmd_hostname (char **argv)
{
  assert (argv);

  if (!argv[1])
    {
      _cmd_hostname_clear ();
      ipmipower_cbuf_printf (ttyout, "hostname(s) unconfigured\n");
    }
  else
    {
      struct ipmipower_connection *icsPtr;
      unsigned int len = 0;
      
      if (!(icsPtr = ipmipower_connection_array_create (argv[1], &len)))
        {
          /* dump error outputs here, most notably invalid hostname output */
          if (cbuf_read_to_fd (ttyout, STDOUT_FILENO, -1) > 0)
            return;
          else
            ipmipower_cbuf_printf (ttyout,
                                   "ipmipower_connection_array_create: %s\n",
                                   strerror (errno));
          return;
        }
      
      _cmd_hostname_clear ();

      ics = icsPtr;
      ics_len = len;
      
      if (!(cmd_args.common_args.hostname = strdup (argv[1])))
        {
          IPMIPOWER_ERROR (("strdup: %s", strerror(errno)));
          exit (EXIT_FAILURE);
        }

      ipmipower_ping_force_discovery_sweep ();

      ipmipower_cbuf_printf (ttyout,
                             "hostname: %s\n",
                             cmd_args.common_args.hostname);
    }
}

static void
_cmd_username (char **argv)
{
  assert (argv);

  if (!argv[1]
      || (argv[1] && strlen (argv[1]) <= IPMI_MAX_USER_NAME_LENGTH))
    {
      free (cmd_args.common_args.username);
      cmd_args.common_args.username = NULL;

      if (argv[1])
        {
          if (!(cmd_args.common_args.username = strdup (argv[1])))
            {
              IPMIPOWER_ERROR (("strdup: %s", strerror(errno)));
              exit (EXIT_FAILURE);
            }
        }

      ipmipower_cbuf_printf (ttyout,
                             "username: %s\n",
                             (cmd_args.common_args.username) ? cmd_args.common_args.username : "NULL");
    }
  else
    ipmipower_cbuf_printf (ttyout, "username invalid length\n");
}

static void
_cmd_password (char **argv)
{
  assert (argv);

  if (argv[1] && cmd_args.common_args.authentication_type == IPMI_AUTHENTICATION_TYPE_NONE)
    ipmipower_cbuf_printf (ttyout,
                           "password cannot be set for authentication_type '%s'\n",
                           IPMI_PARSE_AUTHENTICATION_TYPE_NONE_STR);
  else if (!argv[1]
           || (argv[1]
               && ((cmd_args.common_args.driver_type == IPMI_DEVICE_LAN_2_0
                    && strlen (argv[1]) <= IPMI_2_0_MAX_PASSWORD_LENGTH)
                   || (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN
                       && strlen (argv[1]) <= IPMI_1_5_MAX_PASSWORD_LENGTH))))
    {
      free (cmd_args.common_args.password);
      cmd_args.common_args.password = NULL;

      if (argv[1])
        {
          if (!(cmd_args.common_args.password = strdup (argv[1])))
            {
              IPMIPOWER_ERROR (("strdup: %s", strerror(errno)));
              exit (EXIT_FAILURE);
            }
        }

#ifdef NDEBUG
      ipmipower_cbuf_printf (ttyout, "password changed\n");
#else  /* !NDEBUG */
      ipmipower_cbuf_printf (ttyout,
                             "password: %s\n",
                             (cmd_args.common_args.password) ? cmd_args.common_args.password : "NULL");
#endif /* !NDEBUG */
    }
  else
    ipmipower_cbuf_printf (ttyout, "password invalid length\n");
}

static void
_cmd_k_g (char **argv)
{
  int rv = 0;
#ifndef NDEBUG
  char buf[IPMI_MAX_K_G_LENGTH*2+3];
#endif /* !NDEBUG */

  assert (argv);

  if (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN)
    ipmipower_cbuf_printf (ttyout, "k_g is only used for IPMI 2.0");
  else
    {
      memset (cmd_args.common_args.k_g, '\0', IPMI_MAX_K_G_LENGTH);

      if (argv[1])
        rv = parse_kg (cmd_args.common_args.k_g, IPMI_MAX_K_G_LENGTH + 1, argv[1]);

      if (rv < 0)
        ipmipower_cbuf_printf (ttyout, "k_g invalid\n");
      else
        {
          cmd_args.common_args.k_g_len = rv;
#ifdef NDEBUG
          ipmipower_cbuf_printf (ttyout, "k_g changed\n");
#else  /* !NDEBUG */
          ipmipower_cbuf_printf (ttyout,
"k_g: %s\n",
                                 (cmd_args.common_args.k_g_len) ? format_kg (buf,
									     IPMI_MAX_K_G_LENGTH*2+3,
									     cmd_args.common_args.k_g) : "NULL");
#endif /* !NDEBUG */
        }
    }
}

static void
_cmd_authentication_type (char **argv)
{
  assert (argv);

  if (argv[1])
    {
      int tmp;

      if ((tmp = parse_authentication_type (argv[1])) < 0)
        ipmipower_cbuf_printf (ttyout,
                               "%s invalid authentication_type\n",
                               argv[1]);
      else
        {
          cmd_args.common_args.authentication_type = tmp;
          ipmipower_cbuf_printf (ttyout,
                                 "authentication type is now %s\n",
                                 argv[1]);
        }
    }
  else
    ipmipower_cbuf_printf (ttyout,
                           "authentication_type must be specified: %s, %s, %s, %s\n",
                           IPMI_PARSE_AUTHENTICATION_TYPE_NONE_STR,
                           IPMI_PARSE_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY_STR,
                           IPMI_PARSE_AUTHENTICATION_TYPE_MD2_STR,
                           IPMI_PARSE_AUTHENTICATION_TYPE_MD5_STR);
}

static void
_cmd_cipher_suite_id (char **argv)
{
  assert (argv);

  if (argv[1])
    {
      char *endptr;
      int tmp;

      errno = 0;
      tmp = strtol (argv[1], &endptr, 10);
      if (errno
	  || endptr[0] != '\0'
          || tmp < IPMI_CIPHER_SUITE_ID_MIN
          || tmp > IPMI_CIPHER_SUITE_ID_MAX)
        ipmipower_cbuf_printf (ttyout,
                               "%s invalid cipher suite id\n",
                               argv[1]);
      else if (!IPMI_CIPHER_SUITE_ID_SUPPORTED (tmp))
        ipmipower_cbuf_printf (ttyout,
                               "%s unsupported cipher suite id\n",
                               argv[1]);
      else
        {
          cmd_args.common_args.cipher_suite_id = tmp;
          ipmipower_cbuf_printf (ttyout,
                                 "cipher suite id is now %s\n",
                                 argv[1]);
        }
    }
  else
    ipmipower_cbuf_printf (ttyout,
                           "cipher_suite_id must be specified: 0, 1, 2, 3, 6, 7, 8, 11, 12\n");
}

static void
_cmd_privilege_level (char **argv)
{
  assert (argv);

  if (argv[1])
    {
      int tmp;

      if ((tmp = parse_privilege_level (argv[1])) < 0)
        ipmipower_cbuf_printf (ttyout,
                               "%s invalid privilege_level\n",
                               argv[1]);
      else
        {
          cmd_args.common_args.authentication_type = tmp;
          ipmipower_cbuf_printf (ttyout,
                                 "privilege_level type is now %s\n",
                                 argv[1]);
        }
    }
  else
    ipmipower_cbuf_printf (ttyout,
                           "privilege must be specified: %s, %s, %s\n",
                           IPMI_PARSE_PRIVILEGE_LEVEL_USER_STR,
                           IPMI_PARSE_PRIVILEGE_LEVEL_OPERATOR_STR,
                           IPMI_PARSE_PRIVILEGE_LEVEL_ADMIN_STR);
}

static void
_cmd_workaround_flags (char **argv)
{
  assert (argv);

  if (argv[1])
    {
      unsigned int outofband_flags, outofband_2_0_flags;

      if (parse_workaround_flags_tool (argv[1],
				       &outofband_flags,
				       &outofband_2_0_flags,
				       NULL,
				       NULL,
				       NULL) < 0)
        ipmipower_cbuf_printf (ttyout,
                               "%s invalid workaround flags specified\n",
                               argv[1]);
      else
        {
          cmd_args.common_args.workaround_flags_outofband = outofband_flags;
          cmd_args.common_args.workaround_flags_outofband_2_0 = outofband_2_0_flags;
          ipmipower_cbuf_printf (ttyout,
                                 "workaround flags are now %s\n",
                                 argv[1]);
        }
    }
  else
    ipmipower_cbuf_printf (ttyout,
                           "workaround_flags must be specified: %s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n",
			   IPMI_PARSE_WORKAROUND_FLAGS_NONE_STR,
                           IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES_STR,
			   IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_NO_CHECKSUM_CHECK_STR,
                           IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_ACCEPT_SESSION_ID_ZERO_STR,
                           IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_FORCE_PERMSG_AUTHENTICATION_STR,
                           IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_CHECK_UNEXPECTED_AUTHCODE_STR,
                           IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_BIG_ENDIAN_SEQUENCE_NUMBER_STR,
                           IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_NO_AUTH_CODE_CHECK_STR,
                           IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION_STR,
                           IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUPERMICRO_2_0_SESSION_STR,
                           IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUN_2_0_SESSION_STR,
                           IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_OPEN_SESSION_PRIVILEGE_STR,
                           IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NON_EMPTY_INTEGRITY_CHECK_VALUE_STR,
			   IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IPMIPING_STR);
}

static void
_cmd_power_all_nodes (ipmipower_power_cmd_t cmd)
{
  struct ipmipower_connection_extra_arg *eanode;
  unsigned int nodes_queued = 0;
  int i;

  assert (IPMIPOWER_POWER_CMD_VALID (cmd));
     
  memset (output_counts, '\0', sizeof (output_counts));

  for (i = 0; i < ics_len; i++)
    {
      if (cmd_args.ping_interval
	  && ics[i].discover_state == IPMIPOWER_DISCOVER_STATE_UNDISCOVERED)
	ipmipower_output (IPMIPOWER_MSG_TYPE_NOTDISCOVERED, ics[i].hostname, NULL);
      else if (cmd_args.ping_interval
	       && cmd_args.ping_packet_count
	       && cmd_args.ping_percent
	       && ics[i].discover_state == IPMIPOWER_DISCOVER_STATE_BADCONNECTION)
	ipmipower_output (IPMIPOWER_MSG_TYPE_BADCONNECTION, ics[i].hostname, NULL);
      else
	{
	  if (cmd_args.oem_power_type != IPMIPOWER_OEM_POWER_TYPE_NONE)
	    {
	      eanode = ics[i].extra_args;
	      while (eanode)
		{
		  if (ipmipower_oem_power_cmd_check_extra_arg (eanode->extra_arg,
							       NULL,
							       0) <= 0)
		    ipmipower_output (IPMIPOWER_MSG_TYPE_INVALID_ARGUMENT_FOR_OEM_EXTENSION,
				      ics[i].hostname,
				      eanode->extra_arg);
		  else
		    {
		      ipmipower_connection_clear (&ics[i]);
		      ipmipower_powercmd_queue (cmd, &ics[i], eanode->extra_arg);
		      nodes_queued++;
		    }
		  eanode = eanode->next;
		}
	    }
	  else
	    {
	      ipmipower_connection_clear (&ics[i]);
	      ipmipower_powercmd_queue (cmd, &ics[i], NULL);
	      nodes_queued++;
	    }
	}
    }
  
  /* Special corner case when no nodes are discovered */
  if (!nodes_queued)
    ipmipower_output_finish ();
}

static void
_cmd_power_specific_nodes (char **argv, ipmipower_power_cmd_t cmd)
{
  hostlist_t h = NULL;
  hostlist_iterator_t hitr = NULL;
  hostlist_t h2 = NULL;
  hostlist_iterator_t h2itr = NULL;
  char *hstr = NULL; 
  char *h2str = NULL;

  assert (argv);
  assert (IPMIPOWER_POWER_CMD_VALID (cmd));
  assert (IPMIPOWER_OEM_POWER_TYPE_VALID (cmd_args.oem_power_type));
      
  if (!(h = hostlist_create (argv[1])))
    {
      ipmipower_cbuf_printf (ttyout, "invalid hostname(s) specified\n");
      goto cleanup;
    }
  
  if (!(hitr = hostlist_iterator_create (h)))
    {
      IPMIPOWER_ERROR (("hostlist_iterator_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  
  memset (output_counts, '\0', sizeof (output_counts));

  while ((hstr = hostlist_next (hitr)))
    {
      /* achu: The double hostlist_create is to handle the corner case
       * of someone inputting.
       *
       * foohost[1-3]+[1-3]
       *
       * We need to double hostlist to get all the hosts and extra
       * args.
       *
       * Under most scenarios, this is just inefficient code.  But we
       * feel the performance hit isn't egregious.  In addition, the
       * code logic is simpler to do it this way then have a whole
       * bunch of wacky if-check scenarios to make it more efficient.
       * We'll revisit as necessary in the future.
       */

      if (!(h2 = hostlist_create (hstr)))
        {
	  ipmipower_cbuf_printf (ttyout, "invalid hostname(s) specified\n");
	  goto cleanup;
        }

      hostlist_uniq (h2);

      if (!(h2itr = hostlist_iterator_create (h2)))
        {
          IPMIPOWER_ERROR (("hostlist_iterator_create: %s", strerror (errno)));
          exit (EXIT_FAILURE);
        }

      while ((h2str = hostlist_next (h2itr)))
	{
	  char *h2str_extra_arg = NULL;
	  int i;
	  
	  if (cmd_args.oem_power_type != IPMIPOWER_OEM_POWER_TYPE_NONE)
	    {
	      char *ptr;
	      
	      if ((ptr = strchr (h2str, '+')))
		{
		  *ptr = '\0';
		  ptr++;
		  
		  if (!(h2str_extra_arg = strdup (ptr)))
		    {
		      IPMIPOWER_ERROR (("strdup: %s", strerror(errno)));
		      exit (EXIT_FAILURE);
		    }
		}
	    }
	  
	  i = ipmipower_connection_hostname_index (ics, ics_len, h2str);
	  
	  if (i < 0)
	    ipmipower_output (IPMIPOWER_MSG_TYPE_UNCONFIGURED_HOSTNAME, h2str, NULL);
	  else if (cmd_args.ping_interval
		   && ics[i].discover_state == IPMIPOWER_DISCOVER_STATE_UNDISCOVERED)
	    ipmipower_output (IPMIPOWER_MSG_TYPE_NOTDISCOVERED, ics[i].hostname, NULL);
	  else if (cmd_args.ping_interval
		   && cmd_args.ping_packet_count
		   && cmd_args.ping_percent
		   && ics[i].discover_state == IPMIPOWER_DISCOVER_STATE_BADCONNECTION)
	    ipmipower_output (IPMIPOWER_MSG_TYPE_BADCONNECTION, ics[i].hostname, NULL);
	  else
	    {
	      if (cmd_args.oem_power_type != IPMIPOWER_OEM_POWER_TYPE_NONE)
		{
		  if (ipmipower_oem_power_cmd_check_extra_arg (h2str_extra_arg,
							       NULL,
							       0) <= 0)
		    {
		      ipmipower_output (IPMIPOWER_MSG_TYPE_INVALID_ARGUMENT_FOR_OEM_EXTENSION,
					ics[i].hostname,
					h2str_extra_arg);
		      goto end_inner_loop;
		    }
		  ipmipower_connection_clear (&ics[i]);
		  ipmipower_powercmd_queue (cmd, &ics[i], h2str_extra_arg);
		}
	      else
		{
		  ipmipower_connection_clear (&ics[i]);
		  ipmipower_powercmd_queue (cmd, &ics[i], NULL);
		}
	    }
	  
	end_inner_loop:
	  free (h2str_extra_arg);
	  free (h2str);
	  h2str = NULL;
	}
      
      hostlist_iterator_destroy (h2itr);
      hostlist_destroy (h2);
      h2itr = NULL;
      h2 = NULL;
    }
  
 cleanup:
  hostlist_iterator_destroy (h2itr);
  hostlist_destroy (h2);
  hostlist_iterator_destroy (hitr);
  hostlist_destroy (h);
  free (hstr);
  free (h2str);
}

static void
_cmd_power (char **argv, ipmipower_power_cmd_t cmd)
{
  char errbuf[IPMIPOWER_OUTPUT_BUFLEN + 1];

  assert (argv);
  assert (IPMIPOWER_POWER_CMD_VALID (cmd));
  assert (IPMIPOWER_OEM_POWER_TYPE_VALID (cmd_args.oem_power_type));
  
  if (!cmd_args.common_args.hostname)
    {
      ipmipower_cbuf_printf (ttyout, "no hostname(s) configured\n");
      return;
    }
  
  memset (errbuf, '\0', IPMIPOWER_OUTPUT_BUFLEN + 1);
  if (cmd_args.oem_power_type == IPMIPOWER_OEM_POWER_TYPE_NONE)
    {
      if (ipmipower_power_cmd_check_privilege (cmd,
					       errbuf,
					       IPMIPOWER_OUTPUT_BUFLEN) <= 0)
	{
	  ipmipower_cbuf_printf (ttyout, "%s\n", errbuf);
	  return;
	}
    }
  else
    {
      if (ipmipower_oem_power_cmd_check_support_and_privilege (cmd,
							       errbuf,
							       IPMIPOWER_OUTPUT_BUFLEN) <= 0)
  	{
	  ipmipower_cbuf_printf (ttyout, "%s\n", errbuf);
	  return;
	}
    }
  
  /* all nodes */
  if (!argv[1])
    _cmd_power_all_nodes (cmd);
  else
    _cmd_power_specific_nodes (argv, cmd);
}

static void
_cmd_help (void)
{
  ipmipower_cbuf_printf (ttyout,
                         "driver-type IPMIDRIVER                   - Specify the ipmi driver to use.\n"
                         "hostname [IPMIHOST]                      - Specify a new set of hosts.  No input to unconfigure all hosts.\n"
                         "username [USERNAME]                      - Specify a new username.  No input for null username.\n"
                         "password [PASSWORD]                      - Specify a new password.  No input for null password.\n"
                         "k_g [K_G]                                - Specify a new K_g BMC Key.  No input for null key.\n"
                         "session-timeout MILLISECONDS             - Specify a new session timeout length.\n"
                         "retransmission-timeout MILLISECONDS      - Specify a new retransmission timeout length.\n"
                         "authentication-type AUTHENTICATION-TYPE  - Specify the authentication type to use.\n"
                         "cipher-suite-id CIPHER-SUITE-ID          - Specify the cipher suite id to use.\n"
                         "privilege-level PRIVILEGE-LEVEL          - Specify the privilege level to use.\n"
                         "workaround-flags WORKAROUNDS             - Specify workaround flags.\n"
                         "debug [on|off]                           - Toggle debug to stderr.\n");
#ifndef NDEBUG
  ipmipower_cbuf_printf (ttyout,
                         "rmcpdump [on|off]                        - Toggle RMCP dump output.\n");
#endif /* NDEBUG */
  ipmipower_cbuf_printf (ttyout,
                         "on [IPMIHOST(s)]                         - Turn on all configured hosts or specified hosts.\n"
                         "off [IPMIHOST(s)]                        - Turn off all configured hosts or specified hosts.\n"
                         "cycle [IPMIHOST(s)]                      - Power cycle all configured hosts or specified hosts.\n"
                         "reset [IPMIHOST(s)]                      - Reset all configured hosts or specified hosts.\n"
                         "stat [IPMIHOST(s)]                       - Query power status for all configured hosts or specified hosts.\n"
                         "pulse [IPMIHOST(s)]                      - Pulse diagnostic interrupt all configured hosts or specified hosts.\n"
                         "soft [IPMIHOST(s)]                       - Initiate a soft-shutdown for all configured hosts or specified hosts.\n"
                         "identify-on [IPMIHOST(s)]                - Turn on physical system identification.\n"
                         "identify-off [IPMIHOST(s)]               - Turn off physical system identification.\n"
                         "identify-status [IPMIHOST(s)]            - Query physical system identification status.\n"
                         "on-if-off [on|off]                       - Toggle on-if-off functionality.\n"
                         "wait-until-on [on|off]                   - Toggle wait-until-on functionality.\n"
                         "wait-until-off [on|off]                  - Toggle wait-until-off functionality.\n"
                         "retransmission-wait-timeout MILLISECONDS - Specify a new retransmission timeout length.\n"
                         "retransmission-backoff-count COUNT       - Specify a new retransmission backoff count.\n"
                         "ping-interval MILLISECONDS               - Specify a new ping interval length.\n"
                         "ping-timeout MILLISECONDS                - Specify a new ping timeout length.\n"
                         "ping-packet-count COUNT                  - Specify a new ping packet count.\n"
                         "ping-percent COUNT                       - Specify a new ping percent number.\n"
                         "ping-consec-count COUNT                  - Specify a new ping consec count.\n"
                         "buffer-output [on|off]                   - Toggle buffer-output functionality.\n"
                         "consolidate-output [on|off]              - Toggle consolidate-output functionality.\n"
                         "fanout COUNT                             - Specify a fanout.\n"
                         "always-prefix [on|off]                   - Toggle always-prefix functionality.\n"
                         "help                                     - Output help menu.\n"
                         "version                                  - Output version.\n"
                         "config                                   - Output current configuration.\n"
                         "quit                                     - Quit program.\n");
}

static void
_cmd_version (void)
{
  ipmipower_cbuf_printf (ttyout, "ipmipower %s\n", VERSION);
}

static void
_workarounds_strcat (char *strbuf,
                     unsigned int bitmask,
                     unsigned int mask,
                     const char *str,
                     int *is_first)
{
  assert (strbuf && str && is_first);

  if (bitmask & mask)
    {
      if ((*is_first))
        strcat (strbuf, ",");
      strcat (strbuf, str);
      (*is_first)++;
    }
}

static void
_cmd_debug (char **argv)
{
  assert (argv);

  if (!argv[1])
    cmd_args.common_args.debug = !cmd_args.common_args.debug;
  else
    {
      if (!strcasecmp (argv[1], "on"))
        cmd_args.common_args.debug = 1;
      else if (!strcasecmp (argv[1], "off"))
        cmd_args.common_args.debug = 0;
      else
        {
          ipmipower_cbuf_printf (ttyout, "invalid parameter\n");
          return;
        }
    }
  ipmipower_cbuf_printf (ttyout,
                         "debugging is now %s\n", (cmd_args.common_args.debug) ? "on" : "off");
}

static void
_cmd_config (void)
{
#ifndef NDEBUG
  char kgbuf[IPMI_MAX_K_G_LENGTH*2+3];
#endif /* NDEBUG */
  char strbuf[IPMIPOWER_OUTPUT_BUFLEN];
  char *str;
  int is_first = 0;

  str = "";
  if (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN)
    str = IPMI_PARSE_DEVICE_LAN_STR;
  else if (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN_2_0)
    str = IPMI_PARSE_DEVICE_LAN_2_0_STR;

  ipmipower_cbuf_printf (ttyout,
                         "Driver_Type:                  %s\n",
                         str);

  if (cmd_args.common_args.hostname)
    {
#ifndef NDEBUG
      int i;
      hostlist_t discovered = NULL;
      hostlist_t undiscovered = NULL;
      hostlist_t badconnection = NULL;
      char buf[IPMIPOWER_OUTPUT_BUFLEN];
      int rv;
#endif /* NDEBUG */

      ipmipower_cbuf_printf (ttyout,
                             "Hostname:                     %s\n",
                             cmd_args.common_args.hostname);

#ifndef NDEBUG
      if (!(discovered = hostlist_create (NULL)))
        goto cleanup;
      if (!(undiscovered = hostlist_create (NULL)))
        goto cleanup;
      if (!(badconnection = hostlist_create (NULL)))
        goto cleanup;

      for (i = 0; i < ics_len; i++)
        {
          if (ics[i].discover_state == IPMIPOWER_DISCOVER_STATE_DISCOVERED)
            rv = hostlist_push_host (discovered, ics[i].hostname);
          else if (ics[i].discover_state == IPMIPOWER_DISCOVER_STATE_UNDISCOVERED)
            rv = hostlist_push_host (undiscovered, ics[i].hostname);
          else
            rv = hostlist_push_host (badconnection, ics[i].hostname);
          
          if (!rv)
            goto cleanup;
        }
      
      hostlist_sort (discovered);
      hostlist_sort (undiscovered);
      hostlist_sort (badconnection);

      if ((rv = hostlist_ranged_string (discovered, IPMIPOWER_OUTPUT_BUFLEN, buf)) < 0)
        {
          IPMIPOWER_ERROR (("hostlist_ranged_string: %s", strerror (errno)));
          exit (EXIT_FAILURE);
        }

      if (rv > 0)
        ipmipower_cbuf_printf (ttyout,
                               "Discovered:                   %s\n",
                               buf);

      if ((rv = hostlist_ranged_string (undiscovered, IPMIPOWER_OUTPUT_BUFLEN, buf)) < 0)
        {
          IPMIPOWER_ERROR (("hostlist_ranged_string: %s", strerror (errno)));
          exit (EXIT_FAILURE);
        }

      if (rv > 0)
        ipmipower_cbuf_printf (ttyout,
                               "Undiscovered:                 %s\n",
                               buf);

      if ((rv = hostlist_ranged_string (badconnection, IPMIPOWER_OUTPUT_BUFLEN, buf)) < 0)
        {
          IPMIPOWER_ERROR (("hostlist_ranged_string: %s", strerror (errno)));
          exit (EXIT_FAILURE);
        }

      if (rv > 0)
        ipmipower_cbuf_printf (ttyout,
                               "BadConnection:                %s\n",
                               buf);

    cleanup:
      hostlist_destroy (discovered);
      hostlist_destroy (undiscovered);
      hostlist_destroy (badconnection);
#endif /* NDEBUG */
    }
  else
    ipmipower_cbuf_printf (ttyout,
                           "Hostname:                     NONE\n");

  ipmipower_cbuf_printf (ttyout,
                         "Username:                     %s\n",
                         (cmd_args.common_args.username) ? cmd_args.common_args.username : "NULL");

#ifndef NDEBUG
  ipmipower_cbuf_printf (ttyout,
                         "Password:                     %s\n",
                         (cmd_args.common_args.password) ? cmd_args.common_args.password : "NULL");
  ipmipower_cbuf_printf (ttyout,
                         "K_g:                          %s\n",
                         (cmd_args.common_args.k_g_len) ?
                         format_kg (kgbuf, IPMI_MAX_K_G_LENGTH*2+3, cmd_args.common_args.k_g) : "NULL");
#else  /* !NDEBUG */
  ipmipower_cbuf_printf (ttyout,
                         "Password:                     *****\n");
  ipmipower_cbuf_printf (ttyout,
                         "K_g:                          *****\n");
#endif /* !NDEBUG */

  ipmipower_cbuf_printf (ttyout,
                         "Session Timeout:              %u ms\n",
                         cmd_args.common_args.session_timeout);
  ipmipower_cbuf_printf (ttyout,
                         "Retransmission Timeout:       %u ms\n",
                         cmd_args.common_args.retransmission_timeout);

  str = "";
  if (cmd_args.common_args.authentication_type == IPMI_AUTHENTICATION_TYPE_NONE)
    str = IPMI_PARSE_AUTHENTICATION_TYPE_NONE_STR;
  else if (cmd_args.common_args.authentication_type == IPMI_AUTHENTICATION_TYPE_MD2)
    str = IPMI_PARSE_AUTHENTICATION_TYPE_MD2_STR;
  else if (cmd_args.common_args.authentication_type == IPMI_AUTHENTICATION_TYPE_MD5)
    str = IPMI_PARSE_AUTHENTICATION_TYPE_MD5_STR;
  else if (cmd_args.common_args.authentication_type == IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY)
    str = IPMI_PARSE_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY_STR;

  ipmipower_cbuf_printf (ttyout,
                         "Authentication_Type:          %s\n",
                         str);

  str = "";
  if (cmd_args.common_args.cipher_suite_id == 0)
    str = "0";
  else if (cmd_args.common_args.cipher_suite_id == 1)
    str = "1";
  else if (cmd_args.common_args.cipher_suite_id == 2)
    str = "2";
  else if (cmd_args.common_args.cipher_suite_id == 3)
    str = "3";
  else if (cmd_args.common_args.cipher_suite_id == 6)
    str = "6";
  else if (cmd_args.common_args.cipher_suite_id == 7)
    str = "7";
  else if (cmd_args.common_args.cipher_suite_id == 8)
    str = "8";
  else if (cmd_args.common_args.cipher_suite_id == 11)
    str = "11";
  else if (cmd_args.common_args.cipher_suite_id == 12)
    str = "12";

  ipmipower_cbuf_printf (ttyout,
                         "Cipher Suite Id:              %s\n",
                         str);

  str = "";
  if (cmd_args.common_args.privilege_level == IPMI_PRIVILEGE_LEVEL_USER)
    str = IPMI_PARSE_PRIVILEGE_LEVEL_USER_STR;
  else if (cmd_args.common_args.privilege_level == IPMI_PRIVILEGE_LEVEL_OPERATOR)
    str = IPMI_PARSE_PRIVILEGE_LEVEL_OPERATOR_STR;
  else if (cmd_args.common_args.privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN)
    str = IPMI_PARSE_PRIVILEGE_LEVEL_ADMIN_STR;

  ipmipower_cbuf_printf (ttyout,
                         "Privilege_Level:              %s\n",
                         str);

  memset (strbuf, '\0', IPMIPOWER_OUTPUT_BUFLEN);
  is_first = 0;
  _workarounds_strcat (strbuf,
                       cmd_args.common_args.workaround_flags_outofband,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES_STR,
                       &is_first);
  _workarounds_strcat (strbuf,
                       cmd_args.common_args.workaround_flags_outofband,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_ACCEPT_SESSION_ID_ZERO,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_ACCEPT_SESSION_ID_ZERO_STR,
                       &is_first);
  _workarounds_strcat (strbuf,
                       cmd_args.common_args.workaround_flags_outofband,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_FORCE_PERMSG_AUTHENTICATION,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_FORCE_PERMSG_AUTHENTICATION_STR,
                       &is_first);
  _workarounds_strcat (strbuf,
                       cmd_args.common_args.workaround_flags_outofband,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_CHECK_UNEXPECTED_AUTHCODE,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_CHECK_UNEXPECTED_AUTHCODE_STR,
                       &is_first);
  _workarounds_strcat (strbuf,
                       cmd_args.common_args.workaround_flags_outofband,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_BIG_ENDIAN_SEQUENCE_NUMBER,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_BIG_ENDIAN_SEQUENCE_NUMBER_STR,
                       &is_first);
  _workarounds_strcat (strbuf,
                       cmd_args.common_args.workaround_flags_outofband,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_NO_AUTH_CODE_CHECK,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_NO_AUTH_CODE_CHECK_STR,
                       &is_first);
  _workarounds_strcat (strbuf,
                       cmd_args.common_args.workaround_flags_outofband,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_NO_CHECKSUM_CHECK,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_NO_CHECKSUM_CHECK_STR,
                       &is_first);
  /* This is a duplicate of the IPMI 1.5 version */
#if 0
  _workarounds_strcat (strbuf,
                       cmd_args.common_args.workaround_flags_outofband_2_0,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_AUTHENTICATION_CAPABILITIES,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_AUTHENTICATION_CAPABILITIES_STR,
                       &is_first);
#endif
  _workarounds_strcat (strbuf,
                       cmd_args.common_args.workaround_flags_outofband_2_0,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION_STR,
                       &is_first);
  _workarounds_strcat (strbuf,
                       cmd_args.common_args.workaround_flags_outofband_2_0,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUPERMICRO_2_0_SESSION,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUPERMICRO_2_0_SESSION_STR,
                       &is_first);
  _workarounds_strcat (strbuf,
                       cmd_args.common_args.workaround_flags_outofband_2_0,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUN_2_0_SESSION,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUN_2_0_SESSION_STR,
                       &is_first);
  _workarounds_strcat (strbuf,
                       cmd_args.common_args.workaround_flags_outofband_2_0,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_OPEN_SESSION_PRIVILEGE,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_OPEN_SESSION_PRIVILEGE_STR,
                       &is_first);
  _workarounds_strcat (strbuf,
                       cmd_args.common_args.workaround_flags_outofband_2_0,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NON_EMPTY_INTEGRITY_CHECK_VALUE,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NON_EMPTY_INTEGRITY_CHECK_VALUE_STR,
                       &is_first);
  /* This is a duplicate of the IPMI 1.5 version */
#if 0
  _workarounds_strcat (strbuf,
                       cmd_args.common_args.workaround_flags_outofband_2_0,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NO_CHECKSUM_CHECK,
                       IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_NO_CHECKSUM_CHECK_STR,
                       &is_first);
#endif
  _workarounds_strcat (strbuf,
                       cmd_args.common_args.workaround_flags_outofband_2_0,
                       IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IPMIPING,
                       IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_IPMIPING_STR,
                       &is_first);

  ipmipower_cbuf_printf (ttyout,
                         "WorkaroundFlags:              %s\n",
                         strbuf);

  ipmipower_cbuf_printf (ttyout,
                         "Debug:                        %s\n",
                         cmd_args.common_args.debug ? "on" : "off");

#ifndef NDEBUG
  ipmipower_cbuf_printf (ttyout,
                         "Rmcpdump:                     %s\n",
                         (cmd_args.rmcpdump) ? "on" : "off");
#endif /* NDEBUG */

  ipmipower_cbuf_printf (ttyout,
                         "On-If-Off:                    %s\n",
                         (cmd_args.on_if_off) ? "enabled" : "disabled");
  ipmipower_cbuf_printf (ttyout,
                         "Wait-Until-On:                %s\n",
                         (cmd_args.wait_until_on) ? "enabled" : "disabled");
  ipmipower_cbuf_printf (ttyout,
                         "Wait-Until-Off:               %s\n",
                         (cmd_args.wait_until_off) ? "enabled" : "disabled");
  ipmipower_cbuf_printf (ttyout,
                         "Retransmission Wait Timeout:  %u ms\n",
                         cmd_args.retransmission_wait_timeout);
  ipmipower_cbuf_printf (ttyout,
                         "Retransmission Backoff Count: %u\n",
                         cmd_args.retransmission_backoff_count);
  ipmipower_cbuf_printf (ttyout,
                         "Ping Interval:                %u ms\n",
                         cmd_args.ping_interval);
  ipmipower_cbuf_printf (ttyout,
                         "Ping Timeout:                 %u ms\n",
                         cmd_args.ping_timeout);
  ipmipower_cbuf_printf (ttyout,
                         "Ping Packet Count:            %u\n",
                         cmd_args.ping_packet_count);
  ipmipower_cbuf_printf (ttyout,
                         "Ping Percent:                 %u percent\n",
                         cmd_args.ping_percent);
  ipmipower_cbuf_printf (ttyout,
                         "Ping Consec Count:            %u\n",
                         cmd_args.ping_consec_count);

  ipmipower_cbuf_printf (ttyout,
                         "Buffer-Output:                %s\n",
                         (cmd_args.common_args.buffer_output) ? "enabled" : "disabled");
  ipmipower_cbuf_printf (ttyout,
                         "Consolidate-Output:           %s\n",
                         (cmd_args.common_args.consolidate_output) ? "enabled" : "disabled");
  ipmipower_cbuf_printf (ttyout,
                         "Fanout:                       %u\n",
                         cmd_args.common_args.fanout);
  ipmipower_cbuf_printf (ttyout,
                         "Always-Prefix:                %s\n",
                         (cmd_args.common_args.always_prefix) ? "enabled" : "disabled");
}

static void
_cmd_set_unsigned_int (char **argv,
                       unsigned int *value,
                       const char *str,
                       int allow_zero)
{
  assert (argv && value && str);

  if (!argv[1])
    ipmipower_cbuf_printf (ttyout,
                           "%s not specified\n",
                           str);
  else
    {
      char *endptr;
      unsigned int temp;

      errno = 0;
      temp = strtoul (argv[1], &endptr, 10);
      if (errno
	  || endptr[0] != '\0')
        ipmipower_cbuf_printf (ttyout,
                               "invalid %s input\n",
                               str);
      else if (allow_zero && !temp)
        {
          *value = temp;
          ipmipower_cbuf_printf (ttyout,
                                 "%s is now %d\n",
                                 str,
                                 *value);
        }
      else
        ipmipower_cbuf_printf (ttyout,
                               "invalid %s input\n",
                               str);
    }
}

static void
_cmd_set_unsigned_int_ranged (char **argv,
                              unsigned int *value,
                              const char *str,
                              int allow_zero,
                              int min,
                              int max)
{
  assert (argv && value && str);

  if (!argv[1])
    ipmipower_cbuf_printf (ttyout,
                           "%s not specified\n",
                           str);
  else
    {
      char *endptr;
      int temp;

      errno = 0;
      temp = strtol (argv[1], &endptr, 10);
      if (errno
	  || endptr[0] != '\0')
        ipmipower_cbuf_printf (ttyout,
                               "invalid %s input\n",
                               str);
      else if ((allow_zero
                && !temp)
               || (temp <= max
                   && temp >= min))
        {
          *value = temp;
          ipmipower_cbuf_printf (ttyout,
                                 "%s is now %d\n",
                                 str,
                                 *value);
        }
      else
        ipmipower_cbuf_printf (ttyout,
                               "invalid %s input, range is %d <=> %d\n",
                               str,
                               min,
                               max);
    }
}

static void
_cmd_set_flag (char **argv, int *flag, const char *str)
{
  assert (argv && flag && str);

  if (!argv[1])
    *flag = !(*flag);
  else
    {
      if (!strcasecmp (argv[1], "on"))
        *flag = 1;
      else if (!strcasecmp (argv[1], "off"))
        *flag = 0;
      else
        {
          ipmipower_cbuf_printf (ttyout, "invalid parameter\n");
          return;
        }
    }
  ipmipower_cbuf_printf (ttyout,
                         "%s is now %s\n",
                         str,
                         (*flag) ? "on" : "off");
}

/* _readcmd
 * - Read a command line from the tty and return it in buf.
 *   If no commands are available, return a null-terminated empty string.
 */
static void
_readcmd (char *buf, int buflen)
{
  int dropped, bytes_peeked, len = 0;

  buf[0] = '\0';
  if ((bytes_peeked = cbuf_peek (ttyin, buf, buflen)) < 0)
    {
      IPMIPOWER_ERROR (("cbuf_peek: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  if (!bytes_peeked)
    return;

  for (len = 0; len < bytes_peeked; len++)
    {
      if (buf[len] == '\n')
        {
          buf[len+1] = '\0';
          break;
        }
    }

  if (len == bytes_peeked)
    return;

  len++;

  if ((dropped = cbuf_drop (ttyin, len)) != len)
    IPMIPOWER_DEBUG (("cbuf_drop returned %d != %d)", dropped, len));
}

int
ipmipower_prompt_process_cmdline (void)
{
  static int need_prompt = 1;
  char *buf;
  int quit = 0;

  if (!(buf = (char *)malloc (IPMIPOWER_MAX_TTY_BUF)))
    {
      IPMIPOWER_ERROR (("malloc: %s", strerror(errno)));
      exit (EXIT_FAILURE);
    }

  do
    {
      if (ipmipower_powercmd_pending ())
        break;
      if (need_prompt)
        {
          ipmipower_cbuf_printf (ttyout, "ipmipower> ");
          need_prompt = 0;
        }
      buf[0] = '\0';
      _readcmd (buf, IPMIPOWER_MAX_TTY_BUF);
      if (strlen (buf) > 0)
        {
          char **argv = argv_create (buf, "");
          int i;

          if (argv[0])
            {
              /* support "ipmi_version" and "ipmi-version" for backwards compatability */
              if (!strcmp (argv[0], "driver-type")
                  || !strcmp (argv[0], "ipmi_version")
                  || !strcmp (argv[0], "ipmi-version"))
                _cmd_driver_type (argv);
              /* support hostnames (plural) for backwards compatability */
              else if (!strcmp (argv[0], "hostnames")
                       || !strcmp (argv[0], "hostname"))
                _cmd_hostname (argv);
              else if (!strcmp (argv[0], "username"))
                _cmd_username (argv);
              else if (!strcmp (argv[0], "password"))
                _cmd_password (argv);
              else if (!strcmp (argv[0], "k_g"))
                _cmd_k_g (argv);
              /* support "timeout" for backwards compatability */
              else if (!strcmp (argv[0], "timeout")
                       || !strcmp (argv[0], "session-timeout"))
                _cmd_set_unsigned_int (argv,
                                       &cmd_args.common_args.session_timeout,
                                       "timeout",
                                       0);
              /* support "retry-timeout" for backwards compatability */
              else if (!strcmp (argv[0], "retry-timeout")
                       || !strcmp (argv[0], "retransmission-timeout"))
                _cmd_set_unsigned_int_ranged (argv,
                                              &cmd_args.common_args.retransmission_timeout,
                                              "retransmission-timeout",
                                              0,
                                              1,
                                              cmd_args.common_args.session_timeout);
              /* support underscored version for backwards compatability */
              else if (!strcmp (argv[0], "authentication_type")
                       || !strcmp (argv[0], "authentication-type"))
                _cmd_authentication_type (argv);
              /* support underscored version for backwards compatability */
              else if (!strcmp (argv[0], "cipher_suite_id")
                       || !strcmp (argv[0], "cipher-suite-id"))
                _cmd_cipher_suite_id (argv);
              /* support "privilege" command for backwards compatability */
              else if (!strcmp (argv[0], "privilege")
                       || !strcmp (argv[0], "privilege-level"))
                _cmd_privilege_level (argv);
              else if (!strcmp (argv[0], "workaround-flags"))
                _cmd_workaround_flags (argv);
              else if (!strcmp (argv[0], "debug"))
                _cmd_debug (argv);
#ifndef NDEBUG
              else if (!strcmp (argv[0], "rmcpdump"))
                _cmd_set_flag (argv,
                               &cmd_args.rmcpdump,
                               "rmcp dump");
#endif /* NDEBUG */
              else if (!strcmp (argv[0], "happyeaster"))
                ipmipower_cbuf_printf (ttyout, "by Albert Chu <chu11@llnl.gov>\n");
              else if (!strcmp (argv[0], "on"))
                _cmd_power (argv, IPMIPOWER_POWER_CMD_POWER_ON);
              else if (!strcmp (argv[0], "off"))
                _cmd_power (argv, IPMIPOWER_POWER_CMD_POWER_OFF);
              else if (!strcmp (argv[0], "cycle"))
                _cmd_power (argv, IPMIPOWER_POWER_CMD_POWER_CYCLE);
              else if (!strcmp (argv[0], "reset"))
                _cmd_power (argv, IPMIPOWER_POWER_CMD_POWER_RESET);
              else if (!strcmp (argv[0], "stat"))
                _cmd_power (argv, IPMIPOWER_POWER_CMD_POWER_STATUS);
              else if (!strcmp (argv[0], "pulse"))
                _cmd_power (argv, IPMIPOWER_POWER_CMD_PULSE_DIAGNOSTIC_INTERRUPT);
              else if (!strcmp (argv[0], "soft"))
                _cmd_power (argv, IPMIPOWER_POWER_CMD_SOFT_SHUTDOWN_OS);
              else if (!strcmp (argv[0], "identify-on"))
                _cmd_power (argv, IPMIPOWER_POWER_CMD_IDENTIFY_ON);
              else if (!strcmp (argv[0], "identify-off"))
                _cmd_power (argv, IPMIPOWER_POWER_CMD_IDENTIFY_OFF);
              else if (!strcmp (argv[0], "identify-status"))
                _cmd_power (argv, IPMIPOWER_POWER_CMD_IDENTIFY_STATUS);
              else if (!strcmp (argv[0], "on-if-off"))
                _cmd_set_flag (argv,
                               &cmd_args.on_if_off,
                               "on-if-off");
              else if (!strcmp (argv[0], "wait-until-on"))
                _cmd_set_flag (argv,
                               &cmd_args.wait_until_on,
                               "wait-until-on");
              else if (!strcmp (argv[0], "wait-until-off"))
                _cmd_set_flag (argv,
                               &cmd_args.wait_until_off,
                               "wait-until-off");
              /* support "retry-wait-timeout" for backwards compatability */
              else if (!strcmp (argv[0], "retry-wait-timeout")
                       || !strcmp (argv[0], "retransmission-wait-timeout"))
                _cmd_set_unsigned_int_ranged (argv,
                                              &cmd_args.retransmission_wait_timeout,
                                              "retransmission-wait-timeout",
                                              0,
                                              1,
                                              cmd_args.common_args.session_timeout);
              /* support "retry-backoff-count" for backwards compatability */
              else if (!strcmp (argv[0], "retry-backoff-count")
                       || !strcmp (argv[0], "retransmission-backoff-count"))
                _cmd_set_unsigned_int (argv,
                                       &cmd_args.retransmission_backoff_count,
                                       "retransmission-backoff-count",
                                       0);
              else if (!strcmp (argv[0], "ping-interval"))
                _cmd_set_unsigned_int_ranged (argv,
                                              &cmd_args.ping_interval,
                                              "ping-interval",
                                              1,
                                              0,
                                              cmd_args.ping_timeout);
              else if (!strcmp (argv[0], "ping-timeout"))
                _cmd_set_unsigned_int (argv,
                                       &cmd_args.ping_timeout,
                                       "ping-timeout",
                                       1);
              else if (!strcmp (argv[0], "ping-packet-count"))
                _cmd_set_unsigned_int (argv,
                                       &cmd_args.ping_packet_count,
                                       "ping-packet-count",
                                       1);
              else if (!strcmp (argv[0], "ping-percent"))
                _cmd_set_unsigned_int (argv,
                                       &cmd_args.ping_percent,
                                       "ping-percent",
                                       1);
              else if (!strcmp (argv[0], "ping-consec-count"))
                _cmd_set_unsigned_int_ranged (argv,
                                              &cmd_args.ping_consec_count,
                                              "ping-consec-count",
                                              1,
                                              0,
                                              cmd_args.ping_packet_count);
              else if (!strcmp (argv[0], "buffer-output"))
                _cmd_set_flag (argv,
                               &cmd_args.common_args.buffer_output,
                               "buffer-output");
              else if (!strcmp (argv[0], "consolidate-output"))
                _cmd_set_flag (argv,
                               &cmd_args.common_args.consolidate_output,
                               "consolidate-output");
              else if (!strcmp (argv[0], "always-prefix"))
                _cmd_set_flag (argv,
                               &cmd_args.common_args.always_prefix,
                               "always-prefix");
              else if (!strcmp (argv[0], "fanout"))
                _cmd_set_unsigned_int_ranged (argv,
                                              &cmd_args.common_args.fanout,
                                              "fanout",
                                              1,
                                              PSTDOUT_FANOUT_MIN,
                                              PSTDOUT_FANOUT_MAX);
              else if (!strcmp (argv[0], "help")
                       || !strcmp (argv[0], "?")
                       || !strcmp (argv[0], "advanced") /* legacy */
                       || !strcmp (argv[0], "network")) /* legacy */
                _cmd_help ();
              else if (!strcmp (argv[0], "version"))
                _cmd_version ();
              else if (!strcmp (argv[0], "config"))
                _cmd_config ();
              else if (!strcmp (argv[0], "quit"))
                quit = 1;
              else
                ipmipower_cbuf_printf (ttyout, "unknown command - type \"help\"\n");
            }
          need_prompt = 1;

          /* Clear out argv data for generic security purposes since
           * usernames or passwords could be stored here.  argv_create
           * guarantees a null terminated pointer, so this loop is
           * safe
           */
          i = 0;
          while(argv[i])
            {
              memset (argv[i], '\0', strlen (argv[i]));
              i++;
            }

          argv_destroy (argv);
        }
    } while (!quit && strlen (buf) > 0);
  free (buf);

  return (!quit);
}
