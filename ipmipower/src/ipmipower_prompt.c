/*****************************************************************************\
 *  $Id: ipmipower_prompt.c,v 1.90 2008-05-19 18:47:19 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2008 Lawrence Livermore National Security, LLC.
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
 *  Free Software Foundation; either version 2 of the License, or (at your 
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

#include "ipmipower_config.h"
#include "ipmipower_prompt.h"
#include "ipmipower_ping.h"
#include "ipmipower_connection.h"
#include "ipmipower_powercmd.h"
#include "ipmipower_output.h"
#include "ipmipower_wrappers.h"

#include "freeipmi-portability.h"
#include "pstdout.h"
#include "tool-common.h"
#include "tool-cmdline-common.h"

extern cbuf_t ttyout;
extern cbuf_t ttyin;    
extern cbuf_t ttyerr;
extern struct ipmipower_arguments args;
extern struct ipmipower_connection *ics;
extern unsigned int ics_len;

/* eliminate
 *
 * Note: only for non-interactive mode on the command line.  Won't be 
 * anywhere in this file.
 */

static void 
_cmd_driver_type(char **argv) 
{
  assert(argv);

  if (argv[1]) 
    {
      int tmp;
      
      if ((tmp = parse_outofband_driver_type(argv[1])) < 0)
        cbuf_printf(ttyout, "invalid driver type '%s' specified\n", argv[1]);
      else
        {
          args.common.driver_type = tmp;
          cbuf_printf(ttyout, "driver type is now %s\n", argv[1]);
        }
    }
  else
    cbuf_printf(ttyout, "driver_type must be specified: %s, %s\n",
                IPMI_DEVICE_LAN_STR,
                IPMI_DEVICE_LAN_2_0_STR);
}

static void 
_cmd_hostname(char **argv) 
{
  assert(argv);

  if (!argv[1]) 
    {
      if (args.common.hostname)
        {
          free(args.common.hostname);
          args.common.hostname = NULL;
        }
 
      ipmipower_connection_array_destroy(ics, ics_len);
      ics = NULL;
      ics_len = 0;

      cbuf_printf(ttyout, "hostname(s) unconfigured\n");
    }
  else 
    {
      struct ipmipower_connection *icsPtr;
      unsigned int len = 0;

      if (!(icsPtr = ipmipower_connection_array_create(argv[1], &len))) 
        {
          /* dump error outputs here, most notably invalid hostname output */
          if (cbuf_read_to_fd(ttyout, STDOUT_FILENO, -1) > 0)
            return;
          else
            cbuf_printf(ttyout, "ipmipower_connection_array_create: %s\n", strerror(errno));
          return;
        }
      
      if (args.common.hostname)
        {
          free(args.common.hostname);
          args.common.hostname = NULL;
        }

      ipmipower_connection_array_destroy(ics, ics_len);
      ics = icsPtr;
      ics_len = len;

      if (!(args.common.hostname = strdup(argv[1])))
        ierr_exit("strdup: %s", strerror(errno));

      ipmipower_ping_force_discovery_sweep();
      
      cbuf_printf(ttyout, "hostname: %s\n", args.common.hostname);
    }
}

static void 
_cmd_username(char **argv)
{
  assert(argv);

  if (!argv[1]
      || (argv[1] && strlen(argv[1]) <= IPMI_MAX_USER_NAME_LENGTH)) 
    {
      if (args.common.username)
        {
          free(args.common.username);
          args.common.username = NULL;
        }

      if (argv[1])
        {
          if (!(args.common.username = strdup(argv[1])))
            ierr_exit("strdup: %s", strerror(errno));
        }
      
      cbuf_printf(ttyout, "username: %s\n", 
                  (args.common.username) ? args.common.username : "NULL");
    }
  else
    cbuf_printf(ttyout, "username invalid length\n");
}

static void 
_cmd_password(char **argv) 
{
  assert(argv);

  if (argv[1] && args.common.authentication_type == IPMI_AUTHENTICATION_TYPE_NONE)
    cbuf_printf(ttyout, "password cannot be set for authentication_type '%s'\n",
                IPMI_AUTHENTICATION_TYPE_NONE_STR);
  else if (!argv[1]
           || (argv[1] 
               && ((args.common.driver_type == IPMI_DEVICE_LAN_2_0
                    && strlen(argv[1]) <= IPMI_2_0_MAX_PASSWORD_LENGTH)
                   || (args.common.driver_type == IPMI_DEVICE_LAN
                       && strlen(argv[1]) <= IPMI_1_5_MAX_PASSWORD_LENGTH))))
    {
      if (args.common.password)
        {
          free(args.common.password);
          args.common.password = NULL;
        }

      if (argv[1])
        {
          if (!(args.common.password = strdup(argv[1])))
            ierr_exit("strdup: %s", strerror(errno));
        }

#ifdef NDEBUG
      cbuf_printf(ttyout, "password changed\n");
#else  /* !NDEBUG */
      cbuf_printf(ttyout, "password: %s\n", 
                  (args.common.password) ? args.common.password : "NULL");
#endif /* !NDEBUG */
    }
  else
    cbuf_printf(ttyout, "password invalid length\n");
}

static void 
_cmd_k_g(char **argv) 
{
  int rv = 0;
  char buf[IPMI_MAX_K_G_LENGTH*2+3];
  assert(argv);

  if (args.common.driver_type == IPMI_DEVICE_LAN)
    cbuf_printf(ttyout, "k_g is only used for IPMI 2.0");
  else
    {
      memset(args.common.k_g, '\0', IPMI_MAX_K_G_LENGTH);

      if (argv[1])
        rv = parse_kg(args.common.k_g, IPMI_MAX_K_G_LENGTH + 1, argv[1]);
      
      if (rv < 0)
        cbuf_printf(ttyout, "k_g invalid\n");
      else
        {
          args.common.k_g_len = rv;
#ifdef NDEBUG
          cbuf_printf(ttyout, "k_g changed\n");
#else  /* !NDEBUG */
          cbuf_printf(ttyout, "k_g: %s\n", 
                      (args.common.k_g_len) ? format_kg(buf, IPMI_MAX_K_G_LENGTH*2+3, args.common.k_g) : "NULL");
#endif /* !NDEBUG */
        }
    }
}

static void 
_cmd_authentication_type(char **argv) 
{
  assert(argv);

  if (argv[1]) 
    {
      int tmp;

      if ((tmp = parse_authentication_type(argv[1])) < 0)
        cbuf_printf(ttyout, "%s invalid authentication_type\n", argv[1]);
      else
        {
          args.common.authentication_type = tmp;
          cbuf_printf(ttyout, "authentication type is now %s\n", argv[1]);
        }
    }
  else
    cbuf_printf(ttyout, "authentication_type must be specified: %s, %s, %s, %s\n",
                IPMI_AUTHENTICATION_TYPE_NONE_STR,
                IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY_STR,
                IPMI_AUTHENTICATION_TYPE_MD2_STR,
                IPMI_AUTHENTICATION_TYPE_MD5_STR);
}

static void 
_cmd_cipher_suite_id(char **argv) 
{
  assert(argv);

  if (argv[1]) 
    {
      char *ptr;
      int tmp;

      tmp = strtol(argv[1], &ptr, 10);
      if (ptr != (argv[1] + strlen(argv[1]))
          || tmp < IPMI_CIPHER_SUITE_ID_MIN
          || tmp > IPMI_CIPHER_SUITE_ID_MAX)
        cbuf_printf(ttyout, "%s invalid cipher suite id\n", argv[1]);
      else if (!IPMI_CIPHER_SUITE_ID_SUPPORTED(tmp))
        cbuf_printf(ttyout, "%s unsupported cipher suite id\n", argv[1]);
      else
        {
          args.common.cipher_suite_id = tmp;
          cbuf_printf(ttyout, "cipher suite id is now %s\n", argv[1]);
        }
    }
  else
    cbuf_printf(ttyout, "cipher_suite_id must be specified: 0, 1, 2, 3, 6, 7, 8, 11, 12\n");
}

static void 
_cmd_privilege_level(char **argv) 
{
  assert(argv);

  if (argv[1]) 
    {
      int tmp;

      if ((tmp = parse_privilege_level(argv[1])) < 0)
        cbuf_printf(ttyout, "%s invalid privilege_level\n", argv[1]);
      else
        {
          args.common.authentication_type = tmp;
          cbuf_printf(ttyout, "privilege_level type is now %s\n", argv[1]);
        }
    }
  else
    cbuf_printf(ttyout, "privilege must be specified: %s, %s, %s\n",
                IPMI_PRIVILEGE_LEVEL_USER_STR,
                IPMI_PRIVILEGE_LEVEL_OPERATOR_STR,
                IPMI_PRIVILEGE_LEVEL_ADMIN_STR);
}

static void
_cmd_workaround_flags(char **argv)
{
  assert(argv);

  if (argv[1]) 
    {
      int tmp;

      if ((tmp = parse_workaround_flags(argv[1])) < 0)
        cbuf_printf(ttyout, "%s invalid workaround flags specified\n", argv[1]);
      else 
        {
          args.common.workaround_flags = tmp;
          cbuf_printf(ttyout, "workaround flags are now %s\n", argv[1]);
        }
    }
  else
    cbuf_printf(ttyout, "workaround_flags must be specified: %s,%s,%s,%s,%s,%s,%s,%s\n",
                IPMI_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO_STR,
                IPMI_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION_STR,
                IPMI_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE_STR,
                IPMI_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER_STR,
                IPMI_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES_STR,
                IPMI_WORKAROUND_FLAGS_INTEL_2_0_SESSION_STR,
                IPMI_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION_STR,
                IPMI_WORKAROUND_FLAGS_SUN_2_0_SESSION_STR);
}

static void 
_cmd_power(char **argv, power_cmd_t cmd) 
{
  int i;

  assert(argv && POWER_CMD_VALID(cmd));

  if (!args.common.hostname) 
    {
      cbuf_printf(ttyout, "no hostname(s) configured\n");
      return;
    }

  /* Check for correct privilege type */
  if (args.common.privilege_level == IPMI_PRIVILEGE_LEVEL_USER 
      && POWER_CMD_REQUIRES_OPERATOR_PRIVILEGE_LEVEL(cmd))
    {
      cbuf_printf(ttyout, "power operation requires atleast operator privilege\n");
      return;
    }

  if (!argv[1])  /* all nodes */
    { 
      int nodes_queued = 0;
      
      for (i = 0; i < ics_len; i++) 
        {
          if (args.ping_interval 
              && ics[i].discover_state == STATE_UNDISCOVERED)
            ipmipower_output(MSG_TYPE_NOTDISCOVERED, ics[i].hostname);
          else if (args.ping_interval 
                   && args.ping_packet_count 
                   && args.ping_percent 
                   && ics[i].discover_state == STATE_BADCONNECTION)
            ipmipower_output(MSG_TYPE_BADCONNECTION, ics[i].hostname);
          else 
            {
              ipmipower_connection_clear(&ics[i]);
              ipmipower_powercmd_queue(cmd, &ics[i]);
              nodes_queued++;
            }
        }

      /* Special corner case when no nodes are discovered */
      if (!nodes_queued)
        ipmipower_output_finish();
    } 
  else 
    {
      hostlist_t h;
      hostlist_iterator_t itr;
      char *node;

      if (!(h = hostlist_create(argv[1]))) 
        {
          cbuf_printf(ttyout, "invalid hostname(s) specified");
          return;
        }

      if (!(itr = hostlist_iterator_create(h)))
        ierr_exit("hostlist_iterator_create() error");

      while ((node = hostlist_next(itr)))
        {
          i = ipmipower_connection_hostname_index(ics, ics_len, node);

          if (i < 0)
            ipmipower_output(MSG_TYPE_UNCONFIGURED_HOSTNAME, node);
          else if (args.ping_interval 
                   && ics[i].discover_state == STATE_UNDISCOVERED)
            ipmipower_output(MSG_TYPE_NOTDISCOVERED, ics[i].hostname);
          else if (args.ping_interval 
                   && args.ping_packet_count 
                   && args.ping_percent 
                   && ics[i].discover_state == STATE_BADCONNECTION)
            ipmipower_output(MSG_TYPE_BADCONNECTION, ics[i].hostname);
          else 
            {
              ipmipower_connection_clear(&ics[i]);
              ipmipower_powercmd_queue(cmd, &ics[i]);
            }
          free(node);
        }

      hostlist_iterator_destroy(itr);
      hostlist_destroy(h);
    }
}

static void 
_cmd_help(void) 
{
  cbuf_printf(ttyout, 
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
  cbuf_printf(ttyout,
              "rmcpdump [on|off]                        - Toggle RMCP dump output.\n");
#endif /* NDEBUG */
  cbuf_printf(ttyout,
              "on [IPMIHOST(s)]                         - Turn on all configured hosts or specified hosts.\n"
              "off [IPMIHOST(s)]                        - Turn off all configured hosts or specified hosts.\n"
              "cycle [IPMIHOST(s)]                      - Power cycle all configured hosts or specified hosts.\n"
              "reset [IPMIHOST(s)]                      - Reset all configured hosts or specified hosts.\n"
              "stat [IPMIHOST(s)]                       - Query power status for all configured hosts or specified hosts.\n"
              "pulse [IPMIHOST(s)]                      - Pulse diagnostic interrupt all configured hosts or specified hosts.\n"
              "soft [IPMIHOST(s)]                       - Initiate a soft-shutdown for all configured hosts or specified hosts.\n"
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
	      "buffer-output [on|off]                   - Toggle buffer-output functionality\n"
              "consolidate-output [on|off]              - Toggle consolidate-output functionality.\n"
              "fanout COUNT                             - Specify a fanout.\n"
              "always-prefix [on|off]                   - Toggle always-prefix functionality.\n"
              "help                                     - Output help menu.\n"
              "version                                  - Output version.\n"
              "config                                   - Output current configuration.\n"
              "quit                                     - Quit program.\n");
}

static void 
_cmd_version(void) 
{
  cbuf_printf(ttyout, "ipmipower %s\n", VERSION);
}

static void
_workarounds_strcat(char *strbuf, unsigned int mask, char *str, int *is_first)
{
  assert(strbuf && str && is_first);
  
  if (args.common.workaround_flags & mask)
    {
      if ((*is_first))
        strcat(strbuf, ",");
      strcat(strbuf, str);
      (*is_first)++;
    }
}

static void 
_cmd_debug(char **argv) 
{
  assert(argv);
  
  if (!argv[1]) 
    {
      if (args.common.flags & IPMI_FLAGS_DEBUG_DUMP)
        args.common.flags = args.common.flags & ~IPMI_FLAGS_DEBUG_DUMP;
      else
        args.common.flags = args.common.flags |= IPMI_FLAGS_DEBUG_DUMP;
    }
  else 
    {
      if (!strcasecmp(argv[1], "on"))
        args.common.flags = args.common.flags |= IPMI_FLAGS_DEBUG_DUMP;
      else if (!strcasecmp(argv[1], "off"))  
        args.common.flags = args.common.flags & ~IPMI_FLAGS_DEBUG_DUMP;
      else 
        {
          cbuf_printf(ttyout, "invalid parameter\n");
          return;
        }
    }
  cbuf_printf(ttyout, "debugging is now %s\n", (args.common.flags & IPMI_FLAGS_DEBUG_DUMP) ? "on" : "off");
}

static void 
_cmd_config(void) 
{
  char kgbuf[IPMI_MAX_K_G_LENGTH*2+3];
  char strbuf[IPMIPOWER_OUTPUT_BUFLEN];
  char *str;
  int is_first = 0;

  str = "";
  if (args.common.driver_type == IPMI_DEVICE_LAN)
    str = IPMI_DEVICE_LAN_STR;
  else if (args.common.driver_type == IPMI_DEVICE_LAN_2_0)
    str = IPMI_DEVICE_LAN_2_0_STR;

  cbuf_printf(ttyout, "Driver_Type:                  %s\n", str);

  if (args.common.hostname) 
    {
      char buffer[IPMIPOWER_OUTPUT_BUFLEN];
#ifndef NDEBUG
      int i;
      hostlist_t discovered = NULL;
      hostlist_t undiscovered = NULL;
      hostlist_t badconnection = NULL;
#endif /* NDEBUG */
      int rv;

      cbuf_printf(ttyout, "Hostname:                     %s\n", args.common.hostname);

#ifndef NDEBUG
      if (!(discovered = hostlist_create(NULL)))
        goto cleanup;
      if (!(undiscovered = hostlist_create(NULL)))
        goto cleanup;
      if (!(badconnection = hostlist_create(NULL)))
        goto cleanup;
      
      for (i = 0; i < ics_len; i++) {
        if (ics[i].discover_state == STATE_DISCOVERED)
          rv = hostlist_push_host(discovered, ics[i].hostname);
        else if (ics[i].discover_state == STATE_UNDISCOVERED)
          rv = hostlist_push_host(undiscovered, ics[i].hostname);
        else
          rv = hostlist_push_host(badconnection, ics[i].hostname);
        
        if (!rv)
          goto cleanup;
      }
      
      if ((rv = hostlist_ranged_string(discovered, IPMIPOWER_OUTPUT_BUFLEN, buffer)) < 0)
        ierr_exit("hostlist_ranged_string: %s", strerror(errno));
      if (rv > 0)
        cbuf_printf(ttyout, "Discovered:                   %s\n", buffer);
      
      if ((rv = hostlist_ranged_string(undiscovered, IPMIPOWER_OUTPUT_BUFLEN, buffer)) < 0)
        ierr_exit("hostlist_ranged_string: %s", strerror(errno));
      if (rv > 0)
        cbuf_printf(ttyout, "Undiscovered:                 %s\n", buffer);
      
      if ((rv = hostlist_ranged_string(badconnection, IPMIPOWER_OUTPUT_BUFLEN, buffer)) < 0)
        ierr_exit("hostlist_ranged_string: %s", strerror(errno));
      if (rv > 0)
        cbuf_printf(ttyout, "BadConnection:                %s\n", buffer);

    cleanup:
      hostlist_destroy(discovered);
      hostlist_destroy(undiscovered);
      hostlist_destroy(badconnection);
#endif /* NDEBUG */
    }
  else
    cbuf_printf(ttyout, "Hostname:                     NONE\n");

  cbuf_printf(ttyout, "Username:                     %s\n", 
              (args.common.username) ? args.common.username : "NULL");

#ifndef NDEBUG
  cbuf_printf(ttyout, "Password:                     %s\n", 
              (args.common.password) ? args.common.password : "NULL");
  cbuf_printf(ttyout, "K_g:                          %s\n", 
              (args.common.k_g_len) ? 
              format_kg(kgbuf, IPMI_MAX_K_G_LENGTH*2+3, args.common.k_g) : "NULL");
#else  /* !NDEBUG */
  cbuf_printf(ttyout, "Password:                     *****\n");
  cbuf_printf(ttyout, "K_g:                          *****\n");
#endif /* !NDEBUG */

  cbuf_printf(ttyout, "Session Timeout:              %u ms\n", 
              args.common.session_timeout);
  cbuf_printf(ttyout, "Retransmission Timeout:       %u ms\n", 
              args.common.retransmission_timeout);

  str = "";
  if (args.common.authentication_type == IPMI_AUTHENTICATION_TYPE_NONE)
    str = IPMI_AUTHENTICATION_TYPE_NONE_STR;
  else if (args.common.authentication_type == IPMI_AUTHENTICATION_TYPE_MD2)
    str = IPMI_AUTHENTICATION_TYPE_MD2_STR;
  else if (args.common.authentication_type == IPMI_AUTHENTICATION_TYPE_MD5)
    str = IPMI_AUTHENTICATION_TYPE_MD5_STR;
  else if (args.common.authentication_type == IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY)
    str = IPMI_AUTHENTICATION_TYPE_STRAIGHT_PASSWORD_KEY_STR;

  cbuf_printf(ttyout, "Authentication_Type:          %s\n", str);

  str = "";
  if (args.common.cipher_suite_id == 0)
    str = "0";
  else if (args.common.cipher_suite_id == 1)
   str = "1";
  else if (args.common.cipher_suite_id == 2)
    str = "2";
  else if (args.common.cipher_suite_id == 3)
    str = "3";
  else if (args.common.cipher_suite_id == 6)
    str = "6";
  else if (args.common.cipher_suite_id == 7)
    str = "7";
  else if (args.common.cipher_suite_id == 8)
    str = "8";
  else if (args.common.cipher_suite_id == 11)
    str = "11";
  else if (args.common.cipher_suite_id == 12)
    str = "12";

  cbuf_printf(ttyout, "Cipher Suite Id:              %s\n", str);

  str = "";
  if (args.common.privilege_level == IPMI_PRIVILEGE_LEVEL_USER)
    str = IPMI_PRIVILEGE_LEVEL_USER_STR;
  else if (args.common.privilege_level == IPMI_PRIVILEGE_LEVEL_OPERATOR)
    str = IPMI_PRIVILEGE_LEVEL_OPERATOR_STR;
  else if (args.common.privilege_level == IPMI_PRIVILEGE_LEVEL_ADMIN)
    str = IPMI_PRIVILEGE_LEVEL_ADMIN_STR;

  cbuf_printf(ttyout, "Privilege_Level:              %s\n", str);

  memset(strbuf, '\0', IPMIPOWER_OUTPUT_BUFLEN);
  is_first = 0;
  _workarounds_strcat(strbuf, 
                      IPMI_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO,
                      IPMI_WORKAROUND_FLAGS_ACCEPT_SESSION_ID_ZERO_STR,
                      &is_first);
  _workarounds_strcat(strbuf, 
                      IPMI_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION,
                      IPMI_WORKAROUND_FLAGS_FORCE_PERMSG_AUTHENTICATION_STR,
                      &is_first);
  _workarounds_strcat(strbuf, 
                      IPMI_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE,
                      IPMI_WORKAROUND_FLAGS_CHECK_UNEXPECTED_AUTHCODE_STR,
                      &is_first);
  _workarounds_strcat(strbuf, 
                      IPMI_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER,
                      IPMI_WORKAROUND_FLAGS_BIG_ENDIAN_SEQUENCE_NUMBER_STR,
                      &is_first);
  _workarounds_strcat(strbuf, 
                      IPMI_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES,
                      IPMI_WORKAROUND_FLAGS_AUTHENTICATION_CAPABILITIES_STR,
                      &is_first);
  _workarounds_strcat(strbuf, 
                      IPMI_WORKAROUND_FLAGS_INTEL_2_0_SESSION,
                      IPMI_WORKAROUND_FLAGS_INTEL_2_0_SESSION_STR,
                      &is_first);
  _workarounds_strcat(strbuf, 
                      IPMI_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION,
                      IPMI_WORKAROUND_FLAGS_SUPERMICRO_2_0_SESSION_STR,
                      &is_first);
  _workarounds_strcat(strbuf, 
                      IPMI_WORKAROUND_FLAGS_SUN_2_0_SESSION,
                      IPMI_WORKAROUND_FLAGS_SUN_2_0_SESSION_STR,
                      &is_first);

  cbuf_printf(ttyout, "WorkaroundFlags:              %s\n", strbuf);

  cbuf_printf(ttyout, "Debug:                        %s\n", 
              (args.common.flags & IPMI_FLAGS_DEBUG_DUMP) ? "on" : "off");

#ifndef NDEBUG
  cbuf_printf(ttyout, "Rmcpdump:                     %s\n", 
              (args.rmcpdump) ? "on" : "off");
#endif /* NDEBUG */

  cbuf_printf(ttyout, "On-If-Off:                    %s\n",
              (args.on_if_off) ? "enabled" : "disabled");
  cbuf_printf(ttyout, "Wait-Until-On:                %s\n",
              (args.wait_until_on) ? "enabled" : "disabled");
  cbuf_printf(ttyout, "Wait-Until-Off:               %s\n",
              (args.wait_until_off) ? "enabled" : "disabled");
  cbuf_printf(ttyout, "Retransmission Wait Timeout:  %u ms\n", 
              args.retransmission_wait_timeout);
  cbuf_printf(ttyout, "Retransmission Backoff Count: %u\n", 
              args.retransmission_backoff_count);
  cbuf_printf(ttyout, "Ping Interval:                %u ms\n",
              args.ping_interval);
  cbuf_printf(ttyout, "Ping Timeout:                 %u ms\n", 
              args.ping_timeout);
  cbuf_printf(ttyout, "Ping Packet Count:            %u\n", 
              args.ping_packet_count);
  cbuf_printf(ttyout, "Ping Percent:                 %u percent\n", 
              args.ping_percent);
  cbuf_printf(ttyout, "Ping Consec Count:            %u\n", 
              args.ping_consec_count);

  cbuf_printf(ttyout, "Buffer-Output:                %s\n",
              (args.hostrange.buffer_hostrange_output) ? "enabled" : "disabled");
  cbuf_printf(ttyout, "Consolidate-Output:           %s\n",
              (args.hostrange.consolidate_hostrange_output) ? "enabled" : "disabled");
  cbuf_printf(ttyout, "Fanout:                       %u\n",
	      args.hostrange.fanout);
  cbuf_printf(ttyout, "Always-Prefix:                %s\n",
              (args.hostrange.always_prefix) ? "enabled" : "disabled");
}

static void 
_cmd_set_unsigned_int(char **argv, 
                      unsigned int *val, 
                      char *str, 
                      int allow_zero)
{
  assert(argv && val && str);

  if (!argv[1])
    cbuf_printf(ttyout, "%s not specified\n", str);
  else 
    {
      char *ptr;
      unsigned int temp = strtoul(argv[1], &ptr, 10);
      if (ptr != (argv[1] + strlen(argv[1]))) 
        cbuf_printf(ttyout, "invalid %s input\n", str);
      else if (allow_zero && !temp)
        {
          *val = temp;
          cbuf_printf(ttyout, "%s is now %d\n", str, *val);
        }
      else
        cbuf_printf(ttyout, "invalid %s input\n", str);
    }
}

static void 
_cmd_set_unsigned_int_ranged(char **argv, 
                             unsigned int *val, 
                             char *str, 
                             int allow_zero,
                             int min, 
                             int max) 
{
  assert(argv && val && str);

  if (!argv[1])
    cbuf_printf(ttyout, "%s not specified\n", str);
  else 
    {
      char *ptr;
      int temp = strtol(argv[1], &ptr, 10);
      if (ptr != (argv[1] + strlen(argv[1]))) 
        cbuf_printf(ttyout, "invalid %s input\n", str);
      else if ((allow_zero && !temp) || (temp <= max && temp >= min)) {
        *val = temp;
        cbuf_printf(ttyout, "%s is now %d\n", str, *val);
      }
      else
        cbuf_printf(ttyout, "invalid %s input, range is %d <=> %d\n", 
                    str, min, max);
    }
}

static void 
_cmd_set_flag(char **argv, int *flag, char *str) 
{
  assert(argv && flag && str);
  
  if (!argv[1]) 
    *flag = !(*flag);
  else 
    {
      if (!strcasecmp(argv[1], "on"))
        *flag = 1;
      else if (!strcasecmp(argv[1], "off"))  
        *flag = 0;
      else 
        {
          cbuf_printf(ttyout, "invalid parameter\n");
          return;
        }
    }
  cbuf_printf(ttyout, "%s is now %s\n", str, (*flag) ? "on" : "off");
}

/* _readcmd
 * - Read a command line from the tty and return it in buf.
 *   If no commands are available, return a null-terminated empty string.
 */
static void 
_readcmd(char *buf, int maxlen) 
{
  int dropped, bytes_peeked, len = 0; 

  /* Don't use Cbuf_peek(), we may not want to cbuf_drop data */
  buf[0] = '\0';
  if ((bytes_peeked = cbuf_peek(ttyin, buf, maxlen)) <= 0) 
    {
      if (bytes_peeked < 0)
        ierr_exit("_readcmd: cbuf_peek returned %d", bytes_peeked);
      return;
    }

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
  if ((dropped = cbuf_drop(ttyin, ++len)) != len)
    ierr_dbg("warning: _readcmd: cbuf_drop returned %d (!= %d)", dropped, len);
}

int 
ipmipower_prompt_process_cmdline(void) 
{
  static int need_prompt = 1;
  char *buf;
  int quit = 0;

  buf = (char *)Malloc(IPMIPOWER_MAX_TTY_BUF);
  do 
    {
      if (ipmipower_powercmd_pending())
        break;
      if (need_prompt) 
        {
          cbuf_printf(ttyout, "ipmipower> ");
          need_prompt = 0;
        }
      buf[0] = '\0';
      _readcmd(buf, IPMIPOWER_MAX_TTY_BUF);
      if (strlen(buf) > 0) 
        {
          char **argv = argv_create(buf, "");
          int i;

          if (argv[0]) 
            {
              /* support "ipmi_version" and "ipmi-version" for backwards compatability */
              if (!strcmp(argv[0], "driver-type")
                  || !strcmp(argv[0], "ipmi_version")
                  || !strcmp(argv[0], "ipmi-version"))
                _cmd_driver_type(argv);
              /* support hostnames (plural) for backwards compatability */
              else if (!strcmp(argv[0], "hostnames")
                       || !strcmp(argv[0], "hostname"))
                _cmd_hostname(argv);
              else if (!strcmp(argv[0], "username"))
                _cmd_username(argv); 
              else if (!strcmp(argv[0], "password"))
                _cmd_password(argv);
              else if (!strcmp(argv[0], "k_g"))
                _cmd_k_g(argv);
              /* support "timeout" for backwards compatability */
              else if (!strcmp(argv[0], "timeout")
                       || !strcmp(argv[0], "session-timeout"))
                _cmd_set_unsigned_int(argv, 
                                      &args.common.session_timeout, 
                                      "timeout",
                                      0);
              /* support "retry-timeout" for backwards compatability */
              else if (!strcmp(argv[0], "retry-timeout")
                       || !strcmp(argv[0], "retransmission-timeout"))
                _cmd_set_unsigned_int_ranged(argv, 
                                             &args.common.retransmission_timeout, 
                                             "retransmission-timeout", 
                                             0,
                                             1, 
                                             args.common.session_timeout);
              /* support underscored version for backwards compatability */
              else if (!strcmp(argv[0], "authentication_type")
                       || !strcmp(argv[0], "authentication-type"))
                _cmd_authentication_type(argv);
              /* support underscored version for backwards compatability */
              else if (!strcmp(argv[0], "cipher_suite_id")
                       || !strcmp(argv[0], "cipher-suite-id"))
                _cmd_cipher_suite_id(argv);
              /* support "privilege" command for backwards compatability */
              else if (!strcmp(argv[0], "privilege")
                       || !strcmp(argv[0], "privilege-level"))
                _cmd_privilege_level(argv);
              else if (!strcmp(argv[0], "workaround-flags"))
                _cmd_workaround_flags(argv);
              else if (!strcmp(argv[0], "debug")) 
                {
                  _cmd_debug(argv);
                  ierr_cbuf((args.common.flags & IPMI_FLAGS_DEBUG_DUMP), ttyerr);
                  ierr_cbuf_dump_file_stream((args.common.flags & IPMI_FLAGS_DEBUG_DUMP), stderr);
                }
#ifndef NDEBUG
              else if (!strcmp(argv[0], "rmcpdump"))
                _cmd_set_flag(argv, 
                              &args.rmcpdump,
                              "rmcp dump");
#endif /* NDEBUG */
              else if (!strcmp(argv[0], "happyeaster"))
                cbuf_printf(ttyout, "by Albert Chu <chu11@llnl.gov>\n");
              else if (!strcmp(argv[0], "on"))
                _cmd_power(argv, POWER_CMD_POWER_ON);
              else if (!strcmp(argv[0], "off"))
                _cmd_power(argv, POWER_CMD_POWER_OFF);
              else if (!strcmp(argv[0], "cycle"))
                _cmd_power(argv, POWER_CMD_POWER_CYCLE);
              else if (!strcmp(argv[0], "reset"))
                _cmd_power(argv, POWER_CMD_POWER_RESET);
              else if (!strcmp(argv[0], "stat"))
                _cmd_power(argv, POWER_CMD_POWER_STATUS);
              else if (!strcmp(argv[0], "pulse"))
                _cmd_power(argv, POWER_CMD_PULSE_DIAG_INTR);
              else if (!strcmp(argv[0], "soft"))
                _cmd_power(argv, POWER_CMD_SOFT_SHUTDOWN_OS);
              else if (!strcmp(argv[0], "on-if-off"))
                _cmd_set_flag(argv,
                              &args.on_if_off, 
                              "on-if-off");
              else if (!strcmp(argv[0], "wait-until-on"))
                _cmd_set_flag(argv,
                              &args.wait_until_on, 
                              "wait-until-on");
              else if (!strcmp(argv[0], "wait-until-off"))
                _cmd_set_flag(argv,
                              &args.wait_until_off,
                              "wait-until-off");
              /* support "retry-wait-timeout" for backwards compatability */
              else if (!strcmp(argv[0], "retry-wait-timeout")
                       || !strcmp(argv[0], "retransmission-wait-timeout"))
                _cmd_set_unsigned_int_ranged(argv, 
                                             &args.retransmission_wait_timeout, 
                                             "retransmission-wait-timeout", 
                                             0,
                                             1, 
                                             args.common.session_timeout);
              /* support "retry-backoff-count" for backwards compatability */
              else if (!strcmp(argv[0], "retry-backoff-count")
                       || !strcmp(argv[0], "retransmission-backoff-count"))
                _cmd_set_unsigned_int(argv, 
                                      &args.retransmission_backoff_count, 
                                      "retransmission-backoff-count", 
                                      0);
              else if (!strcmp(argv[0], "ping-interval"))
                _cmd_set_unsigned_int_ranged(argv,
                                             &args.ping_interval, 
                                             "ping-interval", 
                                             1, 
                                             0,
                                             args.ping_timeout);
              else if (!strcmp(argv[0], "ping-timeout"))
                _cmd_set_unsigned_int(argv, 
                                      &args.ping_timeout, 
                                      "ping-timeout",
                                      1);
              else if (!strcmp(argv[0], "ping-packet-count"))
                _cmd_set_unsigned_int(argv, 
                                      &args.ping_packet_count, 
                                      "ping-packet-count",
                                      1);
              else if (!strcmp(argv[0], "ping-percent"))
                _cmd_set_unsigned_int(argv,
                                      &args.ping_percent,
                                      "ping-percent", 
                                      1);
              else if (!strcmp(argv[0], "ping-consec-count"))
                _cmd_set_unsigned_int_ranged(argv,
                                             &args.ping_consec_count, 
                                             "ping-consec-count", 
                                             1, 
                                             0, 
                                             args.ping_packet_count);
	      else if (!strcmp(argv[0], "buffer-output"))
		_cmd_set_flag(argv,
			      &args.hostrange.buffer_hostrange_output,
			      "buffer-output");
              else if (!strcmp(argv[0], "consolidate-output"))
                _cmd_set_flag(argv, 
                              &args.hostrange.consolidate_hostrange_output, 
                              "consolidate-output");
              else if (!strcmp(argv[0], "always-prefix"))
                _cmd_set_flag(argv, 
                              &args.hostrange.always_prefix, 
                              "always-prefix");
	      else if (!strcmp(argv[0], "fanout"))
                _cmd_set_unsigned_int_ranged(argv, 
                                             &args.hostrange.fanout, 
                                             "fanout",
                                             1, 
                                             PSTDOUT_FANOUT_MIN, 
                                             PSTDOUT_FANOUT_MAX);
              else if (!strcmp(argv[0], "help") 
                       || !strcmp(argv[0], "?")
		       || !strcmp(argv[0], "advanced") /* legacy */
		       || !strcmp(argv[0], "network")) /* legacy */
                _cmd_help();
              else if (!strcmp(argv[0], "version"))
                _cmd_version();
              else if (!strcmp(argv[0], "config"))
                _cmd_config();
              else if (!strcmp(argv[0], "quit"))
                quit = 1;
              else
                cbuf_printf(ttyout, "unknown command - type \"help\"\n");
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
              memset(argv[i], '\0', strlen(argv[i]));
              i++;
            }

          argv_destroy(argv);
        }
    } while (!quit && strlen((char *)buf) > 0);
  Free(buf);

  return !quit;
}
