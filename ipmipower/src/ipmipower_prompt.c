/*****************************************************************************\
 *  $Id: ipmipower_prompt.c,v 1.56 2007-10-18 16:18:53 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007 Lawrence Livermore National Security, LLC.
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
#include "ipmipower_authentication_type.h"
#include "ipmipower_cipher_suite_id.h"
#include "ipmipower_connection.h"
#include "ipmipower_ipmi_version.h"
#include "ipmipower_powercmd.h"
#include "ipmipower_output.h"
#include "ipmipower_privilege_level.h"
#include "ipmipower_workarounds.h"
#include "ipmipower_wrappers.h"

#include "freeipmi-portability.h"
#include "tool-common.h"

extern cbuf_t ttyout;
extern cbuf_t ttyin;    
extern cbuf_t ttyerr;
extern struct ipmipower_config *conf;
extern struct ipmipower_connection *ics;

/* eliminate
 *
 * Note: only for non-interactive mode on the command line.  Won't be 
 * anywhere in this file.
 */

static void 
_cmd_help(void) 
{
  cbuf_printf(ttyout, 
              "hostname [IPMIHOST]             - Specify a new set of hosts.  No input to unconfigure all hosts.\n"
              "username [USERNAME]             - Specify a new username.  No input for null username.\n"
              "password [PASSWORD]             - Specify a new password.  No input for null password.\n"
              "k_g [K_G]                       - Specify a new K_g BMC Key.  No input for null key.\n"
              "on [IPMIHOST(s)]                - Turn on all configured hosts or specified hosts.\n"
              "off [IPMIHOST(s)]               - Turn off all configured hosts or specified hosts.\n"
              "cycle [IPMIHOST(s)]             - Power cycle all configured hosts or specified hosts.\n"
              "reset [IPMIHOST(s)]             - Reset all configured hosts or specified hosts.\n"
              "stat [IPMIHOST(s)]              - Query power status for all configured hosts or specified hosts.\n"
              "pulse [IPMIHOST(s)]             - Pulse diagnostic interrupt all configured hosts or specified hosts.\n"
              "soft [IPMIHOST(s)]              - Initiate a soft-shutdown for all configured hosts or specified hosts.\n"
              "help                            - Output help menu.\n"
              "advanced                        - Output advanced help menu.\n"
              "network                         - Output network help menu.\n"
              "version                         - Quit program.\n"
              "quit                            - Quit program.\n");
}

static void 
_cmd_advanced(void) 
{
  cbuf_printf(ttyout, 
              "authentication-type AUTHENTICATION-TYPE - Specify the authentication type to use.\n"
              "privilege-level PRIVILEGE-LEVEL         - Specify the privilege level to use.\n"
	      "ipmi-version IPMIVERSION                - Specify the ipmi version to use.\n"
              "cipher-suite-id CIPHER-SUITE-ID         - Specify the cipher suite id to use.\n"
              "on-if-off [on|off]                      - Toggle on-if-off functionality.\n"
              "wait-until-on [on|off]                  - Toggle wait-until-on functionality.\n"
              "wait-until-off [on|off]                 - Toggle wait-until-off functionality.\n"
              "consolidate-output [on|off]             - Toggle consolidate-output functionality.\n"
              "workaround-flags WORKAROUNDS            - Specify workaround flags.\n"
              "debug [on|off]                          - Toggle debug to stderr.\n");
#ifndef NDEBUG
  cbuf_printf(ttyout,
              "rmcpdump [on|off]                       - Toggle RMCP dump output.\n"
	      "log [on|off]                            - Toggle logging output.\n"
	      "logfile [FILE]                          - Specify a new log file.  No input for default.\n");
#endif /* NDEBUG */
  cbuf_printf(ttyout,
              "config                                  - Output current configuration.\n");
} 

static void
_cmd_network(void)
{
  cbuf_printf(ttyout, 
              "session-timeout MILLISECONDS               - Specify a new session timeout length.\n"
              "retransmission-timeout MILLISECONDS        - Specify a new retransmission timeout length.\n"
              "retransmission-wait-timeout MILLISECONDS   - Specify a new retransmission timeout length.\n"
              "retransmission-backoff-count COUNT         - Specify a new retransmission backoff count.\n"
              "ping-interval MILLISECONDS                 - Specify a new ping interval length.\n"
              "ping-timeout MILLISECONDS                  - Specify a new ping timeout length.\n"
              "ping-packet-count COUNT                    - Specify a new ping packet count.\n"
              "ping-percent COUNT                         - Specify a new ping percent number.\n"
              "ping-consec-count COUNT                    - Specify a new ping consec count.\n");
}

static void 
_cmd_version(void) 
{
  cbuf_printf(ttyout, "ipmipower %s\n", VERSION);
}

static void 
_cmd_hostname(char **argv) 
{
  hostlist_t hl; 

  assert(argv != NULL);

  if (argv[1] == NULL) 
    {
      ipmipower_connection_array_destroy(ics, conf->hosts_count);
      hostlist_destroy(conf->hosts);

      ics = NULL;
      conf->hosts = NULL;
      conf->hosts_count = 0;

      cbuf_printf(ttyout, "hostname(s) unconfigured\n");
    }
  else if ((hl = hostlist_create(argv[1])) == NULL)
    cbuf_printf(ttyout, "hostname(s) incorrectly formatted\n");
  else 
    {
      int rv, hl_count;
      struct ipmipower_connection *icsPtr;
      char buffer[IPMIPOWER_HOSTLIST_BUFLEN];

      hostlist_uniq(hl);

      hl_count = hostlist_count(hl);
      if (hl_count < IPMIPOWER_MINNODES || hl_count > IPMIPOWER_MAXNODES) 
        {
          cbuf_printf(ttyout, "invalid number of hostname(s)\n");
          hostlist_destroy(hl);
          return;
        }

      if ((icsPtr = ipmipower_connection_array_create(hl, hl_count)) == NULL) 
        {
          if (errno == EMFILE)
            cbuf_printf(ttyout, "too many files open, file descriptor "
                        "limit too small\n");
          else
            cbuf_printf(ttyout, "error resolving hostname(s)\n");
          hostlist_destroy(hl);
          return;
        }

      ipmipower_connection_array_destroy(ics, conf->hosts_count);
      hostlist_destroy(conf->hosts);

      ics = icsPtr;
      conf->hosts = hl;
      conf->hosts_count = hl_count;
      ipmipower_ping_force_discovery_sweep();

      rv = hostlist_ranged_string(conf->hosts, IPMIPOWER_HOSTLIST_BUFLEN, 
                                  buffer);
      if (rv < 0)
        cbuf_printf(ttyout, "hostname: can't output, overflows internal "
                    "buffer\n");
      if (rv > 0)
        cbuf_printf(ttyout, "hostname: %s\n", buffer);
    }
}

static void 
_cmd_power(char **argv, power_cmd_t cmd) 
{
  int i;

  assert(argv != NULL && POWER_CMD_VALID(cmd));

  if (conf->hosts == NULL) 
    {
      cbuf_printf(ttyout, "no hostname(s) configured\n");
      return;
    }

  /* Check for correct privilege type */
  if (conf->privilege_level == PRIVILEGE_LEVEL_USER 
      && POWER_CMD_REQUIRES_OPERATOR_PRIVILEGE_LEVEL(cmd))
    {
      cbuf_printf(ttyout, "power operation requires atleast operator privilege");
      return;
    }

  if (argv[1] == NULL)  /* all nodes */
    { 
      int nodes_queued = 0;
      
      for (i = 0; i <  conf->hosts_count; i++) 
        {
          if (conf->ping_interval_len 
              && ics[i].discover_state == STATE_UNDISCOVERED)
            ipmipower_output(MSG_TYPE_NOTDISCOVERED, ics[i].hostname);
          else if (conf->ping_interval_len 
                   && conf->ping_packet_count 
                   && conf->ping_percent 
                   && ics[i].discover_state == STATE_BADCONNECTION)
            ipmipower_output(MSG_TYPE_BADCONNECTION, ics[i].hostname);
          else {
            ipmipower_connection_clear(&ics[i]);
            ipmipower_powercmd_queue(cmd, &ics[i]);
            nodes_queued++;
          }
        }

      /* Special corner case when no nodes are discovered */
      if (nodes_queued == 0)
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
        err_exit("hostlist_iterator_create() error");

      while ((node = hostlist_next(itr)))
        {
          i = ipmipower_connection_hostname_index(ics, 
                                                  conf->hosts_count, 
                                                  node);

          if (i < 0)
            ipmipower_output(MSG_TYPE_UNKNOWNNODE, node);
          else if (conf->ping_interval_len 
                   && ics[i].discover_state == STATE_UNDISCOVERED)
            ipmipower_output(MSG_TYPE_NOTDISCOVERED, ics[i].hostname);
          else if (conf->ping_interval_len 
                   && conf->ping_packet_count 
                   && conf->ping_percent 
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
_cmd_username(char **argv)
{
  assert(argv != NULL);

  if (argv[1] == NULL 
           || (argv[1] && strlen(argv[1]) <= IPMI_MAX_USER_NAME_LENGTH)) 
    {
      memset(conf->username, '\0', IPMI_MAX_USER_NAME_LENGTH+1);

      if (argv[1])
        strcpy(conf->username, argv[1]);

      cbuf_printf(ttyout, "username: %s\n", 
                  (strlen(conf->username)) ? conf->username : "NULL");
    }
  else
    cbuf_printf(ttyout, "username invalid length\n");
}

static void 
_cmd_password(char **argv) 
{
  assert(argv != NULL);

  if (argv[1] && conf->authentication_type == AUTHENTICATION_TYPE_NONE)
    cbuf_printf(ttyout, "password cannot be set for authentication_type \"%s\"\n",
                ipmipower_authentication_type_string(conf->authentication_type));
  else if (argv[1] == NULL 
           || (argv[1] 
               && (((conf->ipmi_version == IPMI_VERSION_AUTO
                     || conf->ipmi_version == IPMI_VERSION_2_0)
                    && strlen(argv[1]) <= IPMI_2_0_MAX_PASSWORD_LENGTH)
                   || (conf->ipmi_version == IPMI_VERSION_1_5
                       && strlen(argv[1]) <= IPMI_1_5_MAX_PASSWORD_LENGTH))))
    {
      memset(conf->password, '\0', IPMI_2_0_MAX_PASSWORD_LENGTH+1);

      if (argv[1])
        strcpy(conf->password, argv[1]);

#ifdef NDEBUG
      cbuf_printf(ttyout, "password changed\n");
#else  /* !NDEBUG */
      cbuf_printf(ttyout, "password: %s\n", 
                  (strlen(conf->password)) ? conf->password : "NULL");
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
  assert(argv != NULL);

  if (conf->ipmi_version != IPMI_VERSION_AUTO
      && conf->ipmi_version != IPMI_VERSION_2_0)
    cbuf_printf(ttyout, "k_g is only used for IPMI 2.0");
  else
    {
      memset(conf->k_g, '\0', IPMI_MAX_K_G_LENGTH);

      if (argv[1])
        rv = parse_kg(conf->k_g, IPMI_MAX_K_G_LENGTH + 1, argv[1]);
      
      if (rv < 0)
        cbuf_printf(ttyout, "k_g invalid\n");
      else
        {
          conf->k_g_len = rv;
#ifdef NDEBUG
          cbuf_printf(ttyout, "k_g changed\n");
#else  /* !NDEBUG */
          cbuf_printf(ttyout, "k_g: %s\n", 
                      (conf->k_g_len) ? format_kg(buf, IPMI_MAX_K_G_LENGTH*2+3, conf->k_g) : "NULL");
#endif /* !NDEBUG */
        }
    }
}

static void 
_cmd_authentication_type(char **argv) 
{
  assert(argv != NULL);

  if (argv[1] != NULL) 
    {
      authentication_type_t at = ipmipower_authentication_type_index(argv[1]);
      if (at == AUTHENTICATION_TYPE_INVALID)
        cbuf_printf(ttyout, "%s invalid authentication_type\n", argv[1]);
      else if (at == AUTHENTICATION_TYPE_NONE && strlen(conf->password) > 0)
        cbuf_printf(ttyout, 
		    "password cannot be set for authentication type \"%s\"\n", 
		    argv[1]);
      else 
        {
          conf->authentication_type = at;
          cbuf_printf(ttyout, "authentication type is now %s\n", argv[1]);
        }
    }
  else
    cbuf_printf(ttyout, "authentication_type must be specified: %s\n",
                ipmipower_authentication_type_list());
}

static void 
_cmd_privilege_level(char **argv) 
{
  assert(argv != NULL);

  if (argv[1] != NULL) 
    {
      privilege_level_t priv = ipmipower_privilege_level_index(argv[1]);
      if (priv == PRIVILEGE_LEVEL_INVALID)
        cbuf_printf(ttyout, "%s invalid privilege\n", argv[1]);
      else 
        {
          conf->privilege_level = priv;
          cbuf_printf(ttyout, "privilege is now %s\n", argv[1]);
        }
    }
  else
    cbuf_printf(ttyout, "privilege must be specified: %s\n",
                ipmipower_privilege_level_list());
}

static void 
_cmd_ipmi_version(char **argv) 
{
  assert(argv != NULL);

  if (argv[1] != NULL) 
    {
      ipmi_version_t ipmi_version = ipmipower_ipmi_version_index(argv[1]);
      if (ipmi_version == IPMI_VERSION_INVALID)
        cbuf_printf(ttyout, "%s invalid ipmi version\n", argv[1]);
      else 
        {
          conf->ipmi_version = ipmi_version;
          cbuf_printf(ttyout, "ipmi version is now %s\n", argv[1]);
        }
    }
  else
    cbuf_printf(ttyout, "ipmi_version must be specified: %s\n",
                ipmipower_ipmi_version_list());
}

static void 
_cmd_cipher_suite_id(char **argv) 
{
  assert(argv != NULL);

  if (argv[1] != NULL) 
    {
      cipher_suite_id_t cipher_suite_id = ipmipower_cipher_suite_id_index(argv[1]);
      if (cipher_suite_id == CIPHER_SUITE_ID_INVALID)
        cbuf_printf(ttyout, "%s invalid cipher suite id\n", argv[1]);
      else 
        {
          conf->cipher_suite_id = cipher_suite_id;
          cbuf_printf(ttyout, "cipher suite id is now %s\n", argv[1]);
        }
    }
  else
    cbuf_printf(ttyout, "cipher_suite_id must be specified: %s\n",
                ipmipower_cipher_suite_id_list());
}

static void
_cmd_workaround_flags(char **argv)
{
  assert(argv != NULL);

  if (argv[1] != NULL) 
    {
      uint32_t flags;

      if (ipmipower_workarounds_parse(argv[1], &flags) < 0)
        cbuf_printf(ttyout, "%s invalid workaround specified\n", argv[1]);
      else 
        {
          conf->workaround_flags = flags;
          cbuf_printf(ttyout, "workaround flags is now %s\n", argv[1]);
        }
    }
  else
    cbuf_printf(ttyout, "workaround_flags must be specified: %s\n",
                ipmipower_workarounds_list());
}

#ifndef NDEBUG
static void
_cmd_log(char **argv)
{
  assert(argv != NULL);
  
  if (argv[1] == NULL) 
    conf->log = !conf->log;
  else 
    {
      if (!strcasecmp(argv[1], "on"))
        conf->log = IPMIPOWER_TRUE;
      else if (!strcasecmp(argv[1], "off"))  
        conf->log = IPMIPOWER_FALSE;
      else 
        {
          cbuf_printf(ttyout, "invalid parameter\n");
          return;
        }
    }
  
  if (conf->log && conf->logfile_fd == -1)
    {
      /* Don't use Open wrapper, we don't want to err_exit on failure */
      if ((conf->logfile_fd = open(conf->logfile, 
                                   O_WRONLY | O_CREAT | O_APPEND, 
                                   S_IRUSR | S_IWUSR)) < 0)
        { 
          cbuf_printf(ttyout, "error opening log file %s: %s\n", 
                      conf->logfile, strerror(errno));
          cbuf_printf(ttyout, "logging not enabled\n");
          conf->log = IPMIPOWER_FALSE;
          return;
        }
    }
  else if (!conf->log && conf->logfile_fd != -1)
    {
      close(conf->logfile_fd);
      conf->logfile_fd = -1;
    }

  err_cbuf_dump_file_descriptor(conf->log, conf->logfile_fd);
  cbuf_printf(ttyout, "logging is now %s\n", (conf->log) ? "on" : "off");
}

static void 
_cmd_logfile(char **argv) 
{
  int fd = -1;
  char *file;
  char tempfile[MAXPATHLEN+1];

  assert(argv != NULL);

  if (argv[1] != NULL && strlen(argv[1]) > MAXPATHLEN)
    {
      cbuf_printf(ttyout, "log file pathname too long");
      return;
    }

  if (argv[1] == NULL)
    {
      memset(tempfile, '\0', MAXPATHLEN+1);
      ipmipower_config_default_logfile(tempfile, MAXPATHLEN);
      file = tempfile;
    }
  else
    file = argv[1];

  if (conf->log)
    {
      /* Don't use Open wrapper, we don't want to err_exit on failure */
      if ((fd = open(file, O_WRONLY | O_CREAT | O_APPEND, S_IRUSR | S_IWUSR)) < 0)
        { 
          cbuf_printf(ttyout, "error opening log file %s: %s\n", 
                      file, strerror(errno));
          return;
        }
      close(conf->logfile_fd);
    }

  memset(conf->logfile, '\0', MAXPATHLEN+1);
  strcpy(conf->logfile, file);
  conf->logfile_fd = fd;
  err_cbuf_dump_file_descriptor(conf->log, conf->logfile_fd);
  
  cbuf_printf(ttyout, "log file set to %s\n", conf->logfile);
}
#endif /* NDEBUG */

static void 
_cmd_config(void) 
{
  char buf[IPMI_MAX_K_G_LENGTH*2+3];

  if (conf->hosts != NULL) 
    {
      int rv;
      char buffer[IPMIPOWER_HOSTLIST_BUFLEN];
#ifndef NDEBUG
      int i;
      hostlist_t discovered = NULL;
      hostlist_t undiscovered = NULL;
      hostlist_t badconnection = NULL;
#endif /* NDEBUG */

      rv = hostlist_ranged_string(conf->hosts, 
                                  IPMIPOWER_HOSTLIST_BUFLEN, 
                                  buffer);
      if (rv < 0)
        cbuf_printf(ttyout, "Hostname:                     can't output\n");
      if (rv > 0)
        cbuf_printf(ttyout, "Hostname:                     %s\n", buffer);

#ifndef NDEBUG
      if ((discovered = hostlist_create(NULL)) == NULL)
        goto cleanup;
      if ((undiscovered = hostlist_create(NULL)) == NULL)
        goto cleanup;
      if ((badconnection = hostlist_create(NULL)) == NULL)
        goto cleanup;
      
      for (i = 0; i < conf->hosts_count; i++) {
        if (ics[i].discover_state == STATE_DISCOVERED)
          rv = hostlist_push_host(discovered, ics[i].hostname);
        else if (ics[i].discover_state == STATE_UNDISCOVERED)
          rv = hostlist_push_host(undiscovered, ics[i].hostname);
        else
          rv = hostlist_push_host(badconnection, ics[i].hostname);
        
        if (rv == 0)
          goto cleanup;
      }

      rv = hostlist_ranged_string(discovered, IPMIPOWER_HOSTLIST_BUFLEN, 
                                  buffer);
      if (rv < 0)
        cbuf_printf(ttyout, "Discovered:                   can't output\n");
      if (rv > 0)
        cbuf_printf(ttyout, "Discovered:                   %s\n", buffer);

      rv = hostlist_ranged_string(undiscovered, IPMIPOWER_HOSTLIST_BUFLEN, 
                                  buffer);
      if (rv < 0)
        cbuf_printf(ttyout, "Undiscovered:                 can't output\n");
      if (rv > 0)
        cbuf_printf(ttyout, "Undiscovered:                 %s\n", buffer);

      rv = hostlist_ranged_string(badconnection, IPMIPOWER_HOSTLIST_BUFLEN, 
                                  buffer);
      if (rv < 0) 
        cbuf_printf(ttyout, "BadConnection:                can't output\n");
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
              (strlen(conf->username)) ? conf->username : "NULL");

#ifndef NDEBUG
  cbuf_printf(ttyout, "Password:                     %s\n", 
              (strlen(conf->password)) ? conf->password : "NULL");
  cbuf_printf(ttyout, "K_g:                          %s\n", 
              (conf->k_g_len) ? 
              format_kg(buf, IPMI_MAX_K_G_LENGTH*2+3, conf->k_g) : "NULL");
#else  /* !NDEBUG */
  cbuf_printf(ttyout, "Password:                     *****\n");
  cbuf_printf(ttyout, "K_g:                          *****\n");
#endif /* !NDEBUG */

  cbuf_printf(ttyout, "Authentication_Type:          %s\n", 
              ipmipower_authentication_type_string(conf->authentication_type));
  cbuf_printf(ttyout, "Privilege_Level:              %s\n", 
              ipmipower_privilege_level_string(conf->privilege_level));
  cbuf_printf(ttyout, "IPMI_Version:                 %s\n",
              ipmipower_ipmi_version_string(conf->ipmi_version));
  cbuf_printf(ttyout, "Cipher Suite Id:              %s\n",
	      ipmipower_cipher_suite_id_string(conf->cipher_suite_id));
  cbuf_printf(ttyout, "On-If-Off:                    %s\n",
              (conf->on_if_off) ? "enabled" : "disabled");
  cbuf_printf(ttyout, "Wait-Until-On:                %s\n",
              (conf->wait_until_on) ? "enabled" : "disabled");
  cbuf_printf(ttyout, "Wait-Until-Off:               %s\n",
              (conf->wait_until_off) ? "enabled" : "disabled");
  cbuf_printf(ttyout, "Consolidate-Output:           %s\n",
              (conf->consolidate_output) ? "enabled" : "disabled");
  cbuf_printf(ttyout, "WorkaroundFlags:              %s\n",
              ipmipower_workarounds_string(conf->workaround_flags));
#ifndef NDEBUG
  cbuf_printf(ttyout, "Debug:                        %s\n", 
              (conf->debug) ? "on" : "off");
  cbuf_printf(ttyout, "Rmcpdump:                     %s\n", 
              (conf->rmcpdump) ? "on" : "off");
  cbuf_printf(ttyout, "Logging:                      %s\n",
	      (conf->log) ? "on" : "off");
  if (conf->log)
    cbuf_printf(ttyout, "Logfile:                      %s\n", 
                conf->logfile);
#endif /* NDEBUG */
  cbuf_printf(ttyout, "Session Timeout:              %d ms\n", 
              conf->session_timeout_len);
  cbuf_printf(ttyout, "Retransmission Timeout:       %d ms\n", 
              conf->retransmission_timeout_len);
  cbuf_printf(ttyout, "Retransmission Wait Timeout:  %d ms\n", 
              conf->retransmission_wait_timeout_len);
  cbuf_printf(ttyout, "Retransmission Backoff Count: %d\n", 
              conf->retransmission_backoff_count);
  cbuf_printf(ttyout, "Ping Interval:                %d ms\n",
              conf->ping_interval_len);
  cbuf_printf(ttyout, "Ping Timeout:                 %d ms\n", 
              conf->ping_timeout_len);
  cbuf_printf(ttyout, "Ping Packet Count:            %d\n", 
              conf->ping_packet_count);
  cbuf_printf(ttyout, "Ping Percent:                 %d percent\n", 
              conf->ping_percent);
  cbuf_printf(ttyout, "Ping Consec Count:            %d\n", 
              conf->ping_consec_count);
}

static void 
_cmd_set_int(char **argv, 
             int *val, 
             char *str, 
             int allow_zero,
             int min, 
             int max) 
{
  assert(argv != NULL && val != NULL && str != NULL);

  if (argv[1] == NULL)
    cbuf_printf(ttyout, "%s not specified\n", str);
  else 
    {
      char *ptr;
      int temp = strtol(argv[1], &ptr, 10);
      if (ptr != (argv[1] + strlen(argv[1]))) 
        cbuf_printf(ttyout, "invalid %s input\n", str);
      else if ((allow_zero && temp == 0) || (temp <= max && temp >= min)) {
        *val = temp;
        cbuf_printf(ttyout, "%s is now %d\n", str, *val);
      }
      else
        cbuf_printf(ttyout, "invalid %s input, range is %d <=> %d\n", 
                    str, min, max);
    }
}

static void 
_cmd_set_flag(char **argv, ipmipower_bool_t *flag, char *str) 
{
  assert(argv != NULL && flag != NULL && str != NULL);
  
  if (argv[1] == NULL) 
    *flag = !(*flag);
  else 
    {
      if (!strcasecmp(argv[1], "on"))
        *flag = IPMIPOWER_TRUE;
      else if (!strcasecmp(argv[1], "off"))  
        *flag = IPMIPOWER_FALSE;
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
        err_exit("_readcmd: cbuf_peek returned %d", bytes_peeked);
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
    err_output("warning: _readcmd: cbuf_drop returned %d (!= %d)", dropped, len);
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

          if (argv[0] != NULL) 
            {
              /* support hostnames (plural) for backwards compatability */
              if (strcmp(argv[0], "hostnames") == 0
                  || strcmp(argv[0], "hostname") == 0)
                _cmd_hostname(argv);
              else if (strcmp(argv[0], "username") == 0)
                _cmd_username(argv); 
              else if (strcmp(argv[0], "password") == 0)
                _cmd_password(argv);
              else if (strcmp(argv[0], "k_g") == 0)
                _cmd_k_g(argv);
              else if (strcmp(argv[0], "on") == 0)
                _cmd_power(argv, POWER_CMD_POWER_ON);
              else if (strcmp(argv[0], "off") == 0)
                _cmd_power(argv, POWER_CMD_POWER_OFF);
              else if (strcmp(argv[0], "cycle") == 0)
                _cmd_power(argv, POWER_CMD_POWER_CYCLE);
              else if (strcmp(argv[0], "reset") == 0)
                _cmd_power(argv, POWER_CMD_POWER_RESET);
              else if (strcmp(argv[0], "stat") == 0)
                _cmd_power(argv, POWER_CMD_POWER_STATUS);
              else if (strcmp(argv[0], "pulse") == 0)
                _cmd_power(argv, POWER_CMD_PULSE_DIAG_INTR);
              else if (strcmp(argv[0], "soft") == 0)
                _cmd_power(argv, POWER_CMD_SOFT_SHUTDOWN_OS);
              else if (strcmp(argv[0], "help") == 0 
                       || strcmp(argv[0], "?") == 0)
                _cmd_help();
              else if (strcmp(argv[0], "advanced") == 0)
                _cmd_advanced();
              else if (strcmp(argv[0], "network") == 0)
                _cmd_network();
              else if (strcmp(argv[0], "version") == 0)
                _cmd_version();
              else if (strcmp(argv[0], "quit") == 0)
                quit = 1;
              /* support underscored version for backwards compatability */
              else if (strcmp(argv[0], "authentication_type") == 0
                       || strcmp(argv[0], "authentication-type") == 0)
                _cmd_authentication_type(argv);
              /* support "privilege" command for backwards compatability */
              else if (strcmp(argv[0], "privilege") == 0
                       || strcmp(argv[0], "privilege-level") == 0)
                _cmd_privilege_level(argv);
              /* support underscored version for backwards compatability */
              else if (strcmp(argv[0], "ipmi_version") == 0
                       || strcmp(argv[0], "ipmi-version") == 0)
                _cmd_ipmi_version(argv);
              /* support underscored version for backwards compatability */
              else if (strcmp(argv[0], "cipher_suite_id") == 0
                       || strcmp(argv[0], "cipher-suite-id") == 0)
                _cmd_cipher_suite_id(argv);
              else if (strcmp(argv[0], "on-if-off") == 0)
                _cmd_set_flag(argv,
                              &conf->on_if_off, 
                              "on-if-off");
              else if (strcmp(argv[0], "wait-until-on") == 0)
                _cmd_set_flag(argv,
                              &conf->wait_until_on, 
                              "wait-until-on");
              else if (strcmp(argv[0], "wait-until-off") == 0)
                _cmd_set_flag(argv,
                              &conf->wait_until_off,
                              "wait-until-off");
              else if (strcmp(argv[0], "consolidate-output") == 0)
                _cmd_set_flag(argv, 
                              &conf->consolidate_output, 
                              "consolidate-output");
              else if (strcmp(argv[0], "workaround-flags") == 0)
                _cmd_workaround_flags(argv);
              else if (strcmp(argv[0], "debug") == 0) 
                {
                  _cmd_set_flag(argv,
                                &conf->debug, 
                                "debugging");
                  err_cbuf(conf->debug, ttyerr);
                  err_cbuf_dump_file_stream(conf->debug, stderr);
                }
#ifndef NDEBUG
              else if (strcmp(argv[0], "rmcpdump") == 0)
                _cmd_set_flag(argv, 
                              &conf->rmcpdump,
                              "rmcp dump");
              else if (strcmp(argv[0], "log") == 0)
                _cmd_log(argv);
              else if (strcmp(argv[0], "logfile") == 0)
                _cmd_logfile(argv);
#endif /* NDEBUG */
              else if (strcmp(argv[0], "happyeaster") == 0)
                cbuf_printf(ttyout, "by Albert Chu <chu11@llnl.gov>\n");
              else if (strcmp(argv[0], "config") == 0)
                _cmd_config();
              /* support "timeout" for backwards compatability */
              else if (strcmp(argv[0], "timeout") == 0
                       || strcmp(argv[0], "session-timeout") == 0)
                _cmd_set_int(argv, 
                             &conf->session_timeout_len, 
                             "timeout",
                             0, 
                             IPMIPOWER_SESSION_TIMEOUT_MIN, 
                             IPMIPOWER_SESSION_TIMEOUT_MAX);
              /* support "retry-timeout" for backwards compatability */
              else if (strcmp(argv[0], "retry-timeout") == 0
                       || strcmp(argv[0], "retransmission-timeout") == 0)
                _cmd_set_int(argv, 
                             &conf->retransmission_timeout_len, 
                             "retransmission-timeout", 1,
                             IPMIPOWER_RETRANSMISSION_TIMEOUT_MIN, 
                             conf->session_timeout_len);
              /* support "retry-wait-timeout" for backwards compatability */
              else if (strcmp(argv[0], "retry-wait-timeout") == 0
                       || strcmp(argv[0], "retransmission-wait-timeout") == 0)
                _cmd_set_int(argv, 
                             &conf->retransmission_wait_timeout_len, 
			     "retransmission-wait-timeout", 
                             1,
                             IPMIPOWER_RETRANSMISSION_WAIT_TIMEOUT_MIN, 
                             conf->session_timeout_len);
              /* support "retry-backoff-count" for backwards compatability */
              else if (strcmp(argv[0], "retry-backoff-count") == 0
                       || strcmp(argv[0], "retransmission-backoff-count") == 0)
                _cmd_set_int(argv, 
                             &conf->retransmission_backoff_count, 
                             "retransmission-backoff-count", 
                             1,
                             IPMIPOWER_RETRANSMISSION_BACKOFF_COUNT_MIN,
                             IPMIPOWER_RETRANSMISSION_BACKOFF_COUNT_MAX);
              else if (strcmp(argv[0], "ping-interval") == 0)
                _cmd_set_int(argv,
                             &conf->ping_interval_len, 
                             "ping-interval", 
                             1, 
                             IPMIPOWER_PING_INTERVAL_MIN,
                             conf->ping_timeout_len);
              else if (strcmp(argv[0], "ping-timeout") == 0)
                _cmd_set_int(argv, 
                             &conf->ping_timeout_len, 
                             "ping-timeout",
                             1, 
                             IPMIPOWER_PING_TIMEOUT_MIN, 
                             IPMIPOWER_PING_TIMEOUT_MAX);
              else if (strcmp(argv[0], "ping-packet-count") == 0)
                _cmd_set_int(argv, 
                             &conf->ping_packet_count, 
                             "ping-packet-count",
                             1, 
                             IPMIPOWER_PING_PACKET_COUNT_MIN, 
                             IPMIPOWER_PING_PACKET_COUNT_MAX);
              else if (strcmp(argv[0], "ping-percent") == 0)
                _cmd_set_int(argv,
                             &conf->ping_percent,
                             "ping-percent", 
                             1, 
                             IPMIPOWER_PING_PERCENT_MIN, 
                             IPMIPOWER_PING_PERCENT_MAX);
              else if (strcmp(argv[0], "ping-consec-count") == 0)
                _cmd_set_int(argv,
                             &conf->ping_consec_count, 
                             "ping-consec-count", 
                             1, 
                             IPMIPOWER_PING_CONSEC_COUNT_MIN, 
                             conf->ping_packet_count);
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
          while(argv[i] != NULL) 
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
