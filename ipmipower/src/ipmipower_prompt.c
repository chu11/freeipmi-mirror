/*****************************************************************************\
 *  $Id: ipmipower_prompt.c,v 1.14.2.4 2006-02-15 14:46:59 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2003 The Regents of the University of California.
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
 *  with Ipmipower; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif
#include <stdint.h>
#include <sys/stat.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <assert.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h> 

#include "ipmipower_config.h"
#include "ipmipower_prompt.h"
#include "ipmipower_ping.h"
#include "ipmipower_auth.h"
#include "ipmipower_connection.h"
#include "ipmipower_powercmd.h"
#include "ipmipower_output.h"
#include "ipmipower_privilege.h"
#include "ipmipower_wrappers.h"

extern cbuf_t ttyout;
extern cbuf_t ttyin;    
extern cbuf_t ttyerr;
extern struct ipmipower_config *conf;
extern struct ipmipower_connection *ics;

static void 
_cmd_help(void) 
{
  cbuf_printf(ttyout, 
              "hostnames [str]                 - set a new set of hostnames\n"
              "username [str]                  - set a new username (no str for null)\n"
              "password [str]                  - set a new password (no str for null)\n"
              "on [node]                       - turn on all nodes, or listed node\n"
              "off [node]                      - turn off all nodes, or listed node\n"
              "cycle [node]                    - power cycle all nodes, or listed node\n"
              "reset [node]                    - hard reset all nodes, or listed node\n"
              "stat [node]                     - list power of all nodes, or listed node\n"
              "pulse [node]                    - send pulse diagnostic interrupt to all nodes, or listed nodes\n"
              "soft [node]                     - soft shutdown all nodes, or listed nodes\n"
              "help                            - output this help menu\n"
              "advanced                        - output advanced help menu\n"
              "network                         - output network help menu\n"
              "version                         - output ipmipower version\n"
              "quit                            - quit program\n");
}

static void 
_cmd_advanced(void) 
{
  cbuf_printf(ttyout, 
              "authtype str                    - set a new authentication type\n"
              "privilege str                   - set a new privilege type\n"
              "on-if-off [on|off]              - toggle on-if-off functionality\n"
              "outputtype str                  - set a new output type\n"
              "force-permsg-auth [on|off]      - toggle force-permsg-auth functionality\n"
              "accept-session-id-zero [on|off] - toggle accept-session-id-zero functionality\n"
              "check-unexpected-authcode [on|off] - toggle check-unexpected-authcode functionality\n");
#ifndef NDEBUG
  cbuf_printf(ttyout,
              "debug [on|off]                  - toggle debug to stderr\n"
              "ipmidump [on|off]               - toggle IPMI dump output\n"
              "rmcpdump [on|off]               - toggle RMCP dump output\n"
	      "log [on|off]                    - toggle logging\n"
	      "logfile [str]                   - set a new logfile (no str for default)\n");
#endif /* ifndef NDEBUG */
  cbuf_printf(ttyout,
              "config                          - output current configuration\n");
} 

static void
_cmd_network(void)
{
  cbuf_printf(ttyout, 
              "timeout len                     - set a new timeout length\n"
              "retry-timeout len               - set a new retry timeout length\n"
              "retry-backoff-count num         - set a new retry backoff count\n"
              "ping-interval len               - set a new ping interval length\n"
              "ping-timeout len                - set a new ping timeout length\n"
              "ping-packet-count num           - set a new ping packet count\n"
              "ping-percent num                - set a new ping percent number\n"
              "ping-consec-count num           - set a new ping consec count\n");
}

static void 
_cmd_version(void) 
{
  cbuf_printf(ttyout, "ipmipower %s\n", VERSION);
}

static void 
_cmd_hostnames(char **argv) 
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

      cbuf_printf(ttyout, "hostnames unconfigured\n");
    }
  else if ((hl = hostlist_create(argv[1])) == NULL)
    cbuf_printf(ttyout, "hostnames incorrectly formatted\n");
  else 
    {
      int rv, hl_count;
      struct ipmipower_connection *icsPtr;
      char buffer[IPMIPOWER_HOSTLIST_BUFLEN];

      hostlist_uniq(hl);

      hl_count = hostlist_count(hl);
      if (hl_count < IPMIPOWER_MINNODES || hl_count > IPMIPOWER_MAXNODES) 
        {
          cbuf_printf(ttyout, "invalid number of hostnames\n");
          hostlist_destroy(hl);
          return;
        }

      if ((icsPtr = ipmipower_connection_array_create(hl, hl_count)) == NULL) 
        {
          if (errno == EMFILE)
            cbuf_printf(ttyout, "too many files open, file descriptor "
                        "limit too small\n");
          else
            cbuf_printf(ttyout, "error resolving hostnames\n");
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
        cbuf_printf(ttyout, "hostnames: can't output, overflows internal "
                    "buffer\n");
      if (rv > 0)
        cbuf_printf(ttyout, "hostnames: %s\n", buffer);
    }
}

static void 
_cmd_power(char **argv, power_cmd_t cmd) 
{
  int i;

  assert(argv != NULL && POWER_CMD_VALID(cmd));

  if (conf->hosts == NULL) 
    {
      cbuf_printf(ttyout, "no hostnames configured\n");
      return;
    }

  /* Check for correct privilege type */
  if (conf->privilege == PRIVILEGE_TYPE_USER && POWER_CMD_REQUIRES_OPERATOR(cmd))
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
    {                                /* single node */
      if (hostlist_find(conf->hosts, argv[1]) >= 0) {
        i = ipmipower_connection_hostname_index(ics, conf->hosts_count, 
                                                argv[1]);
        if (i < 0)
          ipmipower_output(MSG_TYPE_UNKNOWNNODE, ics[i].hostname);
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
        }
      }
      else
        cbuf_printf(ttyout, "%s: unknown node name\n", argv[1]);
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

  if (argv[1] && conf->authtype == AUTH_TYPE_NONE)
    {
      cbuf_printf(ttyout, "password cannot be set for authtype \"%s\"\n",
                  ipmipower_auth_string(conf->authtype));
    }
  else if (argv[1] == NULL 
           || (argv[1] && strlen(argv[1]) <= IPMI_MAX_AUTHENTICATION_CODE_LENGTH)) 
    {
      memset(conf->password, '\0', IPMI_MAX_AUTHENTICATION_CODE_LENGTH+1);

      if (argv[1])
        strcpy(conf->password, argv[1]);

#ifdef NDEBUG
      cbuf_printf(ttyout, "password changed\n");
#else
      cbuf_printf(ttyout, "password: %s\n", 
                  (strlen(conf->password)) ? conf->password : "NULL");
#endif
    }
  else
    cbuf_printf(ttyout, "password invalid length\n");
}

static void 
_cmd_authtype(char **argv) 
{
  assert(argv != NULL);

  if (argv[1] != NULL) 
    {
      auth_type_t at = ipmipower_auth_index(argv[1]);
      if (at == AUTH_TYPE_INVALID)
        cbuf_printf(ttyout, "%s invalid authtype\n", argv[1]);
      else if (at == AUTH_TYPE_NONE && strlen(conf->password) > 0)
        cbuf_printf(ttyout, "password cannot be set for authtype \"%s\"\n", 
                    argv[1]);
      else 
        {
          conf->authtype = at;
          cbuf_printf(ttyout, "authtype is now %s\n", argv[1]);
        }
    }
  else
    cbuf_printf(ttyout, "authtype must be specified: %s\n",
                ipmipower_auth_list());
}

static void 
_cmd_privilege(char **argv) 
{
  assert(argv != NULL);

  if (argv[1] != NULL) 
    {
      privilege_type_t priv = ipmipower_privilege_index(argv[1]);
      if (priv == PRIVILEGE_TYPE_INVALID)
        cbuf_printf(ttyout, "%s invalid privilege\n", argv[1]);
      else 
        {
          conf->privilege = priv;
          cbuf_printf(ttyout, "privilege is now %s\n", argv[1]);
        }
    }
  else
    cbuf_printf(ttyout, "privilege must be specified: %s\n",
                ipmipower_privilege_list());
}

static void 
_cmd_outputtype(char **argv) 
{
  assert(argv != NULL);

  if (argv[1] != NULL) 
    {
      output_type_t ot = ipmipower_output_index(argv[1]);
      if (ot == OUTPUT_TYPE_INVALID)
        cbuf_printf(ttyout, "%s invalid outputtype\n", argv[1]);
      else 
        {
          conf->outputtype = ot;
          cbuf_printf(ttyout, "outputtype is now %s\n", argv[1]);
        }
    }
  else
    cbuf_printf(ttyout, "outputtype must be specified: %s\n",
                ipmipower_output_list());
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
#endif

static void 
_cmd_config(void) 
{
  if (conf->hosts != NULL) 
    {
      int rv;
      char buffer[IPMIPOWER_HOSTLIST_BUFLEN];
#ifndef NDEBUG
      int i;
      hostlist_t discovered = NULL;
      hostlist_t undiscovered = NULL;
      hostlist_t badconnection = NULL;
#endif

      rv = hostlist_ranged_string(conf->hosts, IPMIPOWER_HOSTLIST_BUFLEN, 
                                  buffer);
      if (rv < 0)
        cbuf_printf(ttyout, "Hostnames:                 can't output, overflows "
                    "internal buffer\n");
      if (rv > 0)
        cbuf_printf(ttyout, "Hostnames:                 %s\n", buffer);

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
        cbuf_printf(ttyout, "Discovered:                can't output, overflows "
                    "internal buffer\n");
      if (rv > 0)
        cbuf_printf(ttyout, "Discovered:                %s\n", buffer);

      rv = hostlist_ranged_string(undiscovered, IPMIPOWER_HOSTLIST_BUFLEN, 
                                  buffer);
      if (rv < 0)
        cbuf_printf(ttyout, "Undiscovered:              can't output, overflows "
                    "internal buffer\n");
      if (rv > 0)
        cbuf_printf(ttyout, "Undiscovered:              %s\n", buffer);

      rv = hostlist_ranged_string(badconnection, IPMIPOWER_HOSTLIST_BUFLEN, 
                                  buffer);
      if (rv < 0) 
        cbuf_printf(ttyout, "BadConnection:             can't output, overflows "
                    "internal buffer\n");
      if (rv > 0)
        cbuf_printf(ttyout, "BadConnection:             %s\n", buffer);

    cleanup:
      hostlist_destroy(discovered);
      hostlist_destroy(undiscovered);
      hostlist_destroy(badconnection);
#endif
    }
  else
    cbuf_printf(ttyout, "Hostnames:                 NONE\n");

  cbuf_printf(ttyout, "Username:                  %s\n", 
              (strlen(conf->username)) ? conf->username : "NULL");

#ifndef NDEBUG
  cbuf_printf(ttyout, "Password:                  %s\n", 
              (strlen(conf->password)) ? conf->password : "NULL");
#else
  cbuf_printf(ttyout, "Password:                  *****\n");
#endif

  cbuf_printf(ttyout, "Authtype:                  %s\n", 
              ipmipower_auth_string(conf->authtype));
  cbuf_printf(ttyout, "Privilege:                 %s\n", 
              ipmipower_privilege_string(conf->privilege));
  cbuf_printf(ttyout, "On-If-Off:                 %s\n",
              (conf->on_if_off) ? "enabled" : "disabled");
  cbuf_printf(ttyout, "OutputType:                %s\n",
              ipmipower_output_string(conf->outputtype));
  cbuf_printf(ttyout, "Force-Permsg_auth:         %s\n",
              (conf->force_permsg_auth) ? "enabled" : "disabled");
  cbuf_printf(ttyout, "Accept-Session-ID-Zero:    %s\n",
              (conf->accept_session_id_zero) ? "enabled" : "disabled");
  cbuf_printf(ttyout, "Check-Unexpected-Authcode:    %s\n",
              (conf->check_unexpected_authcode) ? "enabled" : "disabled");
              
#ifndef NDEBUG
  cbuf_printf(ttyout, "Debug:                     %s\n", 
              (conf->debug) ? "on" : "off");
  cbuf_printf(ttyout, "Ipmidump:                  %s\n", 
              (conf->ipmidump) ? "on" : "off");
  cbuf_printf(ttyout, "Rmcpdump:                  %s\n", 
              (conf->rmcpdump) ? "on" : "off");
  cbuf_printf(ttyout, "Logging:                   %s\n",
	      (conf->log) ? "on" : "off");
  if (conf->log)
    cbuf_printf(ttyout, "Logfile:                   %s\n", conf->logfile);
#endif
  cbuf_printf(ttyout, "Timeout:                   %d ms\n", conf->timeout_len);
  cbuf_printf(ttyout, "Retry Timeout:             %d ms\n", conf->retry_timeout_len);
  cbuf_printf(ttyout, "Retry Backoff Count:       %d\n", conf->retry_backoff_count);
  cbuf_printf(ttyout, "Ping Interval:             %d ms\n", conf->ping_interval_len);
  cbuf_printf(ttyout, "Ping Timeout:              %d ms\n", conf->ping_timeout_len);
  cbuf_printf(ttyout, "Ping Packet Count:         %d\n", conf->ping_packet_count);
  cbuf_printf(ttyout, "Ping Percent:              %d percent\n", conf->ping_percent);
  cbuf_printf(ttyout, "Ping Consec Count:         %d\n", conf->ping_consec_count);
}

static void 
_cmd_set_int(char **argv, int *val, char *str, int allow_zero, int min, int max) 
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
  unsigned char *buf;
  int quit = 0;

  buf = (unsigned char *)Malloc(IPMIPOWER_MAX_TTY_BUF);
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
      _readcmd((char *)buf, IPMIPOWER_MAX_TTY_BUF);
      if (strlen((char *)buf) > 0) 
        {
          char **argv = argv_create((char *)buf, "");
          int i;

          if (argv[0] != NULL) {
            if (strcmp(argv[0], "hostnames") == 0)
              _cmd_hostnames(argv);
            else if (strcmp(argv[0], "username") == 0)
              _cmd_username(argv); 
            else if (strcmp(argv[0], "password") == 0)
              _cmd_password(argv);
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
            else if (strcmp(argv[0], "authtype") == 0)
              _cmd_authtype(argv);
            else if (strcmp(argv[0], "privilege") == 0)
              _cmd_privilege(argv);
            else if (strcmp(argv[0], "on-if-off") == 0)
              _cmd_set_flag(argv, &conf->on_if_off, "on-if-off");
            else if (strcmp(argv[0], "outputtype") == 0)
              _cmd_outputtype(argv);
            else if (strcmp(argv[0], "force-permsg-auth") == 0)
              _cmd_set_flag(argv, &conf->force_permsg_auth, "force-permsg-auth");
            else if (strcmp(argv[0], "accept-session-id-zero") == 0)
              _cmd_set_flag(argv, &conf->accept_session_id_zero, "accept-session-id-zero");
            else if (strcmp(argv[0], "check-unexpected-authcode") == 0)
              _cmd_set_flag(argv, &conf->check_unexpected_authcode, "check-unexpected-authcode");
#ifndef NDEBUG
            else if (strcmp(argv[0], "debug") == 0) 
	      {
		_cmd_set_flag(argv, &conf->debug, "debugging");
		err_cbuf(conf->debug, ttyerr);
                err_cbuf_dump_file_stream(conf->debug, stderr);
	      }
            else if (strcmp(argv[0], "ipmidump") == 0)
              _cmd_set_flag(argv, &conf->ipmidump, "ipmi dump");
            else if (strcmp(argv[0], "rmcpdump") == 0)
              _cmd_set_flag(argv, &conf->rmcpdump, "rmcp dump");
	    else if (strcmp(argv[0], "log") == 0)
              _cmd_log(argv);
	    else if (strcmp(argv[0], "logfile") == 0)
	      _cmd_logfile(argv);
#endif /* ifndef NDEBUG */
            else if (strcmp(argv[0], "happyeaster") == 0)
              cbuf_printf(ttyout, "Ipmipower by Albert Chu <chu11@llnl.gov>\n");
            else if (strcmp(argv[0], "config") == 0)
              _cmd_config();
            else if (strcmp(argv[0], "timeout") == 0)
              _cmd_set_int(argv, &conf->timeout_len, "timeout", 0, 
                           IPMIPOWER_TIMEOUT_MIN, IPMIPOWER_TIMEOUT_MAX);
            else if (strcmp(argv[0], "retry-timeout") == 0)
              _cmd_set_int(argv, &conf->retry_timeout_len, "retry-timeout", 1,
                           IPMIPOWER_RETRY_TIMEOUT_MIN, conf->timeout_len);
            else if (strcmp(argv[0], "retry-backoff-count") == 0)
              _cmd_set_int(argv, &conf->retry_backoff_count, 
                           "retry-backoff-count", 1,
                           IPMIPOWER_RETRY_BACKOFF_COUNT_MIN,
                           IPMIPOWER_RETRY_BACKOFF_COUNT_MAX);
            else if (strcmp(argv[0], "ping-interval") == 0)
              _cmd_set_int(argv, &conf->ping_interval_len, "ping-interval", 1, 
                           IPMIPOWER_PING_INTERVAL_MIN, conf->ping_timeout_len);
            else if (strcmp(argv[0], "ping-timeout") == 0)
              _cmd_set_int(argv, &conf->ping_timeout_len, "ping-timeout", 1, 
                           IPMIPOWER_PING_TIMEOUT_MIN, 
                           IPMIPOWER_PING_TIMEOUT_MAX);
            else if (strcmp(argv[0], "ping-packet-count") == 0)
              _cmd_set_int(argv, &conf->ping_packet_count, "ping-packet-count",
                           1, IPMIPOWER_PING_PACKET_COUNT_MIN, 
                           IPMIPOWER_PING_PACKET_COUNT_MAX);
            else if (strcmp(argv[0], "ping-percent") == 0)
              _cmd_set_int(argv, &conf->ping_percent, "ping-percent", 
                           1, IPMIPOWER_PING_PERCENT_MIN, 
                           IPMIPOWER_PING_PERCENT_MAX);
            else if (strcmp(argv[0], "ping-consec-count") == 0)
              _cmd_set_int(argv, &conf->ping_consec_count, "ping-consec-count", 
                           1, IPMIPOWER_PING_CONSEC_COUNT_MIN, 
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
