/* 
   $Id: ipmi-raw-argp.c,v 1.1 2005-12-09 01:11:32 balamurugan Exp $ 
   
   ipmi-raw-argp.c - ipmi-raw command line argument parser.
   
   Copyright (C) 2005 FreeIPMI Core Team
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
*/

#include <argp.h>
#include "freeipmi.h"
#include "ipmi-raw-argp.h"

static struct arguments arguments;

static error_t parse_opt (int key, char *arg, struct argp_state *state);

static char version_doc[] = 
"FreeIPMI Raw [ipmi-raw-" PACKAGE_VERSION "]\n"
   "Copyright (C) 2003-2005 FreeIPMI Core Team\n"
   "This program is free software; you may redistribute it under the terms of\n"
   "the GNU General Public License.  This program has absolutely no warranty.";

void (*argp_program_version_hook) (FILE *, struct argp_state *) = fi_show_version;
const char *argp_program_bug_address = "<freeipmi-devel@gnu.org>";

static char doc[] = "IPMI Raw - executes IPMI commands by hex values.";

static char args_doc[] = "";

static struct argp_option options[] = 
  {
    { "quiet",                QUIET_KEY,         NULL,          0, 
      "Inhibit usual output.", 0 },
    { "silent",               0,                 NULL,          OPTION_ALIAS, 
      NULL, 0 },
    { "brief",                BRIEF_KEY,         NULL,          0, 
      "Shorten output.", 0 },
    { "verbose",              VERBOSE_KEY,       NULL,          0, 
      "Print more information.", 0 },
    { "driver-poll-interval", POLL_INTERVAL_KEY, "USEC",        0, 
      "User USEC driver poll interval.", 0 }, 
    { "sms-io-base",          SMS_IO_BASE_KEY,   "SMS-IO-BASE", 0, 
      "SMS IO Base address.", 0}, 
    { "host",                 HOST_KEY,          "IPMIHOST",    0, 
      "Connect to IPMIHOST.", 0},
    { "username",             USERNAME_KEY,      "USERNAME",    0, 
      "Use USERNAME instead of NULL.  Maximum USERNAME length is 16.", 0},
    { "password",             PASSWORD_KEY,      "PASSWORD",    0, 
      "Use PASSWORD instead of NULL.  Maximum PASSWORD length is 16.", 0},
    { "auth-type",            AUTH_TYPE_KEY,     "AUTHTYPE",    0, 
      "Use AUTHTYPE instead of NONE.  "
	 "Allowed values are NONE, MD2, MD5, PLAIN and OEM.", 0},
    { "priv-level",           PRIV_LEVEL_KEY,    "PRIVILEGE-LEVEL", 0, 
      "Use this PRIVILEGE-LEVEL instead of USER.  "
	 "Allowed values are CALLBACK, USER, OPERATOR, ADMIN and OEM.", 0},
    { NULL, 0, NULL, 0, NULL, 0 }
  };

static struct argp argp = 
{
  options, parse_opt, args_doc, doc, NULL, NULL, NULL
};

static error_t 
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct arguments *arguments = state->input;
  
  switch (key)
    {
    case QUIET_KEY:
      arguments->quiet = 1;
      break;
    case BRIEF_KEY:
      arguments->brief = 1;
      break;
    case VERBOSE_KEY:
      arguments->verbose = 1;
      break;
    case POLL_INTERVAL_KEY:
      arguments->poll_interval = atol (arg);
      break;
    case SMS_IO_BASE_KEY:
      arguments->sms_io_base = atol (arg);
      break;
    case HOST_KEY:
      arguments->host = arg;
      break;
    case USERNAME_KEY:
      if (strlen (arg) > 16)
	argp_usage (state);
      else 
	arguments->username = arg;
      break;
    case PASSWORD_KEY:
      if (strlen (arg) > 16)
	argp_usage (state);
      else 
	arguments->password = arg;
      break;
    case AUTH_TYPE_KEY:
      if (strcasecmp (arg, "none") == 0)
	{
	  arguments->auth_type = IPMI_SESSION_AUTH_TYPE_NONE;
	}
      else 
	if (strcasecmp (arg, "md2") == 0)
	  {
	    arguments->auth_type = IPMI_SESSION_AUTH_TYPE_MD2;
	  }
	else 
	  if (strcasecmp (arg, "md5") == 0)
	    {
	      arguments->auth_type = IPMI_SESSION_AUTH_TYPE_MD5;
	    }
	  else 
	    if (strcasecmp (arg, "plain") == 0)
	      {
		arguments->auth_type = IPMI_SESSION_AUTH_TYPE_STRAIGHT_PASSWD_KEY;
	      }
	    else 
	      if (strcasecmp (arg, "oem") == 0)
		{
		  arguments->auth_type = IPMI_SESSION_AUTH_TYPE_OEM_PROP;
		}
	      else 
		{
		  argp_usage (state);
		}
      break;
    case PRIV_LEVEL_KEY:
      if (strcasecmp (arg, "callback") == 0)
	{
	  arguments->priv_level = IPMI_PRIV_LEVEL_CALLBACK;
	}
      else 
	if (strcasecmp (arg, "user") == 0)
	  {
	    arguments->priv_level = IPMI_PRIV_LEVEL_USER;
	  }
	else 
	  if (strcasecmp (arg, "operator") == 0)
	    {
	      arguments->priv_level = IPMI_PRIV_LEVEL_OPERATOR;
	    }
	  else 
	    if (strcasecmp (arg, "admin") == 0)
	      {
		arguments->priv_level = IPMI_PRIV_LEVEL_ADMIN;
	      }
	    else 
	      if (strcasecmp (arg, "oem") == 0)
		{
		  arguments->priv_level = IPMI_PRIV_LEVEL_OEM;
		}
	      else 
		{
		  argp_usage (state);
		}
      break;
    case ARGP_KEY_ARG:
      /* Too many arguments. */
      argp_usage (state);
      break;
    case ARGP_KEY_END:
      break;
    default:
      return ARGP_ERR_UNKNOWN;
    }
  
  return 0;
}

void 
fi_show_version (FILE *stream, struct argp_state *state)
{
  (void) state;
  
  fprintf (stream, "%s\n", version_doc);
}

void 
fi_argp_parse (int argc, char **argv)
{
  arguments.quiet = 0;
  arguments.brief = 0;
  arguments.verbose = 0;
  arguments.poll_interval = IPMI_POLL_INTERVAL_USECS;
#ifdef __ia64__
  arguments.sms_io_base = IPMI_KCS_SMS_IO_BASE_SR870BN4;
#else
  arguments.sms_io_base = IPMI_KCS_SMS_IO_BASE_DEFAULT;
#endif
  arguments.host = NULL;
  arguments.username = NULL;
  arguments.password = NULL;
  arguments.auth_type = IPMI_SESSION_AUTH_TYPE_NONE;
  arguments.priv_level = IPMI_PRIV_LEVEL_USER;
  
  argp_parse (&argp, argc, argv, ARGP_IN_ORDER, NULL, &arguments);
  
}

struct arguments *
fi_get_arguments ()
{
  return &arguments;
}

