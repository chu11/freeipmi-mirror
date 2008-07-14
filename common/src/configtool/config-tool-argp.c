/* 
   Copyright (C) 2003-2008 FreeIPMI Core Team

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.  
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <sys/stat.h>
#include <errno.h>
#include <assert.h>

#include "config-tool-argp.h"
#include "config-tool-utils.h"

#include "freeipmi-portability.h"

void 
init_config_args (struct config_arguments *config_args)
{
  assert(config_args);

  config_args->action = 0;
  config_args->verbose = 0;
  config_args->filename = NULL;
  config_args->keypairs = NULL;
  config_args->section_strs = NULL;
}

error_t 
config_parse_opt (int key,
                  char *arg,
                  struct argp_state *state,
                  struct config_arguments *config_args)
{
  struct config_keypair *kp = NULL;
  struct config_section_str *sstr = NULL;
  char *section_name = NULL;
  char *key_name = NULL;
  char *value = NULL;

  switch (key)
    {
    case CONFIG_ARGP_CHECKOUT_KEY:
      if (!config_args->action)
        config_args->action = CONFIG_ACTION_CHECKOUT;
      else
        config_args->action = -1;
      break;
    case CONFIG_ARGP_COMMIT_KEY:
      if (!config_args->action)
        config_args->action = CONFIG_ACTION_COMMIT;
      else
        config_args->action = -1;
      break;
    case CONFIG_ARGP_DIFF_KEY:
      if (!config_args->action)
        config_args->action = CONFIG_ACTION_DIFF;
      else
        config_args->action = -1;
      break;
    case CONFIG_ARGP_FILENAME_KEY:
      if (config_args->filename) /* If specified more than once */
        free (config_args->filename);
      if (!(config_args->filename = strdup (arg)))
        {
          perror("strdup");
          exit(1);
        }
      break;
    case CONFIG_ARGP_KEYPAIR_KEY:
      if (config_keypair_parse_string(arg,
                                      &section_name,
                                      &key_name,
                                      &value) < 0)
        {
          /* error printed in function call */
          exit(1);
        }
      if (!(kp = config_keypair_create(section_name,
                                       key_name,
                                       value)))
        {
          fprintf(stderr,
                  "config_keypair_create error\n");
          exit(1);
        }
      if (config_keypair_append(&(config_args->keypairs),
                                kp) < 0)
        {
          /* error printed in function call */
          exit(1);
        }
      if (section_name)
        free(section_name);
      section_name = NULL;
      if (key_name)
        free(key_name);
      key_name = NULL;
      if (value)
        free(value);
      value = NULL;
      kp = NULL;
      break;
    case CONFIG_ARGP_SECTIONS_KEY:
      if (!(sstr = config_section_str_create(arg)))
        {
          fprintf(stderr,
                  "config_section_str_create error\n");
          exit(1);
        }
      if (config_section_str_append(&(config_args->section_strs),
                                    sstr) < 0)
        {
          /* error printed in function call */
          exit(1);
        }
      sstr = NULL;
      break;
    case CONFIG_ARGP_LIST_SECTIONS_KEY:
      if (!config_args->action)
        config_args->action = CONFIG_ACTION_LIST_SECTIONS;
      else
        config_args->action = -1;
      break;
    case CONFIG_ARGP_VERBOSE_KEY:
      config_args->verbose = 1;
      break;
    default:
      return ARGP_ERR_UNKNOWN;
    }
 
  return 0;
}

void 
config_args_validate (struct config_arguments *config_args)
{
  assert(config_args);
  
  /* filename and keypair both given for diff */
  if (config_args->filename && config_args->keypairs
      && config_args->action == CONFIG_ACTION_DIFF)
    {
      fprintf (stderr,
               "Both --filename or --keypair cannot be used\n");
      exit(1);
    }

  /* only one of keypairs or section can be given for checkout */
  if (config_args->action == CONFIG_ACTION_CHECKOUT
      && (config_args->keypairs && config_args->section_strs))
    {
      fprintf (stderr,
               "Only one of --filename, --keypair, and --section can be used\n");
      exit(1);
    }

  /* filename is readable if commit, writable/creatable if checkout */
  if (config_args->filename)
    {
      switch (config_args->action)
        {
        case CONFIG_ACTION_COMMIT:
        case CONFIG_ACTION_DIFF:
          if (access (config_args->filename, R_OK) != 0)
            {
              fprintf(stderr,
                      "Cannot read '%s': %s\n",
                      config_args->filename,
                      strerror(errno));
              exit(1);
            }
          break;
        case CONFIG_ACTION_CHECKOUT:
          if (access (config_args->filename, F_OK) == 0)
            {
              if (access (config_args->filename, W_OK) != 0)
                {
                  fprintf(stderr,
                          "Cannot write to '%s': %s\n",
                          config_args->filename,
                          strerror(errno));
                  exit(1);
                }
            }
          else
            {
              int fd;
              fd = open (config_args->filename, O_CREAT, 0644);
              if (fd == -1)
                {
                  fprintf(stderr,
                          "Cannot open '%s': %s\n",
                          config_args->filename,
                          strerror(errno));
                  exit(1);
                }
              else
                {
                  close (fd);
                  unlink (config_args->filename);
                }
            }
          break;
        case CONFIG_ACTION_LIST_SECTIONS:
          /* do nothing - here to remove compile warning */
          break;
        }
    }
}
