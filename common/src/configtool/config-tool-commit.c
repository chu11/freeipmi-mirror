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
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#include <assert.h>

#include "config-tool-commit.h"
#include "config-tool-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

config_err_t
config_commit_section(pstdout_state_t pstate,
                      struct config_section *section,
                      struct config_arguments *cmd_args,
                      void *arg)
{
  struct config_keyvalue *kv;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;
  config_err_t this_ret;
  unsigned int commit_count = 0;

  assert(section);
  assert(cmd_args);

  if (section->section_pre_commit)
    {
      if ((this_ret = section->section_pre_commit (section->section_name, 
                                                   arg)) == CONFIG_ERR_FATAL_ERROR)
        goto cleanup;

      if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr, 
                           "ERROR: Section pre-commit `%s'\n", 
                           section->section_name);
          ret = CONFIG_ERR_NON_FATAL_ERROR;
        }
    }

  kv = section->keyvalues;
  while (kv) 
    {
      assert(kv->value_input);

      if (!(kv->key->flags & CONFIG_READABLE_ONLY)
          && !(kv->key->flags & CONFIG_UNDEFINED))
        {
          if ((this_ret = kv->key->commit (section->section_name,
                                           kv,
                                           arg)) == CONFIG_ERR_FATAL_ERROR)
            goto cleanup;

          if (this_ret == CONFIG_ERR_SUCCESS)
            commit_count++;
          
          if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
            {
              PSTDOUT_FPRINTF (pstate,
                               stderr, 
                               "ERROR: Failed to commit `%s:%s'\n", 
                               section->section_name,
                               kv->key->key_name);
              ret = CONFIG_ERR_NON_FATAL_ERROR;
            }
        }
      else
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr, 
                           "ERROR: `%s:%s' is not writeable\n", 
                           section->section_name, 
                           kv->key->key_name);
          ret = CONFIG_ERR_NON_FATAL_ERROR;
        }
      
      kv = kv->next;
    }
  
  if (cmd_args->verbose)
    PSTDOUT_FPRINTF (pstate,
                     stderr, 
                     "Completed commit of Section: %s\n",
                     section->section_name);
  
  if (commit_count && section->section_post_commit)
    {
      if ((this_ret = section->section_post_commit (section->section_name,
                                                    arg)) == CONFIG_ERR_FATAL_ERROR)
        goto cleanup;

      if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr, 
                           "ERROR: Section post-commit `%s:%s'\n", 
                           section->section_name,
                           kv->key->key_name);
          ret = CONFIG_ERR_NON_FATAL_ERROR;
        }
    }

  rv = ret;
 cleanup:
  return rv;
}

config_err_t
config_commit (pstdout_state_t pstate,
               struct config_section *sections,
               struct config_arguments *cmd_args,
               void *arg)
{
  struct config_section *s;
  config_err_t rv = CONFIG_ERR_SUCCESS;
  config_err_t ret;

  assert(sections);
  assert(cmd_args);

  s = sections;
  while (s)
    {
      if ((ret = config_commit_section(pstate,
                                       s, 
                                       cmd_args,
                                       arg)) != CONFIG_ERR_SUCCESS)
        {
          if (ret == CONFIG_ERR_FATAL_ERROR)
            {
              rv = CONFIG_ERR_FATAL_ERROR;
              break;
            }
          rv = ret;
        }
      s = s->next;
    }

  return rv;
}
