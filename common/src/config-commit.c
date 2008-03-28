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

#include "config-commit.h"
#include "config-utils.h"

config_err_t
config_commit_section(struct config_section *section,
                      struct config_arguments *cmd_args,
                      FILE *fp,
                      void *arg)
{
  struct config_keyvalue *kv;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;

  assert(section);
  assert(cmd_args);
  assert(fp);

  kv = section->keyvalues;
  while (kv) 
    {
      assert(kv->value_input);

      if (!(kv->key->flags & CONFIG_READABLE_ONLY))
        {
          if ((ret = kv->key->commit (section->section_name,
                                      kv,
                                      arg)) == CONFIG_ERR_FATAL_ERROR)
            goto cleanup;
          
          if (ret == CONFIG_ERR_NON_FATAL_ERROR)
            {
              fprintf (stderr, "ERROR: Failed to commit `%s:%s'\n", 
                       section->section_name, kv->key->key_name);
              ret = CONFIG_ERR_NON_FATAL_ERROR;
            }
        }
      else
        fprintf (stderr, "ERROR: `%s:%s' is readable only\n", 
                 section->section_name, kv->key->key_name);
      
      kv = kv->next;
    }
  
  if (cmd_args->verbose)
    fprintf (stderr, "Completed commit of Section: %s\n",
             section->section_name);
  
  rv = ret;
 cleanup:
  return rv;
}


config_err_t
config_commit (struct config_section *sections,
               struct config_arguments *cmd_args,
               FILE *fp,
               void *arg)
{
  struct config_section *s;
  config_err_t rv = CONFIG_ERR_SUCCESS;
  config_err_t ret;

  assert(sections);
  assert(cmd_args);
  assert(fp);

  s = sections;
  while (s)
    {
      if ((ret = config_commit_section(s, cmd_args, fp, arg)) != CONFIG_ERR_SUCCESS)
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
