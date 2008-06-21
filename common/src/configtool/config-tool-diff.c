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
#include <assert.h>

#include "config-tool-diff.h"
#include "config-tool-checkout.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

config_err_t
config_diff (pstdout_state_t pstate,
             struct config_section *sections,
             struct config_arguments *cmd_args,
             void *arg)
{
  struct config_section *s;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;
  config_err_t this_ret;

  assert(sections);
  assert(cmd_args);

  s = sections;
  while (s) 
    {
      struct config_keyvalue *kv = s->keyvalues;
      while (kv) 
        {
          assert(kv->value_input);

          if ((this_ret = kv->key->checkout (s->section_name,
                                             kv,
                                             arg)) == CONFIG_ERR_FATAL_ERROR)
            goto cleanup;
          
          if (this_ret == CONFIG_ERR_SUCCESS)
            {
              if (!same (kv->value_input, kv->value_output))
                PSTDOUT_PRINTF (pstate,
                                "%s:%s - input=`%s':actual=`%s'\n",
                                s->section_name,
                                kv->key->key_name,
                                kv->value_input,
                                kv->value_output);
            }
          else
            {
              PSTDOUT_PRINTF(pstate,
                             "\t## ERROR: Unable to checkout %s:%s\n",
                             s->section_name,
                             kv->key->key_name);
              ret = this_ret;
            }
          kv = kv->next;
        }

      if (cmd_args->verbose)
        PSTDOUT_FPRINTF (pstate,
                         stderr, 
                         "Completed diff of Section: %s\n",
                         s->section_name);

      s = s->next;
    }
  
  rv = ret;
 cleanup:
  return rv;
}


