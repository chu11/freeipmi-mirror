/*
 * Copyright (C) 2003-2012 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
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
#endif /* HAVE_UNISTD_H */
#include <errno.h>
#include <assert.h>

#include "config-tool-commit.h"
#include "config-tool-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

config_err_t
config_commit_section (pstdout_state_t pstate,
                       struct config_section *section,
                       struct config_arguments *cmd_args,
                       void *arg)
{
  struct config_keyvalue *kv;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;
  config_err_t this_ret;
  unsigned int commit_count = 0;

  assert (section);
  assert (cmd_args);

  if (section->section_pre_commit)
    {
      if ((this_ret = section->section_pre_commit (section->section_name,
                                                   arg)) == CONFIG_ERR_FATAL_ERROR)
        goto cleanup;

      if (CONFIG_IS_NON_FATAL_ERROR (this_ret))
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
      assert (kv->value_input);

      if (!(kv->key->flags & CONFIG_READABLE_ONLY)
          && !(kv->key->flags & CONFIG_UNDEFINED))
        {
          if ((this_ret = kv->key->commit (section->section_name,
                                           kv,
                                           arg)) == CONFIG_ERR_FATAL_ERROR)
            goto cleanup;

          if (this_ret == CONFIG_ERR_SUCCESS)
	    {
	      /* Discovered on Quanta S99Q/Dell FS12-TY
	       *
	       * A number of values on this motherboard appear to take
	       * a reasonable amount of time to store, causing the BMC
	       * to return BUSY errors for a bit.  In some cases, the
	       * BMC eventually hangs or subsequent writes are
	       * ignored.  We want to try to avoid this.
	       */
	      
	      if (cmd_args->common.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_VERY_SLOW_COMMIT)
		sleep (1);
	      
	      commit_count++;
	    }

          if (CONFIG_IS_NON_FATAL_ERROR (this_ret))
            {
              if (this_ret == CONFIG_ERR_NON_FATAL_ERROR_READ_ONLY)
                PSTDOUT_FPRINTF (pstate,
                                 stderr,
                                 "ERROR: Failed to commit `%s:%s': Read Only Field\n",
                                 section->section_name,
                                 kv->key->key_name);
              else if (this_ret == CONFIG_ERR_NON_FATAL_ERROR_NOT_SUPPORTED)
                PSTDOUT_FPRINTF (pstate,
                                 stderr,
                                 "ERROR: Failed to commit `%s:%s': Not Supported\n",
                                 section->section_name,
                                 kv->key->key_name);
              else if (this_ret == CONFIG_ERR_NON_FATAL_ERROR_INVALID_UNSUPPORTED_CONFIG)
                PSTDOUT_FPRINTF (pstate,
                                 stderr,
                                 "ERROR: Failed to commit `%s:%s': Invalid/Unsupported Config\n",
                                 section->section_name,
                                 kv->key->key_name);
              else
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

  if (commit_count && section->section_post_commit)
    {
      if ((this_ret = section->section_post_commit (section->section_name,
                                                    arg)) == CONFIG_ERR_FATAL_ERROR)
        goto cleanup;

      if (CONFIG_IS_NON_FATAL_ERROR (this_ret))
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr,
                           "ERROR: Section post-commit `%s'\n",
                           section->section_name);
          ret = CONFIG_ERR_NON_FATAL_ERROR;
        }
    }

  rv = ret;
 cleanup:
  return (rv);
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

  assert (sections);
  assert (cmd_args);

  s = sections;
  while (s)
    {
      if ((ret = config_commit_section (pstate,
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

      /* IPMI Workaround (achu)
       *
       * Discovered on Supermicro H8QME with SIMSO daughter card.
       *
       * Some BMCs appear to not be able to accept a high number of
       * commits/writes and eventually commits/writes are lost.  This
       * workaround will slow down the commits/writes to give the BMC
       * a better chance to accept all changes.
       *
       * Discovered on Quanta S99Q/Dell FS12-TY
       *
       * A number of values on this motherboard appear to take
       * a reasonable amount of time to store, causing the BMC
       * to return BUSY errors for a bit.  In some cases, the
       * BMC eventually hangs or subsequent writes are
       * ignored.  We want to try to avoid this.
       */

      if (cmd_args->common.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SLOW_COMMIT
	  || cmd_args->common.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_VERY_SLOW_COMMIT)
	sleep (1);

      s = s->next;
    }

  return (rv);
}
