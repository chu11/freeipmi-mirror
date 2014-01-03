/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
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

#include "ipmi-config-commit.h"
#include "ipmi-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

static ipmi_config_err_t
_ipmi_config_commit_section (ipmi_config_state_data_t *state_data,
			     struct ipmi_config_section *section)
{
  struct ipmi_config_keyvalue *kv;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret = IPMI_CONFIG_ERR_SUCCESS;
  ipmi_config_err_t this_ret;
  unsigned int commit_count = 0;

  assert (state_data);
  assert (section);

  if (section->section_pre_commit)
    {
      if ((this_ret = section->section_pre_commit (state_data,
						   section->section_name)) == IPMI_CONFIG_ERR_FATAL_ERROR)
        goto cleanup;

      if (IPMI_CONFIG_IS_NON_FATAL_ERROR (this_ret))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ERROR: Section pre-commit `%s'\n",
                           section->section_name);
          ret = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
        }
    }

  kv = section->keyvalues;
  while (kv)
    {
      assert (kv->value_input);

      if (!(kv->key->flags & IPMI_CONFIG_READABLE_ONLY)
          && !(kv->key->flags & IPMI_CONFIG_UNDEFINED))
        {
          if ((this_ret = kv->key->commit (state_data,
					   section->section_name,
                                           kv)) == IPMI_CONFIG_ERR_FATAL_ERROR)
            goto cleanup;

          if (this_ret == IPMI_CONFIG_ERR_SUCCESS)
            {
              /* Discovered on Quanta S99Q/Dell FS12-TY
               *
               * A number of values on this motherboard appear to take
               * a reasonable amount of time to store, causing the BMC
               * to return BUSY errors for a bit.  In some cases, the
               * BMC eventually hangs or subsequent writes are
               * ignored.  We want to try to avoid this.
               */
              
              if (state_data->prog_data->args->common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_VERY_SLOW_COMMIT)
                sleep (1);
              
              commit_count++;
            }

          if (IPMI_CONFIG_IS_NON_FATAL_ERROR (this_ret))
            {
              if (this_ret == IPMI_CONFIG_ERR_NON_FATAL_ERROR_READ_ONLY)
                pstdout_fprintf (state_data->pstate,
                                 stderr,
                                 "ERROR: Failed to commit `%s:%s': Read Only Field\n",
                                 section->section_name,
                                 kv->key->key_name);
              else if (this_ret == IPMI_CONFIG_ERR_NON_FATAL_ERROR_NOT_SUPPORTED)
                pstdout_fprintf (state_data->pstate,
                                 stderr,
                                 "ERROR: Failed to commit `%s:%s': Not Supported\n",
                                 section->section_name,
                                 kv->key->key_name);
              else if (this_ret == IPMI_CONFIG_ERR_NON_FATAL_ERROR_INVALID_UNSUPPORTED_CONFIG)
                pstdout_fprintf (state_data->pstate,
                                 stderr,
                                 "ERROR: Failed to commit `%s:%s': Invalid/Unsupported Config\n",
                                 section->section_name,
                                 kv->key->key_name);
              else
                pstdout_fprintf (state_data->pstate,
                                 stderr,
                                 "ERROR: Failed to commit `%s:%s'\n",
                                 section->section_name,
                                 kv->key->key_name);
              ret = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
            }
        }
      else
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ERROR: `%s:%s' is not writeable\n",
                           section->section_name,
                           kv->key->key_name);
          ret = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
        }

      kv = kv->next;
    }

  if (commit_count && section->section_post_commit)
    {
      if ((this_ret = section->section_post_commit (state_data,
						    section->section_name)) == IPMI_CONFIG_ERR_FATAL_ERROR)
        goto cleanup;

      if (IPMI_CONFIG_IS_NON_FATAL_ERROR (this_ret))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "ERROR: Section post-commit `%s'\n",
                           section->section_name);
          ret = IPMI_CONFIG_ERR_NON_FATAL_ERROR;
        }
    }

  rv = ret;
 cleanup:
  return (rv);
}

ipmi_config_err_t
ipmi_config_commit (ipmi_config_state_data_t *state_data)
{
  struct ipmi_config_section *s;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_SUCCESS;
  ipmi_config_err_t ret;

  assert (state_data);

  s = state_data->sections;
  while (s)
    {
      if ((ret = _ipmi_config_commit_section (state_data,
					      s)) != IPMI_CONFIG_ERR_SUCCESS)
        {
          if (ret == IPMI_CONFIG_ERR_FATAL_ERROR)
            {
              rv = IPMI_CONFIG_ERR_FATAL_ERROR;
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

      if (state_data->prog_data->args->common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_SLOW_COMMIT
          || state_data->prog_data->args->common_args.section_specific_workaround_flags & IPMI_PARSE_SECTION_SPECIFIC_WORKAROUND_FLAGS_VERY_SLOW_COMMIT)
        sleep (1);

      s = s->next;
    }

  return (rv);
}
