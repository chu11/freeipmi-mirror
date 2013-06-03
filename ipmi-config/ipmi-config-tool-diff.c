/*
 * Copyright (C) 2003-2013 FreeIPMI Core Team
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
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "ipmi-config-tool-diff.h"
#include "ipmi-config-tool-checkout.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

ipmi_config_err_t
ipmi_config_diff (pstdout_state_t pstate,
                  struct ipmi_config_section *sections,
                  struct ipmi_config_arguments *cmd_args,
                  void *arg)
{
  struct ipmi_config_section *s;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;
  ipmi_config_err_t ret = IPMI_CONFIG_ERR_SUCCESS;
  ipmi_config_err_t this_ret;

  assert (sections);
  assert (cmd_args);

  s = sections;
  while (s)
    {
      struct ipmi_config_keyvalue *kv = s->keyvalues;
      while (kv)
        {
          assert (kv->value_input);

          if ((this_ret = kv->key->checkout (s->section_name,
                                             kv,
                                             arg)) == IPMI_CONFIG_ERR_FATAL_ERROR)
            goto cleanup;

          if (this_ret == IPMI_CONFIG_ERR_SUCCESS)
            {
              if (!same (kv->value_input, kv->value_output))
                pstdout_printf (pstate,
                                "%s:%s - input=`%s':actual=`%s'\n",
                                s->section_name,
                                kv->key->key_name,
                                kv->value_input,
                                kv->value_output);
            }
          else
            {
              pstdout_printf (pstate,
                              "\t## ERROR: Unable to checkout %s:%s\n",
                              s->section_name,
                              kv->key->key_name);
              ret = this_ret;
            }
          kv = kv->next;
        }

      s = s->next;
    }

  rv = ret;
 cleanup:
  return (rv);
}


