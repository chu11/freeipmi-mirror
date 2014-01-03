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
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <assert.h>

#include "ipmi-config-parse.h"
#include "ipmi-config-section.h"
#include "ipmi-config-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

ipmi_config_err_t
ipmi_config_parse (ipmi_config_state_data_t *state_data,
                   FILE *fp)
{
  char buf[IPMI_CONFIG_PARSE_BUFLEN];
  int line_num = 0;
  struct ipmi_config_section *section = NULL;
  struct ipmi_config_key *key;
  char *str, *tok;
  ipmi_config_err_t rv = IPMI_CONFIG_ERR_FATAL_ERROR;

  assert (state_data);

  while (fgets (buf, IPMI_CONFIG_PARSE_BUFLEN, fp))
    {
      line_num++;

      buf[IPMI_CONFIG_PARSE_BUFLEN-1] = '\0';

      str = strtok (buf, " \t\n");

      if (!str)
        {
          if (state_data->prog_data->args->common_args.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "%d: empty line\n",
                             line_num);
          continue;
        }

      if (str[0] == '#')
        {
          if (state_data->prog_data->args->common_args.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "Comment on line %d\n",
                             line_num);
          continue;
        }

      if (same (str, "Section"))
        {
          if (!(tok = strtok (NULL, " \t\n")))
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "FATAL: Error parsing line number %d\n",
                               line_num);
              goto cleanup;
            }

          if (!(section = ipmi_config_find_section (state_data,
                                                    tok)))
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "Unknown section `%s'\n",
                               tok);
              goto cleanup;
            }

          if (state_data->prog_data->args->common_args.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "Entering section `%s'\n",
                             section->section_name);

          continue;
        }
      /* same (str, "Section") */

      if (same (str, "EndSection"))
        {
          if (!section)
            {
              pstdout_fprintf (state_data->pstate,
                               stderr,
                               "FATAL: encountered `%s' without a matching Section\n",
                               str);
              goto cleanup;
            }

          if (state_data->prog_data->args->common_args.debug)
            pstdout_fprintf (state_data->pstate,
                             stderr,
                             "Leaving section `%s'\n",
                             section->section_name);

          section = NULL;
          continue;
        }
      /* same (str, "EndSection") */

      if (!section)
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "FATAL: Key `%s' not inside a valid Section\n",
                           str);
          goto cleanup;
        }

      if (!(key = ipmi_config_find_key (section,
                                        str)))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "Unknown key `%s' in section `%s'\n",
                           str,
                           section->section_name);
          goto cleanup;
        }

      tok = strtok (NULL, " \t\n");
      if (!tok)
        tok = "";

      if (state_data->prog_data->args->common_args.debug)
        pstdout_fprintf (state_data->pstate,
                         stderr,
                         "Parsed `%s:%s=%s'\n",
                         section->section_name,
                         key->key_name,
                         tok);

      if (ipmi_config_find_keyvalue (section,
                                     key->key_name))
        {
          pstdout_fprintf (state_data->pstate,
                           stderr,
                           "Key '%s' specified twice in section '%s'\n",
                           key->key_name,
                           section->section_name);
          goto cleanup;
        }

      if (ipmi_config_section_add_keyvalue (state_data,
                                            section,
                                            key,
                                            tok,
                                            NULL) < 0)
        goto cleanup;
    }

  rv = IPMI_CONFIG_ERR_SUCCESS;
 cleanup:
  return (rv);
}
