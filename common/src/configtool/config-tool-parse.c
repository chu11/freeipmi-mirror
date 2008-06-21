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

#include "config-tool-parse.h"
#include "config-tool-section.h"
#include "config-tool-utils.h"

#include "freeipmi-portability.h"
#include "pstdout.h"

config_err_t
config_parse (pstdout_state_t pstate,
              struct config_section *sections, 
              struct config_arguments *cmd_args,
              FILE *fp)
{ 
  char buf[CONFIG_PARSE_BUFLEN];
  int line_num = 0;
  struct config_section *section = NULL;
  struct config_key *key;
  char *str, *tok;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  while (fgets (buf, CONFIG_PARSE_BUFLEN, fp)) 
    {
      line_num++;

      buf[CONFIG_PARSE_BUFLEN-1] = '\0';

      str = strtok (buf, " \t\n");
      
      if (!str) 
        {
          if (cmd_args->common.debug)
            PSTDOUT_FPRINTF (pstate,
                             stderr, 
                             "%d: empty line\n", 
                             line_num);
          continue;
        }
    
      if (str[0] == '#') 
        {
          if (cmd_args->common.debug)
            PSTDOUT_FPRINTF (pstate,
                             stderr, 
                             "Comment on line %d\n", 
                             line_num);
          continue;
        }
      
      if (same (str, "Section")) 
        {
          if (!(tok = strtok (NULL, " \t\n")))
            {
              PSTDOUT_FPRINTF (pstate,
                               stderr,
                               "FATAL: Error parsing line number %d\n",
                               line_num);
              goto cleanup;
            }

          if (!(section = config_find_section(pstate,
                                              sections, 
                                              tok)))
            {
              PSTDOUT_FPRINTF(pstate,
                              stderr, 
                              "Unknown section `%s'\n", 
                              tok);
              goto cleanup;
            }

          if (cmd_args->common.debug) 
            PSTDOUT_FPRINTF (pstate,
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
              PSTDOUT_FPRINTF (pstate,
                               stderr, 
                               "FATAL: encountered `%s' without a matching Section\n",
                               str);
              goto cleanup;
            }

          if (cmd_args->common.debug)
            PSTDOUT_FPRINTF (pstate,
                             stderr, 
                             "Leaving section `%s'\n", 
                             section->section_name);

          section = NULL;
          continue;
        } 
      /* same (str, "EndSection") */
      
      if (!section) 
        {
          PSTDOUT_FPRINTF (pstate,
                           stderr, 
                           "FATAL: Key `%s' not inside a valid Section\n",
                           str);
          goto cleanup;
        }
      
      if (!(key = config_find_key(pstate,
                                  section, 
                                  str)))
        {
          PSTDOUT_FPRINTF(pstate,
                          stderr,
                          "Unknown key `%s' in section `%s'\n",
                          str,
                          section->section_name);
          goto cleanup;
        }

      tok = strtok(NULL, " \t\n");
      if (!tok)
        tok = "";
      
      if (cmd_args->common.debug)
        PSTDOUT_FPRINTF(pstate,
                        stderr,
                        "Parsed `%s:%s=%s'\n",
                        section->section_name,
                        key->key_name,
                        tok);

      if (config_find_keyvalue(pstate,
                               section, 
                               key->key_name))
        {
          PSTDOUT_FPRINTF(pstate,
                          stderr,
                          "Key '%s' specified twice in section '%s'\n",
                          key->key_name,
                          section->section_name);
          goto cleanup;
        }

      if (config_section_add_keyvalue (pstate,
                                       section,
                                       key,
                                       tok,
                                       NULL) < 0) 
        goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return rv;
}
