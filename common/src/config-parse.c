#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "config-parse.h"
#include "config-section.h"
#include "config-utils.h"

config_err_t
config_parse (struct config_section *sections, 
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
          if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
            fprintf (stderr, "%d: empty line\n", line_num);
          continue;
        }
    
      if (str[0] == '#') 
        {
          if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
            fprintf (stderr, "Comment on line %d\n", line_num);
          continue;
        }
      
      if (same (str, "Section")) 
        {
          if (!(tok = strtok (NULL, " \t\n")))
            {
              fprintf (stderr, "FATAL: Error parsing line number %d\n",
                       line_num);
              goto cleanup;
            }

          if (!(section = config_find_section(sections, tok)))
            {
              fprintf(stderr, "Unknown section `%s'\n", tok);
              goto cleanup;
            }

          if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP) 
            fprintf (stderr, "Entering section `%s'\n", section->section_name);

          continue;
        } 
      /* same (str, "Section") */
      
      if (same (str, "EndSection")) 
        {
          if (!section) 
            {
              fprintf (stderr, "FATAL: encountered `%s' without a matching Section\n",
                       str);
              goto cleanup;
            }

          if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
            fprintf (stderr, "Leaving section `%s'\n", section->section_name);

          section = NULL;
          continue;
        } 
      /* same (str, "EndSection") */
      
      if (!section) 
        {
          fprintf (stderr, "FATAL: Key `%s' not inside a valid Section\n",
                   str);
          goto cleanup;
        }
      
      if (!(key = config_find_key(section, str)))
        {
          fprintf(stderr,
                  "Unknown key `%s' in section `%s'\n",
                  str,
                  section->section_name);
          goto cleanup;
        }

      tok = strtok(NULL, " \t\n");
      if (!tok)
        tok = "";
      
      if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
        fprintf(stderr,
                "Parsed `%s:%s=%s'\n",
                section->section_name,
                key->key_name,
                tok);

      if (config_find_keyvalue(section, key->key_name))
        {
          fprintf(stderr,
                  "Key '%s' specified twice in section '%s'\n",
                  key->key_name,
                  section->section_name);
          goto cleanup;
        }

      if (config_section_add_keyvalue (section,
                                       key,
                                       tok,
                                       NULL) < 0) 
        goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  return rv;
}
