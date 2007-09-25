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

config_err_t
config_parse (struct config_section *sections, 
              struct config_arguments *cmd_args,
              FILE *fp)
{ 
  char buf[CONFIG_PARSE_BUFLEN];
  int line_num = 0;
  char *section_name = NULL;
  char *key_name = NULL;
  char *value = NULL;
  char *tok;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;

  while (fgets (buf, CONFIG_PARSE_BUFLEN, fp)) 
    {
      line_num++;
      char *first_word;

      buf[CONFIG_PARSE_BUFLEN-1] = '\0';

      first_word = strtok (buf, " \t\n");
      
      if (!first_word) 
        {
          if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
            fprintf (stderr, "%d: empty line\n", line_num);
          continue;
        }
    
      if (first_word[0] == '#') 
        {
          if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
            fprintf (stderr, "Comment on line %d\n", line_num);
          continue;
        }
      
      if (same (first_word, "Section")) 
        {
          if (!(tok = strtok (NULL, " \t\n")))
            {
              fprintf (stderr, "FATAL: Error parsing line number %d\n",
                       line_num);
              goto cleanup;
            }
          
          if (section_name)
            {
              free (section_name);
              section_name = NULL;
            }

          if (!(section_name = strdup (tok)))
            {
              perror("strdup");
              goto cleanup;
            }

          if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP) 
            fprintf (stderr, "Entering section `%s'\n", section_name);

          continue;
        } 
      /* same (first_word, "Section") */
      
      if (same (first_word, "EndSection")) 
        {
          if (!section_name) 
            {
              fprintf (stderr, "FATAL: encountered `%s' without a matching Section\n",
                       first_word);
              goto cleanup;
            }

          if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
            fprintf (stderr, "Leaving section `%s'\n", section_name);

          free (section_name);
          section_name = NULL;
          
          continue;
        } 
      /* same (first_word, "EndSection") */
      
      if (!section_name) 
        {
          fprintf (stderr, "FATAL: Key `%s' not inside any Section\n",
                 first_word);
          goto cleanup;
        }
      
      if (key_name)
        {
          free (key_name);
          key_name = NULL;
        }

      if (!(key_name = strdup (first_word)))
        {
          perror("strdup");
          goto cleanup;
        }

      if (value)
        {
          free (value);
          value = NULL;
        }

      if ((tok = strtok (NULL, " \t\n")))
        {
          if (!(value = strdup (tok)))
            {
              perror("strdup");
              goto cleanup;
            }
        }
      else
        {
          if (!(value = strdup ("")))
            {
              perror("strdup");
              goto cleanup;
            }
        }
      
      if (cmd_args->common.flags & IPMI_FLAGS_DEBUG_DUMP) 
        fprintf (stderr, "Trying to set `%s:%s=%s'\n",
                 section_name, key_name, value);
      
      if (config_section_set_value (sections,
                                        section_name,
                                        key_name,
                                        value) < 0) 
        goto cleanup;
    }

  rv = CONFIG_ERR_SUCCESS;
 cleanup:
  if (section_name)
    free(section_name);
  if (key_name)
    free(key_name);
  if (value)
    free(value);
  return rv;
}
