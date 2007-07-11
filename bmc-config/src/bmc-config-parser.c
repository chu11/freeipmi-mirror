#include "bmc-config.h"
#include "bmc-config-common.h"
#include "bmc-config-wrapper.h"
#include "bmc-config-diff.h"
#include "bmc-config-sections.h"

bmc_err_t
bmc_config_parser (bmc_config_state_data_t *state_data, FILE *fp)
{ 
  char buf[4096];
  int line_num = 0;
  char *section_name = NULL;
  char *key_name = NULL;
  char *value = NULL;
  char *tok;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
#ifndef NDEBUG 
  struct bmc_config_arguments *args;

  args = state_data->prog_data->args;
#endif /* NDEBUG */

  while (fgets (buf, 4096, fp)) 
    {
      line_num ++;
      buf[4095] = 0;
      char *first_word = strtok (buf, " \t\n");
      
      if (!first_word) 
        {
#ifndef NDEBUG 
          if (args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
            fprintf (stderr, "%d: empty line\n", line_num);
#endif /* NDEBUG */
          continue;
        }
    
      if (first_word[0] == '#') 
        {
#ifndef NDEBUG 
          if (args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
            fprintf (stderr, "Comment on line %d\n", line_num);
#endif /* NDEBUG */
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

#ifndef NDEBUG 
          if (args->common.flags & IPMI_FLAGS_DEBUG_DUMP) 
            fprintf (stderr, "Entering section `%s'\n", section_name);
#endif /* NDEBUG */

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
#ifndef NDEBUG 
          if (args->common.flags & IPMI_FLAGS_DEBUG_DUMP)
            fprintf (stderr, "Leaving section `%s'\n", section_name);
#endif /* NDEBUG */

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
      
#ifndef NDEBUG 
      if (args->common.flags & IPMI_FLAGS_DEBUG_DUMP) 
        fprintf (stderr, "Trying to set `%s:%s=%s'\n",
                 section_name, key_name, value);
#endif /* NDEBUG */
      
      if (bmc_config_section_set_value (state_data,
                                        section_name,
                                        key_name,
                                        value) < 0) 
        goto cleanup;
    }

  rv = BMC_ERR_SUCCESS;
 cleanup:
  if (section_name)
    free(section_name);
  if (key_name)
    free(key_name);
  if (value)
    free(value);
  return rv;
}
