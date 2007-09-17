#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

static struct config_section *
_find_section(struct config_section *sections, const char *section_name)
{
  while (sections)
    {
      if (!strcasecmp(section_name, sections->section_name))
        break;
      sections = sections->next;
    }

  return sections;
}

int 
config_parse (struct config_section *sections, FILE *fp, int debug)
{
  char buf[4096];
  int line_num = 0;
  char *section_name = NULL;
  char *key_name = NULL;
  char *value = NULL;
  char *tok;
  int rv = -1;

  assert(sections);
  assert(fp);

  while (fgets(buf, 4096, fp)) 
    {
      line_num ++;
      buf[4095] = 0;
      char *first_word;

      first_word = strtok(buf, " \t\n");
      
      if (!first_word) 
        {
          if (debug);
            fprintf(stderr, "%d: empty line\n", line_num);
          continue;
        }
    
      if (first_word[0] == '#') 
        {
          if (debug);
            fprintf(stderr, "Comment on line %d\n", line_num);
          continue;
        }
      
      if (!strcasecmp(first_word, "Section")) 
        {
          if (!(tok = strtok(NULL, " \t\n")))
            {
              fprintf(stderr, 
                      "FATAL: Error parsing line number %d\n",
                      line_num);
              goto cleanup;
            }
          
          if (section_name)
            {
              free (section_name);
              section_name = NULL;
            }

          if (!(section_name = strdup(tok)))
            {
              perror("strdup");
              goto cleanup;
            }

          if (!_find_section(sections, section_name))
            {
              fprintf(stderr, "Unknown section `%s'\n", section_name);
              goto cleanup;
            }

          if (debug)
            fprintf(stderr, "Entering section `%s'\n", section_name);

            

          continue;
        } 
      /* "Section" */
      
      if (!strcasecmp(first_word, "EndSection")) 
        {
          if (!section_name) 
            {
              fprintf(stderr, 
                      "FATAL: encountered `%s' without a matching Section\n",
                      first_word);
              goto cleanup;
            }

          if (debug);
            fprintf(stderr, "Leaving section `%s'\n", section_name);

          free (section_name);
          section_name = NULL;
          
          continue;
        } 
      /* "EndSection" */
      
      if (!section_name) 
        {
          fprintf(stderr, 
                  "FATAL: Key `%s' not inside any Section\n",
                  first_word);
          goto cleanup;
        }
      
      if (key_name)
        {
          free (key_name);
          key_name = NULL;
        }

      if (!(key_name = strdup(first_word)))
        {
          perror("strdup");
          goto cleanup;
        }

      if (value)
        {
          free (value);
          value = NULL;
        }

      if ((tok = strtok(NULL, " \t\n")))
        {
          if (!(value = strdup(tok)))
            {
              perror("strdup");
              goto cleanup;
            }
        }
      else
        {
          if (!(value = strdup("")))
            {
              perror("strdup");
              goto cleanup;
            }
        }
      
      if (debug);
        fprintf(stderr, 
                "Parsed `%s:%s=%s'\n", 
                section_name, 
                key_name, 
                value);
      
        /* XXX */
        /* find section */
        /* put key value pair in it */
    }

  rv = 1;
 cleanup:
  if (section_name)
    free(section_name);
  if (key_name)
    free(key_name);
  if (value)
    free(value);
  return rv;
}
