#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <errno.h>
#include <assert.h>

#include "config-parse.h"

#define CONFIG_PARSE_BUFLEN 4096

static struct config_section *
_find_section(struct config_section *sections, const char *section_name)
{
  struct config_section *s = NULL;

  assert(sections);
  assert(section_name);

  s = sections;
  while (s)
    {
      if (!strcasecmp(section_name, s->section_name))
        break;
      s = s->next;
    }

  return s;
}

static struct config_key *
_find_key(struct config_section *section, const char *key_name)
{
  struct config_key *k = NULL;

  assert(section);
  assert(key_name);

  k = section->keys;
  while (k)
    {
      if (!strcasecmp(key_name, k->key_name))
        break;
      k = k->next;
    }

  return k;
}

static int
_set_keyvalue(struct config_section *section, 
              struct config_key *key,
              const char *value)
{
  struct config_keyvalue *kv = NULL;

  assert(section);
  assert(key);
  assert(value);
  
  if (!(kv = malloc(sizeof(struct config_keyvalue))))
    {
      perror("malloc");
      goto cleanup;
    }
  memset(kv, '\0', sizeof(struct config_keyvalue));

  if (!(kv->key_name = strdup(key->key_name)))
    {
      perror("strdup");
      goto cleanup;
    }

  if (!(kv->value = strdup(value)))
    {
      perror("strdup");
      goto cleanup;
    }

  kv->next = NULL;

  if (section->keyvalues_last)
    {
      (section->keyvalues_last)->next = kv;
      section->keyvalues_last = kv;
    }
  else
    {
      section->keyvalues = kv;
      section->keyvalues_last = kv;
    }

  return 0;

 cleanup:
  if (kv)
    {
      if (kv->key_name)
        free(kv->key_name);
      if (kv->value)
        free(kv->value);
      free(kv);
    }
  return -1;
}

int 
config_parse (struct config_section *sections, FILE *fp, int debug)
{
  char buf[CONFIG_PARSE_BUFLEN];
  int line_num = 0;
  struct config_section *section;
  struct config_key *key;
  char *str, *tok;
  int rv = -1;

  assert(sections);
  assert(fp);

  memset(buf, '\0', CONFIG_PARSE_BUFLEN);

  while (fgets(buf, CONFIG_PARSE_BUFLEN, fp)) 
    {
      line_num++;

      str = strtok(buf, " \t\n");
      
      if (!str) 
        {
          if (debug)
            fprintf(stderr, "%d: empty line\n", line_num);
          continue;
        }
    
      if (str[0] == '#') 
        {
          if (debug)
            fprintf(stderr, "Comment on line %d\n", line_num);
          continue;
        }
      
      if (!strcasecmp(str, "Section")) 
        {
          if (!(tok = strtok(NULL, " \t\n")))
            {
              fprintf(stderr, 
                      "FATAL: Error parsing line number %d\n",
                      line_num);
              goto cleanup;
            }
          
          if (!(section = _find_section(sections, tok)))
            {
              fprintf(stderr, "Unknown section `%s'\n", tok);
              goto cleanup;
            }

          if (debug)
            fprintf(stderr, "Entering section `%s'\n", section->section_name);

          continue;
        } 
      /* "Section" */
      
      if (!strcasecmp(str, "EndSection")) 
        {
          if (!section) 
            {
              fprintf(stderr, 
                      "FATAL: encountered `%s' without a matching Section\n",
                      str);
              goto cleanup;
            }

          if (debug)
            fprintf(stderr, "Leaving section `%s'\n", section->section_name);

          section = NULL;
          continue;
        } 
      /* "EndSection" */
      
      if (!section) 
        {
          fprintf(stderr, 
                  "FATAL: Key `%s' not inside any Section\n",
                  str);
          goto cleanup;
        }

      if (!(key = _find_key(section, str)))
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

      if (debug)
        fprintf(stderr, 
                "Parsed `%s:%s=%s'\n", 
                section->section_name, 
                key->key_name, 
                tok);

      if (_set_keyvalue(section, key, tok) < 0)
        goto cleanup;
    }

  rv = 0;
 cleanup:
  return rv;
}
