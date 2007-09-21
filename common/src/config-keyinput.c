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

#include "config-keyinput.h"

int 
config_keyinput_parse_string(char *str,
                             char **section_name,
                             char **key_name,
                             char **value)
{
  char *str_temp = NULL;
  char *section_name_tok = NULL;
  char *key_name_tok = NULL;
  char *value_tok = NULL;
  char *ptr;
  char *buf;
  int rv;

  assert(str);
  assert(section_name);
  assert(key_name);
  assert(value);

  *section_name = NULL;
  *key_name = NULL;
  *value = NULL;

  if (!(str_temp = strdup(str)))
    {
      perror("strdup");
      goto cleanup;
    }

  section_name_tok = strtok_r(str_temp, ":", &buf);
  key_name_tok = strtok_r(NULL, "=", &buf);
  value_tok = strtok_r(NULL, "\0", &buf);

  if (!(section_name_tok && key_name_tok))
    {
      fprintf(stderr,
              "Improperly input keypair '%s'\n",
              str);
      goto cleanup;
    }

  /* get rid of spaces stuck in the string */
  if (section_name_tok)
    section_name_tok = strtok_r(section_name_tok, " \t", &buf);
  if (key_name_tok)
    key_name_tok = strtok_r(key_name_tok, " \t", &buf);
  if (value_tok)
    value_tok = strtok_r(value_tok, " \t", &buf);

  if (section_name_tok)
    {
      if (!(ptr = strdup(section_name_tok)))
        {
          perror("strdup");
          goto cleanup;
        }
      *section_name = ptr;
    }
  if (key_name_tok)
    {
      if (!(ptr = strdup(key_name_tok)))
        {
          perror("strdup");
          goto cleanup;
        }
      *key_name = ptr;
    }
  if (value_tok)
    {
      if (!(ptr = strdup(value_tok)))
        {
          perror("strdup");
          goto cleanup;
        }
      *value = ptr;
    }

  rv = 0;
 cleanup:
  if (str_temp)
    free(str_temp);
  if (rv < 0)
    {
      if (*section_name)
        {
          free(*section_name);
          *section_name = NULL;
        }
      if (*key_name)
        {
          free(*key_name);
          *key_name = NULL;
        }
      if (*value)
        {
          free(*value);
          *value = NULL;
        }
    }
  return rv;
}

int 
config_keyinput_append(struct config_keyinput **keyinputs,
                       struct config_keyinput *keyinput)
{
  assert(keyinputs);
  assert(keyinput);

  if (*keyinputs)
    {
      struct config_keyinput *ki;

      ki = *keyinputs;
      while (ki)
        {
          if (!strcasecmp(ki->section_name, keyinput->section_name)
              && !strcasecmp(ki->key_name, keyinput->key_name))
            {
              fprintf(stderr,
                      "Duplicate section:key pair '%s:%s' specified\n",
                      ki->section_name, ki->key_name);
              return -1;
            }
          ki = ki->next;
        }

      ki = *keyinputs;
      while (ki->next)
        ki = ki->next;
      ki->next = keyinput;
    }
  else
    *keyinputs = keyinput;

  return 0;
}

void 
config_keyinputs_destroy(struct config_keyinput *keyinputs)
{
  if (keyinputs)
    {
      while (keyinputs)
        {
          struct config_keyinput *ki_next = keyinputs->next;
          config_keyinput_destroy(keyinputs);
          keyinputs = ki_next;
        }
    }
}

struct config_keyinput *
config_keyinput_create(const char *section_name,
                       const char *key_name,
                       const char *value_input)
{
  struct config_keyinput *keyinput = NULL;

  assert(section_name);
  assert(key_name);

  if (!(keyinput = (struct config_keyinput *)malloc(sizeof(struct config_keyinput))))
    {
      perror("malloc");
      goto cleanup;
    }
  memset(keyinput, '\0', sizeof(struct config_keyinput));

  if (!(keyinput->section_name = strdup(section_name)))
    {
      perror("strdup");
      goto cleanup;
    }

  if (!(keyinput->key_name = strdup(key_name)))
    {
      perror("strdup");
      goto cleanup;
    }

  if (!(keyinput->value_input = strdup(value_input)))
    {
      perror("strdup");
      goto cleanup;
    }

  return keyinput;

 cleanup:
  if (keyinput)
    config_keyinput_destroy(keyinput);
  return NULL;
}

void 
config_keyinput_destroy(struct config_keyinput *keyinput)
{
  if (keyinput)
    {
      if (keyinput->section_name)
        free(keyinput->section_name);
      if (keyinput->key_name)
        free(keyinput->key_name);
      if (keyinput->value_input)
        free(keyinput->value_input);
      free(keyinput);
    }
}

