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
config_keyinput_append(struct config_keyinput **keyinputs,
                       struct config_keyinput *keyinput)
{
  assert(keyinputs);
  assert(keyinput);

  if (*keyinputs)
    {
      struct config_keyinput *s = *keyinputs;
      while (s->next)
        s = s->next;
      s->next = keyinput;
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

