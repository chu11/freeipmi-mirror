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

#include "config-diff.h"
#include "config-checkout.h"

static int
_output_diff(struct config_section *section)
{
  struct config_keyvalue *kv;
  config_err_t rv = CONFIG_ERR_SUCCESS;

  assert(section);

  kv = section->keyvalues;
  while (kv)
    {
      if (kv->value_input)
        {
          if (kv->value_output)
            {
              if (strcasecmp(kv->value_input, kv->value_output))
                printf("%s:%s - input=`%s':actual=`%s'\n",
                       section->section_name,
                       kv->key->key_name,
                       kv->value_input,
                       kv->value_output);
            }
          else
            printf("%s:%s - cannot retrieve value\n",
                   section->section_name,
                   kv->key->key_name);
        }
      kv = kv->next;
    }

  return rv;
}

config_err_t 
config_diff(struct config_section *sections,
            int debug,
            void *arg)
{
  struct config_section *s;
  config_err_t rv = CONFIG_ERR_SUCCESS;
  config_err_t ret;

  assert(sections);

  s = sections;
  while (s)
    {
      if ((ret = config_checkout_section(s, 
                                         NULL, 
                                         debug, 
                                         arg)) != CONFIG_ERR_SUCCESS)
        {
          if (ret == CONFIG_ERR_FATAL_ERROR)
            {
              rv = CONFIG_ERR_FATAL_ERROR;
              break;
            }
          rv = ret;
        }

      if ((ret = _output_diff(s)) != CONFIG_ERR_SUCCESS)
        {
          if (ret == CONFIG_ERR_FATAL_ERROR)
            {
              rv = CONFIG_ERR_FATAL_ERROR;
              break;
            }
          rv = ret;
        }

      s = s->next;
    }

  return rv;
}
