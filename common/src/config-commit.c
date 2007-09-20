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

#include "config-commit.h"

config_err_t
config_commit_section(struct config_section *section, 
                      FILE *fp,
                      int debug,
                      void *arg)
{
  assert(section);
  assert(fp);

  return section->commit(section->section_name,
                         section->keyvalues,
                         debug,
                         arg);
}

config_err_t 
config_commit_all(struct config_section *sections, 
                  FILE *fp,
                  int debug,
                  void *arg)
{
  struct config_section *s;
  config_err_t rv = CONFIG_ERR_SUCCESS;
  config_err_t ret;
  
  assert(sections);
  assert(fp);
  
  s = sections;
  while (s)
    {
      if ((ret = config_commit_section(s, fp, debug, arg)) != CONFIG_ERR_SUCCESS)
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
