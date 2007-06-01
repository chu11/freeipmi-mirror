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

#include "fiid-wrappers.h"

#include "pef-config.h"
#include "pef-config-commit.h"
#include "pef-config-parser.h"
#include "pef-config-sections.h"
#include "pef-config-wrapper.h"

static pef_err_t
pef_commit_file (pef_config_state_data_t *state_data)
{
  struct pef_config_arguments *args;
  int file_opened = 0;
  FILE *fp;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  pef_err_t ret = PEF_ERR_SUCCESS;
  pef_err_t this_ret;

  args = state_data->prog_data->args;

  if (args->filename && strcmp (args->filename, "-"))
    {
      if (!(fp = fopen (args->filename, "r")))
        {
          perror("fopen");
          goto cleanup;
        }
      file_opened++;
    }
  else
    fp = stdin;

  if ((this_ret = pef_config_parser (state_data, fp)) == PEF_ERR_FATAL_ERROR)
    goto cleanup;

  if (this_ret == PEF_ERR_NON_FATAL_ERROR)
    ret = PEF_ERR_NON_FATAL_ERROR;

  if (ret == PEF_ERR_SUCCESS)
    {
      struct section *sect = state_data->sections;
      while (sect)
        {
          struct keyvalue *kv = sect->keyvalues;
          while (kv)
            {
              if (kv->value)
                {
                  if ((this_ret = kv->commit (state_data, sect, kv)) == PEF_ERR_FATAL_ERROR)
                    goto cleanup;

                  if (this_ret == PEF_ERR_NON_FATAL_ERROR)
                    {
                      if (args->verbose)
                        fprintf (stderr, "FATAL: Error commiting `%s:%s'\n", sect->section_name, kv->key);
                      ret = PEF_ERR_NON_FATAL_ERROR;
                    }
                }
              kv = kv->next;
            }

          if (args->verbose)
            fprintf (stderr, "Completed commit of Section: %s\n",
                     sect->section_name);

          sect = sect->next;
        }
    }

  rv = ret;
 cleanup:
  if (file_opened)
    fclose(fp);
  return rv;
}

pef_err_t
pef_commit (pef_config_state_data_t *state_data)
{
  struct pef_config_arguments *args;
  pef_err_t ret;

  args = state_data->prog_data->args;
  ret = pef_commit_file (state_data);

  return ret;
}
