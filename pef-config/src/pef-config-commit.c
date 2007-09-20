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

#include "config-common.h"

static config_err_t
pef_commit_keyinputs (pef_config_state_data_t *state_data)
{
  struct pef_config_arguments *args;
  struct config_keyinput *ki;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;

  args = state_data->prog_data->args;

  ki = args->keyinputs;
  while (ki)
    {
      config_err_t this_ret;
      
      if ((this_ret = pef_config_section_commit_value (state_data,
                                                       ki->section_name,
                                                       ki->key_name,
                                                       ki->value_input)) == CONFIG_ERR_FATAL_ERROR)
        goto cleanup;
      
      if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
        ret = CONFIG_ERR_NON_FATAL_ERROR;
      
      ki = ki->next;
    }

  rv = ret;
 cleanup:
  return rv;
}

static config_err_t
pef_keyinput_feed (pef_config_state_data_t *state_data)
{
  struct pef_config_arguments *args;
  struct config_keyinput *ki;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;

  args = state_data->prog_data->args;

  ki = args->keyinputs;
  while (ki)
    {
      struct section *sect;
      int found_section;

      sect = state_data->sections;
      found_section = 0;
      while (sect)
        {
          if (!strcasecmp(sect->section_name, ki->section_name))
            {
              struct keyvalue *kv = sect->keyvalues;

              int found_key = 0;

              found_section++;

              while (kv)
                {
                  if (!strcasecmp(kv->key, ki->key_name))
                    {
                      found_key++;

                      if (kv->value)
                        free(kv->value);

                      if (!(kv->value = strdup(ki->value_input)))
                        {
                          perror("strdup");
                          goto cleanup;
                        }

                      break;    /* break out of 'kv' loop */
                    }
                  kv = kv->next;
                }

              if (!found_key)
                {
                  fprintf (stderr, "Invalid KEY `%s'\n", ki->key_name);
                  rv = CONFIG_ERR_NON_FATAL_ERROR;
                  goto cleanup;
                }

              break;            /* break out of 'sect' loop */
            }
          sect = sect->next;
        }

      if (!found_section)
        {
          fprintf (stderr, "Invalid SECTION in `%s'\n", ki->section_name);
          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      ki = ki->next;
    }

  rv = ret;
 cleanup:
  return rv;
}


static config_err_t
pef_commit_file (pef_config_state_data_t *state_data)
{
  struct pef_config_arguments *args;
  int file_opened = 0;
  FILE *fp;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;
  config_err_t this_ret;

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

  /* 1st pass - read in input from file */
  if ((this_ret = pef_config_parser (state_data, fp)) == CONFIG_ERR_FATAL_ERROR)
    goto cleanup;

  if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
    ret = CONFIG_ERR_NON_FATAL_ERROR;

  /* 2nd pass - feed in keyinput elements from the command line to override file keyinputs */
  if (args->keyinputs)
    {
      if ((this_ret = pef_keyinput_feed (state_data)) == CONFIG_ERR_FATAL_ERROR)
        goto cleanup;

      if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
        ret = CONFIG_ERR_NON_FATAL_ERROR;
    }

  if (ret == CONFIG_ERR_SUCCESS)
    {
      /* 3rd pass */
      struct section *sect = state_data->sections;
      while (sect)
        {
          struct keyvalue *kv = sect->keyvalues;
          while (kv)
            {
              if (kv->value)
                {
                  if ((this_ret = kv->commit (state_data, sect, kv)) == CONFIG_ERR_FATAL_ERROR)
                    goto cleanup;

                  if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
                    {
                      if (args->verbose)
                        fprintf (stderr, "FATAL: Error commiting `%s:%s'\n", sect->section_name, kv->key);
                      ret = CONFIG_ERR_NON_FATAL_ERROR;
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

config_err_t
pef_commit (pef_config_state_data_t *state_data)
{
  struct pef_config_arguments *args;
  config_err_t ret;

  args = state_data->prog_data->args;
  if (args->filename)
    ret = pef_commit_file (state_data);
  else
    ret = pef_commit_keyinputs (state_data);

  return ret;
}
