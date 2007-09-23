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

#include "bmc-config.h"
#include "bmc-config-commit.h"
#include "bmc-config-common.h"
#include "bmc-config-parser.h"
#include "bmc-config-sections.h"

static config_err_t
bmc_commit_keypairs (bmc_config_state_data_t *state_data)
{
  struct config_arguments *args;
  struct config_keypair *kp;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;

  args = state_data->prog_data->args;

  kp = args->keypairs;
  while (kp)
    {
      config_err_t this_ret;

      if ((this_ret = bmc_config_section_commit_value (state_data, 
                                                       kp->section_name,
                                                       kp->key_name, 
                                                       kp->value_input)) == CONFIG_ERR_FATAL_ERROR)
        goto cleanup;
      
      if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
        ret = CONFIG_ERR_NON_FATAL_ERROR;

      kp = kp->next;
    }

  rv = ret;
 cleanup:
  return rv;
}

static config_err_t
bmc_keypair_feed (bmc_config_state_data_t *state_data)
{
  struct config_arguments *args;
  struct config_keypair *kp;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;

  args = state_data->prog_data->args;

  kp = args->keypairs;
  while (kp)
    {
      struct config_section *section;
      int found_section;

      section = state_data->sections;
      found_section = 0;
      while (section) 
        {
          if (!strcasecmp(section->section_name, kp->section_name))
            {
              struct config_keyvalue *kv = section->keyvalues;

              int found_key = 0;

              found_section++;

              while (kv) 
                {
                  if (!strcasecmp(kv->key_name, kp->key_name))
                    {
                      found_key++;

                      if (kv->value)
                        free(kv->value);
                      
                      if (!(kv->value = strdup(kp->value_input)))
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
                  fprintf (stderr, "Invalid KEY `%s'\n", kp->key_name);
                  rv = CONFIG_ERR_NON_FATAL_ERROR;
                  goto cleanup;
                }

              break;            /* break out of 'sect' loop */
            }
          section = section->next;
        }
      
      if (!found_section)
        {
          fprintf (stderr, "Invalid SECTION `%s'\n", kp->section_name);
          rv = CONFIG_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      kp = kp->next;
    }

  rv = ret;
 cleanup:
  return rv;
}

static config_err_t
bmc_commit_file (bmc_config_state_data_t *state_data)
{
  struct config_arguments *args;
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
  if ((this_ret = bmc_config_parser (state_data, fp)) == CONFIG_ERR_FATAL_ERROR)
    goto cleanup;

  if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
    ret = CONFIG_ERR_NON_FATAL_ERROR;

  /* 2nd pass - feed in keypair elements from the command line to override file keypairs */
  if (args->keypairs)
    {
      if ((this_ret = bmc_keypair_feed (state_data)) == CONFIG_ERR_FATAL_ERROR)
        goto cleanup;

      if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
        ret = CONFIG_ERR_NON_FATAL_ERROR;
    }

  if (ret == CONFIG_ERR_SUCCESS) 
    {
      /* 3rd pass */
      struct config_section *section = state_data->sections;
      while (section) 
        {
          struct config_keyvalue *kv = section->keyvalues;
          while (kv) 
            {
              if (kv->value) 
                {
                  if ((this_ret = kv->commit (section, kv, state_data)) == CONFIG_ERR_FATAL_ERROR)
                    goto cleanup;

                  if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
                    {
                      fprintf (stderr, "FATAL: Error commiting `%s:%s'\n", section->section_name, kv->key_name);
                      ret = CONFIG_ERR_NON_FATAL_ERROR;
                    }
                }
              kv = kv->next;
            }

          if (args->verbose)
            fprintf (stderr, "Completed commit of Section: %s\n",
                     section->section_name);

          section = section->next;
        }
    }

  rv = ret;
 cleanup:
  if (file_opened)
    fclose(fp);
  return rv;
}

config_err_t
bmc_commit (bmc_config_state_data_t *state_data)
{
  struct config_arguments *args;
  config_err_t ret;

  args = state_data->prog_data->args;
  if (args->filename)
    ret = bmc_commit_file (state_data);
  else
    ret = bmc_commit_keypairs (state_data);

  return ret;
}
