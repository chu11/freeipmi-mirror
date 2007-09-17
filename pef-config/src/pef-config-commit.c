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
pef_commit_keypair (pef_config_state_data_t *state_data,
                    struct keypair *kp)
{
  char *keypair = NULL;
  char *section_name;
  char *key_name;
  char *value;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;

  if (!(keypair = strdup (kp->keypair)))
    {
      perror("strdup");
      goto cleanup;
    }

  section_name = strtok (keypair, ":");
  key_name = strtok (NULL, "=");
  value = strtok (NULL, "");

  if (!(section_name && key_name && value))
    {
      fprintf (stderr, "Invalid KEY-PAIR spec `%s'\n", kp->keypair);
      rv = PEF_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  section_name = strtok (section_name, " \t");
  key_name = strtok (key_name, " \t");
  value = strtok (value, " \t");

  rv = pef_config_section_commit_value (state_data,
                                        section_name,
                                        key_name,
                                        value);
 cleanup:
  if (keypair)
    free(keypair);
  return rv;
}

static pef_err_t
pef_commit_keypairs (pef_config_state_data_t *state_data)
{
  struct pef_config_arguments *args;
  struct keypair *kp;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  pef_err_t ret = PEF_ERR_SUCCESS;

  args = state_data->prog_data->args;

  kp = args->keypairs;
  while (kp)
    {
      pef_err_t this_ret;

      if ((this_ret = pef_commit_keypair(state_data, kp)) == PEF_ERR_FATAL_ERROR)
        goto cleanup;

      if (this_ret == PEF_ERR_NON_FATAL_ERROR)
        ret = PEF_ERR_NON_FATAL_ERROR;

      kp = kp->next;
    }

  rv = ret;
 cleanup:
  return rv;
}

static pef_err_t
pef_keypair_feed (pef_config_state_data_t *state_data)
{
  struct pef_config_arguments *args;
  struct keypair *kp;
  pef_err_t rv = PEF_ERR_FATAL_ERROR;
  pef_err_t ret = PEF_ERR_SUCCESS;

  args = state_data->prog_data->args;

  kp = args->keypairs;
  while (kp)
    {
      struct section *sect;
      char *keypair;
      char *section_name;
      char *key_name;
      char *value;
      int found_section;

      if (!(keypair = strdup (kp->keypair)))
        {
          perror("strdup");
          goto cleanup;
        }

      section_name = strtok (keypair, ":");
      key_name = strtok (NULL, "=");
      value = strtok (NULL, "");

      if (!(section_name && key_name && value))
        {
          fprintf (stderr, "Invalid KEY-PAIR spec `%s'\n", kp->keypair);
          free (keypair);
          rv = PEF_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      section_name = strtok (section_name, " \t");
      key_name = strtok (key_name, " \t");
      value = strtok (value, " \t");

      sect = state_data->sections;
      found_section = 0;
      while (sect)
        {
          if (!strcasecmp(sect->section_name, section_name))
            {
              struct keyvalue *kv = sect->keyvalues;

              int found_key = 0;

              found_section++;

              while (kv)
                {
                  if (!strcasecmp(kv->key, key_name))
                    {
                      found_key++;

                      if (kv->value)
                        free(kv->value);

                      if (!(kv->value = strdup(value)))
                        {
                          perror("strdup");
                          free(keypair);
                          goto cleanup;
                        }

                      break;    /* break out of 'kv' loop */
                    }
                  kv = kv->next;
                }

              if (!found_key)
                {
                  fprintf (stderr, "Invalid KEY in `%s'\n", kp->keypair);
                  free (keypair);
                  rv = PEF_ERR_NON_FATAL_ERROR;
                  goto cleanup;
                }

              break;            /* break out of 'sect' loop */
            }
          sect = sect->next;
        }

      if (!found_section)
        {
          fprintf (stderr, "Invalid SECTION in `%s'\n", kp->keypair);
          free (keypair);
          rv = PEF_ERR_NON_FATAL_ERROR;
          goto cleanup;
        }

      free (keypair);
      keypair = NULL;
      kp = kp->next;
    }

  rv = ret;
 cleanup:
  return rv;
}


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
  if (args->filename)
    ret = pef_commit_file (state_data);
  else
    ret = pef_commit_keypairs (state_data);

  return ret;
}
