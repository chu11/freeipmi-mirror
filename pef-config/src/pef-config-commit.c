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

static config_err_t
pef_commit_keypairs (struct config_section *sections,
                     struct config_arguments *cmd_args,
                     void *arg)
{
  struct config_keypair *kp;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;

  kp = cmd_args->keypairs;
  while (kp)
    {
      config_err_t this_ret;

      if ((this_ret = pef_config_section_commit_value (sections,
                                                       kp->section_name,
                                                       kp->key_name,
                                                       kp->value_input,
                                                       arg)) == CONFIG_ERR_FATAL_ERROR)
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
pef_keypair_feed (struct config_section *sections,
                  struct config_arguments *cmd_args)
{
  struct config_keypair *kp;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;

  kp = cmd_args->keypairs;
  while (kp)
    {
      struct config_section *section;
      int found_section;

      section = sections;
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
pef_commit_file (struct config_section *sections,
                 struct config_arguments *cmd_args,
                 void *arg)
{
  int file_opened = 0;
  FILE *fp;
  config_err_t rv = CONFIG_ERR_FATAL_ERROR;
  config_err_t ret = CONFIG_ERR_SUCCESS;
  config_err_t this_ret;

  if (cmd_args->filename && strcmp (cmd_args->filename, "-"))
    {
      if (!(fp = fopen (cmd_args->filename, "r")))
        {
          perror("fopen");
          goto cleanup;
        }
      file_opened++;
    }
  else
    fp = stdin;

  /* 1st pass - read in input from file */
  if ((this_ret = pef_config_parser (sections, cmd_args, fp)) == CONFIG_ERR_FATAL_ERROR)
    goto cleanup;

  if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
    ret = CONFIG_ERR_NON_FATAL_ERROR;

  /* 2nd pass - feed in keypair elements from the command line to override file keypairs */
  if (cmd_args->keypairs)
    {
      if ((this_ret = pef_keypair_feed (sections, cmd_args)) == CONFIG_ERR_FATAL_ERROR)
        goto cleanup;

      if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
        ret = CONFIG_ERR_NON_FATAL_ERROR;
    }

  if (ret == CONFIG_ERR_SUCCESS)
    {
      /* 3rd pass */
      struct config_section *section = sections;
      while (section)
        {
          struct config_keyvalue *kv = section->keyvalues;
          while (kv)
            {
              if (kv->value)
                {
                  if ((this_ret = kv->commit (section, kv, arg)) == CONFIG_ERR_FATAL_ERROR)
                    goto cleanup;

                  if (this_ret == CONFIG_ERR_NON_FATAL_ERROR)
                    {
                      if (cmd_args->verbose)
                        fprintf (stderr, "FATAL: Error commiting `%s:%s'\n", section->section_name, kv->key_name);
                      ret = CONFIG_ERR_NON_FATAL_ERROR;
                    }
                }
              kv = kv->next;
            }

          if (cmd_args->verbose)
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
pef_commit (struct config_section *sections,
            struct config_arguments *cmd_args,
            void *arg)
{
  config_err_t ret;

  if (cmd_args->filename)
    ret = pef_commit_file (sections, cmd_args, arg);
  else
    ret = pef_commit_keypairs (sections, cmd_args, arg);

  return ret;
}
