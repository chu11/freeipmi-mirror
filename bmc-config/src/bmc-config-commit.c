#include "bmc-config-commit.h"

#include "bmc-config.h"
#include "bmc-config-common.h"
#include "bmc-config-parser.h"
#include "bmc-config-sections.h"

static bmc_err_t
bmc_commit_keypair (bmc_config_state_data_t *state_data,
                    struct keypair *kp)
{
  char *keypair = NULL;
  char *section_name;
  char *key_name;
  char *value;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;

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
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
     
  section_name = strtok (section_name, " \t");
  key_name = strtok (key_name, " \t");
  value = strtok (value, " \t");

  rv = bmc_config_section_commit_value (state_data, 
                                        section_name,
                                        key_name, 
                                        value);
 cleanup:
  if (keypair)
    free(keypair);
  return rv;
}

static bmc_err_t
bmc_commit_keypairs (bmc_config_state_data_t *state_data)
{
  struct bmc_config_arguments *args;
  struct keypair *kp;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  bmc_err_t ret = BMC_ERR_SUCCESS;

  args = state_data->prog_data->args;

  kp = args->keypairs;
  while (kp)
    {
      bmc_err_t this_ret;

      if ((this_ret = bmc_commit_keypair(state_data, kp)) == BMC_ERR_FATAL_ERROR)
        goto cleanup;
      
      if (this_ret == BMC_ERR_NON_FATAL_ERROR)
        ret = BMC_ERR_NON_FATAL_ERROR;

      kp = kp->next;
    }

  rv = ret;
 cleanup:
  return rv;
}

static bmc_err_t
bmc_keypair_feed (bmc_config_state_data_t *state_data)
{
  struct bmc_config_arguments *args;
  struct keypair *kp;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  bmc_err_t ret = BMC_ERR_SUCCESS;

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
          rv = BMC_ERR_NON_FATAL_ERROR;
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
                  rv = BMC_ERR_NON_FATAL_ERROR;
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
          rv = BMC_ERR_NON_FATAL_ERROR;
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

static bmc_err_t
bmc_commit_file (bmc_config_state_data_t *state_data)
{
  struct bmc_config_arguments *args;
  int file_opened = 0;
  FILE *fp;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  bmc_err_t ret = BMC_ERR_SUCCESS;
  bmc_err_t this_ret;

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
  if ((this_ret = bmc_config_parser (state_data, fp)) == BMC_ERR_FATAL_ERROR)
    goto cleanup;

  if (this_ret == BMC_ERR_NON_FATAL_ERROR)
    ret = BMC_ERR_NON_FATAL_ERROR;

  /* 2nd pass - feed in keypair elements from the command line to override file keypairs */
  if (args->keypairs)
    {
      if ((this_ret = bmc_keypair_feed (state_data)) == BMC_ERR_FATAL_ERROR)
        goto cleanup;

      if (this_ret == BMC_ERR_NON_FATAL_ERROR)
        ret = BMC_ERR_NON_FATAL_ERROR;
    }

  if (ret == BMC_ERR_SUCCESS) 
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
                  if ((this_ret = kv->commit (state_data, sect, kv)) == BMC_ERR_FATAL_ERROR)
                    goto cleanup;

                  if (this_ret == BMC_ERR_NON_FATAL_ERROR)
                    {
                      if (args->verbose)
                        fprintf (stderr, "FATAL: Error commiting `%s:%s'\n", sect->section_name, kv->key);
                      ret = BMC_ERR_NON_FATAL_ERROR;
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

bmc_err_t
bmc_commit (bmc_config_state_data_t *state_data)
{
  struct bmc_config_arguments *args;
  bmc_err_t ret;

  args = state_data->prog_data->args;
  if (args->filename)
    ret = bmc_commit_file (state_data);
  else
    ret = bmc_commit_keypairs (state_data);

  return ret;
}
