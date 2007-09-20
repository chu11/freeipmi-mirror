#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */

#include "bmc-config.h"
#include "bmc-config-common.h"
#include "bmc-config-diff.h"
#include "bmc-config-parser.h"
#include "bmc-config-sections.h"

static bmc_diff_t
bmc_diff_keypair (bmc_config_state_data_t *state_data,
                  struct keypair *kp)
{
  char *keypair = NULL;
  char *section_name;
  char *key_name;
  char *value;
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;
  
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
      rv = BMC_DIFF_NON_FATAL_ERROR; 
      goto cleanup;
    }
     
  section_name = strtok (section_name, " \t");
  key_name = strtok (key_name, " \t");
  value = strtok (value, " \t");

  rv = bmc_config_section_diff_value (state_data,
                                      section_name, 
                                      key_name,
                                      value);
 cleanup:
  if (keypair)
    free (keypair);
  return rv;
}

static bmc_diff_t
bmc_diff_keypairs (bmc_config_state_data_t *state_data)
{
  struct bmc_config_arguments *args;
  struct keypair *kp;
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;
  bmc_diff_t ret = BMC_DIFF_SAME;

  args = state_data->prog_data->args;

  kp = args->keypairs;
  while (kp)
    {
      bmc_diff_t this_ret;

      if ((this_ret = bmc_diff_keypair (state_data, 
                                        kp)) == BMC_DIFF_FATAL_ERROR)
        goto cleanup;

      if (this_ret == BMC_DIFF_NON_FATAL_ERROR)
        ret = BMC_DIFF_NON_FATAL_ERROR;

      kp = kp->next;
    }

  rv = ret;
 cleanup:
  return rv;
}

static bmc_diff_t
bmc_diff_file (bmc_config_state_data_t *state_data)
{
  struct bmc_config_arguments *args;
  FILE *fp;
  int file_opened = 0;
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;
  bmc_diff_t ret = BMC_DIFF_SAME;
  bmc_diff_t this_ret;

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

  /* 1st pass */
  if ((this_ret = bmc_config_parser (state_data, fp)) == BMC_DIFF_FATAL_ERROR)
    goto cleanup;

  if (this_ret == BMC_DIFF_NON_FATAL_ERROR)
    ret = BMC_DIFF_NON_FATAL_ERROR;

  if (ret == BMC_DIFF_SAME) 
    {
      /* 2nd pass if 1st pass was successful */
      struct section *sect = state_data->sections;
      while (sect) 
        {
          struct keyvalue *kv = sect->keyvalues;
          while (kv) 
            {
              if (kv->value) 
                {
                  if ((this_ret = kv->diff (state_data, sect, kv)) == BMC_DIFF_FATAL_ERROR)
                    goto cleanup;

                  if (this_ret == BMC_DIFF_NON_FATAL_ERROR)
                    ret = BMC_DIFF_NON_FATAL_ERROR;
                }
              kv = kv->next;
            }
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
bmc_diff (bmc_config_state_data_t *state_data)
{
  struct bmc_config_arguments *args;
  bmc_diff_t ret;

  args = state_data->prog_data->args;

  if (args->keypairs)
    ret = bmc_diff_keypairs (state_data);
  else
    ret = bmc_diff_file (state_data);

  if (ret == BMC_DIFF_SAME)
    return CONFIG_ERR_SUCCESS;
  if (ret == BMC_DIFF_DIFFERENT || ret == BMC_DIFF_NON_FATAL_ERROR)
    return CONFIG_ERR_NON_FATAL_ERROR;
  return CONFIG_ERR_FATAL_ERROR;
}

void 
report_diff (const char *section, 
	     const char *key, 
	     const char *input_value,
	     const char *actual_value)
{
  printf ("%s:%s - input=`%s':actual=`%s'\n",
	  section, key, input_value, actual_value);
}

