#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-diff.h"
#include "bmc-parser.h"
#include "bmc-sections.h"

static bmc_diff_t
bmc_diff_keypair (struct bmc_config_arguments *args,
		  struct section *sections,
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

  rv = bmc_section_diff_value (section_name, key_name, value,
                               args, sections);
 cleanup:
  if (keypair)
    free (keypair);
  return rv;
}

static bmc_diff_t
bmc_diff_keypairs (struct bmc_config_arguments *args,
                   struct section *sections)
{
  struct keypair *kp;
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;
  bmc_diff_t ret = BMC_DIFF_SAME;

  kp = args->keypairs;
  while (kp)
    {
      bmc_diff_t this_ret;

      if ((this_ret = bmc_diff_keypair (args, sections, kp)) == BMC_DIFF_FATAL_ERROR)
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
bmc_diff_file (struct bmc_config_arguments *args,
	       struct section *sections)
{
  FILE *fp;
  int file_opened = 0;
  bmc_diff_t rv = BMC_DIFF_FATAL_ERROR;
  bmc_diff_t ret = BMC_DIFF_SAME;
  bmc_diff_t this_ret;

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
  if ((this_ret = bmc_parser (args, sections, fp)) == BMC_DIFF_FATAL_ERROR)
    goto cleanup;

  if (this_ret == BMC_DIFF_NON_FATAL_ERROR)
    ret = BMC_DIFF_NON_FATAL_ERROR;

  if (ret == BMC_DIFF_SAME) 
    {
      /* 2nd pass if 1st pass was successful */
      struct section *sect = sections;
      while (sect) 
        {
          struct keyvalue *kv = sect->keyvalues;
          while (kv) 
            {
              if (kv->value) 
                {
                  if ((this_ret = kv->diff (args, sect, kv)) == BMC_DIFF_FATAL_ERROR)
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

bmc_err_t
bmc_diff (struct bmc_config_arguments *args,
	  struct section *sections)
{
  bmc_diff_t ret;

  if (args->keypairs)
    ret = bmc_diff_keypairs (args, sections);
  else
    ret = bmc_diff_file (args, sections);

  if (ret == BMC_DIFF_SAME)
    return BMC_ERR_SUCCESS;
  if (ret == BMC_DIFF_DIFFERENT || ret == BMC_DIFF_NON_FATAL_ERROR)
    return BMC_ERR_NON_FATAL_ERROR;
  return BMC_ERR_FATAL_ERROR;
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

