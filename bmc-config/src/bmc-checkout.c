#include "bmc-checkout.h"

#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-sections.h"

static bmc_err_t
bmc_checkout_keypair (bmc_config_state_data_t *state_data,
                      struct keypair *kp)
{
  char *keypair = NULL;
  char *section_name;
  char *key_name;
  struct section *sect;
  struct keyvalue *kv;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  bmc_err_t ret = BMC_ERR_SUCCESS;

  if (!(keypair = strdup (kp->keypair)))
    {
      perror("strdup");
      goto cleanup;
    }

  section_name = strtok (keypair, ":");
  key_name = strtok (NULL, "");

  if (!(section_name && key_name)) 
    {
      fprintf (stderr, "Invalid KEY-PAIR spec `%s'\n", kp->keypair);
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }
     
  section_name = strtok (section_name, " \t");
  key_name = strtok (key_name, " \t");

  sect = state_data->sections;
  while (sect) 
    {
      if (same (section_name, sect->section_name)) 
        break;
      sect = sect->next;
    }
  
  if (!sect) 
    {
      fprintf (stderr, "Unknown section `%s'\n", section_name);
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  kv = sect->keyvalues;

  while (kv) 
    {
      if (same (key_name, kv->key))
        break;
      kv = kv->next;
    }

  if (!kv) 
    {
      fprintf (stderr, "Unknown key `%s' in section `%s'\n",
               key_name, section_name);
      rv = BMC_ERR_NON_FATAL_ERROR;
      goto cleanup;
    }

  if ((ret = kv->checkout (state_data, sect, kv)) == BMC_ERR_FATAL_ERROR)
    goto cleanup;

  if (ret == BMC_ERR_SUCCESS) 
    printf ("%s:%s=%s\n", key_name, section_name, kv->value);
  else
    printf ("Error fetching value for %s in %s\n", key_name, section_name);

  rv = ret;
 cleanup:
  if (keypair)
    free(keypair);
  return rv;
}

static bmc_err_t
bmc_checkout_keypairs (bmc_config_state_data_t *state_data)
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

      if ((this_ret = bmc_checkout_keypair(state_data,
                                           kp)) == BMC_ERR_FATAL_ERROR)
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
bmc_checkout_section_common (bmc_config_state_data_t *state_data,
                             struct section *sect, 
                             FILE *fp)
{
  struct bmc_config_arguments *args;
  struct keyvalue *kv;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  bmc_err_t ret = BMC_ERR_SUCCESS;

  fprintf (fp, "Section %s\n", sect->section_name);

  args = state_data->prog_data->args;
  kv = sect->keyvalues;
  
  while (kv) 
    {
      bmc_err_t this_ret;
      
      /* achu: Certain keys should be "hidden" and not be checked out.
       * They only linger for backwards compatability to FreeIPMI's
       * 0.2.0 bmc-config, which have several options with typoed
       * names.
       */
      if (kv->flags & BMC_DO_NOT_CHECKOUT)
        {
          kv = kv->next;
          continue;
        }

      if ((this_ret = kv->checkout (state_data, 
                                    sect,
                                    kv)) == BMC_ERR_FATAL_ERROR)
        goto cleanup;
      
      if (this_ret == BMC_ERR_NON_FATAL_ERROR)
        {
          if (args->verbose)
            fprintf (fp, "\t## FATAL: Unable to checkout %s:%s\n",
                     sect->section_name,
                     kv->key);
          ret = BMC_ERR_NON_FATAL_ERROR;
        } 
      else 
        {
          int key_len = 0;
                  
          fprintf (fp, "\t## %s\n", kv->desc);
                  
          /* beauty comes at a cost */
          
          /* achu: Certain keys should have their checked out
           * value automatically commented out.  Sometimes (in the
           * case of passwords) they cannot be checked out, so the
           * default is for value to be empty.  We do not want the
           * user accidently commiting this checked out file,
           * which (in this example) clears the password.
           *
           * Some other keys may or may not have a value, depending on
           * the IPMI version or the implementation.
           */
          if (kv->flags & BMC_CHECKOUT_KEY_COMMENTED_OUT)
            key_len = fprintf(fp, "\t## %s", kv->key);
          else if (kv->flags & BMC_CHECKOUT_KEY_COMMENTED_OUT_IF_VALUE_EMPTY)
            {
              if (kv->value && strlen(kv->value))
                key_len = fprintf (fp, "\t%s", kv->key);
              else
                key_len = fprintf(fp, "\t## %s", kv->key);
            }
          else
            key_len = fprintf (fp, "\t%s", kv->key);
          
          while (key_len <= 45) 
            {
              fprintf (fp, " ");
              key_len++;
            }
          
          fprintf (fp, "%s\n", kv->value);
        }
      
      kv = kv->next;
    }

  fprintf (fp, "EndSection\n");
  return ret;

 cleanup:
  return rv;
}

static bmc_err_t
bmc_checkout_section (bmc_config_state_data_t *state_data)
{
  struct bmc_config_arguments *args;
  struct sectionstr *sstr;
  FILE *fp;
  int file_opened = 0;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  bmc_err_t ret = BMC_ERR_SUCCESS;

  args = state_data->prog_data->args;

  if (args->filename && strcmp (args->filename, "-"))
    {
      if (!(fp = fopen (args->filename, "w")))
        {
          perror("fopen");
          goto cleanup;
        }
      file_opened++;
    }
  else
    fp = stdout;

  sstr = args->sectionstrs;
  while (sstr)
    {
      struct section *sect = state_data->sections;
      int found = 0;
      
      while (sect) 
        {
          if (!strcasecmp(sect->section_name, sstr->sectionstr))
            {
              bmc_err_t this_ret;
              
              found++;

              if ((this_ret = bmc_checkout_section_common (state_data, 
                                                           sect, 
                                                           fp)) == BMC_ERR_FATAL_ERROR)
                goto cleanup;
              
              if (this_ret == BMC_ERR_NON_FATAL_ERROR)
                ret = BMC_ERR_NON_FATAL_ERROR;
              
              break;
            }

          sect = sect->next;
        }

      if (!found)
        {
          fprintf(stderr, "Invalid section `%s'\n", sstr->sectionstr);
          ret = 0;
        } 

      sstr = sstr->next;
    }
  
  rv = ret;
 cleanup:
  if (file_opened)
    fclose(fp);
  return rv;
}

static bmc_err_t
bmc_checkout_file (bmc_config_state_data_t *state_data)
{
  struct bmc_config_arguments *args;
  struct section *sect;
  FILE *fp;
  int file_opened = 0;
  bmc_err_t rv = BMC_ERR_FATAL_ERROR;
  bmc_err_t ret = BMC_ERR_SUCCESS;

  args = state_data->prog_data->args;

  if (args->filename && strcmp (args->filename, "-"))
    {
      if (!(fp = fopen (args->filename, "w")))
        {
          perror("fopen");
          goto cleanup;
        }
      file_opened++;
    }
  else
    fp = stdout;

  sect = state_data->sections;
  while (sect) 
    {
      bmc_err_t this_ret;
      
      if ((this_ret = bmc_checkout_section_common (state_data,
                                                   sect,
                                                   fp)) == BMC_ERR_FATAL_ERROR)
        goto cleanup;

      if (this_ret == BMC_ERR_NON_FATAL_ERROR)
        ret = BMC_ERR_NON_FATAL_ERROR;

      sect = sect->next;
    }

  rv = ret;
 cleanup:
  if (file_opened)
    fclose(fp);
  return rv;
}

bmc_err_t
bmc_checkout (bmc_config_state_data_t *state_data)
{
  struct bmc_config_arguments *args;
  bmc_err_t ret;

  args = state_data->prog_data->args;

  if (args->keypairs) 
    ret = bmc_checkout_keypairs (state_data);
  else if (args->sectionstrs)
    ret = bmc_checkout_section (state_data);
  else
    ret = bmc_checkout_file (state_data);

  return ret;
}
