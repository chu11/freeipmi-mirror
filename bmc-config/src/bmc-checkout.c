#include "bmc-checkout.h"

#include "bmc-config.h"
#include "bmc-common.h"
#include "bmc-sections.h"

static int
bmc_checkout_keypair (struct bmc_config_arguments *args,
		      struct section *sections,
                      struct keypair *kp)
{
  char *keypair;
  char *section_name;
  char *key_name;
  int ret = 0;
  struct section *sect = sections;
  struct keyvalue *kv;

  if (!(keypair = strdup (kp->keypair)))
    {
      perror("strdup");
      exit(1);
    }

  section_name = strtok (keypair, ":");
  key_name = strtok (NULL, "");

  if (!(section_name && key_name)) 
    {
      fprintf (stderr, "Invalid KEY-PAIR spec `%s'\n", kp->keypair);
      free (keypair);
      return -1;
    }
     
  section_name = strtok (section_name, " \t");
  key_name = strtok (key_name, " \t");

  while (sect) 
    {
      if (same (section_name, sect->section_name)) 
        break;
      sect = sect->next;
    }
  
  if (!sect) 
    {
      fprintf (stderr, "Unknown section `%s'\n",
               section_name);
      free (keypair);
      return -1;
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
      free (keypair);
      return -1;
    }

  ret = kv->checkout (args, sect, kv);

  if (ret == 0) 
    printf ("%s:%s=%s\n", key_name, section_name, kv->value);
  else
    printf ("Error fetching value for %s in %s (errcode=%d)\n",
	    key_name, section_name, ret);

  free (keypair);
  return ret;
}

static int
bmc_checkout_keypairs (struct bmc_config_arguments *args,
                       struct section *sections)
{
  struct keypair *kp;

  kp = args->keypairs;
  while (kp)
    {
      bmc_checkout_keypair(args, sections, kp);
      kp = kp->next;
    }

  return 0;
}

static int
bmc_checkout_section_common (struct bmc_config_arguments *args,
                             struct section *sect, 
                             FILE *fp)
{
  struct keyvalue *kv = sect->keyvalues;
  int ret = 0;

  fprintf (fp, "Section %s\n", sect->section_name);
  
  while (kv) 
    {
      /* exit with non- zero if any field fails to
         checkout, but continue to checkout other
         fields */
      int this_ret = 0;
      
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

      ret = ((this_ret = kv->checkout (args, sect, kv)) || ret);
              
      if (this_ret != 0) 
        {
          if (args->verbose)
            fprintf (fp, "\t## FATAL: Unable to checkout %s:%s\n",
                     sect->section_name,
                     kv->key);
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
           * which (in this example) clear the password.
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
}

static int
bmc_checkout_section (struct bmc_config_arguments *args,
                      struct section *sections)
{
  int ret = 0;
  FILE *fp;
  struct sectionstr *sstr = args->sectionstrs;

  if (args->filename && strcmp (args->filename, "-"))
    fp = fopen (args->filename, "w");
  else
    fp = stdout;

  if (!fp) 
    {
      perror (args->filename);
      return -1;
    }



  while (sstr)
    {
      struct section *sect = sections;
      int found = 0;
      
      while (sect) 
        {
          if (!strcasecmp(sect->section_name, sstr->sectionstr))
            {
              /* exit with non- zero if any field fails to
                 checkout, but continue to checkout other
                 fields */
              int this_ret = 0;
              
              found++;
              
              ret = ((this_ret = bmc_checkout_section_common (args, sect, fp)) || ret);
              
              break;
            }

          sect = sect->next;
        }

      if (!found)
        {
          fprintf(stderr, "Invalid section `%s'\n", sstr->sectionstr);
          ret = -1;
        } 

      sstr = sstr->next;
    }
  
  return ret;
}

static int
bmc_checkout_file (struct bmc_config_arguments *args,
		   struct section *sections)
{
  int ret = 0;
  FILE *fp;
  struct section *sect = sections;

  if (args->filename && strcmp (args->filename, "-"))
    fp = fopen (args->filename, "w");
  else
    fp = stdout;

  if (!fp) 
    {
      perror (args->filename);
      return -1;
    }

  while (sect) 
    {
      /* exit with non- zero if any field fails to
         checkout, but continue to checkout other
         fields */
      int this_ret = 0;
      
      ret = ((this_ret = bmc_checkout_section_common (args, sect, fp)) || ret);

      sect = sect->next;
    }
  return ret;
}

int
bmc_checkout (struct bmc_config_arguments *args,
	      struct section *sections)
{
  int ret = 0;

  if (args->keypairs) 
    ret = bmc_checkout_keypairs (args, sections);
  else if (args->sectionstrs)
    ret = bmc_checkout_section (args, sections);
  else
    ret = bmc_checkout_file (args, sections);

  return ret;
}
