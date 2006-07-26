#include "bmc-common.h"
#include "bmc-config.h"
#include "bmc-sections.h"
#include "bmc-types.h"

static int
bmc_checkout_keypair (struct arguments *arguments,
		      struct section *sections)
{
  char *keypair = strdup (arguments->keypair);
  char *section_name;
  char *key_name;

  int ret = 0;

  struct section *sect = sections;
  struct keyvalue *kv;

  section_name = strtok (keypair, ":");
  key_name = strtok (NULL, "");

  if (! (section_name && key_name)) 
    {
      fprintf (stderr, "Invalid KEY-PAIR spec `%s'\n", 
               arguments->keypair);
      free (keypair);
      return -1;
    }
     
  section_name = strtok (section_name, " \t");
  key_name = strtok (key_name, " \t");

  while (sect) 
    {
      if (same (section_name, sect->section)) 
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

  ret = kv->checkout (arguments, sect, kv);

  if (ret == 0) 
    printf ("%s:%s=%s\n", key_name, section_name, kv->value);
  else
    printf ("error fetching value for %s in %s (errcode=%d)\n",
	    key_name, section_name, ret);

  return ret;
}


static int
bmc_checkout_file (struct arguments *arguments,
		   struct section *sections)
{
  int ret = 0;
  FILE *fp;
  struct section *sect = sections;

  if (arguments->filename && strcmp (arguments->filename, "-"))
    fp = fopen (arguments->filename, "w");
  else
    fp = stdout;

  if (!fp) 
    {
      perror (arguments->filename);
      return -1;
    }

  while (sect) 
    {
      struct keyvalue *kv = sect->keyvalues;
      fprintf (fp, "Section %s\n", sect->section);

      while (kv) 
        {
          /* exit with non- zero if any field fails to
             checkout, but continue to checkout other
             fields */
          int this_ret = 0;

          /* achu: Certain keys should be "hidden" and not be checked
           * out.  They only linger for backwards compatability to
           * FreeIPMI's 0.2.0 bmc-config, which several options with
           * typoed names.
           */
          if (kv->flags & BMC_DO_NOT_CHECKOUT)
            {
              kv = kv->next;
              continue;
            }
              
          ret = ((this_ret = kv->checkout (arguments, sect, kv)) || ret);
          
          if (this_ret != 0) 
            {
              if (arguments->verbose)
                fprintf (fp, "\t## FATAL: Unable to checkout %s:%s\n",
                         sect->section,
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
               * which will clear the password.
               */
              if (kv->flags & BMC_CHECKOUT_KEY_COMMENTED_OUT)
                key_len = fprintf(fp, "\t## %s", kv->key);
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
      sect = sect->next;
    }
  return ret;
}

int
bmc_checkout (struct arguments *arguments,
	      struct section *sections)
{
  int ret = 0;

  if (arguments->keypair) 
    ret = bmc_checkout_keypair (arguments, sections);
  else
    ret = bmc_checkout_file (arguments, sections);

  return ret;
}
