#include "bmc-commit.h"
#include "bmc-parser.h"

#include "bmc-common.h"
#include "bmc-config.h"
#include "bmc-sections.h"
#include "bmc-types.h"

static int
bmc_commit_keypair (struct arguments *arguments,
                    struct section *sections,
                    struct keypair *kp)
{
  char *keypair;
  char *section_name;
  char *key_name;
  char *value;
  int ret = 0;

  if (!(keypair = strdup (kp->keypair)))
    {
      perror("strdup");
      exit(1);
    }

  section_name = strtok (keypair, ":");
  key_name = strtok (NULL, "=");
  value = strtok (NULL, "");

  if (!(section_name && key_name && value)) 
    {
      fprintf (stderr, "Invalid KEY-PAIR spec `%s'\n", kp->keypair);
      free (keypair);
      return -1;
    }
     
  section_name = strtok (section_name, " \t");
  key_name = strtok (key_name, " \t");
  value = strtok (value, " \t");

  ret = bmc_section_commit_value (section_name, key_name, value,
				  arguments, sections);

  free (keypair);
  return ret;
}

static int
bmc_commit_keypairs (struct arguments *arguments,
                     struct section *sections)
{
  struct keypair *kp;

  kp = arguments->keypairs;
  while (kp)
    {
      bmc_commit_keypair(arguments, sections, kp);
      kp = kp->next;
    }

  return 0;
}

static int
bmc_keypair_feed (struct arguments *arguments,
                  struct section *sections)
{
  struct keypair *kp = arguments->keypairs;

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
          exit(1);
        }
      
      section_name = strtok (keypair, ":");
      key_name = strtok (NULL, "=");
      value = strtok (NULL, "");
      
      if (!(section_name && key_name && value)) 
        {
          fprintf (stderr, "Invalid KEY-PAIR spec `%s'\n", kp->keypair);
          free (keypair);
          return -1;
        }
     
      section_name = strtok (section_name, " \t");
      key_name = strtok (key_name, " \t");
      value = strtok (value, " \t");
      
      sect = sections;
      found_section = 0;
      while (sect) 
        {
          if (!strcasecmp(sect->section, section_name))
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
                          exit(1);
                        }

                      break;    /* break out of 'kv' loop */
                    }
                  kv = kv->next;
                }

              if (!found_key)
                {
                  fprintf (stderr, "Invalid KEY in `%s'\n", kp->keypair);
                  free (keypair);
                  return -1;
                }

              break;            /* break out of 'sect' loop */
            }
          sect = sect->next;
        }
      
      if (!found_section)
        {
          fprintf (stderr, "Invalid SECTION in `%s'\n", kp->keypair);
          free (keypair);
          return -1;
        }

      free (keypair);
      kp = kp->next;
    }

  return 0;
}

static int
bmc_commit_file (struct arguments *arguments,
		 struct section *sections)
{
  int ret = 0;
  FILE *fp;

  if (arguments->filename && strcmp (arguments->filename, "-"))
    fp = fopen (arguments->filename, "r");
  else
    fp = stdin;

  if (!fp) 
    {
      perror (arguments->filename);
      return -1;
    }

  /* 1st pass */
  ret = bmc_parser (arguments, sections, fp);

  if (fp != stdin)
    fclose (fp);

  /* 2nd pass - feed in keypair elements from the command line */
  if (arguments->keypairs)
    ret = bmc_keypair_feed (arguments, sections);

  if (!ret) 
    {
      /* 3rd pass */
      struct section *sect = sections;
      while (sect) 
        {
          struct keyvalue *kv = sect->keyvalues;
          while (kv) 
            {
              if (kv->value) 
                {
                  ret = kv->commit (arguments, sect, kv);
                  if (ret != 0)
		    fprintf (stderr, "FATAL: Error commiting `%s:%s'\n", sect->section, kv->key);
                }
              kv = kv->next;
            }
          sect = sect->next;
        }
    }

  return ret;
}

int
bmc_commit (struct arguments *arguments,
	      struct section *sections)
{
  int ret = 0;

  if (arguments->filename)
    ret = bmc_commit_file (arguments, sections);
  else
    ret = bmc_commit_keypairs (arguments, sections);
  return ret;
}
