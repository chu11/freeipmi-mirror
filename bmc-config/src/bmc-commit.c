
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "bmc-types.h"
#include "bmc-config.h"
#include "bmc-sections.h"



static int
bmc_commit_keypair (struct arguments *arguments,
		      struct section *sections)
{
  char *keypair = strdup (arguments->keypair);
  char *section_name;
  char *key_name;
  char *value;

  int ret = 0;

  section_name = strtok (keypair, ":");
  key_name = strtok (NULL, "=");
  value = strtok (NULL, "");

  if (! (section_name && key_name && value)) 
    {
      fprintf (stderr, "Invalid KEY-PAIR spec `%s'\n", 
               arguments->keypair);
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

  if (!ret) 
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
                  ret = kv->commit (arguments, sect, kv);
                  if (ret != 0)
                    break;
                }
              kv = kv->next;
            }
          if (ret != 0)
            break;
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

  if (arguments->keypair)
    ret = bmc_commit_keypair (arguments, sections);
  else
    ret = bmc_commit_file (arguments, sections);
  return ret;
}
