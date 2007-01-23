#include "bmc-common.h"
#include "bmc-config.h"
#include "bmc-diff.h"
#include "bmc-parser.h"
#include "bmc-sections.h"
#include "bmc-types.h"

static int
bmc_diff_keypair (struct arguments *arguments,
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

  ret = bmc_section_diff_value (section_name, key_name, value,
				arguments, sections);

  free (keypair);
  return ret;
}

static int
bmc_diff_keypairs (struct arguments *arguments,
                   struct section *sections)
{
  struct keypair *kp;

  kp = arguments->keypairs;
  while (kp)
    {
      bmc_diff_keypair(arguments, sections, kp);
      kp = kp->next;
    }

  return 0;
}

static int
bmc_diff_file (struct arguments *arguments,
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
                ret = (kv->diff (arguments, sect, kv) || ret);
              kv = kv->next;
            }
          sect = sect->next;
        }
    }

  return ret;
}

int
bmc_diff (struct arguments *arguments,
	  struct section *sections)
{
  int ret = 0;

  if (arguments->keypairs)
    ret = bmc_diff_keypairs (arguments, sections);
  else
    ret = bmc_diff_file (arguments, sections);

  return ret;
}

void report_diff (const char *section, 
		  const char *key, 
		  const char *input_value,
		  const char *actual_value)
{
  printf ("%s:%s - input=`%s':actual=`%s'\n",
	  section, key, input_value, actual_value);
}

