

#include "bmc-types.h"
#include "bmc-config.h"
#include "bmc-sections.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int
bmc_parser (struct arguments *arguments,
	    struct section *sections,
	    FILE *fp)
{ 
  char buf[4096];
  int line_num = 0;
  char *section_name = NULL;
  char *key_name = NULL;
  char *value = NULL;
  int ret = 0;


  while (fgets (buf, 4096, fp)) {
    line_num ++;
    buf[4095] = 0;
    char *first_word = strtok (buf, " \t\n");

    if (! first_word) {
      if (arguments->verbose)
	fprintf (stderr, "%d: empty line\n", line_num);
      continue;
    }
    
    /*      
	    if (arguments->verbose)
	    fprintf (stderr, "%d: First word = '%s'\n", line_num, first_word);
    */
    
    if (first_word[0] == '#') {
      if (arguments->verbose)
	  fprintf (stderr, "Comment on line %d\n", line_num);
      continue;
    }
    
    if (same (first_word, "Section")) {
      if (section_name)
	free (section_name);
      section_name = strtok (NULL, " \t\n");
      
      if (!section_name) {
	fprintf (stderr, "FATAL: Error parsing line number %d\n",
		 line_num);
	if (section_name)
	  free (section_name);
	  if (key_name)
	    free (key_name);
	  if (value)
	    free (value);
	  ret = -1;
	  break;
      }
      
      section_name = strdup (section_name);
      if (arguments->verbose) 
	fprintf (stderr, "Entering section `%s'\n", section_name);
      continue;
    } /* same (first_word, "Section") */

    if (same (first_word, "EndSection")) {
      if (!section_name) {
	fprintf (stderr, "FATAL: encountered `%s' without a matching Section\n",
		 first_word);
	if (key_name)
	  free (key_name);
	if (value)
	  free (value);
	ret = -1;
	break;
      }
      if (arguments->verbose)
	  fprintf (stderr, "Leaving section `%s'\n", section_name);
      free (section_name);
      section_name = NULL;
      
      continue;
    } /* same (first_word, "EndSection") */
    
    if (!section_name) {
      fprintf (stderr, "FATAL: Key `%s' not inside any Section\n",
	       first_word);
      if (key_name)
	free (key_name);
	if (value)
	  free (value);
	ret = -1;
	break;
    }
    
    if (key_name)
      free (key_name);
    key_name = strdup (first_word);
    if (value)
	free (value);
    value = strtok (NULL, " \t\n");
    if (value)
      value = strdup (value);
    else
      value = strdup ("");
    
    if (arguments->verbose) 
      fprintf (stderr, "Trying to set `%s:%s=%s'\n",
	       section_name, key_name, value);
    
    if (bmc_section_set_value (section_name, key_name, value,
			       arguments, sections) != 0) {
      if (section_name) 
	free (section_name);
      if (key_name)
	free (key_name);
      if (value)
	free (value);
      ret = -1;
      break;
    }
  }

  return ret;
}
