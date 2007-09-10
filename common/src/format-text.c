#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <ctype.h>
#include <errno.h>
#include <assert.h>

/* achu: There must be a library out there that can do this better,
 * but I can't find one.  So here's my hacked up simple one.
 */

#define MAX_COLUMN_WIDTH 80

int
format_text(char prefix,
            int column_width,
            char *in,
            char *out,
            unsigned int outsize)
{
  char *inbuf = NULL;
  char *tokbuf;
  char *spacechars = " ";
  char *tok;
  int linelen = 0;
  int totalwritten = 0;
  int rv = -1;

  assert(in);
  assert(out);
  assert(outsize);
  
  if (!(inbuf = strdup(in)))
    goto cleanup;

  memset(out, '\0', outsize);

  /* -1 b/c we care about end null */
  /* +2 because "# " is two bytes */
  if ((outsize - 1) <= (totalwritten + 2))
    goto cleanup;
    
  sprintf(out, "%c ", prefix);
  totalwritten += 2;
  linelen += 2;

  tok = strtok_r(inbuf, spacechars, &tokbuf);
  while (tok)
    {
      int toklen = strlen(tok);

      if ((linelen + toklen) > MAX_COLUMN_WIDTH)
        {
          /* +1 because "\n# " is 3 bytes*/
          if ((outsize - 1) <= (totalwritten + 3))
            goto cleanup;

          strcat(out, "\n# ");
          totalwritten += 3;
          linelen = 2;
        }

      /* +1 because of the space */
      if ((outsize - 1) <= (totalwritten + toklen + 1))
        goto cleanup;

      strcat(out, tok);
      strcat(out, " ");
      totalwritten += (toklen + 1);
      linelen += (toklen + 1);

      tok = strtok_r(NULL, spacechars, &tokbuf);
    }

  /* +1 for the newline at the end */
  if ((outsize -1) <= (totalwritten + 1))
    goto cleanup;
  
  strcat(out, "\n");
  
  rv = 0;
 cleanup:
  free(inbuf);
  return rv;
}
