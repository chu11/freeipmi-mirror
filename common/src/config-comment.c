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

#include "config-comment.h"

/* achu: There must be a library out there that can do this better,
 * but I can't find one.  So here's my hacked up simple one.
 */

#define FORMAT_COMMENT_BUFLEN       4096
#define FORMAT_COMMENT_COLUMN_WIDTH 80

static int
_format_comment(char *in,
                char *out,
                unsigned int outsize)
{
  char *inbuf = NULL;
  char *tokbuf;
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
    
  sprintf(out, "# ");
  totalwritten += 2;
  linelen += 2;

  tok = strtok_r(inbuf, " ", &tokbuf);
  while (tok)
    {
      int toklen = strlen(tok);

      if ((linelen + toklen) > FORMAT_COMMENT_COLUMN_WIDTH)
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

      tok = strtok_r(NULL, " ", &tokbuf);
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

int
config_section_comments(char *section_name,
                        char *in,
                        FILE *fp)
{
  char section_name_buf[FORMAT_COMMENT_BUFLEN];
  char buf[FORMAT_COMMENT_BUFLEN];
  char *inbuf = NULL;
  char *tokbuf;
  char *tok;
  int rv = -1;

  assert(section_name);
  assert(in);
  assert(fp);

  if (!(inbuf = strdup(in)))
    goto cleanup;

  fprintf(fp, "#\n");

  /* XXX: assume no overrun */
  snprintf(section_name_buf, 
           FORMAT_COMMENT_BUFLEN,
           "Section %s Comments", 
           section_name);

  if (_format_comment(section_name_buf,
                      buf,
                      FORMAT_COMMENT_BUFLEN) < 0)
    goto cleanup;
  fprintf(fp, "%s", buf);
  fprintf(fp, "#\n");
  
  tok = strtok_r(inbuf, "\n", &tokbuf);
  while (tok)
    {
      if (_format_comment(tok,
                          buf,
                          FORMAT_COMMENT_BUFLEN) < 0)
        goto cleanup;
      fprintf(fp, "%s", buf);
      fprintf(fp, "#\n");

      tok = strtok_r(NULL, "\n", &tokbuf);
    }
  
  rv = 0;
 cleanup:
  free(inbuf);
  return rv;
}
