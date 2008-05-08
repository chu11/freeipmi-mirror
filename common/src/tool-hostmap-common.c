/*
  Copyright (C) 2003-2008 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.
  
  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <ctype.h>
#endif /* STDC_HEADERS */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <errno.h>
#include <assert.h>

#include "freeipmi-portability.h"

#include "fd.h"
#include "hash.h"
#include "hostlist.h"

#include "tool-hostmap-common.h"

static char *ipmi_errmsg[] =
  {
    "success",
    "invalid parameters",
    "parse error",
    "internal system error",
    "internal error",
    "errnum out of range",
  };

struct hostmap
{
  int32_t magic;
  int errnum;
  int line;
  hash_t h;
};

#define HOSTMAP_MAGIC 0xFF33D00D

#define HOSTMAP_HASH_SIZE 2048

#define HOSTMAP_BUFLEN    4096

struct hostmap_data 
{
  char *althost;
  char *ipmihost;
};

static void
_hostmap_data_free(void *x)
{
  struct hostmap_data *hd;

  assert(x);

  hd = (struct hostmap_data *)x;

  free(hd->althost);
  free(hd->ipmihost);
  free(hd);
}

hostmap_t
hostmap_create(void)
{
  struct hostmap *hm = NULL;

  if (!(hm = (struct hostmap *)malloc(sizeof(struct hostmap))))
    goto cleanup;
  memset(hm, '\0', sizeof(struct hostmap));

  hm->magic = HOSTMAP_MAGIC;

  if (!(hm->h = hash_create(HOSTMAP_HASH_SIZE,
                            (hash_key_f)hash_key_string,
                            (hash_cmp_f)strcmp,
                            (hash_del_f)_hostmap_data_free)))
    goto cleanup;

  hm->errnum = HOSTMAP_ERR_SUCCESS;
  return hm;

 cleanup:
  if (hm)
    {
      if (hm->h)
        hash_destroy(hm->h);
    }
  return NULL;
}

void
hostmap_destroy(hostmap_t hm)
{
  assert(hm->magic == HOSTMAP_MAGIC);

  hm->magic = ~HOSTMAP_MAGIC;
  hm->errnum = HOSTMAP_ERR_SUCCESS;
  hash_destroy(hm->h);
  free(hm);
}

/* achu: Some of this parsing code was lifted out of Cerebro.
 *
 * See http://cerebro.sourceforge.net
 */

/*
 * _readline
 *
 * read a line from the file.  Buffer guaranteed to be null
 * terminated.
 *
 * - fd - file descriptor to read from
 * - buf - buffer pointer
 * - buflen - buffer length
 *
 * Return amount of data read into the buffer, -1 on error
 */
static int
_readline(hostmap_t hm, int fd, char *buf, unsigned int buflen)
{
  int len;

  assert(hm && hm->magic == HOSTMAP_MAGIC && fd && buf && buflen);

  if ((len = fd_read_line(fd, buf, buflen)) < 0)
    {
      hm->errnum = HOSTMAP_ERR_SYSTEM_ERROR;
      return -1;
    }

  /* buflen - 1 b/c fd_read_line guarantees null termination */
  if (len >= (buflen-1))
    {
      hm->errnum = HOSTMAP_ERR_PARSE;
      return -1;
    }

  return len;
}

/*
 * _remove_comments
 *
 * remove comments from the buffer
 *
 * - buf - buffer pointer
 * - buflen - buffer length
 *
 * Return length of buffer left after comments were removed, -1 on
 * error
 */
static int
_remove_comments(hostmap_t hm, char *buf, int buflen)
{
  int i, comment_flag, lenleft;

  assert(hm && hm->magic == HOSTMAP_MAGIC && buf);

  if (!strchr(buf, '#'))
    return buflen;
  
  i = 0;
  comment_flag = 0;
  lenleft = buflen;
  while (i < buflen)
    {
      if (comment_flag)
        {
          buf[i] = '\0';
          lenleft--;
        }

      if (buf[i] == '#')
        {
          buf[i] = '\0';
          comment_flag++;
          lenleft--;
        }
      i++;
    }

  return lenleft;
}

/*
 * _remove_trailing_whitespace
 *
 * remove trailing whitespace from the buffer
 *
 * - buf - buffer pointer
 * - buflen - buffer length
 *
 * Return length of buffer left after trailing whitespace was removed,
 * -1 on error
 */
static int
_remove_trailing_whitespace(hostmap_t hm, char *buf, int buflen)
{
  char *temp;

  assert(hm && hm->magic == HOSTMAP_MAGIC && buf);

  temp = buf + buflen;
  for (--temp; temp >= buf; temp--)
    {
      if (isspace(*temp))
        *temp = '\0';
      else
        break;
      buflen--;
    }

  return buflen;
}

/*
 * _move_past_whitespace
 *
 * move past whitespace at the beginning of the buffer
 *
 * - buf - buffer pointer
 *
 * Return pointer to beginning of first non-whitespace char, NULL on
 * error
 */
static char *
_move_past_whitespace(hostmap_t hm, char *buf)
{
  assert(hm && hm->magic == HOSTMAP_MAGIC && buf);

  while (*buf != '\0' && isspace(*buf))
    buf++;

  return buf;
}

static struct hostmap_data *
_hostmap_data_create(hostmap_t hm,
                     const char *althost, 
                     const char *ipmihost)
{
  struct hostmap_data *hd = NULL;

  assert(hm && hm->magic == HOSTMAP_MAGIC && althost && ipmihost);

  if (!(hd = (struct hostmap_data *)malloc(sizeof(struct hostmap_data))))
    {
      hm->errnum = HOSTMAP_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }
  memset(hd, '\0', sizeof(struct hostmap_data));
  
  if (!(hd->althost = strdup(althost)))
    {
      hm->errnum = HOSTMAP_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }

  if (!(hd->ipmihost = strdup(ipmihost)))
    {
      hm->errnum = HOSTMAP_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }

  return hd;
  
 cleanup:
  if (hd)
    {
      if (hd->althost)
        free(hd->althost);
      if (hd->ipmihost)
        free(hd->ipmihost);
    }
  return NULL;
}

/* for hash to delete all items */
static int
_all_items (void *data, const void *key, void *arg)
{
  return 1;
}

int
hostmap_parse(hostmap_t hm, const char *filename)
{
  int rv = -1;
  int fd = -1;
  int len;
  int line = 0;
  char *althostptr = NULL;
  char *ipmihostptr = NULL;
  hostlist_t hl_althost = NULL;
  hostlist_t hl_ipmihost = NULL;
  hostlist_iterator_t hl_althost_itr = NULL;
  hostlist_iterator_t hl_ipmihost_itr = NULL;
  struct hostmap_data *hd = NULL;
  char buf[HOSTMAP_BUFLEN];

  assert(hm && hm->magic == HOSTMAP_MAGIC);

  /* FREEIPMI_HOSTMAP_FILE_DEFAULT configured via autoconf */
  if (!filename)
    filename = FREEIPMI_HOSTMAP_FILE_DEFAULT;
  
  if (access(filename, R_OK) < 0)
    {
      hm->errnum = HOSTMAP_ERR_FILE_NOT_FOUND;
      goto cleanup;
    }

  if ((fd = open(filename, O_RDONLY)) < 0)
    {
      hm->errnum = HOSTMAP_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  while ((len = _readline(hm, fd, buf, HOSTMAP_BUFLEN)) > 0)
    {
      char *strptr;
      char *althost = NULL;
      char *ipmihost = NULL;

      line++;

      if (!(len = _remove_comments(hm, buf, len)))
        continue;

      if (len < 0)
        {
          if (hm->errnum == HOSTMAP_ERR_PARSE)
            hm->line = line;
          goto cleanup;
        }

      if (!(len = _remove_trailing_whitespace(hm, buf, len)))
        continue;

      if (len < 0)
        {
          if (hm->errnum == HOSTMAP_ERR_PARSE)
            hm->line = line;
          goto cleanup;
        }

      if (!(strptr = _move_past_whitespace(hm, buf)))
        {
          if (hm->errnum == HOSTMAP_ERR_PARSE)
            hm->line = line;
          goto cleanup;
        }

      if (strptr[0] == '\0')
        continue;
      
      althost = strptr;
      
      /* find first whitespace to separate alternate hostname and real hostname */

      while (strptr != '\0' && !isspace(*strptr))
        strptr++;

      if (*strptr == '\0')
        {
          /* no whitespace, so user didn't not specify two hosts */
          hm->errnum = HOSTMAP_ERR_PARSE;
          hm->line = line;
          goto cleanup;
        }

      *strptr = '\0';
      strptr++;

      while (strptr != '\0' && isspace(*strptr))
        strptr++;

      if (*strptr == '\0')
        {
          /* shouldn't reach this point, but just in case */
          hm->errnum = HOSTMAP_ERR_PARSE;
          hm->line = line;
          goto cleanup;
        }

      ipmihost = strptr;
      
      /* multiple hosts listed */
      if (strchr(althost, ',')
          || strchr(althost, '[')
          || strchr(althost, ']')
          || strchr(ipmihost, ',')
          || strchr(ipmihost, '[')
          || strchr(ipmihost, ']'))
        {
          if (!(hl_althost = hostlist_create(althost)))
            {
              hm->errnum = HOSTMAP_ERR_PARSE;
              hm->line = line;
              goto cleanup;
            }

          if (!(hl_ipmihost = hostlist_create(ipmihost)))
            {
              hm->errnum = HOSTMAP_ERR_PARSE;
              hm->line = line;
              goto cleanup;
            }

          /* ensure same number of hosts listed for both */
          if (hostlist_count(hl_althost) != hostlist_count(hl_ipmihost))
            {
              hm->errnum = HOSTMAP_ERR_PARSE;
              hm->line = line;
              goto cleanup;
            }

          if (!(hl_althost_itr = hostlist_iterator_create(hl_althost)))
            {
              hm->errnum = HOSTMAP_ERR_OUT_OF_MEMORY;
              goto cleanup;
            }

          if (!(hl_ipmihost_itr = hostlist_iterator_create(hl_ipmihost)))
            {
              hm->errnum = HOSTMAP_ERR_OUT_OF_MEMORY;
              goto cleanup;
            }

          while ((althostptr = hostlist_next(hl_althost_itr))
                 && (ipmihostptr = hostlist_next(hl_ipmihost_itr)))
            {
              if (!(hd = _hostmap_data_create(hm, althostptr, ipmihostptr)))
                goto cleanup;
              
              if (!hash_insert(hm->h, hd->althost, hd))
                {
                  hm->errnum = HOSTMAP_ERR_SYSTEM_ERROR;
                  goto cleanup;
                }
              
              free(althostptr);
              free(ipmihostptr);
              hd = NULL;
            }

          hostlist_iterator_destroy(hl_althost_itr);
          hostlist_iterator_destroy(hl_ipmihost_itr);
          hostlist_destroy(hl_althost);
          hostlist_destroy(hl_ipmihost);
          hl_althost_itr = NULL;
          hl_ipmihost_itr = NULL;
          hl_althost = NULL;
          hl_ipmihost = NULL;
        }
      else
        {
          /* single hosts listed */
          if (!(hd = _hostmap_data_create(hm, althost, ipmihost)))
            goto cleanup;

          if (!hash_insert(hm->h, hd->althost, hd))
            {
              hm->errnum = HOSTMAP_ERR_SYSTEM_ERROR;
              goto cleanup;
            }
          
          hd = NULL;
        }
    }

  rv = 0;
  hm->errnum = HOSTMAP_ERR_SUCCESS;
 cleanup:
  if (hl_althost_itr)
    hostlist_iterator_destroy(hl_althost_itr);
  if (hl_ipmihost_itr)
    hostlist_iterator_destroy(hl_ipmihost_itr);
  if (hl_althost)
    hostlist_destroy(hl_althost);
  if (hl_ipmihost)
    hostlist_destroy(hl_ipmihost);
  if (althostptr)
    free(althostptr);
  if (ipmihostptr)
    free(ipmihostptr);
  if (hd)
    _hostmap_data_free(hd);
  hash_delete_if(hm->h, _all_items, NULL);
  close(fd);
  return rv;
}

int
hostmap_line(hostmap_t hm)
{
  assert(hm && hm->magic == HOSTMAP_MAGIC);

  if (hm->errnum == HOSTMAP_ERR_PARSE)
    return hm->line;
  else
    return -1;
}
