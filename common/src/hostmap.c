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
#include <sys/param.h>
#include <errno.h>
#include <assert.h>

#include "freeipmi-portability.h"

#include "fd.h"
#include "hash.h"
#include "hostlist.h"
#include "list.h"

#include "hostmap.h"

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif

static char *hostmap_errmsg[] =
  {
    "success",
    "file not found",
    "out of memory",
    "invalid parameters",
    "parse error",
    "invalid hostname",
    "invalid hostname count",
    "duplicate entry",
    "host not found",
    "internal system error",
    "internal error",
    "errnum out of range",
  };

struct hostmap
{
  int32_t magic;
  int errnum;
  int line;
  hash_t h_althost_keyed;
  hash_t h_ipmihost_keyed;
  List l;
};

#define HOSTMAP_MAGIC       0xFF33D00D

#define HOSTMAP_HASH_SIZE   2048

#define HOSTMAP_BUFLEN      4096

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
  struct hostmap *hmap = NULL;

  if (!(hmap = (struct hostmap *)malloc(sizeof(struct hostmap))))
    goto cleanup;
  memset(hmap, '\0', sizeof(struct hostmap));

  hmap->magic = HOSTMAP_MAGIC;

  if (!(hmap->h_althost_keyed = hash_create(HOSTMAP_HASH_SIZE,
                                            (hash_key_f)hash_key_string,
                                            (hash_cmp_f)strcmp,
                                            (hash_del_f)_hostmap_data_free)))
    goto cleanup;

  /* points to same info in h_althost_keyed, let hash deal w/ freeing memory */
  if (!(hmap->h_ipmihost_keyed = hash_create(HOSTMAP_HASH_SIZE,
                                             (hash_key_f)hash_key_string,
                                             (hash_cmp_f)strcmp,
                                             (hash_del_f)NULL)))
    goto cleanup;

  /* points to same info in h_althost_keyed, let hash deal w/ freeing memory */
  if (!(hmap->l = list_create(NULL)))
    goto cleanup;

  hmap->errnum = HOSTMAP_ERR_SUCCESS;
  return hmap;

 cleanup:
  if (hmap)
    {
      if (hmap->h_althost_keyed)
        hash_destroy(hmap->h_althost_keyed);
      if (hmap->h_ipmihost_keyed)
        hash_destroy(hmap->h_ipmihost_keyed);
      if (hmap->l)
        list_destroy(hmap->l);
    }
  return NULL;
}

void
hostmap_destroy(hostmap_t hmap)
{
  assert(hmap && hmap->magic == HOSTMAP_MAGIC);

  hmap->magic = ~HOSTMAP_MAGIC;
  hmap->errnum = HOSTMAP_ERR_SUCCESS;
  hash_destroy(hmap->h_althost_keyed);
  hash_destroy(hmap->h_ipmihost_keyed);
  list_destroy(hmap->l);
  free(hmap);
}

int
hostmap_errnum(hostmap_t hmap)
{
  assert(hmap && hmap->magic == HOSTMAP_MAGIC);

  return hmap->errnum;
}

char *
hostmap_strerror(int errnum)
{
  if (errnum >= HOSTMAP_ERR_SUCCESS && errnum <= HOSTMAP_ERR_ERRNUMRANGE)
    return hostmap_errmsg[errnum];
  else
    return hostmap_errmsg[HOSTMAP_ERR_ERRNUMRANGE];
}

/* for hash callback to delete all items */
static int
_all_hash_items (void *data, const void *key, void *arg)
{
  return 1;
}

/* for list callback to delete all items */
static int
_all_list_items (void *x, void *key)
{
  return 1;
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
_readline(hostmap_t hmap, int fd, char *buf, unsigned int buflen)
{
  int len;

  assert(hmap && hmap->magic == HOSTMAP_MAGIC && fd && buf && buflen);

  if ((len = fd_read_line(fd, buf, buflen)) < 0)
    {
      hmap->errnum = HOSTMAP_ERR_SYSTEM_ERROR;
      return -1;
    }

  /* buflen - 1 b/c fd_read_line guarantees null termination */
  if (len >= (buflen-1))
    {
      hmap->errnum = HOSTMAP_ERR_PARSE;
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
_remove_comments(hostmap_t hmap, char *buf, int buflen)
{
  int i, comment_flag, lenleft;

  assert(hmap && hmap->magic == HOSTMAP_MAGIC && buf);

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
_remove_trailing_whitespace(hostmap_t hmap, char *buf, int buflen)
{
  char *temp;

  assert(hmap && hmap->magic == HOSTMAP_MAGIC && buf);

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
_move_past_whitespace(hostmap_t hmap, char *buf)
{
  assert(hmap && hmap->magic == HOSTMAP_MAGIC && buf);

  while (*buf != '\0' && isspace(*buf))
    buf++;

  return buf;
}

static struct hostmap_data *
_hostmap_data_create(hostmap_t hmap,
                     const char *althost, 
                     const char *ipmihost)
{
  struct hostmap_data *hd = NULL;

  assert(hmap && hmap->magic == HOSTMAP_MAGIC && althost && ipmihost);

  if (!(hd = (struct hostmap_data *)malloc(sizeof(struct hostmap_data))))
    {
      hmap->errnum = HOSTMAP_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }
  memset(hd, '\0', sizeof(struct hostmap_data));
  
  if (!(hd->althost = strdup(althost)))
    {
      hmap->errnum = HOSTMAP_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }

  if (!(hd->ipmihost = strdup(ipmihost)))
    {
      hmap->errnum = HOSTMAP_ERR_OUT_OF_MEMORY;
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

static int
_hostmap_data_insert(hostmap_t hmap,
                     const char *althost, 
                     const char *ipmihost)
{
  struct hostmap_data *hd = NULL;

  assert(hmap && hmap->magic == HOSTMAP_MAGIC && althost && ipmihost);
  
  if (strlen(althost) > MAXHOSTNAMELEN
      || strlen(ipmihost) > MAXHOSTNAMELEN)
    {
      hmap->errnum = HOSTMAP_ERR_HOSTNAME_INVALID;
      goto cleanup;
    }

  if (!(hd = _hostmap_data_create(hmap, althost, ipmihost)))
    goto cleanup;

  if (!hash_insert(hmap->h_althost_keyed, hd->althost, hd))
    {
      _hostmap_data_free(hd);
      if (errno == EEXIST)
        hmap->errnum = HOSTMAP_ERR_DUPLICATE_ENTRY;
      else
        hmap->errnum = HOSTMAP_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  /* XXX duplicates deal with */
  if (!hash_insert(hmap->h_ipmihost_keyed, hd->ipmihost, hd))
    {
      _hostmap_data_free(hd);
      hmap->errnum = HOSTMAP_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  if (!list_append(hmap->l, hd))
    {
      hmap->errnum = HOSTMAP_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  return 0;

 cleanup:
  return -1;
}

static int
_hostmap_multiple_data_insert(hostmap_t hmap,
                              const char *althost, 
                              const char *ipmihost)
{
  char *althostptr = NULL;
  char *ipmihostptr = NULL;
  hostlist_t hl_althost = NULL;
  hostlist_t hl_ipmihost = NULL;
  hostlist_iterator_t hl_althost_itr = NULL;
  hostlist_iterator_t hl_ipmihost_itr = NULL;
  int rv = -1;

  if (!(hl_althost = hostlist_create(althost)))
    {
      hmap->errnum = HOSTMAP_ERR_PARSE;
      goto cleanup;
    }

  if (!(hl_ipmihost = hostlist_create(ipmihost)))
    {
      hmap->errnum = HOSTMAP_ERR_PARSE;
      goto cleanup;
    }

  hostlist_uniq(hl_althost);
  hostlist_uniq(hl_ipmihost);
  hostlist_sort(hl_althost);
  hostlist_sort(hl_ipmihost);

  /* ensure same number of hosts listed for both */
  if (hostlist_count(hl_althost) != hostlist_count(hl_ipmihost))
    {
      hmap->errnum = HOSTMAP_ERR_HOSTNAME_COUNT_INVALID;
      goto cleanup;
    }

  if (!(hl_althost_itr = hostlist_iterator_create(hl_althost)))
    {
      hmap->errnum = HOSTMAP_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }

  if (!(hl_ipmihost_itr = hostlist_iterator_create(hl_ipmihost)))
    {
      hmap->errnum = HOSTMAP_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }

  while ((althostptr = hostlist_next(hl_althost_itr))
         && (ipmihostptr = hostlist_next(hl_ipmihost_itr)))
    {
      if (_hostmap_data_insert (hmap, althostptr, ipmihostptr) < 0)
        goto cleanup;
      free(althostptr);
      free(ipmihostptr);
    }
  althostptr = NULL;
  ipmihostptr = NULL;

  rv = 0;
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
  return rv;
}

int
hostmap_parse(hostmap_t hmap, const char *filename)
{
  int rv = -1;
  int fd = -1;
  int len;
  int line = 0;
  char buf[HOSTMAP_BUFLEN];

  assert(hmap && hmap->magic == HOSTMAP_MAGIC);

  /* FREEIPMI_HOSTMAP_FILE_DEFAULT configured via autoconf */
  if (!filename)
    filename = FREEIPMI_HOSTMAP_FILE_DEFAULT;
  
  if (access(filename, R_OK) < 0)
    {
      hmap->errnum = HOSTMAP_ERR_FILE_NOT_FOUND;
      goto cleanup;
    }

  if ((fd = open(filename, O_RDONLY)) < 0)
    {
      hmap->errnum = HOSTMAP_ERR_SYSTEM_ERROR;
      goto cleanup;
    }

  while ((len = _readline(hmap, fd, buf, HOSTMAP_BUFLEN)) > 0)
    {
      char *strptr;
      char *althost = NULL;
      char *ipmihost = NULL;

      line++;

      if (!(len = _remove_comments(hmap, buf, len)))
        continue;

      if (len < 0)
        {
          if (hmap->errnum == HOSTMAP_ERR_PARSE
              || hmap->errnum == HOSTMAP_ERR_HOSTNAME_INVALID
              || hmap->errnum == HOSTMAP_ERR_HOSTNAME_COUNT_INVALID
              || hmap->errnum == HOSTMAP_ERR_DUPLICATE_ENTRY)
            hmap->line = line;
          goto cleanup;
        }

      if (!(len = _remove_trailing_whitespace(hmap, buf, len)))
        continue;

      if (len < 0)
        {
          if (hmap->errnum == HOSTMAP_ERR_PARSE
              || hmap->errnum == HOSTMAP_ERR_HOSTNAME_INVALID
              || hmap->errnum == HOSTMAP_ERR_HOSTNAME_COUNT_INVALID
              || hmap->errnum == HOSTMAP_ERR_DUPLICATE_ENTRY)
            hmap->line = line;
          goto cleanup;
        }

      if (!(strptr = _move_past_whitespace(hmap, buf)))
        {
          if (hmap->errnum == HOSTMAP_ERR_PARSE
              || hmap->errnum == HOSTMAP_ERR_HOSTNAME_INVALID
              || hmap->errnum == HOSTMAP_ERR_HOSTNAME_COUNT_INVALID
              || hmap->errnum == HOSTMAP_ERR_DUPLICATE_ENTRY)
            hmap->line = line;
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
          hmap->errnum = HOSTMAP_ERR_PARSE;
          hmap->line = line;
          goto cleanup;
        }

      *strptr = '\0';
      strptr++;

      while (strptr != '\0' && isspace(*strptr))
        strptr++;

      if (*strptr == '\0')
        {
          /* shouldn't reach this point, but just in case */
          hmap->errnum = HOSTMAP_ERR_PARSE;
          hmap->line = line;
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
          if (_hostmap_multiple_data_insert (hmap, althost, ipmihost) < 0)
            {
              if (hmap->errnum == HOSTMAP_ERR_PARSE
                  || hmap->errnum == HOSTMAP_ERR_HOSTNAME_INVALID
                  || hmap->errnum == HOSTMAP_ERR_HOSTNAME_COUNT_INVALID
                  || hmap->errnum == HOSTMAP_ERR_DUPLICATE_ENTRY)
                hmap->line = line;
              goto cleanup;
            }
        }
      else
        {
          /* single hosts listed */
          if (_hostmap_data_insert (hmap, althost, ipmihost) < 0)
            {
              if (hmap->errnum == HOSTMAP_ERR_PARSE
                  || hmap->errnum == HOSTMAP_ERR_HOSTNAME_INVALID
                  || hmap->errnum == HOSTMAP_ERR_HOSTNAME_COUNT_INVALID
                  || hmap->errnum == HOSTMAP_ERR_DUPLICATE_ENTRY)
                hmap->line = line;
              goto cleanup;
            }
        }
    }

  rv = 0;
  hmap->errnum = HOSTMAP_ERR_SUCCESS;
 cleanup:
  if (rv != 0)
    {
      hash_delete_if(hmap->h_althost_keyed, _all_hash_items, "foo");
      hash_delete_if(hmap->h_ipmihost_keyed, _all_hash_items, "foo");
      list_delete_all(hmap->l, _all_list_items, "foo");
    }
  close(fd);
  return rv;
}

int
hostmap_line(hostmap_t hmap)
{
  assert(hmap && hmap->magic == HOSTMAP_MAGIC);

  if (hmap->errnum == HOSTMAP_ERR_PARSE 
      || hmap->errnum == HOSTMAP_ERR_HOSTNAME_INVALID
      || hmap->errnum == HOSTMAP_ERR_HOSTNAME_COUNT_INVALID
      || hmap->errnum == HOSTMAP_ERR_DUPLICATE_ENTRY)
    return hmap->line;
  else
    return -1;
}

char *
hostmap_map_althost(hostmap_t hmap, const char *althost)
{
  struct hostmap_data *hd;

  assert(hmap && hmap->magic == HOSTMAP_MAGIC);
  
  if (!(hd = hash_find(hmap->h_althost_keyed, althost)))
    {
      hmap->errnum = HOSTMAP_ERR_HOST_NOT_FOUND;
      return NULL;
    }

  hmap->errnum = HOSTMAP_ERR_SUCCESS;
  return hd->ipmihost;
}

char *
hostmap_map_ipmihost(hostmap_t hmap, const char *ipmihost)
{
  struct hostmap_data *hd;

  assert(hmap && hmap->magic == HOSTMAP_MAGIC);
  
  if (!(hd = hash_find(hmap->h_ipmihost_keyed, ipmihost)))
    {
      hmap->errnum = HOSTMAP_ERR_HOST_NOT_FOUND;
      return NULL;
    }
  
  hmap->errnum = HOSTMAP_ERR_SUCCESS;
  return hd->althost;
}

int
hostmap_for_each(hostmap_t hmap, hostmap_for_each_f f, void *arg)
{
  struct hostmap_data *hd;
  ListIterator itr = NULL;
  int rv = -1;

  assert(hmap && hmap->magic == HOSTMAP_MAGIC && f);

  if (!(itr = list_iterator_create(hmap->l)))
    {
      hmap->errnum = HOSTMAP_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }

  while ((hd = list_next(itr)))
    {
      if (f(hd->althost, hd->ipmihost, arg) < 0)
        {
          hmap->errnum = HOSTMAP_ERR_SYSTEM_ERROR;
          goto cleanup;
        }
    }
  
  hmap->errnum = HOSTMAP_ERR_SUCCESS;
  rv = 0;
 cleanup:
  if (itr)
    list_iterator_destroy(itr);
  return rv;
}

int 
hostmap_open(hostmap_t *ptrhmap, const char *filename)
{
  hostmap_t hmap = NULL;

  assert(ptrhmap);

  *ptrhmap = NULL;

  if (!(hmap = hostmap_create()))
    {
      fprintf(stderr, "hostmap_create error\n");
      goto cleanup;
    }

  if (hostmap_parse(hmap, filename) < 0)
    {
      if (hostmap_errnum(hmap) == HOSTMAP_ERR_FILE_NOT_FOUND)
        {
          if (filename)
            {
              fprintf(stderr, "hostmap file '%s' not found\n", filename);
              goto cleanup;
            }
          else
            /* if default doesn't exist, it's not an error */
            goto out;
        }
      else if (hostmap_errnum(hmap) == HOSTMAP_ERR_PARSE
               || hostmap_errnum(hmap) == HOSTMAP_ERR_HOSTNAME_INVALID
               || hostmap_errnum(hmap) == HOSTMAP_ERR_HOSTNAME_COUNT_INVALID
               || hostmap_errnum(hmap) == HOSTMAP_ERR_DUPLICATE_ENTRY)
        
        {
          /* FREEIPMI_HOSTMAP_FILE_DEFAULT configured via autoconf */
          fprintf(stderr,
                  "hostmap file '%s' %s on line %d\n",
                  (filename) ? filename : FREEIPMI_HOSTMAP_FILE_DEFAULT,
                  hostmap_strerror(hostmap_errnum(hmap)),
                  hostmap_line(hmap));
          goto cleanup;
        }
      else
        {
          fprintf(stderr,
                  "hostmap file error: %s\n", 
                  hostmap_strerror(hostmap_errnum(hmap)));
          goto cleanup;
        }
    }

  *ptrhmap = hmap;
 out:
  return 0;

 cleanup:
  if (hmap)
    hostmap_destroy(hmap);
  return -1;
}
