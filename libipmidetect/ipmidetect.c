/*****************************************************************************\
 *  $Id: ipmidetect.c,v 1.21 2010-02-08 22:02:31 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-228523
 *
 *  This file is part of Ipmidetect, tools and libraries for detecting
 *  IPMI nodes in a cluster. For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmidetect is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmidetect is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmidetect.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_STRINGS_H
#include <strings.h>            /* bzero */
#endif /* HAVE_STRINGS_H */
#include <sys/types.h>
#include <sys/stat.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <netinet/in.h>
#include <netdb.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/select.h>
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
# include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
# include <time.h>
#endif /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <assert.h>
#include <errno.h>

#include "ipmidetect.h"

#include "conffile.h"
#include "fd.h"
#include "freeipmi-portability.h"
#include "hostlist.h"

/*
 * ipmidetect_errmsg
 *
 * error messages array
 */
static char * ipmidetect_errmsg[] =
  {
    "success",
    "ipmidetect handle null",
    "ipmidetect handle invalid",
    "connection to server error",
    "connection to server timeout",
    "hostname invalid",
    "data already loaded",
    "data not loaded",
    "array or string not large enough to store result",
    "incorrect parameters",
    "null pointer reached in list",
    "out of memory",
    "node not found",
    "parse config file error",
    "invalid config file input",
    "internal config file error",
    "internal system error",
    "error number out of range",
  };

#define IPMIDETECT_MAGIC_NUM            0xbeefbeef

#define IPMIDETECT_CONFIG_HOSTNAMES_MAX 8
#define IPMIDETECT_MAXHOSTNAMELEN       64

#define IPMIDETECT_DETECTED_NODES     1
#define IPMIDETECT_UNDETECTED_NODES   0

#define IPMIDETECT_LOAD_STATE_UNLOADED  0
#define IPMIDETECT_LOAD_STATE_SETUP     1
#define IPMIDETECT_LOAD_STATE_LOADED    2

#define IPMIDETECT_BUFLEN               1024
#define IPMIDETECT_PORT_DEFAULT         9225
#define IPMIDETECT_TIMEOUT_LEN_DEFAULT  60
#define IPMIDETECT_BACKEND_CONNECT_LEN  5


struct ipmidetect {
  int magic;
  int errnum;
  int load_state;
  hostlist_t detected_nodes;
  hostlist_t undetected_nodes;
};

struct ipmidetect_config
{
  char hostnames[IPMIDETECT_CONFIG_HOSTNAMES_MAX+1][IPMIDETECT_MAXHOSTNAMELEN+1];
  unsigned int hostnames_len;
  int hostnames_flag;
  int port;
  int port_flag;
  int timeout_len;
  int timeout_len_flag;
};

/*
 * _ipmidetect_handle_error_check
 *
 * standard handle error checker
 *
 * Returns 0 on success, -1 one error
 */
static int
_ipmidetect_handle_error_check (ipmidetect_t handle)
{
  if (!handle || handle->magic != IPMIDETECT_MAGIC_NUM)
    return (-1);

  return (0);
}

/*
 * _unloaded_handle_error_check
 *
 * standard unloaded handle error checker
 *
 * Returns -1 on error, 0 on success
 */
static int
_unloaded_handle_error_check (ipmidetect_t handle)
{
  if (_ipmidetect_handle_error_check (handle) < 0)
    return (-1);

  if (handle->load_state == IPMIDETECT_LOAD_STATE_LOADED)
    {
      handle->errnum = IPMIDETECT_ERR_ISLOADED;
      return (-1);
    }

  if (handle->load_state != IPMIDETECT_LOAD_STATE_UNLOADED)
    {
      handle->errnum = IPMIDETECT_ERR_INTERNAL;
      return (-1);
    }

  return (0);
}

/*
 * _loaded_handle_error_check
 *
 * standard loaded handle error checker
 *
 * Returns -1 on error, 0 on success
 */
static int
_loaded_handle_error_check (ipmidetect_t handle)
{
  if (_ipmidetect_handle_error_check (handle) < 0)
    return (-1);

  if (handle->load_state == IPMIDETECT_LOAD_STATE_UNLOADED)
    {
      handle->errnum = IPMIDETECT_ERR_NOTLOADED;
      return (-1);
    }

  if (handle->load_state != IPMIDETECT_LOAD_STATE_LOADED)
    {
      handle->errnum = IPMIDETECT_ERR_INTERNAL;
      return (-1);
    }

  return (0);
}

/*
 * _initialize_handle
 *
 * initialize ipmidetect handle
 */
static void
_initialize_handle (ipmidetect_t handle)
{
  handle->magic = IPMIDETECT_MAGIC_NUM;
  handle->load_state = IPMIDETECT_LOAD_STATE_UNLOADED;
  handle->detected_nodes = NULL;
  handle->undetected_nodes = NULL;
}

ipmidetect_t
ipmidetect_handle_create ()
{
  ipmidetect_t handle;

  if (!(handle = (ipmidetect_t)malloc (sizeof (struct ipmidetect))))
    return (NULL);

  _initialize_handle (handle);
  handle->errnum = IPMIDETECT_ERR_SUCCESS;
  return (handle);
}

/*
 * _free_handle_data
 *
 * free ipmidetect handle data
 */
static void
_free_handle_data (ipmidetect_t handle)
{
  hostlist_destroy (handle->detected_nodes);
  hostlist_destroy (handle->undetected_nodes);
  _initialize_handle (handle);
}

int
ipmidetect_handle_destroy (ipmidetect_t handle)
{
  if (_ipmidetect_handle_error_check (handle) < 0)
    return (-1);

  _free_handle_data (handle);

  /* "clean" magic number */
  handle->magic = ~IPMIDETECT_MAGIC_NUM;

  free (handle);
  return (0);
}

/*
 * _cb_hostnames
 *
 * callback function for configuration parsing of hostnames option.
 *
 * Returns -1 on error, 0 on success
 */
static int
_cb_hostnames (conffile_t cf, struct conffile_data *data, char *optionname,
               int option_type, void *option_ptr, int option_data,
               void *app_ptr, int app_data)
{
  struct ipmidetect_config *conf = option_ptr;
  unsigned int i;

  if (!option_ptr)
    {
      conffile_seterrnum (cf, CONFFILE_ERR_PARAMETERS);
      return (-1);
    }

  if (data->stringlist_len > IPMIDETECT_CONFIG_HOSTNAMES_MAX)
    return (-1);

  for (i = 0; i < data->stringlist_len; i++)
    {
      if (strlen (data->stringlist[i]) > IPMIDETECT_MAXHOSTNAMELEN)
        return (-1);
      strcpy (conf->hostnames[i], data->stringlist[i]);
    }
  conf->hostnames_len = data->stringlist_len;
  return (0);
}

/*
 * _read_conffile
 *
 * read and parse the ipmidetect configuration file
 *
 * Returns 0 on success, -1 on error
 */
static int
_read_conffile (ipmidetect_t handle, struct ipmidetect_config *conf)
{
  struct conffile_option options[] =
    {
      { "hostnames", CONFFILE_OPTION_LIST_STRING, -1,
        _cb_hostnames, 1, 0, &(conf->hostnames_flag),
        conf, 0},
      { "port", CONFFILE_OPTION_INT, 0,
        conffile_int, 1, 0, &(conf->port_flag),
        &(conf->port), 0},
      { "timeout_len", CONFFILE_OPTION_INT, 0,
        conffile_int, 1, 0, &(conf->timeout_len_flag),
        &(conf->timeout_len), 0},
    };
  conffile_t cf = NULL;
  int num, rv = -1;
  int legacy_file_loaded = 0;

  if (!(cf = conffile_handle_create ()))
    {
      handle->errnum = IPMIDETECT_ERR_INTERNAL;
      goto cleanup;
    }

  num = sizeof (options)/sizeof (struct conffile_option);

  /* Try legacy file first */

  if (!conffile_parse (cf,
                       IPMIDETECT_CONFIG_FILE_LEGACY,
                       options,
                       num,
                       NULL,
                       0,
                       0))
    legacy_file_loaded++;
  
  if (!legacy_file_loaded)
    {
      /* IPMIDETECT_CONFIG_FILE_DEFAULT defined in config.h */
      if (conffile_parse (cf,
                          IPMIDETECT_CONFIG_FILE_DEFAULT,
                          options,
                          num,
                          NULL,
                          0,
                          0) < 0)
        {
          /* Not an error if the configuration file does not exist */
          if (conffile_errnum (cf) != CONFFILE_ERR_EXIST)
            {
              int errnum = conffile_errnum (cf);
              if (CONFFILE_IS_PARSE_ERR (errnum))
                handle->errnum = IPMIDETECT_ERR_CONF_PARSE;
              else if (errnum == CONFFILE_ERR_OUTMEM)
                handle->errnum = IPMIDETECT_ERR_OUT_OF_MEMORY;
              else
                handle->errnum = IPMIDETECT_ERR_CONF_INTERNAL;
              goto cleanup;
            }
        }
    }
  
  rv = 0;
 cleanup:
  conffile_handle_destroy (cf);
  return (rv);
}

static int
_low_timeout_connect (ipmidetect_t handle,
                      const char *hostname,
                      int port,
                      int connect_timeout)
{
  int rv, old_flags, fd = -1;
  struct sockaddr_in servaddr;
  struct hostent *hptr;

  /* valgrind will report a mem-leak in gethostbyname() */
  if (!(hptr = gethostbyname (hostname)))
    {
      handle->errnum = IPMIDETECT_ERR_HOSTNAME_INVALID;
      return (-1);
    }

  /* Alot of this code is from Unix Network Programming, by Stevens */
  if ((fd = socket (AF_INET, SOCK_STREAM, 0)) < 0)
    {
      handle->errnum = IPMIDETECT_ERR_INTERNAL;
      goto cleanup;
    }
  bzero (&servaddr, sizeof (servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_port = htons (port);
  servaddr.sin_addr = *((struct in_addr *)hptr->h_addr);

  if ((old_flags = fcntl (fd, F_GETFL, 0)) < 0)
    {
      handle->errnum = IPMIDETECT_ERR_INTERNAL;
      goto cleanup;
    }

  if (fcntl (fd, F_SETFL, old_flags | O_NONBLOCK) < 0)
    {
      handle->errnum = IPMIDETECT_ERR_INTERNAL;
      goto cleanup;
    }

  rv = connect (fd, (struct sockaddr *)&servaddr, sizeof (struct sockaddr_in));
  if (rv < 0 && errno != EINPROGRESS)
    {
      handle->errnum = IPMIDETECT_ERR_CONNECT;
      goto cleanup;
    }
  else if (rv < 0 && errno == EINPROGRESS)
    {
      fd_set rset, wset;
      struct timeval tval;

      FD_ZERO (&rset);
      FD_SET (fd, &rset);
      FD_ZERO (&wset);
      FD_SET (fd, &wset);
      tval.tv_sec = connect_timeout;
      tval.tv_usec = 0;

      if ((rv = select (fd+1, &rset, &wset, NULL, &tval)) < 0)
        {
          handle->errnum = IPMIDETECT_ERR_INTERNAL;
          goto cleanup;
        }

      if (!rv)
        {
          handle->errnum = IPMIDETECT_ERR_CONNECT_TIMEOUT;
          goto cleanup;
        }
      else
        {
          if (FD_ISSET (fd, &rset) || FD_ISSET (fd, &wset))
            {
              int error;
              socklen_t len;

              len = sizeof (int);

              if (getsockopt (fd, SOL_SOCKET, SO_ERROR, &error, &len) < 0)
                {
                  handle->errnum = IPMIDETECT_ERR_INTERNAL;
                  goto cleanup;
                }

              if (error != 0)
                {
                  if (error == ECONNREFUSED)
                    handle->errnum = IPMIDETECT_ERR_CONNECT;
                  else if (error == ETIMEDOUT)
                    handle->errnum = IPMIDETECT_ERR_CONNECT_TIMEOUT;
                  else
                    handle->errnum = IPMIDETECT_ERR_INTERNAL;
                  goto cleanup;
                }
              /* else no error, connected within timeout length */
            }
          else
            {
              handle->errnum = IPMIDETECT_ERR_INTERNAL;
              goto cleanup;
            }
        }
    }

  /* reset flags */
  if (fcntl (fd, F_SETFL, old_flags) < 0)
    {
      handle->errnum = IPMIDETECT_ERR_INTERNAL;
      goto cleanup;
    }

  return (fd);

 cleanup:
  /* ignore potential error, error path */
  close (fd);
  return (-1);
}

static int
_get_data (ipmidetect_t handle,
           const char *hostname,
           int port,
           int timeout_len)
{
  struct timeval tv;
  int fd, rv = -1;

  if ((fd = _low_timeout_connect (handle,
                                  hostname,
                                  port,
                                  IPMIDETECT_BACKEND_CONNECT_LEN)) < 0)
    goto cleanup;

  /* Call gettimeofday at the latest point right before getting data. */
  if (gettimeofday (&tv, NULL) < 0)
    {
      handle->errnum = IPMIDETECT_ERR_INTERNAL;
      goto cleanup;
    }

  while (1)
    {
      char buf[IPMIDETECT_BUFLEN];
      char hostname[IPMIDETECT_MAXHOSTNAMELEN+1];
      unsigned long int localtime;
      int len, num, ret;

      if ((len = fd_read_line (fd, buf, IPMIDETECT_BUFLEN)) < 0)
        {
          handle->errnum = IPMIDETECT_ERR_INTERNAL;
          goto cleanup;
        }

      if (!len)
        break;

      num = sscanf (buf, "%s %lu\n", hostname, &localtime);
      if (num != 2)
        {
          handle->errnum = IPMIDETECT_ERR_INTERNAL;
          goto cleanup;
        }

      if (abs (localtime - tv.tv_sec) < timeout_len)
        ret = hostlist_push (handle->detected_nodes, hostname);
      else
        ret = hostlist_push (handle->undetected_nodes, hostname);

      if (!ret)
        {
          handle->errnum = IPMIDETECT_ERR_OUT_OF_MEMORY;
          goto cleanup;
        }
    }

  rv = 0;
 cleanup:
  /* ignore potential error, done w/ fd */
  close (fd);
  return (rv);

}

int
ipmidetect_load_data (ipmidetect_t handle,
                      const char *hostname,
                      int port,
                      int timeout_len)
{
  struct ipmidetect_config conffile_config;

  if (_unloaded_handle_error_check (handle) < 0)
    goto cleanup;

  memset (&conffile_config, '\0', sizeof (struct ipmidetect_config));

  if (_read_conffile (handle, &conffile_config) < 0)
    goto cleanup;

  if (!(handle->detected_nodes = hostlist_create (NULL)))
    {
      handle->errnum = IPMIDETECT_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }

  if (!(handle->undetected_nodes = hostlist_create (NULL)))
    {
      handle->errnum = IPMIDETECT_ERR_OUT_OF_MEMORY;
      goto cleanup;
    }

  handle->load_state = IPMIDETECT_LOAD_STATE_SETUP;

  if (port <= 0)
    {
      if (conffile_config.port_flag)
        {
          if (conffile_config.port <= 0)
            {
              handle->errnum = IPMIDETECT_ERR_CONF_INPUT;
              goto cleanup;
            }
          port = conffile_config.port;
        }
      else
        port = IPMIDETECT_PORT_DEFAULT;
    }

  if (timeout_len <= 0)
    {
      if (conffile_config.timeout_len_flag)
        {
          if (conffile_config.timeout_len <= 0)
            {
              handle->errnum = IPMIDETECT_ERR_CONF_INPUT;
              goto cleanup;
            }
          timeout_len = conffile_config.timeout_len;
        }
      else
        timeout_len = IPMIDETECT_TIMEOUT_LEN_DEFAULT;
    }

  if (conffile_config.hostnames_flag)
    {
      unsigned int i;

      for (i = 0; i < conffile_config.hostnames_len; i++)
        {
          if (strlen (conffile_config.hostnames[i]) > 0)
            {
              if (_get_data (handle,
                             hostname,
                             port,
                             timeout_len) < 0)
                continue;
              else
                break;
            }
        }

      if (i >= conffile_config.hostnames_len)
        {
          handle->errnum = IPMIDETECT_ERR_CONNECT;
          goto cleanup;
        }
    }
  else
    {
      char *hostPtr;

      if (hostname)
        hostPtr = (char *)hostname;
      else
        hostPtr = "localhost";

      if (_get_data (handle,
                     hostPtr,
                     port,
                     timeout_len) < 0)
        goto cleanup;
    }

  hostlist_sort (handle->detected_nodes);
  hostlist_sort (handle->undetected_nodes);

  /* loading complete */
  handle->load_state = IPMIDETECT_LOAD_STATE_LOADED;

  handle->errnum = IPMIDETECT_ERR_SUCCESS;
  return (0);

 cleanup:
  _free_handle_data (handle);
  return (-1);
}

int
ipmidetect_errnum (ipmidetect_t handle)
{
  if (!handle)
    return (IPMIDETECT_ERR_HANDLE_NULL);
  else if (handle->magic != IPMIDETECT_MAGIC_NUM)
    return (IPMIDETECT_ERR_HANDLE_INVALID);

  return (handle->errnum);
}

char *
ipmidetect_strerror (int errnum)
{
  if (errnum < IPMIDETECT_ERR_SUCCESS || errnum > IPMIDETECT_ERR_ERRNUMRANGE)
    return (ipmidetect_errmsg[IPMIDETECT_ERR_ERRNUMRANGE]);

  return (ipmidetect_errmsg[errnum]);
}

char *
ipmidetect_errormsg (ipmidetect_t handle)
{
  return (ipmidetect_strerror (ipmidetect_errnum (handle)));
}

void
ipmidetect_perror (ipmidetect_t handle, const char *msg)
{
  char *errormsg = ipmidetect_strerror (ipmidetect_errnum (handle));

  if (!msg)
    fprintf (stderr, "%s\n", errormsg);
  else
    fprintf (stderr, "%s: %s\n", msg, errormsg);
}

/*
 * _get_nodes_string
 *
 * common function for ipmidetect_get_detected_nodes_string and
 * ipmidetect_get_undetected_nodes_string
 *
 * Returns 0 on success, -1 on error
 */
static int
_get_nodes_string (ipmidetect_t handle, char *buf, int buflen, int which)
{
  hostlist_t hl;

  if (_loaded_handle_error_check (handle) < 0)
    return (-1);

  if (!buf || buflen <= 0)
    {
      handle->errnum = IPMIDETECT_ERR_PARAMETERS;
      return (-1);
    }

  if (which == IPMIDETECT_DETECTED_NODES)
    hl = handle->detected_nodes;
  else
    hl = handle->undetected_nodes;

  if (hostlist_ranged_string (hl, buflen, buf) < 0)
    {
      handle->errnum = IPMIDETECT_ERR_OVERFLOW;
      return (-1);
    }

  handle->errnum = IPMIDETECT_ERR_SUCCESS;
  return (0);
}

int
ipmidetect_get_detected_nodes_string (ipmidetect_t handle, char *buf, int buflen)
{
  return (_get_nodes_string (handle, buf, buflen, IPMIDETECT_DETECTED_NODES));
}

int
ipmidetect_get_undetected_nodes_string (ipmidetect_t handle, char *buf, int buflen)
{
  return (_get_nodes_string (handle, buf, buflen, IPMIDETECT_UNDETECTED_NODES));
}

/*
 * _is_node
 *
 * common function for ipmidetect_is_node_detected and
 * ipmidetect_is_node_undetected
 *
 * Returns bool on success, -1 on error
 */
static int
_is_node (ipmidetect_t handle, const char *node, int which)
{
  int temp, rv = -1;

  if (_loaded_handle_error_check (handle) < 0)
    return (-1);

  if (!node)
    {
      handle->errnum = IPMIDETECT_ERR_PARAMETERS;
      return (-1);
    }

  if (hostlist_find (handle->detected_nodes, node) < 0
      && hostlist_find (handle->undetected_nodes, node) < 0)
    {
      handle->errnum = IPMIDETECT_ERR_NOTFOUND;
      return (-1);
    }

  if (which == IPMIDETECT_DETECTED_NODES)
    temp = hostlist_find (handle->detected_nodes, node);
  else
    temp = hostlist_find (handle->undetected_nodes, node);

  if (temp != -1)
    rv = 1;
  else
    rv = 0;

  handle->errnum = IPMIDETECT_ERR_SUCCESS;
  return (rv);
}

int
ipmidetect_is_node_detected (ipmidetect_t handle, const char *node)
{
  return (_is_node (handle, node, IPMIDETECT_DETECTED_NODES));
}

int
ipmidetect_is_node_undetected (ipmidetect_t handle, const char *node)
{
  return (_is_node (handle, node, IPMIDETECT_UNDETECTED_NODES));
}

