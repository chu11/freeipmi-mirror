/* 
    fi_utils.c: all utility/misc functions

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2, or (at
    your option) any later version.

    This program is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <stdlib.h>

#ifdef STDC_HEADERS
#include <string.h>
#else
# include <sys/types.h>
# ifndef HAVE_MEMCPY
static void*
memcpy (void *dest, const void *src, size_t n)
{
  while (0 <= --n) ((unsigned char*)dest) [n] = ((unsigned char*)src) [n];
  return dest;
}
# endif
# ifndef HAVE_MEMSET
static void*
memset (void *s, int c, size_t n)
{
  while (0 <= --n) ((unsigned char*)s) [n] = (unsigned char) c;
  return s;
}
# endif
# ifndef HAVE_STRCHR
static char*
strchr (const char* s, int c)
{
  while (*s != '\0')
    if (*s == (char)c) return s;
    else s++;
  return NULL;
}
# endif
#endif

#include <pwd.h>
#include <termios.h>
#include <readline/readline.h>
#include <guile/gh.h>
#include <sys/stat.h>

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#include <errno.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <error.h>

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "fi-utils.h"
#include "fish.h"

static tcflag_t old_lflag;
static cc_t old_vtime;
extern int errno;

struct termios term;

void
get_terminal_attributes (void)
{
  if (tcgetattr (STDIN_FILENO, &term) < 0)
    {
      perror ("tcgetattr()");
      exit (EXIT_FAILURE);
    }
  old_lflag = term.c_lflag;
  old_vtime = term.c_cc[VTIME];
}

void
set_terminal_attributes (void)
{
  term.c_lflag = old_lflag;
  term.c_cc[VTIME] = old_vtime;
  if (tcsetattr (STDIN_FILENO, TCSANOW, &term) < 0)
    {
      perror ("tcsetattr()");
      exit (EXIT_FAILURE);
    }
}

/* Trims the leading and trailing white spaces of a string  */
char *
stripwhite (char *string)
{
  register char *s, *t;

  for (s = string; whitespace (*s); s++)
    ;

  if (*s == 0)
    return s;

  t = s + strlen (s) - 1;
  while (t > s && whitespace (*t))
    t--;
  *++t = '\0';

  return s;
}


char *
get_token (char **line)
{
  char *command;
  while (1)
    {
      command = (char *) strsep (line, " ");
      if (!command)
	break;
      if (*(command))
	break;
    }
  return command;
}

char *
get_token_with_strdelim (char **line, char *delim)
{
  char *token = (char *) NULL;
  char *tmp_str = (char *) NULL;
  int token_length = 0;

  if (!(*line) || !delim)
    return token;

  tmp_str = strstr (*line, delim);
  if (tmp_str)
    token_length = strlen (*line) - strlen (tmp_str);
  else
    token_length = strlen (*line);

  token = (char *) malloc (sizeof (char) * (token_length + 1));
  strncpy (token, *line, token_length);
  token[token_length] = (char) NULL;
  *line = tmp_str;

  return token;
}

char *
get_token_with_strdelim_i (char **line, char *delim)
{
  char *token = (char *) NULL;
  char *tmp_str = (char *) NULL;
  int token_length = 0;

  if (!(*line) || !delim)
    return token;

  tmp_str = strcasestr (*line, delim);
  if (tmp_str)
    token_length = strlen (*line) - strlen (tmp_str);
  else
    token_length = strlen (*line);

  token = (char *) malloc (sizeof (char) * (token_length + 1));
  strncpy (token, *line, token_length);
  token[token_length] = (char) NULL;
  *line = tmp_str;

  return token;
}

char *
get_home_directory (void)
{
  struct passwd *current_passwd;
  uid_t user_id;
  setpwent ();
  user_id = getuid ();
  while ((current_passwd = getpwent ()))
    {
      if (current_passwd->pw_uid == user_id)
	return current_passwd->pw_dir;
    }
  return NULL;
}

char *
get_config_directory (void)
{
  char *config_directory;
  int length =
    strlen (get_home_directory ()) + strlen (FI_CONFIG_DIRECTORY) + 2;

  config_directory = (char *) calloc (length, sizeof (char));
  sprintf (config_directory, "%s/" FI_CONFIG_DIRECTORY,
	   get_home_directory ());

  return config_directory;
}

char *
get_config_filename (void)
{
  char *config_filename;
  int length = strlen (get_home_directory ()) + strlen (FI_CONFIG_FILE) + 2;

  config_filename = (char *) calloc (length, sizeof (char));
  sprintf (config_filename, "%s/" FI_CONFIG_FILE, get_home_directory ());
  return config_filename;
}


char *
get_global_extensions_directory (void)
{
  char *global_extensions_directory;
  int length = strlen (FI_GLOBAL_EXTENSIONS_DIRECTORY) + 2;

  global_extensions_directory = (char *) calloc (length, sizeof (char));
  sprintf (global_extensions_directory, "%s/",
	   FI_GLOBAL_EXTENSIONS_DIRECTORY);

  return global_extensions_directory;
}

char *
get_local_extensions_directory (void)
{
  char *local_extensions_directory;
  int length = strlen (get_home_directory ()) +
    strlen (FI_LOCAL_EXTENSIONS_DIRECTORY) + 2;

  local_extensions_directory = (char *) calloc (length, sizeof (char));
  sprintf (local_extensions_directory, "%s/%s/", get_home_directory (),
	   FI_LOCAL_EXTENSIONS_DIRECTORY);

  return local_extensions_directory;
}

void
fi_load (char *filename)
{
  char *extension_filepath;
  struct stat buf;
  int length;

  if (stat (filename, &buf) == 0)
    {
      gh_eval_file_with_standard_handler (filename);
      return;
    }

  length = strlen (get_local_extensions_directory ()) + strlen (filename) + 1;
  extension_filepath = (char *) calloc (length, sizeof (char));
  sprintf (extension_filepath, "%s%s", get_local_extensions_directory (),
	   filename);
  if (stat (extension_filepath, &buf) == 0)
    {
      gh_eval_file_with_standard_handler (extension_filepath);
      return;
    }
  free (extension_filepath);

  length =
    strlen (get_global_extensions_directory ()) + strlen (filename) + 1;
  extension_filepath = (char *) calloc (length, sizeof (char));
  sprintf (extension_filepath, "%s%s", get_global_extensions_directory (),
	   filename);
  if (stat (extension_filepath, &buf) == 0)
    {
      gh_eval_file_with_standard_handler (extension_filepath);
      return;
    }
}

int
open_free_udp_port (void)
{
  int sockfd;
  int sockname_len;
  struct sockaddr_in sockname;
  int free_port=1025;
  int err;
  extern int errno;

  sockfd = socket (AF_INET, SOCK_DGRAM, 0);
  if (sockfd < 0)
    error (EXIT_FAILURE, errno, "open_free_udp_port[socket]");

  for (; free_port < 65535; free_port++)
    {
      memset (&sockname, 0, sizeof (struct sockaddr_in));
      sockname.sin_family = AF_INET;
      sockname.sin_port   = htons (free_port);
      sockname.sin_addr.s_addr = htonl (INADDR_ANY);
      sockname_len = sizeof (struct sockaddr_in);
      
      if ((err = bind (sockfd, (struct sockaddr *) &sockname, sockname_len)) == 0)
	return sockfd;
      else
	{
	  if (errno == EADDRINUSE)
	    continue;
	  else
	    error (EXIT_FAILURE, errno, "open_free_udp_port [bind err]");
	}
    }
  close (sockfd);
  error (EXIT_FAILURE, errno, "open_free_udp_port [no free port]");
  // avoid compiler warning
  return (-1);
}


char *
fi_getline (FILE *fp)
{
  char *line_string = NULL;
  char *line_start_ptr = NULL;
  size_t n = 0;
  int retval = 0;
  char *line = NULL;
  
  if (fp == NULL)
    return NULL;
  
  while (1)
    {
      if (line_string)
	{
	  free (line_string);
	  line_string = NULL;
	  line_start_ptr = NULL;
	  n = 0;
	}
      
      retval = getline (&line_string, &n, fp);
      if (retval == -1)
	break;
      
/*       printf ("%s:%d: line_string: [%s]\n", __FILE__, __LINE__, line_string); */
      
      line_start_ptr = stripwhite (line_string);
      
      if (strcmp (line_start_ptr, "\n") == 0 || 
	  strcmp (line_start_ptr, "\r\n") == 0)
	continue;
      
      if (line_start_ptr[0] != COMMENT_CHAR)
	break;
    }
  
  if (line_start_ptr)
    line = strdup (line_start_ptr);
  
  if (line_string)
    free (line_string);
  
  if (line)
    {
      if (line[strlen(line) - 1] == '\n')
	line[strlen(line) - 1] = 0;
      
      if (line[strlen(line) - 1] == '\r')
	line[strlen(line) - 1] = 0;
    }
  
  return line;
}


char *
fi_get_value (char *line)
{
  char *token_start_ptr = NULL;
  
  token_start_ptr = strchr (line,  ' ');
  if (token_start_ptr == NULL)
    return NULL;
  
  return strdup (stripwhite (token_start_ptr));
}

int 
is_valid_ip (char *ip)
{
  struct in_addr addr;
  return inet_aton (ip, &addr);
}

int 
is_valid_mac_address (char *mac_address)
{
  int i;
  
  for (i = 0; mac_address[i]; i++)
    {
      if (i >= 17)
	return 0;
      
      if ((i + 1) % 3 == 0)
	{
	  if (mac_address[i] == ':')
	    continue;
	  return 0;
	}
      
      if (!isxdigit (mac_address[i]))
	return 0;
    }
  
  if (i != 17)
    return 0;
  
  return 1;
}

