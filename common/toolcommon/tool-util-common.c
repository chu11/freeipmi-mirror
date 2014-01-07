/*
 * Copyright (C) 2003-2014 FreeIPMI Core Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

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
#include <sys/resource.h>
#include <errno.h>
#include <assert.h>

#include <freeipmi/freeipmi.h>

#include "tool-util-common.h"

#include "freeipmi-portability.h"

int
ipmi_is_root ()
{
#if IPMI_DONT_CHECK_FOR_ROOT
  return (1);
#else /* !IPMI_DONT_CHECK_FOR_ROOT */
  uid_t uid = getuid ();
  if (uid == 0)
    return (1);
  return (0);
#endif /* !IPMI_DONT_CHECK_FOR_ROOT */
}

void
ipmi_disable_coredump (void)
{
  /* Disable core dumping when not-debugging.  Do not want username,
   * password or other important stuff to core dump.
   */
#ifdef NDEBUG
  struct rlimit resource_limit;

  if (!getrlimit (RLIMIT_CORE, &resource_limit))
    {
      resource_limit.rlim_cur = 0;
      if (setrlimit (RLIMIT_CORE, &resource_limit) < 0)
        perror ("warning: setrlimit()");
    }
#endif /* NDEBUG */
}

/* Check if kg len is decent */
int
check_kg_len (const char *in)
{
  assert (in);

  if (strlen (in) == 0)
    return (0);

  if (strncasecmp (in, "0x", 2) == 0)
    {
      if (strlen (in) > IPMI_MAX_K_G_LENGTH*2+2)
        return (-1);
    }
  else
    {
      if (strlen (in) > IPMI_MAX_K_G_LENGTH)
        return (-1);
    }

  return (0);
}

char *
format_kg (char *out, unsigned int outlen, const void *k_g)
{
  unsigned int i;
  int printable = 1;
  int foundnull = 0;
  char *p;
  const uint8_t *k_g_ptr = k_g;

  assert (out);
  assert (outlen > IPMI_MAX_K_G_LENGTH*2+2);
  assert (k_g);

  /* Are there any characters that would prevent printing this as a
     string on a single line? */
  for (i = 0; i < IPMI_MAX_K_G_LENGTH; i++)
    {
      if (k_g_ptr[i] == '\0')
        {
          ++foundnull;
          continue;
        }
      if (!(isgraph (k_g_ptr[i]) || k_g_ptr[i] == ' ') || foundnull)
        {
          printable = 0;
          break;
        }
    }

  /* print out an entirely null key in hex rather than an empty
     string */
  if (foundnull == IPMI_MAX_K_G_LENGTH)
    printable = 0;

  /* don't print out a key starting with a literal '0x' as a string,
     since parse_kg will try to interpret such strings as hex */
  if (k_g_ptr[0] == '0' && k_g_ptr[1] == 'x')
    printable = 0;

  if (printable)
    {
      if (outlen < IPMI_MAX_K_G_LENGTH+1)
        return (NULL);
      p = out;
      for (i = 0; i < IPMI_MAX_K_G_LENGTH; i++)
        {
          if (k_g_ptr[i] == '\0')
            break;
          p[i] = k_g_ptr[i];
        }
      p[i] = 0;
    }
  else
    {
      if (outlen < IPMI_MAX_K_G_LENGTH*2+3)
        return (NULL);
      p = out;
      p[0] = '0'; p[1] = 'x';
      p+=2;
      for (i = 0; i < IPMI_MAX_K_G_LENGTH; i++, p+=2)
        sprintf (p, "%02x", k_g_ptr[i]);
    }

  return (out);
}

unsigned int
get_timestamp_flags (struct common_cmd_args *common_args, unsigned int defaultflags)
{
  unsigned int timestamp_flags = defaultflags;

  if (common_args->utc_to_localtime)
    timestamp_flags |= IPMI_TIMESTAMP_FLAG_UTC_TO_LOCALTIME;

  if (common_args->localtime_to_utc)
    timestamp_flags |= IPMI_TIMESTAMP_FLAG_LOCALTIME_TO_UTC;

  return (timestamp_flags);
} 
