#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#include <stdarg.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/resource.h>
#include <errno.h>
#include <assert.h>

#include "tool-common.h"
#include "freeipmi-portability.h"

#include "freeipmi/ipmi-messaging-support-cmds.h"

int
ipmi_is_root ()
{
  uid_t uid = getuid ();
  if (uid == 0)
    return 1;
  return 0;
}

void
ipmi_disable_coredump(void)
{
  /* Disable core dumping when not-debugging.  Do not want username,
   * password or other important stuff to core dump.
   */
#ifdef NDEBUG
  struct rlimit resource_limit;

  if (!getrlimit(RLIMIT_CORE, &resource_limit))
    {
      resource_limit.rlim_cur = 0;
      if (setrlimit (RLIMIT_CORE, &resource_limit) != 0)
        perror ("warning: setrlimit()");
    }
#endif /* NDEBUG */
}

ipmi_device_t
ipmi_device_open(const char *progname,
                 const char *hostname,
                 struct common_cmd_args *cmd_args,
                 unsigned int debug_flags,
                 char *errmsg,
                 unsigned int errmsglen)
{
  ipmi_device_t dev = NULL;

  if (!(dev = ipmi_device_create()))
    {
      snprintf(errmsg, 
               errmsglen, 
               "ipmi_device_create: %s",
               strerror(errno));
      goto cleanup;
    }

  if (hostname && strcmp(hostname, "localhost") != 0)
    {
      if (cmd_args->driver_type == IPMI_DEVICE_LAN_2_0)
        {
          if (ipmi_open_outofband_2_0 (dev,
                                       IPMI_DEVICE_LAN_2_0,
                                       hostname,
                                       cmd_args->username,
                                       cmd_args->password,
                                       (cmd_args->k_g_configured) ? cmd_args->k_g : NULL,
                                       (cmd_args->k_g_configured) ? IPMI_MAX_K_G_LENGTH : 0,
                                       cmd_args->privilege_level,
                                       cmd_args->cipher_suite_id,
                                       cmd_args->session_timeout,
                                       cmd_args->retry_timeout,
                                       0, /* XXX workaround flags later */
                                       debug_flags) < 0)
            {
              if (ipmi_device_errnum(dev) == IPMI_ERR_USERNAME
                  || ipmi_device_errnum(dev) == IPMI_ERR_PASSWORD
                  || ipmi_device_errnum(dev) == IPMI_ERR_K_G
                  || ipmi_device_errnum(dev) == IPMI_ERR_PRIVILEGE
                  || ipmi_device_errnum(dev) == IPMI_ERR_AUTHENTICATION_TYPE
                  || ipmi_device_errnum(dev) == IPMI_ERR_CIPHER_SUITE_UNAVAILABLE
                  || ipmi_device_errnum(dev) == IPMI_ERR_PASSWORD_VERIFICATION_TIMEOUT
                  || ipmi_device_errnum(dev) == IPMI_ERR_INVALID_HOSTNAME
                  || ipmi_device_errnum(dev) == IPMI_ERR_IPMI_2_0_UNAVAILABLE)
                {
                  snprintf(errmsg,
                           errmsglen,
                           "%s: %s",
                           progname,
                           ipmi_device_strerror(ipmi_device_errnum(dev)));
                }
              else
                {
                  snprintf(errmsg,
                           errmsglen,
                           "ipmi_open_outofband_2_0: %s",
                           ipmi_device_strerror(ipmi_device_errnum(dev)));
                }
              goto cleanup;
            }
        }
      else
        {
          if (ipmi_open_outofband (dev,
                                   IPMI_DEVICE_LAN,
                                   hostname,
                                   cmd_args->username,
                                   cmd_args->password,
                                   cmd_args->authentication_type,
                                   cmd_args->privilege_level,
                                   cmd_args->session_timeout,
                                   cmd_args->retry_timeout,
                                   0, /* XXX workaround flags later */
                                   debug_flags) < 0)
            {
              if (ipmi_device_errnum(dev) == IPMI_ERR_USERNAME
                  || ipmi_device_errnum(dev) == IPMI_ERR_PASSWORD
                  || ipmi_device_errnum(dev) == IPMI_ERR_PRIVILEGE
                  || ipmi_device_errnum(dev) == IPMI_ERR_AUTHENTICATION_TYPE
                  || ipmi_device_errnum(dev) == IPMI_ERR_PASSWORD_VERIFICATION_TIMEOUT
                  || ipmi_device_errnum(dev) == IPMI_ERR_INVALID_HOSTNAME)
                {
                  snprintf(errmsg,
                           errmsglen,
                           "%s: %s",
                           progname,
                           ipmi_device_strerror(ipmi_device_errnum(dev)));
                }
              else
                {
                  snprintf(errmsg,
                           errmsglen,
                           "ipmi_open_outofband: %s",
                           ipmi_device_strerror(ipmi_device_errnum(dev)));
                }
              goto cleanup;
            }
        }
    }
  else
    {
      if (!ipmi_is_root())
        {
          snprintf(errmsg,
                   errmsglen,
                   "%s: %s",
                   progname,
                   ipmi_device_strerror(IPMI_ERR_PERMISSION));
          goto cleanup;
        }

      if (cmd_args->driver_type == IPMI_DEVICE_UNKNOWN)
        {
          if (ipmi_open_inband (dev,
                                IPMI_DEVICE_OPENIPMI,
                                cmd_args->disable_auto_probe,
                                cmd_args->driver_address,
                                cmd_args->register_spacing,
                                cmd_args->driver_device,
                                0, /* XXX workaround flags later */
                                debug_flags) < 0)
            {
              if (ipmi_open_inband (dev,
                                    IPMI_DEVICE_KCS,
                                    cmd_args->disable_auto_probe,
                                    cmd_args->driver_address,
                                    cmd_args->register_spacing,
                                    cmd_args->driver_device,
                                    0, /* XXX workaround flags later */
                                    debug_flags) < 0)
                {
                  if (ipmi_open_inband (dev,
                                        IPMI_DEVICE_SSIF,
                                        cmd_args->disable_auto_probe,
                                        cmd_args->driver_address,
                                        cmd_args->register_spacing,
                                        cmd_args->driver_device,
                                        0, /* XXX workaround flags later */
                                        debug_flags) < 0)
                    {
                      snprintf(errmsg,
                               errmsglen,
                               "ipmi_open_inband: %s",
                               ipmi_device_strerror(ipmi_device_errnum(dev)));
                      goto cleanup;
                    }
                }
            }
        }
      else
        {
          if (ipmi_open_inband (dev,
                                cmd_args->driver_type,
                                cmd_args->disable_auto_probe,
                                cmd_args->driver_address,
                                cmd_args->register_spacing,
                                cmd_args->driver_device,
                                0, /* XXX workaround flags later */
                                debug_flags) < 0)
            {
              snprintf(errmsg,
                       errmsglen,
                       "ipmi_open_inband: %s",
                       ipmi_device_strerror(ipmi_device_errnum(dev)));
              goto cleanup;
            }
        }
    }

  return dev;

 cleanup:
  if (dev)
    {
      ipmi_close_device (dev);
      ipmi_device_destroy (dev);
    }
  return NULL;
}              

/* a k_g key is interpreted as ascii text unless it is prefixed with
   "0x", in which case is it interpreted as hexadecimal */
int
parse_kg(unsigned char *outbuf, int outsz, const char *instr)
{
  char *p, *q;
  int i, j;
  char buf[3] = {0, 0, 0};

  assert(outbuf != NULL);
  assert(instr != NULL);
  assert(outsz == IPMI_MAX_K_G_LENGTH);

  if (strlen(instr) == 0)
    return 0;

  if (strncmp(instr, "0x", 2) == 0) 
    {
      if (strlen(instr) > IPMI_MAX_K_G_LENGTH*2+2)
        return -1;
      p = (char *)instr + 2;
      memset(outbuf, 0, IPMI_MAX_K_G_LENGTH);
      for (i = j = 0; i < strlen(p); i+=2, j++)
        {
          if (p[i+1] == '\0')
            return -1;
          buf[0] = p[i]; buf[1] = p[i+1]; buf[2] = 0;
          errno = 0;
          outbuf[j] = strtoul(buf, &q, 16);
          if (errno || (q != buf + 2))
            return -1;
        }
    }
  else
    {
      if (strlen(instr) > IPMI_MAX_K_G_LENGTH)
        return -1;
      memset(outbuf, 0, IPMI_MAX_K_G_LENGTH);
      memcpy(outbuf, instr, strlen(instr));
    }

  return 1;
}

char *
format_kg(char *outstr, int outsz, const unsigned char *k_g)
{
  int i;
  int printable = 1;
  int foundnull = 0;
  char *p;

  assert(outstr != NULL);
  assert(outsz > IPMI_MAX_K_G_LENGTH*2+2);
  assert(k_g != NULL);

  /* Are there any characters that would prevent printing this as a
     string on a single line? */
  for (i = 0; i < IPMI_MAX_K_G_LENGTH; i++)
    {
      if (k_g[i] == '\0')
        {
          ++foundnull;
          continue;
        }
      if (!(isgraph(k_g[i]) || k_g[i] == ' ') || foundnull)
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
  if (k_g[0] == '0' && k_g[1] == 'x')
    printable = 0;

  if (printable)
    {
      if (outsz < IPMI_MAX_K_G_LENGTH+1)
        return NULL;
      p = outstr;
      for (i = 0; i < IPMI_MAX_K_G_LENGTH; i++)
        {
          if (k_g[i] == '\0')
            break;
          p[i] = k_g[i];
        }
      p[i] = 0;
    }
  else
    {
      if (outsz < IPMI_MAX_K_G_LENGTH*2+3)
        return NULL;
      p = outstr;
      p[0] = '0'; p[1] = 'x';
      p+=2;
      for (i = 0; i < IPMI_MAX_K_G_LENGTH; i++, p+=2)
        sprintf(p, "%02x", k_g[i]);
    }

  return outstr;
}

