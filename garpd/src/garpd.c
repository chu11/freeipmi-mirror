/* 
   $Id: garpd.c,v 1.3.4.1 2006-02-13 17:01:42 chu11 Exp $

   garpd - Gratuitous ARP Daemon - Send Gratuitous ARPs for each ARP
   MAC address mapping from the config file.

   Copyright (C) 2005 FreeIPMI Core Team

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
*/

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <argp.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif
#ifndef __FreeBSD__
#include <sys/io.h>
#endif
#include <syslog.h>
#include <assert.h>
#include <stdarg.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <errno.h>
#include <getopt.h>
#include <stdint.h>
#include <sys/stat.h>
#include <sys/select.h>
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

#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <net/if.h>
#include <net/ethernet.h>
#include <netpacket/packet.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef STDC_HEADERS
#include <string.h>
#else
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

#define FI_DEFAULT_CONFIG_DIRECTORY PATH_CFG "/garpd"
#define FI_DEFAULT_CONFIG_FILE      FI_DEFAULT_CONFIG_DIRECTORY "/garpd.conf"

const char *argp_program_version = PACKAGE_VERSION;
const char *argp_program_bug_address = "<" PACKAGE_BUGREPORT ">";
/* Program documentation. */
static char doc[] =  "GNU FreeIPMI (garpd) -- Gratuitous ARP Daemon";
/* A description of the arguments we accept. */
static char args_doc[] = "";

void parse_config_line (char *line);
char mode; 

/* ********************** */
#define IFACE_MAX       64
#define DEFAULT_DELAY   3000

/* ********************** */

unsigned long batch_delay = DEFAULT_DELAY;
unsigned long interval_delay = DEFAULT_DELAY;


/* The options we understand. */
static struct argp_option options[] = {
  {"verbose",   'v', 0, 0,  "Produce verbose output" },
  {"quiet",     'q', 0, 0,  "Do not produce any output" },
  {"silent",    's', 0, OPTION_ALIAS },
  {"batch-delay", 'b', "NUM", 0, "Batch delay"},
  {"interval-delay", 'i', "NUM", 0, "Interval delay"},
  {"no-daemon", 'D', 0, 0, "Do not become a daemon"},
  {"send-once", 'o', "SPEC", 0, "Send Gratuitous ARP defined by \"spec\""},
  {"config-file", 'c', "FILE", 0,
   "Configuration file"},
  { 0 }
};

struct arguments
{
  char *args[2];                /* ARG1 & ARG2 */
  int silent, verbose, no_daemon;
  char *config_file;
};

/* Parse a single option. */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  /* Get the INPUT argument from 'argp_parse', which we
     know is a pointer to our arguments structure. */
  struct arguments *arguments = state->input;
  switch (key)
    {
    case 'q': case 's':
      arguments->silent = 1;
      break;
    case 'v':
      arguments->verbose = 1;
      break;
    case 'b':
      batch_delay = atoi (arg);
      break;
    case 'i':
      interval_delay = atoi (arg);
      break;
    case 'D':
      arguments->no_daemon = 1;
      break;
    case 'o':
      mode = 1;
      parse_config_line (arg);
      break;
    case 'c':
      arguments->config_file = arg;
      break;
    case ARGP_KEY_ARG:
      if (state->arg_num >= 5)
	/* Too many arguments. */
	argp_usage (state);
      arguments->args[state->arg_num] = arg;
      break;
/*     case ARGP_KEY_END: */
/*       if (state->arg_num < 2) */
/* 	/\* Not enough arguments. *\/ */
/* 	argp_usage (state); */
/*       break; */

    default:
      return ARGP_ERR_UNKNOWN;
    }
  return 0;
}

/* Our argp parser. */
static struct argp argp = { options, parse_opt, args_doc, doc};

#ifndef IFNAMSIZ
#define IFNAMSIZ 16
#endif

#define DELIMITER " \t\n\r="
#define MSG(fmt, args...) printf (fmt, ## args)
#define ERR(fmt, args...) fprintf (stderr, fmt, ## args)

int iface_sock_map[IFACE_MAX];

struct map {
    struct map *next;
    struct map *maps;
    char name[IFNAMSIZ * 64];
    int index[IFACE_MAX];
    int count;
    unsigned char mac[6];
    unsigned long int ip;
/*     unsigned long delay; */
/*     struct timeval last_shot; */
};

struct map *garpd_list;
char *config_filename;
char default_iface[IFNAMSIZ * 64];

static inline unsigned int
get_random_seed (void)
{
  unsigned int seed;
  int fd;
  
  if ((fd = open ("/dev/urandom", O_RDONLY)) == -1)
    goto fail_over_seed;
  
  if (read (fd, &seed, sizeof (seed)) < sizeof (seed))
    goto fail_over_seed;

  close (fd);
  return (seed);

 fail_over_seed:
  return ((unsigned int) time (0));
}

void
parse_error (char *file, int line, char *msg)
{
    fprintf(stderr, "Error ");
    if (file)
	fprintf (stderr, "in file %s ", file);
    if (line)
	fprintf (stderr, "(line %d) ", line);
    if (msg)
	fprintf (stderr, "\"%s\"\n", msg);
    exit (1);
}

unsigned long int 
do_dns (char *name)
{
    struct hostent *he;
    struct in_addr in;

    he = gethostbyname (name);
    if (!he) {
	MSG ("Resolving %s ...", name);
	MSG ("Failed!\n");
	return 0;
    }
    in.s_addr = *(unsigned long *) he->h_addr;
/*     MSG ("ip=%x\n",in.s_addr); */
    return ((unsigned long) in.s_addr);
}

inline void
free_resources ()
{
    struct map *clean = garpd_list;
    int i;

    garpd_list = NULL;

    while (clean) 
      {
	struct map *old;	/* = clean; */
	old = clean;
	clean = clean->next;
	free (old);
      }

    for (i = 0; i < IFACE_MAX; i++) 
      {
	if (iface_sock_map[i] != -1)
	  {
	    close (iface_sock_map[i]);
	    iface_sock_map[i] = -1;
	  }
      }
}

void parse_config_line (char *chic)
{
	char *ptr;
	char mac_flag, ip_flag;

        static int line_no = 0;
	char mac[18];
	unsigned long ip=0;
	unsigned long delay;
	char iface[IFNAMSIZ];

	struct in_addr ip_in;


	/* consider her comments */
	if ((ptr = strchr (chic, '#')))
	  *ptr = 0;
	if ((ptr = strchr (chic, ';')))
	  *ptr = 0;

	ptr = strtok (chic, DELIMITER);
	line_no++;

	mac_flag = ip_flag = 0;
	delay = 0;		/* batch_delay; */
	strcpy (iface, "default");

	/* empty line */
	if (!ptr)
	    return;


#define NEXT_TOKEN  do {    \
        ptr = strtok (NULL, DELIMITER);    \
} while (0)


#define NEXT_LIST_TOKEN  do {    \
        ptr = strtok (NULL, "[]");    \
        if (!ptr) { \
            parse_error (config_filename, line_no, "Incomplete line");   \
        }   \
} while (0)

#define NEXT_EMPTY_TOKEN  do {    \
        ptr = strtok (NULL, DELIMITER);    \
        if (ptr) { \
            parse_error (config_filename, line_no, ptr);   \
        }   \
} while (0)


#define NEXT_NONEMPTY_TOKEN  do {    \
        ptr = strtok (NULL, DELIMITER);    \
        if (!ptr) { \
            parse_error (config_filename, line_no, "Incomplete line");   \
        }   \
} while (0)

#define NEXT_TOKEN_IS_EQUAL_OR_DIE  do {    \
        NEXT_NONEMPTY_TOKEN; \
        if (strcmp (ptr,"=")) { \
            parse_error (config_filename, line_no, ptr); \
        }   \
} while (0)

	if (!strcasecmp(ptr, "default")) {
	    NEXT_NONEMPTY_TOKEN;
	    if (!strcasecmp(ptr, "iface")) {
		ptr += 6;
		while (*ptr
		       && (*ptr == ' ' || *ptr == '\t' || *ptr == '\n'))
		    ptr++;
		if (*ptr == '[') {
		    NEXT_LIST_TOKEN;
		} else {
		    NEXT_NONEMPTY_TOKEN;
		}
		strcpy(default_iface, ptr);
	    } else {
		parse_error(config_filename, line_no, ptr);
	    }
	    NEXT_EMPTY_TOKEN;
	    return;
	}
       if (!strcasecmp(ptr, "batch-delay")) {
           NEXT_NONEMPTY_TOKEN;
           if (batch_delay) return; /* already set from command line */
	   batch_delay = atol(ptr);
           if (!batch_delay) {
               ERR("Invalid batch delay %s\n", ptr);
               exit(1);
           }
           return;
       }

       if (!strcasecmp(ptr, "interval-delay")) {
           NEXT_NONEMPTY_TOKEN;
           if (interval_delay) return;
	   interval_delay = atol(ptr);
           if (!interval_delay) {
               ERR("Invalid interval delay %s\n", ptr);
               exit(1);
           }
           return; 
       }

#define NEXT_TOKEN_IS_IP_OR_DIE  do {    \
        NEXT_NONEMPTY_TOKEN; \
        ip = do_dns (ptr);  \
        if (!ip) {  \
            parse_error (config_filename, line_no, ptr); \
        }   \
} while (0)

#define NEXT_TOKEN_IS_MAC_OR_DIE do { \
    NEXT_NONEMPTY_TOKEN;        \
    int dummy;      \
    if (sscanf (ptr,"%2x:%2x:%2x:%2x:%2x:%2x",        \
             &dummy,&dummy,&dummy,&dummy,&dummy,&dummy) != 6) { \
            parse_error (config_filename, line_no, ptr);     \
    }       \
}while (0)

        if (mode == 2) return;
	/* one iteration per var=value */
	while (ptr) {
	    if (!strcasecmp(ptr, "host")) {
/*                 NEXT_TOKEN_IS_EQUAL_OR_DIE; */
		NEXT_TOKEN_IS_IP_OR_DIE;
		ip_flag = 1;
	    } else if (!strcasecmp(ptr, "mac")) {
/*                 NEXT_TOKEN_IS_EQUAL_OR_DIE; */
		NEXT_TOKEN_IS_MAC_OR_DIE;
		strcpy(mac, ptr);
		mac_flag = 1;
/* 	    } else if (!strcasecmp(ptr, "delay")) { */
/*              NEXT_TOKEN_IS_EQUAL_OR_DIE; */
/* 		NEXT_NONEMPTY_TOKEN; */
/* 		delay = atol(ptr); */
	    } else if (!strcasecmp(ptr, "iface")) {
		ptr += 6;
		while (*ptr &&
		       (*ptr == ' ' || *ptr == '\t' || *ptr == '\n'
			|| *ptr == '='))
		    ptr++;
		if (*ptr == '[') {
		    NEXT_LIST_TOKEN;
		} else {
		    NEXT_NONEMPTY_TOKEN;
		}
		strcpy(iface, ptr);
	    } else {
		parse_error(config_filename, line_no, ptr);
	    }
	    NEXT_TOKEN;
	}
	/* process entry */
	if (!ip_flag || !mac_flag) {
	    parse_error(config_filename, line_no, "Insufficient info");
	}

	ip_in.s_addr = ip;
	{
	    struct map *newif,*trav;

	    newif = (struct map *) calloc(1, sizeof(*garpd_list));

            if (!garpd_list) {
                garpd_list = newif;
            } else {
                trav = garpd_list;
                while (trav->next)
                    trav = trav->next;
                trav->next = newif;
            }

	    sscanf (mac, "%2x:%2x:%2x:%2x:%2x:%2x",
		    (unsigned int *)(unsigned char *) &garpd_list->mac[0],
		    (unsigned int *)(unsigned char *) &garpd_list->mac[1],
		    (unsigned int *)(unsigned char *) &garpd_list->mac[2],
		    (unsigned int *)(unsigned char *) &garpd_list->mac[3],
		    (unsigned int *)(unsigned char *) &garpd_list->mac[4],
		    (unsigned int *)(unsigned char *) &garpd_list->mac[5]);

       strcpy(newif->name, iface);
       newif->ip = ip;
/*     newif->delay = delay; */
/*     rnd = rand() % batch_delay; */
/*     garpd_list->last_shot.tv_sec = time(NULL) + (rnd / 1000); */
/*     garpd_list->last_shot.tv_usec = (rnd % 1000) * 1000; */
    }
}

void
load_config_file (char *cf)
{
    FILE *cfp;
    char chic[2048];
    extern int errno;

    cfp = fopen (cf, "r");

    if (!cfp)
      {
	fprintf (stderr, "garpd: fopen (%s): %s\n", cf, strerror (errno));
	exit (1);
      }

    /* one iteration per entry */
    while (fgets (chic, 2048, cfp)) {
	/* play safe, just in case :) */
	chic[2047] = 0;

        parse_config_line (chic);
    }
}


void
send_garpd (int socket, struct map *map)
{
    static unsigned char arp_req[42] = {
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff,	/* ff:ff:ff:ff:ff:ff dst mac */
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00,	/* src mac */
	0x08, 0x06,		/* ETH_P_ARP */

	0x00, 0x01,		/* ARPHRD_ETHER    */
	0x08, 0x00,		/* ETH_P_IP */
	0x06,			/* ETH_ALEN */
	0x04,			/* 4 */
	0x00, 0x01,		/* ARPOP_REQUEST */

	0x00, 0x00, 0x00, 0x00, 0x00, 0x00,	/* ar_sha */
	0x00, 0x00, 0x00, 0x00,	/* ar_sip */
	0xff, 0xff, 0xff, 0xff, 0xff, 0xff,	/* ff:ff:ff:ff:ff:ff ar_tha */
	0xff, 0xff, 0xff, 0xff	/* 255.255.255.255 ar_tip */
    };

    memcpy ((void *) &arp_req[6], map->mac, 6);
    memcpy ((void *) &arp_req[22], map->mac, 6);
    *(unsigned long *) &arp_req[28] = map->ip;
/*     memcpy ((void *) &arp_req[32], map->mac, 6); */
    *(unsigned long *) &arp_req[38] = map->ip;

    write (socket, arp_req, 42);
}

#if 0
void
garpd_ticker (struct map *map, int socket)
{
    struct timeval tv;
    gettimeofday (&tv, NULL);
    if (!map->delay) {
	map->delay = batch_delay;
    }
    if ((map->last_shot.tv_usec + map->last_shot.tv_sec * 1000000) +
	(map->delay * 1000) <= (tv.tv_usec + tv.tv_sec * 1000000)) {
	/* waited enough! */
	send_garpd (socket, map);
	map->last_shot.tv_sec = tv.tv_sec;
	map->last_shot.tv_usec = tv.tv_usec;
    }
}
#endif

void
garpd_socket_init ()
{
    struct map *inf = garpd_list;
    struct sockaddr_ll sll;

    for (inf = garpd_list; inf; inf = inf->next) {
	char *iface;

	if (!strcasecmp (inf->name, "default")) {
	    if (!default_iface[0]) {
		ERR ("No default interface set, but needed\n");
		exit (1);
	    }
	    strcpy (inf->name, default_iface);
	}

	iface = inf->name;

	iface = strtok (iface, " \t\n");

	while (iface) {
	    int idx;

	    idx = if_nametoindex (iface);
	    if (idx == 0) {
		perror ("if_nametoindex()");
		exit (1);
	    }
	    inf->index[inf->count++] = idx;
	    if (iface_sock_map[idx] == -1) {
		iface_sock_map[idx] = socket (PF_PACKET,
					     SOCK_RAW, htons(ETH_P_ARP));
		if (iface_sock_map[idx] == -1) {
		    perror ("socket()");
		    exit (1);
		}
		sll.sll_family = PF_PACKET;
		sll.sll_ifindex = idx;
		sll.sll_protocol = htons (ETH_P_ARP);

		if (bind (iface_sock_map[idx], (struct sockaddr *) &sll,
			 sizeof (struct sockaddr_ll)) == -1) {
		    perror ("bind()");
		    exit (1);
		}
	    }
	    iface = strtok (NULL, " \t\n");
	}
    }
}

void garpd_loop ()
{
    struct map *inf;

    while (1) {
	inf = garpd_list;
	while (inf)
	  {
	    int i;
	    for (i = 0; i < inf->count; i++)
	      {
                /* each send_garpd here goes to a different
                 * interface. no need to sleep () between
                 * them as I am not burst flooding
                 */
		send_garpd (iface_sock_map[inf->index[i]], inf);
	      }
	    inf = inf->next;
            usleep (interval_delay * 1000);
	  }
        if (mode == 2) return;
	usleep (batch_delay * 1000);
    }
}

int
main (int argc, char *argv[])
{
  struct arguments arguments;

  /* Default values. */
  memset (&arguments, 0, sizeof (arguments));

  argp_parse (&argp, argc, argv, 0, 0, &arguments);

/*     if (argc != 2) { */
/* 	fprintf (stderr, "Usage: %s <config file>\n", argv[0]); */
/* 	exit (1); */
/*     } */

    srand (get_random_seed ());
    if (arguments.config_file)
      config_filename = arguments.config_file;
    else
      config_filename = strdup (FI_DEFAULT_CONFIG_FILE);

//    garpd_list = NULL;
    memset (iface_sock_map, -1, IFACE_MAX);
    /* clean_me (); */

    mode++;
    load_config_file (config_filename);
    garpd_socket_init ();

    if (!arguments.no_daemon)
      daemon (0, 0);

    garpd_loop ();
 
    /* control should never reach here */
    /* why not? :) */
    return (0);
}
