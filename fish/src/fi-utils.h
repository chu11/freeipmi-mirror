/* 
    fi-utils.h: all utility/misc functions

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

#ifndef whitespace
#define whitespace(c) (((c) == ' ') || ((c) == '\t'))
#endif

#ifdef __ia64__
#define F_D64 "%ld"
#define F_X64 "%lX"
#else
#define F_D64 "%qd"
#define F_X64 "%qX"
#endif

#define COMMENT_CHAR    '#'

/* This macro safely inserts the message into readline buffer without
   distracting the current line or state of readline
*/
int temp_rl_point;


/* this macro is ment for printing asynchronous messages. for example
   messages from process_* and handle_yahoo_message in yahoo-wrapper.c
   are asynchronous messages 
*/
#define PRINTF_MESSAGE(format, args...) \
{ \
  temp_rl_point = rl_point; \
  rl_kill_text (1, rl_end); \
  rl_redisplay (); \
 \
  printf (format, ##args); \
 \
  rl_do_undo (); \
  rl_point = temp_rl_point; \
  rl_reset_line_state (); \
  rl_forced_update_display (); \
  bell (); \
}

/* use this macro only for exporting to scheme environment.
   FIXME: still not working. test with proudofgnuyahoo.scm
*/
 #define EX_PRINTF_MESSAGE(format, args...) \
{ \
  temp_rl_point = rl_point; \
  rl_kill_text (0, rl_end); \
  rl_redisplay (); \
 \
  printf (format, ##args); \
 \
  rl_do_undo (); \
  rl_point = temp_rl_point; \
  rl_reset_line_state (); \
  rl_forced_update_display (); \
  bell (); \
}

#ifndef __FreeBSD__
/* defining strcasestr function temporarily */
extern char *strcasestr (__const char *__haystack, __const char *__needle)
     __THROW __attribute_pure__;
#endif

void get_terminal_attributes (void);
void set_terminal_attributes (void);
void bell (void);
char *stripwhite (char *string);
char *get_token (char **line);
char *get_token_with_strdelim (char **line, char *delim);
char *get_token_with_strdelim_i (char **line, char *delim);
char *get_home_directory (void);
char *get_config_filename (void);
char *get_config_directory (void);
char *get_global_extensions_directory (void);
char *get_local_extensions_directory (void);
int fi_load (char *filename);
char *fi_getline (FILE *fp);
char *fi_get_value (char *line);
int is_valid_ip (char *ip);
int is_valid_mac_address (char *mac_address);
