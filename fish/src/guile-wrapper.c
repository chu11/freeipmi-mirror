/* 
   guile_wrapper.c: higher level wrapper to guile functions
   
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
#include <guile/gh.h>
#include "guile-wrapper.h"

SCM
fish_exception_handler (void *data, SCM tag, SCM throw_args)
{
  fprintf (stderr, "\n>>--:>  >>--:>  >>--:> >>--:>");
  fprintf (stderr, "\n~ ~   Cat ate the fish!!  ~ ~");
  fprintf (stderr, "\n>>--:>  >>--:>  >>--:> >>--:>");
  fprintf (stderr, "\nFish Exception Handler:");
  fprintf (stderr, "\ntag        : ");
  fflush (stdout);
  gh_display (tag);
  fprintf (stderr, "\nthrow args : ");
  fflush (stdout);
  gh_display (throw_args);
  fprintf (stderr, "\ndata       : [%s]\n", (char *) data);
  fflush (stdout);
  scm_backtrace ();
  fflush (stdout);
  gh_newline();
  return SCM_UNSPECIFIED;
}

