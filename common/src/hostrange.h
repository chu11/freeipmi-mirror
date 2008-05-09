/*****************************************************************************\
 *  $Id: hostrange.h,v 1.4.2.2 2008-05-09 21:12:05 chu11 Exp $
\*****************************************************************************/

#ifndef _HOSTRANGE_H
#define _HOSTRANGE_H

#include "hostmap.h"

/* Returns number of hosts setup for, -1 on error */
int pstdout_setup(char **hosts,
                  hostmap_t hmap,
                  int buffer_hostrange_output,
                  int consolidate_hostrange_output,
                  int fanout,
                  int eliminate,
                  int always_prefix);

#endif /* _HOSTRANGE_H */
