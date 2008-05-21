/*****************************************************************************\
 *  $Id: hostrange.h,v 1.5 2008-05-21 16:40:16 chu11 Exp $
\*****************************************************************************/

#ifndef _HOSTRANGE_H
#define _HOSTRANGE_H

/* Returns number of hosts setup for, -1 on error */
int pstdout_setup(char **hosts,
                  int buffer_output,
                  int consolidate_output,
                  int fanout,
                  int eliminate,
                  int always_prefix);

#endif /* _HOSTRANGE_H */
