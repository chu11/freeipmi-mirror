/*****************************************************************************\
 *  $Id: hostrange.h,v 1.1 2007-06-02 19:48:14 chu11 Exp $
\*****************************************************************************/

#ifndef _HOSTRANGE_H
#define _HOSTRANGE_H

/* Returns number of hosts setup for, -1 on error */
int pstdout_setup(char **hosts,
                  int buffer_hostrange_output,
                  int consolidate_hostrange_output,
                  int fanout,
                  int eliminate);

#endif /* _HOSTRANGE_H */
