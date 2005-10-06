/* 
   ipmi-utils.h - general utility procedures

   Copyright (C) 2003, 2004, 2005 FreeIPMI Core Team

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

*/

#ifndef _IPMI_UTILS_H
#define	_IPMI_UTILS_H	1

#ifdef __cplusplus
extern "C" {
#endif

#ifndef __cplusplus
 
#if !defined(__STDC_VERSION__) || (__STDC_VERSION__ < 199901L)
# if !defined(__GNUC__) ||(__GNUC__ < 3)
  typedef char _Bool;           /* For C compilers without _Bool */
# endif
#endif
 
#define bool  _Bool
#define true  1
#define false 0
 
#else
 
  /* C++ */
#define bool  bool
#define true  true
#define false false
#endif
#define __bool_true_false_are_defined 1

#define FREEIPMI_MIN(x,y)  ((x < y) ? x : y)

#define IPMI_COMP_CODE(obj_cmd)  obj_cmd[1]

typedef  int8_t ipmi_chksum_t;
ipmi_chksum_t ipmi_chksum (u_int8_t *buf, u_int64_t len);
int8_t ipmi_chksum_test (u_int8_t *buf, u_int64_t len);
int8_t ipmi_comp_test (fiid_obj_t obj_cmd);
int ipmi_input_timeout (int fd, unsigned int seconds);
int ipmi_is_root ();
unsigned int ipmi_get_random_seed (void);
int ipmi_open_free_udp_port (void);
int ipmi_ioremap (u_int64_t physical_addr, size_t physical_addr_len, void **virtual_addr, void **mapped_addr, size_t *mapped_addr_len);
int ipmi_iounmap (void *mapped_addr, size_t mapped_addr_len);
int ipmi_get_physical_mem_data (u_int64_t physical_address, size_t length, u_int8_t *data);



#ifdef __cplusplus
}
#endif

#endif /* ipmi-utils.h */


