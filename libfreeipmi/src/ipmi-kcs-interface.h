/* 
   ipmi-kcs-interface.h - IPMI KCS SMS Interface

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

#ifndef _IPMI_KCS_INTERFACE_H
#define _IPMI_KCS_INTERFACE_H 1

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_KCS_SMS_IO_BASE_DEFAULT    0x0CA2
#define IPMI_KCS_SMS_IO_BASE_CDC1620    0x0CA2
#define IPMI_KCS_SMS_IO_BASE_CDC9416    0x0CA2
#define IPMI_KCS_SMS_IO_BASE_SR870BN4   0x08A2 
#define IPMI_KCS_SMS_IO_BASE_CDC6440    0x08A2

/* KCS Interface Status Register Bits */
/* Scheme BIT Calculator Example
  To BIN: 
  (format #f "[~8,'0b]" #x80) => "[10000000]"
  To HEX:
  (format #f "[0x~2,'0x]" #b10000000) => "[0x80]"
*/
#define IPMI_KCS_STATUS_REG_S1          0x80
#define IPMI_KCS_STATUS_REG_S0          0x40
#define IPMI_KCS_STATUS_REG_STATE       (IPMI_KCS_STATUS_REG_S0 | IPMI_KCS_STATUS_REG_S1)
#define IPMI_KCS_STATUS_REG_OEM2        0x20
#define IPMI_KCS_STATUS_REG_OEM1        0x10
#define IPMI_KCS_STATUS_REG_CD          0x08 /* last-written (command or data) */
#define IPMI_KCS_STATUS_REG_SMS_ATN     0x04
#define IPMI_KCS_STATUS_REG_IBF         0x02
#define IPMI_KCS_STATUS_REG_OBF         0x01

/* IPMI KCS Interface State Bits */ 
#define IPMI_KCS_STATE_IDLE   0x00
#define IPMI_KCS_STATE_READ   IPMI_KCS_STATUS_REG_S0
#define IPMI_KCS_STATE_WRITE  IPMI_KCS_STATUS_REG_S1
#define IPMI_KCS_STATE_ERROR  (IPMI_KCS_STATUS_REG_S0 & IPMI_KCS_STATUS_REG_S1)

extern fiid_template_t tmpl_hdr_kcs;

/* High level calls */
/* BMC treats "write followed by a read" as one transaction. It is
   highly recommended to use ipmi_kcs_cmd instead. Otherwise make sure
   you check the return status of write before calling read.
*/

#if 0 /* TEST */
int8_t ipmi_kcs_cmd2 (ipmi_device_t *dev, 
		      fiid_obj_t obj_cmd_rq, 
		      fiid_template_t tmpl_cmd_rq, 
		      fiid_obj_t obj_cmd_rs, 
		      fiid_template_t tmpl_cmd_rs);
int8_t ipmi_kcs_cmd_raw2 (ipmi_device_t *dev, 
			  uint8_t *buf_rq, 
			  size_t buf_rq_len, 
			  uint8_t *buf_rs, 
			  size_t *buf_rs_len);
#endif /* TEST */

int8_t fill_hdr_ipmi_kcs (uint8_t lun, 
			  uint8_t fn, 
			  fiid_obj_t obj_hdr);
int32_t assemble_ipmi_kcs_pkt (fiid_obj_t obj_hdr, 
                               fiid_obj_t obj_cmd, 
                               uint8_t *pkt, 
                               uint32_t pkt_len);
int32_t unassemble_ipmi_kcs_pkt (uint8_t *pkt, 
                                 uint32_t pkt_len, 
                                 fiid_obj_t obj_hdr, 
                                 fiid_obj_t obj_cmd);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-kcs-interface.h */

