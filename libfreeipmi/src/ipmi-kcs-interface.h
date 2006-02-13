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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  

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

/* IPMI KCS Interface Status Codes */
#define IPMI_KCS_STATUS_NO_ERROR             0x00
#define IPMI_KCS_STATUS_SUCCESS              IPMI_KCS_STATUS_NO_ERR
#define IPMI_KCS_STATUS_OK                   IPMI_KCS_STATUS_NO_ERR
#define IPMI_KCS_STATUS_NO_ERROR_STR \
"No error"
          
#define IPMI_KCS_STATUS_ABORTED_BY_CMD         0x01
#define IPMI_KCS_STATUS_ABORTED_BY_CMD_STR \
"Aborted by command (Transfer in progress was " \
"aborted by SMS issuing the Abort/Status control code)"

#define IPMI_KCS_STATUS_ILLEGAL_CTRL_CODE      0x02
#define IPMI_KCS_STATUS_ILLEGAL_CTRL_CODE_STR \
"Illegal control code"

#define IPMI_KCS_STATUS_LEN_ERROR              0x06
#define IPMI_KCS_STATUS_LEN_ERROR_STR \
"Length error (e.g.overrun)"

#define IPMI_KCS_STATUS_OEM_ERROR_BEGIN        0xC0
#define IPMI_KCS_STATUS_OEM_ERROR_END          0xFE

#define IPMI_KCS_STATUS_UNSPECIFIED_ERROR      0xFF
#define IPMI_KCS_STATUS_UNSPECIFIED_ERROR_STR \
"Unspecified error"

/* Reserved - all others */

extern fiid_template_t tmpl_hdr_kcs;

#define IPMI_KCS_CTX_ERR_SUCCESS         0
#define IPMI_KCS_CTX_ERR_NULL            1
#define IPMI_KCS_CTX_ERR_INVALID         2
#define IPMI_KCS_CTX_ERR_PARAMETERS      3
#define IPMI_KCS_CTX_ERR_PERMISSION      4
#define IPMI_KCS_CTX_ERR_IO_PARAMETERS   5
#define IPMI_KCS_CTX_ERR_IO_INIT         6
#define IPMI_KCS_CTX_ERR_OVERFLOW        7
#define IPMI_KCS_CTX_ERR_BUSY            8
#define IPMI_KCS_CTX_ERR_OUTMEM          9
#define IPMI_KCS_CTX_ERR_INTERNAL        10
#define IPMI_KCS_CTX_ERR_ERRNUMRANGE     11

#define IPMI_KCS_MODE_BLOCKING    0
#define IPMI_KCS_MODE_NONBLOCKING 1
#define IPMI_KCS_MODE_DEFAULT     IPMI_KCS_MODE_BLOCKING

typedef struct ipmi_kcs_ctx *ipmi_kcs_ctx_t;

ipmi_kcs_ctx_t ipmi_kcs_ctx_create(void);
int8_t ipmi_kcs_ctx_destroy(ipmi_kcs_ctx_t ctx);
int32_t ipmi_kcs_ctx_errnum(ipmi_kcs_ctx_t ctx);
char *ipmi_kcs_ctx_strerror(int32_t errnum);

int8_t ipmi_kcs_ctx_get_bmc_iobase_addr(ipmi_kcs_ctx_t ctx, uint16_t *bmc_iobase_addr);
int8_t ipmi_kcs_ctx_get_register_space(ipmi_kcs_ctx_t ctx, uint8_t *reg_space);
int8_t ipmi_kcs_ctx_get_poll_interval(ipmi_kcs_ctx_t ctx, uint8_t *poll_interval);
int8_t ipmi_kcs_ctx_get_mode(ipmi_kcs_ctx_t ctx, uint8_t *mode);

int8_t ipmi_kcs_ctx_set_bmc_iobase_addr(ipmi_kcs_ctx_t ctx, uint16_t bmc_iobase_addr);
int8_t ipmi_kcs_ctx_set_register_space(ipmi_kcs_ctx_t ctx, uint8_t reg_space);
int8_t ipmi_kcs_ctx_set_poll_interval(ipmi_kcs_ctx_t ctx, uint8_t poll_interval);
int8_t ipmi_kcs_ctx_set_mode(ipmi_kcs_ctx_t ctx, uint8_t mode);

int8_t ipmi_kcs_ctx_io_init(ipmi_kcs_ctx_t ctx);

int32_t ipmi_kcs_write (ipmi_kcs_ctx_t ctx,
                        uint8_t *bytes,
                        uint32_t  bytes_len);
  
int32_t ipmi_kcs_read (ipmi_kcs_ctx_t ctx,
                       uint8_t* bytes,
                       uint32_t bytes_len);

int8_t fill_hdr_ipmi_kcs (uint8_t lun, 
			  uint8_t fn, 
			  fiid_obj_t obj_hdr);
int32_t assemble_ipmi_kcs_pkt (fiid_obj_t obj_hdr, 
			       fiid_obj_t obj_cmd, 
			       fiid_template_t tmpl_cmd, 
			       uint8_t *pkt, 
			       uint32_t pkt_len);
int32_t unassemble_ipmi_kcs_pkt (uint8_t *pkt, 
				 uint32_t pkt_len, 
				 fiid_obj_t obj_hdr, 
				 fiid_obj_t obj_cmd, 
				 fiid_template_t tmpl_cmd);

#ifdef __cplusplus
}
#endif

#endif /* ipmi-kcs-interface.h */

