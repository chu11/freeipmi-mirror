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

#define IPMI_KCS_SLEEP_USECS            0x01

#define IPMI_KCS_HDR_LEN                0x01

#define IPMI_KCS_SMS_IO_BASE_DEFAULT    0x0CA2
#define IPMI_KCS_SMS_IO_BASE_CDC1620    0x0CA2
#define IPMI_KCS_SMS_IO_BASE_CDC9416    0x0CA2
#define IPMI_KCS_SMS_IO_BASE_SR870BN4   0x08A2 
#define IPMI_KCS_SMS_IO_BASE_CDC6440    0x08A2

/* IPMI KCS SMS Interface Registers */
#define IPMI_KCS_REG_DATAIN(sms_io_base)   (sms_io_base)
#define IPMI_KCS_REG_DATAOUT(sms_io_base)  (sms_io_base)
#define IPMI_KCS_REG_CMD(sms_io_base, reg_space)     \
       (sms_io_base + reg_space)
#define IPMI_KCS_REG_STATUS(sms_io_base, reg_space)  \
       (sms_io_base + reg_space)


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
/*
#define IPMI_KCS_STATE_IDLE   0x00
#define IPMI_KCS_STATE_READ   0x01
#define IPMI_KCS_STATE_WRITE  0x02
#define IPMI_KCS_STATE_ERROR  0x03
*/
#define IPMI_KCS_STATE_IDLE   0x00
#define IPMI_KCS_STATE_READ   IPMI_KCS_STATUS_REG_S0
#define IPMI_KCS_STATE_WRITE  IPMI_KCS_STATUS_REG_S1
#define IPMI_KCS_STATE_ERROR  (IPMI_KCS_STATUS_REG_S0 & IPMI_KCS_STATUS_REG_S1)

/* IPMI KCS Control Codes */
#define IPMI_KCS_CTRL_GET_STATUS       0x60 /* Request Interface Status / 
                                               Abort Current operation */
#define IPMI_KCS_CTRL_GET_ABORT        IPMI_KCS_CTRL_GET_STATUS
#define IPMI_KCS_CTRL_WRITE_START      0x61 /* Write the First byte of an Write Transfer */
#define IPMI_KCS_CTRL_WRITE_END        0x62 /* Write the Last byte of an Write Transfer */
/* reserved      0x63 - 0x67 */
#define IPMI_KCS_CTRL_READ             0x68 /* Request the next data byte */
/* reserved      0x69 - 0x6F */

extern fiid_template_t tmpl_hdr_kcs;

/* Never call these functions directly, unless you are a FreeIPMI hacker */
int8_t ipmi_kcs_get_status (ipmi_device_t *dev);
void ipmi_kcs_wait_for_ibf_clear (ipmi_device_t *dev);
void ipmi_kcs_wait_for_obf_set (ipmi_device_t *dev);
int8_t ipmi_kcs_read_byte (ipmi_device_t *dev);
void ipmi_kcs_read_next (ipmi_device_t *dev);
void ipmi_kcs_start_write (ipmi_device_t *dev);
void ipmi_kcs_write_byte (ipmi_device_t *dev, uint8_t byte);
void ipmi_kcs_end_write (ipmi_device_t *dev);
void ipmi_kcs_get_abort (ipmi_device_t *dev);
int8_t ipmi_kcs_test_if_state (ipmi_device_t *dev, uint8_t status);
void ipmi_kcs_clear_obf (ipmi_device_t *dev);

/* High level calls */
void ipmi_enable_old_kcs_init (ipmi_device_t *dev);
int ipmi_kcs_io_init (uint16_t sms_io_base, 
		      uint8_t reg_space, 
		      unsigned long sleep_usecs);
int ipmi_kcs_open (uint16_t sms_io_base, 
		   uint8_t reg_space, 
		   unsigned long sleep_usecs);

ssize_t ipmi_kcs_read (ipmi_device_t *dev, 
		       uint8_t *bytes, 
		       uint32_t bytes_len);
ssize_t ipmi_kcs_write (ipmi_device_t *dev, 
			uint8_t *bytes, 
			uint32_t bytes_len);

/* BMC treats "write followed by a read" as one transaction. It is
   highly recommended to use ipmi_kcs_cmd instead. Otherwise make sure
   you check the return status of write before calling read.
*/
int8_t ipmi_kcs_cmd (uint8_t lun, 
		     uint8_t fn, 
		     fiid_obj_t obj_cmd_rq, 
		     fiid_template_t tmpl_cmd_rq, 
		     fiid_obj_t obj_cmd_rs, 
		     fiid_template_t tmpl_cmd_rs);
int8_t ipmi_kcs_cmd2 (ipmi_device_t *dev, 
		      fiid_obj_t obj_cmd_rq, 
		      fiid_template_t tmpl_cmd_rq, 
		      fiid_obj_t obj_cmd_rs, 
		      fiid_template_t tmpl_cmd_rs);
int8_t ipmi_kcs_cmd_interruptible (uint8_t lun, 
				   uint8_t fn, 
				   fiid_obj_t obj_cmd_rq, 
				   fiid_template_t tmpl_cmd_rq, 
				   fiid_obj_t obj_cmd_rs, 
				   fiid_template_t tmpl_cmd_rs);
int8_t ipmi_kcs_cmd_raw (uint8_t lun, 
			 uint8_t fn, 
			 uint8_t *buf_cmd_rq, 
			 size_t buf_rq_len, 
			 uint8_t *buf_cmd_rs, 
			 size_t *buf_rs_len);
int8_t ipmi_kcs_cmd_raw2 (ipmi_device_t *dev, 
			  uint8_t *buf_rq, 
			  size_t buf_rq_len, 
			  uint8_t *buf_rs, 
			  size_t *buf_rs_len);
int8_t ipmi_kcs_cmd_raw_interruptible (uint8_t lun, 
				       uint8_t fn, 
				       uint8_t *buf_cmd_rq, 
				       uint32_t buf_rq_len, 
				       uint8_t *buf_cmd_rs, 
				       uint32_t *buf_rs_len);

int8_t fill_hdr_ipmi_kcs (uint8_t lun, 
			  uint8_t fn, 
			  fiid_obj_t obj_hdr);
int8_t assemble_ipmi_kcs_pkt (fiid_obj_t obj_hdr, 
			      fiid_obj_t obj_cmd, 
			      fiid_template_t tmpl_cmd, 
			      uint8_t *pkt, 
			      uint32_t pkt_len);
int8_t unassemble_ipmi_kcs_pkt (uint8_t *pkt, 
				uint32_t pkt_len, 
				fiid_obj_t obj_hdr, 
				fiid_obj_t obj_cmd, 
				fiid_template_t tmpl_cmd);
int ipmi_kcs_get_mutex_semid (void);


#ifdef __cplusplus
}
#endif

#endif /* ipmi-kcs-interface.h */

