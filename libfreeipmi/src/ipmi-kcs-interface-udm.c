/* 
   ipmi-kcs-interface-udm.c: IPMI UDM - Keyboard Controller Style - SMS Interface

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

#include "freeipmi.h"

int8_t 
ipmi_kcs_cmd2 (ipmi_device_t *dev, 
	       fiid_obj_t obj_cmd_rq, 
	       fiid_template_t tmpl_cmd_rq, 
	       fiid_obj_t obj_cmd_rs, 
	       fiid_template_t tmpl_cmd_rs)
{
  if (!(dev && tmpl_cmd_rq && obj_cmd_rq && tmpl_cmd_rs && obj_cmd_rs))
    {
      errno = EINVAL;
      return (-1);
    }
  
  { 
    uint8_t *pkt;
    uint32_t pkt_len;
    
    pkt_len = fiid_obj_len_bytes (*(dev->io.inband.rq.tmpl_hdr_ptr)) + 
      fiid_obj_len_bytes (tmpl_cmd_rq);
    pkt = alloca (pkt_len);
    memset (pkt, 0, pkt_len);
    ERR (pkt);
    
    ERR (fill_hdr_ipmi_kcs (dev->lun, 
			    dev->net_fn, 
			    dev->io.inband.rq.obj_hdr) == 0);
    ERR (assemble_ipmi_kcs_pkt (dev->io.inband.rq.obj_hdr, 
				obj_cmd_rq, 
				tmpl_cmd_rq, 
				pkt, 
				pkt_len) > 0);
    ERR (ipmi_kcs_write (dev->io.inband.kcs_ctx, pkt, pkt_len) != -1);
  }
  
  { 
    uint8_t *pkt;
    uint32_t pkt_len;
    int32_t read_len;

    pkt_len = fiid_obj_len_bytes (*(dev->io.inband.rs.tmpl_hdr_ptr)) + 
      fiid_obj_len_bytes (tmpl_cmd_rs);
    pkt = alloca (pkt_len);
    memset (pkt, 0, pkt_len);
    ERR (pkt);
    
    ERR (fill_hdr_ipmi_kcs (dev->lun, 
			    dev->net_fn, 
			    dev->io.inband.rs.obj_hdr) == 0);
    ERR ((read_len = ipmi_kcs_read (dev->io.inband.kcs_ctx, pkt, pkt_len)) != -1);
    ERR (unassemble_ipmi_kcs_pkt (pkt, 
                                  read_len,
				  dev->io.inband.rs.obj_hdr, 
				  obj_cmd_rs, 
				  tmpl_cmd_rs) != -1);
  }
  
  return (0);
}

int8_t
ipmi_kcs_cmd_raw2 (ipmi_device_t *dev, 
		   uint8_t *buf_rq, 
		   size_t buf_rq_len, 
		   uint8_t *buf_rs, 
		   size_t *buf_rs_len)
{
  if (!(dev && buf_rq && buf_rq_len > 0 
        && buf_rs && buf_rs_len && *buf_rs_len > 0))
    {
      errno = EINVAL;
      return (-1);
    }
  
  { 
    /* Request Block */
    ERR (ipmi_kcs_write (dev->io.inband.kcs_ctx, buf_rq, buf_rq_len) != -1);
  }
  
  { 
    /* Response Block */
    uint32_t bytes_read = 0;
    
    ERR ((bytes_read = ipmi_kcs_read (dev->io.inband.kcs_ctx, buf_rs, *buf_rs_len)) != -1);
    *buf_rs_len = bytes_read;
  }
  
  return (0);
}



