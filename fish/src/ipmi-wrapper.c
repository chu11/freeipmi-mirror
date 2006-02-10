/* 
   ipmi_wrapper.c: higher level wrapper to libfreeipmi functions
   
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

#include "common.h"

static ipmi_device_t dev;
static int dev_opened = false;

static sel_descriptor_t seld;

uint8_t channel_info_list_initialized = false;
channel_info channel_info_list[8];

/* achu: caching to make bmc-config work more quickly */
static uint8_t lan_channel_number_initialized = false;
static int8_t lan_channel_number;
static uint8_t serial_channel_number_initialized = false;
static int8_t serial_channel_number;

static unsigned int rmcp_msg_tag = 0;

ipmi_device_t *
fi_get_ipmi_device ()
{
  return &dev;
}

sel_descriptor_t *
fi_get_seld ()
{
  return &seld;
}

int 
fi_ipmi_open (struct arguments *args)
{
  if (dev_opened)
    return 0;
  
  if (args->common.host != NULL || 
      args->common.driver_type == IPMI_DEVICE_LAN)
    {
      struct hostent *hostinfo;
      struct sockaddr_in host;
      
      host.sin_family = AF_INET;
      host.sin_port = htons (RMCP_AUX_BUS_SHUNT);
      hostinfo = gethostbyname (args->common.host);
      if (hostinfo == NULL)
	{
	  perror ("gethostbyname()");
	  return (-1);
	}
      host.sin_addr = *(struct in_addr *) hostinfo->h_addr;
      
      memset (&dev, 0, sizeof (ipmi_device_t));
      if (ipmi_open_outofband (&dev, 
			       IPMI_DEVICE_LAN, 
			       IPMI_MODE_DEFAULT, 
			       (struct sockaddr *) &host, 
			       sizeof (struct sockaddr), 
			       args->common.auth_type, 
			       args->common.username, 
			       args->common.password, 
			       args->common.priv_level) != 0)
	{
	  perror ("ipmi_open_outofband()");
	  return (-1);
	}
    }
  else 
    {
      if (!ipmi_is_root ())
	{
	  fprintf (stderr, 
		   "Warning: You are NOT root; "
		   "inband access may NOT work\n");
	}
      memset (&dev, 0, sizeof (ipmi_device_t));
      if (args->common.driver_type == IPMI_DEVICE_UNKNOWN)
	{
	  if (ipmi_open_inband (&dev, 
				args->common.disable_auto_probe, 
				IPMI_DEVICE_KCS, 
				args->common.driver_address,
				0,
				args->common.driver_device, 
				IPMI_MODE_DEFAULT) != 0)
	    {
	      if (ipmi_open_inband (&dev, 
				    args->common.disable_auto_probe, 
				    IPMI_DEVICE_SSIF, 
				    args->common.driver_address, 
				    0,
				    args->common.driver_device, 
				    IPMI_MODE_DEFAULT) != 0)
		{
		  perror ("ipmi_open_inband()");
		  return (-1);
		}
	    }
	}
      else 
	{
	  if (ipmi_open_inband (&dev, 
				args->common.disable_auto_probe, 
				args->common.driver_type, 
				args->common.driver_address, 
				0,
				args->common.driver_device, 
				IPMI_MODE_DEFAULT) != 0)
	    {
	      perror ("ipmi_open_inband()");
	      return (-1);
	    }
	}
    }
  
  dev_opened = true;
  
  return 0;
}

int 
fi_ipmi_close ()
{
  if (!dev_opened)
    return 0;
  
  if (ipmi_close (&dev) != 0)
    {
      perror ("ipmi_close()");
      return (-1);
    }
  
  dev_opened = false;
  
  return 0;
}

static char *
get_ipmi_host_ip_address ()
{
  struct arguments *args = NULL;
  
  args = fi_get_arguments ();
  if (args->common.host != NULL) /* OUT-OF-BAND */
    {
      struct hostent *hostinfo = NULL;
      struct in_addr *in_addr = NULL;
      
      hostinfo = gethostbyname (args->common.host);
      if (hostinfo == NULL)
	return NULL;
      
      in_addr = (struct in_addr *) hostinfo->h_addr_list[0];
      
      return strdup (inet_ntoa (*in_addr));
    }
  else /* IN-BAND */
    {
      return strdup ("127.0.0.1");
    }
}

char *
get_sdr_cache_filename ()
{
  char *cache_filename = NULL;
  char *ipmi_host_ip_address = NULL;
  
  ipmi_host_ip_address = get_ipmi_host_ip_address ();
  
  asprintf (&cache_filename, 
	    "%s/%s/%s/%s.%s", 
	    get_home_directory (), 
	    FI_CONFIG_DIRECTORY, 
	    FI_SDR_CACHE_DIR, 
	    FI_SDR_CACHE_FILENAME_PREFIX, 
	    ipmi_host_ip_address);
  
  free (ipmi_host_ip_address);
  
  return cache_filename;
}

channel_info *
get_channel_info_list ()
{
  uint8_t *data_rs = NULL; 
  uint8_t i;
  uint8_t ci;
  uint64_t val;
  
  if (channel_info_list_initialized)
    return (channel_info_list);
  
  fiid_obj_alloca (data_rs, tmpl_get_channel_info_rs);
  for (i = 0, ci = 0; i < 8; i++)
    {
      if (ipmi_cmd_get_channel_info2 (fi_get_ipmi_device (), 
				      i, 
				      data_rs) != 0)
	continue;
      
      fiid_obj_get (data_rs, 
		    tmpl_get_channel_info_rs, 
		    (uint8_t *)"actual_channel_number", 
		    &val);
      channel_info_list[ci].channel_number = (uint8_t) val;
      
      fiid_obj_get (data_rs, 
		    tmpl_get_channel_info_rs, 
		    (uint8_t *)"channel_medium_type", 
		    &val);
      channel_info_list[ci].medium_type = 
	channel_info_list[ci].actual_medium_type = (uint8_t) val;
      
      fiid_obj_get (data_rs, 
		    tmpl_get_channel_info_rs, 
		    (uint8_t *)"channel_protocol_type", 
		    &val);
      channel_info_list[ci].protocol_type = 
	channel_info_list[ci].actual_protocol_type = (uint8_t) val;
      
      if (channel_info_list[ci].actual_medium_type >= 0x0D && 
	  channel_info_list[ci].actual_medium_type <= 0x5F)
	channel_info_list[ci].medium_type = IPMI_CHANNEL_MEDIUM_TYPE_RESERVED;
      
      if (channel_info_list[ci].actual_medium_type >= 0x60 && 
	  channel_info_list[ci].actual_medium_type <= 0x7F)
	channel_info_list[ci].medium_type = IPMI_CHANNEL_MEDIUM_TYPE_OEM;
      
      if (channel_info_list[ci].actual_protocol_type == 0x03 || 
	  (channel_info_list[ci].actual_protocol_type >= 0x0A && 
	   channel_info_list[ci].actual_protocol_type <= 0x1B))
	channel_info_list[ci].protocol_type = IPMI_CHANNEL_PROTOCOL_TYPE_RESERVED;
      
      if (channel_info_list[ci].actual_protocol_type >= 0x1C && 
	  channel_info_list[ci].actual_protocol_type <= 0x1F)
	channel_info_list[ci].protocol_type = IPMI_CHANNEL_PROTOCOL_TYPE_OEM;
      
      ci++;
    }
  
  channel_info_list_initialized = true;
  
  return (channel_info_list);
}

int8_t 
get_lan_channel_number ()
{
  if (lan_channel_number_initialized)
    return lan_channel_number;
  
  lan_channel_number = ipmi_get_channel_number2 (fi_get_ipmi_device (), 
						 IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3);
  if (!(lan_channel_number < 0))
    lan_channel_number_initialized = true;
  return lan_channel_number;
}

int8_t 
get_serial_channel_number ()
{
  if (serial_channel_number_initialized)
    return serial_channel_number;
  
  serial_channel_number = ipmi_get_channel_number2 (fi_get_ipmi_device (), 
						    IPMI_CHANNEL_MEDIUM_TYPE_RS232);
  if (!(serial_channel_number < 0))
    serial_channel_number_initialized = true;
  return serial_channel_number;
}

uint8_t 
get_lan_channel_number_known ()
{
  return 7;
}

uint8_t 
get_serial_channel_number_known ()
{
  return 1;
}

static int 
display_channel_info ()
{
  channel_info *channel_list;
  uint8_t i;
  
  channel_list = get_channel_info_list ();
  
  printf ("Channel Information:\n");
  for (i = 0; i < 8; i++)
    {
      if (channel_list[i].medium_type == IPMI_CHANNEL_MEDIUM_TYPE_RESERVED)
	continue;
      
      printf ("       Channel No: %d\n", channel_list[i].channel_number);
      
      switch (channel_list[i].medium_type)
	{
	case IPMI_CHANNEL_MEDIUM_TYPE_RESERVED:
	  printf ("      Medium Type: %s\n", "Reserved");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_IPMB:
	  printf ("      Medium Type: %s\n", "IPMB (I2C)");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_ICMB_10:
	  printf ("      Medium Type: %s\n", "ICMB v1.0");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_ICMB_09:
	  printf ("      Medium Type: %s\n", "ICMB v0.9");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3:
	  printf ("      Medium Type: %s\n", "802.3 LAN");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_RS232:
	  printf ("      Medium Type: %s\n", "Asynch. Serial/Modem (RS-232)");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_OTHER_LAN:
	  printf ("      Medium Type: %s\n", "Other LAN");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_PCI_SMBUS:
	  printf ("      Medium Type: %s\n", "PCI SMBus");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_SMBUS_10_11:
	  printf ("      Medium Type: %s\n", "SMBus v1.0/1.1");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_SMBUS_20:
	  printf ("      Medium Type: %s\n", "SMBus v2.0");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_USB_1X:
	  printf ("      Medium Type: %s\n", "USB 1.x");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_USB_2X:
	  printf ("      Medium Type: %s\n", "USB 2.x");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_SYS_IFACE:
	  printf ("      Medium Type: %s\n", "System Interface (KCS, SMIC, or BT)");
	  break;
	case IPMI_CHANNEL_MEDIUM_TYPE_OEM:
	  printf ("      Medium Type: %s\n", "OEM");
	  break;
	}
      
      switch (channel_list[i].protocol_type)
	{
	case IPMI_CHANNEL_PROTOCOL_TYPE_RESERVED:
	  printf ("    Protocol Type: %s\n", "Reserved");
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_IPMB:
	  printf ("    Protocol Type: %s\n", "IPMB-1.0");
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_ICMB_10:
	  printf ("    Protocol Type: %s\n", "ICMB-1.0");
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_SMBUS_1X_2X:
	  printf ("    Protocol Type: %s\n", "IPMI-SMBus");
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_KCS:
	  printf ("    Protocol Type: %s\n", "KCS");
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_SMIC:
	  printf ("    Protocol Type: %s\n", "SMIC");
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_BT_10:
	  printf ("    Protocol Type: %s\n", "BT-10");
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_BT_15:
	  printf ("    Protocol Type: %s\n", "BT-15");
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_TMODE:
	  printf ("    Protocol Type: %s\n", "TMODE");
	  break;
	case IPMI_CHANNEL_PROTOCOL_TYPE_OEM:
	  printf ("    Protocol Type: %s\n", "OEM");
	  break;
	}
    }
  
  return 0;
}

int 
display_get_dev_id ()
{
  fiid_obj_t cmd_rs = NULL;
  uint64_t val = 0;
  
  fiid_obj_alloca (cmd_rs, tmpl_cmd_get_dev_id_rs);
  if (ipmi_cmd_get_dev_id (fi_get_ipmi_device (), cmd_rs) != 0)
    {
      ipmi_error (cmd_rs, "ipmi_cmd_get_dev_id()");
      return (-1);
    }
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"dev_id", 
		&val);
  fprintf (stdout, "Device ID:         %X\n", (unsigned int) val);
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"dev_rev.rev", 
		&val);
  fprintf (stdout, "Device Revision:   %d\n", (unsigned int) val);
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"dev_rev.sdr_support", 
		&val);
  if (val)
    fprintf (stdout, "                   [SDR Support]\n");
  
  {
    uint64_t maj, min;
    FIID_OBJ_GET (cmd_rs, 
		  tmpl_cmd_get_dev_id_rs, 
		  (uint8_t *)"firmware_rev1.major_rev", 
		  &maj);
    FIID_OBJ_GET (cmd_rs, 
		  tmpl_cmd_get_dev_id_rs, 
		  (uint8_t *)"firmware_rev2.minor_rev", 
		  &min);
    fprintf (stdout, "Firmware Revision: %d.%d\n", 
	     (unsigned int) maj, (unsigned int) min);
  }
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"firmware_rev1.dev_available", 
		&val);
  if (val == 0)
    fprintf (stdout, 
	     "                   [Device Available (normal operation)]\n");
  else
    {
      fprintf (stdout, 
	       "                   [Device Not Available]\n");
      fprintf (stdout, 
	       "                   [firmware, SDR update or self init in progress]\n");
    }
  
  {
    uint64_t ms, ls;
    FIID_OBJ_GET (cmd_rs, 
		  tmpl_cmd_get_dev_id_rs, 
		  (uint8_t *)"ipmi_ver.ms_bits", 
		  &ms);
    FIID_OBJ_GET (cmd_rs, 
		  tmpl_cmd_get_dev_id_rs, 
		  (uint8_t *)"ipmi_ver.ls_bits", 
		  &ls);
    fprintf (stdout, 
	     "IPMI Version:      %d.%d\n", (unsigned int) ms, (unsigned int) ls);
  }
  
  fprintf (stdout, "Additional Device Support:\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"additional_dev_support.sensor_dev", 
		&val);
  if(val)
    fprintf (stdout, "                   [Sensor Device]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"additional_dev_support.sdr_repo_dev", 
		&val);
  if(val)
    fprintf (stdout, "                   [SDR Repository Device]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"additional_dev_support.sel_dev", 
		&val);
  if(val)
    fprintf (stdout, "                   [SEL Device]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"additional_dev_support.fru_inventory_dev", 
		&val);
  if(val)
    fprintf (stdout, "                   [FRU Inventory Device]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"additional_dev_support.ipmb_evnt_receiver", 
		&val);
  if(val)
    fprintf (stdout, "                   [IPMB Event Receiver]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"additional_dev_support.ipmb_evnt_generator", 
		&val);
  if(val)
    fprintf (stdout, "                   [IPMB Event Generator]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"additional_dev_support.bridge", 
		&val);
  if(val)
    fprintf (stdout, "                   [Bridge]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		(uint8_t *)"additional_dev_support.chassis_dev", 
		&val);
  if(val)
    fprintf (stdout, "                   [Chassis Device]\n");
  
  {
    uint64_t manf_id, prod_id;
    
    FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, (uint8_t *)"manf_id.id", &manf_id);
    fprintf (stdout, "Manufacturer ID:   %Xh\n", (unsigned int) manf_id);
    
    FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, (uint8_t *)"prod_id", &prod_id);
    fprintf (stdout, "Product ID:        %Xh\n", (unsigned int) prod_id);
    
    FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, (uint8_t *)"aux_firmware_rev_info", &val);
    switch (manf_id)
      {
      case IPMI_MANF_ID_INTEL: 
	switch (prod_id)
	  {
	    /* I am assuming all Intel products will decode alike.
                                 -- Anand Babu <ab@gnu.org.in>  */
	  case IPMI_PROD_ID_SR870BN4:
	  default:
	    {
	      uint64_t bc_maj, bc_min, pia_maj, pia_min;
	      FIID_OBJ_GET (cmd_rs,
			    tmpl_cmd_get_dev_id_sr870bn4_rs, 
			    (uint8_t *)"aux_firmware_rev_info.boot_code.major",
			    &bc_maj);
	      FIID_OBJ_GET (cmd_rs,
			    tmpl_cmd_get_dev_id_sr870bn4_rs, 
			    (uint8_t *)"aux_firmware_rev_info.boot_code.minor",
			    &bc_min);
	      FIID_OBJ_GET (cmd_rs,
			    tmpl_cmd_get_dev_id_sr870bn4_rs, 
			    (uint8_t *)"aux_firmware_rev_info.pia.major",
			    &pia_maj);
	      FIID_OBJ_GET (cmd_rs,
			    tmpl_cmd_get_dev_id_sr870bn4_rs, 
			    (uint8_t *)"aux_firmware_rev_info.pia.minor",
			    &pia_min);
	      fprintf (stdout, 
		       "Aux Firmware Revision Info: Boot Code v%02x.%2x, PIA v%02x.%2x\n",
		       (unsigned int) bc_maj, (unsigned int) bc_min, 
		       (unsigned int) pia_maj, (unsigned int) pia_min);
	      break;
	    }
	  }
	break;
      default:
	fprintf (stdout, "Aux Firmware Revision Info: %Xh\n", (unsigned int) val);
      }
  }
  
  return (display_channel_info ());
}

static unsigned int 
_get_rmcp_msg_tag (void)
{
  if (rmcp_msg_tag == 0xFE)
    rmcp_msg_tag = 0; /* roll over */
  
  return (rmcp_msg_tag++);
}

int8_t
ipmi_rmcp_ping (int sockfd, struct sockaddr *hostaddr, unsigned long hostaddr_len, uint32_t msg_tag, fiid_obj_t pong)
{
  int status = 0;
  fiid_obj_t obj_hdr = NULL;
  fiid_obj_t obj_cmd = NULL;
  uint8_t *pkt = NULL;;

  if (!(sockfd && hostaddr && pong))
    {
      errno = EINVAL;
      return -1;
    }
  
  {/* asf_presence_ping request */
    uint32_t pkt_len = 0;

    obj_hdr = alloca (fiid_obj_len_bytes (tmpl_hdr_rmcp));
    if (!obj_hdr)
      return -1;
    memset (obj_hdr, 0, fiid_obj_len_bytes (tmpl_hdr_rmcp));

    obj_cmd = alloca (fiid_obj_len_bytes (tmpl_cmd_asf_presence_ping));
    if (!obj_cmd)
      return -1;
    memset (obj_cmd, 0, fiid_obj_len_bytes (tmpl_cmd_asf_presence_ping));

    pkt_len = fiid_obj_len_bytes (tmpl_hdr_rmcp) + 
      fiid_obj_len_bytes (tmpl_cmd_asf_presence_ping);
    pkt = alloca (pkt_len);
    if (!pkt)
      return -1;
    memset (pkt, 0, pkt_len);

    if (fill_hdr_rmcp_asf (obj_hdr) < 0)
      return -1;
    if (fill_cmd_asf_presence_ping (msg_tag, obj_cmd) < 0)
      return -1;
    if (assemble_rmcp_pkt (obj_hdr, obj_cmd, 
                           tmpl_cmd_asf_presence_ping, pkt, pkt_len) < 0)
      return -1;
    status = ipmi_lan_sendto (sockfd, pkt, pkt_len, 0, hostaddr, hostaddr_len);
    if (status < 0);
      return -1;
  }

  {/* asf_presence_ping response */ 
    struct sockaddr_in from, *hostaddr_in;
    socklen_t fromlen;
    uint32_t  pkt_len;

    pkt_len = fiid_obj_len_bytes (tmpl_hdr_rmcp) + 
      fiid_obj_len_bytes (tmpl_cmd_asf_presence_pong);
    pkt = alloca (pkt_len);
    if (!pkt)
      return -1;
    memset (pkt, 0, pkt_len);

    status = ipmi_lan_recvfrom (sockfd, pkt, pkt_len, 0, (struct sockaddr *)&from, &fromlen);
    if (status < 0)
      return -1;

    /* FIXME: <ab@gnu.org.in>
       We need to verify 
       - IANA Enterprise Number (4542 = ASF IANA)
       - Msg Type (40h = Presence Pong)
       - Msg TAG  (from Ping Request)
    */

    hostaddr_in = (struct sockaddr_in *) hostaddr;
    if ((from.sin_family == AF_INET) && 
	(from.sin_addr.s_addr != hostaddr_in->sin_addr.s_addr))
      {
#if 0
	printf ("ipmi_ping warning: Reply came from [%s] instead of [%s]." 
		"Please tune your time-out value to something higher\n", 
		inet_ntoa (from.sin_addr), inet_ntoa (hostaddr->sin_addr));
#endif
	errno = EBADMSG;
	return (-1);
      }
    if (unassemble_rmcp_pkt (pkt, pkt_len, 
                             tmpl_cmd_asf_presence_pong, NULL, pong) < 0)
      return -1;
    }
  return (0);
}

int 
ipmi_ping (char *host, unsigned int sock_timeout)
{
  int sockfd;
  int status;
  struct sockaddr_in to_addr;
  struct hostent *hostinfo;
  
  if (host == NULL)
    return (-1);
  
  hostinfo = gethostbyname (host);
  if (hostinfo == NULL)
    {
      perror ("gethostbyname()");
      return (-1);
    }
  
  sockfd = ipmi_open_free_udp_port ();
  if (sockfd == -1)
    {
      perror ("ipmi_open_free_udp_port()");
      return (-1);
    }
  
  {
    struct timeval time;
    
    time.tv_sec  = sock_timeout / 1000;
    time.tv_usec = (sock_timeout % 1000) * 1000;
    
    if (setsockopt (sockfd, SOL_SOCKET, SO_RCVTIMEO, 
		    &time, sizeof (time)) < 0)
      {
	perror ("setsockopt()");
	close (sockfd);
	return (-1);
      }
  }
  
  to_addr.sin_family = AF_INET;
  to_addr.sin_port   = htons (RMCP_PRI_RMCP_PORT);
  to_addr.sin_addr   = *(struct in_addr *) hostinfo->h_addr;
  
  {
    fiid_obj_t pong = NULL;
    
    fiid_obj_alloca (pong, tmpl_cmd_asf_presence_pong);
    status = ipmi_rmcp_ping (sockfd, 
			     (struct sockaddr *) &to_addr, 
			     sizeof (struct sockaddr_in), 
			     _get_rmcp_msg_tag (), 
			     pong);
  }
  
  close (sockfd);
  
  return (status);
}

