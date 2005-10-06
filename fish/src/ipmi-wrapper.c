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

u_int8_t channel_info_list_initialized = false;
channel_info channel_info_list[8];

/* achu: caching to make bmc-config work more quickly */
static u_int8_t lan_channel_number_initialized = false;
static int8_t lan_channel_number;
static u_int8_t serial_channel_number_initialized = false;
static int8_t serial_channel_number;

static unsigned int rmcp_msg_tag = 0;

ipmi_device_t *
fi_get_ipmi_device ()
{
  return &dev;
}

int 
fi_ipmi_open (struct arguments *args)
{
  if (dev_opened)
    return 0;
  
  if (args->host != NULL)
    {
      struct hostent *hostinfo;
      struct sockaddr_in host;
      
      host.sin_family = AF_INET;
      host.sin_port = htons (RMCP_AUX_BUS_SHUNT);
      hostinfo = gethostbyname (args->host);
      if (hostinfo == NULL)
	{
	  perror ("gethostbyname()");
	  return (-1);
	}
      host.sin_addr = *(struct in_addr *) hostinfo->h_addr;
      
      if (ipmi_open_outofband (&dev, 
			       IPMI_DEVICE_LAN, 
			       IPMI_MODE_DEFAULT, 
			       (struct sockaddr *) &host, 
			       sizeof (struct sockaddr), 
			       args->auth_type, 
			       args->username, 
			       args->password, 
			       args->priv_level) != 0)
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
      if (ipmi_open_inband (&dev, 
			    IPMI_DEVICE_KCS, 
			    IPMI_MODE_DEFAULT) != 0)
	{
	  perror ("ipmi_open_inband()");
	  return (-1);
	}
      ipmi_enable_old_kcs_init (&dev);
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
  /* if IN-BAND */
  return strdup ("127.0.0.1");
  
  /* if OUT-OF-BAND */
  {
    char hostname[] = {"localhost"};
    struct hostent *hostinfo = NULL;
    struct in_addr *in_addr = NULL;
    
    hostinfo = gethostbyname (hostname);
    if (hostinfo == NULL)
      return NULL;
    
    in_addr = (struct in_addr *) hostinfo->h_addr_list[0];
    
    return strdup (inet_ntoa (*in_addr));
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
  u_int8_t *data_rs = NULL; 
  u_int8_t i;
  u_int8_t ci;
  u_int64_t val;
  
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
		    "actual_channel_number", 
		    &val);
      channel_info_list[ci].channel_number = (u_int8_t) val;
      
      fiid_obj_get (data_rs, 
		    tmpl_get_channel_info_rs, 
		    "channel_medium_type", 
		    &val);
      channel_info_list[ci].medium_type = 
	channel_info_list[ci].actual_medium_type = (u_int8_t) val;
      
      fiid_obj_get (data_rs, 
		    tmpl_get_channel_info_rs, 
		    "channel_protocol_type", 
		    &val);
      channel_info_list[ci].protocol_type = 
	channel_info_list[ci].actual_protocol_type = (u_int8_t) val;
      
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

  lan_channel_number = ipmi_get_channel_number (IPMI_CHANNEL_MEDIUM_TYPE_LAN_802_3);
  if (!(lan_channel_number < 0))
    lan_channel_number_initialized = true;
  return lan_channel_number;
}

int8_t 
get_serial_channel_number ()
{
  if (serial_channel_number_initialized)
    return serial_channel_number;

  serial_channel_number = ipmi_get_channel_number (IPMI_CHANNEL_MEDIUM_TYPE_RS232);
  if (!(serial_channel_number < 0))
    serial_channel_number_initialized = true;
  return serial_channel_number;
}

u_int8_t 
get_lan_channel_number_known ()
{
  return 7;
}

u_int8_t 
get_serial_channel_number_known ()
{
  return 1;
}

static int 
display_channel_info ()
{
  channel_info *channel_list;
  u_int8_t i;
  
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
  u_int64_t val = 0;
  
  fiid_obj_alloca (cmd_rs, tmpl_cmd_get_dev_id_rs);
  if (ipmi_cmd_get_dev_id (fi_get_ipmi_device (), cmd_rs) != 0)
    {
      perror ("ipmi_cmd_get_dev_id()");
      return (-1);
    }
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		"dev_id", 
		&val);
  fprintf (stdout, "Device ID:         %X\n", (unsigned int) val);
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		"dev_rev.rev", 
		&val);
  fprintf (stdout, "Device Revision:   %d\n", (unsigned int) val);
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		"dev_rev.sdr_support", 
		&val);
  if (val)
    fprintf (stdout, "                   [SDR Support]\n");
  
  {
    u_int64_t maj, min;
    FIID_OBJ_GET (cmd_rs, 
		  tmpl_cmd_get_dev_id_rs, 
		  "firmware_rev1.major_rev", 
		  &maj);
    FIID_OBJ_GET (cmd_rs, 
		  tmpl_cmd_get_dev_id_rs, 
		  "firmware_rev2.minor_rev", 
		  &min);
    fprintf (stdout, "Firmware Revision: %d.%d\n", 
	     (unsigned int) maj, (unsigned int) min);
  }
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		"firmware_rev1.dev_available", 
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
    u_int64_t ms, ls;
    FIID_OBJ_GET (cmd_rs, 
		  tmpl_cmd_get_dev_id_rs, 
		  "ipmi_ver.ms_bits", 
		  &ms);
    FIID_OBJ_GET (cmd_rs, 
		  tmpl_cmd_get_dev_id_rs, 
		  "ipmi_ver.ls_bits", 
		  &ls);
    fprintf (stdout, 
	     "IPMI Version:      %d.%d\n", (unsigned int) ms, (unsigned int) ls);
  }
  
  fprintf (stdout, "Additional Device Support:\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		"additional_dev_support.sensor_dev", 
		&val);
  if(val)
    fprintf (stdout, "                   [Sensor Device]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		"additional_dev_support.sdr_repo_dev", 
		&val);
  if(val)
    fprintf (stdout, "                   [SDR Repository Device]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		"additional_dev_support.sel_dev", 
		&val);
  if(val)
    fprintf (stdout, "                   [SEL Device]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		"additional_dev_support.fru_inventory_dev", 
		&val);
  if(val)
    fprintf (stdout, "                   [FRU Inventory Device]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		"additional_dev_support.ipmb_evnt_receiver", 
		&val);
  if(val)
    fprintf (stdout, "                   [IPMB Event Receiver]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		"additional_dev_support.ipmb_evnt_generator", 
		&val);
  if(val)
    fprintf (stdout, "                   [IPMB Event Generator]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		"additional_dev_support.bridge", 
		&val);
  if(val)
    fprintf (stdout, "                   [Bridge]\n");
  
  FIID_OBJ_GET (cmd_rs, 
		tmpl_cmd_get_dev_id_rs, 
		"additional_dev_support.chassis_dev", 
		&val);
  if(val)
    fprintf (stdout, "                   [Chassis Device]\n");
  
  {
    u_int64_t manf_id, prod_id;
    
    FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "manf_id.id", &manf_id);
    fprintf (stdout, "Manufacturer ID:   %Xh\n", (unsigned int) manf_id);
    
    FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "prod_id", &prod_id);
    fprintf (stdout, "Product ID:        %Xh\n", (unsigned int) prod_id);
    
    FIID_OBJ_GET (cmd_rs, tmpl_cmd_get_dev_id_rs, "aux_firmware_rev_info", &val);
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
	      u_int64_t bc_maj, bc_min, pia_maj, pia_min;
	      FIID_OBJ_GET (cmd_rs,
			    tmpl_cmd_get_dev_id_sr870bn4_rs, 
			    "aux_firmware_rev_info.boot_code.major",
			    &bc_maj);
	      FIID_OBJ_GET (cmd_rs,
			    tmpl_cmd_get_dev_id_sr870bn4_rs, 
			    "aux_firmware_rev_info.boot_code.minor",
			    &bc_min);
	      FIID_OBJ_GET (cmd_rs,
			    tmpl_cmd_get_dev_id_sr870bn4_rs, 
			    "aux_firmware_rev_info.pia.major",
			    &pia_maj);
	      FIID_OBJ_GET (cmd_rs,
			    tmpl_cmd_get_dev_id_sr870bn4_rs, 
			    "aux_firmware_rev_info.pia.minor",
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
    
    if (errno == EBADMSG)
      warn ("Increase your socket timeout value");
  }
  
  close (sockfd);
  
  return (status);
}

