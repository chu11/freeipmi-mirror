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

static ipmi_device_t dev = NULL;
static int dev_opened = false;

uint8_t channel_info_list_initialized = false;
channel_info channel_info_list[8];

/* achu: caching to make bmc-config work more quickly */
static uint8_t lan_channel_number_initialized = false;
static int8_t lan_channel_number;
static uint8_t serial_channel_number_initialized = false;
static int8_t serial_channel_number;
static uint8_t sol_channel_number_initialized = false;
static int8_t sol_channel_number;

static unsigned int rmcp_message_tag = 0;

ipmi_device_t
fi_get_ipmi_device ()
{
  return dev;
}

int 
fi_ipmi_open (struct arguments *args)
{
  uint32_t flags = 0;

  if (dev_opened)
    return 0;

#ifndef NDEBUG
  if (args->common.debug)
    flags = IPMI_FLAGS_DEBUG_DUMP;
  else
    flags = IPMI_FLAGS_DEFAULT;
#else  /* NDEBUG */
  flags = IPMI_FLAGS_DEFAULT;
#endif /* NDEBUG */
  
  if (args->common.host != NULL || 
      args->common.driver_type == IPMI_DEVICE_LAN)
    {
      if (!(dev = ipmi_open_outofband (IPMI_DEVICE_LAN, 
				       args->common.host,
                                       args->common.username, 
                                       args->common.password, 
                                       args->common.authentication_type, 
                                       args->common.privilege_level,
                                       args->common.session_timeout, 
                                       args->common.retry_timeout, 
                                       flags)))
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
      
      if (args->common.driver_type == IPMI_DEVICE_UNKNOWN)
	{
	  if (!(dev = ipmi_open_inband (IPMI_DEVICE_OPENIPMI, 
					args->common.disable_auto_probe, 
                                        args->common.driver_address, 
                                        args->common.register_spacing,
                                        args->common.driver_device, 
                                        flags)))
	    {
	      if (!(dev = ipmi_open_inband (IPMI_DEVICE_KCS, 
					    args->common.disable_auto_probe, 
					    args->common.driver_address,
					    args->common.register_spacing,
					    args->common.driver_device, 
					    flags)))
		{
		  if (!(dev = ipmi_open_inband (IPMI_DEVICE_SSIF, 
						args->common.disable_auto_probe, 
						args->common.driver_address, 
						args->common.register_spacing,
						args->common.driver_device, 
						flags)))
		    {
		      perror ("ipmi_open_inband()");
		      return (-1);
		    }
		}
	    }
	}
      else 
	{
	  if (!(dev = ipmi_open_inband (args->common.driver_type, 
                                        args->common.disable_auto_probe, 
                                        args->common.driver_address, 
                                        args->common.register_spacing,
                                        args->common.driver_device, 
                                        flags)))
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
  
  ipmi_close_device(dev);
  dev = NULL;
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

int8_t 
get_lan_channel_number ()
{
  if (lan_channel_number_initialized)
    return lan_channel_number;
  
  lan_channel_number = ipmi_get_channel_number (fi_get_ipmi_device (), 
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
  
  serial_channel_number = ipmi_get_channel_number (fi_get_ipmi_device (), 
						   IPMI_CHANNEL_MEDIUM_TYPE_RS232);
  if (!(serial_channel_number < 0))
    serial_channel_number_initialized = true;
  return serial_channel_number;
}

int8_t 
get_sol_channel_number ()
{
  fiid_obj_t obj_cmd_rs = NULL;
  uint64_t val;

  if (sol_channel_number_initialized)
    return sol_channel_number;
  
  if (!(obj_cmd_rs = fiid_obj_create(tmpl_cmd_get_sol_configuration_parameters_sol_payload_channel_rs)))
    goto cleanup;

  if (ipmi_cmd_get_sol_configuration_parameters_sol_payload_channel (fi_get_ipmi_device (),
								     get_lan_channel_number (),
								     IPMI_GET_SOL_PARAMETER,
								     SET_SELECTOR,
								     BLOCK_SELECTOR,
								     obj_cmd_rs) != 0)
    {
      sol_channel_number = get_lan_channel_number ();
      sol_channel_number_initialized = true;
      goto cleanup;
    }
  
  if (fiid_obj_get(obj_cmd_rs,
		   "payload_channel",
		   &val) < 0)
    {
      sol_channel_number = get_lan_channel_number ();
      sol_channel_number_initialized = true;
      goto cleanup;
    }
  sol_channel_number = val;
  sol_channel_number_initialized = true;

 cleanup:
  if (obj_cmd_rs)
    fiid_obj_destroy(obj_cmd_rs);
  return sol_channel_number;
}

static unsigned int 
_get_rmcp_message_tag (void)
{
  if (rmcp_message_tag == 0xFE)
    rmcp_message_tag = 0; /* roll over */
  
  return (rmcp_message_tag++);
}

int8_t
ipmi_rmcp_ping (int sockfd, struct sockaddr *hostaddr, unsigned long hostaddr_len, uint32_t message_tag, fiid_obj_t pong)
{
  int8_t rv;

  if (!(sockfd && hostaddr && fiid_obj_valid(pong)))
    {
      errno = EINVAL;
      return -1;
    }

  if ((rv = fiid_obj_template_compare(pong, tmpl_cmd_asf_presence_pong)) < 0)
    return (-1);

  if (!rv)
    {
      errno = EINVAL;
      return -1;
    }
  
  {/* asf_presence_ping request */
    fiid_obj_t obj_hdr = NULL;
    fiid_obj_t obj_cmd = NULL;
    int32_t hdr_len, cmd_len;
    uint8_t *pkt = NULL;
    uint32_t pkt_len = 0;
    int status = 0;
  
    rv = -1;

    if (!(obj_hdr = fiid_obj_create(tmpl_rmcp_hdr)))
      goto cleanup1;

    if (!(obj_cmd = fiid_obj_create(tmpl_cmd_asf_presence_ping)))
      goto cleanup1;

    if ((hdr_len = fiid_template_len_bytes(tmpl_rmcp_hdr)) < 0)
      goto cleanup1;

    if ((cmd_len = fiid_template_len_bytes(tmpl_cmd_asf_presence_ping)) < 0)
      goto cleanup1;
	
    pkt_len = hdr_len + cmd_len;
    pkt = alloca (pkt_len);
    if (!pkt)
      return -1;
    memset (pkt, 0, pkt_len);

    if (fill_rmcp_hdr_asf (obj_hdr) < 0)
      goto cleanup1;

    if (fill_cmd_asf_presence_ping (message_tag, obj_cmd) < 0)
      goto cleanup1;

    if (assemble_rmcp_pkt (obj_hdr, 
			   obj_cmd, 
			   pkt, 
			   pkt_len) < 0)
      goto cleanup1;

    if ((status = ipmi_lan_sendto (sockfd, 
				   pkt,
				   pkt_len,
				   0, 
				   hostaddr, 
				   hostaddr_len)) < 0)
      goto cleanup1;

    rv = 0;
  cleanup1:
    if (obj_hdr)
      fiid_obj_destroy(obj_hdr);
    if (obj_cmd)
      fiid_obj_destroy(obj_cmd);
    if (rv < 0)
      return (rv);
  }

  {/* asf_presence_ping response */ 
    struct sockaddr_in from, *hostaddr_in;
    socklen_t fromlen;
    fiid_obj_t obj_hdr = NULL;
    int32_t hdr_len, cmd_len;
    uint8_t *pkt = NULL;
    uint32_t pkt_len = 0;
    int32_t recv_len;
    uint64_t val;

    rv = -1;

    if (!(obj_hdr = fiid_obj_create(tmpl_rmcp_hdr)))
      goto cleanup2;

    if ((hdr_len = fiid_template_len_bytes(tmpl_rmcp_hdr)) < 0)
      goto cleanup2;

    if ((cmd_len = fiid_template_len_bytes(tmpl_cmd_asf_presence_pong)) < 0)
      goto cleanup2;

    pkt_len = hdr_len + cmd_len;
    pkt = alloca (pkt_len);
    if (!pkt)
      return -1;
    memset (pkt, 0, pkt_len);

    if ((recv_len = ipmi_lan_recvfrom (sockfd,
				       pkt,
				       pkt_len,
				       0, 
				       (struct sockaddr *)&from, 
				       &fromlen)) < 0)
      goto cleanup2;

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
	goto cleanup2;
      }
    
    if (unassemble_rmcp_pkt (pkt, 
			     recv_len,
			     obj_hdr, 
			     pong) < 0)
      goto cleanup2;

    if (fiid_obj_get(pong, "msg_type", &val) < 0)
      goto cleanup2;

    if (val != RMCP_ASF_MESSAGE_TYPE_PRESENCE_PONG)
      {
	errno = EBADMSG;
	goto cleanup2;
      }

    if (fiid_obj_get(pong, "message_tag", &val) < 0)
      goto cleanup2;

    if (val != message_tag)
      {
	errno = EBADMSG;
	goto cleanup2;
      }
    
    rv = 0;
  cleanup2:
    if (obj_hdr)
      fiid_obj_destroy(obj_hdr);
    if (rv < 0)
      return (rv);
    }

  return (0);
}

int 
ipmi_ping (char *host, unsigned int sock_timeout)
{
  int sockfd;
  int status = -1;
  struct sockaddr_in from_addr;
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
  
  if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
    {
      perror ("socket()");
      return (-1);
    }
  
  memset (&from_addr, 0, sizeof (struct sockaddr_in));
  from_addr.sin_family = AF_INET;
  from_addr.sin_port   = htons (0);
  from_addr.sin_addr.s_addr = htonl (INADDR_ANY);

  if (bind (sockfd, (struct sockaddr *)&from_addr, sizeof(struct sockaddr_in)) < 0)
    {
      perror("bind");
      close (sockfd);
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
  to_addr.sin_port   = htons (RMCP_PRIMARY_RMCP_PORT);
  to_addr.sin_addr   = *(struct in_addr *) hostinfo->h_addr;
  
  {
    fiid_obj_t pong = NULL;
    
    if (!(pong = fiid_obj_create(tmpl_cmd_asf_presence_pong)))
      goto cleanup;

    if ((status = ipmi_rmcp_ping (sockfd, 
                                  (struct sockaddr *) &to_addr, 
                                  sizeof (struct sockaddr_in), 
                                  _get_rmcp_message_tag (), 
                                  pong)) < 0)
      goto cleanup;

  cleanup:
    if (pong)
      fiid_obj_destroy(pong);
  }
  
  close (sockfd);
  
  return (status);
}

