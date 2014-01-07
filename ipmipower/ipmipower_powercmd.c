/*****************************************************************************\
 *  $Id: ipmipower_powercmd.c,v 1.206 2010-08-03 00:10:59 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2003-2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-155698
 *
 *  This file is part of Ipmipower, a remote power control utility.
 *  For details, see http://www.llnl.gov/linux/.
 *
 *  Ipmipower is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmipower is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmipower.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#if HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#if STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#if HAVE_STRINGS_H
#include <strings.h>
#endif /* HAVE_STRINGS_H */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else  /* !TIME_WITH_SYS_TIME */
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else /* !HAVE_SYS_TIME_H */
#include <time.h>
#endif  /* !HAVE_SYS_TIME_H */
#endif /* !TIME_WITH_SYS_TIME */
#include <netinet/in.h>
#include <assert.h>
#include <errno.h>

#include "ipmipower.h"
#include "ipmipower_connection.h"
#include "ipmipower_error.h"
#include "ipmipower_oem.h"
#include "ipmipower_output.h"
#include "ipmipower_powercmd.h"
#include "ipmipower_packet.h"
#include "ipmipower_check.h"
#include "ipmipower_util.h"

#include "freeipmi-portability.h"
#include "cbuf.h"
#include "list.h"
#include "secure.h"
#include "timeval.h"

extern struct ipmipower_arguments cmd_args;

/* Queue of all pending power commands */
static List pending = NULL;

/* Queue of power commands to be added to the pending, for serializing
 * OEM power control to the same host
 */
static List add_to_pending = NULL;

/* Count of currently executing power commands for fanout */
static unsigned int executing_count = 0;

static int
_find_ipmipower_powercmd (void *x, void *key)
{
  ipmipower_powercmd_t ip;
  char *hostname;

  assert (x);
  assert (key);
  
  ip = (ipmipower_powercmd_t)x;
  hostname = (char *)key;

  return (!strcasecmp (ip->ic->hostname, hostname));
}

static void
_destroy_ipmipower_powercmd (void *x)
{
  ipmipower_powercmd_t ip;

  assert (x);

  ip = (ipmipower_powercmd_t)x;

  fiid_obj_destroy (ip->obj_rmcp_hdr_rq);
  fiid_obj_destroy (ip->obj_rmcp_hdr_rs);
  fiid_obj_destroy (ip->obj_lan_session_hdr_rq);
  fiid_obj_destroy (ip->obj_lan_session_hdr_rs);
  fiid_obj_destroy (ip->obj_lan_msg_hdr_rq);
  fiid_obj_destroy (ip->obj_lan_msg_hdr_rs);
  fiid_obj_destroy (ip->obj_lan_msg_trlr_rs);
  fiid_obj_destroy (ip->obj_rmcpplus_session_hdr_rq);
  fiid_obj_destroy (ip->obj_rmcpplus_session_hdr_rs);
  fiid_obj_destroy (ip->obj_rmcpplus_payload_rs);
  fiid_obj_destroy (ip->obj_rmcpplus_session_trlr_rq);
  fiid_obj_destroy (ip->obj_rmcpplus_session_trlr_rs);
  fiid_obj_destroy (ip->obj_authentication_capabilities_rq);
  fiid_obj_destroy (ip->obj_authentication_capabilities_rs);
  fiid_obj_destroy (ip->obj_get_session_challenge_rq);
  fiid_obj_destroy (ip->obj_get_session_challenge_rs);
  fiid_obj_destroy (ip->obj_activate_session_rq);
  fiid_obj_destroy (ip->obj_activate_session_rs);
  fiid_obj_destroy (ip->obj_open_session_rq);
  fiid_obj_destroy (ip->obj_open_session_rs);
  fiid_obj_destroy (ip->obj_rakp_message_1_rq);
  fiid_obj_destroy (ip->obj_rakp_message_2_rs);
  fiid_obj_destroy (ip->obj_rakp_message_3_rq);
  fiid_obj_destroy (ip->obj_rakp_message_4_rs);
  fiid_obj_destroy (ip->obj_set_session_privilege_level_rq);
  fiid_obj_destroy (ip->obj_set_session_privilege_level_rs);
  fiid_obj_destroy (ip->obj_get_chassis_status_rq);
  fiid_obj_destroy (ip->obj_get_chassis_status_rs);
  fiid_obj_destroy (ip->obj_chassis_control_rq);
  fiid_obj_destroy (ip->obj_chassis_control_rs);
  fiid_obj_destroy (ip->obj_chassis_identify_rq);
  fiid_obj_destroy (ip->obj_chassis_identify_rs);

  if (cmd_args.oem_power_type != IPMIPOWER_OEM_POWER_TYPE_NONE)
    {
      if (cmd_args.oem_power_type == IPMIPOWER_OEM_POWER_TYPE_C410X)
	{
	  fiid_obj_destroy (ip->obj_c410x_get_sensor_reading_rq);
	  fiid_obj_destroy (ip->obj_c410x_get_sensor_reading_rs);
	  fiid_obj_destroy (ip->obj_c410x_slot_power_control_rq);
	  fiid_obj_destroy (ip->obj_c410x_slot_power_control_rs);
	}
    }

  fiid_obj_destroy (ip->obj_close_session_rq);
  fiid_obj_destroy (ip->obj_close_session_rs);

  /* Close all sockets that were saved during the Get Session
   * Challenge phase of the IPMI protocol.
   */
  if (list_count (ip->sockets_to_close) > 0)
    {
      int *fd;
      while ((fd = list_pop (ip->sockets_to_close)))
        {
          /* cleanup path, ignore potential error */
          close (*fd);
          free (fd);
        }
    }
  
  list_destroy (ip->sockets_to_close);

  free (ip->extra_arg);

  /* Any additional queued commands should be moved to add_to_pending
   * before destroy
   */
  assert (!ip->next);

  free (ip);
}

void
ipmipower_powercmd_setup ()
{
  assert (!pending);  /* need to cleanup first! */

  pending = list_create ((ListDelF)_destroy_ipmipower_powercmd);
  if (!pending)
    {
      IPMIPOWER_ERROR (("list_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  add_to_pending = list_create (NULL);
  if (!add_to_pending)
    {
      IPMIPOWER_ERROR (("list_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
}

void
ipmipower_powercmd_cleanup ()
{
  assert (pending);  /* did not run ipmipower_powercmd_setup() */
  list_destroy (pending);
  list_destroy (add_to_pending); 
  pending = NULL;
  add_to_pending = NULL;
}

void
ipmipower_powercmd_queue (ipmipower_power_cmd_t cmd,
			  struct ipmipower_connection *ic,
			  const char *extra_arg)
{
  ipmipower_powercmd_t ip;

  assert (pending);  /* did not run ipmipower_powercmd_setup() */
  assert (ic);
  assert (IPMIPOWER_POWER_CMD_VALID (cmd));

  ipmipower_connection_clear (ic);

  if (!(ip = (ipmipower_powercmd_t)malloc (sizeof (struct ipmipower_powercmd))))
    {
      IPMIPOWER_ERROR (("malloc: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  memset (ip, '\0', sizeof (struct ipmipower_powercmd));

  ip->cmd = cmd;
  ip->protocol_state = IPMIPOWER_PROTOCOL_STATE_START;

  /*
   * Protocol State Machine Variables
   */
#if 0
  /* Initialize when protocol really begins.  Necessary b/c of fanout support */
  if (gettimeofday (&(ip->time_begin), NULL) < 0)
    {
      IPMIPOWER_ERROR (("gettimeofday: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
#endif
  ip->retransmission_count = 0;
  ip->close_timeout = 0;

  /*
   * Protocol Maintenance Variables
   */

  ip->session_inbound_count = 0;

  if (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN)
    {
      if (ipmi_check_session_sequence_number_1_5_init (&(ip->highest_received_sequence_number), 
                                                       &(ip->previously_received_list)) < 0)
        {
          IPMIPOWER_ERROR (("ipmi_check_session_sequence_number_1_5_init: %s", strerror (errno)));
          exit (EXIT_FAILURE);
        }
    }
  else
    {
      if (ipmi_check_session_sequence_number_2_0_init (&(ip->highest_received_sequence_number), 
                                                       &(ip->previously_received_list)) < 0)
        {
          IPMIPOWER_ERROR (("ipmi_check_session_sequence_number_2_0_init: %s", strerror (errno)));
          exit (EXIT_FAILURE);
        }
    }

  /* IPMI 1.5 */
#if 0
  if (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN)
    {
      /* ip->permsgauth_enabled is set after the Get Authentication
       * Capabilities Response and/or Activate Session Response is
       * received
       */
    }
#endif /* 0 */

  /* IPMI 2.0 */

  if (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN_2_0)
    {
      if (ipmi_cipher_suite_id_to_algorithms (cmd_args.common_args.cipher_suite_id,
                                              &(ip->authentication_algorithm),
                                              &(ip->integrity_algorithm),
                                              &(ip->confidentiality_algorithm)) < 0)
        {
          IPMIPOWER_ERROR (("ipmipower_powercmd_queue: ipmi_cipher_suite_id_to_algorithms: ",
                            "cmd_args.common_args.cipher_suite_id: %d: %s",
                            cmd_args.common_args.cipher_suite_id, strerror (errno)));
          exit (EXIT_FAILURE);
        }

      /*
       * IPMI Workaround (achu)
       *
       * Forgotten Motherboard
       *
       * Cipher suite IDs are attached to specific privilege levels
       * rather than a maximum privilege level limit.  So you can only
       * authenticate at the configured privilege level rather than a
       * privilege level <= to it.
       *
       * To deal with this situation.  We send the "request highest
       * privilege" flag in the open session request.  This should be
       * enough to work around this issue but still work with other
       * motherboards.
       */

      /* IPMI Workaround (achu)
       *
       * Discovered on SE7520AF2 with Intel Server Management Module
       * (Professional Edition)
       *
       * The Intel's return IPMI_PRIVILEGE_LEVEL_HIGHEST_LEVEL instead
       * of an actual privilege, so have to pass the actual privilege
       * we want to use.
       */

      /* IPMI Workaround (achu)
       *
       * Discovered on Sun Fire 4100, Inventec 5441/Dell Xanadu II,
       * Supermicro X8DTH, Supermicro X8DTG, Supermicro X8DTU, Intel
       * S5500WBV/Penguin Relion 700
       *
       * The remote BMC incorrectly calculates keys using the privilege
       * specified in the open session stage rather than the privilege
       * used during the RAKP1 stage.  This can be problematic if you
       * specify IPMI_PRIVILEGE_LEVEL_HIGHEST_LEVEL during that stage
       * instead of a real privilege level.  So we must pass the actual
       * privilege we want to use.
       */
      if (cmd_args.common_args.workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION
          || cmd_args.common_args.workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_SUN_2_0_SESSION
          || cmd_args.common_args.workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_OPEN_SESSION_PRIVILEGE)
        ip->requested_maximum_privilege_level = cmd_args.common_args.privilege_level;
      else
        ip->requested_maximum_privilege_level = IPMI_PRIVILEGE_LEVEL_HIGHEST_LEVEL;
      memset (ip->sik_key, '\0', IPMI_MAX_SIK_KEY_LENGTH);
      ip->sik_key_ptr = ip->sik_key;
      ip->sik_key_len = IPMI_MAX_SIK_KEY_LENGTH;
      memset (ip->integrity_key, '\0', IPMI_MAX_INTEGRITY_KEY_LENGTH);
      ip->integrity_key_ptr = ip->integrity_key;
      ip->integrity_key_len = IPMI_MAX_INTEGRITY_KEY_LENGTH;
      memset (ip->confidentiality_key, '\0', IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH);
      ip->confidentiality_key_ptr = ip->confidentiality_key;
      ip->confidentiality_key_len = IPMI_MAX_CONFIDENTIALITY_KEY_LENGTH;

      if (ipmi_get_random (&ip->initial_message_tag,
                           sizeof (ip->initial_message_tag)) < 0)
        {
          IPMIPOWER_ERROR (("ipmi_get_random: %s", strerror (errno)));
          exit (EXIT_FAILURE);
        }

      ip->message_tag_count = 0;
      ip->session_sequence_number = 0;
      ip->name_only_lookup = IPMI_NAME_ONLY_LOOKUP;

      /* In IPMI 2.0, session_ids of 0 are special */
      do
        {
          if (ipmi_get_random (&ip->remote_console_session_id,
                               sizeof (ip->remote_console_session_id)) < 0)
            {
              IPMIPOWER_ERROR (("ipmi_get_random: %s", strerror (errno)));
              exit (EXIT_FAILURE);
            }
        } while (!ip->remote_console_session_id);

      if (ipmi_get_random (ip->remote_console_random_number,
                           IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH) < 0)
        {
          IPMIPOWER_ERROR (("ipmi_get_random: %s", strerror (errno)));
          exit (EXIT_FAILURE);
        }

      ip->wait_until_on_state = 0;
      ip->wait_until_off_state = 0;
    }

  ip->ic = ic;

  if (!(ip->obj_rmcp_hdr_rq = fiid_obj_create (tmpl_rmcp_hdr)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_rmcp_hdr_rs = fiid_obj_create (tmpl_rmcp_hdr)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_lan_session_hdr_rq = fiid_obj_create (tmpl_lan_session_hdr)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_lan_session_hdr_rs = fiid_obj_create (tmpl_lan_session_hdr)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_lan_msg_hdr_rq = fiid_obj_create (tmpl_lan_msg_hdr_rq)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_lan_msg_hdr_rs = fiid_obj_create (tmpl_lan_msg_hdr_rs)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_lan_msg_trlr_rs = fiid_obj_create (tmpl_lan_msg_trlr)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_rmcpplus_session_hdr_rq = fiid_obj_create (tmpl_rmcpplus_session_hdr)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_rmcpplus_session_hdr_rs = fiid_obj_create (tmpl_rmcpplus_session_hdr)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_rmcpplus_payload_rs = fiid_obj_create (tmpl_rmcpplus_payload)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_rmcpplus_session_trlr_rq = fiid_obj_create (tmpl_rmcpplus_session_trlr)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_rmcpplus_session_trlr_rs = fiid_obj_create (tmpl_rmcpplus_session_trlr)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_authentication_capabilities_rq = fiid_obj_create (tmpl_cmd_get_channel_authentication_capabilities_rq)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_authentication_capabilities_rs = fiid_obj_create (tmpl_cmd_get_channel_authentication_capabilities_rs)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_get_session_challenge_rq = fiid_obj_create (tmpl_cmd_get_session_challenge_rq)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_get_session_challenge_rs = fiid_obj_create (tmpl_cmd_get_session_challenge_rs)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_activate_session_rq = fiid_obj_create (tmpl_cmd_activate_session_rq)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_activate_session_rs = fiid_obj_create (tmpl_cmd_activate_session_rs)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_open_session_rq = fiid_obj_create (tmpl_rmcpplus_open_session_request)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_open_session_rs = fiid_obj_create (tmpl_rmcpplus_open_session_response)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_rakp_message_1_rq = fiid_obj_create (tmpl_rmcpplus_rakp_message_1)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_rakp_message_2_rs = fiid_obj_create (tmpl_rmcpplus_rakp_message_2)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_rakp_message_3_rq = fiid_obj_create (tmpl_rmcpplus_rakp_message_3)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_rakp_message_4_rs = fiid_obj_create (tmpl_rmcpplus_rakp_message_4)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_set_session_privilege_level_rq = fiid_obj_create (tmpl_cmd_set_session_privilege_level_rq)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_set_session_privilege_level_rs = fiid_obj_create (tmpl_cmd_set_session_privilege_level_rs)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_get_chassis_status_rq = fiid_obj_create (tmpl_cmd_get_chassis_status_rq)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_get_chassis_status_rs = fiid_obj_create (tmpl_cmd_get_chassis_status_rs)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_chassis_control_rq = fiid_obj_create (tmpl_cmd_chassis_control_rq)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_chassis_control_rs = fiid_obj_create (tmpl_cmd_chassis_control_rs)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_chassis_identify_rq = fiid_obj_create (tmpl_cmd_chassis_identify_rq)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_chassis_identify_rs = fiid_obj_create (tmpl_cmd_chassis_identify_rs)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  
  if (cmd_args.oem_power_type != IPMIPOWER_OEM_POWER_TYPE_NONE)
    {
      if (cmd_args.oem_power_type == IPMIPOWER_OEM_POWER_TYPE_C410X)
	{
	  if (!(ip->obj_c410x_get_sensor_reading_rq = fiid_obj_create (tmpl_cmd_get_sensor_reading_rq)))
	    {
	      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
	      exit (EXIT_FAILURE);
	    }
	  if (!(ip->obj_c410x_get_sensor_reading_rs = fiid_obj_create (tmpl_cmd_get_sensor_reading_rs)))
	    {
	      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
	      exit (EXIT_FAILURE);
	    }
	  if (!(ip->obj_c410x_slot_power_control_rq = fiid_obj_create (tmpl_cmd_c410x_slot_power_control_rq)))
	    {
	      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
	      exit (EXIT_FAILURE);
	    }
	  if (!(ip->obj_c410x_slot_power_control_rs = fiid_obj_create (tmpl_cmd_c410x_slot_power_control_rs)))
	    {
	      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
	      exit (EXIT_FAILURE);
	    }
	}
    }
  
  if (!(ip->obj_close_session_rq = fiid_obj_create (tmpl_cmd_close_session_rq)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  if (!(ip->obj_close_session_rs = fiid_obj_create (tmpl_cmd_close_session_rs)))
    {
      IPMIPOWER_ERROR (("fiid_obj_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  if (!(ip->sockets_to_close = list_create (NULL)))
    {
      IPMIPOWER_ERROR (("list_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  if (cmd_args.oem_power_type != IPMIPOWER_OEM_POWER_TYPE_NONE)
    {
      assert (ipmipower_oem_power_cmd_check_support_and_privilege (cmd, NULL, 0) > 0);
      assert (ipmipower_oem_power_cmd_check_extra_arg (extra_arg, NULL, 0) > 0);

      if (extra_arg)
	{
	  if (!(ip->extra_arg = strdup (extra_arg)))
	    {
	      IPMIPOWER_ERROR (("strdup"));
	      exit (EXIT_FAILURE);
	    }
	}
      else
	ip->extra_arg = NULL;
    }
  else
    ip->extra_arg = NULL;

  /* When doing OEM power control, it is possible the user may specify
   * the same host multiple times.  This wouldn't be possible under
   * normal cases.  For example, under normal circumstances if the user did
   *
   * -h foohost,foohost --on
   *
   * foohost would be collapsed to just one "foohost" (through a call
   * to hostlist_uniq()), because it doesn't make sense to turn it on
   * twice.
   *
   * However, now someone might want to do
   *
   * --oem-power-type=FOO -h foohost+1,foohost+2 --on
   *
   * Which logically can make sense now.
   *
   * We do not want to do power control to the host in parallel b/c
   * many BMCs can't handle parallel sessions (you will BUSY errors).
   * So we will serialize power control operations to the same host.
   */

  /* XXX: The constant strcmp and searching of this list can be slow
   * (O(n^2)), but for the time being it is assumed this will not
   * be an overall performance issue for ipmipower.  If it does become
   * an issue, a bigger rearchitecture will be required.
   */
  
  ip->next = NULL;

  if (cmd_args.oem_power_type == IPMIPOWER_OEM_POWER_TYPE_C410X)
    {
      ipmipower_powercmd_t iptmp;

      if ((iptmp = list_find_first (pending,
				    _find_ipmipower_powercmd,
				    ip->ic->hostname)))
	{
	  /* find the last one in the list */
	  while (iptmp->next)
	    iptmp = iptmp->next;
	  iptmp->next = ip;
	  return;
	}
    }

  if (!list_append (pending, ip))
    {
      IPMIPOWER_ERROR (("list_append: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
}

int
ipmipower_powercmd_pending ()
{
  assert (pending);  /* did not run ipmipower_powercmd_setup() */

  return (!list_is_empty (pending));
}

/* _send_packet
 * - Send a packet of the specified type
 * - updates state and counts
 * - if this is a retransmission, do not update inbound and rqseq
 *   count.  BMC may need to know if this is a retransmission.  Must
 *   increment outbound sequence number, since BMC may increase outbound
 *   sequence number.
 */
static void
_send_packet (ipmipower_powercmd_t ip, ipmipower_packet_type_t pkt)
{
  uint8_t buf[IPMIPOWER_PACKET_BUFLEN];
  int ret, len = 0, dropped = 0;

  assert (ip);
  assert (IPMIPOWER_PACKET_TYPE_RQ (pkt));

  /* The following sequence number counts must be set before
   * ipmipower_packet_create, so the same value that is sent can be
   * matched later.
   */
  ip->ic->ipmi_requester_sequence_number_counter++;

  if (IPMIPOWER_PACKET_TYPE_IPMI_2_0_SETUP_RQ (pkt))
    ip->message_tag_count++;
  else if (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN_2_0
           && IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RQ (pkt))
    {
      /* IPMI 2.0 is special, sequence numbers of 0 don't count */
      ip->session_sequence_number++;
      if (!ip->session_sequence_number)
        ip->session_sequence_number++;
    }

  len = ipmipower_packet_create (ip, pkt, buf, IPMIPOWER_PACKET_BUFLEN);
  ipmipower_packet_dump (ip, pkt, buf, len);

  if ((ret = cbuf_write (ip->ic->ipmi_out, buf, len, &dropped)) < 0)
    {
      IPMIPOWER_ERROR (("cbuf_write: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  if (ret != len)
    {
      IPMIPOWER_ERROR (("cbuf_write: incorrect bytes written %d", ret));
      exit (EXIT_FAILURE);
    }
  
  if (dropped)
    IPMIPOWER_DEBUG (("cbuf_write: dropped %d bytes", dropped));

  secure_memset (buf, '\0', IPMIPOWER_PACKET_BUFLEN);

  switch (pkt)
    {
    case IPMIPOWER_PACKET_TYPE_AUTHENTICATION_CAPABILITIES_RQ:
      ip->protocol_state = IPMIPOWER_PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_SENT;
      break;
    case IPMIPOWER_PACKET_TYPE_GET_SESSION_CHALLENGE_RQ:
      ip->protocol_state = IPMIPOWER_PROTOCOL_STATE_GET_SESSION_CHALLENGE_SENT;
      break;
    case IPMIPOWER_PACKET_TYPE_ACTIVATE_SESSION_RQ:
      ip->protocol_state = IPMIPOWER_PROTOCOL_STATE_ACTIVATE_SESSION_SENT;
	
      /* IPMI Workaround (achu)
       *
       * Close all sockets that were saved during the Get Session
       * Challenge phase of the IPMI protocol.  See comments in
       * _retry_packets().
       */
      if (list_count (ip->sockets_to_close) > 0)
	{
	  int *fd;
	  while ((fd = list_pop (ip->sockets_to_close)))
	    {
	      /* cleanup path, ignore potential error */
	      close (*fd);
	      free (fd);
	    }
	}
      break;
    case IPMIPOWER_PACKET_TYPE_OPEN_SESSION_REQUEST:
      ip->protocol_state = IPMIPOWER_PROTOCOL_STATE_OPEN_SESSION_REQUEST_SENT;
      break;
    case IPMIPOWER_PACKET_TYPE_RAKP_MESSAGE_1:
      ip->protocol_state = IPMIPOWER_PROTOCOL_STATE_RAKP_MESSAGE_1_SENT;
      break;
    case IPMIPOWER_PACKET_TYPE_RAKP_MESSAGE_3:
      ip->protocol_state = IPMIPOWER_PROTOCOL_STATE_RAKP_MESSAGE_3_SENT;
      break;
    case IPMIPOWER_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RQ:
      ip->protocol_state = IPMIPOWER_PROTOCOL_STATE_SET_SESSION_PRIVILEGE_LEVEL_SENT;
      break;
    case IPMIPOWER_PACKET_TYPE_GET_CHASSIS_STATUS_RQ:
      ip->protocol_state = IPMIPOWER_PROTOCOL_STATE_GET_CHASSIS_STATUS_SENT;
      break;
    case IPMIPOWER_PACKET_TYPE_CHASSIS_CONTROL_RQ:
      ip->protocol_state = IPMIPOWER_PROTOCOL_STATE_CHASSIS_CONTROL_SENT;
      break;
    case IPMIPOWER_PACKET_TYPE_CHASSIS_IDENTIFY_RQ:
      ip->protocol_state = IPMIPOWER_PROTOCOL_STATE_CHASSIS_IDENTIFY_SENT;
      break;
    case IPMIPOWER_PACKET_TYPE_C410X_GET_SENSOR_READING_RQ:
      ip->protocol_state = IPMIPOWER_PROTOCOL_STATE_C410X_GET_SENSOR_READING_SENT;
      break;
    case IPMIPOWER_PACKET_TYPE_C410X_SLOT_POWER_CONTROL_RQ:
      ip->protocol_state = IPMIPOWER_PROTOCOL_STATE_C410X_SLOT_POWER_CONTROL_SENT;
      break;
    case IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ:
      ip->protocol_state = IPMIPOWER_PROTOCOL_STATE_CLOSE_SESSION_SENT;
      break;
    default:
      IPMIPOWER_ERROR (("_send_packet: invalid pkt type: %d", pkt));
      exit (EXIT_FAILURE);
    }

  /* Session inbound count is incremented after the packet is sent,
   * since the first inbound sequence number is specified by the
   * activate session command.
   */
  if (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN
      && IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RQ (pkt))
    ip->session_inbound_count++;

  if (gettimeofday (&(ip->ic->last_ipmi_send), NULL) < 0)
    {
      IPMIPOWER_ERROR (("gettimeofday: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
}

/* _recv_packet
 * - Receive a packet
 * Returns 1 if packet is of correct size and passes checks
 * Returns 0 if no packet received yet or packet should be ignored
 * Returns -1 if packet returned error
 */
static int
_recv_packet (ipmipower_powercmd_t ip, ipmipower_packet_type_t pkt)
{
  uint8_t recv_buf[IPMIPOWER_PACKET_BUFLEN];
  int recv_len = 0;
  int rv = -1;
  uint64_t val;

  assert (ip);
  assert (IPMIPOWER_PACKET_TYPE_RS (pkt));

  if (!(recv_len = ipmipower_cbuf_peek_and_drop (ip->ic->ipmi_in,
                                                 recv_buf,
                                                 IPMIPOWER_PACKET_BUFLEN)))
    return (0);

  ipmipower_packet_dump (ip, pkt, recv_buf, recv_len);

  /* rv = 0 if the packet is unparseable */
  if (!ipmipower_packet_store (ip, pkt, recv_buf, recv_len))
    {
      rv = 0;
      goto cleanup;
    }

  if (pkt == IPMIPOWER_PACKET_TYPE_AUTHENTICATION_CAPABILITIES_RS
      || pkt == IPMIPOWER_PACKET_TYPE_GET_SESSION_CHALLENGE_RS)
    {
      if (!ipmipower_check_checksum (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }

      if (!ipmipower_check_network_function (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }

      if (!ipmipower_check_command (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }

      if (!ipmipower_check_requester_sequence_number (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }

      /* If everything else is correct besides completion code, packet
       * returned an error.
       */
      if (!ipmipower_check_completion_code (ip, pkt))
        {
          ipmipower_output (ipmipower_packet_errmsg (ip, pkt), ip->ic->hostname, ip->extra_arg);
          ip->retransmission_count = 0;  /* important to reset */
          if (gettimeofday (&ip->ic->last_ipmi_recv, NULL) < 0)
            {
              IPMIPOWER_ERROR (("gettimeofday: %s", strerror (errno)));
              exit (EXIT_FAILURE);
            }
          goto cleanup;
        }

      /* If packet is no good though, ignore it, treat it like a
       * checksum error.  Note, you must check after checking
       * completion code.
       */
      if (!ipmipower_check_packet (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }
    }
  else if (pkt == IPMIPOWER_PACKET_TYPE_ACTIVATE_SESSION_RS)
    {
      if (!ipmipower_check_checksum (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }

      if (!ipmipower_check_authentication_code (ip,
                                                pkt,
                                                recv_buf,
                                                recv_len))
        {
          rv = 0;
          goto cleanup;
        }

      if (!ipmipower_check_network_function (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }

      if (!ipmipower_check_command (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }

      if (!ipmipower_check_requester_sequence_number (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }

      /* If everything else is correct besides completion code, packet
       * returned an error.
       */
      if (!ipmipower_check_completion_code (ip, pkt))
        {
          ipmipower_output (ipmipower_packet_errmsg (ip, pkt), ip->ic->hostname, ip->extra_arg);
          ip->retransmission_count = 0;  /* important to reset */
          if (gettimeofday (&ip->ic->last_ipmi_recv, NULL) < 0)
            {
              IPMIPOWER_ERROR (("gettimeofday: %s", strerror (errno)));
              exit (EXIT_FAILURE);
            }
          goto cleanup;
        }

      /* If packet is no good though, ignore it, treat it like a
       * checksum error.  Note, you must check after checking
       * completion code.
       */
      if (!ipmipower_check_packet (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }

      /* achu:
       *
       * this should really be done in _process_ipmi_packets(), but
       * because we are going to clear out the lan session header (b/c
       * it has sensitive information in it), we'll do this here.
       */
      
      if (FIID_OBJ_GET (ip->obj_lan_session_hdr_rs,
                        "session_sequence_number",
                        &val) < 0)
        {
          IPMIPOWER_ERROR (("FIID_OBJ_GET: 'session_sequence_number': %s",
                            fiid_obj_errormsg (ip->obj_lan_session_hdr_rs)));
          exit (EXIT_FAILURE);
        }
      
      ip->highest_received_sequence_number = val;
      
      /* IPMI Workaround (achu)
       *
       * Discovered on Sun Fire 4100.
       *
       * The session sequence numbers for IPMI 1.5 are the wrong endian.
       * So we have to flip the bits to workaround it.
       */
      if (cmd_args.common_args.workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_BIG_ENDIAN_SEQUENCE_NUMBER)
        {
          uint32_t tmp_session_sequence_number = ip->highest_received_sequence_number;
          
          ip->highest_received_sequence_number =
            ((tmp_session_sequence_number & 0xFF000000) >> 24)
            | ((tmp_session_sequence_number & 0x00FF0000) >> 8)
            | ((tmp_session_sequence_number & 0x0000FF00) << 8)
            | ((tmp_session_sequence_number & 0x000000FF) << 24);
        }
    }
  else if (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN
	   && IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RS (pkt))
    {
      if (!ipmipower_check_checksum (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }

      if (!ipmipower_check_authentication_code (ip,
                                                pkt,
                                                recv_buf,
                                                recv_len))
        {
          rv = 0;
          goto cleanup;
        }

      if (!ipmipower_check_outbound_sequence_number (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }

      if (!ipmipower_check_session_id (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }

      if (!ipmipower_check_network_function (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }

      if (!ipmipower_check_command (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }

      if (!ipmipower_check_requester_sequence_number (ip, pkt))
        {
          if (pkt == IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RS)
            goto close_session_workaround;
          rv = 0;
          goto cleanup;
        }

      /* If everything else is correct besides completion code, packet
       * returned an error.
       */
      if (!ipmipower_check_completion_code (ip, pkt))
        {
          if (pkt == IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RS)
            goto close_session_workaround;

          ipmipower_output (ipmipower_packet_errmsg (ip, pkt), ip->ic->hostname, ip->extra_arg);

          ip->retransmission_count = 0;  /* important to reset */
          if (gettimeofday (&ip->ic->last_ipmi_recv, NULL) < 0)
            {
              IPMIPOWER_ERROR (("gettimeofday: %s", strerror (errno)));
              exit (EXIT_FAILURE);
            }
          goto cleanup;
        }

      /* If packet is no good though, ignore it, treat it like a
       * checksum error.  Note, you must check after checking
       * completion code.
       */
      if (!ipmipower_check_packet (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }
    }
  else if (IPMIPOWER_PACKET_TYPE_IPMI_2_0_SETUP_RS (pkt))
    {
      if (!ipmipower_check_payload_type (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }
      
      if (!ipmipower_check_message_tag (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }
      
      /* I don't think there is a guarantee the data (authentication
       * keys, session id's, etc.) in the RAKP response will be valid
       * if there is a status code error.  So we check this status
       * code first, then the other stuff afterwards.
       */
      if (!ipmipower_check_rmcpplus_status_code (ip, pkt))
        {
          ipmipower_output (ipmipower_packet_errmsg (ip, pkt), ip->ic->hostname, ip->extra_arg);
          ip->retransmission_count = 0;  /* important to reset */
          if (gettimeofday (&ip->ic->last_ipmi_recv, NULL) < 0)
            {
              IPMIPOWER_ERROR (("gettimeofday: %s", strerror (errno)));
              exit (EXIT_FAILURE);
            }
          goto cleanup;
        }

      if (!ipmipower_check_session_id (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }

      if (pkt == IPMIPOWER_PACKET_TYPE_OPEN_SESSION_RESPONSE)
        {
          if (!ipmipower_check_open_session_response_privilege (ip, pkt))
            {
              ipmipower_output (IPMIPOWER_MSG_TYPE_PRIVILEGE_LEVEL_CANNOT_BE_OBTAINED, ip->ic->hostname, ip->extra_arg);
              goto cleanup;
            }
        }
      else if (pkt == IPMIPOWER_PACKET_TYPE_RAKP_MESSAGE_2)
        {
          if (!ipmipower_check_rakp_2_key_exchange_authentication_code (ip, pkt))
            {
              /* IPMI Compliance Issue
               *
               * On some systems, password could be correct, but
               * privilege is too high.  The error is b/c the
               * privilege error is not handled properly in the open
               * session stage (i.e. they tell me I can authenticate
               * at a high privilege level, that in reality is not
               * allowed).  Dunno how to deal with this.
               */
              ipmipower_output (IPMIPOWER_MSG_TYPE_PASSWORD_INVALID, ip->ic->hostname, ip->extra_arg);
              goto cleanup;
            }
        }
      else if (pkt == IPMIPOWER_PACKET_TYPE_RAKP_MESSAGE_4)
        {
          if (!ipmipower_check_rakp_4_integrity_check_value (ip, pkt))
            {
              ipmipower_output (IPMIPOWER_MSG_TYPE_K_G_INVALID, ip->ic->hostname, ip->extra_arg);
              goto cleanup;
            }
        }

      /* If packet is no good though, ignore it, treat it like a
       * checksum error.  Note, you must check after checking
       * completion code.
       */
      if (!ipmipower_check_packet (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }
    }
  else if (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN_2_0
	   && IPMIPOWER_PACKET_TYPE_IPMI_SESSION_PACKET_RS (pkt))
    {
      if (!ipmipower_check_payload_type (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }

      if (!ipmipower_check_payload_pad (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }
      
      if (!ipmipower_check_integrity_pad (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }
      
      if (!ipmipower_check_checksum (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }
      
      if (!ipmipower_check_authentication_code (ip,
                                                pkt,
                                                recv_buf,
                                                recv_len))
        {
          rv = 0;
          goto cleanup;
        }
      
      if (!ipmipower_check_outbound_sequence_number (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }
      
      if (!ipmipower_check_session_id (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }
      
      if (!ipmipower_check_network_function (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }

      if (!ipmipower_check_command (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }

      if (!ipmipower_check_requester_sequence_number (ip, pkt))
        {
          if (pkt == IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RS)
            goto close_session_workaround;
          rv = 0;
          goto cleanup;
        }
      
      /* If everything else is correct besides completion code, packet
       * returned an error.
       */
      if (!ipmipower_check_completion_code (ip, pkt))
        {
          if (pkt == IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RS)
            goto close_session_workaround;
          
          ip->retransmission_count = 0;  /* important to reset */
          if (gettimeofday (&ip->ic->last_ipmi_recv, NULL) < 0)
            {
              IPMIPOWER_ERROR (("gettimeofday: %s", strerror (errno)));
              exit (EXIT_FAILURE);
            }
          goto cleanup;
        }

      /* If packet is no good though, ignore it, treat it like a
       * checksum error.  Note, you must check after checking
       * completion code.
       */
      if (!ipmipower_check_packet (ip, pkt))
        {
          rv = 0;
          goto cleanup;
        }
    }

  /* Yipee everything passed, the packet is good.  Continue */

  /* achu: If this is the close session response and the packet is
   * mostly legit, go ahead and just accept the packet.  We'll
   * close the session anyways.
   */
 close_session_workaround:
  ip->retransmission_count = 0;  /* important to reset */
  if (gettimeofday (&ip->ic->last_ipmi_recv, NULL) < 0)
    {
      IPMIPOWER_ERROR (("gettimeofday: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  rv = 1;

 cleanup:
  /* Clear out data */
  secure_memset (recv_buf, '\0', IPMIPOWER_PACKET_BUFLEN);
  if (fiid_obj_clear (ip->obj_lan_session_hdr_rs) < 0)
    {
      IPMIPOWER_ERROR (("fiid_obj_clear: %s", fiid_obj_errormsg (ip->obj_lan_session_hdr_rs)));
      exit (EXIT_FAILURE);
    }
  if (fiid_obj_clear (ip->obj_rmcpplus_session_trlr_rs) < 0)
    {
      IPMIPOWER_ERROR (("fiid_obj_clear: %s", fiid_obj_errormsg (ip->obj_rmcpplus_session_trlr_rs)));
      exit (EXIT_FAILURE);
    }
  return (rv);
}

/* _has_timed_out
 * - Check if command timed out
 * Returns 1 if timed out, 0 if not
 */
static int
_has_timed_out (ipmipower_powercmd_t ip)
{
  struct timeval cur_time, result;
  unsigned int session_timeout;

  assert (ip);

  /* If we haven't started yet */
  if (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_START)
    return (0);

  if (gettimeofday (&cur_time, NULL) < 0)
    {
      IPMIPOWER_ERROR (("gettimeofday: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  timeval_sub (&cur_time, &(ip->time_begin), &result);
  timeval_millisecond_calc (&result, &session_timeout);

  /* Must use >=, otherwise we could potentially spin */
  if (session_timeout >= cmd_args.common_args.session_timeout)
    {
      /* Don't bother outputting timeout if we have finished the power
         control operation */
      if (ip->protocol_state != IPMIPOWER_PROTOCOL_STATE_CLOSE_SESSION_SENT)
        {
          /* Special cases */
          if (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_SENT)
            ipmipower_output (IPMIPOWER_MSG_TYPE_CONNECTION_TIMEOUT, ip->ic->hostname, ip->extra_arg);
          else if (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_ACTIVATE_SESSION_SENT)
            ipmipower_output (IPMIPOWER_MSG_TYPE_PASSWORD_VERIFICATION_TIMEOUT, ip->ic->hostname, ip->extra_arg);
          else
            ipmipower_output (IPMIPOWER_MSG_TYPE_SESSION_TIMEOUT, ip->ic->hostname, ip->extra_arg);
        }
      return (1);
    }

  return (0);
}

/* _retry_packets
 * - Check if we should retransmit and retransmit if necessary
 * Returns 1 if we sent a packet, 0 if not
 */
static int
_retry_packets (ipmipower_powercmd_t ip)
{
  struct timeval cur_time, end_time, result;
  unsigned int time_since_last_ipmi_send;
  unsigned int time_left;
  unsigned int retransmission_timeout;

  assert (ip);

  /* Don't retransmit if any of the following are true */
  if (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_START) /* we haven't started yet */
    return (0);

  /* Did we timeout on this packet? */
  if ((ip->wait_until_on_state
       && ip->cmd == IPMIPOWER_POWER_CMD_POWER_ON)
      || (ip->wait_until_off_state
          && ip->cmd == IPMIPOWER_POWER_CMD_POWER_OFF))
    retransmission_timeout = cmd_args.retransmission_wait_timeout * (1 + (ip->retransmission_count/cmd_args.retransmission_backoff_count));
  else
    retransmission_timeout = cmd_args.common_args.retransmission_timeout * (1 + (ip->retransmission_count/cmd_args.retransmission_backoff_count));

  if (gettimeofday (&cur_time, NULL) < 0)
    {
      IPMIPOWER_ERROR (("gettimeofday: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  timeval_sub (&cur_time, &(ip->ic->last_ipmi_send), &result);
  timeval_millisecond_calc (&result, &time_since_last_ipmi_send);

  if (time_since_last_ipmi_send < retransmission_timeout)
    return (0);

  /* Do we have enough time to retransmit? */
  timeval_add_ms (&cur_time, cmd_args.common_args.session_timeout, &end_time);
  timeval_sub (&end_time, &cur_time, &result);
  timeval_millisecond_calc (&result, &time_left);
  if (time_left < retransmission_timeout)
    return (0);

  ip->retransmission_count++;

  IPMIPOWER_DEBUG (("host = %s; p = %d; Sending retry, retry count=%d",
                    ip->ic->hostname,
                    ip->protocol_state,
                    ip->retransmission_count));

  switch (ip->protocol_state)
    {
    case IPMIPOWER_PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_SENT:
      _send_packet (ip, IPMIPOWER_PACKET_TYPE_AUTHENTICATION_CAPABILITIES_RQ);
      break;
    case IPMIPOWER_PROTOCOL_STATE_GET_SESSION_CHALLENGE_SENT:
      {
	/* IPMI Workaround (achu)
	 *
	 * Discovered on Intel Tiger4 (SR870BN4)
	 *
	 * If the reply from a previous Get Session Challenge request is
	 * lost on the network, the following retransmission will make
	 * the BMC confused and it will not respond to future packets.
	 *
	 * The problem seems to exist only when the retransmitted packet
	 * is transmitted from the same source port.  Therefore, the fix
	 * is to send the retransmission from a different source port.
	 * So we'll create a new socket, re-bind to an ephemereal port
	 * (guaranteeing us a brand new port), and store this new
	 * socket.
	 *
	 * In the event we need to resend this packet multiple times, we
	 * do not want the chance that old ports will be used again.  We
	 * store the old file descriptrs (which are bound to the old
	 * ports) on a list, and close all of them after we have gotten
	 * past the Get Session Challenge phase of the protocol.
	 */
	int new_fd, *old_fd;
	struct sockaddr_in srcaddr;
	
	if ((new_fd = socket (AF_INET, SOCK_DGRAM, 0)) < 0)
	  {
	    if (errno != EMFILE)
	      {
		IPMIPOWER_ERROR (("socket: %s", strerror (errno)));
		exit (EXIT_FAILURE);
	      }
	    
	    ipmipower_output (IPMIPOWER_MSG_TYPE_RESOURCES, ip->ic->hostname, ip->extra_arg);
	    return (-1);
	  }
	
	bzero (&srcaddr, sizeof (struct sockaddr_in));
	srcaddr.sin_family = AF_INET;
	srcaddr.sin_port = htons (0);
	srcaddr.sin_addr.s_addr = htonl (INADDR_ANY);
	
	if (bind (new_fd, &srcaddr, sizeof (struct sockaddr_in)) < 0)
	  {
	    IPMIPOWER_ERROR (("bind: %s", strerror (errno)));
	    exit (EXIT_FAILURE);
	  }
	
	if (!(old_fd = (int *)malloc (sizeof (int))))
	  {
	    IPMIPOWER_ERROR (("malloc: %s", strerror (errno)));
	    exit (EXIT_FAILURE);
	  }
	
	*old_fd = ip->ic->ipmi_fd;
	list_push (ip->sockets_to_close, old_fd);
	
	ip->ic->ipmi_fd = new_fd;
	
	_send_packet (ip, IPMIPOWER_PACKET_TYPE_GET_SESSION_CHALLENGE_RQ);
      }
      break;
    case IPMIPOWER_PROTOCOL_STATE_ACTIVATE_SESSION_SENT:
      _send_packet (ip, IPMIPOWER_PACKET_TYPE_ACTIVATE_SESSION_RQ);
      break;
    case IPMIPOWER_PROTOCOL_STATE_OPEN_SESSION_REQUEST_SENT:
      _send_packet (ip, IPMIPOWER_PACKET_TYPE_OPEN_SESSION_REQUEST);
      break;
    case IPMIPOWER_PROTOCOL_STATE_RAKP_MESSAGE_1_SENT:
      _send_packet (ip, IPMIPOWER_PACKET_TYPE_RAKP_MESSAGE_1);
      break;
    case IPMIPOWER_PROTOCOL_STATE_RAKP_MESSAGE_3_SENT:
      _send_packet (ip, IPMIPOWER_PACKET_TYPE_RAKP_MESSAGE_3);
      break;
    case IPMIPOWER_PROTOCOL_STATE_SET_SESSION_PRIVILEGE_LEVEL_SENT:
      _send_packet (ip, IPMIPOWER_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RQ);
      break;
    case IPMIPOWER_PROTOCOL_STATE_GET_CHASSIS_STATUS_SENT:
      _send_packet (ip, IPMIPOWER_PACKET_TYPE_GET_CHASSIS_STATUS_RQ);
      break;
    case IPMIPOWER_PROTOCOL_STATE_CHASSIS_CONTROL_SENT:
      _send_packet (ip, IPMIPOWER_PACKET_TYPE_CHASSIS_CONTROL_RQ);
      break;
    case IPMIPOWER_PROTOCOL_STATE_CHASSIS_IDENTIFY_SENT:
      _send_packet (ip, IPMIPOWER_PACKET_TYPE_CHASSIS_IDENTIFY_RQ);
      break;
    case IPMIPOWER_PROTOCOL_STATE_C410X_GET_SENSOR_READING_SENT:
      _send_packet (ip, IPMIPOWER_PACKET_TYPE_C410X_GET_SENSOR_READING_RQ);
      break;
    case IPMIPOWER_PROTOCOL_STATE_C410X_SLOT_POWER_CONTROL_SENT:
      _send_packet (ip, IPMIPOWER_PACKET_TYPE_C410X_SLOT_POWER_CONTROL_RQ);
      break;
    case IPMIPOWER_PROTOCOL_STATE_CLOSE_SESSION_SENT:
      {
	/*
	 * It's pointless to retransmit a close-session.
	 *
	 * 1) The power control operation has already completed.
	 *
	 * 2) There is no guarantee the remote BMC will respond.  If the
	 * previous close session response was dropped by the network,
	 * then the session has already been closed by the BMC.  Any
	 * retransmission will send a session id that is unknown to the
	 * BMC, and they will either respond with an error or ignore the
	 * packet.
	 *
	 * _send_packet(ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
	 */
	ip->close_timeout++;
	return (0);
      }
      break;
    default:
      IPMIPOWER_ERROR (("_retry_packets: invalid protocol state: %d\n",
			ip->protocol_state));
      exit (EXIT_FAILURE);
    }
  
  return (1);
}

/* _check_ipmi_1_5_authentication_capabilities
 *
 * Check the contents of a ipmi 1.5 or 2.0 authentication capabilities
 * response.
 *
 * Returns  0 if authentication passed and the protocol should continue
 * Returns -1 on ipmi protocol error or discovery error
 */
static int
_check_ipmi_1_5_authentication_capabilities (ipmipower_powercmd_t ip)
{
  uint8_t authentication_status_per_message_authentication;
  uint64_t val;
  int ret;

  assert (ip);

  if (FIID_OBJ_GET (ip->obj_authentication_capabilities_rs,
                    "authentication_status.per_message_authentication",
                    &val) < 0)
    {
      IPMIPOWER_ERROR (("FIID_OBJ_GET: 'authentication_status.per_message_authentication': %s",
                        fiid_obj_errormsg (ip->obj_authentication_capabilities_rs)));
      exit (EXIT_FAILURE);
    }
  authentication_status_per_message_authentication = val;

  /* IPMI Workaround (achu)
   *
   * Discovered on an ASUS P5M2 motherboard.
   *
   * The ASUS motherboard reports incorrect settings of anonymous
   * vs. null vs non-null username capabilities. The workaround is to
   * skip these checks.
   */
  if (!(cmd_args.common_args.workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES))
    {
      if ((ret = ipmi_check_authentication_capabilities_username (cmd_args.common_args.username,
                                                                  cmd_args.common_args.password,
                                                                  ip->obj_authentication_capabilities_rs)) < 0)
        {
          IPMIPOWER_ERROR (("ipmi_check_authentication_capabilities_username: %s",
                            strerror (errno)));
          exit (EXIT_FAILURE);
        }

      if (!ret)
        {
          ipmipower_output (IPMIPOWER_MSG_TYPE_USERNAME_INVALID, ip->ic->hostname, ip->extra_arg);
          return (-1);
        }
    }

  /* IPMI Workaround (achu)
   *
   * Not discovered yet, assume some motherboard will have it some
   * day.
   *
   * Authentication capabilities flags are not listed properly in the
   * response.  The workaround is to skip these checks.
   */
  if (!(cmd_args.common_args.workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_AUTHENTICATION_CAPABILITIES))
    {
      if ((ret = ipmi_check_authentication_capabilities_authentication_type (cmd_args.common_args.authentication_type,
                                                                             ip->obj_authentication_capabilities_rs)) < 0)
        {
          IPMIPOWER_ERROR (("ipmi_check_authentication_capabilities_authentication_type: %s",
                            strerror (errno)));
          exit (EXIT_FAILURE);
        }
      
      if (!ret)
        {
          ipmipower_output (IPMIPOWER_MSG_TYPE_AUTHENTICATION_TYPE_UNAVAILABLE, ip->ic->hostname, ip->extra_arg);
          return (-1);
        }
    }
      
  /* IPMI Workaround (achu)
   *
   * Discovered on IBM eServer 325
   *
   * The remote BMC ignores if permsg authentiction is enabled
   * or disabled.  So we need to force it no matter what.
   */
  if (!(cmd_args.common_args.workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_FORCE_PERMSG_AUTHENTICATION))
    {
      if (!authentication_status_per_message_authentication)
        ip->permsgauth_enabled = 1;
      else
        ip->permsgauth_enabled = 0;
    }
  else
    ip->permsgauth_enabled = 1;

  return (0);
}

/* _check_ipmi_2_0_authentication_capabilities
 *
 * Check the contents of a ipmi 2.0 authentication capabilities response.
 *
 * Returns  0 if authentication passed
 * Returns -1 on ipmi protocol error
 */
static int
_check_ipmi_2_0_authentication_capabilities (ipmipower_powercmd_t ip)
{
  void *tmp_k_g_ptr = NULL;
  int ret;

  assert (ip);

  if ((ret = ipmi_check_authentication_capabilities_ipmi_2_0 (ip->obj_authentication_capabilities_rs)) < 0)
    {
      IPMIPOWER_ERROR (("ipmi_check_authentication_capabilities_ipmi_2_0: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  if (!ret)
    {
      ipmipower_output (IPMIPOWER_MSG_TYPE_IPMI_2_0_UNAVAILABLE, ip->ic->hostname, ip->extra_arg);
      return (-1);
    }

  /* IPMI Workaround (achu)
   *
   * Discovered on an ASUS P5M2 motherboard.
   *
   * The ASUS motherboard reports incorrect settings of anonymous
   * vs. null vs non-null username capabilities.  The workaround is to
   * skip these checks.
   *
   * Discovered on an ASUS P5MT-R motherboard
   *
   * K_g status is reported incorrectly too.  Again, skip the checks.
   */
  if (!(cmd_args.common_args.workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_AUTHENTICATION_CAPABILITIES))
    {
      if ((ret = ipmi_check_authentication_capabilities_username (cmd_args.common_args.username,
                                                                  cmd_args.common_args.password,
                                                                  ip->obj_authentication_capabilities_rs)) < 0)
        {
          IPMIPOWER_ERROR (("ipmi_check_authentication_capabilities_username: %s",
                            strerror (errno)));
          exit (EXIT_FAILURE);
        }

      if (!ret)
        {
          ipmipower_output (IPMIPOWER_MSG_TYPE_USERNAME_INVALID, ip->ic->hostname, ip->extra_arg);
          return (-1);
        }

      if (cmd_args.common_args.k_g_len)
        tmp_k_g_ptr = cmd_args.common_args.k_g;

      if ((ret = ipmi_check_authentication_capabilities_k_g (tmp_k_g_ptr,
                                                             ip->obj_authentication_capabilities_rs)) < 0)
        {
          IPMIPOWER_ERROR (("ipmi_check_authentication_capabilities_k_g: %s",
                            strerror (errno)));
          exit (EXIT_FAILURE);
        }

      if (!ret)
        {
          ipmipower_output (IPMIPOWER_MSG_TYPE_K_G_INVALID, ip->ic->hostname, ip->extra_arg);
          return (-1);
        }
    }

  return (0);
}

/* _check_activate_session_authentication_type
 *
 * Check if the activate session response has the appropriate
 * authentication type.
 *
 * Returns  0 if yes
 * Returns -1 if no, which is an ipmi protocol error
 */
static int
_check_activate_session_authentication_type (ipmipower_powercmd_t ip)
{
  uint8_t authentication_type;
  uint64_t val;

  assert (ip);
  assert (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_ACTIVATE_SESSION_SENT);

  if (FIID_OBJ_GET (ip->obj_activate_session_rs,
                    "authentication_type",
                    &val) < 0)
    {
      IPMIPOWER_ERROR (("FIID_OBJ_GET: 'authentication_type': %s",
                        fiid_obj_errormsg (ip->obj_activate_session_rs)));
      exit (EXIT_FAILURE);
    }
  authentication_type = val;

  if (cmd_args.common_args.workaround_flags_outofband & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_FORCE_PERMSG_AUTHENTICATION)
    return (0);

  /* IPMI Workaround (achu)
   *
   * Discovered on Supermicro H8QME with SIMSO daughter card.
   *
   * (Note: This could work for "IBM eServer 325" per msg auth
   * problem.  But I don't have hardware to test it :-()
   *
   * The remote BMC ignores if permsg authentiction is disabled.
   * Handle it appropriately by just not doing permsg authentication.
   */
  if (!ip->permsgauth_enabled)
    {
      if (authentication_type != IPMI_AUTHENTICATION_TYPE_NONE)
        {
          IPMIPOWER_DEBUG (("host = %s; p = %d; not none authentcation",
                            ip->ic->hostname,
                            ip->protocol_state));

          ip->permsgauth_enabled = 1;
        }
    }

  if (ip->permsgauth_enabled)
    {
      if (authentication_type != cmd_args.common_args.authentication_type)
        {
          IPMIPOWER_DEBUG (("host = %s; p = %d; authentication_type mismatch",
                            ip->ic->hostname,
                            ip->protocol_state));

          ipmipower_output (IPMIPOWER_MSG_TYPE_BMC_ERROR, ip->ic->hostname, ip->extra_arg);

          ip->retransmission_count = 0;  /* important to reset */
          if (gettimeofday (&ip->ic->last_ipmi_recv, NULL) < 0)
            {
              IPMIPOWER_ERROR (("gettimeofday: %s", strerror (errno)));
              exit (EXIT_FAILURE);
            }
          return (-1);
        }
    }

  return (0);
}

/* _calculate_cipher_keys
 *
 * Calculate cipher keys for the remaining IPMI 2.0 protocol
 *
 * Returns  0 on success
 * Returns -1 on ipmi protocol error
 */
static int
_calculate_cipher_keys (ipmipower_powercmd_t ip)
{
  uint8_t managed_system_random_number[IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH];
  int managed_system_random_number_len;
  char *username;
  char username_buf[IPMI_MAX_USER_NAME_LENGTH+1];
  unsigned int username_len;
  char *password;
  unsigned int password_len;
  void *k_g;

  assert (ip);
  assert (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_RAKP_MESSAGE_1_SENT);

  /* IPMI Workaround (achu)
   *
   * Discovered on SE7520AF2 with Intel Server Management Module
   * (Professional Edition)
   *
   * The username must be padded despite explicitly not being
   * allowed.  "No Null characters (00h) are allowed in the name".
   * Table 13-11 in the IPMI 2.0 spec.
   */
  if (cmd_args.common_args.workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION)
    {
      memset (username_buf, '\0', IPMI_MAX_USER_NAME_LENGTH+1);
      if (cmd_args.common_args.username)
        strcpy (username_buf, cmd_args.common_args.username);
      username = username_buf;
      username_len = IPMI_MAX_USER_NAME_LENGTH;
    }
  else
    {
      username = cmd_args.common_args.username;
      username_len = (username) ? strlen (username) : 0;
    }

  password = cmd_args.common_args.password;
  password_len = (password) ? strlen (password) : 0;

  /* IPMI Workaround (achu)
   *
   * Discovered on SE7520AF2 with Intel Server Management Module
   * (Professional Edition)
   *
   * When the authentication algorithm is HMAC-MD5-128 and the
   * password is greater than 16 bytes, the Intel BMC truncates the
   * password to 16 bytes when generating keys, hashes, etc.  So we
   * have to do the same when generating keys, hashes, etc.
   */
  if ((cmd_args.common_args.workaround_flags_outofband_2_0 & IPMI_PARSE_WORKAROUND_FLAGS_OUTOFBAND_2_0_INTEL_2_0_SESSION)
      && ip->authentication_algorithm == IPMI_AUTHENTICATION_ALGORITHM_RAKP_HMAC_MD5
      && password_len > IPMI_1_5_MAX_PASSWORD_LENGTH)
    password_len = IPMI_1_5_MAX_PASSWORD_LENGTH;

  if (cmd_args.common_args.k_g_len)
    k_g = cmd_args.common_args.k_g;
  else
    k_g = NULL;

  if ((managed_system_random_number_len = fiid_obj_get_data (ip->obj_rakp_message_2_rs,
                                                             "managed_system_random_number",
                                                             managed_system_random_number,
                                                             IPMI_MANAGED_SYSTEM_RANDOM_NUMBER_LENGTH)) < 0)
    {
      IPMIPOWER_ERROR (("fiid_obj_get_data: 'managed_system_random_number': %s",
                        fiid_obj_errormsg (ip->obj_rakp_message_2_rs)));
      exit (EXIT_FAILURE);
    }

  if (ipmi_calculate_rmcpplus_session_keys (ip->authentication_algorithm,
                                            ip->integrity_algorithm,
                                            ip->confidentiality_algorithm,
                                            password,
                                            password_len,
                                            k_g,
                                            (k_g) ? cmd_args.common_args.k_g_len : 0,
                                            ip->remote_console_random_number,
                                            IPMI_REMOTE_CONSOLE_RANDOM_NUMBER_LENGTH,
                                            managed_system_random_number,
                                            managed_system_random_number_len,
                                            ip->name_only_lookup,
                                            cmd_args.common_args.privilege_level,
                                            username,
                                            username_len,
                                            &(ip->sik_key_ptr),
                                            &(ip->sik_key_len),
                                            &(ip->integrity_key_ptr),
                                            &(ip->integrity_key_len),
                                            &(ip->confidentiality_key_ptr),
                                            &(ip->confidentiality_key_len)) < 0)
    {
      IPMIPOWER_ERROR (("_calculate_cipher_keys(%s:%d): ipmi_calculate_rmcpplus_session_keys: %s",
                        ip->ic->hostname, ip->protocol_state, strerror (errno)));
      exit (EXIT_FAILURE);
    }

  return (0);
}

/* _process_ipmi_packets
 * - Main function that handles packet sends/receives for
 *   the power control protocol
 * - Returns timeout length, or < 0 if command completed and should
 *   be removed from pending.
 */
static int
_process_ipmi_packets (ipmipower_powercmd_t ip)
{
  struct timeval cur_time, end_time, result;
  unsigned int timeout;
  uint64_t val;
  int rv;

  assert (ip);
  assert (IPMIPOWER_PROTOCOL_STATE_VALID (ip->protocol_state));
  assert (IPMIPOWER_OEM_POWER_TYPE_VALID (cmd_args.oem_power_type));

  /* if timeout, give up */
  if (_has_timed_out (ip))
    return (-1);

  /* retransmit? */
  if ((rv = _retry_packets (ip)))
    {
      if (rv < 0)
        return (-1);
      goto done;
    }

  if (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_START)
    {
      /* Don't execute if fanout turned on and we're in the middle of too
       * many power commands.
       */
      if (cmd_args.common_args.fanout
          && (executing_count >= cmd_args.common_args.fanout))
        return (cmd_args.common_args.session_timeout);

      _send_packet (ip, IPMIPOWER_PACKET_TYPE_AUTHENTICATION_CAPABILITIES_RQ);

      if (gettimeofday (&(ip->time_begin), NULL) < 0)
        {
          IPMIPOWER_ERROR (("gettimeofday: %s", strerror (errno)));
          exit (EXIT_FAILURE);
        }
      executing_count++;
    }
  else if (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_AUTHENTICATION_CAPABILITIES_SENT)
    {
      if ((rv = _recv_packet (ip, IPMIPOWER_PACKET_TYPE_AUTHENTICATION_CAPABILITIES_RS)) != 1)
        {
          if (rv < 0)
            return (-1);
          goto done;
        }
      
      if (cmd_args.common_args.driver_type == IPMI_DEVICE_LAN_2_0)
        {
          if (_check_ipmi_2_0_authentication_capabilities (ip) < 0)
            return (-1);
          
          _send_packet (ip, IPMIPOWER_PACKET_TYPE_OPEN_SESSION_REQUEST);
        }
      else
        {
          if (_check_ipmi_1_5_authentication_capabilities (ip) < 0)
            return (-1);
          
          _send_packet (ip, IPMIPOWER_PACKET_TYPE_GET_SESSION_CHALLENGE_RQ);
        }
    }
  else if (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_GET_SESSION_CHALLENGE_SENT)
    {
      if ((rv = _recv_packet (ip, IPMIPOWER_PACKET_TYPE_GET_SESSION_CHALLENGE_RS)) != 1)
        {
          if (rv < 0)
            /* XXX Session is not up, is it ok to quit here?  Or
             * should we timeout?? */
            return (-1);
          goto done;
        }

      _send_packet (ip, IPMIPOWER_PACKET_TYPE_ACTIVATE_SESSION_RQ);
    }
  else if (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_ACTIVATE_SESSION_SENT)
    {
      if ((rv = _recv_packet (ip, IPMIPOWER_PACKET_TYPE_ACTIVATE_SESSION_RS)) != 1)
        {
          if (rv < 0)
            /* XXX Session is not up, is it ok to quit here?  Or
             * should we timeout?? */
            return (-1);
          goto done;
        }

      if (_check_activate_session_authentication_type (ip) < 0)
        /* XXX Session is not up, is it ok to quit here?  Or
         * should we timeout?? */
        return (-1);
     
      _send_packet (ip, IPMIPOWER_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RQ);
    }
  else if (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_OPEN_SESSION_REQUEST_SENT)
    {
      if ((rv = _recv_packet (ip, IPMIPOWER_PACKET_TYPE_OPEN_SESSION_RESPONSE)) != 1)
        {
          if (rv < 0)
            /* XXX Session is not up, is it ok to quit here?  Or
             * should we timeout?? */
            return (-1);
          goto done;
        }

      _send_packet (ip, IPMIPOWER_PACKET_TYPE_RAKP_MESSAGE_1);
    }
  else if (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_RAKP_MESSAGE_1_SENT)
    {
      if ((rv = _recv_packet (ip, IPMIPOWER_PACKET_TYPE_RAKP_MESSAGE_2)) != 1)
        {
          if (rv < 0)
            /* XXX Session is not up, is it ok to quit here?  Or
             * should we timeout?? */
            return (-1);
          goto done;
        }

      if (_calculate_cipher_keys (ip) < 0)
        return (-1);

      _send_packet (ip, IPMIPOWER_PACKET_TYPE_RAKP_MESSAGE_3);
    }
  else if (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_RAKP_MESSAGE_3_SENT)
    {
      if ((rv = _recv_packet (ip, IPMIPOWER_PACKET_TYPE_RAKP_MESSAGE_4)) != 1)
        {
          if (rv < 0)
            /* XXX Session is not up, is it ok to quit here?  Or
             * should we timeout?? */
            return (-1);
          goto done;
        }

      _send_packet (ip, IPMIPOWER_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RQ);
    }
  else if (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_SET_SESSION_PRIVILEGE_LEVEL_SENT)
    {
      if ((rv = _recv_packet (ip, IPMIPOWER_PACKET_TYPE_SET_SESSION_PRIVILEGE_LEVEL_RS)) != 1)
        {
          if (rv < 0)
            /* Session is up, so close it */
            _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
          goto done;
        }

      if (cmd_args.oem_power_type == IPMIPOWER_OEM_POWER_TYPE_NONE)
	{
	  if (ip->cmd == IPMIPOWER_POWER_CMD_POWER_STATUS
	      || ip->cmd == IPMIPOWER_POWER_CMD_IDENTIFY_STATUS
	      || (cmd_args.on_if_off
		  && (ip->cmd == IPMIPOWER_POWER_CMD_POWER_CYCLE
		      || ip->cmd == IPMIPOWER_POWER_CMD_POWER_RESET)))
	    _send_packet (ip, IPMIPOWER_PACKET_TYPE_GET_CHASSIS_STATUS_RQ);
	  else if (ip->cmd == IPMIPOWER_POWER_CMD_IDENTIFY_ON
		   || ip->cmd == IPMIPOWER_POWER_CMD_IDENTIFY_OFF)
	    _send_packet (ip, IPMIPOWER_PACKET_TYPE_CHASSIS_IDENTIFY_RQ);
	  else /* on, off, cycle, reset, pulse diag interupt, soft shutdown */
	    _send_packet (ip, IPMIPOWER_PACKET_TYPE_CHASSIS_CONTROL_RQ);
	}
      else /* cmd_args.oem_power_type == IPMIPOWER_OEM_POWER_TYPE_C410X */
	{
	  assert (ip->cmd == IPMIPOWER_POWER_CMD_POWER_STATUS
		  || ip->cmd == IPMIPOWER_POWER_CMD_POWER_OFF
		  || ip->cmd == IPMIPOWER_POWER_CMD_POWER_ON);

	  _send_packet (ip, IPMIPOWER_PACKET_TYPE_C410X_GET_SENSOR_READING_RQ);
	}
    }
  else if (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_GET_CHASSIS_STATUS_SENT)
    {
      uint8_t power_state;

      if ((rv = _recv_packet (ip, IPMIPOWER_PACKET_TYPE_GET_CHASSIS_STATUS_RS)) != 1)
        {
          if (rv < 0)
            /* Session is up, so close it */
            _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
          goto done;
        }

      if (FIID_OBJ_GET (ip->obj_get_chassis_status_rs,
                        "current_power_state.power_is_on",
                        &val) < 0)
        {
          IPMIPOWER_ERROR (("FIID_OBJ_GET: 'current_power_state.power_is_on': %s",
                            fiid_obj_errormsg (ip->obj_get_chassis_status_rs)));
          exit (EXIT_FAILURE);
        }
      power_state = val;

      if (cmd_args.wait_until_on
          && ip->cmd == IPMIPOWER_POWER_CMD_POWER_ON
          && ip->wait_until_on_state)
        {
          if (power_state == IPMI_SYSTEM_POWER_IS_ON)
            {
              ipmipower_output (IPMIPOWER_MSG_TYPE_OK, ip->ic->hostname, ip->extra_arg);
              ip->wait_until_on_state = 0;
              _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
            }
        }
      else if (cmd_args.wait_until_off
               && ip->cmd == IPMIPOWER_POWER_CMD_POWER_OFF
               && ip->wait_until_off_state)
        {
          if (power_state == IPMI_SYSTEM_POWER_IS_OFF)
            {
              ipmipower_output (IPMIPOWER_MSG_TYPE_OK, ip->ic->hostname, ip->extra_arg);
              ip->wait_until_off_state = 0;
              _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
            }
        }
      else if (ip->cmd == IPMIPOWER_POWER_CMD_POWER_STATUS)
        {
          ipmipower_output ((power_state == IPMI_SYSTEM_POWER_IS_ON) ? IPMIPOWER_MSG_TYPE_ON : IPMIPOWER_MSG_TYPE_OFF,
                            ip->ic->hostname,
			    ip->extra_arg);
          _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
        }
      else if (cmd_args.on_if_off && (ip->cmd == IPMIPOWER_POWER_CMD_POWER_CYCLE
                                      || ip->cmd == IPMIPOWER_POWER_CMD_POWER_RESET))
        {
          if (!power_state)
            {
              /* This is now a power-on operation */
              ip->cmd = IPMIPOWER_POWER_CMD_POWER_ON;
            }
          _send_packet (ip, IPMIPOWER_PACKET_TYPE_CHASSIS_CONTROL_RQ);
        }
      else if (ip->cmd == IPMIPOWER_POWER_CMD_IDENTIFY_STATUS)
        {
          uint8_t identify_status_supported;

          if (FIID_OBJ_GET (ip->obj_get_chassis_status_rs,
                            "misc_chassis_state.chassis_identify_command_and_state_info_supported",
                            &val) < 0)
            {
              IPMIPOWER_ERROR (("FIID_OBJ_GET: 'misc_chassis_state.chassis_identify_command_and_state_info_supported': %s",
                                fiid_obj_errormsg (ip->obj_get_chassis_status_rs)));
              exit (EXIT_FAILURE);
            }
          identify_status_supported = val;

          if (identify_status_supported)
            {
              uint8_t identify_status;

              if (FIID_OBJ_GET (ip->obj_get_chassis_status_rs,
                                "misc_chassis_state.chassis_identify_state",
                                &val) < 0)
                {
                  IPMIPOWER_ERROR (("FIID_OBJ_GET: 'misc_chassis_state.chassis_identify_state': %s",
                                    fiid_obj_errormsg (ip->obj_get_chassis_status_rs)));
                  exit (EXIT_FAILURE);
                }
              identify_status = val;

              if (identify_status == IPMI_CHASSIS_IDENTIFY_STATE_OFF)
                ipmipower_output (IPMIPOWER_MSG_TYPE_OFF, ip->ic->hostname, ip->extra_arg);
              else if (identify_status == IPMI_CHASSIS_IDENTIFY_STATE_TEMPORARY_ON
                       || identify_status == IPMI_CHASSIS_IDENTIFY_STATE_INDEFINITE_ON)
                ipmipower_output (IPMIPOWER_MSG_TYPE_ON, ip->ic->hostname, ip->extra_arg);
              else
                ipmipower_output (IPMIPOWER_MSG_TYPE_UNKNOWN, ip->ic->hostname, ip->extra_arg);
            }
          else
            ipmipower_output (IPMIPOWER_MSG_TYPE_UNKNOWN, ip->ic->hostname, ip->extra_arg);

          _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
        }
      else
        {
          IPMIPOWER_ERROR (("_process_ipmi_packets: invalid command state: %d", ip->cmd));
          exit (EXIT_FAILURE);
        }
    }
  else if (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_CHASSIS_CONTROL_SENT)
    {
      if ((rv = _recv_packet (ip, IPMIPOWER_PACKET_TYPE_CHASSIS_CONTROL_RS)) != 1)
        {
          if (rv < 0)
            /* Session is up, so close it */
            _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
          goto done;
        }

      if ((cmd_args.wait_until_on
           && ip->cmd == IPMIPOWER_POWER_CMD_POWER_ON)
          || (cmd_args.wait_until_off
              && ip->cmd == IPMIPOWER_POWER_CMD_POWER_OFF))
        {
          if (ip->cmd == IPMIPOWER_POWER_CMD_POWER_ON)
            ip->wait_until_on_state++;
          else
            ip->wait_until_off_state++;
          _send_packet (ip, IPMIPOWER_PACKET_TYPE_GET_CHASSIS_STATUS_RQ);
        }
      else
        {
          ipmipower_output (IPMIPOWER_MSG_TYPE_OK, ip->ic->hostname, ip->extra_arg);

          /* IPMI Workaround (achu)
           *
           * Discovered on Intel Tiger4 (SR870BN4)
           *
           * There is no response from the IPMI close command if the
           * IPMIPOWER_POWER_CMD_POWER_RESET power control command is
           * successful.  So just skip the close session.
           */
          if (ip->cmd == IPMIPOWER_POWER_CMD_POWER_RESET)
            goto finish_up;
          else
            _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
        }
    }
  else if (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_CHASSIS_IDENTIFY_SENT)
    {
      if ((rv = _recv_packet (ip, IPMIPOWER_PACKET_TYPE_CHASSIS_IDENTIFY_RS)) != 1)
        {
          if (rv < 0)
            /* Session is up, so close it */
            _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
          goto done;
        }

      ipmipower_output (IPMIPOWER_MSG_TYPE_OK, ip->ic->hostname, ip->extra_arg);
      _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
    }
  else if (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_C410X_GET_SENSOR_READING_SENT)
    {
      uint8_t sensor_reading;
      uint8_t reading_state;
      uint8_t sensor_scanning;
      int slot_power_on_flag;

      if ((rv = _recv_packet (ip, IPMIPOWER_PACKET_TYPE_C410X_GET_SENSOR_READING_RS)) != 1)
        {
          if (rv < 0)
            /* Session is up, so close it */
            _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
          goto done;
        }

      if (FIID_OBJ_GET (ip->obj_c410x_get_sensor_reading_rs,
                        "sensor_reading",
                        &val) < 0)
        {
          IPMIPOWER_ERROR (("FIID_OBJ_GET: 'sensor_reading': %s",
                            fiid_obj_errormsg (ip->obj_get_chassis_status_rs)));
          exit (EXIT_FAILURE);
        }
      sensor_reading = val;

      if (FIID_OBJ_GET (ip->obj_c410x_get_sensor_reading_rs,
                        "reading_state",
                        &val) < 0)
        {
          IPMIPOWER_ERROR (("FIID_OBJ_GET: 'reading_state': %s",
                            fiid_obj_errormsg (ip->obj_get_chassis_status_rs)));
          exit (EXIT_FAILURE);
        }
      reading_state = val;

      if (FIID_OBJ_GET (ip->obj_c410x_get_sensor_reading_rs,
                        "sensor_scanning",
                        &val) < 0)
        {
          IPMIPOWER_ERROR (("FIID_OBJ_GET: 'sensor_scanning': %s",
                            fiid_obj_errormsg (ip->obj_get_chassis_status_rs)));
          exit (EXIT_FAILURE);
        }
      sensor_scanning = val;

      if (reading_state == IPMI_SENSOR_READING_STATE_UNAVAILABLE
	  || sensor_scanning == IPMI_SENSOR_SCANNING_ON_THIS_SENSOR_DISABLE)
	{
	  ipmipower_output (IPMIPOWER_MSG_TYPE_BMC_ERROR, ip->ic->hostname, ip->extra_arg);
	  _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
	  goto done;
	}

      /* If non-zero, then it's on */
      /* achu: Sometimes "off" is 2.0 Watts, which equates to a sensor reading of 1 */
      if (sensor_reading > 1)
	slot_power_on_flag = 1;
      else
	slot_power_on_flag = 0;

      if (cmd_args.wait_until_on
          && ip->cmd == IPMIPOWER_POWER_CMD_POWER_ON
          && ip->wait_until_on_state)
        {
          if (slot_power_on_flag)
            {
              ipmipower_output (IPMIPOWER_MSG_TYPE_OK, ip->ic->hostname, ip->extra_arg);
              ip->wait_until_on_state = 0;
              _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
            }
        }
      else if (cmd_args.wait_until_off
               && ip->cmd == IPMIPOWER_POWER_CMD_POWER_OFF
               && ip->wait_until_off_state)
        {
          if (!slot_power_on_flag)
            {
              ipmipower_output (IPMIPOWER_MSG_TYPE_OK, ip->ic->hostname, ip->extra_arg);
              ip->wait_until_off_state = 0;
              _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
            }
        }
      else if (ip->cmd == IPMIPOWER_POWER_CMD_POWER_STATUS)
        {
          ipmipower_output ((slot_power_on_flag) ? IPMIPOWER_MSG_TYPE_ON : IPMIPOWER_MSG_TYPE_OFF,
                            ip->ic->hostname,
			    ip->extra_arg);
          _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
        }
      else if (ip->cmd == IPMIPOWER_POWER_CMD_POWER_ON)
	{
	  if (slot_power_on_flag)
	    {
	      ipmipower_output (IPMIPOWER_MSG_TYPE_OK, ip->ic->hostname, ip->extra_arg);
	      _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
	    }
	  else
	    _send_packet (ip, IPMIPOWER_PACKET_TYPE_C410X_SLOT_POWER_CONTROL_RQ);
	}
      else if (ip->cmd == IPMIPOWER_POWER_CMD_POWER_OFF)
	{
	  if (!slot_power_on_flag)
	    {
	      ipmipower_output (IPMIPOWER_MSG_TYPE_OK, ip->ic->hostname, ip->extra_arg);
	      _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
	    }
	  else
	    _send_packet (ip, IPMIPOWER_PACKET_TYPE_C410X_SLOT_POWER_CONTROL_RQ);
	}
      else
        {
          IPMIPOWER_ERROR (("_process_ipmi_packets: invalid command state: %d", ip->cmd));
          exit (EXIT_FAILURE);
        }
    }
  else if (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_C410X_SLOT_POWER_CONTROL_SENT)
    {
      if ((rv = _recv_packet (ip, IPMIPOWER_PACKET_TYPE_C410X_SLOT_POWER_CONTROL_RS)) != 1)
        {
          if (rv < 0)
            /* Session is up, so close it */
            _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
          goto done;
        }

      if ((cmd_args.wait_until_on
           && ip->cmd == IPMIPOWER_POWER_CMD_POWER_ON)
          || (cmd_args.wait_until_off
              && ip->cmd == IPMIPOWER_POWER_CMD_POWER_OFF))
        {
          if (ip->cmd == IPMIPOWER_POWER_CMD_POWER_ON)
            ip->wait_until_on_state++;
          else
            ip->wait_until_off_state++;
          _send_packet (ip, IPMIPOWER_PACKET_TYPE_C410X_GET_SENSOR_READING_RQ);
        }
      else
        {
          ipmipower_output (IPMIPOWER_MSG_TYPE_OK, ip->ic->hostname, ip->extra_arg);
	  _send_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RQ);
        }
    }
  else if (ip->protocol_state == IPMIPOWER_PROTOCOL_STATE_CLOSE_SESSION_SENT)
    {
      /* achu: Note that it's possible we're timing out too early and
       * the close session response will still arrive.  It's no
       * matter.  If we are in non-interactive mode, the file
       * descriptor will be closed and the packet lost.  If we are in
       * interactive mode, the next power control command will call
       * 'ipmipower_connection_clear' and get rid of the packet if it
       * is sitting on a buf.
       */
      if (ip->close_timeout)
        {
          IPMIPOWER_DEBUG (("host = %s; p = %d; close session timeout, skip retransmission",
                            ip->ic->hostname,
                            ip->protocol_state));
          goto finish_up;
        }

      if (!_recv_packet (ip, IPMIPOWER_PACKET_TYPE_CLOSE_SESSION_RS))
        goto done;

      /* Regardless of packet error or success, finish up */
    finish_up:
      ip->protocol_state = IPMIPOWER_PROTOCOL_STATE_END;
      return (-1); /* don't goto done and calculate timeout */
    }
  else
    {
      IPMIPOWER_ERROR (("_process_ipmi_packets: invalid state: %d", ip->protocol_state));
      exit (EXIT_FAILURE);
    }

 done:
  if (gettimeofday (&cur_time, NULL) < 0)
    {
      IPMIPOWER_ERROR (("gettimeofday: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }
  timeval_add_ms (&(ip->time_begin), cmd_args.common_args.session_timeout, &end_time);
  timeval_sub (&end_time, &cur_time, &result);
  timeval_millisecond_calc (&result, &timeout);

  /* shorter timeout b/c of retransmission timeout */
  if ((ip->wait_until_on_state && ip->cmd == IPMIPOWER_POWER_CMD_POWER_ON)
      || (ip->wait_until_off_state && ip->cmd == IPMIPOWER_POWER_CMD_POWER_OFF))
    {
      int retransmission_wait_timeout = cmd_args.retransmission_wait_timeout * (1 + (ip->retransmission_count/cmd_args.retransmission_backoff_count));
      if (timeout > retransmission_wait_timeout)
        timeout = retransmission_wait_timeout;
    }
  else
    {
      int retransmission_timeout = cmd_args.common_args.retransmission_timeout * (1 + (ip->retransmission_count/cmd_args.retransmission_backoff_count));
      if (timeout > retransmission_timeout)
        timeout = retransmission_timeout;
    }

  return (timeout);
}

int
ipmipower_powercmd_process_pending (int *timeout)
{
  ListIterator itr;
  ipmipower_powercmd_t ip;
  int min_timeout = cmd_args.common_args.session_timeout;
  int num_pending;

  assert (pending);  /* did not run ipmipower_powercmd_setup() */
  assert (timeout);

  /* if there are no pending jobs, don't edit the timeout */
  if (list_is_empty (pending))
    return (0);

  /* If we have a fanout, powercmds should be executed "in order" on
   * this list.  So no need to iterate through this list twice.
   */

  if (!(itr = list_iterator_create (pending)))
    {
      IPMIPOWER_ERROR (("list_iterator_create: %s", strerror (errno)));
      exit (EXIT_FAILURE);
    }

  while ((ip = (ipmipower_powercmd_t)list_next (itr)))
    {
      int tmp_timeout = -1;

      if ((tmp_timeout = _process_ipmi_packets (ip)) < 0)
        {
	  if (cmd_args.oem_power_type == IPMIPOWER_OEM_POWER_TYPE_C410X)
	    {
	      if (ip->next)
		{
		  if (!list_append (add_to_pending, ip->next))
		    {
		      IPMIPOWER_ERROR (("list_append: %s", strerror (errno)));
		      exit (EXIT_FAILURE);
		    }

		  ip->next = NULL;
		}
	    }

          if (!list_delete (itr))
            {
              IPMIPOWER_ERROR (("list_delete"));
              exit (EXIT_FAILURE);
            }

          executing_count--;
          continue;
        }

      if (tmp_timeout < min_timeout)
        min_timeout = tmp_timeout;
    }
  list_iterator_destroy (itr);

  if (list_count (add_to_pending) > 0)
    {
      ListIterator addtoitr;

      if (!(addtoitr = list_iterator_create (add_to_pending)))
	{
	  IPMIPOWER_ERROR (("list_iterator_create: %s", strerror (errno)));
	  exit (EXIT_FAILURE);
	}
      while ((ip = list_next (addtoitr)))
	{
	  ipmipower_connection_clear (ip->ic);
	  if (!list_append (pending, ip))
	    {
	      IPMIPOWER_ERROR (("list_append: %s", strerror (errno)));
	      exit (EXIT_FAILURE);
	    }

	  if (!list_delete (addtoitr))
	    {
	      IPMIPOWER_ERROR (("list_delete"));
	      exit (EXIT_FAILURE);
	    }
	}

      list_iterator_destroy (addtoitr);

      /* If by chance all commands are going to the same host, then
       * the next powercmd will start after a default timeout.  We
       * don't want that.  We'll shorten the timeout to a
       * retransmission timeout so it appears more normal.
       */

      if (cmd_args.common_args.retransmission_timeout < min_timeout)
	min_timeout = cmd_args.common_args.retransmission_timeout;
    } 

  if (!(num_pending = list_count (pending)))
    ipmipower_output_finish ();

  /* If the last pending power control command finished, the timeout
   * is 0 to get the primary poll loop to "re-init" at the start of
   * the loop.
   */
  if (num_pending)
    *timeout = min_timeout;
  else
    *timeout = 0;
  return (num_pending);
}
