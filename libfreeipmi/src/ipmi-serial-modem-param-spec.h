/* 
   ipmi-serial-modem-param-spec.h - IPMI Serial/Modem parameters spec

   Copyright (C) 2005 FreeIPMI Core Team

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

#ifndef _IPMI_SERIAL_MODEM_PARAM_SPEC_H
#define _IPMI_SERIAL_MODEM_PARAM_SPEC_H

#define IPMI_SERIAL_PARAM_SET_IN_PROGRESS                       0
#define IPMI_SERIAL_PARAM_AUTH_TYPE_SUPPORT                     1
#define IPMI_SERIAL_PARAM_AUTH_TYPE_ENABLES                     2
#define IPMI_SERIAL_PARAM_CONNECTION_MODE                       3
#define IPMI_SERIAL_PARAM_SESSION_INACTIVITY_TIMEOUT            4
#define IPMI_SERIAL_PARAM_CHANNEL_CALLBACK_CONTROL              5
#define IPMI_SERIAL_PARAM_SESSION_TERMINATION                   6
#define IPMI_SERIAL_PARAM_COMM_BITS                             7
#define IPMI_SERIAL_PARAM_MUX_SWITCH_CONTROL                    8
#define IPMI_SERIAL_PARAM_MODEM_RING_TIME                       9
#define IPMI_SERIAL_PARAM_MODEM_INIT_STRING                     10
#define IPMI_SERIAL_PARAM_MODEM_ESCAPE_SEQUENCE                 11
#define IPMI_SERIAL_PARAM_MODEM_HANG_UP_SEQUENCE                12
#define IPMI_SERIAL_PARAM_MODEM_DIAL_COMMAND                    13
#define IPMI_SERIAL_PARAM_PAGE_BLACKOUT_INTERVAL                14
#define IPMI_SERIAL_PARAM_COMMUNITY_STRING                      15
#define IPMI_SERIAL_PARAM_NO_OF_ALERT_DESTINATIONS              16
#define IPMI_SERIAL_PARAM_DESTINATION_INFO                      17
#define IPMI_SERIAL_PARAM_RETRY_TIME                            18
#define IPMI_SERIAL_PARAM_DESTINATION_COMM_SETTINGS             19
#define IPMI_SERIAL_PARAM_NO_OF_DIAL_STRINGS                    20
#define IPMI_SERIAL_PARAM_DESTINATION_DIAL_STRINGS              21
#define IPMI_SERIAL_PARAM_NO_OF_ALERT_DESTINATION_IP_ADDRS      22
#define IPMI_SERIAL_PARAM_DESTINATION_IP_ADDRS                  23
#define IPMI_SERIAL_PARAM_NO_OF_TAP_ACCOUNTS                    24
#define IPMI_SERIAL_PARAM_TAP_ACCOUNT                           25
#define IPMI_SERIAL_PARAM_TAP_PASSWORDS                         26
#define IPMI_SERIAL_PARAM_TAP_PAGER_ID_STRINGS                  27
#define IPMI_SERIAL_PARAM_TAP_SERVICE_SETTINGS                  28
#define IPMI_SERIAL_PARAM_TERMINAL_MODE_CONF                    29
#define IPMI_SERIAL_PARAM_PPP_PROTOCOL_OPTIONS                  30
#define IPMI_SERIAL_PARAM_PPP_PRIMARY_RMCP_PORT_NO              31
#define IPMI_SERIAL_PARAM_PPP_SECONDARY_RMCP_PORT_NO            32
#define IPMI_SERIAL_PARAM_PPP_LINK_AUTH                         33
#define IPMI_SERIAL_PARAM_CHAP_NAME                             34
#define IPMI_SERIAL_PARAM_PPP_ACCM                              35
#define IPMI_SERIAL_PARAM_PPP_SNOOP_ACCM                        36
#define IPMI_SERIAL_PARAM_NO_OF_PPP_ACCOUNT                     37
#define IPMI_SERIAL_PARAM_PPP_ACCOUNT_DIAL_STRING_SELECTOR      38
#define IPMI_SERIAL_PARAM_PPP_ACCOUNT_BMC_IP_ADDRESSES          39
#define IPMI_SERIAL_PARAM_PPP_ACCOUNT_USER_NAMES                40
#define IPMI_SERIAL_PARAM_PPP_ACCOUNT_USER_DOMAINS              41
#define IPMI_SERIAL_PARAM_PPP_ACCOUNT_USER_PASSWORDS            42
#define IPMI_SERIAL_PARAM_PPP_ACCOUNT_AUTH_SETTINGS             43
#define IPMI_SERIAL_PARAM_PPP_ACCOUNT_CONNECTION_HOLD_TIMES     44
#define IPMI_SERIAL_PARAM_PPP_UDP_PROXY_IP_HEADER_DATA          45
#define IPMI_SERIAL_PARAM_PPP_UDP_PROXY_TRANSMIT_BUFFER_SIZE    46
#define IPMI_SERIAL_PARAM_PPP_UDP_PROXY_RECEIVE_BUFFER_SIZE     47
#define IPMI_SERIAL_PARAM_PPP_REMOTE_CONSOLE_IP_ADDRESS         48

#endif
