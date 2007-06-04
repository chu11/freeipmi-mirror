/* 
   ipmi-chassis-boot-option.h - IPMI Chassis Boot Option Information 
   
   Copyright (C) 2007 FreeIPMI Core Team
   
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

#ifndef _IPMI_CHASSIS_BOOT_OPTIONS_PARAM_SPEC_H
#define _IPMI_CHASSIS_BOOT_OPTIONS_PARAM_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

#define IPMI_CHASSIS_BOOT_OPTIONS_PARAM_SET_IN_PROGRESS                  0x0
#define IPMI_CHASSIS_BOOT_OPTIONS_PARAM_SERVICE_PARTITION_SELECTOR       0x1
#define IPMI_CHASSIS_BOOT_OPTIONS_PARAM_SERVICE_PARTITION_SCAN           0x2
#define IPMI_CHASSIS_BOOT_OPTIONS_BMC_BOOT_FLAG_VALID_BIT_CLEARING       0x3
#define IPMI_CHASSIS_BOOT_OPTIONS_PARAM_BOOT_INFO_ACKNOWLEDGE            0x4
#define IPMI_CHASSIS_BOOT_OPTIONS_PARAM_BOOT_FLAGS                       0x5
#define IPMI_CHASSIS_BOOT_OPTIONS_PARAM_BOOT_INITIATOR_INFO              0x6
#define IPMI_CHASSIS_BOOT_OPTIONS_PARAM_BOOT_INITIATOR_MAILBOX           0x7

#define IPMI_CHASSIS_BOOT_OPTIONS_PARAM_VALID_UNLOCKED                   0x0
#define IPMI_CHASSIS_BOOT_OPTIONS_PARAM_INVALID_LOCKED                   0x1

/* Add +1 and -1 to avoid compiler warnings */
#define IPMI_CHASSIS_BOOT_OPTIONS_PARAM_SELECTOR_VALID(__param_selector) \
        ((((__param_selector + 1)) > IPMI_CHASSIS_BOOT_OPTIONS_PARAM_SET_IN_PROGRESS && \
          ((__param_selector - 1)) < IPMI_CHASSIS_BOOT_OPTIONS_PARAM_BOOT_INITIATOR_MAILBOX) ? 1 : 0)

#define IPMI_CHASSIS_BOOT_OPTIONS_NO_SET_SELECTOR                         0x0
#define IPMI_CHASSIS_BOOT_OPTIONS_SET_IN_PROGRESS_SET_SELCTOR_MAX         0x1
#define IPMI_CHASSIS_BOOT_OPTIONS_SERVICE_PARTITION_SELECTOR_SET_SELECTOR_MAX                                                                           0x1
#define IPMI_CHASSIS_BOOT_OPTIONS_SERVICE_PARTITION_SCAN_SET_SELECTOR_MAX 0x1
#define IPMI_CHASSIS_BOOT_OPTIONS_BMC_BOOT_FLAG_VALID_BIT_CLEARING_SET_SELECTOR_MAX                                                                     0x1
#define IPMI_CHASSIS_BOOT_OPTIONS_BOOT_INFO_ACKNOWLEDGE_SET_SELECTOR_MAX  0x2
#define IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAGS_SET_SELECTOR_MAX             0x5
#define IPMI_CHASSIS_BOOT_OPTIONS_BOOT_INITIATOR_INFO_SET_SELECTOR_MAX    0x9
#define IPMI_CHASSIS_BOOT_OPTIONS_BOOT_INITIATOR_MAILBOX_SET_SELECTOR_MAX 0x2 

#define IPMI_CHASSIS_BOOT_OPTIONS_NO_BLOCK_SELECTOR                       0x0

#define IPMI_CHASSIS_BOOT_OPTIONS_SET_IN_PROGRESS_SET_SELECTOR_VALID(__set_selector) \
  (((__set_selector) >= IPMI_CHASSIS_BOOT_OPTIONS_NO_SET_SELECTOR && \
    (__set_selector) <= IPMI_CHASSIS_BOOT_OPTIONS_SET_IN_PROGRESS_SET_SELECTOR_MAX) ? 1 : 0)

#define IPMI_CHASSIS_BOOT_OPTIONS_SERVICE_PARTITION_SELECTOR_SET_SELECTOR_VALID(__set_selector) \
  (((__set_selector) >= IPMI_CHASSIS_BOOT_OPTIONS_NO_SET_SELECTOR && \
    (__set_selector) <= IPMI_CHASSIS_BOOT_OPTIONS_SERVICE_PARTITION_SELECTOR_SET_SELECTOR_MAX) ? 1 : 0)

#define IPMI_CHASSIS_BOOT_OPTIONS_SERVICE_PARTITION_SCAN_SET_SELECTOR_VALID(__set_selector) \
  (((__set_selector) >= IPMI_CHASSIS_BOOT_OPTIONS_NO_SET_SELECTOR && \
    (__set_selector) <= IPMI_CHASSIS_BOOT_OPTIONS_SERVICE_PARTITION_SCAN_SET_SELECTOR_MAX) ? 1 : 0)

#define IPMI_CHASSIS_BOOT_OPTIONS_BMC_BOOT_FLAG_VALID_BIT_CLEARING_SET_SELECTOR_VALID(__set_selector) \
  (((__set_selector) >= IPMI_CHASSIS_BOOT_OPTIONS_NO_SET_SELECTOR && \
    (__set_selector) <= IPMI_CHASSIS_BOOT_OPTIONS_BMC_BOOT_FLAG_VALID_BIT_CLEARING_SET_SELECTOR_MAX) ? 1 : 0)

#define IPMI_CHASSIS_BOOT_OPTIONS_BOOT_INFO_ACKNOWLEDGE_SET_SELECTOR_VALID(__set_selector) \
  (((__set_selector) >= IPMI_CHASSIS_BOOT_OPTIONS_NO_SET_SELECTOR && \
    (__set_selector) <= IPMI_CHASSIS_BOOT_OPTIONS_BOOT_INFO_ACKNOWLEDGE_SET_SELECTOR_MAX) ? 1 : 0)

#define IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAGS_SET_SELECTOR_VALID(__set_selector) \
  (((__set_selector + 1 ) > IPMI_CHASSIS_BOOT_OPTIONS_NO_SET_SELECTOR && \
    (__set_selector - 1 ) < IPMI_CHASSIS_BOOT_OPTIONS_BOOT_FLAGS_SET_SELECTOR_MAX) ? 1 : 0)

#define IPMI_CHASSIS_BOOT_OPTIONS_BOOT_INITIATOR_INFO_SET_SELECTOR_VALID(__set_selector) \
  (((__set_selector) >= IPMI_CHASSIS_BOOT_OPTIONS_NO_SET_SELECTOR && \
    (__set_selector) <= IPMI_CHASSIS_BOOT_OPTIONS_BOOT_INITIATOR_INFO_SET_SELECTOR_MAX) ? 1 : 0)

#define IPMI_CHASSIS_BOOT_OPTIONS_BOOT_INITIATOR_MAILBOX_SET_SELECTOR_VALID(__set_selector) \
  (((__set_selector) >= IPMI_CHASSIS_BOOT_OPTIONS_NO_SET_SELECTOR && \
    (__set_selector) <= IPMI_CHASSIS_BOOT_OPTIONS_BOOT_INITIATOR_MAILBOX_SET_SELECTOR_MAX) ? 1 : 0)

#define IPMI_CHASSIS_BOOT_OPTIONS_BLOCK_SELECTOR_VALID(__block_selector) \
  (((__block_selector + 1 ) == ( IPMI_CHASSIS_BOOT_OPTIONS_NO_BLOCK_SELECTOR + 1) ) ? 1 : 0)

#ifdef __cplusplus
}
#endif
#endif /* _IPMI_CHASSIS_BOOT_OPTIONS_PARAM_SPEC_H */