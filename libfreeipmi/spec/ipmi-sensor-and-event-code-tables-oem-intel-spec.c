/*
 * Copyright (C) 2003-2015 FreeIPMI Core Team
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#ifdef STDC_HEADERS
#include <string.h>
#endif /* STDC_HEADERS */
#include <sys/types.h>
#include <assert.h>
#include <errno.h>

#include "freeipmi/spec/oem/ipmi-sensor-and-event-code-tables-oem-intel-spec.h"
#include "freeipmi/fiid/fiid.h"

#include "libcommon/ipmi-fiid-util.h"
#include "libcommon/ipmi-trace.h"

#include "freeipmi-portability.h"

/*
 * Intel S5500WB/Penguin Computing Relion 700
 */

const char * const ipmi_oem_intel_specific_pci_fatal_sensor[] =
  {
    "Data Link Layer Protocol Error",
    "Surprise Link Down",
    "Unexpected Completion",
    "Received Unsupported request condition on inbound address decode with exception of SAD",
    "Poisoned TLP Error",
    "Flow Control Protocol Error",
    "Completion Timeout Error",
    "Completer Abort Error",
    "Receiver Buffer Overflow Error",
    "ACS Violation Error",
    "Malformed TLP Error",
    "Received ERR_FATAL Message From Downstream Error",
    "Unexpected Completion",    /* not a typo, identical to above */
    "Received ERR_NONFATAL Message Error",
    NULL
  };
unsigned int ipmi_oem_intel_specific_pci_fatal_sensor_max_index = 0x0D;

const char * const ipmi_oem_intel_specific_pci_correctable_sensor[] =
  {
    "Receiver Error",
    "Bad DLLP Error",
    "Bad TLLP Error",
    "REPLAY_NUM Rollover Error",
    "REPLAY Timer Timeout Error",
    "Advisory Non-fatal Error (Received ERR_COR message)",
    "Link Bandwidth Changed (ECN) Error",
    NULL
  };
unsigned int ipmi_oem_intel_specific_pci_correctable_sensor_max_index = 0x06;

/*
 * Quanta QSSC-S4R/Appro GB812X-CN
 * (Quanta motherboard contains Intel manufacturer ID)
 */

/* achu: Similar to above, but some events text changed, so new arrays to differentiate */

const char * const ipmi_oem_intel_quanta_qssc_s4r_specific_pci_fatal_sensor[] =
  {
    "Data Link Layer Protocol Error",
    "Surprise Link Down",
    "Unexpected Completer",
    "Received Unsupported request condition on inbound address decode with exception of SAD",
    "Poisoned TLP Error",
    "Flow Control Protocol Error",
    "Completion Timeout Error",
    "Completer Abort Error",
    "Receiver Buffer Overflow Error",
    "ACS Violation Error",
    "Malformed TLP Error",
    "Received ERR_FATAL Message From Downstream Error",
    "Unexpected Completion Error",
    "Received ERR_NONFATAL Message Error",
    NULL
  };
unsigned int ipmi_oem_intel_quanta_qssc_s4r_specific_pci_fatal_sensor_max_index = 0x0D;

const char * const ipmi_oem_intel_quanta_qssc_s4r_specific_pci_correctable_sensor[] =
  {
    "Receiver Error",
    "Bad DLLP Error",
    "Bad TLLP Error",
    "REPLAY_NUM Rollover Error",
    "REPLAY Timer Timeout Error",
    "Advisory Non-fatal Error (Received ERR_COR message)",
    "Link Bandwidth Changed (ECN) Error",
    NULL
  };
unsigned int ipmi_oem_intel_quanta_qssc_s4r_specific_pci_correctable_sensor_max_index = 0x06;

/*
 * Intel S2600JF/Appro 512X
 */

/* achu: Similar to above, but some events text changed and new ones,
 * so new arrays to differentiate */

const char * const ipmi_oem_intel_s2600jf_specific_pci_fatal_error[] =
  {
    "Data Link Layer Protocol Error",
    "Surprise Link Down Error",
    "Completer Abort",
    "Unsupported Request",
    "Poisoned TLP",
    "Flow Control Protocol",
    "Completion Timeout",
    "Receiver Buffer Overflow",
    "ACS Violation Error",
    "Malformed TLP Error",
    "ECRC Error",
    "Received Fatal Message From Downstream",
    "Unexpected Completion",
    "Received ERR_NONFATAL Message",
    "Uncorrectable Internal",
    "MC Blocked TLP",
    NULL
  };
unsigned int ipmi_oem_intel_s2600jf_specific_pci_fatal_error_max_index = 0x0F;

const char * const ipmi_oem_intel_s2600jf_specific_pci_fatal_error_2[] =
  {
    "Atomic Egress Blocked",
    "TLP Prefix Blocked",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "Unspecified Non-AER Fatal Error",
    NULL
  };
unsigned int ipmi_oem_intel_s2600jf_specific_pci_fatal_error_2_max_index = 0x0F;

const char * const ipmi_oem_intel_s2600jf_specific_pci_correctable_error[] =
  {
    "Receiver Error",
    "Bad DLLP",
    "Bad TLP",
    "Replay Num Rollover",
    "Replay Timer timeout",
    "Advisory Non-fatal",
    "Link BW Changed",
    "Correctable Internal",
    "Header Log Overflow",
    NULL
  };
unsigned int ipmi_oem_intel_s2600jf_specific_pci_correctable_error_max_index = 0x08;

const char * const ipmi_oem_intel_s2600jf_specific_opi_fatal_error[] =
  {
    "Link Layer Uncorrectable ECC Error",
    "Protocol Layer Poisoned Packet Reception Error",
    "LINK/PHY Init Failure with resultant degradation in link width",
    "CSI PHY Layer detected drift buffer alarm",
    "CSI PHY detected latency buffer rollover",
    "CSI PHY Init Failure",
    "CSI Link Layer generic control error (buffer overflow/underflow, credit underflow and so on.)",
    "Parity error in link or PHY layer",
    "Protocol layer timeout detected",
    "Protocol layer failed response",
    "Protocol layer illegal packet field, target Node ID and so on.",
    "Protocol Layer Queue/table overflow/underflow",
    "Viral Error",
    "Protocol Layer parity error",
    "Routing Table Error",
    NULL
  };
unsigned int ipmi_oem_intel_s2600jf_specific_opi_fatal_error_max_index = 0x0E;

#if 0
/* achu: Intel informed me there was an error in their documentation and the following was not correct.
 * I'll leave this here for legacy documentation
 */
const char * const ipmi_oem_intel_s2600jf_specific_opi_fatal_error_2[] =
  {
    "Illegal inbound request",
    "PCH Write Cache Uncorrectable Data ECC Error",
    "PCH Write Cache Uncorrectable Data ECC Error", /* same not typo, typo in spec? */
    "PCH Write Cache Uncorrectable Data ECC Error", /* same not typo, typo in spec? */
    "PCH Received XPF physical/logical redirect interrupt inbound",
    "PCH Illegal SAD or Illegal or non-existent address or memory",
    "PCH Write Cache Coherency Violation",
    NULL
  };
unsigned int ipmi_oem_intel_s2600jf_specific_opi_fatal_error_2_max_index = 0x06;
#else  /* !0 */
const char * const ipmi_oem_intel_s2600jf_specific_opi_fatal_error_2[] =
  {
    "Illegal inbound request",
    "IIO Write Cache Uncorrectable Data ECC Error",
    "IIO CSR crossing 32-bit boundary Error",
    "IIO Received XPF physical/logical redirect interrupt inbound",
    "IIO Illegal SAD or Illegal or non-existent address or memory",
    "IIO Write Cache Coherency Violation",
    NULL
  };
unsigned int ipmi_oem_intel_s2600jf_specific_opi_fatal_error_2_max_index = 0x05;
#endif  /* !0 */

const char * const ipmi_oem_intel_s2600jf_specific_qpi_link_width_reduced[] =
  {
    "reserved",
    "Reduced to 1/2 width",
    "Reduced to 1/4 width",
    NULL
  };
unsigned int ipmi_oem_intel_s2600jf_specific_qpi_link_width_reduced_max_index = 0x02;

/*
 * Intel S2600WP
 */

/* achu: Similar to above, but some events text changed and new ones,
 * so new arrays to differentiate */

const char * const ipmi_oem_intel_s2600wp_specific_pcie_fatal_error[] =
  {
    "Data Link Layer Protocol Error",
    "Surprise Link Down Error",
    "Completer Abort",
    "Unsupported Request",
    "Poisoned TLP",
    "Flow Control Protocol",
    "Completion Timeout",
    "Receiver Buffer Overflow",
    "ACS Violation Error",
    "Malformed TLP Error",
    "ECRC Error",
    "Received Fatal Message From Downstream",
    "Unexpected Completion",
    "Received ERR_NONFATAL Message",
    "Uncorrectable Internal",
    "MC Blocked TLP",
    NULL
  };
unsigned int ipmi_oem_intel_s2600wp_specific_pcie_fatal_error_max_index = 0x0F;

const char * const ipmi_oem_intel_s2600wp_specific_pcie_fatal_error_2[] =
  {
    "Atomic Egress Blocked",
    "TLP Prefix Blocked",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "Unspecified Non-AER Fatal Error",
    NULL
  };
unsigned int ipmi_oem_intel_s2600wp_specific_pcie_fatal_error_2_max_index = 0x0F;

const char * const ipmi_oem_intel_s2600wp_specific_pcie_correctable_error[] =
  {
    "Receiver Error",
    "Bad DLLP",
    "Bad TLP",
    "Replay Num Rollover",
    "Replay Timer timeout",
    "Advisory Non-fatal",
    "Link BW Changed",
    "Correctable Internal",
    "Header Log Overflow",
    NULL
  };
unsigned int ipmi_oem_intel_s2600wp_specific_pcie_correctable_error_max_index = 0x08;

const char * const ipmi_oem_intel_s2600wp_specific_qpi_fatal_error[] =
  {
    "Link Layer Uncorrectable ECC Error",
    "Protocol Layer Poisoned Packet Reception Error",
    "LINK/PHY Init Failure with resultant degradation in link width",
    "CSI PHY Layer detected drift buffer alarm",
    "CSI PHY detected latency buffer rollover",
    "CSI PHY Init Failure",
    "CSI Link Layer generic control error (buffer overflow/underflow, credit underflow and so on.)",
    "Parity error in link or PHY layer",
    "Protocol layer timeout detected",
    "Protocol layer failed response",
    "Protocol layer illegal packet field, target Node ID and so on.",
    "Protocol Layer Queue/table overflow/underflow",
    "Viral Error",
    "Protocol Layer parity error",
    "Routing Table Error",
    NULL
  };
unsigned int ipmi_oem_intel_s2600wp_specific_qpi_fatal_error_max_index = 0x0E;

const char * const ipmi_oem_intel_s2600wp_specific_qpi_fatal_error_2[] =
  {
    "Illegal inbound request",
    "IIO Write Cache Uncorrectable Data ECC Error",
    "IIO CSR crossing 32-bit boundary Error",
    "IIO Received XPF physical/logical redirect interrupt inbound",
    "IIO Illegal SAD or Illegal or non-existent address or memory",
    "IIO Write Cache Coherency Violation",
    NULL
  };
unsigned int ipmi_oem_intel_s2600wp_specific_qpi_fatal_error_2_max_index = 0x05;

const char * const ipmi_oem_intel_s2600wp_specific_qpi_link_width_reduced[] =
  {
    "reserved",
    "Reduced to 1/2 width",
    "Reduced to 1/4 width",
    NULL
  };
unsigned int ipmi_oem_intel_s2600wp_specific_qpi_link_width_reduced_max_index = 0x02;

const char * const ipmi_oem_intel_s2600wp_specific_firmware_update_status_sensor[] =
  {
    "Update started",
    "Update completed successfully",
    "Update failure",
    NULL
  };

unsigned int ipmi_oem_intel_s2600wp_specific_firmware_update_status_sensor_max_index = 0x02;

/*
 * Intel S2600KP
 * Intel S2600WT2
 * Intel S2600WTT
 * Intel S2600GZ
 */

const char * const ipmi_oem_intel_e52600v3_specific_qpi_fatal_error[] =
  {
    "Link Layer Uncorrectable ECC Error",
    "Protocol Layer Poisoned Packet Reception Error",
    "LINK/PHY Init Failure with resultant degradation in link width",
    "PHY Layer detected drift buffer alarm",
    "PHY detected latency buffer rollover",
    "PHY Init Failure",
    "Link Layer generic control error (buffer overflow/underflow, credit underflow, and so on.)",
    "Parity error in link or PHY layer",
    "Protocol layer timeout detected",
    "Protocol layer failed response",
    "Protocol layer illegal packet field, target Node ID, and so on.",
    "Protocol Layer Queue/table overflow/underflow",
    "Viral Error",
    "Protocol Layer parity error",
    "Routing Table Error",
    NULL
  };
unsigned int ipmi_oem_intel_e52600v3_specific_qpi_fatal_error_max_index = 0x0E;

const char * const ipmi_oem_intel_e52600v3_specific_qpi_fatal_error_2[] =
  {
    "Illegal inbound request",
    "IIO Write Cache Uncorrectable Data ECC Error",
    "IIO CSR crossing 32-bit boundary Error",
    "IIO Received XPF physical/logical redirect interrupt inbound",
    "IIO Illegal SAD or Illegal or non-existent address or memory",
    "IIO Write Cache Coherency Violation",
    NULL
  };
unsigned int ipmi_oem_intel_e52600v3_specific_qpi_fatal_error_2_max_index = 0x05;

const char * const ipmi_oem_intel_e52600v3_specific_qpi_link_width_reduced[] =
  {
    "reserved",
    "Reduced to 1/2 width",
    "Reduced to 1/4 width",
    NULL
  };
unsigned int ipmi_oem_intel_e52600v3_specific_qpi_link_width_reduced_max_index = 0x02;

const char * const ipmi_oem_intel_e52600v3_specific_pci_express_fatal_errors[] =
  {
    "Data Link Layer Protocol Error",
    "Surprise Link Down Error",
    "Completer Abort",
    "Unsupported Request",
    "Poisoned TLP",
    "Flow Control Protocol",
    "Completion Timeout",
    "Receiver Buffer Overflow",
    "ACS Violation",
    "Malformed TLP",
    "ECRC Error",
    "Received Fatal Message From Downstream",
    "Unexpected Completion",
    "Received ERR_NONFATAL Message",
    "Uncorrectable Internal",
    "MC Blocked TLP",
    NULL
  };
unsigned int ipmi_oem_intel_e52600v3_specific_pci_express_fatal_errors_max_index = 0x0F;

const char * const ipmi_oem_intel_e52600v3_specific_pci_express_fatal_errors_2[] =
  {
    "Atomic Egress Blocked",
    "TLP Prefix Blocked",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "reserved",
    "Unspecified Non-AER Fatal Error",
    NULL
  };
unsigned int ipmi_oem_intel_e52600v3_specific_pci_express_fatal_errors_2_max_index = 0x0F;

const char * const ipmi_oem_intel_e52600v3_specific_pci_express_correctable_errors[] =
  {
    "Receiver Error",
    "Bad DLLP",
    "Bad TLP",
    "Replay Num Rollover",
    "Replay Timer timeout",
    "Advisory Non-fatal",
    "Link BW Changed",
    "Correctable Internal",
    "Header Log Overflow",
    NULL
  };
unsigned int ipmi_oem_intel_e52600v3_specific_pci_express_correctable_errors_max_index = 0x08;


const char * const ipmi_oem_intel_e52600v3_specific_firmware_update_status_sensor[] =
  {
    "Update started",
    "Update completed successfully",
    "Update failure",
    NULL
  };

unsigned int ipmi_oem_intel_e52600v3_specific_firmware_update_status_sensor_max_index = 0x02;

const char * const ipmi_oem_intel_e52600v3_specific_bios_recovery_start[] =
  {
    "reserved",
    "BIOS Recovery Start",
    NULL
  };

unsigned int ipmi_oem_intel_e52600v3_specific_bios_recovery_start_max_index = 0x01;

const char * const ipmi_oem_intel_e52600v3_specific_bios_recovery_finish[] =
  {
    "reserved",
    "BIOS Recovery Finish",
    NULL
  };

unsigned int ipmi_oem_intel_e52600v3_specific_bios_recovery_finish_max_index = 0x01;

const char * const ipmi_oem_intel_e52600v3_specific_ierr_recovery_dump_info[] =
  {
    "reserved",
    "Dump failed",
    NULL,
  };
unsigned int ipmi_oem_intel_e52600v3_specific_ierr_recovery_dump_info_max_index = 0x01;
