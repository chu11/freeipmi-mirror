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

#ifndef IPMI_SENSOR_AND_EVENT_CODE_TABLES_OEM_INTEL_COMMON_SPEC_H
#define IPMI_SENSOR_AND_EVENT_CODE_TABLES_OEM_INTEL_COMMON_SPEC_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Intel S5500WB/Penguin Computing Relion 700
 */
/*
 * Event Reading Type Code = IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_SENSOR
 * Sensor Type = IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
 */
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_SENSOR_RECEIVER_ERROR       0x00
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_SENSOR_BAD_DLLP             0x01
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_SENSOR_BAD_TLLP             0x02
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_SENSOR_REPLAY_NUM_ROLLOVER  0x03
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_SENSOR_REPLAY_TIMER_TIMEOUT 0x04
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_SENSOR_ADVISORY_NON_FATAL   0x05
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_SENSOR_LINK_BW_CHANGED      0x06

/*
 * String arrays for above
 */

extern const char * const ipmi_oem_intel_specific_pci_correctable_sensor[];
extern unsigned int ipmi_oem_intel_specific_pci_correctable_sensor_max_index;

/*
 * Intel S2600JF/Appro 512X
 * Intel S2600WP
 * Intel S2600KP
 * Intel S2600WT2
 * Intel S2600WTT
 * Intel S2600GZ
 */
/*
 * Event Reading Type Code = IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_ERROR
 * Sensor Type = IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
 */
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_DATA_LINK_LAYER_PROTOCOL_ERROR 0x00
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_SURPRISE_LINK_DOWN_ERROR       0x01
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_COMPLETER_ABORT                0x02
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_UNSUPPORTED_REQUEST            0x03
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_POISONED_TLP                   0x04
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_FLOW_CONTROL_PROTOCOL          0x05
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_COMPLETION_TIMEOUT             0x06
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_RECEIVER_BUFFER_OVERFLOW       0x07
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_ACS_VIOLATION                  0x08
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_MALFORMED_TLP                  0x09
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_ECRC_ERROR                     0x0A
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_RECEIVED_FATAL_MESSAGE         0x0B
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_UNEXPECTED_COMPLETION          0x0C
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_RECEIVED_NON_FATAL_MESSAGE     0x0D
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_UNCORRECTABLE_INTERNAL         0x0E
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_MC_BLOCKED_TLP                 0x0F

/*
 * Event Reading Type Code = IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_FATAL_ERROR_2
 * Sensor Type = IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
 */
/* achu: not a typo, why a from from 0x01 to 0x0f? */
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_2_ATOMIC_EGRESS_BLOCKED           0x00
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_2_TLP_PREFIX_BLOCKED              0x01
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_FATAL_ERROR_2_UNSPECIFIED_NON_AER_FATAL_ERROR 0x0F

/*
 * Event Reading Type Code = IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_PCIE_CORRECTABLE_ERROR
 * Sensor Type = IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
 */
/* achu: Similar to S5500WB, but some events text changed and new bitmasks, so new macros to differentiate */
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_ERROR_RECEIVER_ERROR       0x00
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_ERROR_BAD_DLLP             0x01
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_ERROR_BAD_TLP              0x02
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_ERROR_REPLAY_NUM_ROLLOVER  0x03
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_ERROR_REPLAY_TIMER_TIMEOUT 0x04
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_ERROR_ADVISORY_NON_FATAL   0x05
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_ERROR_LINK_BW_CHANGED      0x06
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_ERROR_CORRECTABLE_INTERNAL 0x07
#define IPMI_OEM_INTEL_SPECIFIC_PCIE_CORRECTABLE_ERROR_HEADER_LOG_OVERFLOW  0x08

/*
 * Event Reading Type Code = IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_FATAL_ERROR
 * Sensor Type = IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
 */
/* achu: earlier code "OPI", assumed typo for "QPI" */
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_LINK_LAYER_UNCORRECTABLE_ECC_ERROR             0x00
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_PROTOCOL_LAYER_POISONED_PACKET_RECEPTION_ERROR 0x01
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_LINK_PHY_INIT_FAILURE                          0x02
/* achu: earlier S2600JF / S2600WP implementatino prefixed next four with CSI */
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_PHY_LAYER_DETECTED_DRIFT_BUFFER_ALARM          0x03
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_PHY_DETECTED_LATENCY_BUFFER_ROLLOVER           0x04
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_PHY_INIT_FAILURE                               0x05
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_LINK_LAYER_GENERIC_CONTROL_ERROR               0x06
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_PARITY_ERROR_IN_LINK_OR_PHY_LAYER              0x07
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_PROTOCOL_LAYER_TIMEOUT_DETECTED                0x08
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_PROTOCOL_LAYER_FAILED_RESPONSE                 0x09
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_PROTOCOL_LAYER_ILLEGAL_PACKET_FIELD            0x0A
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_PROTOCOL_LAYER_QUEUE_TABLE_OVERFLOW_UNDERFLOW  0x0B
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_VIRAL_ERROR                                    0x0C
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_PROTOCOL_LAYER_PARITY_ERROR                    0x0D
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_ROUTING_TABLE_ERROR                            0x0E

/*
 * Event Reading Type Code = IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_FATAL_ERROR_2
 * Sensor Type = IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
 */
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_2_ILLEGAL_INBOUND_REQUEST                                      0x00
/* achu: earlier code used "OPI", assumed typo for "QPI" */
#if 0
/* achu: On S2600JF docs, Intel informed me there was an error in
 * their documentation and the following was not correct.  I'll leave
 * this here for legacy documentation
 */
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_2_PCH_WRITE_CACHE_UNCORRECTABLE_DATA_ECC_ERROR                 0x01
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_2_PCH_WRITE_CACHE_UNCORRECTABLE_DATA_ECC_ERROR_2               0x02
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_2_PCH_WRITE_CACHE_UNCORRECTABLE_DATA_ECC_ERROR_3               0x03
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_2_PCH_RECEIVED_XPF_PHYSICAL_LOGICAL_REDIRECT_INTERRUPT_INBOUND 0x04
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_2_PCH_ILLEGAL_SAD_OR_ILLEGAL_OR_NON_EXISTENT_ADDRESS_OR_MEMORY 0x05
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_2_PCH_WRITE_CACHE_COHERENCY_VIOLATION                          0x06
#else  /* !0 */
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_2_IIO_WRITE_CACHE_UNCORRECTABLE_DATA_ECC_ERROR                 0x01
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_2_IIO_CSR_CROSSING_32_BIT_BOUNDARY_ERROR                       0x02
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_2_IIO_RECEIVED_XPF_PHYSICAL_LOGICAL_REDIRECT_INTERRUPT_INBOUND 0x03
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_2_IIO_ILLEGAL_SAD_OR_ILLEGAL_OR_NON_EXISTENT_ADDRESS_OR_MEMORY 0x04
#define IPMI_OEM_INTEL_SPECIFIC_QPI_FATAL_ERROR_2_IIO_WRITE_CACHE_COHERENCY_VIOLATION                          0x05
#endif  /* !0 */

/*
 * Event Reading Type Code = IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_QPI_LINK_WIDTH_REDUCED
 * Sensor Type = IPMI_SENSOR_TYPE_CRITICAL_INTERRUPT
 */
/* achu: not a typo, no 0x00 offset */
#define IPMI_OEM_INTEL_SPECIFIC_QPI_LINK_WIDTH_REDUCED_HALF_WIDTH    0x01
#define IPMI_OEM_INTEL_SPECIFIC_QPI_LINK_WIDTH_REDUCED_QUARTER_WIDTH 0x02

/*
 * String arrays for above
 */

/* achu: in earlier code S2600JF labeled "opi", assumed typo and is now "qpi" */
/* achu: in earlier code S2600JF labeled "pci", now is "pcie" */
extern const char * const ipmi_oem_intel_specific_pcie_fatal_error[];
extern unsigned int ipmi_oem_intel_specific_pcie_fatal_error_max_index;

extern const char * const ipmi_oem_intel_specific_pcie_fatal_error_2[];
extern unsigned int ipmi_oem_intel_specific_pcie_fatal_error_2_max_index;

extern const char * const ipmi_oem_intel_specific_pcie_correctable_error[];
extern unsigned int ipmi_oem_intel_specific_pcie_correctable_error_max_index;

extern const char * const ipmi_oem_intel_specific_qpi_fatal_error[];
extern unsigned int ipmi_oem_intel_specific_qpi_fatal_error_max_index;

extern const char * const ipmi_oem_intel_specific_qpi_fatal_error_2[];
extern unsigned int ipmi_oem_intel_specific_qpi_fatal_error_2_max_index;

extern const char * const ipmi_oem_intel_specific_qpi_link_width_reduced[];
extern unsigned int ipmi_oem_intel_specific_qpi_link_width_reduced_max_index;

/*
 * Intel S2600WP
 * Intel S2600KP
 * Intel S2600WT2
 * Intel S2600WTT
 * Intel S2600GZ
 */

/*
 * Event Reading Type Code = IPMI_EVENT_READING_TYPE_CODE_OEM_INTEL_FIRMWARE_UPDATE_STATUS_SENSOR
 * Sensor Type = IPMI_SENSOR_TYPE_VERSION_CHANGE
 */
/* achu: not a typo, no 0x00 offset */
#define IPMI_OEM_INTEL_SPECIFIC_FIRMWARE_UPDATE_STATUS_SENSOR_UPDATE_STARTED                0x00
#define IPMI_OEM_INTEL_SPECIFIC_FIRMWARE_UPDATE_STATUS_SENSOR_UPDATE_COMPLETED_SUCCESSFULLY 0x01
#define IPMI_OEM_INTEL_SPECIFIC_FIRMWARE_UPDATE_STATUS_SENSOR_UPDATE_FAILURE                0x02

/*
 * String arrays for above
 */

extern const char * const ipmi_oem_intel_specific_firmware_update_status_sensor[];
extern unsigned int ipmi_oem_intel_specific_firmware_update_status_sensor_max_index;

/*
 * Intel S5500WB/Penguin Computing Relion 700
 * Quanta QSSC-S4R/Appro GB812X-CN
 * (Quanta motherboard contains Intel manufacturer ID)
 * Intel S2600JF/Appro 512X
 * Intel S2600WP
 */

#define IPMI_OEM_INTEL_EVENT_DATA3_DEVICE_NUMBER_BITMASK   0xF8
#define IPMI_OEM_INTEL_EVENT_DATA3_DEVICE_NUMBER_SHIFT     3

#define IPMI_OEM_INTEL_EVENT_DATA3_FUNCTION_NUMBER_BITMASK 0x07
#define IPMI_OEM_INTEL_EVENT_DATA3_FUNCTION_NUMBER_SHIFT   0

#ifdef __cplusplus
}
#endif

#endif /* IPMI_SENSOR_AND_EVENT_CODE_TABLES_OEM_INTEL_COMMON_SPEC_H */
