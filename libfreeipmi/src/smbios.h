/* SMBIOS Reference Specification: map area between 000f0000 and
   000fffff.  The IPMI Entry Structure begins on a 16-byte boundary,
   with a 4 byte "_SM_" signature.  */

#ifndef _SMBIOS_H

#define ipmi_minor          version.minor
#define ipmi_major          version.major
#define ipmi_membase        base.bmc_membase_addr
#define ipmi_iobase         base.bmc_iobase_addr
#define ipmi_int_specified  intr.intinfo_specified
#define ipmi_int_polarity   intr.intinfo_polarity_high
#define ipmi_int_trigger    intr.intinfo_trigger_level

#define SMBIOS_ENTRY_CSUM_OFFSET 	0x4
#define SMBIOS_ENTRY_LEN_OFFSET 	0x5
#define SMBIOS_ENTRY_ANCHOR_OFFSET 	0x10
#define SMBIOS_ENTRY_ANCHOR_CSUM_OFFSET 0x15

#define SMBIOS_IPMI_DEV_INFO_SIG 		38
#define SMBIOS_IPMI_DEV_INFO_TYPE_OFFSET 	0x4

#define SMBIOS_AREA_START 		0x000f0000
#define SMBIOS_AREA_END 		0x000fffff
#define SMBIOS_AREA_LEN 		((SMBIOS_AREA_END - SMBIOS_AREA_START) + 1)
#define SMBIOS_AREA_ALIGN 		16
#define SMBIOS_ENTRY_TLEN_OFFSET 	0x16
#define SMBIOS_ENTRY_PTR_OFFSET 	0x18
#define SMBIOS_DEV_INFO_LEN_OFFSET 	0x1
#define SMBIOS_IPMI_DEV_INFO_VER_OFFSET 	0x5
#define SMBIOS_IPMI_DEV_INFO_I2C_OFFSET 	0x6
#define SMBIOS_IPMI_DEV_INFO_NVSTOR_OFFSET 	0x7
#define SMBIOS_IPMI_DEV_INFO_ADDR_OFFSET 	0x8
#define SMBIOS_IPMI_DEV_INFO_MODIFIER_OFFSET 	0x10
#define SMBIOS_LSB_BIT 				4
#define SMBIOS_REGSPACING_SHIFT 		6
#define SMBIOS_REGSPACING_MASK 			0x3
#define SMBIOS_INTINFO_PRESENT_BIT 		3
#define SMBIOS_INTINFO_POLARITY_BIT 		1
#define SMBIOS_INTINFO_TRIGGER_BIT 		0
#define SMBIOS_DEV_INFO_INTNUM_OFFSET 		0x11

enum smbios_reg_spacing
{
  smbios_reg_spacing_1 = 0,
  smbios_reg_spacing_2 = 1,
  smbios_reg_spacing_4 = 2,
  smbios_reg_spacing_last
};
typedef enum smbios_reg_spacing smbios_reg_spacing_t;

ipmi_probe_info_t* smbios_get_dev_info (ipmi_interface_t type, ipmi_probe_info_t* pinfo, int* statusp);

#endif
