SUBDIRS = udm

if WITH_GNU_LD
VERSION_SCRIPT = $(srcdir)/freeipmi.map
VERSION_FLAGS = -Wl,--version-script=$(VERSION_SCRIPT)
endif

lib_LTLIBRARIES = libfreeipmi.la

libfreeipmi_la_SOURCES = \
	fiid.c \
	ipmi-cipher-suite-spec.c \
	ipmi-sensor-types-spec.c \
	ipmi-sensor-units-spec.c \
	rmcp.c \
	rmcp-utils.c \
	ipmi-debug.c \
	ipmi-error.c \
	ipmi-utils.c \
	ipmi-locate.c \
	ipmi-locate-dmidecode.c \
	ipmi-locate-acpi-spmi.c \
	ipmi-locate-defaults.c \
	ipmi-locate-pci.c \
	ipmi-locate-smbios.c \
	ipmi-kcs-driver.c \
	ipmi-kcs-interface.c \
	ipmi-lan-interface.c \
	ipmi-lan-utils.c \
	ipmi-openipmi-driver.c \
	ipmi-crypt.c \
	ipmi-rmcpplus.c \
	ipmi-rmcpplus-debug.c \
	ipmi-rmcpplus-interface.c \
	ipmi-rmcpplus-utils.c \
	ipmi-ssif-driver.c \
	ipmi-semaphores.c \
	ipmi-bmc-watchdog-timer-cmds.c \
	ipmi-chassis-cmds.c \
	ipmi-device-global-cmds.c \
	ipmi-fru-inventory-device-cmds.c \
	ipmi-fru-information-storage-definition.c \
	ipmi-lan-cmds.c \
	ipmi-messaging-support-cmds.c \
	ipmi-pef-and-alerting-cmds.c \
	ipmi-rmcpplus-support-and-payload-cmds.c \
	ipmi-sdr-repository-cmds.c \
	ipmi-sel-cmds.c \
	ipmi-sensor-cmds.c \
	ipmi-serial-modem-cmds.c \
	ipmi-sol-cmds.c \
	ipmi-sdr-record-types.c \
	ipmi-sel-record-types.c \
	ipmi-sensor-and-event-code-tables.c \
	ipmi-sensor-utils.c \
	ipmi-common.c

libfreeipmi_la_LDFLAGS = -module -version-info @LIBFREEIPMI_VERSION_INFO@ $(VERSION_FLAGS) -lm -lgcrypt
# libfreeipmi_la_CPPFLAGS = 

# achu
#
# IPMI_DEBUG_IPCKEY is so we can do inband development without the real ipckey being
# installed.

libfreeipmi_la_CFLAGS = -Wall
libfreeipmi_la_CPPFLAGS = \
	-I$(srcdir)/../include \
	-I$(srcdir)/../../common/src \
	-DIPMI_IPCKEY=\"$(localstatedir)/lib/$(PACKAGE_NAME)/ipckey\" \
	-DIPMI_DEBUG_IPCKEY=\"$(top_builddir)/libfreeipmi/src/ipmi-semaphores.h\" \
	-D_GNU_SOURCE \
	-D_REENTRANT

libfreeipmi_la_LIBADD = \
	udm/libudm.la \
	../../common/src/libbitops.la \
	../../common/src/libfreeipmiportability.la \
	../../common/src/libmd2.la \
	../../common/src/libmd5.la \
	../../common/src/libxmalloc.la

noinst_HEADERS = \
	ipmi-inband.h \
	ipmi-semaphores.h \
	ipmi-common.h

udm/libudm.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libbitops.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libfreeipmiportability.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libmd2.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libmd5.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libxmalloc.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
EXTRA_DIST = freeipmi.map

force-dependency-check:
