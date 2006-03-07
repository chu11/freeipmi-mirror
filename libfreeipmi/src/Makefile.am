SUBDIRS = udm

lib_LTLIBRARIES = libfreeipmi.la

libfreeipmi_la_SOURCES = \
	fiid.c \
	ipmi-sensor-types-spec.c \
	ipmi-sensor-units-spec.c \
	rmcp.c \
	rmcp-utils.c \
	ipmi-debug.c \
	ipmi-error.c \
	ipmi-utils.c \
	ipmi-locate.c \
	ipmi-locate-acpi-spmi.c \
	ipmi-locate-defaults.c \
	ipmi-locate-pci.c \
	ipmi-locate-smbios.c \
	ipmi-kcs.c \
	ipmi-kcs-api.c \
	ipmi-lan.c \
	ipmi-lan-interface.c \
	ipmi-lan-utils.c \
	ipmi-rmcpplus.c \
	ipmi-rmcpplus-crypt.c \
	ipmi-rmcpplus-debug.c \
	ipmi-rmcpplus-interface.c \
	ipmi-rmcpplus-utils.c \
	ipmi-smic-interface.c \
	ipmi-ssif-api.c \
	ipmi-semaphores.c \
	ipmi-bmc-watchdog-timer-cmds.c \
	ipmi-chassis-cmds.c \
	ipmi-device-global-cmds.c \
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
	ipmi-sensor-event-messages.c \
	ipmi-sensor-utils.c

libfreeipmi_la_LDFLAGS = -module -version-info @LIBFREEIPMI_VERSION_INFO@ -lm -lgcrypt

libfreeipmi_la_CFLAGS = \
	-I$(srcdir)/../include \
	-I$(srcdir)/../../common/src \
	-Wall \
	-D_GNU_SOURCE \
	-D_REENTRANT

libfreeipmi_la_LIBADD = \
	udm/libudm.la \
	../../common/src/libbitops.la \
	../../common/src/libfreeipmiportability.la \
	../../common/src/libipmicommon.la \
	../../common/src/libmd2.la \
	../../common/src/libmd5.la \
	../../common/src/libxmalloc.la

noinst_HEADERS = \
	ipmi-inband.h \
	ipmi-semaphores.h

udm/libudm.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libbitops.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libipmicommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libfreeipmiportability.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libmd2.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libmd5.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libxmalloc.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
force-dependency-check:
