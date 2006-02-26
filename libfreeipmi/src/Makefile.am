lib_LTLIBRARIES = libfreeipmi.la

libfreeipmi_la_SOURCES = \
	fiid.c \
	ipmi-sensor-types-spec.c \
	ipmi-sensor-units-spec.c \
	rmcp.c \
	ipmi-debug.c \
	ipmi-error.c \
	ipmi-utils.c \
	ipmi-locate.c \
	ipmi-locate-acpi-spmi.c \
	ipmi-locate-defaults.c \
	ipmi-locate-pci.c \
	ipmi-locate-smbios.c \
	ipmi-kcs-interface.c \
	ipmi-lan-interface.c \
	ipmi-smic-interface.c \
	ipmi-ssif-interface.c \
	ipmi-bmc-watchdog-timer-cmds.c \
	ipmi-chassis-cmds.c \
	ipmi-device-global-cmds.c \
	ipmi-lan-cmds.c \
	ipmi-messaging-support-cmds.c \
	ipmi-pef-and-alerting-cmds.c \
	ipmi-sdr-repository-cmds.c \
	ipmi-sel-cmds.c \
	ipmi-sensor-cmds.c \
	ipmi-serial-modem-cmds.c \
	ipmi-sol-cmds.c \
	ipmi-sdr-record-types.c \
	ipmi-sel-record-types.c \
	ipmi-sensor-event-messages.c \
	ipmi-sensor-utils.c \
	ipmi-sdr-repository-cache-api.c \
	ipmi-sel-api.c \
	ipmi-sensor-api.c \
	ipmi-udm.c \
	ipmi-kcs-interface-udm.c \
	ipmi-lan-interface-udm.c \
	ipmi-ssif-interface-udm.c \
	ipmi-chassis-cmds-udm.c \
	ipmi-device-global-cmds-udm.c \
	ipmi-lan-cmds-udm.c \
	ipmi-messaging-support-cmds-udm.c \
	ipmi-pef-and-alerting-cmds-udm.c \
	ipmi-sel-cmds-udm.c \
	ipmi-sdr-repository-cmds-udm.c \
	ipmi-sensor-cmds-udm.c \
	ipmi-serial-modem-cmds-udm.c \
	ipmi-sol-cmds-udm.c \
	ipmi-semaphores.c \
	freeipmi-portability.c

libfreeipmi_la_LDFLAGS = -module -version-info @LIBFREEIPMI_VERSION_INFO@ -lm

libfreeipmi_la_CFLAGS = \
	-I$(srcdir)/../include \
	-I$(srcdir)/../../common/src \
	-Wall \
	-D_GNU_SOURCE \
	-D_REENTRANT

libfreeipmi_la_LIBADD = \
	../../common/src/libbitops.la \
	../../common/src/libipmicommon.la \
	../../common/src/libmd2.la \
	../../common/src/libmd5.la \
	../../common/src/libxmalloc.la

noinst_HEADERS = \
	err-wrappers.h \
	fiid-wrappers.h \
	ipmi-inband.h \
	ipmi-semaphores.h \
	freeipmi-portability.h

../../common/src/libbitops.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libipmicommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libmd2.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libmd5.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libxmalloc.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
force-dependency-check:
