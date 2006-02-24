lib_LTLIBRARIES = libfreeipmi.la

libfreeipmi_la_SOURCES = \
	libfreeipmi.c \
	fiid.c \
	rmcp.c \
	ipmi-error.c \
	ipmi-debug.c \
	ipmi-semaphores.c \
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
	ipmi-sensor-types-spec.c \
	ipmi-sensor-units-spec.c \
	ipmi-sdr-record-types.c \
	ipmi-sel-record-types.c \
	ipmi-sensor-event-messages.c \
	ipmi-sensor-utils.c \
	ipmi-sdr-repository-cache-api.c \
	ipmi-sel-api.c \
	ipmi-sensor-api.c \
	ipmi-udm.c \
	ipmi-lan-cmds-udm.c \
	ipmi-sel-cmds-udm.c \
	ipmi-sensor-cmds-udm.c \
	ipmi-chassis-cmds-udm.c \
	ipmi-device-global-cmds-udm.c \
	ipmi-messaging-support-cmds-udm.c \
	ipmi-pef-and-alerting-cmds-udm.c \
	ipmi-sdr-repository-cmds-udm.c \
	ipmi-serial-modem-cmds-udm.c \
	ipmi-sol-cmds-udm.c \
	ipmi-kcs-interface-udm.c \
	ipmi-lan-interface-udm.c \
	ipmi-ssif-interface-udm.c

libfreeipmi_la_LDFLAGS = -module -version-info @LIBFREEIPMI_VERSION_INFO@ -lm

libfreeipmi_la_CFLAGS = -I$(srcdir)/../../common/src -Wall -D_GNU_SOURCE -D_REENTRANT

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
	freeipmi-build.h \
	ipmi-semaphores.h \
	freeipmi-portability.h

modincludedir = $(includedir)/freeipmi

modinclude_HEADERS = \
	freeipmi.h \
	fiid.h \
	rmcp.h \
	ipmi-error.h \
	ipmi-debug.h \
	ipmi-utils.h \
	ipmi-locate.h \
	ipmi-ipmb-interface.h \
	ipmi-kcs-interface.h \
	ipmi-lan-interface.h \
	ipmi-smic-interface.h \
	ipmi-ssif-interface.h \
	ipmi-bmc-watchdog-timer-cmds.h \
	ipmi-chassis-cmds.h \
	ipmi-device-global-cmds.h \
	ipmi-messaging-support-cmds.h \
	ipmi-lan-cmds.h \
	ipmi-pef-and-alerting-cmds.h \
	ipmi-sdr-repository-cmds.h \
	ipmi-sel-cmds.h \
	ipmi-sensor-cmds.h \
	ipmi-serial-modem-cmds.h \
	ipmi-sol-cmds.h \
	ipmi-sensor-types-spec.c \
	ipmi-sensor-units-spec.c \
	ipmi-sdr-record-types.c \
	ipmi-sel-record-types.c \
	ipmi-sensor-event-messages.c \
	ipmi-sensor-utils.c \
	ipmi-cmd-spec.h \
	ipmi-comp-code-spec.h \
	ipmi-netfn-spec.h \
	ipmi-lan-param-spec.h \
	ipmi-pef-param-spec.h \
	ipmi-serial-modem-param-spec.h \
	ipmi-sol-param-spec.h \
	ipmi-sensor-types-spec.h \
	ipmi-sensor-units-spec.h \
	ipmi-sdr-record-types.h \
	ipmi-sel-record-types.h \
	ipmi-sensor-event-messages.h \
	ipmi-sensor-utils.h \
	ipmi-sdr-repository-cache-api.h \
	ipmi-sel-api.h \
	ipmi-sensor-api.h \
	ipmi-udm.h \
	ipmi-chassis-cmds-udm.h \
	ipmi-device-global-cmds-udm.h \
	ipmi-messaging-support-cmds-udm.h \
	ipmi-lan-cmds-udm.h \
	ipmi-pef-and-alerting-cmds-udm.h \
	ipmi-sdr-repository-cmds-udm.h \
	ipmi-sel-cmds-udm.h \
	ipmi-sensor-cmds-udm.h \
	ipmi-serial-modem-cmds-udm.h \
	ipmi-sol-cmds-udm.h \
	ipmi-kcs-interface-udm.h \
	ipmi-lan-interface-udm.h \
	ipmi-ssif-interface-udm.h

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
