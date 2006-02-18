lib_LTLIBRARIES = libfreeipmi.la

# AM_CFLAGS = -Wall -D_GNU_SOURCE -DFREEIPMI_LIBRARY -fpack-struct
AM_CFLAGS = -Wall -D_GNU_SOURCE -DFREEIPMI_BUILD -DFREEIPMI_LIBRARY -D_REENTRANT

EXTRA_DIST = libfreeipmi.map

libfreeipmi_la_SOURCES= bit-ops.c libfreeipmi.c xmalloc.c fiid.c ipmi-locate.c smbios-locate.c acpi-spmi-locate.c pci-locate.c defaults-locate.c rmcp.c ipmi-utils.c ipmi-error.c ipmi-semaphores.c ipmi-udm.c ipmi-lan-interface.c ipmi-kcs-interface.c ipmi-smic-interface.c ipmi-ssif-interface.c ipmi-messaging-support-cmds.c ipmi-chassis-cmds.c ipmi-device-global-cmds.c ipmi-sdr-record-types.c ipmi-sdr-repository-cmds.c ipmi-sensor-types.c ipmi-sensor-cmds.c ipmi-sel-record-types.c ipmi-sel-cmds.c ipmi-lan-cmds.c ipmi-sol-cmds.c ipmi-md2.c ipmi-md5.c ipmi-serial-modem-cmds.c ipmi-sdr-repository-cache-api.c ipmi-sensor-utils.c ipmi-debug.c ipmi-bmc-watchdog-timer-cmds.c ipmi-pef-and-alerting-cmds.c ipmi-sensor-event-messages.c ipmi-sensor-api.c ipmi-sel-api.c ipmi-sensor-cmds-udm.c ipmi-sel-cmds-udm.c ipmi-lan-cmds-udm.c ipmi-chassis-cmds-udm.c ipmi-device-global-cmds-udm.c ipmi-pef-and-alerting-cmds-udm.c ipmi-sdr-repository-cmds-udm.c ipmi-serial-modem-cmds-udm.c ipmi-sol-cmds-udm.c ipmi-messaging-support-cmds-udm.c ipmi-lan-interface-udm.c ipmi-kcs-interface-udm.c ipmi-ssif-interface-udm.c

libfreeipmi_la_LDFLAGS = -module -version-info @LIBFREEIPMI_VERSION_INFO@ -lm

noinst_HEADERS = xmalloc.h
# These are headers visible as <freeipmi/freeipmi.h>
modincludedir = $(includedir)/freeipmi

modinclude_HEADERS = freeipmi.h fiid.h ipmi-locate.h smbios-locate.h acpi-spmi-locate.h pci-locate.h defaults-locate.h bit-ops.h ipmi-cmd-spec.h ipmi-netfn-spec.h ipmi-semaphores.h ipmi-udm.h ipmi-ipmb-interface.h ipmi-lan-interface.h ipmi-kcs-interface.h ipmi-smic-interface.h ipmi-ssif-interface.h rmcp.h ipmi-utils.h ipmi-error.h ipmi-messaging-support-cmds.h ipmi-chassis-cmds.h ipmi-device-global-cmds.h ipmi-sdr-record-types.h ipmi-sdr-repository-cmds.h ipmi-sensor-types.h ipmi-sensor-cmds.h ipmi-sel-record-types.h ipmi-sel-cmds.h ipmi-lan-param-spec.h ipmi-lan-cmds.h ipmi-sol-cmds.h ipmi-sol-param-spec.h ipmi-md2.h ipmi-md5.h ipmi-md5.h ipmi-serial-modem-param-spec.h ipmi-serial-modem-cmds.h ipmi-sdr-repository-cache-api.h ipmi-sensor-utils.h ipmi-debug.h ipmi-bmc-watchdog-timer-cmds.h ipmi-pef-and-alerting-cmds.h ipmi-pef-param-spec.h ipmi-sensor-event-messages.h ipmi-sensor-api.h ipmi-sel-api.h ipmi-comp-code-spec.h ipmi-sensor-cmds-udm.h ipmi-sel-cmds-udm.h ipmi-lan-cmds-udm.h ipmi-chassis-cmds-udm.h ipmi-device-global-cmds-udm.h ipmi-pef-and-alerting-cmds-udm.h ipmi-sdr-repository-cmds-udm.h ipmi-serial-modem-cmds-udm.h ipmi-sol-cmds-udm.h ipmi-messaging-support-cmds-udm.h ipmi-lan-interface-udm.h ipmi-kcs-interface-udm.h ipmi-ssif-interface-udm.h
