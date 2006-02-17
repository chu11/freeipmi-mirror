lib_LTLIBRARIES = libfreeipmi.la

# AM_CFLAGS = -Wall -D_GNU_SOURCE -DFREEIPMI_LIBRARY -fpack-struct
AM_CFLAGS = -Wall -D_GNU_SOURCE -DFREEIPMI_BUILD -DFREEIPMI_LIBRARY -D_REENTRANT

EXTRA_DIST = 

libfreeipmi_la_SOURCES= bit-ops.c libfreeipmi.c xmalloc.c fiid.c ipmi-locate.c smbios-locate.c acpi-spmi-locate.c pci-locate.c defaults-locate.c rmcp.c ipmi-utils.c ipmi-error.c ipmi-sessions.c ipmi-semaphores.c ipmi-interface.c ipmi-lan-interface.c ipmi-kcs-interface.c ipmi-smic-interface.c ipmi-ssif-interface.c ipmi-msg-support-cmds.c ipmi-chassis-cmds.c ipmi-dev-global-cmds.c ipmi-sdr-record-types.c ipmi-sdr-repo-cmds.c ipmi-sensor-types.c ipmi-sensor-cmds.c ipmi-sel-record-types.c ipmi-sel-cmds.c ipmi-lan-cmds.c ipmi-sol-cmds.c ipmi-md2.c ipmi-md5.c ipmi-serial-cmds.c ipmi-sdr-repo-cache.c ipmi-sensor-utils.c ipmi-debug.c ipmi-watchdog.c ipmi-msg-interface-desc.c ipmi-pef-cmds.c ipmi-sensor-event-messages.c ipmi-sensor-api.c ipmi-sel-api.c ipmi-rmcpplus.c ipmi-rmcpplus-debug.c ipmi-rmcpplus-interface.c ipmi-rmcpplus-util.c 

libfreeipmi_la_LDFLAGS = -module -version-info @LIBFREEIPMI_VERSION_INFO@ -lm -lgcrypt

noinst_HEADERS = xmalloc.h
# These are headers visible as <freeipmi/freeipmi.h>
modincludedir = $(includedir)/freeipmi

modinclude_HEADERS = freeipmi.h fiid.h ipmi-locate.h smbios-locate.h acpi-spmi-locate.h pci-locate.h defaults-locate.h bit-ops.h ipmi-cmd-spec.h ipmi-netfn-spec.h ipmi-semaphores.h ipmi-interface.h ipmi-ipmb-interface.h ipmi-lan-interface.h ipmi-kcs-interface.h ipmi-smic-interface.h ipmi-ssif-interface.h rmcp.h ipmi-sessions.h ipmi-utils.h ipmi-error.h ipmi-msg-support-cmds.h ipmi-chassis-cmds.h ipmi-dev-global-cmds.h ipmi-sdr-record-types.h ipmi-sdr-repo-cmds.h ipmi-sensor-types.h ipmi-sensor-cmds.h ipmi-sel-record-types.h ipmi-sel-cmds.h ipmi-lan-param-spec.h ipmi-lan-cmds.h ipmi-sol-cmds.h ipmi-md2.h ipmi-md5.h ipmi-md5.h ipmi-serial-modem-param-spec.h ipmi-serial-cmds.h ipmi-sdr-repo-cache.h ipmi-sensor-utils.h ipmi-debug.h ipmi-watchdog.h ipmi-msg-interface-desc.h ipmi-pef-cmds.h ipmi-pef-param-spec.h ipmi-sensor-event-messages.h ipmi-sensor-api.h ipmi-sel-api.h ipmi-comp-code-spec.h ipmi-rmcpplus.h ipmi-rmcpplus-debug.h ipmi-rmcpplus-interface.h ipmi-rmcpplus-util.h ipmi-rmcpplus-status-spec.h
