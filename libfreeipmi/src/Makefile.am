lib_LTLIBRARIES = libfreeipmi.la

# AM_CFLAGS = -Wall -D_GNU_SOURCE -DFREEIPMI_LIBRARY -fpack-struct
AM_CFLAGS = -Wall -D_GNU_SOURCE -DFREEIPMI_BUILD -DFREEIPMI_LIBRARY -D_REENTRANT

EXTRA_DIST = libfreeipmi.map

# TEST
#libfreeipmi_la_SOURCES= bit-ops.c libfreeipmi.c xmalloc.c fiid.c ipmi-locate.c smbios-locate.c acpi-spmi-locate.c pci-locate.c defaults-locate.c rmcp.c ipmi-utils.c ipmi-netfn-spec.c ipmi-error.c ipmi-sessions.c ipmi-semaphores.c ipmi-interface.c ipmi-lan-interface.c ipmi-kcs-interface.c ipmi-smic-interface.c ipmi-ssif-interface.c ipmi-msg-support-cmds.c ipmi-chassis-cmds.c ipmi-dev-global-cmds.c ipmi-sdr-record-types.c ipmi-sdr-repo-cmds.c ipmi-sensor-types.c ipmi-sensor-cmds.c ipmi-sel-record-types.c ipmi-sel-cmds.c ipmi-lan-cmds.c ipmi-sol-cmds.c ipmi-md2.c ipmi-md5.c ipmi-sha1.c ipmi-hmac.c ipmi-serial-cmds.c ipmi-sdr-repo-cache.c ipmi-sensor-utils.c ipmi-debug.c ipmi-watchdog.c ipmi-msg-interface-desc.c ipmi-pef-cmds.c ipmi-sensor-event-messages.c ipmi-sensor-api.c ipmi-sel-api.c

#TODO
#libfreeipmi_la_SOURCES= ipmi-sensor-api.c ipmi-sel-api.c 

libfreeipmi_la_SOURCES= bit-ops.c fiid.c ipmi-debug.c xmalloc.c ipmi-lan-interface.c rmcp.c ipmi-msg-support-cmds.c ipmi-sessions.c ipmi-md2.c ipmi-md5.c ipmi-sha1.c ipmi-hmac.c ipmi-utils.c ipmi-chassis-cmds.c ipmi-error.c ipmi-kcs-interface.c ipmi-smic-interface.c ipmi-watchdog.c ipmi-interface.c acpi-spmi-locate.c defaults-locate.c ipmi-locate.c pci-locate.c smbios-locate.c ipmi-ssif-interface.c ipmi-dev-global-cmds.c ipmi-semaphores.c ipmi-netfn-spec.c libfreeipmi.c ipmi-msg-interface-desc.c ipmi-lan-cmds.c ipmi-sdr-repo-cache.c ipmi-sdr-record-types.c ipmi-sdr-repo-cmds.c ipmi-sol-cmds.c ipmi-pef-cmds.c ipmi-sensor-types.c ipmi-sensor-cmds.c ipmi-serial-cmds.c ipmi-sensor-event-messages.c ipmi-sel-record-types.c ipmi-sel-cmds.c ipmi-sensor-utils.c

libfreeipmi_la_LDFLAGS = -module -version-info @LIBFREEIPMI_VERSION_INFO@ -lm

# TEST
#noinst_HEADERS = xmalloc.h
# These are headers visible as <freeipmi/freeipmi.h>
modincludedir = $(includedir)/freeipmi

# TEST
#modinclude_HEADERS = freeipmi.h fiid.h ipmi-locate.h smbios-locate.h acpi-spmi-locate.h pci-locate.h defaults-locate.h bit-ops.h ipmi-cmd-spec.h ipmi-netfn-spec.h ipmi-semaphores.h ipmi-interface.h ipmi-ipmb-interface.h ipmi-lan-interface.h ipmi-kcs-interface.h ipmi-smic-interface.h ipmi-ssif-interface.h rmcp.h ipmi-sessions.h ipmi-utils.h ipmi-error.h ipmi-msg-support-cmds.h ipmi-chassis-cmds.h ipmi-dev-global-cmds.h ipmi-sdr-record-types.h ipmi-sdr-repo-cmds.h ipmi-sensor-types.h ipmi-sensor-cmds.h ipmi-sel-record-types.h ipmi-sel-cmds.h ipmi-lan-param-spec.h ipmi-lan-cmds.h ipmi-sol-cmds.h ipmi-md2.h ipmi-md5.h ipmi-md5.h ipmi-sha1.h ipmi-hmac.h ipmi-serial-modem-param-spec.h ipmi-serial-cmds.h ipmi-sdr-repo-cache.h ipmi-sensor-utils.h ipmi-debug.h ipmi-watchdog.h ipmi-msg-interface-desc.h ipmi-pef-cmds.h ipmi-pef-param-spec.h ipmi-sensor-event-messages.h ipmi-sensor-api.h ipmi-sel-api.h

modinclude_HEADERS = bit-ops.h freeipmi.h fiid.h ipmi-debug.h ipmi-error.h rmcp.h ipmi-lan-interface.h rmcp.h ipmi-utils.h ipmi-msg-support-cmds.h ipmi-sessions.h ipmi-ipmb-interface.h ipmi-interface.h ipmi-cmd-spec.h ipmi-md2.h ipmi-md5.h ipmi-sha1.h ipmi-hmac.h ipmi-utils.h ipmi-chassis-cmds.h ipmi-kcs-interface.h ipmi-smic-interface.c ipmi-watchdog.h acpi-spmi-locate.h  defaults-locate.h ipmi-locate.h pci-locate.h smbios-locate.h ipmi-ssif-interface.h ipmi-dev-global-cmds.h ipmi-semaphores.h ipmi-lan-cmds.h ipmi-lan-param-spec.h ipmi-sdr-repo-cache.h ipmi-sdr-record-types.h  ipmi-sdr-record-types.h ipmi-sdr-repo-cmds.h ipmi-sol-cmds.h ipmi-pef-cmds.h ipmi-sensor-types.h ipmi-sensor-cmds.h ipmi-sel-cmds.h ipmi-sensor-utils.h
