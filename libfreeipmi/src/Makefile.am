SUBDIRS = debug driver locate api

if WITH_GNU_LD
VERSION_SCRIPT = $(srcdir)/freeipmi.map
VERSION_FLAGS = -Wl,--version-script=$(VERSION_SCRIPT)
endif

lib_LTLIBRARIES = libfreeipmi.la

libfreeipmi_la_SOURCES = \
	fiid.c \
	ipmi-md2.c \
	ipmi-md5.c \
	ipmi-cipher-suite-record-format.c \
	ipmi-cipher-suite-util.c \
	ipmi-sensor-types-spec.c \
	ipmi-sensor-units-spec.c \
	rmcp-cmds.c \
	rmcp-interface.c \
	rmcp-util.c \
	ipmi-error.c \
	ipmi-util.c \
	ipmi-kcs-interface.c \
	ipmi-lan-interface.c \
	ipmi-lan-util.c \
	ipmi-crypt.c \
	ipmi-rmcpplus-interface.c \
	ipmi-rmcpplus-util.c \
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
	ipmi-sdr-record-format.c \
	ipmi-sel-record-format.c \
	ipmi-sensor-and-event-code-tables.c \
	ipmi-sensor-util.c

libfreeipmi_la_LDFLAGS = -module -version-info @LIBFREEIPMI_VERSION_INFO@ $(VERSION_FLAGS) -lm -lgcrypt
# libfreeipmi_la_CPPFLAGS = 

libfreeipmi_la_CFLAGS = -Wall
libfreeipmi_la_CPPFLAGS = \
	-I$(srcdir)/../include \
	-I$(srcdir)/../../common/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

libfreeipmi_la_LIBADD = \
	debug/libdebug.la \
	driver/libdriver.la \
	locate/liblocate.la \
	api/libapi.la \
	../../common/src/libbitops.la \
	../../common/src/libfreeipmiportability.la \
	../../common/src/libsecure.la \
	../../common/src/libxmalloc.la

noinst_HEADERS = \
	ipmi-crypt.h \
	ipmi-err-wrappers.h \
	ipmi-fiid-wrappers.h \
	ipmi-md2.h \
	ipmi-md5.h

debug/libdebug.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

driver/libdriver.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

locate/liblocate.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

api/libapi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libbitops.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libfreeipmiportability.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libsecure.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libxmalloc.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
EXTRA_DIST = freeipmi.map

force-dependency-check:
