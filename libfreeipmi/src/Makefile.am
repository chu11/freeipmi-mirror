SUBDIRS = \
	api \
	debug \
	driver \
	fiid \
	interface \
	libcommon \
	locate \
	record-format \
	spec \
	util

if WITH_GNU_LD
VERSION_SCRIPT = $(srcdir)/freeipmi.map
VERSION_FLAGS = -Wl,--version-script=$(VERSION_SCRIPT)
endif

lib_LTLIBRARIES = libfreeipmi.la

libfreeipmi_la_SOURCES = \
	rmcp-cmds.c \
	ipmi-bmc-watchdog-timer-cmds.c \
	ipmi-chassis-cmds.c \
	ipmi-device-global-cmds.c \
	ipmi-fru-inventory-device-cmds.c \
	ipmi-lan-cmds.c \
	ipmi-messaging-support-cmds.c \
	ipmi-pef-and-alerting-cmds.c \
	ipmi-rmcpplus-support-and-payload-cmds.c \
	ipmi-sdr-repository-cmds.c \
	ipmi-sel-cmds.c \
	ipmi-sensor-cmds.c \
	ipmi-serial-modem-cmds.c \
	ipmi-sol-cmds.c

libfreeipmi_la_LDFLAGS = -module -version-info @LIBFREEIPMI_VERSION_INFO@ $(VERSION_FLAGS) -lm 
# libfreeipmi_la_CPPFLAGS = 

libfreeipmi_la_CFLAGS = -Wall
libfreeipmi_la_CPPFLAGS = \
	-I$(srcdir)/../include \
	-I$(srcdir)/../../common/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

libfreeipmi_la_LIBADD = \
	api/libapi.la \
	debug/libdebug.la \
	driver/libdriver.la \
	fiid/libfiid.la \
	interface/libinterface.la \
	libcommon/libcommon.la \
	locate/liblocate.la \
	record-format/librecordformat.la \
	spec/libspec.la \
	util/libutil.la \
	../../common/src/libfreeipmiportability.la \
	../../common/src/libmiscutils.la

api/libapi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

debug/libdebug.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

driver/libdriver.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

fiid/libfiid.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

interface/libinterface.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

libcommon/libcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

locate/liblocate.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

record-format/librecordformat.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

spec/libspec.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libfreeipmiportability.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libmiscutils.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

EXTRA_DIST = freeipmi.map

force-dependency-check:
