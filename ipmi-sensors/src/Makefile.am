sbin_PROGRAMS = ipmi-sensors

ipmi_sensors_SOURCES = \
	ipmi-sensors.c \
	ipmi-sensors-argp.c \
	ipmi-sensors-reading.c \
	ipmi-sensors-simple-display.c \
	ipmi-sensors-verbose-display.c \
	ipmi-sensors-very-verbose-display.c \
	ipmi-sensors-display-common.c \
	ipmi-sensors-util.c

ipmi_sensors_LDADD = \
	../../common/src/libtoolcommon.la \
	../../common/src/libhostrange.la \
	../../common/src/libmiscutils.la \
	../../ipmidetect/src/libipmidetect/libipmidetect.la \
	../../libfreeipmi/src/libfreeipmi.la \
	../../common/src/libfreeipmiportability.la
 
noinst_HEADERS = \
	ipmi-sensors.h \
	ipmi-sensors-argp.h \
	ipmi-sensors-reading.h \
	ipmi-sensors-simple-display.h \
	ipmi-sensors-verbose-display.h \
	ipmi-sensors-very-verbose-display.h \
	ipmi-sensors-display-common.h \
	ipmi-sensors-util.h
 
AM_CPPFLAGS = -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/include -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" \
	-D_REENTRANT
 
../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libfreeipmiportability.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libhostrange.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libmiscutils.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../ipmidetect/src/libipmidetect/libipmidetect.la : force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
