noinst_HEADERS = \
	ipmi-sensors.h \
	ipmi-sensors-argp.h \
	ipmi-sensors-reading.h \
	ipmi-sensors-simple-display.h \
	ipmi-sensors-verbose-display.h \
	ipmi-sensors-very-verbose-display.h \
	ipmi-sensors-display-common.h \
	ipmi-sensors-util.h

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = ipmi-sensors

ipmi_sensors_LDADD = \
	$(top_builddir)/common/src/libtoolcommon.la
 
ipmi_sensors_SOURCES = \
	ipmi-sensors.c \
	ipmi-sensors-argp.c \
	ipmi-sensors-reading.c \
	ipmi-sensors-simple-display.c \
	ipmi-sensors-verbose-display.c \
	ipmi-sensors-very-verbose-display.c \
	ipmi-sensors-display-common.c \
	ipmi-sensors-util.c

$(top_builddir)/common/src/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
