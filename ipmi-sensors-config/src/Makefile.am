noinst_HEADERS = \
	ipmi-sensors-config.h \
	ipmi-sensors-config-argp.h \
	ipmi-sensors-config-sections.h \
	ipmi-sensors-config-threshold-section.h \
	ipmi-sensors-config-utils.h

sbin_PROGRAMS = ipmi-sensors-config

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

# ipmi_sensors_config doesn't use hostrange, but
# we need pstdout for linking reasons
# we need miscutils for hostlist lib in pstdout too
ipmi_sensors_config_LDADD = \
	$(top_builddir)/common/src/libtoolcommon.la \
	$(top_builddir)/common/src/libmiscutils.la \
	$(top_builddir)/common/src/libhostrange.la \
	$(top_builddir)/common/src/libconfigtoolcommon.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

ipmi_sensors_config_SOURCES = \
	ipmi-sensors-config.c \
	ipmi-sensors-config-argp.c \
	ipmi-sensors-config-sections.c \
	ipmi-sensors-config-threshold-section.c \
	ipmi-sensors-config-utils.c
