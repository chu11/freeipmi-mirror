noinst_HEADERS = \
	ipmi-sensors-config.h \
	ipmi-sensors-config-argp.h \
	ipmi-sensors-config-sections.h \
	ipmi-sensors-config-threshold-section.h \
	ipmi-sensors-config-utils.h

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src/configtool \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/common/src/toolcommon \
	-I$(top_builddir)/common/src/util \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = ipmi-sensors-config

ipmi_sensors_config_LDADD = \
	$(top_builddir)/common/src/configtool/libconfigtoolcommon.la

ipmi_sensors_config_SOURCES = \
	ipmi-sensors-config.c \
	ipmi-sensors-config-argp.c \
	ipmi-sensors-config-sections.c \
	ipmi-sensors-config-threshold-section.c \
	ipmi-sensors-config-utils.c

$(top_builddir)/common/src/configtool/libconfigtoolcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
