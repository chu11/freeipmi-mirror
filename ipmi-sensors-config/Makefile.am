ipmi_sensors_config_CPPFLAGS = \
	-I$(top_srcdir)/common/configtool \
	-I$(top_srcdir)/common/toolcommon \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = ipmi-sensors-config

ipmi_sensors_config_LDADD = \
	$(top_builddir)/common/configtool/libconfigtool.la \
	$(top_builddir)/common/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/miscutil/libmiscutil.la \
	$(top_builddir)/common/parsecommon/libparsecommon.la \
	$(top_builddir)/common/portability/libportability.la \
	$(top_builddir)/libipmidetect/libipmidetect.la \
	$(top_builddir)/libfreeipmi/libfreeipmi.la

ipmi_sensors_config_SOURCES = \
	ipmi-sensors-config.c \
	ipmi-sensors-config.h \
	ipmi-sensors-config-argp.c \
	ipmi-sensors-config-argp.h \
	ipmi-sensors-config-discrete-section.c \
	ipmi-sensors-config-discrete-section.h \
	ipmi-sensors-config-sections.c \
	ipmi-sensors-config-sections.h \
	ipmi-sensors-config-sensor-event-enable-common.c \
	ipmi-sensors-config-sensor-event-enable-common.h \
	ipmi-sensors-config-threshold-section.c \
	ipmi-sensors-config-threshold-section.h \
	ipmi-sensors-config-utils.c \
	ipmi-sensors-config-utils.h

$(top_builddir)/common/configtool/libconfigtool.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/miscutil/libmiscutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/parsecommon/libparsecommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/portability/libportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libipmidetect/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:
