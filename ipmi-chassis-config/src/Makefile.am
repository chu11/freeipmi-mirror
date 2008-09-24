noinst_HEADERS = \
	ipmi-chassis-config.h \
	ipmi-chassis-config-argp.h \
	ipmi-chassis-config-sections.h \
	ipmi-chassis-config-boot-flags.h \
	ipmi-chassis-config-front-panel-buttons.h \
	ipmi-chassis-config-power-conf.h \
	ipmi-chassis-config-map.h \
	ipmi-chassis-config-validate.h

AM_CPPFLAGS = \
	-I$(top_srcdir)/common/src/configtool \
	-I$(top_srcdir)/common/src/toolcommon \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = ipmi-chassis-config

ipmi_chassis_config_LDADD = \
	$(top_builddir)/common/src/configtool/libconfigtoolcommon.la \
	$(top_builddir)/common/src/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/src/toolhostrange/libtoolhostrange.la \
	$(top_builddir)/common/src/miscutil/libmiscutils.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
	$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

ipmi_chassis_config_SOURCES = \
	ipmi-chassis-config.c \
	ipmi-chassis-config-argp.c \
	ipmi-chassis-config-sections.c \
	ipmi-chassis-config-boot-flags.c \
	ipmi-chassis-config-front-panel-buttons.c \
	ipmi-chassis-config-power-conf.c \
	ipmi-chassis-config-map.c \
	ipmi-chassis-config-validate.c

$(top_builddir)/common/src/configtool/libconfigtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/toolhostrange/libtoolhostrange.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/miscutil/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:
