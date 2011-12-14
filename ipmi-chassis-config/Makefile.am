sbin_PROGRAMS = ipmi-chassis-config

ipmi_chassis_config_CPPFLAGS = \
	-I$(top_srcdir)/common/configtool \
	-I$(top_srcdir)/common/toolcommon \
	-I$(top_srcdir)/common/toolhostrange \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

ipmi_chassis_config_LDADD = \
	$(top_builddir)/common/configtool/libconfigtool.la \
	$(top_builddir)/common/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/toolhostrange/libtoolhostrange.la \
	$(top_builddir)/common/miscutil/libmiscutil.la \
	$(top_builddir)/common/parsecommon/libparsecommon.la \
	$(top_builddir)/common/portability/libfreeipmiportability.la \
	$(top_builddir)/libipmidetect/libipmidetect.la \
	$(top_builddir)/libfreeipmi/libfreeipmi.la

ipmi_chassis_config_SOURCES = \
	ipmi-chassis-config.c \
	ipmi-chassis-config.h \
	ipmi-chassis-config-argp.c \
	ipmi-chassis-config-argp.h \
	ipmi-chassis-config-boot-flags.c \
	ipmi-chassis-config-boot-flags.h \
	ipmi-chassis-config-front-panel-buttons.c \
	ipmi-chassis-config-front-panel-buttons.h \
	ipmi-chassis-config-map.c \
	ipmi-chassis-config-map.h \
	ipmi-chassis-config-sections.c \
	ipmi-chassis-config-sections.h \
	ipmi-chassis-config-power-conf.c \
	ipmi-chassis-config-power-conf.h \
	ipmi-chassis-config-validate.c \
	ipmi-chassis-config-validate.h

$(top_builddir)/common/configtool/libconfigtool.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/toolhostrange/libtoolhostrange.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/miscutil/libmiscutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/parsecommon/libparsecommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libipmidetect/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:
