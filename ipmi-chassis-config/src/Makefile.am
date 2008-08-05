noinst_HEADERS = \
	ipmi-chassis-config.h \
	ipmi-chassis-config-argp.h \
	ipmi-chassis-config-sections.h \
	ipmi-chassis-config-front-panel-buttons.h

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src/configtool \
	-I$(top_builddir)/common/src/miscutil \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/common/src/toolcommon \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = ipmi-chassis-config

ipmi_chassis_config_LDADD = \
	$(top_builddir)/common/src/configtool/libconfigtoolcommon.la

ipmi_chassis_config_SOURCES = \
	ipmi-chassis-config.c \
	ipmi-chassis-config-argp.c \
	ipmi-chassis-config-sections.c \
	ipmi-chassis-config-front-panel-buttons.c

$(top_builddir)/common/src/configtool/libconfigtoolcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
