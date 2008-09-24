noinst_HEADERS = \
	bmc-config.h \
	bmc-config-argp.h \
	bmc-config-channel-common.h \
	bmc-config-utils.h \
	bmc-config-user-sections.h \
	bmc-config-lan-channel-section.h \
	bmc-config-lan-conf-auth-section.h \
	bmc-config-lan-conf-misc-section.h \
	bmc-config-lan-conf-section.h \
	bmc-config-lan-conf-security-keys-section.h \
	bmc-config-rmcpplus-conf-privilege-section.h \
	bmc-config-serial-channel-section.h \
	bmc-config-serial-conf-section.h \
	bmc-config-pef-conf-section.h \
	bmc-config-sol-conf-section.h \
	bmc-config-misc-section.h \
	bmc-config-map.h \
	bmc-config-sections.h \
	bmc-config-validate.h

AM_CPPFLAGS = \
	-I$(top_srcdir)/common/src/configtool \
	-I$(top_srcdir)/common/src/toolcommon \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = bmc-config

bmc_config_LDADD = \
	$(top_builddir)/common/src/configtool/libconfigtoolcommon.la \
	$(top_builddir)/common/src/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/src/toolhostrange/libtoolhostrange.la \
	$(top_builddir)/common/src/miscutil/libmiscutils.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
	$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

bmc_config_SOURCES = \
	bmc-config.c \
	bmc-config-argp.c \
	bmc-config-channel-common.c \
	bmc-config-map.c \
	bmc-config-sections.c \
	bmc-config-utils.c \
	bmc-config-user-sections.c \
	bmc-config-lan-channel-section.c \
	bmc-config-serial-channel-section.c \
	bmc-config-lan-conf-section.c \
	bmc-config-lan-conf-auth-section.c \
	bmc-config-lan-conf-security-keys-section.c \
	bmc-config-lan-conf-misc-section.c \
	bmc-config-rmcpplus-conf-privilege-section.c \
	bmc-config-serial-conf-section.c \
	bmc-config-pef-conf-section.c \
	bmc-config-sol-conf-section.c \
	bmc-config-misc-section.c \
	bmc-config-validate.c

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
