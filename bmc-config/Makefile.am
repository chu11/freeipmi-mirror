sbin_PROGRAMS = bmc-config

bmc_config_CPPFLAGS = \
	-I$(top_srcdir)/common/configtool \
	-I$(top_srcdir)/common/toolcommon \
	-I$(top_srcdir)/common/toolhostrange \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

bmc_config_LDADD = \
	$(top_builddir)/common/configtool/libconfigtoolcommon.la \
	$(top_builddir)/common/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/toolhostrange/libtoolhostrange.la \
	$(top_builddir)/common/miscutil/libmiscutils.la \
	$(top_builddir)/common/parsecommon/libparsecommon.la \
	$(top_builddir)/common/portability/libfreeipmiportability.la \
	$(top_builddir)/libipmidetect/libipmidetect.la \
	$(top_builddir)/libfreeipmi/libfreeipmi.la

bmc_config_SOURCES = \
	bmc-config.c \
	bmc-config.h \
	bmc-config-argp.c \
	bmc-config-argp.h \
	bmc-config-channel-common.c \
	bmc-config-channel-common.h \
	bmc-config-lan-channel-section.c \
	bmc-config-lan-channel-section.h \
	bmc-config-lan-conf-auth-section.c \
	bmc-config-lan-conf-auth-section.h \
	bmc-config-lan-conf-misc-section.c \
	bmc-config-lan-conf-misc-section.h \
	bmc-config-lan-conf-section.c \
	bmc-config-lan-conf-section.h \
	bmc-config-lan-conf-security-keys-section.c \
	bmc-config-lan-conf-security-keys-section.h \
	bmc-config-lan-conf-user-security-section.c \
	bmc-config-lan-conf-user-security-section.h \
	bmc-config-map.c \
	bmc-config-map.h \
	bmc-config-misc-section.c \
	bmc-config-misc-section.h \
	bmc-config-pef-conf-section.c \
	bmc-config-pef-conf-section.h \
	bmc-config-rmcpplus-conf-privilege-section.c \
	bmc-config-rmcpplus-conf-privilege-section.h \
	bmc-config-sections.c \
	bmc-config-sections.h \
	bmc-config-serial-channel-section.c \
	bmc-config-serial-channel-section.h \
	bmc-config-serial-conf-section.c \
	bmc-config-serial-conf-section.h \
	bmc-config-sol-conf-section.c \
	bmc-config-sol-conf-section.h \
	bmc-config-user-sections.c \
	bmc-config-user-sections.h \
	bmc-config-utils.c \
	bmc-config-utils.h \
	bmc-config-validate.c \
	bmc-config-validate.h

$(top_builddir)/common/configtool/libconfigtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/toolhostrange/libtoolhostrange.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/miscutil/libmiscutils.la : force-dependency-check
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
