sbin_PROGRAMS = ipmi-config

ipmi_config_CPPFLAGS = \
	-I$(top_srcdir)/common/toolcommon \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

ipmi_config_LDADD = \
	$(top_builddir)/common/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/miscutil/libmiscutil.la \
	$(top_builddir)/common/parsecommon/libparsecommon.la \
	$(top_builddir)/common/portability/libportability.la \
	$(top_builddir)/libipmidetect/libipmidetect.la \
	$(top_builddir)/libfreeipmi/libfreeipmi.la

ipmi_config_SOURCES = \
	ipmi-config.c \
	ipmi-config.h \
	ipmi-config-argp.c \
	ipmi-config-argp.h \
	ipmi-config-chassis-boot-flags.c \
	ipmi-config-chassis-boot-flags.h \
	ipmi-config-chassis-front-panel-buttons.c \
	ipmi-config-chassis-front-panel-buttons.h \
	ipmi-config-chassis-power-conf.c \
	ipmi-config-chassis-power-conf.h \
	ipmi-config-chassis-sections.c \
	ipmi-config-chassis-sections.h \
	ipmi-config-checkout.c \
	ipmi-config-checkout.h \
	ipmi-config-comment.c \
	ipmi-config-comment.h \
	ipmi-config-commit.c \
	ipmi-config-commit.h \
	ipmi-config-common-pef-conf-section.c \
	ipmi-config-common-pef-conf-section.h \
	ipmi-config-core-channel-common.c \
	ipmi-config-core-channel-common.h \
	ipmi-config-core-lan-channel-section.c \
	ipmi-config-core-lan-channel-section.h \
	ipmi-config-core-lan-conf-auth-section.c \
	ipmi-config-core-lan-conf-auth-section.h \
	ipmi-config-core-lan-conf-misc-section.c \
	ipmi-config-core-lan-conf-misc-section.h \
	ipmi-config-core-lan-conf-section.c \
	ipmi-config-core-lan-conf-section.h \
	ipmi-config-core-lan-conf-security-keys-section.c \
	ipmi-config-core-lan-conf-security-keys-section.h \
	ipmi-config-core-lan-conf-user-security-section.c \
	ipmi-config-core-lan-conf-user-security-section.h \
	ipmi-config-core-misc-section.c \
	ipmi-config-core-misc-section.h \
	ipmi-config-core-pef-conf-section.c \
	ipmi-config-core-pef-conf-section.h \
	ipmi-config-core-rmcpplus-conf-privilege-section.c \
	ipmi-config-core-rmcpplus-conf-privilege-section.h \
	ipmi-config-core-sections.c \
	ipmi-config-core-sections.h \
	ipmi-config-core-serial-channel-section.c \
	ipmi-config-core-serial-channel-section.h \
	ipmi-config-core-serial-conf-section.c \
	ipmi-config-core-serial-conf-section.h \
	ipmi-config-core-sol-conf-section.c \
	ipmi-config-core-sol-conf-section.h \
	ipmi-config-core-user-sections.c \
	ipmi-config-core-user-sections.h \
	ipmi-config-diff.c \
	ipmi-config-diff.h \
	ipmi-config-map.c \
	ipmi-config-map.h \
	ipmi-config-parse.c \
	ipmi-config-parse.h \
	ipmi-config-section.c \
	ipmi-config-section.h \
	ipmi-config-sensors-utils.c \
	ipmi-config-sensors-utils.h \
	ipmi-config-utils.c \
	ipmi-config-utils.h \
	ipmi-config-validate.c \
	ipmi-config-validate.h

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
