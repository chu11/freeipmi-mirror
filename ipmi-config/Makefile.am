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
	ipmi-config-channel-common.c \
	ipmi-config-channel-common.h \
	ipmi-config-lan-channel-section.c \
	ipmi-config-lan-channel-section.h \
	ipmi-config-lan-conf-auth-section.c \
	ipmi-config-lan-conf-auth-section.h \
	ipmi-config-lan-conf-misc-section.c \
	ipmi-config-lan-conf-misc-section.h \
	ipmi-config-lan-conf-section.c \
	ipmi-config-lan-conf-section.h \
	ipmi-config-lan-conf-security-keys-section.c \
	ipmi-config-lan-conf-security-keys-section.h \
	ipmi-config-lan-conf-user-security-section.c \
	ipmi-config-lan-conf-user-security-section.h \
	ipmi-config-map.c \
	ipmi-config-map.h \
	ipmi-config-misc-section.c \
	ipmi-config-misc-section.h \
	ipmi-config-pef-conf-section.c \
	ipmi-config-pef-conf-section.h \
	ipmi-config-rmcpplus-conf-privilege-section.c \
	ipmi-config-rmcpplus-conf-privilege-section.h \
	ipmi-config-sections.c \
	ipmi-config-sections.h \
	ipmi-config-serial-channel-section.c \
	ipmi-config-serial-channel-section.h \
	ipmi-config-serial-conf-section.c \
	ipmi-config-serial-conf-section.h \
	ipmi-config-sol-conf-section.c \
	ipmi-config-sol-conf-section.h \
	ipmi-config-user-sections.c \
	ipmi-config-user-sections.h \
	ipmi-config-utils.c \
	ipmi-config-utils.h \
	ipmi-config-validate.c \
	ipmi-config-validate.h \
	ipmi-config-tool-comment.c \
	ipmi-config-tool-comment.h \
	ipmi-config-tool-checkout.c \
	ipmi-config-tool-checkout.h \
	ipmi-config-tool-commit.c \
	ipmi-config-tool-commit.h \
	ipmi-config-tool-diff.c \
	ipmi-config-tool-diff.h \
	ipmi-config-tool-parse.c \
	ipmi-config-tool-parse.h \
	ipmi-config-tool-pef-conf-section.c \
	ipmi-config-tool-pef-conf-section.h \
	ipmi-config-tool-section.c \
	ipmi-config-tool-section.h

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
