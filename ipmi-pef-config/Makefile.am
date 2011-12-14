sbin_PROGRAMS = ipmi-pef-config

ipmi_pef_config_CPPFLAGS = \
	-I$(top_srcdir)/common/configtool \
	-I$(top_srcdir)/common/toolcommon \
	-I$(top_srcdir)/common/toolhostrange \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT


ipmi_pef_config_LDADD = \
	$(top_builddir)/common/configtool/libconfigtool.la \
	$(top_builddir)/common/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/toolhostrange/libtoolhostrange.la \
	$(top_builddir)/common/miscutil/libmiscutil.la \
	$(top_builddir)/common/parsecommon/libparsecommon.la \
	$(top_builddir)/common/portability/libfreeipmiportability.la \
	$(top_builddir)/libipmidetect/libipmidetect.la \
	$(top_builddir)/libfreeipmi/libfreeipmi.la

ipmi_pef_config_SOURCES = \
	ipmi-pef-config.c \
	ipmi-pef-config.h \
	ipmi-pef-config-alert-policy-table.c \
	ipmi-pef-config-alert-policy-table.h \
	ipmi-pef-config-alert-string.c \
	ipmi-pef-config-alert-string.h \
	ipmi-pef-config-argp.c \
	ipmi-pef-config-argp.h \
	ipmi-pef-config-community-string.c \
	ipmi-pef-config-community-string.h \
	ipmi-pef-config-event-filter-table.c \
	ipmi-pef-config-event-filter-table.h \
	ipmi-pef-config-info.c \
	ipmi-pef-config-info.h \
	ipmi-pef-config-lan-alert-destination.c \
	ipmi-pef-config-lan-alert-destination.h \
	ipmi-pef-config-map.c \
	ipmi-pef-config-map.h \
	ipmi-pef-config-pef-conf-section.c \
	ipmi-pef-config-pef-conf-section.h \
	ipmi-pef-config-sections.c \
	ipmi-pef-config-sections.h \
	ipmi-pef-config-utils.c \
	ipmi-pef-config-utils.h \
	ipmi-pef-config-validate.c \
	ipmi-pef-config-validate.h

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

install-exec-hook:
	$(install_sh) -d -m 755 $(DESTDIR)$(sbindir)
	$(LN_S) -f $(sbindir)/ipmi-pef-config $(DESTDIR)$(sbindir)/pef-config

uninstall-hook:
	$(RM) $(DESTDIR)$(sbindir)/pef-config
