noinst_HEADERS = \
	pef-config.h \
	pef-config-argp.h \
	pef-config-pef-conf-section.h \
	pef-config-community-string.h \
	pef-config-lan-alert-destination.h \
	pef-config-alert-string.h \
	pef-config-alert-policy-table.h \
	pef-config-event-filter-table.h \
	pef-config-info.h \
	pef-config-map.h \
	pef-config-sections.h \
	pef-config-utils.h \
	pef-config-validate.h

AM_CPPFLAGS = \
	-I$(top_srcdir)/common/src/configtool \
	-I$(top_srcdir)/common/src/toolcommon \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = pef-config

pef_config_LDADD = \
	$(top_builddir)/common/src/configtool/libconfigtoolcommon.la \
	$(top_builddir)/common/src/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/src/toolhostrange/libtoolhostrange.la \
	$(top_builddir)/common/src/miscutil/libmiscutils.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
	$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

pef_config_SOURCES = \
	pef-config.c \
	pef-config-argp.c \
	pef-config-pef-conf-section.c \
	pef-config-community-string.c \
	pef-config-lan-alert-destination.c \
	pef-config-alert-string.c \
	pef-config-alert-policy-table.c \
	pef-config-event-filter-table.c \
	pef-config-info.c \
	pef-config-map.c \
	pef-config-sections.c \
	pef-config-utils.c \
	pef-config-validate.c

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
