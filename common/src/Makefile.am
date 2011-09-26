SUBDIRS = \
	debugutil \
	miscutil \
	parsecommon \
	portability

noinst_LTLIBRARIES = \
	configtool/libconfigtoolcommon.la \
	pingtool/libpingtoolcommon.la \
	toolcommon/libtoolcommon.la \
	toolhostrange/libtoolhostrange.la \
	tooloem/libtooloem.la \
	toolsdr/libtoolsdr.la \
	toolsensor/libtoolsensor.la

configtool_libconfigtoolcommon_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/parsecommon \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolcommon \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

configtool_libconfigtoolcommon_la_SOURCES = \
	configtool/config-tool-argp.c \
	configtool/config-tool-argp.h \
	configtool/config-tool-comment.c \
	configtool/config-tool-comment.h \
	configtool/config-tool-commit.c \
	configtool/config-tool-commit.h \
	configtool/config-tool-common.h \
	configtool/config-tool-checkout.c \
	configtool/config-tool-checkout.h \
	configtool/config-tool-diff.c \
	configtool/config-tool-diff.h \
	configtool/config-tool-parse.c \
	configtool/config-tool-parse.h \
	configtool/config-tool-pef-conf-section.c \
	configtool/config-tool-pef-conf-section.h \
	configtool/config-tool-section.c \
	configtool/config-tool-section.h \
	configtool/config-tool-utils.c \
	configtool/config-tool-utils.h \
	configtool/config-tool-validate.c \
	configtool/config-tool-validate.h

pingtool_libpingtoolcommon_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolcommon \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE

pingtool_libpingtoolcommon_la_SOURCES = \
	pingtool/ping-tool-common.c \
	pingtool/ping-tool-common.h

toolcommon_libtoolcommon_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/parsecommon \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/common/src/toolsdr \
	-I$(top_srcdir)/common/src/toolsensor \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

toolcommon_libtoolcommon_la_SOURCES = \
	toolcommon/tool-common.c \
	toolcommon/tool-common.h \
	toolcommon/tool-config-file-common.c \
	toolcommon/tool-config-file-common.h \
	toolcommon/tool-cmdline-common.c \
	toolcommon/tool-cmdline-common.h

toolhostrange_libtoolhostrange_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libipmidetect/src \
	-D_GNU_SOURCE \
	-D_REENTRANT

toolhostrange_libtoolhostrange_la_LDFLAGS = \
	$(PTHREAD_LIBS)

toolhostrange_libtoolhostrange_la_SOURCES = \
	toolhostrange/pstdout.c \
	toolhostrange/pstdout.h \
	toolhostrange/tool-hostrange-common.c \
	toolhostrange/tool-hostrange-common.h

tooloem_libtooloem_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/libfreeipmi/include  \
	-D_GNU_SOURCE \
	-D_REENTRANT

tooloem_libtooloem_la_SOURCES = \
	tooloem/tool-oem-common.c \
	tooloem/tool-oem-common.h

toolsdr_libtoolsdr_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/libfreeipmi/include  \
	-D_GNU_SOURCE \
	-D_REENTRANT

toolsdr_libtoolsdr_la_SOURCES = \
	toolsdr/tool-sdr-cache-common.c \
	toolsdr/tool-sdr-cache-common.h

toolsensor_libtoolsensor_la_CPPFLAGS = \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/common/src/toolsdr \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

toolsensor_libtoolsensor_la_SOURCES = \
	toolsensor/tool-sensor-common.c \
	toolsensor/tool-sensor-common.h
