noinst_HEADERS = \
	freeipmi-portability.h \
	freeipmi-argp-fmtstream.h \
	freeipmi-argp-namefrob.h \
	freeipmi-argp.h \
	tool-common.h \
	tool-config-file-common.h \
	tool-cmdline-common.h \
	tool-hostrange-common.h \
	tool-fiid-wrappers.h \
	tool-sdr-cache-common.h \
	tool-sensor-common.h \
	pstdout.h \
	ping-tool-common.h \
	cbuf.h \
	conffile.h \
	error.h \
	fd.h \
	hash.h \
	hostlist.h \
	list.h \
	secure.h \
	timeval.h \
	thread.h \
	config-tool-argp.h \
	config-tool-common.h \
	config-tool-comment.h \
	config-tool-checkout.h \
	config-tool-commit.h \
	config-tool-diff.h \
	config-tool-parse.h \
	config-tool-pef-conf-section.h \
	config-tool-section.h \
	config-tool-utils.h \
	config-tool-validate.h \
	debug-common.h

if WITH_FREEIPMI_ARGP
FREEIPMI_ARG_SOURCES = \
	freeipmi-argp-ba.c \
	freeipmi-argp-eexst.c \
	freeipmi-argp-fmtstream.c \
	freeipmi-argp-help.c \
	freeipmi-argp-parse.c \
	freeipmi-argp-pv.c \
	freeipmi-argp-pvh.c
endif

noinst_LTLIBRARIES = \
	libfreeipmiportability.la \
	libtoolcommon.la \
	libpingtoolcommon.la \
	libmiscutils.la \
	libconfigtoolcommon.la \
	libdebugcommon.la

libfreeipmiportability_la_CPPFLAGS = \
	-I$(srcdir)/../../libfreeipmi/include 

libfreeipmiportability_la_SOURCES = \
	freeipmi-portability.c \
	$(FREEIPMI_ARG_SOURCES)

libtoolcommon_la_LIBADD = \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la \
	$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la \
	$(top_builddir)/common/src/libfreeipmiportability.la \
	$(top_builddir)/common/src/libmiscutils.la

libtoolcommon_la_LDFLAGS = \
	$(PTHREAD_LIBS)

libtoolcommon_la_CPPFLAGS = \
	-D_GNU_SOURCE \
	-D_REENTRANT \
	-I$(top_builddir)/ipmidetect/src/libipmidetect/ \
	-I$(top_builddir)/libfreeipmi/include 

libtoolcommon_la_SOURCES = \
	tool-common.c \
	tool-config-file-common.c \
	tool-cmdline-common.c \
	tool-hostrange-common.c \
	tool-sdr-cache-common.c \
	tool-sensor-common.c \
	pstdout.c

libdebugcommon_la_CPPFLAGS = \
	-I$(top_builddir)/libfreeipmi/include 

libdebugcommon_la_SOURCES = \
	debug-common.c

libpingtoolcommon_la_LIBADD = \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la \
	$(top_builddir)/common/src/libfreeipmiportability.la

libpingtoolcommon_la_CPPFLAGS = \
	-I$(top_builddir)/libfreeipmi/include 

libpingtoolcommon_la_SOURCES = \
	ping-tool-common.c

libmiscutils_la_SOURCES = \
	cbuf.c \
	conffile.c \
	error.c \
	fd.c \
	hash.c \
	hostlist.c \
	list.c \
	secure.c \
	thread.c \
	timeval.c

libconfigtoolcommon_la_LIBADD = \
	$(top_builddir)/common/src/libtoolcommon.la

libconfigtoolcommon_la_CPPFLAGS = \
	-I$(top_builddir)/libfreeipmi/include

libconfigtoolcommon_la_SOURCES = \
	config-tool-argp.c \
	config-tool-comment.c \
	config-tool-checkout.c \
	config-tool-commit.c \
	config-tool-diff.c \
	config-tool-parse.c \
	config-tool-pef-conf-section.c \
	config-tool-section.c \
	config-tool-utils.c \
	config-tool-validate.c

$(top_builddir)/libfreeipmi/src/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
