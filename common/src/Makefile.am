SUBDIRS = \
	configtool \
	debugutil \
	pingtool

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
	cbuf.h \
	conffile.h \
	error.h \
	fd.h \
	hash.h \
	hostlist.h \
	list.h \
	secure.h \
	timeval.h \
	thread.h

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
	libmiscutils.la

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

$(top_builddir)/libfreeipmi/src/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
