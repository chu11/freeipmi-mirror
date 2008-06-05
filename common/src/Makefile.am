noinst_HEADERS = \
	freeipmi-portability.h \
	tool-common.h \
	tool-config-file-common.h \
	tool-cmdline-common.h \
	tool-fiid-wrappers.h \
	tool-sdr-cache-common.h \
	tool-sensor-common.h \
	ipmi-ping.h \
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
	pstdout.h \
	hostrange.h \
	config-tool-argp.h \
	config-tool-common.h \
	config-tool-comment.h \
	config-tool-checkout.h \
	config-tool-commit.h \
	config-tool-diff.h \
	config-tool-fiid.h \
	config-tool-parse.h \
	config-tool-pef-conf-section.h \
	config-tool-section.h \
	config-tool-utils.h \
	config-tool-validate.h \
	debug-common.h \
	freeipmi-argp-fmtstream.h \
	freeipmi-argp-namefrob.h \
	freeipmi-argp.h

noinst_LTLIBRARIES = \
	libfreeipmiportability.la \
	libtoolcommon.la \
	libipmiping.la \
	libmiscutils.la \
	libhostrange.la \
	libconfigtoolcommon.la \
	libdebugcommon.la \
	libfreeipmiargp.la

libfreeipmiportability_la_SOURCES = freeipmi-portability.c
libfreeipmiportability_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

libtoolcommon_la_SOURCES = \
	tool-common.c \
	tool-config-file-common.c \
	tool-cmdline-common.c \
	tool-sdr-cache-common.c \
	tool-sensor-common.c
libtoolcommon_la_CPPFLAGS = \
	-D_GNU_SOURCE \
	-D_REENTRANT \
	-I$(srcdir)/../../libfreeipmi/include 

libdebugcommon_la_SOURCES = \
	debug-common.c
libdebugcommon_la_CPPFLAGS = \
	-I$(srcdir)/../../libfreeipmi/include 

libipmiping_la_SOURCES = ipmi-ping.c
libipmiping_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

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

libhostrange_la_SOURCES = pstdout.c hostrange.c
libhostrange_la_LDFLAGS = $(PTHREAD_LIBS)
libhostrange_la_CPPFLAGS = \
	-D_REENTRANT \
	-I$(srcdir)/../../ipmidetect/src/libipmidetect/

libconfigtoolcommon_la_SOURCES = \
	config-tool-argp.c \
	config-tool-comment.c \
	config-tool-checkout.c \
	config-tool-commit.c \
	config-tool-diff.c \
	config-tool-fiid.c \
	config-tool-parse.c \
	config-tool-pef-conf-section.c \
	config-tool-section.c \
	config-tool-utils.c \
	config-tool-validate.c
libconfigtoolcommon_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include

libfreeipmiargp_la_SOURCES = \
	freeipmi-argp-ba.c \
	freeipmi-argp-eexst.c \
	freeipmi-argp-fmtstream.c \
	freeipmi-argp-help.c \
	freeipmi-argp-parse.c \
	freeipmi-argp-pv.c \
	freeipmi-argp-pvh.c
