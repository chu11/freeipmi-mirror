noinst_HEADERS = \
	freeipmi-portability.h \
	tool-common.h \
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
	config-common.h \
	config-comment.h \
	config-checkout.h \
	config-commit.h \
	config-diff.h \
	config-fiid.h \
	config-parse.h \
	config-pef-conf-section.h \
	config-section.h \
	config-utils.h \
	config-validate.h

noinst_LTLIBRARIES = \
	libfreeipmiportability.la \
	libtoolcommon.la \
	libipmiping.la \
	libmiscutils.la \
	libhostrange.la \
	libconfigcommon.la

libfreeipmiportability_la_SOURCES = freeipmi-portability.c
libfreeipmiportability_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

libtoolcommon_la_SOURCES = \
	tool-common.c \
	tool-cmdline-common.c \
	tool-sdr-cache-common.c \
	tool-sensor-common.c
libtoolcommon_la_CPPFLAGS = \
	-D_GNU_SOURCE \
	-D_REENTRANT \
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

libconfigcommon_la_SOURCES = \
	config-comment.c \
	config-checkout.c \
	config-commit.c \
	config-diff.c \
	config-fiid.c \
	config-parse.c \
	config-pef-conf-section.c \
	config-section.c \
	config-utils.c \
	config-validate.c
libconfigcommon_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include
