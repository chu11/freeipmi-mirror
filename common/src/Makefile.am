noinst_HEADERS = \
	freeipmi-portability.h \
	tool-common.h \
	tool-cmdline-common.h \
	ipmi-ping.h \
	ipmi-sdr-cache-writes.h \
	ipmi-sdr-cache-reads.h \
	ipmi-sdr-cache.h \
	ipmi-sdr-cache-defs.h \
	string-utils.h \
	ipmi-sensor-api.h \
	cbuf.h \
	conffile.h \
	fd.h \
	hostlist.h \
	list.h \
	secure.h \
	timeval.h \
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
	config-util.h \
	config-validate.h

noinst_LTLIBRARIES = \
	libfreeipmiportability.la \
	libtoolcommon.la \
	libipmiping.la \
	libsdr.la \
	libsensor.la \
	libllnlcommon.la \
	libsecure.la \
	libpstdout.la \
	libhostrange.la \
	libconfigcommon.la

libfreeipmiportability_la_SOURCES = freeipmi-portability.c
libfreeipmiportability_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

libtoolcommon_la_SOURCES = tool-cmdline-common.c tool-common.c
libtoolcommon_la_CPPFLAGS = -D_GNU_SOURCE -I$(srcdir)/../../libfreeipmi/include 

libipmiping_la_SOURCES = ipmi-ping.c
libipmiping_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

libsdr_la_SOURCES = \
	string-utils.c \
	ipmi-sdr-cache-writes.c \
	ipmi-sdr-cache-reads.c \
	ipmi-sdr-cache.c
libsdr_la_CPPFLAGS = -D_GNU_SOURCE \
		     -D_REENTRANT \
		     -I$(srcdir)/../../libfreeipmi/include

libsensor_la_SOURCES = ipmi-sensor-api.c
libsensor_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include

libllnlcommon_la_SOURCES = \
	cbuf.c \
	conffile.c \
	fd.c \
	hostlist.c \
	list.c \
	secure.c \
	timeval.c

libsecure_la_SOURCES = secure.c

libpstdout_la_SOURCES = pstdout.c
libpstdout_la_CPPFLAGS = -D_REENTRANT
libpstdout_la_LDFLAGS = $(PTHREAD_LIBS)

libhostrange_la_SOURCES = hostrange.c
libhostrange_la_CPPFLAGS = -I$(srcdir)/../../ipmidetect/src/libipmidetect/

libconfigcommon_la_SOURCES = \
	config-comment.c \
	config-checkout.c \
	config-commit.c \
	config-diff.c \
	config-fiid.c \
	config-parse.c \
	config-pef-conf-section.c \
	config-section.c \
	config-util.c \
	config-validate.c
libconfigcommon_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include

../../ipmidetect/src/libipmidetect/libipmidetect.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
