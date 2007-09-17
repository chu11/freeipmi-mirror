noinst_HEADERS = \
	cmdline-parse-common.h \
	bit-ops.h \
	err-wrappers.h \
	fiid-wrappers.h \
	udm-fiid-wrappers.h \
	udm-err-wrappers.h \
	freeipmi-portability.h \
	tool-common.h \
	ipmi-ping.h \
	md2.h \
	md5.h \
	common-utils.h \
	ipmi-sdr-cache-writes.h \
	ipmi-sdr-cache-reads.h \
	ipmi-sdr-cache.h \
	ipmi-sdr-cache-defs.h \
	ipmi-sensor-api.h \
	xmalloc.h \
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
	config-comment.h 

noinst_LTLIBRARIES = \
	libcmdlineparsecommon.la \
	libbitops.la \
	libfreeipmiportability.la \
	libtoolcommon.la \
	libipmiping.la \
	libmd2.la \
	libmd5.la \
	libsdr.la \
	libsensor.la \
	libxmalloc.la \
	libcbuf.la \
	libllnlcommon.la \
	libpstdout.la \
	libhostrange.la \
	libconfigcommon.la

libcmdlineparsecommon_la_SOURCES = cmdline-parse-common.c
libcmdlineparsecommon_la_CPPFLAGS = -D_GNU_SOURCE -I$(srcdir)/../../libfreeipmi/include 

libbitops_la_SOURCES = bit-ops.c
libbitops_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

libfreeipmiportability_la_SOURCES = freeipmi-portability.c
libfreeipmiportability_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

libtoolcommon_la_SOURCES = tool-common.c
libtoolcommon_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

libipmiping_la_SOURCES = ipmi-ping.c
libipmiping_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

libmd2_la_SOURCES = md2.c
libmd2_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

libmd5_la_SOURCES = md5.c
libmd5_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

libsdr_la_SOURCES = common-utils.c ipmi-sdr-cache-writes.c ipmi-sdr-cache-reads.c \
	ipmi-sdr-cache.c
libsdr_la_CPPFLAGS = -D_GNU_SOURCE \
		     -D_REENTRANT \
		     -I$(srcdir)/../../libfreeipmi/include
libsdr_la_LIBADD = ./libbitops.la

libsensor_la_SOURCES = ipmi-sensor-api.c
libsensor_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include

libxmalloc_la_SOURCES = xmalloc.c
libxmalloc_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

libcbuf_la_SOURCES = cbuf.c

libllnlcommon_la_SOURCES = \
	conffile.c \
	fd.c \
	hostlist.c \
	list.c \
	secure.c \
	timeval.c

libpstdout_la_SOURCES = pstdout.c
libpstdout_la_CPPFLAGS = -D_REENTRANT
libpstdout_la_LDFLAGS = $(PTHREAD_LIBS)

libhostrange_la_SOURCES = hostrange.c
libhostrange_la_CPPFLAGS = -I$(srcdir)/../../ipmidetect/src/libipmidetect/

libconfigcommon_la_SOURCES = config-parse.c config-comment.c

../../ipmidetect/src/libipmidetect/libipmidetect.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
