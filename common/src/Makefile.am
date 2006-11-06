noinst_HEADERS = \
	argp-common.h \
	bit-ops.h \
	err-wrappers.h \
	fiid-wrappers.h \
	freeipmi-portability.h \
	ipmi-common.h \
	ipmi-ping.h \
	md2.h \
	md5.h \
	common-utils.h \
	ipmi-sdr-cache-writes.h \
	ipmi-sdr-cache-reads.h \
	ipmi-sdr-api.h \
	ipmi-sensor-api.h \
	xmalloc.h \
	cbuf.h \
	conffile.h \
	fd.h \
	list.h \
	secure.h \
	timeval.h

noinst_LTLIBRARIES = \
	libargpcommon.la \
	libbitops.la \
	libfreeipmiportability.la \
	libipmicommon.la \
	libipmiping.la \
	libmd2.la \
	libmd5.la \
	libsdr.la \
	libsensor.la \
	libxmalloc.la \
	libllnlcommon.la

libargpcommon_la_SOURCES = argp-common.c
libargpcommon_la_CPPFLAGS = -D_GNU_SOURCE -I$(srcdir)/../../libfreeipmi/include 

libbitops_la_SOURCES = bit-ops.c
libbitops_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

libfreeipmiportability_la_SOURCES = freeipmi-portability.c
libfreeipmiportability_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

libipmicommon_la_SOURCES = ipmi-common.c
libipmicommon_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

libipmiping_la_SOURCES = ipmi-ping.c
libipmiping_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

libmd2_la_SOURCES = md2.c
libmd2_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

libmd5_la_SOURCES = md5.c
libmd5_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

libsdr_la_SOURCES = common-utils.c ipmi-sdr-cache-writes.c ipmi-sdr-cache-reads.c \
	ipmi-sdr-api.c
libsdr_la_CPPFLAGS = -D_GNU_SOURCE -I$(srcdir)/../../libfreeipmi/include
libsdr_la_LIBADD = ./libbitops.la

libsensor_la_SOURCES = ipmi-sensor-api.c
libsensor_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include

libxmalloc_la_SOURCES = xmalloc.c
libxmalloc_la_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include 

libllnlcommon_la_SOURCES = \
	cbuf.c \
	conffile.c \
	fd.c \
	list.c \
	secure.c \
	timeval.c


