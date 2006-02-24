noinst_HEADERS = \
	argp-common.h \
	bit-ops.h \
	ipmi-common.h \
	ipmi-ping.h \
	md2.h \
	md5.h \
	xmalloc.h

noinst_LTLIBRARIES = \
	libargpcommon.la \
	libbitops.la \
	libipmicommon.la \
	libipmiping.la \
	libmd2.la \
	libmd5.la \
	libxmalloc.la 

libargpcommon_la_SOURCES = argp-common.c
libargpcommon_la_CFLAGS = -I$(srcdir)/../../libfreeipmi/src 

libbitops_la_SOURCES = bit-ops.c
libbitops_la_CFLAGS = -I$(srcdir)/../../libfreeipmi/src 

libipmicommon_la_SOURCES = ipmi-common.c
libipmicommon_la_CFLAGS = -I$(srcdir)/../../libfreeipmi/src 

libipmiping_la_SOURCES = ipmi-ping.c
libipmiping_la_CFLAGS = -I$(srcdir)/../../libfreeipmi/src 

libmd2_la_SOURCES = md2.c
libmd2_la_CFLAGS = -I$(srcdir)/../../libfreeipmi/src 

libmd5_la_SOURCES = md5.c
libmd5_la_CFLAGS = -I$(srcdir)/../../libfreeipmi/src 

libxmalloc_la_SOURCES = xmalloc.c
libxmalloc_la_CFLAGS = -I$(srcdir)/../../libfreeipmi/src 

