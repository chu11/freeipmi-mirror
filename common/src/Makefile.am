EXTRA_DIST = argp-common.h argp-common.c

noinst_HEADERS = ipmi-ping.h argp-common.h ipmi-common.h bit-ops.h

noinst_LTLIBRARIES = libipmiping.la libargpcommon.la libipmicommon.la libbitops.la libxmalloc.la

libipmiping_la_SOURCES = ipmi-ping.c
libipmiping_la_CFLAGS = -I$(srcdir)/../../libfreeipmi/src 

libargpcommon_la_SOURCES = argp-common.c
libargpcommon_la_CFLAGS = -I$(srcdir)/../../libfreeipmi/src 

libipmicommon_la_SOURCES = ipmi-common.c
libipmicommon_la_CFLAGS = -I$(srcdir)/../../libfreeipmi/src 

libbitops_la_SOURCES = bit-ops.c
libbitops_la_CFLAGS = -I$(srcdir)/../../libfreeipmi/src 

libxmalloc_la_SOURCES = xmalloc.c
libxmalloc_la_CFLAGS = -I$(srcdir)/../../libfreeipmi/src 

force-dependency-check:


