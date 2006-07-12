EXTRA_DIST = argp-common.h argp-common.c

noinst_HEADERS = ipmi-ping.h argp-common.h ipmi-common.h

noinst_LTLIBRARIES = libipmiping.la libargpcommon.la libipmicommon.la

libipmiping_la_SOURCES = ipmi-ping.c
libipmiping_la_CFLAGS = -DFREEIPMI_BUILD -I$(srcdir)/../../libfreeipmi/src 

libargpcommon_la_SOURCES = argp-common.c
libargpcommon_la_CFLAGS = -D_GNU_SOURCE -DFREEIPMI_BUILD -I$(srcdir)/../../libfreeipmi/src 

libipmicommon_la_SOURCES = ipmi-common.c
libipmicommon_la_CFLAGS = -DFREEIPMI_BUILD -I$(srcdir)/../../libfreeipmi/src 

force-dependency-check:


