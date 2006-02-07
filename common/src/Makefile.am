EXTRA_DIST = argp-common.h argp-common.c

noinst_HEADERS = ipmi-ping.h argp-common.h
noinst_LTLIBRARIES = libipmiping.la libargpcommon.la

libipmiping_la_SOURCES = ipmi-ping.c
libipmiping_la_CFLAGS = -DFREEIPMI_BUILD -I$(srcdir)/../../libfreeipmi/src 
libipmiping_la_LIBADD = ../../libfreeipmi/src/libfreeipmi.la

libargpcommon_la_SOURCES = argp-common.c
libargpcommon_la_CFLAGS = -DFREEIPMI_BUILD -I$(srcdir)/../../libfreeipmi/src 

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:


