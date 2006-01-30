EXTRA_DIST = argp-common.h argp-common.c

noinst_HEADERS = ipmi-ping.h
noinst_LTLIBRARIES = libipmiping.la
libipmiping_la_SOURCES = ipmi-ping.c
libipmiping_la_CFLAGS = -DFREEIPMI_BUILD -I$(srcdir)/../../libfreeipmi/src 
libipmiping_la_LIBADD = ../../libfreeipmi/src/libfreeipmi.la

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:


