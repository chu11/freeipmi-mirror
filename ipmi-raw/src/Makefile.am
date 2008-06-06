sbin_PROGRAMS=ipmi-raw
ipmi_raw_SOURCES=ipmi-raw-argp.c ipmi-raw.c 
ipmi_raw_LDADD = ../../common/src/libtoolcommon.la \
		 ../../libfreeipmi/src/libfreeipmi.la \
		 ../../common/src/libhostrange.la \
		 ../../common/src/libmiscutils.la \
		 ../../ipmidetect/src/libipmidetect/libipmidetect.la \
		 ../../common/src/libfreeipmiportability.la
 
noinst_HEADERS=ipmi-raw-argp.h ipmi-raw.h
 
AM_CPPFLAGS = -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/include -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" \
	-D_REENTRANT 
 
../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libfreeipmiportability.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libhostrange.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libmiscutils.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../ipmidetect/src/libipmidetect/libipmidetect.la : force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
