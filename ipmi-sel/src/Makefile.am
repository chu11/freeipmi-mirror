sbin_PROGRAMS=ipmi-sel
ipmi_sel_SOURCES=ipmi-sel-argp.c ipmi-sel-wrapper.c ipmi-sel.c 
ipmi_sel_LDADD = ../../common/src/libargpcommon.la \
		 ../../common/src/libtoolcommon.la \
		 ../../common/src/libsensor.la \
		 ../../common/src/libsdr.la \
		 ../../common/src/libpstdout.la \
		 ../../common/src/libhostrange.la \
		 ../../common/src/libcbuf.la \
		 ../../common/src/libllnlcommon.la \
		 ../../ipmidetect/src/libipmidetect/libipmidetect.la \
		 ../../libfreeipmi/src/libfreeipmi.la \
		 ../../common/src/libfreeipmiportability.la
 
noinst_HEADERS=ipmi-sel-argp.h ipmi-sel-wrapper.h ipmi-sel.h
 
AM_CFLAGS = -Wall
AM_CPPFLAGS = -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/include -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" \
	-D_REENTRANT

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
../../common/src/libargpcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libsensor.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libsdr.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libpstdout.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libfreeipmiportability.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libhostrange.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libcbuf.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libllnlcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../ipmidetect/src/libipmidetect/libipmidetect.la : force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
