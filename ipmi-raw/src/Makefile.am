sbin_PROGRAMS=ipmi-raw
ipmi_raw_SOURCES=ipmi-raw-argp.c ipmi-raw.c 
ipmi_raw_LDADD = ../../common/src/libargpcommon.la \
		 ../../common/src/libipmicommon.la \
		 ../../libfreeipmi/src/libfreeipmi.la \
		 ../../common/src/libpstdout.la \
		 ../../common/src/libeliminate.la \
		 ../../common/src/libllnlcommon.la \
		 ../../ipmidetect/src/libipmidetect/libipmidetect.la \
		 ../../common/src/libfreeipmiportability.la
 
noinst_HEADERS=ipmi-raw-argp.h ipmi-raw.h
 
AM_CFLAGS = -Wall
AM_CPPFLAGS = -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/include -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" \
	-D_REENTRANT 
 
../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
../../common/src/libargpcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libipmicommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libfreeipmiportability.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libpstdout.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libeliminate.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libllnlcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../ipmidetect/src/libipmidetect/libipmidetect.la : force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
