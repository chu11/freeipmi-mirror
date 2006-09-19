sbin_PROGRAMS=ipmi-sel
ipmi_sel_SOURCES=ipmi-sel-argp.c ipmi-sel-wrapper.c ipmi-sel.c 
 
ipmi_sel_LDADD = ../../common/src/libargpcommon.la \
		 ../../common/src/libipmicommon.la \
		 ../../common/src/libsensor.la \
		 ../../common/src/libsdr.la \
		 ../../libfreeipmi/src/libfreeipmi.la \
		 ../../common/src/libfreeipmiportability.la
 
noinst_HEADERS=ipmi-sel-argp.h ipmi-sel-wrapper.h
 
AM_CFLAGS = -Wall
AM_CPPFLAGS = -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/include -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" \
	-DWITH_LSD_FATAL_ERROR_FUNC=1 -DWITH_LSD_NOMEM_ERROR_FUNC=1
 
../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
../../common/src/libargpcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libipmicommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libsensor.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libsdr.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libfreeipmiportability.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
