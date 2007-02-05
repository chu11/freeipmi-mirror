sbin_PROGRAMS=ipmi-pef
ipmi_pef_SOURCES=ipmi-pef-argp.c ipmi-pef-wrapper.c ipmi-pef.c 
 
ipmi_pef_LDADD = ../../common/src/libargpcommon.la \
		 ../../common/src/libipmicommon.la \
		 ../../libfreeipmi/src/libfreeipmi.la \
		 ../../common/src/libfreeipmiportability.la \
		 ../../common/src/libsdr.la 
 
noinst_HEADERS=ipmi-pef-argp.h ipmi-pef-wrapper.h ipmi-pef.h
 
AM_CFLAGS = -Wall
AM_CPPFLAGS = -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/include -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\"
 
../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
../../common/src/libargpcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libipmicommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libfreeipmiportability.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
