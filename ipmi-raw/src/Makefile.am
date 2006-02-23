sbin_PROGRAMS=ipmi-raw
ipmi_raw_SOURCES=ipmi-raw-argp.c ipmi-raw.c 
 
ipmi_raw_LDADD = ../../common/src/libargpcommon.la \
		 ../../common/src/libipmicommon.la \
		 ../../libfreeipmi/src/libfreeipmi.la
 
noinst_HEADERS=ipmi-raw-argp.h 
 
AM_CFLAGS = -Wall -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/src -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" \
	-DWITH_LSD_FATAL_ERROR_FUNC=1 -DWITH_LSD_NOMEM_ERROR_FUNC=1
 
../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
../../common/src/libargpcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libipmicommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
