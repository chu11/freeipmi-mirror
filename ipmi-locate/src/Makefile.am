sbin_PROGRAMS=ipmi-locate
ipmi_locate_SOURCES=ipmi-locate-argp.c ipmi-locate.c
 
ipmi_locate_LDADD = ../../common/src/libipmicommon.la \
		    ../../libfreeipmi/src/libfreeipmi.la
 
noinst_HEADERS=ipmi-locate-argp.h
 
AM_CFLAGS = -Wall
AM_CPPFLAGS = -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/include -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" \
	-DWITH_LSD_FATAL_ERROR_FUNC=1 -DWITH_LSD_NOMEM_ERROR_FUNC=1
 
../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
../../common/src/libipmicommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
