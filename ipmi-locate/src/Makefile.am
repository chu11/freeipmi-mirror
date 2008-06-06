sbin_PROGRAMS=ipmi-locate
ipmi_locate_SOURCES=ipmi-locate-argp.c ipmi-locate.c
 
ipmi_locate_LDADD = ../../common/src/libtoolcommon.la \
		    ../../libfreeipmi/src/libfreeipmi.la
 
noinst_HEADERS=ipmi-locate-argp.h
 
AM_CPPFLAGS = -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/include -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\"
 
../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
