sbin_PROGRAMS=bmc-info
bmc_info_SOURCES=bmc-info-argp.c bmc-info.c 
 
bmc_info_LDADD = ../../common/src/libargpcommon.la \
		 ../../common/src/libipmicommon.la \
	         ../../libfreeipmi/src/libfreeipmi.la
 
noinst_HEADERS=bmc-info-argp.h 
 
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

force-dependency-check:
