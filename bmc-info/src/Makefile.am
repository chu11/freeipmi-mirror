sbin_PROGRAMS=bmc-info
bmc_info_SOURCES=bmc-info-argp.c bmc-info.c 
bmc_info_LDADD = ../../common/src/libargpcommon.la \
		 ../../common/src/libipmicommon.la \
		 ../../common/src/libpstdout.la \
		 ../../common/src/libeliminate.la \
		 ../../common/src/libcbuf.la \
		 ../../common/src/libllnlcommon.la \
                 ../../ipmidetect/src/libipmidetect/libipmidetect.la \
	         ../../libfreeipmi/src/libfreeipmi.la
 
noinst_HEADERS=bmc-info-argp.h bmc-info.h
 
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

../../common/src/libpstdout.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libeliminate.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libcbuf.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libllnlcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../ipmidetect/src/libipmidetect/libipmidetect.la : force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
