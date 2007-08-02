sbin_PROGRAMS=bmc-info
bmc_info_SOURCES=bmc-info-argp.c bmc-info.c 
bmc_info_LDADD = ../../common/src/libcmdlineparsecommon.la \
		 ../../common/src/libtoolcommon.la \
		 ../../common/src/libpstdout.la \
		 ../../common/src/libhostrange.la \
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


../../common/src/libcmdlineparsecommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libpstdout.la: force-dependency-check
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
