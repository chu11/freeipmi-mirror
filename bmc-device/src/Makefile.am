sbin_PROGRAMS=bmc-device
bmc_device_SOURCES=bmc-device-argp.c bmc-device.c 
bmc_device_LDADD = ../../common/src/libtoolcommon.la \
		   ../../common/src/libhostrange.la \
		   ../../common/src/libhostmap.la \
		   ../../common/src/libmiscutils.la \
		   ../../ipmidetect/src/libipmidetect/libipmidetect.la \
		   ../../libfreeipmi/src/libfreeipmi.la
 
noinst_HEADERS=bmc-device-argp.h bmc-device.h
 
AM_CFLAGS = -Wall
AM_CPPFLAGS = -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/include -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" \
	-D_REENTRANT 
 
../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libhostrange.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libmiscutils.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../ipmidetect/src/libipmidetect/libipmidetect.la : force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libhostmap.la : force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
