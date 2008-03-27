sbin_PROGRAMS=ipmi-oem
ipmi_oem_SOURCES=ipmi-oem-argp.c ipmi-oem.c 
ipmi_oem_LDADD = ../../common/src/libtoolcommon.la \
		 ../../libfreeipmi/src/libfreeipmi.la \
		 ../../common/src/libhostrange.la \
		 ../../common/src/libmiscutils.la \
		 ../../ipmidetect/src/libipmidetect/libipmidetect.la \
		 ../../common/src/libfreeipmiportability.la
 
noinst_HEADERS=ipmi-oem-argp.h ipmi-oem.h
 
AM_CFLAGS = -Wall
AM_CPPFLAGS = -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/include -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" \
	-D_REENTRANT 
 
../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libfreeipmiportability.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libhostrange.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libmiscutils.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../ipmidetect/src/libipmidetect/libipmidetect.la : force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
