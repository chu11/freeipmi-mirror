sbin_PROGRAMS = ipmi-sel

ipmi_sel_SOURCES = \
	ipmi-sel.c \
	ipmi-sel-argp.c \
	ipmi-sel-entry.c

ipmi_sel_LDADD = ../../common/src/libtoolcommon.la \
		 ../../common/src/libsensor.la \
		 ../../common/src/libhostrange.la \
		 ../../common/src/libmiscutils.la \
		 ../../ipmidetect/src/libipmidetect/libipmidetect.la \
		 ../../libfreeipmi/src/libfreeipmi.la \
		 ../../common/src/libfreeipmiportability.la
 
noinst_HEADERS = \
	ipmi-sel.h \
	ipmi-sel-argp.h \
	ipmi-sel-fiid.h \
	ipmi-sel-entry.h 
 
AM_CFLAGS = -Wall
AM_CPPFLAGS = -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/include -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" \
	-D_REENTRANT

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libsensor.la: force-dependency-check
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
