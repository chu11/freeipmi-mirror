sbin_PROGRAMS=ipmi-fru
ipmi_fru_SOURCES = ipmi-fru.c \
		   ipmi-fru-argp.c \
		   ipmi-fru-info-area.c \
		   ipmi-fru-multirecord-area.c \
		   ipmi-fru-util.c
ipmi_fru_LDADD = ../../common/src/libtoolcommon.la \
		 ../../common/src/libsdr.la \
		 ../../common/src/libpstdout.la \
		 ../../common/src/libhostrange.la \
		 ../../common/src/libmiscutils.la \
                 ../../ipmidetect/src/libipmidetect/libipmidetect.la \
	         ../../libfreeipmi/src/libfreeipmi.la \
		 ../../common/src/libfreeipmiportability.la
 
noinst_HEADERS = ipmi-fru.h \
		 ipmi-fru-argp.h \
		 ipmi-fru-fiid.h \
		 ipmi-fru-info-area.h \
		 ipmi-fru-multirecord-area.h \
		 ipmi-fru-util.h
 
AM_CFLAGS = -Wall
AM_CPPFLAGS = -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/include -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" \
	-D_REENTRANT 
 
../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libsdr.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libpstdout.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libhostrange.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libmiscutils.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../ipmidetect/src/libipmidetect/libipmidetect.la : force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libfreeipmiportability.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
