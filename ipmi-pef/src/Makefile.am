sbin_PROGRAMS = ipmi-pef

ipmi_pef_SOURCES = \
	ipmi-pef.c \
	ipmi-pef-argp.c \
	ipmi-pef-checkout.c \
	ipmi-pef-commit.c \
	ipmi-pef-pef-conf-section.c \
	ipmi-pef-community-string.c \
	ipmi-pef-lan-alert-destination.c \
	ipmi-pef-alert-policy-table.c \
	ipmi-pef-event-filter-table.c \
	ipmi-pef-diff.c \
	ipmi-pef-info.c \
	ipmi-pef-map.c \
	ipmi-pef-parser.c \
	ipmi-pef-sections.c \
	ipmi-pef-utils.c \
	ipmi-pef-validate.c \
	ipmi-pef-wrapper.c
 
ipmi_pef_LDADD = ../../common/src/libargpcommon.la \
		 ../../common/src/libipmicommon.la \
		 ../../libfreeipmi/src/libfreeipmi.la \
		 ../../common/src/libfreeipmiportability.la \
		 ../../common/src/libsdr.la \
		 ../../common/src/libsensor.la 
 
noinst_HEADERS = \
	ipmi-pef.h \
	ipmi-pef-argp.h \
	ipmi-pef-checkout.h \
	ipmi-pef-commit.h \
	ipmi-pef-common.h \
	ipmi-pef-pef-conf-section.h \
	ipmi-pef-community-string.h \
	ipmi-pef-lan-alert-destination.h \
	ipmi-pef-alert-policy-table.h \
	ipmi-pef-event-filter-table.h \
	ipmi-pef-diff.h \
	ipmi-pef-info.h \
	ipmi-pef-map.h \
	ipmi-pef-parser.h \
	ipmi-pef-sections.h \
	ipmi-pef-utils.h \
	ipmi-pef-validate.h \
	ipmi-pef-wrapper.h
 
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

../../common/src/libfreeipmiportability.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libsdr.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libsensor.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
