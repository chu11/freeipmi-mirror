sbin_PROGRAMS = pef-config

pef_config_SOURCES = \
	pef-config.c \
	pef-config-argp.c \
	pef-config-checkout.c \
	pef-config-commit.c \
	pef-config-pef-conf-section.c \
	pef-config-community-string.c \
	pef-config-lan-alert-destination.c \
	pef-config-alert-string.c \
	pef-config-alert-policy-table.c \
	pef-config-event-filter-table.c \
	pef-config-diff.c \
	pef-config-info.c \
	pef-config-map.c \
	pef-config-parser.c \
	pef-config-sections.c \
	pef-config-utils.c \
	pef-config-validate.c \
	pef-config-wrapper.c
 
pef_config_LDADD = ../../common/src/libargpcommon.la \
		   ../../common/src/libtoolcommon.la \
		   ../../libfreeipmi/src/libfreeipmi.la \
		   ../../common/src/libfreeipmiportability.la \
		   ../../common/src/libsdr.la \
		   ../../common/src/libsensor.la 
 
noinst_HEADERS = \
	pef-config.h \
	pef-config-argp.h \
	pef-config-checkout.h \
	pef-config-commit.h \
	pef-config-common.h \
	pef-config-pef-conf-section.h \
	pef-config-community-string.h \
	pef-config-lan-alert-destination.h \
	pef-config-alert-string.h \
	pef-config-alert-policy-table.h \
	pef-config-event-filter-table.h \
	pef-config-diff.h \
	pef-config-info.h \
	pef-config-map.h \
	pef-config-parser.h \
	pef-config-sections.h \
	pef-config-utils.h \
	pef-config-validate.h \
	pef-config-wrapper.h
 
AM_CFLAGS = -Wall
AM_CPPFLAGS = -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/include -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\"
 
../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
../../common/src/libargpcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libfreeipmiportability.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libsdr.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libsensor.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
