#noinst_PROGRAMS = bmc-config
sbin_PROGRAMS = bmc-config

bmc_config_SOURCES = \
	bmc-config.c \
	bmc-argp.c \
	bmc-map.c \
	bmc-sections.c \
	bmc-parser.c \
	bmc-config-api.c \
	bmc-ipmi-wrapper.c \
	bmc-checkout.c \
	bmc-commit.c \
	bmc-diff.c \
	bmc-user-sections.c \
	bmc-lan-channel-section.c \
	bmc-serial-channel-section.c \
	bmc-lan-conf-section.c \
	bmc-lan-conf-auth-section.c \
	bmc-lan-conf-security-keys-section.c \
	bmc-lan-conf-misc-section.c \
	bmc-rmcpplus-conf-privilege-section.c \
	bmc-serial-conf-section.c \
	bmc-pef-conf-section.c \
	bmc-sol-conf-section.c \
	bmc-misc-section.c

bmc_config_LDADD = ../../common/src/libargpcommon.la \
                 ../../common/src/libipmicommon.la \
                 ../../libfreeipmi/src/libfreeipmi.la

AM_CFLAGS = -Wall
AM_CPPFLAGS = -I$(srcdir)/../../common/src -D_GNU_SOURCE \
		-I$(srcdir)/../../libfreeipmi/include 

noinst_HEADERS = bmc-config.h \
		 bmc-argp.h \
		 bmc-checkout.h \
		 bmc-commit.h \
		 bmc-common.h \
		 bmc-config-api.h \
		 bmc-diff.h \
		 bmc-ipmi-wrapper.h \
		 bmc-map.h \
		 bmc-parser.h \
		 bmc-sections.h \
		 bmc-types.h

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libargpcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libipmicommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
