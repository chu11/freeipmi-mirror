#noinst_PROGRAMS = bmc-config
sbin_PROGRAMS = bmc-config

bmc_config_SOURCES = \
	bmc-config.c \
	bmc-config-argp.c \
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
	bmc-misc-section.c \
	bmc-validate.c

bmc_config_LDADD = ../../common/src/libargpcommon.la \
                 ../../common/src/libipmicommon.la \
                 ../../libfreeipmi/src/libfreeipmi.la

AM_CFLAGS = -Wall
AM_CPPFLAGS = -I$(srcdir)/../../common/src -D_GNU_SOURCE \
		-I$(srcdir)/../../libfreeipmi/include 

noinst_HEADERS = bmc-config.h \
		 bmc-config-argp.h \
		 bmc-checkout.h \
		 bmc-commit.h \
		 bmc-common.h \
		 bmc-config-api.h \
		 bmc-diff.h \
		 bmc-ipmi-wrapper.h \
		 bmc-user-sections.h \
		 bmc-lan-channel-section.h \
		 bmc-lan-conf-auth-section.h \
		 bmc-lan-conf-misc-section.h \
		 bmc-lan-conf-section.h \
		 bmc-lan-conf-security-keys-section.h \
		 bmc-rmcpplus-conf-privilege-section.h \
		 bmc-serial-channel-section.h \
		 bmc-serial-conf-section.h \
		 bmc-pef-conf-section.h \
		 bmc-sol-conf-section.h \
		 bmc-misc-section.h \
		 bmc-map.h \
		 bmc-parser.h \
		 bmc-sections.h \
		 bmc-validate.h

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libargpcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libipmicommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
