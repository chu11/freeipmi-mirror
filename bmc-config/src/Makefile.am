sbin_PROGRAMS = bmc-config

bmc_config_SOURCES = \
	bmc-config.c \
	bmc-config-argp.c \
	bmc-config-map.c \
	bmc-config-sections.c \
	bmc-config-parser.c \
	bmc-config-wrapper.c \
	bmc-config-utils.c \
	bmc-config-checkout.c \
	bmc-config-commit.c \
	bmc-config-diff.c \
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
	bmc-config-validate.c

bmc_config_LDADD = ../../common/src/libcmdlineparsecommon.la \
                 ../../common/src/libtoolcommon.la \
                 ../../libfreeipmi/src/libfreeipmi.la \
		 ../../common/src/libconfigcommon.la

AM_CFLAGS = -Wall
AM_CPPFLAGS = -I$(srcdir)/../../common/src -D_GNU_SOURCE \
		-I$(srcdir)/../../libfreeipmi/include 

noinst_HEADERS = bmc-config.h \
		 bmc-config-argp.h \
		 bmc-config-checkout.h \
		 bmc-config-commit.h \
		 bmc-config-common.h \
		 bmc-config-wrapper.h \
		 bmc-config-diff.h \
		 bmc-config-utils.h \
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
		 bmc-config-map.h \
		 bmc-config-parser.h \
		 bmc-config-sections.h \
		 bmc-config-validate.h

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libcmdlineparsecommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libconfigcommon.la : force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
