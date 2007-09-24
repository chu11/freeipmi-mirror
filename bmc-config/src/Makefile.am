sbin_PROGRAMS = bmc-config

bmc_config_SOURCES = \
	bmc-config.c \
	bmc-config-argp.c \
	bmc-config-map.c \
	bmc-config-sections.c \
	bmc-config-wrapper.c \
	bmc-config-utils.c \
	bmc-config-user-sections.c \
	bmc-config-lan-channel-section.c \
	bmc-config-serial-channel-section.c \
	bmc-config-lan-conf-section.c \
	bmc-config-lan-conf-auth-section.c \
	bmc-config-lan-conf-security-keys-section.c \
	bmc-config-lan-conf-misc-section.c \
	bmc-config-rmcpplus-conf-privilege-section.c \
	bmc-config-serial-conf-section.c \
	bmc-config-pef-conf-section.c \
	bmc-config-sol-conf-section.c \
	bmc-config-misc-section.c \
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
		 bmc-config-wrapper.h \
		 bmc-config-utils.h \
		 bmc-config-user-sections.h \
		 bmc-config-lan-channel-section.h \
		 bmc-config-lan-conf-auth-section.h \
		 bmc-config-lan-conf-misc-section.h \
		 bmc-config-lan-conf-section.h \
		 bmc-config-lan-conf-security-keys-section.h \
		 bmc-config-rmcpplus-conf-privilege-section.h \
		 bmc-config-serial-channel-section.h \
		 bmc-config-serial-conf-section.h \
		 bmc-config-pef-conf-section.h \
		 bmc-config-sol-conf-section.h \
		 bmc-config-misc-section.h \
		 bmc-config-map.h \
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
