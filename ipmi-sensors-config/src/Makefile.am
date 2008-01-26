sbin_PROGRAMS = ipmi-sensors-config

ipmi_sensors_config_SOURCES = \
	ipmi-sensors-config.c \
	ipmi-sensors-config-argp.c \
	ipmi-sensors-config-sections.c

# ipmi_sensors_config doesn't use hostrange, but
# we need pstdout for linking reasons
# we need miscutils for hostlist lib in pstdout too
ipmi_sensors_config_LDADD = ../../common/src/libtoolcommon.la \
			    ../../common/src/libhostrange.la \
			    ../../common/src/libmiscutils.la \
			    ../../libfreeipmi/src/libfreeipmi.la \
			    ../../common/src/libconfigcommon.la

AM_CFLAGS = -Wall
AM_CPPFLAGS = -I$(srcdir)/../../common/src -D_GNU_SOURCE \
		-I$(srcdir)/../../libfreeipmi/include 

noinst_HEADERS = ipmi-sensors-config.h \
		 ipmi-sensors-config-argp.h \
		 ipmi-sensors-config-sections.h 

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libconfigcommon.la : force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
