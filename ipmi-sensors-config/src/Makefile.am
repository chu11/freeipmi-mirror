sbin_PROGRAMS = ipmi-sensors-config

ipmi_sensors_config_SOURCES = \
	ipmi-sensors-config.c \
	ipmi-sensors-config-argp.c \
	ipmi-sensors-config-sections.c \
	ipmi-sensors-config-threshold-section.c \
	ipmi-sensors-config-utils.c

# ipmi_sensors_config doesn't use hostrange, but
# we need pstdout for linking reasons
# we need miscutils for hostlist lib in pstdout too
ipmi_sensors_config_LDADD = ../../common/src/libtoolcommon.la \
			    ../../common/src/libhostrange.la \
			    ../../common/src/libmiscutils.la \
			    ../../libfreeipmi/src/libfreeipmi.la \
			    ../../common/src/libconfigtoolcommon.la

AM_CPPFLAGS = -I$(srcdir)/../../common/src -D_GNU_SOURCE \
		-I$(srcdir)/../../libfreeipmi/include 

noinst_HEADERS = ipmi-sensors-config.h \
		 ipmi-sensors-config-argp.h \
		 ipmi-sensors-config-sections.h \
		 ipmi-sensors-config-threshold-section.h \
		 ipmi-sensors-config-utils.h

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libconfigtoolcommon.la : force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libhostrange.la : force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libmiscutils.la : force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
