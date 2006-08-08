noinst_PROGRAMS=ipmi-sensors
ipmi_sensors_SOURCES=ipmi-sensors.c sensors-very-verbose-display.c \
	ipmi-sensors-argp.c sensors-simple-display.c ipmi-sensors-utils.c \
	sensors-verbose-display.c
ipmi_sensors_LDADD = ../../common/src/libargpcommon.la \
	../../common/src/libipmicommon.la \
	../../common/src/libsdr.la \
	../../common/src/libsensor.la \
	../../libfreeipmi/src/libfreeipmi.la
 
noinst_HEADERS=ipmi-sensors-utils.h sensors-verbose-display.h \
	ipmi-sensors-argp.h sensors-simple-display.h sensors-very-verbose-display.h
 
AM_CFLAGS = -Wall -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/include -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" \
	-DWITH_LSD_FATAL_ERROR_FUNC=1 -DWITH_LSD_NOMEM_ERROR_FUNC=1
 
../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
../../common/src/libargpcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libipmicommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
