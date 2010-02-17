AM_CPPFLAGS = \
	-I$(top_srcdir)/ipmimonitoring/src/libipmimonitoring/ \
	-D_GNU_SOURCE \
	-D_REENTRANT 

noinst_PROGRAMS = ipmimonitoring-sensors

ipmimonitoring_sensors_LDADD = \
	$(top_builddir)/ipmimonitoring/src/libipmimonitoring/libipmimonitoring.la
 
ipmimonitoring_sensors_SOURCES = \
	ipmimonitoring-sensors.c

$(top_builddir)/ipmimonitoring/src/libipmimonitoring/libipmimonitoring.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

EXTRA_DIST = ipmimonitoring-sensors.c
