AM_CPPFLAGS = \
	-I$(top_srcdir)/libipmimonitoring/src/ \
	-D_GNU_SOURCE \
	-D_REENTRANT 

noinst_PROGRAMS = ipmimonitoring-sel ipmimonitoring-sensors

ipmimonitoring_sel_LDADD = \
	$(top_builddir)/libipmimonitoring/src/libipmimonitoring.la
 
ipmimonitoring_sel_SOURCES = \
	ipmimonitoring-sel.c

ipmimonitoring_sensors_LDADD = \
	$(top_builddir)/libipmimonitoring/src/libipmimonitoring.la
 
ipmimonitoring_sensors_SOURCES = \
	ipmimonitoring-sensors.c

$(top_builddir)/libipmimonitoring/src/libipmimonitoring.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

EXTRA_DIST = ipmimonitoring-sel.c ipmimonitoring-sensors.c
