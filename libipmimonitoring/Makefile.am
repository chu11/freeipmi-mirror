SUBDIRS = src

MANS_CPP = \
	libipmimonitoring.3

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	$(CPP) -w -C -P -I. $@.pre  $@

dist_man_MANS = \
	$(MANS_CPP) \
	ipmi_monitoring_sensors.conf.5 \
	ipmimonitoring_sensors.conf.5 \
	ipmimonitoring.conf.5 \
	libipmimonitoring.conf.5

if WITH_PKG_CONFIG
pkgconfig_DATA = libipmimonitoring.pc
endif

CLEANFILES = $(MANS_CPP) *~

DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P
