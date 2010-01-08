SUBDIRS = src

MANS_CPP = \
	ipmimonitoring.8 \
	libipmimonitoring.3 \
	ipmi_monitoring_sensors.conf.5 \
	ipmimonitoring_sensors.conf.5 \
	ipmimonitoring.conf.5 \
	libipmimonitoring.conf.5

$(MANS_CPP): $(MANS_CPP:%=%.pre)
	$(CPP) -C -P -I. $@.pre  $@

dist_man_MANS = $(MANS_CPP) 

CLEANFILES = $(MANS_CPP) *~

DISTCLEANFILES = $(MANS_CPP) $(MANS_CPP:%=%.pre) .deps/*.P
