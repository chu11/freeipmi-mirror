sbin_PROGRAMS = ipmi-sensors

sbin_SCRIPTS = ipmimonitoring

ipmi_sensors_CPPFLAGS = \
	-I$(top_srcdir)/common/toolcommon \
	-I$(top_srcdir)/common/toolhostrange \
	-I$(top_srcdir)/common/tooloem \
	-I$(top_srcdir)/common/toolsdr \
	-I$(top_srcdir)/common/toolsensor \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

ipmi_sensors_LDADD = \
	$(top_builddir)/common/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/toolhostrange/libtoolhostrange.la \
	$(top_builddir)/common/tooloem/libtooloem.la \
	$(top_builddir)/common/toolsdr/libtoolsdr.la \
	$(top_builddir)/common/toolsensor/libtoolsensor.la \
	$(top_builddir)/common/miscutil/libmiscutil.la \
	$(top_builddir)/common/parsecommon/libparsecommon.la \
	$(top_builddir)/common/portability/libportability.la \
	$(top_builddir)/libipmidetect/libipmidetect.la \
	$(top_builddir)/libfreeipmi/libfreeipmi.la
 
ipmi_sensors_SOURCES = \
	ipmi-sensors.c \
	ipmi-sensors.h \
	ipmi-sensors-argp.c \
	ipmi-sensors-argp.h \
	ipmi-sensors-detailed-output.c \
	ipmi-sensors-detailed-output.h \
	ipmi-sensors-oem-intel-node-manager.c \
	ipmi-sensors-oem-intel-node-manager.h \
	ipmi-sensors-output-common.c \
	ipmi-sensors-output-common.h \
	ipmi-sensors-simple-output.c \
	ipmi-sensors-simple-output.h

ipmimonitoring: ipmimonitoring.in
	sed -e 's:@IPMIMONITORINGSBINDIR@:$(sbindir):' $< > $@
	chmod +x ipmimonitoring

$(top_builddir)/common/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/toolhostrange/libtoolhostrange.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/tooloem/libtooloem.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/toolsdr/libtoolsdr.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/toolsensor/libtoolsensor.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/miscutil/libmiscutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/parsecommon/libparsecommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/portability/libportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libipmidetect/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

CLEANFILES = ipmimonitoring

EXTRA_DIST = ipmimonitoring.in
