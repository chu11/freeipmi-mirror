noinst_HEADERS = \
	ipmi-sensors.h \
	ipmi-sensors-argp.h \
	ipmi-sensors-detailed-output.h \
	ipmi-sensors-oem-intel-node-manager.h \
	ipmi-sensors-output-common.h \
	ipmi-sensors-simple-output.h

AM_CPPFLAGS = \
	-I$(top_srcdir)/common/src/toolcommon \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/common/src/tooloem \
	-I$(top_srcdir)/common/src/toolsdr \
	-I$(top_srcdir)/common/src/toolsensor \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/parsecommon \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = ipmi-sensors

sbin_SCRIPTS = ipmimonitoring

ipmi_sensors_LDADD = \
	$(top_builddir)/common/src/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/src/toolhostrange/libtoolhostrange.la \
	$(top_builddir)/common/src/tooloem/libtooloem.la \
	$(top_builddir)/common/src/toolsdr/libtoolsdr.la \
	$(top_builddir)/common/src/toolsensor/libtoolsensor.la \
	$(top_builddir)/common/src/miscutil/libmiscutils.la \
	$(top_builddir)/common/src/parsecommon/libparsecommon.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
	$(top_builddir)/libipmidetect/src/libipmidetect.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la
 
ipmi_sensors_SOURCES = \
	ipmi-sensors.c \
	ipmi-sensors-argp.c \
	ipmi-sensors-detailed-output.c \
	ipmi-sensors-oem-intel-node-manager.c \
	ipmi-sensors-output-common.c \
	ipmi-sensors-simple-output.c

ipmimonitoring: ipmimonitoring.in
	sed -e 's:@IPMIMONITORINGSBINDIR@:$(sbindir):' $< > $@
	chmod +x ipmimonitoring

$(top_builddir)/common/src/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/toolhostrange/libtoolhostrange.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/tooloem/libtooloem.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/toolsdr/libtoolsdr.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/toolsensor/libtoolsensor.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/miscutil/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/parsecommon/libparsecommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libipmidetect/src/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

CLEANFILES = ipmimonitoring

EXTRA_DIST = ipmimonitoring.in
