sbin_PROGRAMS = ipmi-sensors

sbin_SCRIPTS = ipmimonitoring

ipmi_sensors_CPPFLAGS = \
	-I$(top_srcdir)/common/toolcommon \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_REENTRANT

ipmi_sensors_LDADD = \
	$(top_builddir)/common/toolcommon/libtoolcommon.la \
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
	ipmi-sensors-oem-intel.c \
	ipmi-sensors-oem-intel.h \
	ipmi-sensors-oem-intel-node-manager.c \
	ipmi-sensors-oem-intel-node-manager.h \
	ipmi-sensors-oem-intel-s5500wb.c \
	ipmi-sensors-oem-intel-s5500wb.h \
	ipmi-sensors-oem-intel-s2600jf.c \
	ipmi-sensors-oem-intel-s2600jf.h \
	ipmi-sensors-oem-intel-s2600wp.c \
	ipmi-sensors-oem-intel-s2600wp.h \
	ipmi-sensors-oem-intel-quanta-qssc-s4r.c \
	ipmi-sensors-oem-intel-quanta-qssc-s4r.h \
	ipmi-sensors-oem-inventec.c \
	ipmi-sensors-oem-inventec.h \
	ipmi-sensors-oem-inventec-5441.c \
	ipmi-sensors-oem-inventec-5441.h \
	ipmi-sensors-oem-inventec-5442.c \
	ipmi-sensors-oem-inventec-5442.h \
	ipmi-sensors-oem-quanta.c \
	ipmi-sensors-oem-quanta.h \
	ipmi-sensors-oem-quanta-s99q.c \
	ipmi-sensors-oem-quanta-s99q.h \
	ipmi-sensors-oem-wistron.c \
	ipmi-sensors-oem-wistron.h \
	ipmi-sensors-oem-wistron-c6220.c \
	ipmi-sensors-oem-wistron-c6220.h \
	ipmi-sensors-output-common.c \
	ipmi-sensors-output-common.h \
	ipmi-sensors-simple-output.c \
	ipmi-sensors-simple-output.h

ipmimonitoring: ipmimonitoring.in
	sed -e 's:@IPMIMONITORINGSBINDIR@:$(sbindir):' $< > $@
	chmod +x ipmimonitoring

$(top_builddir)/common/toolcommon/libtoolcommon.la : force-dependency-check
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
