noinst_HEADERS = \
	ipmi-oem.h \
	ipmi-oem-argp.h \
	ipmi-oem-common.h \
	ipmi-oem-dell.h \
	ipmi-oem-fujitsu.h \
	ipmi-oem-ibm.h \
	ipmi-oem-intel.h \
	ipmi-oem-inventec.h \
	ipmi-oem-quanta.h \
	ipmi-oem-sun.h \
	ipmi-oem-supermicro.h

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

sbin_PROGRAMS = ipmi-oem

ipmi_oem_LDADD = \
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

ipmi_oem_SOURCES = \
	ipmi-oem.c \
	ipmi-oem-argp.c \
	ipmi-oem-common.c \
	ipmi-oem-dell.c \
	ipmi-oem-fujitsu.c \
	ipmi-oem-ibm.c \
	ipmi-oem-intel.c \
	ipmi-oem-inventec.c \
	ipmi-oem-quanta.c \
	ipmi-oem-sun.c \
	ipmi-oem-supermicro.c

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
