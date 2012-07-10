sbin_PROGRAMS = ipmi-oem

ipmi_oem_CPPFLAGS = \
	-I$(top_srcdir)/common/toolcommon \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT 

ipmi_oem_LDADD = \
	$(top_builddir)/common/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/miscutil/libmiscutil.la \
	$(top_builddir)/common/parsecommon/libparsecommon.la \
	$(top_builddir)/common/portability/libportability.la \
	$(top_builddir)/libipmidetect/libipmidetect.la \
	$(top_builddir)/libfreeipmi/libfreeipmi.la

ipmi_oem_SOURCES = \
	ipmi-oem.c \
	ipmi-oem.h \
	ipmi-oem-argp.c \
	ipmi-oem-argp.h \
	ipmi-oem-common.c \
	ipmi-oem-common.h \
	ipmi-oem-dell.c \
	ipmi-oem-dell.h \
	ipmi-oem-fujitsu.c \
	ipmi-oem-fujitsu.h \
	ipmi-oem-ibm.c \
	ipmi-oem-ibm.h \
	ipmi-oem-intel.c \
	ipmi-oem-intel.h \
	ipmi-oem-intelnm.c \
	ipmi-oem-intelnm.h \
	ipmi-oem-inventec.c \
	ipmi-oem-inventec.h \
	ipmi-oem-quanta.c \
	ipmi-oem-quanta.h \
	ipmi-oem-sun.c \
	ipmi-oem-sun.h \
	ipmi-oem-supermicro.c \
	ipmi-oem-supermicro.h \
	ipmi-oem-thirdparty.c \
	ipmi-oem-thirdparty.h \
	ipmi-oem-wistron.c \
	ipmi-oem-wistron.h

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
