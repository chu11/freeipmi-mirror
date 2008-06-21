noinst_HEADERS = \
	ipmi-oem.h \
	ipmi-oem-argp.h

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src/miscutil \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/common/src/toolcommon \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT 

sbin_PROGRAMS = ipmi-oem

ipmi_oem_LDADD = \
	$(top_builddir)/common/src/toolcommon/libtoolcommon.la

ipmi_oem_SOURCES = \
	ipmi-oem.c \
	ipmi-oem-argp.c

$(top_builddir)/common/src/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
