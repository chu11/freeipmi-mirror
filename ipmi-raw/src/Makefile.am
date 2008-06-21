noinst_HEADERS = \
	ipmi-raw.h \
	ipmi-raw-argp.h

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src/miscutil \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/common/src/toolcommon \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT 

sbin_PROGRAMS = ipmi-raw
 
ipmi_raw_LDADD = \
	$(top_builddir)/common/src/toolcommon/libtoolcommon.la

ipmi_raw_SOURCES = \
	ipmi-raw.c \
	ipmi-raw-argp.c

$(top_builddir)/common/src/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
