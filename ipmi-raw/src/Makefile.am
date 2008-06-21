noinst_HEADERS = \
	ipmi-raw.h \
	ipmi-raw-argp.h

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT 

sbin_PROGRAMS = ipmi-raw
 
ipmi_raw_LDADD = \
	$(top_builddir)/common/src/libtoolcommon.la

ipmi_raw_SOURCES = \
	ipmi-raw.c \
	ipmi-raw-argp.c

$(top_builddir)/common/src/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
