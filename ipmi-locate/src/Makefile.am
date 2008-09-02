noinst_HEADERS = ipmi-locate-argp.h

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/common/src/toolcommon \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = ipmi-locate

ipmi_locate_LDADD = \
	$(top_builddir)/common/src/toolcommon/libtoolcommon.la

ipmi_locate_SOURCES = \
	ipmi-locate.c \
	ipmi-locate-argp.c

$(top_builddir)/common/src/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:
