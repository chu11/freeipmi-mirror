noinst_HEADERS = \
	ipmi-chassis.h \
	ipmi-chassis-argp.h

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

sbin_PROGRAMS = ipmi-chassis

ipmi_chassis_LDADD = \
	$(top_builddir)/common/src/libtoolcommon.la

ipmi_chassis_SOURCES = \
	ipmi-chassis.c \
	ipmi-chassis-argp.c

$(top_builddir)/common/src/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
