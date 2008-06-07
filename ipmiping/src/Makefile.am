AM_CPPFLAGS = \
	-I$(top_builddir)/common/src \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE

sbin_PROGRAMS = ipmiping

ipmiping_LDADD = \
	$(top_builddir)/common/src/libipmiping.la \
	$(top_builddir)/common/src/libfreeipmiportability.la \
	$(top_builddir)/common/src/libdebugcommon.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

ipmiping_SOURCES = ipmiping.c

$(top_builddir)/common/src/libipmipingcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/libdebugcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/libfreeipmi/src/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
