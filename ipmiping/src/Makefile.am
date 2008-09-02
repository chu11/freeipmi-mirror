AM_CPPFLAGS = \
	-I$(top_builddir)/common/src/debugutil \
	-I$(top_builddir)/common/src/pingtool \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE

sbin_PROGRAMS = ipmiping

ipmiping_LDADD = \
	$(top_builddir)/common/src/debugutil/libdebugutil.la \
	$(top_builddir)/common/src/pingtool/libpingtoolcommon.la

ipmiping_SOURCES = ipmiping.c

$(top_builddir)/common/src/debugutil/libdebugutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/pingtool/libpingtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:
