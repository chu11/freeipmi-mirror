sbin_PROGRAMS = rmcpping

AM_CPPFLAGS = \
	-I$(top_builddir)/common/src/debugutil \
	-I$(top_builddir)/common/src/pingtool \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-D_GNU_SOURCE

rmcpping_LDADD = \
	$(top_builddir)/common/src/debugutil/libdebugutil.la \
	$(top_builddir)/common/src/pingtool/libpingtoolcommon.la

rmcpping_SOURCES = rmcpping.c

$(top_builddir)/common/src/debugutil/libdebugutil.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/pingtool/libpingtoolcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
