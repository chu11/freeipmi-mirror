noinst_HEADERS = \
	bmc-watchdog.h \
	bmc-watchdog-argp.h

AM_CPPFLAGS = \
	-I$(top_srcdir)/common/toolcommon \
	-I$(top_srcdir)/common/debugutil \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT \
	-DBMC_WATCHDOG_LOCALSTATEDIR='"$(localstatedir)"'

sbin_PROGRAMS = bmc-watchdog

bmc_watchdog_LDADD = \
	$(top_builddir)/common/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/debugutil/libdebugutil.la \
	$(top_builddir)/common/miscutil/libmiscutils.la \
	$(top_builddir)/common/parsecommon/libparsecommon.la \
	$(top_builddir)/common/portability/libfreeipmiportability.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

bmc_watchdog_SOURCES = \
	bmc-watchdog.c \
	bmc-watchdog-argp.c

$(top_builddir)/common/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/debugutil/libdebugutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/miscutil/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/parsecommon/libparsecommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:
