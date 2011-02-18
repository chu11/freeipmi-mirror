sbin_PROGRAMS = rmcpping

AM_CPPFLAGS = \
	-I$(top_srcdir)/common/src/pingtool \
	-I$(top_srcdir)/common/src/debugutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE

rmcpping_LDADD = \
	$(top_builddir)/common/src/pingtool/libpingtoolcommon.la \
	$(top_builddir)/common/src/debugutil/libdebugutil.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

rmcpping_SOURCES = rmcpping.c

$(top_builddir)/common/src/pingtool/libpingtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/debugutil/libdebugutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

install-exec-hook:
	$(install_sh) -d -m 755 $(DESTDIR)$(sbindir)
	$(LN_S) -f $(sbindir)/rmcpping $(DESTDIR)$(sbindir)/rmcp-ping

uninstall-hook:
	$(RM) $(DESTDIR)$(sbindir)/rmcp-ping
