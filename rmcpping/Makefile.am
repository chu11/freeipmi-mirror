sbin_PROGRAMS = rmcpping

rmcpping_CPPFLAGS = \
	-I$(top_srcdir)/common/pingtool \
	-I$(top_srcdir)/common/debugutil \
	-I$(top_srcdir)/common/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE

rmcpping_LDADD = \
	$(top_builddir)/common/pingtool/libpingtool.la \
	$(top_builddir)/common/debugutil/libdebugutil.la \
	$(top_builddir)/common/portability/libportability.la \
	$(top_builddir)/libfreeipmi/libfreeipmi.la

rmcpping_SOURCES = rmcpping.c

$(top_builddir)/common/pingtool/libpingtool.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/debugutil/libdebugutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/portability/libportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

install-exec-hook:
	$(install_sh) -d -m 755 $(DESTDIR)$(sbindir)
	$(LN_S) -f $(sbindir)/rmcpping $(DESTDIR)$(sbindir)/rmcp-ping

uninstall-hook:
	$(RM) $(DESTDIR)$(sbindir)/rmcp-ping
