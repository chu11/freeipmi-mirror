noinst_HEADERS = \
	argv.h \
	ipmipower_check.h \
	ipmipower_argp.h \
	ipmipower_connection.h \
	ipmipower_error.h \
	ipmipower.h \
	ipmipower_output.h \
	ipmipower_packet.h \
	ipmipower_ping.h \
	ipmipower_powercmd.h \
	ipmipower_prompt.h \
	ipmipower_util.h
 
sbin_PROGRAMS = ipmipower

ipmipower_CPPFLAGS = \
	-I$(top_srcdir)/common/toolcommon \
	-I$(top_srcdir)/common/toolhostrange \
	-I$(top_srcdir)/common/debugutil \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libipmidetect/src \
	-I$(top_srcdir)/libfreeipmi/include \
	-DWITH_LSD_FATAL_ERROR_FUNC=1 \
	-DWITH_LSD_NOMEM_ERROR_FUNC=1 \
	-D_GNU_SOURCE \
	-D_REENTRANT

ipmipower_LDADD = \
	$(top_builddir)/common/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/toolhostrange/libtoolhostrange.la \
	$(top_builddir)/common/debugutil/libdebugutil.la \
	$(top_builddir)/common/miscutil/libmiscutils.la \
	$(top_builddir)/common/parsecommon/libparsecommon.la \
	$(top_builddir)/common/portability/libfreeipmiportability.la \
	$(top_builddir)/libipmidetect/src/libipmidetect.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

ipmipower_SOURCES = \
	argv.c \
	ipmipower_check.c \
	ipmipower_argp.c \
	ipmipower_connection.c \
	ipmipower_error.c \
	ipmipower.c \
	ipmipower_output.c \
	ipmipower_packet.c \
	ipmipower_ping.c \
	ipmipower_powercmd.c \
	ipmipower_prompt.c \
	ipmipower_util.c
 
$(top_builddir)/common/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/toolhostrange/libtoolhostrange.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/debugutil/libdebugutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/miscutil/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/parsecommon/libparsecommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libipmidetect/src/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

install-exec-hook:
	$(install_sh) -d -m 755 $(DESTDIR)$(sbindir)
	$(LN_S) -f $(sbindir)/ipmipower $(DESTDIR)$(sbindir)/ipmi-power

uninstall-hook:
	$(RM) $(DESTDIR)$(sbindir)/ipmi-power
