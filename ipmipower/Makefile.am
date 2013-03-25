sbin_PROGRAMS = ipmipower

ipmipower_CPPFLAGS = \
	-I$(top_srcdir)/common/toolcommon \
	-I$(top_srcdir)/common/debugutil \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
	-I$(top_builddir)/libipmidetect \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/include \
	-DWITH_LSD_FATAL_ERROR_FUNC=1 \
	-DWITH_LSD_NOMEM_ERROR_FUNC=1 \
	-D_GNU_SOURCE \
	-D_REENTRANT

ipmipower_LDADD = \
	$(top_builddir)/common/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/debugutil/libdebugutil.la \
	$(top_builddir)/common/miscutil/libmiscutil.la \
	$(top_builddir)/common/parsecommon/libparsecommon.la \
	$(top_builddir)/common/portability/libportability.la \
	$(top_builddir)/libipmidetect/libipmidetect.la \
	$(top_builddir)/libfreeipmi/libfreeipmi.la

ipmipower_SOURCES = \
	argv.c \
	argv.h \
	ipmipower.c \
	ipmipower.h \
	ipmipower_argp.c \
	ipmipower_argp.h \
	ipmipower_check.c \
	ipmipower_check.h \
	ipmipower_connection.c \
	ipmipower_connection.h \
	ipmipower_error.c \
	ipmipower_error.h \
	ipmipower_oem.c \
	ipmipower_oem.h \
	ipmipower_output.c \
	ipmipower_output.h \
	ipmipower_packet.c \
	ipmipower_packet.h \
	ipmipower_ping.c \
	ipmipower_ping.h \
	ipmipower_powercmd.c \
	ipmipower_powercmd.h \
	ipmipower_prompt.c \
	ipmipower_prompt.h \
	ipmipower_util.c \
	ipmipower_util.h
 
$(top_builddir)/common/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/debugutil/libdebugutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/miscutil/libmiscutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/parsecommon/libparsecommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/portability/libportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libipmidetect/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

install-exec-hook:
	$(install_sh) -d -m 755 $(DESTDIR)$(sbindir)
	$(LN_S) -f $(sbindir)/ipmipower $(DESTDIR)$(sbindir)/ipmi-power

uninstall-hook:
	$(RM) $(DESTDIR)$(sbindir)/ipmi-power
