noinst_HEADERS = \
	argv.h \
	ierror.h \
	ipmipower_check.h \
	ipmipower_argp.h \
	ipmipower_connection.h \
	ipmipower.h \
	ipmipower_output.h \
	ipmipower_packet.h \
	ipmipower_ping.h \
	ipmipower_powercmd.h \
	ipmipower_prompt.h \
	ipmipower_util.h \
	ipmipower_wrappers.h \
	wrappers.h
 
sbin_PROGRAMS = ipmipower

ipmipower_CPPFLAGS = \
	-I$(top_srcdir)/common/src/toolcommon \
	-I$(top_srcdir)/common/src/toolhostrange \
	-I$(top_srcdir)/common/src/debugutil \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/ipmidetect/src/libipmidetect \
	-I$(top_srcdir)/libfreeipmi/include \
	-DWITH_LSD_FATAL_ERROR_FUNC=1 \
	-DWITH_LSD_NOMEM_ERROR_FUNC=1 \
	-D_GNU_SOURCE \
	-D_REENTRANT

ipmipower_LDADD = \
	$(top_builddir)/common/src/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/src/toolhostrange/libtoolhostrange.la \
	$(top_builddir)/common/src/debugutil/libdebugutil.la \
	$(top_builddir)/common/src/miscutil/libmiscutils.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
	$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

ipmipower_SOURCES = \
	argv.c \
	ierror.c \
	ipmipower_check.c \
	ipmipower_argp.c \
	ipmipower_connection.c \
	ipmipower.c \
	ipmipower_output.c \
	ipmipower_packet.c \
	ipmipower_ping.c \
	ipmipower_powercmd.c \
	ipmipower_prompt.c \
	ipmipower_util.c \
	ipmipower_wrappers.c \
	wrappers.c
 
$(top_builddir)/common/src/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/toolhostrange/libtoolhostrange.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/debugutil/libdebugutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/miscutil/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:
