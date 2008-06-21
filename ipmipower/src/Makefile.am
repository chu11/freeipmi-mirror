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
	-I$(top_builddir)/ipmidetect/src/libipmidetect \
	-I$(top_builddir)/common/src \
	-I$(top_builddir)/common/src/debugutil \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-DWITH_LSD_FATAL_ERROR_FUNC=1 \
	-DWITH_LSD_NOMEM_ERROR_FUNC=1 \
	-D_GNU_SOURCE \
	-D_REENTRANT

ipmipower_LDADD = \
	$(top_builddir)/common/src/libtoolcommon.la \
	$(top_builddir)/common/src/debugutil/libdebugutil.la

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
 
$(top_builddir)/common/src/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/debugutil/libdebugutil.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
