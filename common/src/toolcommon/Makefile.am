noinst_HEADERS = \
	pstdout.h \
	tool-common.h \
	tool-config-file-common.h \
	tool-cmdline-common.h \
	tool-hostrange-common.h \
	tool-fiid-wrappers.h \
	tool-sdr-cache-common.h \
	tool-sensor-common.h

noinst_LTLIBRARIES = \
	libtoolcommon.la

libtoolcommon_la_LIBADD = \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la \
	$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
	$(top_builddir)/common/src/util/libmiscutils.la

libtoolcommon_la_LDFLAGS = \
	$(PTHREAD_LIBS)

libtoolcommon_la_CPPFLAGS = \
	-D_GNU_SOURCE \
	-D_REENTRANT \
	-I$(top_builddir)/common/src/portability \
	-I$(top_builddir)/common/src/util \
	-I$(top_builddir)/ipmidetect/src/libipmidetect/ \
	-I$(top_builddir)/libfreeipmi/include 

libtoolcommon_la_SOURCES = \
	pstdout.c \
	tool-common.c \
	tool-config-file-common.c \
	tool-cmdline-common.c \
	tool-hostrange-common.c \
	tool-sdr-cache-common.c \
	tool-sensor-common.c

$(top_builddir)/libfreeipmi/src/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

$(top_builddir)/common/src/util/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && make `basename $@`

force-dependency-check:
