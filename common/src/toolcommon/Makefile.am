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
	$(top_builddir)/common/src/miscutil/libmiscutils.la \
	$(top_builddir)/common/src/portability/libfreeipmiportability.la \
	$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la \
	$(top_builddir)/libfreeipmi/src/libfreeipmi.la

libtoolcommon_la_LDFLAGS = \
	$(PTHREAD_LIBS)

libtoolcommon_la_CPPFLAGS = \
	-D_GNU_SOURCE \
	-D_REENTRANT \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/ipmidetect/src/libipmidetect/ \
	-I$(top_srcdir)/libfreeipmi/include 

libtoolcommon_la_SOURCES = \
	pstdout.c \
	tool-common.c \
	tool-config-file-common.c \
	tool-cmdline-common.c \
	tool-hostrange-common.c \
	tool-sdr-cache-common.c \
	tool-sensor-common.c

$(top_builddir)/common/src/miscutil/libmiscutils.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/src/portability/libfreeipmiportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/ipmidetect/src/libipmidetect/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/src/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:
