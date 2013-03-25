noinst_LTLIBRARIES = \
	libtoolcommon.la

libtoolcommon_la_LDFLAGS = \
	$(PTHREAD_LIBS)

libtoolcommon_la_CPPFLAGS = \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
	-I$(top_builddir)/libipmidetect \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

libtoolcommon_la_SOURCES = \
	pstdout.c \
	pstdout.h \
	tool-common.c \
	tool-common.h \
	tool-config-file-common.c \
	tool-config-file-common.h \
	tool-cmdline-common.c \
	tool-cmdline-common.h \
	tool-daemon-common.c \
	tool-daemon-common.h \
	tool-event-common.c \
	tool-event-common.h \
	tool-hostrange-common.c \
	tool-hostrange-common.h \
	tool-oem-common.c \
	tool-oem-common.h \
	tool-sdr-cache-common.c \
	tool-sdr-cache-common.h \
	tool-sensor-common.c \
	tool-sensor-common.h \
	tool-util-common.c \
	tool-util-common.h
