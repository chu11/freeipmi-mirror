noinst_HEADERS = \
	pstdout.h \
	tool-hostrange-common.h

noinst_LTLIBRARIES = \
	libtoolhostrange.la

libtoolhostrange_la_LDFLAGS = \
	$(PTHREAD_LIBS)

libtoolhostrange_la_CPPFLAGS = \
	-D_GNU_SOURCE \
	-D_REENTRANT \
	-I$(top_srcdir)/common/src/miscutil \
	-I$(top_srcdir)/common/src/portability \
	-I$(top_srcdir)/ipmidetect/src/libipmidetect/

libtoolhostrange_la_SOURCES = \
	pstdout.c \
	tool-hostrange-common.c
