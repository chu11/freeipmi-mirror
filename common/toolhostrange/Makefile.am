noinst_LTLIBRARIES = \
	libtoolhostrange.la

libtoolhostrange_la_LDFLAGS = \
	$(PTHREAD_LIBS)

libtoolhostrange_la_CPPFLAGS = \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/portability \
	-I$(top_srcdir)/libipmidetect \
	-D_GNU_SOURCE \
	-D_REENTRANT

libtoolhostrange_la_SOURCES = \
	pstdout.c \
	pstdout.h \
	tool-hostrange-common.c \
	tool-hostrange-common.h
