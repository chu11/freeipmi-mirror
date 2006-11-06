##*****************************************************************************
## $Id: Makefile.am,v 1.1 2006-11-06 00:13:12 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

if WITH_GNU_LD
VERSION_SCRIPT = $(srcdir)/ipmiconsole.map
OTHER_FLAGS = -Wl,--version-script=$(VERSION_SCRIPT)
endif

noinst_HEADERS = ipmiconsole_checks.h \
		 ipmiconsole_defs.h \
		 ipmiconsole_debug.h \
		 ipmiconsole_engine.h \
		 ipmiconsole_fiid_wrappers.h \
		 ipmiconsole_packet.h \
		 ipmiconsole_processing.h
		 
include_HEADERS = ipmiconsole.h

lib_LTLIBRARIES = libipmiconsole.la
		 
libipmiconsole_la_SOURCES = ipmiconsole.c \
			    ipmiconsole_checks.c \
			    ipmiconsole_debug.c \
			    ipmiconsole_engine.c \
			    ipmiconsole_fiid_wrappers.c \
			    ipmiconsole_packet.c \
			    ipmiconsole_processing.c

libipmiconsole_la_CFLAGS = -I$(srcdir)/../../../config \
			   -I$(srcdir)/../../../common/src \
			   -I$(srcdir)/../../../libfreeipmi/include \
			   $(PTHREAD_CFLAGS) \
			   -DWITH_SECURE_MALLOC=1 \
			   -DWITH_PTHREADS=1

libipmiconsole_la_LDFLAGS = $(PTHREAD_LIBS) \
			    -version-info @LIBIPMICONSOLE_VERSION_INFO@ \
			    $(OTHER_FLAGS)

libipmiconsole_la_LIBADD = ../../../common/src/libllnlcommon.la \
			   ../../../libfreeipmi/src/libfreeipmi.la

install-data-hook:
	$(top_srcdir)/install-sh -m 755 -d $(DESTDIR)/var/log/ipmiconsole

EXTRA_DIST = ipmiconsole.map

../../../common/src/libllnlcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
