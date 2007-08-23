##*****************************************************************************
## $Id: Makefile.am,v 1.9 2007-08-23 00:45:53 chu11 Exp $
##*****************************************************************************
## Process this file with automake to produce Makefile.in.
##*****************************************************************************

if WITH_GNU_LD
VERSION_SCRIPT = $(srcdir)/ipmiconsole.map
OTHER_FLAGS = -Wl,--version-script=$(VERSION_SCRIPT)
endif

noinst_HEADERS = cbuf.h \
		 ipmiconsole_checks.h \
		 ipmiconsole_defs.h \
		 ipmiconsole_debug.h \
		 ipmiconsole_engine.h \
		 ipmiconsole_fiid_wrappers.h \
		 ipmiconsole_garbage_collector.h \
		 ipmiconsole_packet.h \
		 ipmiconsole_processing.h \
		 ipmiconsole_util.h
		 
include_HEADERS = ipmiconsole.h

lib_LTLIBRARIES = libipmiconsole.la
		 
libipmiconsole_la_SOURCES = cbuf.c \
			    ipmiconsole.c \
			    ipmiconsole_checks.c \
			    ipmiconsole_debug.c \
			    ipmiconsole_engine.c \
			    ipmiconsole_fiid_wrappers.c \
			    ipmiconsole_garbage_collector.c \
			    ipmiconsole_packet.c \
			    ipmiconsole_processing.c \
			    ipmiconsole_util.c

libipmiconsole_la_CFLAGS = $(PTHREAD_CFLAGS)

libipmiconsole_la_CPPFLAGS = -I$(srcdir)/../../../common/src \
			     -I$(srcdir)/../../../libfreeipmi/include \
			     -DWITH_PTHREADS=1 \
			     -D_REENTRANT

libipmiconsole_la_LDFLAGS = $(PTHREAD_LIBS) \
			    -version-info @LIBIPMICONSOLE_VERSION_INFO@ \
			    $(OTHER_FLAGS)

libipmiconsole_la_LIBADD = ../../../common/src/libllnlcommon.la \
			   ../../../libfreeipmi/src/libfreeipmi.la \
			   ../../../common/src/libfreeipmiportability.la

install-data-hook: $(INSTALL_LOGDIR)

install-logdir:
	$(INSTALL) -m 755 -d $(DESTDIR)var/log/ipmiconsole

EXTRA_DIST = ipmiconsole.map

../../../common/src/libllnlcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../../common/src/libfreeipmiportability.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
