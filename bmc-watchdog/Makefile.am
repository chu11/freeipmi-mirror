SUBDIRS = src

CLEANFILES = *~
DISTCLEANFILES = .deps/*.P

#  if you write a self-test script named `chk', uncomment the
#  following and add `chk' to the EXTRA_DIST list
#TESTS=chk

DEFINITSCRIPT = bmc-watchdog
DEFINITSCRIPTdir = $(sysconfdir)/init.d
DEFINITSCRIPT_DATA = $(DEFINITSCRIPT)

BMCWATCHDOGLOGdir = /var/log/$(PACKAGE)

EXTRA_DIST = $(DEFINITSCRIPT)

install-data-hook:
	chmod 755 $(DESTDIR)/$(DEFINITSCRIPTdir)/$(DEFINITSCRIPT)
	$(top_srcdir)/mkinstalldirs -m 755 $(DESTDIR)$(BMCWATCHDOGLOGdir)
