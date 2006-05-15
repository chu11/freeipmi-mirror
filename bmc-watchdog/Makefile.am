SUBDIRS = src

dist_man_MANS = bmc-watchdog.8

CLEANFILES = *~
DISTCLEANFILES = .deps/*.P

#  if you write a self-test script named `chk', uncomment the
#  following and add `chk' to the EXTRA_DIST list
#TESTS=chk

DEFINITSCRIPT = freeipmi-bmc-watchdog
DEFINITSCRIPTdir = $(sysconfdir)/init.d
DEFINITSCRIPT_DATA = $(DEFINITSCRIPT)

BMCWATCHDOGLOGdir = /var/log/$(PACKAGE)

BMCWATCHDOGLOGROTATE = bmc-watchdog.logrotate
BMCWATCHDOGLOGROTATEDIR = $(sysconfdir)/logrotate.d/

BMCWATCHDOGLOGROTATEdir = $(BMCWATCHDOGLOGROTATEDIR)
BMCWATCHDOGLOGROTATE_DATA = $(BMCWATCHDOGLOGROTATE)

EXTRA_DIST = $(DEFINITSCRIPT) $(BMCWATCHDOGLOGROTATE)

install-data-hook:
	chmod 755 $(DESTDIR)/$(DEFINITSCRIPTdir)/$(DEFINITSCRIPT)
	$(top_srcdir)/install-sh -m 755 -d $(DESTDIR)$(BMCWATCHDOGLOGdir)
	$(top_srcdir)/install-sh -m 755 -d $(DESTDIR)$(BMCWATCHDOGLOGdir)
