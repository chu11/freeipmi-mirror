SUBDIRS = src

CLEANFILES = *~
DISTCLEANFILES = .deps/*.P

#  if you write a self-test script named `chk', uncomment the
#  following and add `chk' to the EXTRA_DIST list
#TESTS=chk

DEFINITSCRIPT = bmc-watchdog

DEFINITSCRIPTdir = $(sysconfdir)/rc.d/init.d
DEFINITSCRIPT_DATA = $(DEFINITSCRIPT)

BMCWATCHDOGLOG = bmc-watchdog.log

if RPM_INSTALL
BMCWATCHDOGLOGDIR = $(RPM_BUILD_ROOT)/var/log/$(PACKAGE)
else
BMCWATCHDOGLOGDIR = /var/log/$(PACKAGE)
endif

BMCWATCHDOGLOGdir = $(BMCWATCHDOGLOGDIR)
BMCWATCHDOGLOG_DATA = $(BMCWATCHDOGLOG)

EXTRA_DIST = $(DEFINITSCRIPT) $(BMCWATCHDOGLOG)

install-data-hook:
	chmod 755 $(DEFINITSCRIPTdir)/$(DEFINITSCRIPT)
	chmod 600 $(BMCWATCHDOGLOGdir)/$(BMCWATCHDOGLOG)
