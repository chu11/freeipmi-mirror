SUBDIRS = src

CLEANFILES = *~
DISTCLEANFILES = .deps/*.P

#  if you write a self-test script named `chk', uncomment the
#  following and add `chk' to the EXTRA_DIST list
#TESTS=chk

DEFINITSCRIPT = bmc-watchdog
EXTRA_DIST = $(DEFINITSCRIPT)

DEFINITSCRIPTdir = $(sysconfdir)/rc.d/init.d
DEFINITSCRIPT_DATA = $(DEFINITSCRIPT)

install-data-hook:
	chmod 755 $(DEFINITSCRIPTdir)/$(DEFINITSCRIPT)
