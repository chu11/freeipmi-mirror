SUBDIRS = src

dist_man_MANS = bmc-watchdog.8

CLEANFILES = *~
DISTCLEANFILES = .deps/*.P

#  if you write a self-test script named `chk', uncomment the
#  following and add `chk' to the EXTRA_DIST list
#TESTS=chk

BMCWATCHDOGLOGdir = /var/log/$(PACKAGE)

EXTRA_DIST = freeipmi-bmc-watchdog.init freeipmi-bmc-watchdog.logrotate

install-data-hook:
	$(top_srcdir)/install-sh -m 755 -d $(DESTDIR)$(BMCWATCHDOGLOGdir)

