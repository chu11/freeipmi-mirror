SUBDIRS = src

dist_man_MANS = bmc-watchdog.8

CLEANFILES = *~
DISTCLEANFILES = .deps/*.P

#  if you write a self-test script named `chk', uncomment the
#  following and add `chk' to the EXTRA_DIST list
#TESTS=chk

BMCWATCHDOGLOGdir = $(localstatedir)/log/$(PACKAGE)

EXTRA_DIST = freeipmi-bmc-watchdog.init freeipmi-bmc-watchdog.logrotate freeipmi-bmc-watchdog.sysconfig

install-data-local: $(INSTALL_INIT_SCRIPTS) $(INSTALL_LOGROTATE_CONFIG) \
	$(INSTALL_LOGDIR)

install-init-scripts:
	$(INSTALL) -m 755 -d $(DESTDIR)$(sysconfdir)/init.d
	$(INSTALL) -m 755 $(srcdir)/freeipmi-bmc-watchdog.init $(DESTDIR)$(sysconfdir)/init.d/freeipmi-bmc-watchdog
	$(top_srcdir)/install-sh -m 755 -d $(DESTDIR)/$(sysconfdir)/sysconfig
	$(top_srcdir)/install-sh -m 444 $(srcdir)/freeipmi-bmc-watchdog.sysconfig $(DESTDIR)$(sysconfdir)/sysconfig/freeipmi-bmc-watchdog

install-logrotate-config:
	$(INSTALL) -m 755 -d $(DESTDIR)$(sysconfdir)/logrotate.d
	$(INSTALL) -m 444 $(srcdir)/freeipmi-bmc-watchdog.logrotate $(DESTDIR)$(sysconfdir)/logrotate.d/freeipmi-bmc-watchdog

install-logdir:
	$(INSTALL) -m 755 -d $(DESTDIR)$(BMCWATCHDOGLOGdir)

uninstall-local:
	$(RM) -f $(DESTDIR)$(sysconfdir)/init.d/freeipmi-bmc-watchdog
	$(RM) -f $(DESTDIR)$(sysconfdir)/logrotate.d/freeipmi-bmc-watchdog
	$(RM) -f $(DESTDIR)$(sysconfdir)/sysconfig/freeipmi-bmc-watchdog
