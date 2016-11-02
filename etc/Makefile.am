EXTRA_DIST = \
	bmc-watchdog.init \
	ipmidetectd.init \
	ipmiseld.init \
	bmc-watchdog.service \
	ipmidetectd.service \
	ipmiseld.service \
	bmc-watchdog.sysconfig \
	ipmidetect.conf \
	ipmidetectd.conf \
	ipmiseld.conf \
	freeipmi.conf \
	freeipmi_interpret_sel.conf \
	freeipmi_interpret_sensor.conf \
	libipmiconsole.conf

install-data-local: $(INSTALL_DAEMON_SCRIPTS)
	$(INSTALL) -m 755 -d $(DESTDIR)$(FREEIPMI_SYSCONFDIR)
	$(INSTALL) -m 644 $(srcdir)/freeipmi.conf $(DESTDIR)$(FREEIPMI_CONFIG_FILE_DEFAULT)
	$(INSTALL) -m 644 $(srcdir)/freeipmi_interpret_sel.conf $(DESTDIR)$(INTERPRET_SEL_CONFIG_FILE_DEFAULT)
	$(INSTALL) -m 644 $(srcdir)/freeipmi_interpret_sensor.conf $(DESTDIR)$(INTERPRET_SENSOR_CONFIG_FILE_DEFAULT)
	$(INSTALL) -m 644 $(srcdir)/ipmidetect.conf $(DESTDIR)$(IPMIDETECT_CONFIG_FILE_DEFAULT)
	$(INSTALL) -m 644 $(srcdir)/ipmidetectd.conf $(DESTDIR)$(IPMIDETECTD_CONFIG_FILE_DEFAULT)
	$(INSTALL) -m 644 $(srcdir)/ipmiseld.conf $(DESTDIR)$(IPMISELD_CONFIG_FILE_DEFAULT)
	$(INSTALL) -m 644 $(srcdir)/libipmiconsole.conf $(DESTDIR)$(LIBIPMICONSOLE_CONFIG_FILE_DEFAULT)

install-init-scripts: install-sysconfig
	$(INSTALL) -m 755 -d $(DESTDIR)$(sysconfdir)/init.d
	$(INSTALL) -m 755 $(srcdir)/bmc-watchdog.init $(DESTDIR)$(sysconfdir)/init.d/bmc-watchdog
	$(INSTALL) -m 755 $(srcdir)/ipmidetectd.init $(DESTDIR)$(sysconfdir)/init.d/ipmidetectd
	$(INSTALL) -m 755 $(srcdir)/ipmiseld.init $(DESTDIR)$(sysconfdir)/init.d/ipmiseld

install-systemd-scripts: install-sysconfig
	$(INSTALL) -m 755 -d $(DESTDIR)$(systemdsystemunitdir)
	$(INSTALL) -m 755 $(srcdir)/bmc-watchdog.service $(DESTDIR)$(systemdsystemunitdir)/bmc-watchdog.service
	$(INSTALL) -m 755 $(srcdir)/ipmidetectd.service $(DESTDIR)$(systemdsystemunitdir)/ipmidetectd.service
	$(INSTALL) -m 755 $(srcdir)/ipmiseld.service $(DESTDIR)$(systemdsystemunitdir)/ipmiseld.service

install-sysconfig:
	$(INSTALL) -m 755 -d $(DESTDIR)$(sysconfdir)/sysconfig
	$(INSTALL) -m 444 $(srcdir)/bmc-watchdog.sysconfig $(DESTDIR)$(sysconfdir)/sysconfig/bmc-watchdog

uninstall-local: $(UNINSTALL_DAEMON_SCRIPTS)
	$(RM) -f $(DESTDIR)$(sysconfdir)/sysconfig/bmc-watchdog
	$(RM) -f $(DESTDIR)$(FREEIPMI_CONFIG_FILE_DEFAULT)
	$(RM) -f $(DESTDIR)$(INTERPRET_SEL_CONFIG_FILE_DEFAULT)
	$(RM) -f $(DESTDIR)$(INTERPRET_SENSOR_CONFIG_FILE_DEFAULT)
	$(RM) -f $(DESTDIR)$(IPMIDETECT_CONFIG_FILE_DEFAULT)
	$(RM) -f $(DESTDIR)$(IPMIDETECTD_CONFIG_FILE_DEFAULT)
	$(RM) -f $(DESTDIR)$(IPMISELD_CONFIG_FILE_DEFAULT)
	$(RM) -f $(DESTDIR)$(LIBIPMICONSOLE_CONFIG_FILE_DEFAULT)

uninstall-init-scripts:
	$(RM) -f $(DESTDIR)$(sysconfdir)/init.d/bmc-watchdog
	$(RM) -f $(DESTDIR)$(sysconfdir)/init.d/ipmidetectd
	$(RM) -f $(DESTDIR)$(sysconfdir)/init.d/ipmiseld

uninstall-systemd-scripts:
	$(RM) -f $(DESTDIR)$(systemdsystemunitdir)/bmc-watchdog.service
	$(RM) -f $(DESTDIR)$(systemdsystemunitdir)/ipmidetectd.service
	$(RM) -f $(DESTDIR)$(systemdsystemunitdir)/ipmiseld.service
