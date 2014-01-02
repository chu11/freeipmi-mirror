sbin_PROGRAMS = ipmi-config

sbin_SCRIPTS = \
	bmc-config \
	ipmi-chassis-config \
	ipmi-pef-config \
	ipmi-sensors-config

ipmi_config_CPPFLAGS = \
	-I$(top_srcdir)/common/toolcommon \
	-I$(top_srcdir)/common/miscutil \
	-I$(top_srcdir)/common/parsecommon \
	-I$(top_srcdir)/common/portability \
	-I$(top_builddir)/libfreeipmi/include \
	-I$(top_srcdir)/libfreeipmi/include \
	-D_GNU_SOURCE \
	-D_REENTRANT

ipmi_config_LDADD = \
	$(top_builddir)/common/toolcommon/libtoolcommon.la \
	$(top_builddir)/common/miscutil/libmiscutil.la \
	$(top_builddir)/common/parsecommon/libparsecommon.la \
	$(top_builddir)/common/portability/libportability.la \
	$(top_builddir)/libipmidetect/libipmidetect.la \
	$(top_builddir)/libfreeipmi/libfreeipmi.la

ipmi_config_SOURCES = \
	ipmi-config.c \
	ipmi-config.h \
	ipmi-config-argp.c \
	ipmi-config-argp.h \
	ipmi-config-category-chassis-boot-flags.c \
	ipmi-config-category-chassis-boot-flags.h \
	ipmi-config-category-chassis-front-panel-buttons.c \
	ipmi-config-category-chassis-front-panel-buttons.h \
	ipmi-config-category-chassis-power-conf.c \
	ipmi-config-category-chassis-power-conf.h \
	ipmi-config-category-chassis-sections.c \
	ipmi-config-category-chassis-sections.h \
	ipmi-config-category-common-pef-conf-section.c \
	ipmi-config-category-common-pef-conf-section.h \
	ipmi-config-category-core-channel-common.c \
	ipmi-config-category-core-channel-common.h \
	ipmi-config-category-core-lan-channel-section.c \
	ipmi-config-category-core-lan-channel-section.h \
	ipmi-config-category-core-lan-conf-auth-section.c \
	ipmi-config-category-core-lan-conf-auth-section.h \
	ipmi-config-category-core-lan-conf-misc-section.c \
	ipmi-config-category-core-lan-conf-misc-section.h \
	ipmi-config-category-core-lan-conf-section.c \
	ipmi-config-category-core-lan-conf-section.h \
	ipmi-config-category-core-lan-conf-security-keys-section.c \
	ipmi-config-category-core-lan-conf-security-keys-section.h \
	ipmi-config-category-core-lan-conf-user-security-section.c \
	ipmi-config-category-core-lan-conf-user-security-section.h \
	ipmi-config-category-core-misc-section.c \
	ipmi-config-category-core-misc-section.h \
	ipmi-config-category-core-pef-conf-section.c \
	ipmi-config-category-core-pef-conf-section.h \
	ipmi-config-category-core-rmcpplus-conf-privilege-section.c \
	ipmi-config-category-core-rmcpplus-conf-privilege-section.h \
	ipmi-config-category-core-sections.c \
	ipmi-config-category-core-sections.h \
	ipmi-config-category-core-serial-channel-section.c \
	ipmi-config-category-core-serial-channel-section.h \
	ipmi-config-category-core-serial-conf-section.c \
	ipmi-config-category-core-serial-conf-section.h \
	ipmi-config-category-core-sol-conf-section.c \
	ipmi-config-category-core-sol-conf-section.h \
	ipmi-config-category-core-user-sections.c \
	ipmi-config-category-core-user-sections.h \
	ipmi-config-category-dcmi-dcmi-conf-section.c \
	ipmi-config-category-dcmi-dcmi-conf-section.h \
	ipmi-config-category-dcmi-sections.c \
	ipmi-config-category-dcmi-sections.h \
	ipmi-config-category-pef-alert-policy-table.c \
	ipmi-config-category-pef-alert-policy-table.h \
	ipmi-config-category-pef-alert-string.c \
	ipmi-config-category-pef-alert-string.h \
	ipmi-config-category-pef-community-string.c \
	ipmi-config-category-pef-community-string.h \
	ipmi-config-category-pef-event-filter-table.c \
	ipmi-config-category-pef-event-filter-table.h \
	ipmi-config-category-pef-lan-alert-destination.c \
	ipmi-config-category-pef-lan-alert-destination.h \
	ipmi-config-category-pef-pef-conf-section.c \
	ipmi-config-category-pef-pef-conf-section.h \
	ipmi-config-category-pef-sections.c \
	ipmi-config-category-pef-sections.h \
	ipmi-config-category-sensors-discrete-section.c \
	ipmi-config-category-sensors-discrete-section.h \
	ipmi-config-category-sensors-sections.c \
	ipmi-config-category-sensors-sections.h \
	ipmi-config-category-sensors-sensor-event-enable-common.c \
	ipmi-config-category-sensors-sensor-event-enable-common.h \
	ipmi-config-category-sensors-threshold-section.c \
	ipmi-config-category-sensors-threshold-section.h \
	ipmi-config-category-sensors-utils.c \
	ipmi-config-category-sensors-utils.h \
	ipmi-config-checkout.c \
	ipmi-config-checkout.h \
	ipmi-config-comment.c \
	ipmi-config-comment.h \
	ipmi-config-commit.c \
	ipmi-config-commit.h \
	ipmi-config-diff.c \
	ipmi-config-diff.h \
	ipmi-config-legacy-pef-info.c \
	ipmi-config-legacy-pef-info.h \
	ipmi-config-map.c \
	ipmi-config-map.h \
	ipmi-config-parse.c \
	ipmi-config-parse.h \
	ipmi-config-section.c \
	ipmi-config-section.h \
	ipmi-config-utils.c \
	ipmi-config-utils.h \
	ipmi-config-validate.c \
	ipmi-config-validate.h

bmc-config: bmc-config.in
	sed -e 's:@CONFIGSBINDIR@:$(sbindir):' $< > $@
	chmod +x bmc-config

ipmi-chassis-config: ipmi-chassis-config.in
	sed -e 's:@CONFIGSBINDIR@:$(sbindir):' $< > $@
	chmod +x ipmi-chassis-config

ipmi-pef-config: ipmi-pef-config.in
	sed -e 's:@CONFIGSBINDIR@:$(sbindir):' $< > $@
	chmod +x ipmi-pef-config

ipmi-sensors-config: ipmi-sensors-config.in
	sed -e 's:@CONFIGSBINDIR@:$(sbindir):' $< > $@
	chmod +x ipmi-sensors-config

$(top_builddir)/common/toolcommon/libtoolcommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/miscutil/libmiscutil.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/parsecommon/libparsecommon.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/common/portability/libportability.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libipmidetect/libipmidetect.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

$(top_builddir)/libfreeipmi/libfreeipmi.la : force-dependency-check
	@cd `dirname $@` && $(MAKE) `basename $@`

force-dependency-check:

CLEANFILES =  \
	bmc-config \
	ipmi-chassis-config \
	ipmi-pef-config \
	ipmi-sensors-config

EXTRA_DIST =  \
	bmc-config.in \
	ipmi-chassis-config.in \
	ipmi-pef-config.in \
	ipmi-sensors-config.in

install-exec-hook:
	$(install_sh) -d -m 755 $(DESTDIR)$(sbindir)
	$(LN_S) -f $(sbindir)/ipmi-pef-config $(DESTDIR)$(sbindir)/pef-config

uninstall-hook:
	$(RM) $(DESTDIR)$(sbindir)/pef-config

