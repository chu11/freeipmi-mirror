EXTENSIONS =  init.scm shell.scm utils.scm discovery.scm \
	sel.scm bmc-info.scm \
	bc-common.scm bc-user-section.scm bc-lan-serial-channel-section.scm \
	bc-lan-conf-section.scm bc-lan-conf-auth-section.scm \
	bc-lan-conf-misc-section.scm bc-serial-conf-section.scm \
	bc-misc-section.scm bc-section.scm bmc-config.scm \
	sensors-alias.scm sensors-simple-display.scm \
	sensors-common.scm sensors-verbose-display.scm \
	sensors-very-verbose-display.scm sdr.scm sensors.scm

Extensionsdir = $(datadir)/fish/extensions
Extensions_DATA = $(EXTENSIONS)

EXTENSIONSCONF = sensors-conf.scm
Extensionsconfdir = $(sysconfdir)/fish
Extensionsconf_DATA = $(EXTENSIONSCONF)

EXTRA_DIST = $(EXTENSIONS) $(EXTENSIONSCONF) 

CLEANFILES = *~
DISTCLEANFILES = .deps/*.P
