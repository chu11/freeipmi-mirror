EXTENSIONS =  init.scm shell.scm utils.scm discovery.scm \
	sensors-alias.scm sensors-simple-display.scm \
	sensors-common.scm sensors-verbose-display.scm \
	sensors-very-verbose-display.scm sdr.scm sensors.scm pef.scm 

Extensionsdir = $(datadir)/fish/extensions
Extensions_DATA = $(EXTENSIONS)

EXTENSIONSCONF = ipmi-sensors-conf.scm
Extensionsconfdir = $(sysconfdir)/fish
Extensionsconf_DATA = $(EXTENSIONSCONF)

EXTRA_DIST = $(EXTENSIONS) $(EXTENSIONSCONF) 

CLEANFILES = 
DISTCLEANFILES = .deps/*.P
