EXTENSIONS =  init.scm shell.scm utils.scm discovery.scm sensors.scm \
	sensors-utils.scm bmc-config.scm sel.scm bmc-info.scm
Extensionsdir = $(datadir)/fish/extensions
Extensions_DATA = $(EXTENSIONS)

EXTENSIONSCONF = sensors-conf.scm
Extensionsconfdir = $(sysconfdir)/fish
Extensionsconf_DATA = $(EXTENSIONSCONF)

EXTRA_DIST = $(EXTENSIONS) $(EXTENSIONSCONF) 

CLEANFILES = *~
DISTCLEANFILES = .deps/*.P
