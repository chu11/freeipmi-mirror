# libfreeipi Examples
LIBFREEIPMI_EXAMPLES = hello-sensors.c

Examplesdir     = $(datadir)/doc/$(PACKAGE)/examples
Examples_DATA   = $(LIBFREEIPMI_EXAMPLES)

install-data-local:
	$(INSTALL_DATA) $(srcdir)/Makefile.example $(DESTDIR)$(Examplesdir)/Makefile

uninstall-local:
	-rm -f $(Examplesdir)/Makefile

EXTRA_DIST = $(LIBFREEIPMI_EXAMPLES) Makefile.example


