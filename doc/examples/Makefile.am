# libfreeipmi Examples
LIBFREEIPMI_EXAMPLES = udm-test.c ipmi-lan-test.c

docdir   = $(datadir)/doc/$(PACKAGE)
Examplesdir     = $(docdir)/examples
Examples_DATA   = $(LIBFREEIPMI_EXAMPLES)

install-data-local:
	$(INSTALL_DATA) $(srcdir)/Makefile.example $(DESTDIR)$(Examplesdir)/Makefile

uninstall-local:
	rm -f $(DESTDIR)$(Examplesdir)/Makefile

EXTRA_DIST = $(LIBFREEIPMI_EXAMPLES) Makefile.example


