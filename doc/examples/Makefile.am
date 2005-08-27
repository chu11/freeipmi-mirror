# libfreeipmi Examples
LIBFREEIPMI_EXAMPLES = ipmi-lan-test.c ipmi-lan-test-udm.c

docdir   = $(datadir)/doc/$(PACKAGE)
Examplesdir     = $(docdir)/examples
Examples_DATA   = $(LIBFREEIPMI_EXAMPLES)

install-data-local:
	$(INSTALL_DATA) $(srcdir)/Makefile.example $(DESTDIR)$(Examplesdir)/Makefile

uninstall-local:
	-rm -f $(Examplesdir)/Makefile

EXTRA_DIST = $(LIBFREEIPMI_EXAMPLES) Makefile.example


