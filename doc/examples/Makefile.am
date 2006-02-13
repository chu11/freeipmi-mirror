# libfreeipmi Examples
LIBFREEIPMI_EXAMPLES = udm-test.c

noinst_PROGRAMS  = udm-test
udm_test_SOURCES = $(LIBFREEIPMI_EXAMPLES)
udm_test_LDADD   = ../../libfreeipmi/src/libfreeipmi.la
AM_CFLAGS        = -Wall -I$(srcdir)/../../libfreeipmi/src -DFREEIPMI_BUILD \
	-D_GNU_SOURCE -DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" \
	-DWITH_LSD_FATAL_ERROR_FUNC=1 -DWITH_LSD_NOMEM_ERROR_FUNC=1

docdir          = $(datadir)/doc/$(PACKAGE)
Examplesdir     = $(docdir)/examples
Examples_DATA   = $(LIBFREEIPMI_EXAMPLES)

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

install-data-local:
	$(INSTALL_DATA) $(srcdir)/Makefile.example $(DESTDIR)$(Examplesdir)/Makefile

uninstall-local:
	rm -f $(DESTDIR)$(Examplesdir)/Makefile

EXTRA_DIST = $(LIBFREEIPMI_EXAMPLES) Makefile.example

force-dependency-check:
