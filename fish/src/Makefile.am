sbin_PROGRAMS=fish
fish_SOURCES=fish.c xmalloc.c fi-utils.c interpreter.c \
	extension.c scm-procedures.c guile-wrapper.c ipmi-wrapper.c \
	fi-commands.c bmc-conf-checkout.c bmc-conf-commit.c bmc-conf-utils.c \
	ipmi-wrapper-sensor.c ipmi-wrapper-sel.c bmc-conf-key-utils.c

fish_LDADD = ../../libfreeipmi/src/libfreeipmi.la $(GUILE_LDFLAGS)

noinst_HEADERS=fish.h fi-utils.h interpreter.h xmalloc.h\
	extension.h scm-procedures.h guile-wrapper.h ipmi-wrapper.h \
	fi-commands.h bmc-conf-checkout.h bmc-conf-commit.h bmc-conf-utils.h \
	ipmi-wrapper-sensor.h ipmi-wrapper-sel.h bmc-conf-key-utils.h

AM_CFLAGS = -Wall -I$(srcdir)/../../libfreeipmi/src -DFREEIPMI_BUILD -D_GNU_SOURCE \
	     -DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" $(GUILE_CFLAGS)

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
