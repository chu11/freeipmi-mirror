sbin_PROGRAMS=fish
fish_SOURCES=fish.c xmalloc.c fi-utils.c interpreter.c \
	extension.c scm-procedures.c guile-wrapper.c ipmi-wrapper.c \
	fi-commands.c bmc-conf2.c fish-argp.c ../../common/src/argp-common.c

fish_LDADD = ../../libfreeipmi/src/libfreeipmi.la $(GUILE_LDFLAGS) $(READLINE_LIBS)

noinst_HEADERS=fish.h fi-utils.h interpreter.h xmalloc.h \
	extension.h scm-procedures.h guile-wrapper.h ipmi-wrapper.h \
	fi-commands.h bmc-conf2.h fish-argp.h common.h  \
	../../common/src/argp-common.h

AM_CFLAGS = -Wall  -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/src -DFREEIPMI_BUILD -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" $(GUILE_CFLAGS)

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
