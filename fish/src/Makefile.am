sbin_PROGRAMS=fish
fish_SOURCES=fish.c xmalloc.c fi-utils.c interpreter.c \
	extension.c scm-procedures.c guile-wrapper.c ipmi-wrapper.c \
	fi-commands.c fish-argp.c ../../common/src/argp-common.c

fish_LDADD = ../../common/src/libbitops.la \
	     ../../common/src/libipmicommon.la \
	     ../../common/src/libsensor.la \
	     ../../libfreeipmi/src/libfreeipmi.la \
	     $(GUILE_LDFLAGS) $(READLINE_LIBS)

noinst_HEADERS=fish.h fi-utils.h interpreter.h xmalloc.h \
	extension.h scm-procedures.h guile-wrapper.h ipmi-wrapper.h \
	fi-commands.h fish-argp.h common.h  \
	../../common/src/argp-common.h

AM_CFLAGS = -Wall -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/include -DFREEIPMI_BUILD \
	-D_GNU_SOURCE $(GUILE_CFLAGS) \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\"

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libipmicommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libsensor.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
