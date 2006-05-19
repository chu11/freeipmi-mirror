sbin_PROGRAMS=fish
fish_SOURCES=fish.c xmalloc.c fi-utils.c interpreter.c \
	extension.c scm-procedures.c guile-wrapper.c ipmi-wrapper.c \
	fi-commands.c bmc-conf2.c fish-argp.c ../../common/src/argp-common.c

fish_LDADD = ../../common/src/libbitops.la \
	     ../../common/src/libipmicommon.la \
	     ../../common/src/libsel.la \
	     ../../common/src/libsensor.la \
	     ../../libfreeipmi/src/libfreeipmi.la \
	     $(GUILE_LDFLAGS) $(READLINE_LIBS)

noinst_HEADERS=fish.h fi-utils.h interpreter.h xmalloc.h \
	extension.h scm-procedures.h guile-wrapper.h ipmi-wrapper.h \
	fi-commands.h bmc-conf2.h fish-argp.h common.h  \
	../../common/src/argp-common.h

AM_CFLAGS = -Wall -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/include -DFREEIPMI_BUILD \
	-D_GNU_SOURCE $(GUILE_CFLAGS) \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" \
	-DIPMI_IPCKEY=\"$(localstatedir)/lib/$(PACKAGE_NAME)/ipckey\"

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libipmicommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libsel.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libsensor.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
