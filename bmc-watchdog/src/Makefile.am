sbin_PROGRAMS = bmc-watchdog

bmc_watchdog_SOURCES = bmc-watchdog.c bmc-watchdog-argp.c

noinst_HEADERS = bmc-watchdog.h bmc-watchdog-argp.h

bmc_watchdog_LDADD = \
	../../libfreeipmi/src/libfreeipmi.la \
	../../common/src/libtoolcommon.la \
	../../common/src/libmiscutils.la \
	../../common/src/libdebugcommon.la

bmc_watchdog_CPPFLAGS = \
	-I$(srcdir)/../../libfreeipmi/include \
	-I$(srcdir)/../../common/src \
	-DBMC_WATCHDOG_LOGFILE=\"$(localstatedir)/log/freeipmi/bmc-watchdog.log\"

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libmiscutils.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libdebugcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
