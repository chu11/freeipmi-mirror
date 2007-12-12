sbin_PROGRAMS = bmc-watchdog

bmc_watchdog_SOURCES = bmc-watchdog.c

bmc_watchdog_LDADD = \
	../../libfreeipmi/src/libfreeipmi.la \
	../../common/src/libtoolcommon.la

bmc_watchdog_CPPFLAGS = \
	-I$(srcdir)/../../libfreeipmi/include \
	-I$(srcdir)/../../common/src \
	-DBMC_WATCHDOG_LOGFILE=\"$(localstatedir)/log/freeipmi/bmc-watchdog.log\"

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
