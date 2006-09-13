sbin_PROGRAMS=bmc-watchdog
bmc_watchdog_SOURCES=bmc-watchdog.c
bmc_watchdog_LDADD = ../../libfreeipmi/src/libfreeipmi.la
AM_CFLAGS = -Wall
AM_CPPFLAGS = -I$(srcdir)/../../libfreeipmi/include -D_GNU_SOURCE \
	     -DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" \
	     -DBMC_WATCHDOG_LOGFILE=\"$(localstatedir)/log/freeipmi/bmc-watchdog.log\"

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
