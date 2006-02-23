sbin_PROGRAMS=rmcpping
rmcpping_SOURCES=rmcpping.c
 
rmcpping_LDADD = ../../libfreeipmi/src/libfreeipmi.la \
	         ../../common/src/libipmiping.la 
 
AM_CFLAGS = -Wall -I$(srcdir)/../../common/src -I$(srcdir)/../../libfreeipmi/src -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\"
 
../../libfreeipmi/src/libfreeipmie.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libipmiping.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
