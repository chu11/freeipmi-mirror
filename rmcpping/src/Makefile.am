sbin_PROGRAMS=rmcpping
rmcpping_SOURCES=rmcpping.c
 
rmcpping_LDADD = ../../common/src/libipmiping.la 
 
AM_CFLAGS = -Wall -I$(srcdir)/../../common/src -I$(srcdir)/../../libfreeipmi/src -DFREEIPMI_BUILD -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\"
 
../../common/src/libipmiping.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
