sbin_PROGRAMS=ipmiping
ipmiping_SOURCES=ipmiping.c
 
ipmiping_LDADD = ../../libfreeipmi/src/libfreeipmi.la
 
AM_CFLAGS = -Wall -I$(srcdir)/../../libfreeipmi/src -DFREEIPMI_BUILD -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\"
 
../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
force-dependency-check:
