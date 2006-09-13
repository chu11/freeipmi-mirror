sbin_PROGRAMS=rmcpping
rmcpping_SOURCES=rmcpping.c
 
rmcpping_LDADD = ../../libfreeipmi/src/libfreeipmi.la \
	         ../../common/src/libipmiping.la 
 
AM_CFLAGS = -Wall
AM_CPPFLAGS = -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/include -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\"
 
../../libfreeipmi/src/libfreeipmie.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libipmiping.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
