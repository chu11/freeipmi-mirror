sbin_PROGRAMS=garpd
garpd_SOURCES=garpd.c

# garpd_LDADD =

# noinst_HEADERS=

AM_CFLAGS = -Wall -I$(srcdir)/../../libfreeipmi/src -D_GNU_SOURCE \
	     -DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" 

# ../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
# 	$(MAKE) -C $(dir $@) $(notdir $@)

# force-dependency-check:
