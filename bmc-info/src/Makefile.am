sbin_PROGRAMS=bmc-info
bmc_info_SOURCES=bmc-info-argp.c bmc-info.c ../../common/src/argp-common.c
 
bmc_info_LDADD = ../../libfreeipmi/src/libfreeipmi.la
 
noinst_HEADERS=bmc-info-argp.h ../../common/src/argp-common.h
 
AM_CFLAGS = -Wall -I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/src -DFREEIPMI_BUILD -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" \
	-DWITH_LSD_FATAL_ERROR_FUNC=1 -DWITH_LSD_NOMEM_ERROR_FUNC=1
 
../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
force-dependency-check:
