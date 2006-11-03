sbin_PROGRAMS = rmcpping

rmcpping_SOURCES = rmcpping.c

rmcpping_LDADD = \
	../../libfreeipmi/src/libfreeipmi.la \
	../../common/src/libipmiping.la

rmcpping_CFLAGS = \
	-I$(srcdir)/../../libfreeipmi/include \
	-I$(srcdir)/../../common/src
 
../../libfreeipmi/src/libfreeipmie.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libipmiping.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
