sbin_PROGRAMS = rmcpping

rmcpping_SOURCES = rmcpping.c

rmcpping_LDADD = \
	../../libfreeipmi/src/libfreeipmi.la \
	../../common/src/libipmiping.la \
	../../common/src/libdebugcommon.la

rmcpping_CPPFLAGS = \
	-I$(srcdir)/../../libfreeipmi/include \
	-I$(srcdir)/../../common/src
 
../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libipmiping.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libdebugcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

force-dependency-check:
