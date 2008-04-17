sbin_PROGRAMS = ipmiping

ipmiping_SOURCES = ipmiping.c
 
ipmiping_LDADD = \
	../../libfreeipmi/src/libfreeipmi.la \
	../../common/src/libipmiping.la \
	../../common/src/libdebugcommon.la
 
ipmiping_CPPFLAGS = \
	-I$(srcdir)/../../libfreeipmi/include \
	-I$(srcdir)/../../common/src
 
../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libipmiping.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libdebugcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
force-dependency-check:

