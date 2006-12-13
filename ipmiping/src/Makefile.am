sbin_PROGRAMS = ipmiping

ipmiping_SOURCES = ipmiping.c
 
ipmiping_LDADD = \
	../../libfreeipmi/src/libfreeipmi.la \
	 ../../common/src/libipmiping.la
 
ipmiping_CPPFLAGS = \
	-I$(srcdir)/../../libfreeipmi/include \
	-I$(srcdir)/../../common/src
 
../../libfreeipmi/src/libfreeipmie.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libipmiping.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
force-dependency-check:

