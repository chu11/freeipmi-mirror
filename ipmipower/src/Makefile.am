sbin_PROGRAMS=ipmipower
ipmipower_SOURCES=argv.c cbuf.c conffile.c error.c fd.c hostlist.c hprintf.c \
	ipmipower_auth.c ipmipower_check.c ipmipower_config.c \
	ipmipower_connection.c ipmipower.c ipmipower_output.c \
	ipmipower_packet.c ipmipower_ping.c ipmipower_powercmd.c \
	ipmipower_prompt.c ipmipower_util.c ipmipower_wrappers.c \
	list.c wrappers.c
 
ipmipower_LDADD = ../../libfreeipmi/src/libfreeipmi.la
 
noinst_HEADERS=argv.h cbuf.h conffile.h error.h fd.h hostlist.h hprintf.h \
	ipmipower_auth.h ipmipower_check.h ipmipower_config.h \
	ipmipower_connection.h ipmipower.h ipmipower_output.h \
	ipmipower_packet.h ipmipower_ping.h ipmipower_powercmd.h \
	ipmipower_prompt.h ipmipower_util.h ipmipower_wrappers.h \
	list.h wrappers.h
 
AM_CFLAGS = -Wall -I$(srcdir)/../../libfreeipmi/src -DFREEIPMI_BUILD -D_GNU_SOURCE \
	-DPATH_CFG=\"$(sysconfdir)\" -DDATADIR=\"$(datadir)\" \
	-DWITH_LSD_FATAL_ERROR_FUNC=1 -DWITH_LSD_NOMEM_ERROR_FUNC=1
 
../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
force-dependency-check:
