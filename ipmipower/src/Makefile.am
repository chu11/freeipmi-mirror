sbin_PROGRAMS=ipmipower
ipmipower_SOURCES= \
	argv.c \
	error.c \
	hostlist.c \
	ipmipower_authentication.c \
	ipmipower_check.c \
	ipmipower_cipher_suite.c \
	ipmipower_config.c \
	ipmipower_connection.c \
	ipmipower.c \
	ipmipower_ipmi_version.c \
	ipmipower_output.c \
	ipmipower_packet.c \
	ipmipower_ping.c \
	ipmipower_powercmd.c \
	ipmipower_privilege.c \
	ipmipower_prompt.c \
	ipmipower_util.c \
	ipmipower_wrappers.c \
	wrappers.c
 
ipmipower_LDADD = \
	../../common/src/libcbuf.la \
	../../common/src/libllnlcommon.la \
	../../libfreeipmi/src/libfreeipmi.la
 
ipmipower_CPPFLAGS = \
	-I$(srcdir)/../../common/src \
	-I$(srcdir)/../../libfreeipmi/include \
	-DWITH_LSD_FATAL_ERROR_FUNC=1 \
	-DWITH_LSD_NOMEM_ERROR_FUNC=1	

noinst_HEADERS= \
	argv.h \
	error.h \
	hostlist.h \
	ipmipower_authentication.h \
	ipmipower_check.h \
	ipmipower_cipher_suite.h \
	ipmipower_config.h \
	ipmipower_connection.h \
	ipmipower.h \
	ipmipower_ipmi_version.h \
	ipmipower_output.h \
	ipmipower_packet.h \
	ipmipower_ping.h \
	ipmipower_powercmd.h \
	ipmipower_privilege.h \
	ipmipower_prompt.h \
	ipmipower_util.h \
	ipmipower_wrappers.h \
	wrappers.h
 
../../common/src/libcbuf.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libllnlcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
force-dependency-check:
