sbin_PROGRAMS=ipmipower
ipmipower_SOURCES= \
	argv.c \
	ierror.c \
	ipmipower_check.c \
	ipmipower_argp.c \
	ipmipower_connection.c \
	ipmipower.c \
	ipmipower_output.c \
	ipmipower_packet.c \
	ipmipower_ping.c \
	ipmipower_powercmd.c \
	ipmipower_prompt.c \
	ipmipower_util.c \
	ipmipower_wrappers.c \
	wrappers.c
 
ipmipower_LDADD = \
	../../common/src/libtoolcommon.la \
	../../common/src/libmiscutils.la \
	../../ipmidetect/src/libipmidetect/libipmidetect.la \
	../../libfreeipmi/src/libfreeipmi.la \
	../../common/src/libdebugcommon.la
 
ipmipower_CPPFLAGS = \
	-I$(srcdir)/../../common/src \
	-I$(srcdir)/../../ipmidetect/src/libipmidetect \
	-I$(srcdir)/../../libfreeipmi/include \
	-DWITH_LSD_FATAL_ERROR_FUNC=1 \
	-DWITH_LSD_NOMEM_ERROR_FUNC=1	

noinst_HEADERS= \
	argv.h \
	ierror.h \
	ipmipower_check.h \
	ipmipower_argp.h \
	ipmipower_connection.h \
	ipmipower.h \
	ipmipower_output.h \
	ipmipower_packet.h \
	ipmipower_ping.h \
	ipmipower_powercmd.h \
	ipmipower_prompt.h \
	ipmipower_util.h \
	ipmipower_wrappers.h \
	wrappers.h
 
../../common/src/libmiscutils.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libtoolcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../libfreeipmi/src/libfreeipmi.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../ipmidetect/src/libipmidetect/libipmidetect.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)

../../common/src/libdebugcommon.la: force-dependency-check
	$(MAKE) -C $(dir $@) $(notdir $@)
 
force-dependency-check:
