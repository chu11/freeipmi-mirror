MAN_PAGE=bmc-info.8
$(MAN_PAGE): $(MAN_PAGE).pre
	cpp -P -I. -x c $< -o $@

dist_man_MANS = $(MAN_PAGE)
CLEANFILES = $(MAN_PAGE)
DISTCLEANFILES = $(MAN_PAGE) $(MAN_PAGE).pre .deps/*.P
