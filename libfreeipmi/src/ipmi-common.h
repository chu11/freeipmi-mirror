#ifndef _IPMI_COMMON_H
#define _IPMI_COMMON_H

/* From David Wheeler's Secure Programming Guide */
void *guaranteed_memset(void *s, int c, size_t n);

/* Portable version of the extremely unportable Linux dprintf() */
int freeipmi_dprintf(int fd, char *fmt, ...);

#endif
