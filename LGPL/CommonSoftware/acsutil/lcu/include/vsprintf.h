#ifndef _VSPRINTF_H_
#define _VSPRINTF_H_

#ifdef __cplusplus
extern "C" {
#endif

size_t strnlen(const char * s, size_t count);

int vsnprintf(char *buf, size_t size, const char *fmt, va_list args);

int snprintf(char * buf, size_t size, const char *fmt, ...);

int vsprintf(char *buf, const char *fmt, va_list args);

int vsscanf(const char * buf, const char * fmt, va_list args);

#ifdef __cplusplus
}
#endif

#endif


