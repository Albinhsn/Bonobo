#ifndef cpplox_common_h
#define cpplox_common_h

#include <cstdint>

typedef struct {
  char *chars;
  int len;
} str;

#define DEBUG_STRESS_GC
#define DEBUG_LOG_GC
// #define DEBUG_PRINT_CODE
// #define DEBUG_TRACE_EXECUTION
#endif
