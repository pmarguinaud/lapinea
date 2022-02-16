#ifdef USE_ACC
#define create(x) !$acc declare create(x)
#else
#define create(x)
#endif
