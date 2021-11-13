#ifndef _STACK_N
#define _STACK_N

#ifdef USE_STACK

#define temp(t, n, s) t, DIMENSION s :: n; POINTER (IP_##n##_, n)

#define alloc(n) IP_##n##_=LOC(YLSTACK%L);YLSTACK%L=YLSTACK%L+8*SIZE(n);IF(YLSTACK%L>YLSTACK%U)CALL ABOR1("")

#else

#define temp(t, n, s) t, DIMENSION s :: n
#define alloc(n)

#endif

#endif
