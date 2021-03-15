
OPTS=-g -O0
OPTS=-ftz -g -O2 -finline-functions -finline-limit=500 -qopt-prefetch=4 -fast-transcendentals -fimf-use-svml -no-fma

FC = /home/mf/dp/marp/martinezs/apps/mkpack/support/wrapper/mimpifc-18.0.5.274 \
     -convert big_endian -assume byterecl -align array64byte,all -traceback -fpic \
     -qopenmp -qopenmp-threadprivate compat -fp-model source -qopt-report=5 -qopt-report-phase=vec \
     -march=core-avx2 -Winline $(OPTS) -DUSE_STACK


FC = pgf90 -mp -byteswapio -Mlarge_arrays -Minfo=mp -mp -O0 -g -DUSE_STACK
FC = pgf90 -mp -byteswapio -Mlarge_arrays -Minfo=mp -mp -O2 -g -DUSE_STACK
FC = pgf90 -acc -byteswapio -Mlarge_arrays -fast -Minfo=accel,all,intensity,ccff -ta=tesla:managed -O0 -g -DUSE_STACK -ta=tesla:lineinfo -DUSE_ACC 


all: wrap_lapinea.x

