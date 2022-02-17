FC = pgf90 -mp -byteswapio -Mlarge_arrays -fast -acc=gpu -Minfo=accel,all,intensity,ccff -gpu=lineinfo -O3 -DUSE_ACC -DUSE_STACK -cuda=charstring

