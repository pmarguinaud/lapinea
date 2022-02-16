MODULE YOMMP0


#include "create.h"

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

! ------------------------------------------------------------------------------------
!*    Not geometry-dependent variables describing distributed memory parallelization,
!     and some additional variables required in NAMPAR0/SUMPINI.
!     Values are identical for all models run under the OOPS layer.
! ------------------------------------------------------------------------------------

! ---------------------------------------

!  mp_type     :  1=blocked   (MPI_SEND/RECV)
!              :  2=buffered  (MPI_BSEND/MPI_BRECV)
!              :  3=immediate (MPI_ISEND/MPI_IRECV)
!  mbx_size    :  user-provided mailbox size

!  myproc      :  logical processor id (is in the range 1 to nproc)
!  myseta      :  own processor set a (is in the range 1 to nprgpns)
!  mysetb      :  own processor set b (is in the range 1 to nprgpew)
!  my_region_ns:  own processor set a (is in the range 1 to n_regions_ns)
!  my_region_ew:  own processor set b (is in the range 1 to n_regions_ew)
!  mysetw      :  own processor set a in wave space (1..nprtrw)   
!  mysetv      :  own processor set b in wave space (1..nprtrv)    
!  mysetm      :  own processor set a in spectral space (1..nprtrm)    
!  mysetn      :  own processor set b in spectral space (1..nprtrn)    

!  n_regions_ns:  number of regions (LEQ_REGIONS=T) or NPRGPNS (LEQ_REGIONS=F)
!  n_regions_ew:  maximum number of partitions for all regions
!  n_regions   :  number of partitions in each region
!  ngpset2pe   :  grid point space processor mapping array (n_regions_ns,n_regions_ew)
!  nslpad      :  number of pad words initialised to a huge number at either
!                 of side of the sl halo, used to trap halo problems.
!                 The default is 0. 
!  nouttype    :  type of output (post) processing to be performed
!              :  1=pbio
!              :  2=output to FDB
!              :  3=shared blocking MPI-I/O
!              :  4=shared blocking collective MPI-I/O
!              :  5=shared non-blocking MPI_I/O
!              :  6=shared non-blocking collective MPI_I/O
!  nwrtout     :  to be described
!  nfldin      :  number of input  fields to be buffered during distribution
!  nprcids(nproc) : array containing the process ids. It is the mapping
!                 between the process numbering in the application
!                 (from 1 to NPROC) and the numbering used by the
!                 underlying communication library.
!  ndistio(1)   : if set to 1, then fields with be written in separate files
!                (1 file per nstrout proc and per forecast term). 
!                 if set to 2, then fields with be written in separate files
!                (1 file per nstrout proc; all forecast terms in)
!  ndistio(2)   : disable write by proc #1
!  ndistio(3)   : disable field gathering on proc #1
!  ndistio(4)   : disable field compression
!  lusewrgridall     : re-create all grid point fields in one shot
!  ndistio     : if set to 1, then fields with be written in separate files
!                (1 file/nstrout proc).
!  lsplit      :  true - latitudes are shared between a-sets
!                 false - a latitude belongs to only one a-set
!  leq_regions :  true - use new eq_regions partitioning
!                 false - use old NPRGPNS x NPRGPEW partitioning
!  lsplitout   :  output data provided in sequential files (.true.) or
!                 in directories (.false.)
!  lsync_slcom : true: activate a barrier in SL communications
!  lsync_trans : true: activate a barrier in spectral transforms
!  lsldebug    : true: diagnostics regarding use of SL halos is computed and printed
!  lslondem    : .true. default value for sl on demand (used for model)
!  ncombflen : Size of communication buffer. This is the maximum per
!              processor buffer space (in words) that the IFS should use
!              for one or more sends before receives are issued from
!              destination processors.
!  LMPOFF  : .T. = switch off the message passing library initialisation
!                  requested for special cases using NPROC=1
!          : .F. = (default) full message passing features
!  NSPECRESMIN : Minimum spectral resolution used for controlling NPRTRW
!                NPRTRW will not be allowed to be greater than NSPECRESMIN
!  LOUTPUT : .T. = diagnostic output requested on this PE
!  NOUTPUT : 0 = No diagnostic output
!            1 = Only diagnostic output from PE1 ( default )
!            2 = Diagnostic output from all PEs into separate files
!  LMPDIAG : .T. = extensive message passing diagnostic output requested
!  NPRINTLEV   : 0 = "basic prints'; 1 = "more prints"; 2 = "debug prints"
!  LOPT_SCALAR : .T. = to run code optimised for scalar machines.
!  LOPT_RS6K   : .T. = to run code optimised on IBM RS6000 architecture.
!  LSCMEC      : .T. = ECMWF Single Column Model
!                This variable is in YOMMP0 because required in NAMPAR0+SUMPINI

!  * --- Number of processors:
!  NPROC   : Total number of processors requested for this run
!  NPRGPNS : Number of processors used during grid-point phase in North-South
!            direction (previously known as NPROCA)
!  NPRGPEW : Number of processors used during grid-point phase in East-West
!            direction (previously known as NPROCB)
!  NPRTRNS : Number of processors used during transform phase in North-South
!            direction (previously the same as NPROCA and now implemented
!            such that NPRTRNS=NPTRW)
!  NPRTRN  : Number of processors used during spectral computations in total
!            wave direction (previously the same as NPROCA and now implemented
!            such that NPRTRN=NPTRV)
!  NPRTRW  : Number of processors used during transform phase in wave space
!            (previously the same as NPROCA)
!  NPRTRV  : Number of processors used during transform phase in vertical
!            direction (previously known as NPROCB)
!  NSTRIN  : Number of processors required to perform input processing
!  NSTROUT : Number of processors required to perform output processing

INTEGER(KIND=JPIM), ALLOCATABLE :: NPRCIDS(:)
create (NPRCIDS)
INTEGER(KIND=JPIM), ALLOCATABLE, TARGET :: NGPSET2PE(:,:)
create (NGPSET2PE)
INTEGER(KIND=JPIM)              :: N_REGIONS_NS
create (N_REGIONS_NS)
INTEGER(KIND=JPIM)              :: N_REGIONS_EW
create (N_REGIONS_EW)
INTEGER(KIND=JPIM), TARGET, ALLOCATABLE :: N_REGIONS(:)
create (N_REGIONS)
LOGICAL :: LSPLIT
create (LSPLIT)
LOGICAL :: LEQ_REGIONS
create (LEQ_REGIONS)
LOGICAL :: LSPLITOUT
create (LSPLITOUT)
LOGICAL :: LSYNC_SLCOM
create (LSYNC_SLCOM)
LOGICAL :: LSYNC_TRANS
create (LSYNC_TRANS)
LOGICAL :: LSLDEBUG
create (LSLDEBUG)
LOGICAL :: LSLONDEM

create (LSLONDEM)
INTEGER(KIND=JPIM) :: MP_TYPE
create (MP_TYPE)
INTEGER(KIND=JPIM) :: MBX_SIZE
create (MBX_SIZE)
INTEGER(KIND=JPIM) :: MYPROC
create (MYPROC)
INTEGER(KIND=JPIM) :: MYSETA
create (MYSETA)
INTEGER(KIND=JPIM) :: MYSETB
create (MYSETB)
INTEGER(KIND=JPIM) :: MYSETW
create (MYSETW)
INTEGER(KIND=JPIM) :: MYSETV
create (MYSETV)
INTEGER(KIND=JPIM) :: MYSETM
create (MYSETM)
INTEGER(KIND=JPIM) :: MYSETN
create (MYSETN)
INTEGER(KIND=JPIM) :: MY_REGION_NS
create (MY_REGION_NS)
INTEGER(KIND=JPIM) :: MY_REGION_EW
create (MY_REGION_EW)
INTEGER(KIND=JPIM) :: NFLDIN
create (NFLDIN)
INTEGER(KIND=JPIM) :: NSLPAD
create (NSLPAD)
INTEGER(KIND=JPIM) :: NOUTTYPE
create (NOUTTYPE)
INTEGER(KIND=JPIM) :: NWRTOUT
create (NWRTOUT)
INTEGER(KIND=JPIM) :: NDISTIO(50)
create (NDISTIO)
LOGICAL            :: LUSEWRGRIDALL
create (LUSEWRGRIDALL)
INTEGER(KIND=JPIM) :: NCOMBFLEN
create (NCOMBFLEN)
LOGICAL :: LMPOFF
create (LMPOFF)
INTEGER(KIND=JPIM) :: NSPECRESMIN
create (NSPECRESMIN)
LOGICAL :: LOUTPUT
create (LOUTPUT)
INTEGER(KIND=JPIM) :: NOUTPUT
create (NOUTPUT)
INTEGER(KIND=JPIM) :: NPRINTLEV
create (NPRINTLEV)
LOGICAL :: LMPDIAG
create (LMPDIAG)
LOGICAL :: LOPT_SCALAR
create (LOPT_SCALAR)
LOGICAL :: LOPT_RS6K
create (LOPT_RS6K)
LOGICAL :: LSCMEC

create (LSCMEC)
! Use of synchronization/blocking in Transpose (some networks do get flooded)
! 0 = Post IRECVs up-front, use ISENDs, use WAITANY to recv data (default)
! 1 = Use ISENDs, use blocking RECVs, add barrier at the end of each cycle
! 2 = Use buffered SENDs, use blocking RECVs, add barrier at the end of each cycle
INTEGER(KIND=JPIM) :: NTRANS_SYNC_LEVEL = 0

create (NTRANS_SYNC_LEVEL)
! Use of synchronization/blocking in SLCOMM(s) (some networks do get flooded)
! 0 = Post IRECVs and ISENDs followed by WAITALLs (default)
! 1 = Use ISENDs, but use blocking RECVs
! 2 = Use buffered SENDs and blocking RECVs
INTEGER(KIND=JPIM) :: NSLCOMM_SYNC_LEVEL = 0


create (NSLCOMM_SYNC_LEVEL)
INTEGER(KIND=JPIM) :: NPROC
create (NPROC)
INTEGER(KIND=JPIM) :: NPRGPNS
create (NPRGPNS)
INTEGER(KIND=JPIM) :: NPRGPEW
create (NPRGPEW)
INTEGER(KIND=JPIM) :: NPRTRNS
create (NPRTRNS)
INTEGER(KIND=JPIM) :: NPRTRN
create (NPRTRN)
INTEGER(KIND=JPIM) :: NPRTRW
create (NPRTRW)
INTEGER(KIND=JPIM) :: NPRTRV
create (NPRTRV)
INTEGER(KIND=JPIM) :: NSTRIN
create (NSTRIN)
INTEGER(KIND=JPIM) :: NSTROUT

create (NSTROUT)
! ----------------------------------------------------------------------

!     -- for use by diwrgrid*.F90 routine(s) : Override this via nampar1.h

INTEGER(KIND=JPIM) :: M_BARRINC_DIWRGRID = 3 

create (M_BARRINC_DIWRGRID)
#ifdef RS6K
LOGICAL :: L_GATHERV_WRGP = .FALSE.
create (L_GATHERV_WRGP)
#else
LOGICAL :: L_GATHERV_WRGP = .TRUE.
create (L_GATHERV_WRGP)
#endif

END MODULE YOMMP0

