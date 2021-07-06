module param

  implicit none
#ifdef output_grid_twelfdeg
  integer, parameter :: ni = 4500, nj = 3297
#endif
#ifdef output_grid_qdeg
  integer, parameter :: ni = 1440, nj = 1080
#endif
#ifdef output_grid_hdeg
  integer, parameter :: ni =  720, nj = 576
#endif
#ifdef output_grid_072deg
  integer, parameter :: ni =  500, nj = 381
#endif
#ifdef output_grid_1deg
  integer, parameter :: ni =  360, nj = 320
#endif
#ifdef output_grid_3deg
  integer, parameter :: ni =  120, nj =  84
#endif

  ! super-grid source variables
  integer, parameter :: nx  = ni*2, ny  = nj*2

  ! number of adjustment steps
  integer, parameter :: nsteps = 10

end module param
