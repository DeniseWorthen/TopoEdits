module maskdefs

  use netcdf

  implicit none

  integer, parameter :: max_maskvars = 20
  
  type mskdefs
    character(len=12)   ::  var_name = ''
    character(len=64)   :: long_name = ''
    character(len=14)   :: unit_name = ''
    character(len= 2)   ::  var_type = 'r4'
    integer             ::  var_ndims = 2
  end type mskdefs

  type(mskdefs) :: mskgrid(max_maskvars)

  integer :: nmaskvars

  contains

  subroutine msk_typedefine

  integer :: ii = 0

   ii = ii + 1
   mskgrid(ii)%var_name  = 'lonBu'
   mskgrid(ii)%long_name = 'Longitude of corner (Bu) points'
   mskgrid(ii)%unit_name = 'degrees_east'
   mskgrid(ii)%var_type  = 'r8'

   ii = ii + 1
   mskgrid(ii)%var_name  = 'latBu'
   mskgrid(ii)%long_name = 'Latitude of corner (Bu) points'
   mskgrid(ii)%unit_name = 'degrees_north'
   mskgrid(ii)%var_type  = 'r8'

   ii = ii + 1
   mskgrid(ii)%var_name  = 'lonCt'
   mskgrid(ii)%long_name = 'Longitude of center (Ct) points'
   mskgrid(ii)%unit_name = 'degrees_east'
   mskgrid(ii)%var_type  = 'r8'

   ii = ii + 1
   mskgrid(ii)%var_name  = 'latCt'
   mskgrid(ii)%long_name = 'Latitude of center (Ct) points'
   mskgrid(ii)%unit_name = 'degrees_north'
   mskgrid(ii)%var_type  = 'r8'

   ii = ii + 1
   mskgrid(ii)%var_name  = 'wet'
   mskgrid(ii)%long_name = 'land mask'
   mskgrid(ii)%unit_name = 'nd'

   ii = ii + 1
   mskgrid(ii)%var_name  = 'dum'
   mskgrid(ii)%long_name = 'land mask Cu grid'
   mskgrid(ii)%unit_name = 'nd'

   ii = ii + 1
   mskgrid(ii)%var_name  = 'dvm'
   mskgrid(ii)%long_name = 'land mask Cv grid'
   mskgrid(ii)%unit_name = 'nd'

   ii = ii + 1
   mskgrid(ii)%var_name  = 'depth'
   mskgrid(ii)%long_name = 'ocean bathymetry'
   mskgrid(ii)%unit_name = 'm'
   mskgrid(ii)%var_type  = 'r8'

   ii = ii + 1
   mskgrid(ii)%var_name  = 'xdepth'
   mskgrid(ii)%long_name = 'modified ocean bathymetry'
   mskgrid(ii)%unit_name = 'm'
   mskgrid(ii)%var_type  = 'r8'

   ii = ii + 1
   mskgrid(ii)%var_name  = 'modmask'
   mskgrid(ii)%long_name = 'final modified land mask'
   mskgrid(ii)%unit_name = 'nd'

   ! intermediate fields
   ii = ii + 1
   mskgrid(ii)%var_name  = 'kmtsum'
   mskgrid(ii)%long_name = '4point mask sum'
   mskgrid(ii)%unit_name = 'nd'
   mskgrid(ii)%var_ndims = 3

   ii = ii + 1
   mskgrid(ii)%var_name  = 'kmtii'
   mskgrid(ii)%long_name = 'pinch i'
   mskgrid(ii)%unit_name = 'nd'
   mskgrid(ii)%var_ndims = 3

   ii = ii + 1
   mskgrid(ii)%var_name  = 'kmtjj'
   mskgrid(ii)%long_name = 'pinch j'
   mskgrid(ii)%unit_name = 'nd'
   mskgrid(ii)%var_ndims = 3

   ii = ii + 1
   mskgrid(ii)%var_name  = 'kmtij'
   mskgrid(ii)%long_name = 'kiss pinch'
   mskgrid(ii)%unit_name = 'nd'
   mskgrid(ii)%var_ndims = 3

   ii = ii + 1
   mskgrid(ii)%var_name  = 'xwet'
   mskgrid(ii)%long_name = 'modified land mask'
   mskgrid(ii)%unit_name = 'nd'
   mskgrid(ii)%var_ndims = 3

   ii = ii + 1
   mskgrid(ii)%var_name  = 'uvm'
   mskgrid(ii)%long_name = 'cice u-mask'
   mskgrid(ii)%unit_name = 'nd'
   mskgrid(ii)%var_ndims = 3

   nmaskvars = ii
 end subroutine msk_typedefine
end module maskdefs
