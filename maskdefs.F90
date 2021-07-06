module maskdefs

  implicit none

  integer, parameter :: nmskvars = 20
  
  type mskdefs
    character(len=12)   ::  var_name
    character(len=64)   :: long_name
    character(len=12)   :: unit_name
    character(len= 2)   ::  var_type
  end type mskdefs

  type(mskdefs) :: mskgrid(nmskvars)
  contains

  subroutine msk_typedefine

  integer :: ii = 0
  
   !default
   mskgrid(:)%var_name = ''
   mskgrid(:)%var_type = 'r4'

   ii = ii + 1
   mskgrid(ii)%var_name  = 'tlon'
   mskgrid(ii)%long_name = 'Longitude of center (Ct) points'
   mskgrid(ii)%unit_name = 'degrees'
   mskgrid(ii)%var_type  = 'r8'

   ii = ii + 1
   mskgrid(ii)%var_name  = 'tlat'
   mskgrid(ii)%long_name = 'Latitude of center (Ct) points'
   mskgrid(ii)%unit_name = 'degrees'
   mskgrid(ii)%var_type  = 'r8'

   ii = ii + 1
   mskgrid(ii)%var_name  = 'wet'
   mskgrid(ii)%long_name = 'land mask'
   mskgrid(ii)%unit_name = 'nd'

   ii = ii + 1
   mskgrid(ii)%var_name  = 'kmtsum'
   mskgrid(ii)%long_name = '4point mask sum'
   mskgrid(ii)%unit_name = 'nd'

   ii = ii + 1
   mskgrid(ii)%var_name  = 'kmtii'
   mskgrid(ii)%long_name = 'pinch i'
   mskgrid(ii)%unit_name = 'nd'

   ii = ii + 1
   mskgrid(ii)%var_name  = 'kmtjj'
   mskgrid(ii)%long_name = 'pinch j'
   mskgrid(ii)%unit_name = 'nd'

   ii = ii + 1
   mskgrid(ii)%var_name  = 'xwet'
   mskgrid(ii)%long_name = 'modified land mask'
   mskgrid(ii)%unit_name = 'nd'

 end subroutine msk_typedefine
end module maskdefs
