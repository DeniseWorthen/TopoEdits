module grdvars

  use param
 
  implicit none

  ! super-grid source variables
  real(kind=8), dimension(0:nx,0:ny)   :: x, y

  ! ocean mask from fixed file, stored as either r4 or r8
     real(kind=4), dimension(ni,nj) :: wet4
     real(kind=8), dimension(ni,nj) :: wet8

     real(kind=4), dimension(ni,nj) :: dum, dvm

  ! intermediate masks
     real(kind=4), dimension(ni,nj,nsteps) :: kmtsum
     real(kind=4), dimension(ni,nj,nsteps) :: xwet
     real(kind=4), dimension(ni,nj,nsteps) :: kmtii
     real(kind=4), dimension(ni,nj,nsteps) :: kmtjj
     real(kind=4), dimension(ni,nj,nsteps) :: kmtij
  ! cice 'uvm' mask
     real(kind=4), dimension(ni,nj,nsteps) :: uvm

  ! ocn grid variables
  real(kind=8), dimension(ni,nj) :: lonCt, latCt
  real(kind=8), dimension(ni,nj) :: lonBu, latBu

  ! ocean bathy
  real(kind=8), dimension(ni,nj) ::  depth   ! default depth
  real(kind=8), dimension(ni,nj) :: xdepth   ! mask modified depth

  ! modified mask; the land mask which will be created at run time
  ! using the supplied topoedits, which maybe either land->ocean
  ! or ocean->land

   real(kind=4), dimension(ni,nj) :: modmask

end module grdvars
