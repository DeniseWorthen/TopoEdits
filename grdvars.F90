module grdvars

  use param
 
  implicit none

  ! super-grid source variables
  real(kind=8), dimension(0:nx,0:ny)   :: x, y

  ! ocean mask from fixed file, stored as either r4 or r8
     real(kind=4), dimension(ni,nj) :: wet4
     real(kind=8), dimension(ni,nj) :: wet8

  ! intermediate masks
     real(kind=4), dimension(ni,nj,nsteps) :: kmtsum
     real(kind=4), dimension(ni,nj,nsteps) :: xwet
     real(kind=4), dimension(ni,nj,nsteps) :: kmtii
     real(kind=4), dimension(ni,nj,nsteps) :: kmtjj
  ! cice 'uvm' mask
     real(kind=4), dimension(ni,nj,nsteps) :: uvm

  ! ocn grid variables
  real(kind=8), dimension(ni,nj) :: lonCt, latCt


end module grdvars
