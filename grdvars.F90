module grdvars

  use param
 
  implicit none

  ! super-grid source variables
  real(kind=8), dimension(0:nx,0:ny)   :: x, y

  ! ocean mask from fixed file, stored as either r4 or r8
     real(kind=4), dimension(ni,nj) :: wet4
     real(kind=8), dimension(ni,nj) :: wet8
     real(kind=4), dimension(ni,nj) :: xwet

     real(kind=4), dimension(ni,nj) :: kmtsum
     real(kind=4), dimension(ni,nj) :: kmtii
     real(kind=4), dimension(ni,nj) :: kmtjj

  ! ocn grid variables
  real(kind=8), dimension(ni,nj) :: lonCt, latCt

end module grdvars
