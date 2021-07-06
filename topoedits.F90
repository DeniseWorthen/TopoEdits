program topoedits
!
! Denise.Worthen@noaa.gov
!

  use netcdf
  use param
  use grdvars
  use charstrings

  use maskdefs, only : mval4, mval8

  implicit none

  character(len=256) :: fname_out, fname_in

  integer :: rc,ncid,id,xtype
  integer :: i,j,i2,j2,im1,ip1
  integer :: ii,jj,k

!---------------------------------------------------------------------
! read the land mask
!---------------------------------------------------------------------

  fname_in = trim(dirsrc)//trim(res)//'/'//trim(maskfile)//'.nc'

  rc = nf90_open(fname_in, nf90_nowrite, ncid)
  print *, 'reading ocean mask from ',trim(fname_in)
  print *, 'nf90_open = ',trim(nf90_strerror(rc))

  rc = nf90_inq_varid(ncid,  trim(maskname), id)
  rc = nf90_inquire_variable(ncid, id, xtype=xtype)
  if(xtype .eq. 5)rc = nf90_get_var(ncid,      id,  wet4)
  if(xtype .eq. 6)rc = nf90_get_var(ncid,      id,  wet8)
  rc = nf90_close(ncid)

  if(xtype.eq. 6)wet4 = real(wet8,4)

  print *,minval(wet8),maxval(wet8)
  print *,minval(wet4),maxval(wet4)

!---------------------------------------------------------------------
! read supergrid file
!---------------------------------------------------------------------

  fname_in = trim(dirsrc)//trim(res)//'/'//'ocean_hgrid.nc'

  rc = nf90_open(fname_in, nf90_nowrite, ncid)
  print *, 'reading supergrid from ',trim(fname_in)
  print *, 'nf90_open = ',trim(nf90_strerror(rc))

  rc = nf90_inq_varid(ncid, 'x', id)  !lon
  rc = nf90_get_var(ncid,    id,  x)

  rc = nf90_inq_varid(ncid, 'y', id)  !lat
  rc = nf90_get_var(ncid,    id,  y)
  rc = nf90_close(ncid)
  print *,'super grid size ',size(y,1),size(y,2)

!---------------------------------------------------------------------
! fill grid variables
!---------------------------------------------------------------------

  do j = 1,nj
   do i = 1,ni
     i2 = 2*i ; j2 = 2*j
    !deg
     lonCt(i,j) =     x(i2-1,j2-1)
    !deg
     latCt(i,j) =     y(i2-1,j2-1)
   enddo
  enddo

!---------------------------------------------------------------------
! find the initial kmtsum
!---------------------------------------------------------------------

   kmtsum = 0.0
  do j = 2,nj-1
   do i = 1,ni
    if ((wet4(i,j) .eq. 1.0) .and. &
         (abs(latCt(i,j)) .ge. 60.0)) then
                  im1 = i-1
     if(i .eq.  1)im1 = ni
                  ip1 = i+1
     if(i .eq. ni)ip1 = 1

     kmtsum(i,j) = wet4(im1,j) + wet4(ip1,j) + wet4(i,j-1) + wet4(i,j+1)
    end if
   end do
  end do

!---------------------------------------------------------------------
! find the initial pinch points
!---------------------------------------------------------------------

   kmtii = 0.0; kmtjj = 0.0
   do j = 2,nj-1
    do i = 1,ni
    if ((wet4(i,j) .eq. 1.0) .and. &
         (abs(latCt(i,j)) .ge. 60.0)) then
                  im1 = i-1
     if(i .eq.  1)im1 = ni
                  ip1 = i+1
     if(i .eq. ni)ip1 = 1

     kmtii(i,j) = wet4(im1,j) + wet4(ip1,j)
     kmtjj(i,j) = wet4(i,j-1) + wet4(i,j+1)
    end if
   end do
  end do

!---------------------------------------------------------------------
! kludgy fix: 1-deg model has single point which switches froma
! land->ocean at run time. see issue #47 on NOAA-EMC/MOM6
!---------------------------------------------------------------------

!   ii = 88; jj = 132
!   if(wet4(ii+1,jj+1) .eq. 0.0)wet4(ii+1,jj+1) = 1.0

!---------------------------------------------------------------------
! adjust land mask at j=1 and for ocean pts w/ less than 2 active
! neighbors S of 60S
!---------------------------------------------------------------------

!   call adjust_landmask

!---------------------------------------------------------------------
! write out grid file files
!---------------------------------------------------------------------

   call write_mskgrid

end program topoedits