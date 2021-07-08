program topoedits
!
! Denise.Worthen@noaa.gov
!

  use netcdf
  use param
  use grdvars
  use edit_landmask
  use charstrings

  implicit none

  character(len=256) :: fname_out, fname_in

  integer :: rc,ncid,id,xtype
  integer :: i,j,i2,j2,im1,ip1
  integer :: ii,jj,k,nt
  integer :: cnt

  real(kind=8) :: xsum
  real(kind=4), dimension(ni,nj) :: maskdiff

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

  uvm = 0.0
  do j = 1,nj-1
   do i = 1,ni-1
    uvm(i,j,1) = min (wet4(i,j  ), wet4(i+1,j  ), &
                      wet4(i,j+1), wet4(i+1,j+1))
   enddo
  enddo

!---------------------------------------------------------------------
! read the bathymetry
!---------------------------------------------------------------------

  fname_in = trim(dirsrc)//trim(res)//'/'//trim(bathfile)//'.nc'

  rc = nf90_open(fname_in, nf90_nowrite, ncid)
  print *, 'reading ocean bathymetry from ',trim(fname_in)
  print *, 'nf90_open = ',trim(nf90_strerror(rc))

  rc = nf90_inq_varid(ncid,  trim(bathname), id)
  rc = nf90_inquire_variable(ncid,     id)
  rc = nf90_get_var(ncid,      id,  depth)
  rc = nf90_close(ncid)

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
    !deg
     lonBu(i,j) =     x(i2,j2)
     latBu(i,j) =     y(i2,j2)
   enddo
  enddo

!---------------------------------------------------------------------
! initial the updated landmask and iteratively adjust
!---------------------------------------------------------------------

   kmtsum = 0.0
    kmtii = 0.0; kmtjj = 0.0

    xwet(:,:,1) =  wet4(:,:)
    ! set j=1 to land in case it isn't
    xwet(:,1,1) = 0.0

   do nt = 2,nsteps
    xwet(:,:,nt) = xwet(:,:,nt-1)
! single points
    call singlepoints(nt)
! eliminate channels
    call fixchannels(nt)

! cice umask
    do j = 1,nj-1
     do i = 1,ni-1
      uvm(i,j,nt) = min (xwet(i,j,  nt), xwet(i+1,j,  nt), &
                         xwet(i,j+1,nt), xwet(i+1,j+1,nt))
     enddo
    enddo

   end do

   !?
   !call check_result

!---------------------------------------------------------------------
! adjust bathymetry
!---------------------------------------------------------------------

   xdepth = depth
   maskdiff(:,:) = wet4(:,:) - xwet(:,:,nsteps)
   xsum = sum(abs(maskdiff))
   print *,'number of changed points ',xsum,' total points ',ni*nj
   npoints = int(xsum,4)

   ! where land is added, set xdepth to 0.0
   where(maskdiff .gt. 0.0)xdepth = 0.0

   ! where land is removed, use mean of surrounding points
   do j = 2,nj-1
    do i = 1,ni
     if(maskdiff(i,j) .lt. 0.0)then
                   im1 = i-1
      if(i .eq.  1)im1 = ni
                   ip1 = i+1
      if(i .eq. ni)ip1 = 1

      xsum = depth(im1,j) + depth(ip1,j) + depth(i,j-1) + depth(i,j+1)
      xdepth(i,j) = xsum/4.0
     end if
    end do
   end do

!---------------------------------------------------------------------
! kludgy fix: 1-deg model has single point which switches froma
! land->ocean at run time. see issue #47 on NOAA-EMC/MOM6
!---------------------------------------------------------------------

    call write_topoedits

!   ii = 88; jj = 132
!   if(wet4(ii+1,jj+1) .eq. 0.0)wet4(ii+1,jj+1) = 1.0

!---------------------------------------------------------------------
! write out mask file for checking
!---------------------------------------------------------------------

   call write_mskgrid

end program topoedits
