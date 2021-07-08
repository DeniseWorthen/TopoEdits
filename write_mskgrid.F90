subroutine write_mskgrid
 
   use param
   use grdvars
   use charstrings
   use maskdefs
   use netcdf

   implicit none

  ! local variables

  character(len=256) :: fname_out, fname_in
  integer :: ii,id,rc, ncid, dim1(1), dim2(2), dim3(3)
  integer :: xtdim, ytdim, xudim, yudim, kdim

  integer :: i,j

   real(kind=4), dimension(ni) :: xtpos, xupos
   real(kind=4), dimension(nj) :: ytpos, yupos

   character(len=2)  :: vtype
   character(len=12) :: vname
!--------------------------------------------------------------------
!c set up integer grid positions of Ct and Bu grids
!c--------------------------------------------------------------------

    xtpos(1) = 0.5
    xupos(1) = 1.0
   do i = 2,ni
    xtpos(i) = xtpos(i-1) + 1.0
    xupos(i) = xupos(i-1) + 1.0
   enddo

    ytpos(1) = 0.5
    yupos(1) = 1.0
   do j = 2,nj
    ytpos(j) = ytpos(j-1) + 1.0
    yupos(j) = yupos(j-1) + 1.0
   enddo

!---------------------------------------------------------------------
! local variables
!---------------------------------------------------------------------

  call msk_typedefine

  !fname_out= trim(dirout)//'mask.'//trim(res)//'.nc'
  fname_out= 'mask.mx'//trim(res)//'.nc'

  rc = nf90_create(fname_out, nf90_write, ncid)
  print *, 'writing masks to ',trim(fname_out)
  print *, 'nf90_create = ',trim(nf90_strerror(rc))

  rc = nf90_def_dim(ncid, 'Xt',     ni, xtdim)
  rc = nf90_def_dim(ncid, 'Yt',     nj, ytdim)
  rc = nf90_def_dim(ncid, 'Xu',     ni, xudim)
  rc = nf90_def_dim(ncid, 'Yu',     nj, yudim)
  rc = nf90_def_dim(ncid, 'nk', nsteps,  kdim)

  dim2(2) = yudim
  dim2(1) = xudim
  do ii = 1,2
   if(len_trim(mskgrid(ii)%var_name) .gt. 0)then
   print *, 'write = ',ii,'  '//trim(mskgrid(ii)%var_name)//'  '//trim(mskgrid(ii)%var_type)
   vtype = trim(mskgrid(ii)%var_type)
   vname = trim(mskgrid(ii)%var_name)
   if(vtype .eq. 'r8')rc = nf90_def_var(ncid, vname, nf90_double, dim2, id)
   if(vtype .eq. 'r4')rc = nf90_def_var(ncid, vname, nf90_float,  dim2, id)
   if(vtype .eq. 'i4')rc = nf90_def_var(ncid, vname, nf90_int,    dim2, id)
   rc = nf90_put_att(ncid, id,     'units', trim(mskgrid(ii)%unit_name))
   rc = nf90_put_att(ncid, id, 'long_name', trim(mskgrid(ii)%long_name))
   end if
  enddo
  
  dim2(2) = ytdim
  dim2(1) = xtdim
  do ii = 3,7
   if(len_trim(mskgrid(ii)%var_name) .gt. 0)then
   print *, 'write = ',ii,'  '//trim(mskgrid(ii)%var_name)//'  '//trim(mskgrid(ii)%var_type)
   vtype = trim(mskgrid(ii)%var_type)
   vname = trim(mskgrid(ii)%var_name)
   if(vtype .eq. 'r8')rc = nf90_def_var(ncid, vname, nf90_double, dim2, id)
   if(vtype .eq. 'r4')rc = nf90_def_var(ncid, vname, nf90_float,  dim2, id)
   if(vtype .eq. 'i4')rc = nf90_def_var(ncid, vname, nf90_int,    dim2, id)
   rc = nf90_put_att(ncid, id,     'units', trim(mskgrid(ii)%unit_name))
   rc = nf90_put_att(ncid, id, 'long_name', trim(mskgrid(ii)%long_name))
   end if
  enddo

  dim3(3) =  kdim
  dim3(2) = ytdim
  dim3(1) = xtdim
  do ii = 8,nmskvars
   if(len_trim(mskgrid(ii)%var_name) .gt. 0)then
   print *, 'write = ',ii,'  '//trim(mskgrid(ii)%var_name)//'  '//trim(mskgrid(ii)%var_type)
   vtype = trim(mskgrid(ii)%var_type)
   vname = trim(mskgrid(ii)%var_name)
   if(vtype .eq. 'r8')rc = nf90_def_var(ncid, vname, nf90_double, dim3, id)
   if(vtype .eq. 'r4')rc = nf90_def_var(ncid, vname, nf90_float,  dim3, id)
   if(vtype .eq. 'i4')rc = nf90_def_var(ncid, vname, nf90_int,    dim3, id)
   rc = nf90_put_att(ncid, id,     'units', trim(mskgrid(ii)%unit_name))
   rc = nf90_put_att(ncid, id, 'long_name', trim(mskgrid(ii)%long_name))
   end if
  enddo

  dim1(1) = xtdim
  rc = nf90_def_var(ncid,     'Xt', nf90_float, dim1, id)
  !rc = nf90_put_att(ncid, id, 'axis', 'x')
  dim1(1) = xudim
  rc = nf90_def_var(ncid,     'Xu', nf90_float, dim1, id)
  !rc = nf90_put_att(ncid, id, 'axis', 'x')
  dim1(1) = ytdim
  rc = nf90_def_var(ncid,     'Yt', nf90_float, dim1, id)
  !rc = nf90_put_att(ncid, id, 'axis', 'y')
  dim1(1) = yudim
  rc = nf90_def_var(ncid,     'Yu', nf90_float, dim1, id)
  !rc = nf90_put_att(ncid, id, 'axis', 'y')

  rc = nf90_enddef(ncid)

  ! axes
  rc = nf90_inq_varid(ncid, 'Xt',    id)
  rc = nf90_put_var(ncid,     id, xtpos)
  rc = nf90_inq_varid(ncid, 'Xu',    id)
  rc = nf90_put_var(ncid,     id, xupos)
  rc = nf90_inq_varid(ncid, 'Yt',    id)
  rc = nf90_put_var(ncid,     id, ytpos)
  rc = nf90_inq_varid(ncid, 'Yu',    id)
  rc = nf90_put_var(ncid,     id, yupos)

  ! coords
  rc = nf90_inq_varid(ncid, 'lonCt',      id)
  rc = nf90_put_var(ncid,        id,   lonCt)

  rc = nf90_inq_varid(ncid, 'latCt',      id)
  rc = nf90_put_var(ncid,        id,   latCt)

  rc = nf90_inq_varid(ncid, 'lonBu',      id)
  rc = nf90_put_var(ncid,        id,   lonBu)

  rc = nf90_inq_varid(ncid, 'latBu',      id)
  rc = nf90_put_var(ncid,        id,   latBu)

  rc = nf90_inq_varid(ncid,   'wet',      id)
  rc = nf90_put_var(ncid,        id,    wet4)

  rc = nf90_inq_varid(ncid, 'depth',      id)
  rc = nf90_put_var(ncid,        id,   depth)

  rc = nf90_inq_varid(ncid,'xdepth',      id)
  rc = nf90_put_var(ncid,        id,  xdepth)

  ! altered masks
  rc = nf90_inq_varid(ncid, 'kmtsum',      id)
  rc = nf90_put_var(ncid,         id,  kmtsum)
 
  rc = nf90_inq_varid(ncid,  'kmtii',     id)
  rc = nf90_put_var(ncid,         id,  kmtii)

  rc = nf90_inq_varid(ncid,  'kmtjj',     id)
  rc = nf90_put_var(ncid,         id,  kmtjj)

  rc = nf90_inq_varid(ncid,   'xwet',     id)
  rc = nf90_put_var(ncid,         id,   xwet)

  rc = nf90_inq_varid(ncid,    'uvm',     id)
  rc = nf90_put_var(ncid,         id,    uvm)

  rc = nf90_close(ncid)

end subroutine write_mskgrid
