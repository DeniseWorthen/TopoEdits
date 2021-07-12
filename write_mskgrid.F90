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

  integer :: i,j,ndims
  integer :: idimid, jdimid, kdimid

  character(len=2)  :: vtype
  character(len=12) :: vname
  character(len=40) :: vlong
  character(len=12) :: vunit

!---------------------------------------------------------------------
! local variables
!---------------------------------------------------------------------

  call msk_typedefine

  !fname_out= trim(dirout)//'mask.'//trim(res)//'.nc'
  fname_out= 'mask.mx'//trim(res)//'.nc'

  rc = nf90_create(fname_out, nf90_write, ncid)
  print *, 'writing masks to ',trim(fname_out)
  print *, 'nf90_create = ',trim(nf90_strerror(rc))

  rc = nf90_def_dim(ncid, 'ni',     ni, idimid)
  rc = nf90_def_dim(ncid, 'nj',     nj, jdimid)
  rc = nf90_def_dim(ncid, 'nk', nsteps, kdimid)

  do ii = 1,nmaskvars
   vname = trim(mskgrid(ii)%var_name)
   vlong = trim(mskgrid(ii)%long_name)
   vunit = trim(mskgrid(ii)%unit_name)
   vtype = trim(mskgrid(ii)%var_type)
   ndims = mskgrid(ii)%var_ndims
   if(ndims .eq. 3)then
    dim3(:) = (/idimid, jdimid, kdimid/)
    if(vtype .eq. 'r8')rc = nf90_def_var(ncid, vname, nf90_double, dim3, id)
    if(vtype .eq. 'r4')rc = nf90_def_var(ncid, vname, nf90_float,  dim3, id)
    if(vtype .eq. 'i4')rc = nf90_def_var(ncid, vname, nf90_int,    dim3, id)
   else
    dim2(:) = (/idimid, jdimid/)
    if(vtype .eq. 'r8')rc = nf90_def_var(ncid, vname, nf90_double, dim2, id)
    if(vtype .eq. 'r4')rc = nf90_def_var(ncid, vname, nf90_float,  dim2, id)
    if(vtype .eq. 'i4')rc = nf90_def_var(ncid, vname, nf90_int,    dim2, id)
   end if
    rc = nf90_put_att(ncid, id,     'units', vunit)
    rc = nf90_put_att(ncid, id, 'long_name', vlong)
  end do
  rc = nf90_enddef(ncid)

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

  rc = nf90_inq_varid(ncid,'modmask',     id)
  rc = nf90_put_var(ncid,        id, modmask)

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
