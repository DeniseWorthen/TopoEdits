subroutine write_mskgrid
 
   use param
   use grdvars
   use charstrings
   use maskdefs
   use netcdf

   implicit none

  ! local variables

  character(len=256) :: fname_out, fname_in
  integer :: ii,id,rc, ncid, vardim(2)
  integer :: ni_dim,nj_dim

!---------------------------------------------------------------------
! local variables
!---------------------------------------------------------------------

  call msk_typedefine

  !fname_out= trim(dirout)//'mask.'//trim(res)//'.nc'
  fname_out= 'mask.'//trim(res)//'.nc'

  rc = nf90_create(fname_out, nf90_write, ncid)
  print *, 'writing masks to ',trim(fname_out)
  print *, 'nf90_create = ',trim(nf90_strerror(rc))

  rc = nf90_def_dim(ncid,'ni', ni, ni_dim)
  rc = nf90_def_dim(ncid,'nj', nj, nj_dim)

  vardim(2) = nj_dim
  vardim(1) = ni_dim
  do ii = 1,nmskvars
   if(len_trim(mskgrid(ii)%var_name) .gt. 0)then
   print *, 'write = ',ii,'  '//trim(mskgrid(ii)%var_name)//'  '//trim(mskgrid(ii)%var_type)
   if(trim(mskgrid(ii)%var_type) .eq. 'r8')rc = nf90_def_var(ncid, &
                  trim(mskgrid(ii)%var_name), nf90_double, vardim, id)
   if(trim(mskgrid(ii)%var_type) .eq. 'r4')rc = nf90_def_var(ncid, &
                  trim(mskgrid(ii)%var_name), nf90_float, vardim, id)
   if(trim(mskgrid(ii)%var_type) .eq. 'i4')rc = nf90_def_var(ncid, &
                  trim(mskgrid(ii)%var_name), nf90_int,    vardim, id)
   rc = nf90_put_att(ncid, id,     'units', trim(mskgrid(ii)%unit_name))
   !print *, trim(nf90_strerror(rc))
   rc = nf90_put_att(ncid, id, 'long_name', trim(mskgrid(ii)%long_name))
   !print *, trim(nf90_strerror(rc))
   !rc = nf90_put_att(ncid, id, '_FillValue', mskgrid(ii)%var_mval)
   !print *, trim(nf90_strerror(rc))
   end if
  enddo
   rc = nf90_enddef(ncid)
   print *, trim(nf90_strerror(rc))

  rc = nf90_inq_varid(ncid,  'tlon',      id)
  rc = nf90_put_var(ncid,        id,   lonCt)
   print *, 'tlon ',trim(nf90_strerror(rc))

  rc = nf90_inq_varid(ncid,  'tlat',      id)
  rc = nf90_put_var(ncid,        id,   latCt)
   print *, 'tlat ',trim(nf90_strerror(rc))

  rc = nf90_inq_varid(ncid,   'wet',      id)
  rc = nf90_put_var(ncid,        id,    wet4)

  rc = nf90_inq_varid(ncid, 'kmtsum',      id)
  rc = nf90_put_var(ncid,         id,  kmtsum)
 
  rc = nf90_inq_varid(ncid,  'kmtii',     id)
  rc = nf90_put_var(ncid,         id,  kmtii)
 
  rc = nf90_inq_varid(ncid,  'kmtjj',     id)
  rc = nf90_put_var(ncid,         id,  kmtjj)

  rc = nf90_close(ncid)

end subroutine write_mskgrid
