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
  integer :: ni_dim,nj_dim,nk_dim
  integer :: i,j

   real(kind=4), dimension(ni) :: xtpos, xupos
   real(kind=4), dimension(nj) :: ytpos, yupos

!--------------------------------------------------------------------
!c set up integer grid positions of T,U,V grids
!c--------------------------------------------------------------------

    xtpos(1) = 0.5
    xupos(1) = 0.0
   do i = 2,ni
    xtpos(i) = xtpos(i-1) + 1.0
    xupos(i) = xupos(i-1) + 1.0
   enddo

    ytpos(1) = 0.5
    yupos(1) = 0.0
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

  rc = nf90_def_dim(ncid, 'ni',     ni, ni_dim)
  rc = nf90_def_dim(ncid, 'nj',     nj, nj_dim)
  rc = nf90_def_dim(ncid, 'nk', nsteps, nk_dim)

  dim2(2) = nj_dim
  dim2(1) = ni_dim
  do ii = 1,3
   if(len_trim(mskgrid(ii)%var_name) .gt. 0)then
   print *, 'write = ',ii,'  '//trim(mskgrid(ii)%var_name)//'  '//trim(mskgrid(ii)%var_type)
   if(trim(mskgrid(ii)%var_type) .eq. 'r8')rc = nf90_def_var(ncid, &
                  trim(mskgrid(ii)%var_name), nf90_double, dim2, id)
   if(trim(mskgrid(ii)%var_type) .eq. 'r4')rc = nf90_def_var(ncid, &
                  trim(mskgrid(ii)%var_name), nf90_float, dim2, id)
   if(trim(mskgrid(ii)%var_type) .eq. 'i4')rc = nf90_def_var(ncid, &
                  trim(mskgrid(ii)%var_name), nf90_int,    dim2, id)
   rc = nf90_put_att(ncid, id,     'units', trim(mskgrid(ii)%unit_name))
   rc = nf90_put_att(ncid, id, 'long_name', trim(mskgrid(ii)%long_name))
   end if
  enddo

  dim3(3) = nk_dim
  dim3(2) = nj_dim
  dim3(1) = ni_dim
  do ii = 4,nmskvars
   if(len_trim(mskgrid(ii)%var_name) .gt. 0)then
   print *, 'write = ',ii,'  '//trim(mskgrid(ii)%var_name)//'  '//trim(mskgrid(ii)%var_type)
   if(trim(mskgrid(ii)%var_type) .eq. 'r8')rc = nf90_def_var(ncid, &
                  trim(mskgrid(ii)%var_name), nf90_double, dim3, id)
   if(trim(mskgrid(ii)%var_type) .eq. 'r4')rc = nf90_def_var(ncid, &
                  trim(mskgrid(ii)%var_name), nf90_float, dim3, id)
   if(trim(mskgrid(ii)%var_type) .eq. 'i4')rc = nf90_def_var(ncid, &
                  trim(mskgrid(ii)%var_name), nf90_int,    dim3, id)
   rc = nf90_put_att(ncid, id,     'units', trim(mskgrid(ii)%unit_name))
   rc = nf90_put_att(ncid, id, 'long_name', trim(mskgrid(ii)%long_name))
   end if
  enddo

  ! axes
  dim1(1) = ni_dim
  rc = nf90_def_var(ncid,     'Xt', nf90_float, dim1, id)
  rc = nf90_def_var(ncid,     'Xu', nf90_float, dim1, id)
  dim1(1) = nj_dim
  rc = nf90_def_var(ncid,     'Yt', nf90_float, dim1, id)
  rc = nf90_def_var(ncid,     'Yu', nf90_float, dim1, id)

  rc = nf90_enddef(ncid)

  rc = nf90_inq_varid(ncid, 'Xt', id)
  rc = nf90_put_var(ncid, id, xtpos)
  rc = nf90_inq_varid(ncid, 'Xu', id)
  rc = nf90_put_var(ncid, id, xupos)
  rc = nf90_inq_varid(ncid, 'Yt', id)
  rc = nf90_put_var(ncid, id, ytpos)
  rc = nf90_inq_varid(ncid, 'Yu', id)
  rc = nf90_put_var(ncid, id, yupos)

  rc = nf90_inq_varid(ncid,  'tlon',      id)
  rc = nf90_put_var(ncid,        id,   lonCt)

  rc = nf90_inq_varid(ncid,  'tlat',      id)
  rc = nf90_put_var(ncid,        id,   latCt)

  rc = nf90_inq_varid(ncid,   'wet',      id)
  rc = nf90_put_var(ncid,        id,    wet4)

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
