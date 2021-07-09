subroutine write_topoedits

   use param
   use grdvars
   use charstrings
   use netcdf

   implicit none

  ! local variables

  character(len=256) :: fname_out, fname_in
  integer :: dimid,i,j,ii,jj,nv,id,rc, ncid, dim1(1), dim2(2), ni_dim, nj_dim
  integer :: nvals, newvals
  real :: diff

  integer, allocatable, dimension(:) :: iedit, jedit, iedit2, jedit2
  real(kind=4), allocatable, dimension(:) :: zedit, zedit2

   fname_in =  trim(dirsrc)//trim(res)//'/'//trim(topoeditfile)
  !fname_out = trim(dirout)//'ufs.ciceBgrid.topoedits_mx'//trim(res)//'.nc'
  fname_out = 'ufs.ciceBgrid.topoedits_mx'//trim(res)//'.nc'

!---------------------------------------------------------------------
! read existing topo edits
!---------------------------------------------------------------------

  if(len_trim(topoeditfile) .gt. 0)then
    rc = nf90_open(fname_in, nf90_nowrite, ncid)
    print *,'using topo edits file ',trim(fname_in)

    rc = nf90_inq_dimid(ncid, 'nEdits', dimid)
    rc = nf90_inquire_dimension(ncid, dimid, len=nvals)
    rc = nf90_close(ncid)

    ! return the existing values
    allocate(iedit(nvals))
    allocate(jedit(nvals))
    allocate(zedit(nvals))

    rc = nf90_open(fname_in, nf90_nowrite, ncid)
    rc = nf90_inq_varid(ncid, 'iEdit',    id)
    rc = nf90_get_var(ncid,        id, iedit)
    rc = nf90_inq_varid(ncid, 'jEdit',    id)
    rc = nf90_get_var(ncid,        id, jedit)
    rc = nf90_inq_varid(ncid, 'zEdit',    id)
    rc = nf90_get_var(ncid,        id, zedit)
    rc = nf90_close(ncid)

    ! adjust i,j by 1 to account for how MOM6 ingests the edits
    iedit = iedit+1
    jedit = jedit+1
  else
    nvals = 0
  endif

!---------------------------------------------------------------------
! allocate space for change land mask
!---------------------------------------------------------------------

  newvals = nvals+npoints

  allocate(iedit2(1:newvals))
  allocate(jedit2(1:newvals))
  allocate(zedit2(1:newvals))

  if(allocated(iedit))iedit2(1:nvals) = iedit(1:nvals)
  if(allocated(jedit))jedit2(1:nvals) = jedit(1:nvals)
  if(allocated(zedit))zedit2(1:nvals) = zedit(1:nvals)

  ii = nvals
  do j = 1,nj
   do i = 1,ni
    diff = wet4(i,j) - xwet(i,j,nsteps)
    if(diff .ne. 0.)then
     ii = ii+1
     iedit2(ii) = i
     jedit2(ii) = j
     zedit2(ii) = xdepth(i,j)
    end if
   end do
  end do

!---------------------------------------------------------------------
! change mask consistent w/ topoedits (applied at run time)
! this mask is written to tripole file and used to generate mapped ocean
! mask, ice mesh and regrid weights
!---------------------------------------------------------------------

   modmask(:,:) = xwet(:,:,nsteps)
   do nv = 1,nvals
      ii = iedit2(nv); jj = jedit2(nv)
      if(zedit2(nv) .eq. 0.0)then
        modmask(ii,jj) = 0.0
      else
        modmask(ii,jj) = 1.0
      endif
   enddo

!---------------------------------------------------------------------
! adjust i,j by 1 to account for how MOM6 ingests the edits
!---------------------------------------------------------------------

  do ii = 1,newvals
   iedit2(ii) = iedit2(ii)-1
   jedit2(ii) = jedit2(ii)-1
  end do
  do ii = 1,newvals
   print *,ii,iedit2(ii),jedit2(ii),zedit2(ii)
  end do

!---------------------------------------------------------------------
! create new topoedits file
!---------------------------------------------------------------------

  rc = nf90_create(fname_out, nf90_write, ncid)
  print *, 'writing new topo edits to ',trim(fname_out)

  rc = nf90_def_dim(ncid, 'nEdits', newvals, ni_dim)

  rc = nf90_def_var(ncid,    'ni', nf90_int,   id)
  rc = nf90_def_var(ncid,    'nj', nf90_int,   id)

  dim1(1) = ni_dim
  rc = nf90_def_var(ncid, 'iEdit', nf90_int,   dim1, id)
  rc = nf90_def_var(ncid, 'jEdit', nf90_int,   dim1, id)
  rc = nf90_def_var(ncid, 'zEdit', nf90_float, dim1, id)
  rc = nf90_put_att(ncid, nf90_global, 'history', trim(history))
  rc = nf90_enddef(ncid)
 
  rc = nf90_inq_varid(ncid,     'ni',     id)
  rc = nf90_put_var(ncid,         id,     ni)
  rc = nf90_inq_varid(ncid,     'nj',     id)
  rc = nf90_put_var(ncid,         id,     nj)

  rc = nf90_inq_varid(ncid,  'iEdit',     id)
  rc = nf90_put_var(ncid,         id, iedit2)
  rc = nf90_inq_varid(ncid,  'jEdit',     id)
  rc = nf90_put_var(ncid,         id, jedit2)
  rc = nf90_inq_varid(ncid,  'zEdit',     id)
  rc = nf90_put_var(ncid,         id, zedit2)

  rc = nf90_close(ncid)

end subroutine write_topoedits
