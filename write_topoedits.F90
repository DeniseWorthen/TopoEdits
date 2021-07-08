subroutine write_topoedits

   use param
   use grdvars
   use charstrings
   use netcdf

   implicit none

  ! local variables

  character(len=256) :: fname_out, fname_in
  integer :: dimid,i,ii,jj,kk,id,rc, ncid, dim1(1), dim2(2), ni_dim, nj_dim
  integer :: nvals, newvals

  integer, allocatable, dimension(:) :: orii, orij, newi, newj
  real(kind=4), allocatable, dimension(:) :: oriz, newz

   fname_in =  trim(dirsrc)//trim(res)//'/'//trim(topoeditfile)
  fname_out = trim(dirout)//'ufs.ciceBgrid.topoedits_mx'//trim(res)//'.nc'

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
    allocate(orii(nvals))
    allocate(orij(nvals))
    allocate(oriz(nvals))

    rc = nf90_open(fname_in, nf90_nowrite, ncid)
    rc = nf90_inq_varid(ncid, 'iEdit', id)
    rc = nf90_get_var(ncid, id, orii)
    rc = nf90_inq_varid(ncid, 'jEdit', id)
    rc = nf90_get_var(ncid, id, orij)
    rc = nf90_inq_varid(ncid, 'zEdit', id)
    rc = nf90_get_var(ncid, id, oriz)
    rc = nf90_close(ncid)
  else
    nvals = 0
  endif

!---------------------------------------------------------------------
! check for any point which goes from land->ocean in original topoedit
! file at run time and modify mask
! this mask is written to tripole file used to generated mapped ocean
! mask, ice mesh and regrid weights
!
! should find only for 1deg
!---------------------------------------------------------------------

  if(nvals .gt. 0)then
    do kk = 1,nvals
       ii = orii(kk); jj = orij(kk)
       if(depth(ii+1,jj+1) .eq. 0.0 .and. oriz(kk) .ne. 0.0)then
        print '(3i4,3f10.2)',kk,ii,jj,wet4(ii+1,jj+1),depth(ii+1,jj+1),oriz(kk)
        xwet(ii+1,jj+1,nsteps) = 1.0
       endif
    enddo
   endif
#ifdef test
!---------------------------------------------------------------------
! allocate space for change land mask
!---------------------------------------------------------------------

  newvals = nvals+npoints
  print *, 'found ',ii,' open water points at j=1 , newvals = ',newvals
 
  allocate(newi(1:newvals))
  allocate(newj(1:newvals))
  allocate(newz(1:newvals))

  newi(1:nvals) = orii(1:nvals)
  newj(1:nvals) = orij(1:nvals)
  newz(1:nvals) = oriz(1:nvals)

  ii = nvals
  do i = 1,ni
   if(xwet(i,j,nsteps) .eq. 1.0)then
     ii = ii+1
     newi(ii) = i-1
     newj(ii) = 1-1
     newz(ii) = 0.0
   end if
  end do

  !do i = 1,newvals
  ! print *,i,newi(i),newj(i),newz(i)
  !end do

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
  print *,trim(nf90_strerror(rc))
  rc = nf90_put_var(ncid,         id,     ni)
  print *,trim(nf90_strerror(rc))
  rc = nf90_inq_varid(ncid,     'nj',     id)
  rc = nf90_put_var(ncid,         id,     nj)

  rc = nf90_inq_varid(ncid,  'iEdit',     id)
  rc = nf90_put_var(ncid,         id,    newi)
  rc = nf90_inq_varid(ncid,  'jEdit',     id)
  rc = nf90_put_var(ncid,         id,    newj)
  rc = nf90_inq_varid(ncid,  'zEdit',     id)
  rc = nf90_put_var(ncid,         id,    newz)

  rc = nf90_close(ncid)
#endif

end subroutine write_topoedits
