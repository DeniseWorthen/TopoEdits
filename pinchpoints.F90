module pinchpoints

 use param
 use grdvars

 contains

 subroutine fixchannels(nt)

  integer, intent(in) :: nt

!---------------------------------------------------------------------
! find the initial pinch points
!---------------------------------------------------------------------

   do j = 2,nj-1
    do i = 1,ni
    if ((xwet(i,j,nt-1) .eq. 1.0) .and. &
         (abs(latCt(i,j)) .ge. 60.0)) then
                  im1 = i-1
     if(i .eq.  1)im1 = ni
                  ip1 = i+1
     if(i .eq. ni)ip1 = 1

     if(xwet(im1,j,nt-1) .eq. 0.0 .and. xwet(ip1,j,nt-1) .eq. 0.0)kmtii(i,j,nt) = 1.0
     if(xwet(i,j-1,nt-1) .eq. 0.0 .and. xwet(i,j+1,nt-1) .eq. 0.0)kmtjj(i,j,nt) = 1.0
    end if
   end do
  end do

!---------------------------------------------------------------------
! for kmtii pinches, preferentially remove land at i-1
! for kmtjj pinches, preferentially remove land at j-1
!---------------------------------------------------------------------

   xwet(:,:,nt) = xwet(:,:,nt-1)
   do j = 1,nj
    do i = 2,ni
     if (kmtii(i,j,nt) .eq. 1.0) xwet(i-1,j,nt) = 1.0
    enddo
   enddo

   do j = 2,nj
    do i = 1,ni
     if (kmtjj(i,j,nt) .eq. 1.0) xwet(i,j-1,nt) = 1.0
    enddo
   enddo

!---------------------------------------------------------------------
! report remaining pinch points or isolated ocean
!---------------------------------------------------------------------

   do j = 2,nj-1
    do i = 1,ni
    if ((xwet(i,j,nt) .eq. 1.0) .and. &
         (abs(latCt(i,j)) .ge. 60.0)) then
                  im1 = i-1
     if(i .eq.  1)im1 = ni
                  ip1 = i+1
     if(i .eq. ni)ip1 = 1

     if(xwet(im1,j,nt) .eq. 0.0 .and. xwet(ip1,j,nt) .eq. 0.0)print *,'iteration ',nt,' im1,ip1 pinch at ',i,j
     if(xwet(i,j-1,nt) .eq. 0.0 .and. xwet(i,j+1,nt) .eq. 0.0)print *,'iteration ',nt,' jm1,jp1 pinch at ',i,j
    end if
   end do
  end do

 end subroutine fixchannels

 subroutine singlepoints(nt)

  integer, intent(in) :: nt
  integer :: cnt
 
  real, dimension(ni,nj) :: tmp

!---------------------------------------------------------------------
!
!---------------------------------------------------------------------

   tmp = 0.0
  xwet(:,:,nt) = xwet(:,:,nt-1)
  do j = 2,nj-1
   do i = 1,ni
    if ((xwet(i,j,nt) .eq. 1.0) .and. &
         (abs(latCt(i,j)) .ge. 60.0)) then
                  im1 = i-1
     if(i .eq.  1)im1 = ni
                  ip1 = i+1
     if(i .eq. ni)ip1 = 1

     tmp(i,j) = xwet(im1,j,nt-1) + xwet(ip1,j,nt-1) + xwet(i,j-1,nt-1) + xwet(i,j+1,nt-1)
    end if
   end do
  end do

    cnt = 0
    do j = 2,nj-1
     do i = 1,ni
      if ((xwet(i,j,nt-1) .eq. 1.0) .and. &
           (abs(latCt(i,j)) .ge. 60.0)) then
        if(tmp(i,j) .le. 1.0)then
         cnt = cnt + 1
         xwet(i,j,nt) = 0.0
        endif
      endif
     enddo
    enddo
    print *,' number of single points ',cnt

 end subroutine singlepoints
end module pinchpoints
