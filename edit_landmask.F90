module edit_landmask

 use param
 use grdvars

 contains

 subroutine singlepoints(nt)

  integer, intent(in) :: nt
  integer :: cnt
 
!---------------------------------------------------------------------
!
!---------------------------------------------------------------------

    do j = 2,nj-1
     do i = 1,ni
      if ((xwet(i,j,nt) .eq. 1.0) .and. &
           (abs(latCt(i,j)) .ge. 60.0)) then
                    im1 = i-1
       if(i .eq.  1)im1 = ni
                    ip1 = i+1
       if(i .eq. ni)ip1 = 1

       kmtsum(i,j,nt) = xwet(im1,j,nt) + xwet(ip1,j,nt) + xwet(i,j-1,nt) + xwet(i,j+1,nt)
      end if
     end do
    end do

    cnt = 0
    do j = 2,nj-1
     do i = 1,ni
      if ((xwet(i,j,nt) .eq. 1.0) .and. &
           (abs(latCt(i,j)) .ge. 60.0)) then
        if(kmtsum(i,j,nt) .le. 1.0)then
         cnt = cnt + 1
         xwet(i,j,nt) = 0.0
        endif
      endif
     enddo
    enddo
    print *,'iteration ',nt,' number of single points eliminated',cnt
 end subroutine singlepoints

 subroutine fixchannels(nt)

  integer, intent(in) :: nt

!---------------------------------------------------------------------
! find the initial pinch points
!---------------------------------------------------------------------

   do j = 2,nj-1
    do i = 1,ni
    if ((xwet(i,j,nt) .eq. 1.0) .and. &
         (abs(latCt(i,j)) .ge. 60.0)) then
                  im1 = i-1
     if(i .eq.  1)im1 = ni
                  ip1 = i+1
     if(i .eq. ni)ip1 = 1

     if(xwet(i,j+1,nt) .eq. 1.0 .and. xwet(i,j-1,nt) .eq. 1.0)then   !n/s channel
      if(xwet(im1,j,nt) .eq. 0.0 .and. xwet(ip1,j,nt) .eq. 0.0)then
        kmtii(i,j,nt) = 1.0
        print *,'iteration ',nt,' im1,ip1 pinch at ',i,j
      end if
     end if

     if(xwet(im1,j,nt) .eq. 1.0 .and. xwet(ip1,j,nt) .eq. 1.0)then   !e/w channel
      if(xwet(i,j-1,nt) .eq. 0.0 .and. xwet(i,j+1,nt) .eq. 0.0)then
       kmtjj(i,j,nt) = 1.0
       print *,'iteration ',nt,' jm1,jp1 pinch at ',i,j
      end if
     end if

    end if
   end do
  end do

!---------------------------------------------------------------------
! for kmtii pinches, preferentially remove land at i-1
! for kmtjj pinches, preferentially remove land at j-1
!---------------------------------------------------------------------

   do j = 2,nj-1
    do i = 2,ni-1
     if (kmtii(i,j,nt) .eq. 1.0) xwet(i-1,j,nt) = 1.0
     !if (kmtii(i,j,nt) .eq. 1.0) xwet(i+1,j,nt) = 1.0
    enddo
   enddo

   do j = 2,nj-1
    do i = 2,ni-1
     if (kmtjj(i,j,nt) .eq. 1.0) xwet(i,j-1,nt) = 1.0
     !if (kmtjj(i,j,nt) .eq. 1.0) xwet(i,j+1,nt) = 1.0
    enddo
   enddo
#ifdef test
!---------------------------------------------------------------------
!
!---------------------------------------------------------------------

   do j = 2,nj-1
    do i = 1,ni
    if ((xwet(i,j,nt) .eq. 1.0) .and. &
         (abs(latCt(i,j)) .ge. 60.0)) then
                  im1 = i-1
     if(i .eq.  1)im1 = ni
                  ip1 = i+1
     if(i .eq. ni)ip1 = 1

     if(xwet(i,j,nt) .eq. 1.0)then
       if(xwet(im1,j+1,nt) .eq. 0.0 .and. xwet(ip1,j-1,nt) .eq. 0.0)kmtij(i,j,nt) = 1.0
       if(xwet(im1,j-1,nt) .eq. 0.0 .and. xwet(ip1,j+1,nt) .eq. 0.0)kmtij(i,j,nt) = 1.0
     endif
     if(kmtij(i,j,nt) .eq. 1.0)print *,'iteration ',nt,' kiss pinch at ',i,j
    end if
   end do
  end do
#endif
  print *

 end subroutine fixchannels

 subroutine check_result


 end subroutine check_result
end module edit_landmask
