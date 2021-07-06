module adjust_landmask

   use param
   use grdvars

   implicit none

   contains

   subroutine singlepts(ain,aout)

    real(kind=4), dimension(ni,nj), intent( in) :: ain
    real(kind=4), dimension(ni,nj), intent(out) :: aout

    ! local variables
    integer :: i,j,im1,ip1,cnt
  
    aout = ain
  
  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------
  
    cnt = 0
    do j = 2,nj-1
     do i = 1,ni
      if ((ain(i,j) .eq. 1.0) .and. &
           (abs(latCt(i,j)) .ge. 60.0)) then
        if(kmtsum(i,j) .le. 1.0)then
         cnt = cnt + 1
         aout(i,j) = 0.0
        endif
      endif
     enddo
    enddo
    print *,' number of single points ',cnt

 end subroutine singlepts
end module adjust_landmask
