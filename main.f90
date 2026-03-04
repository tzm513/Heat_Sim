program heat
    implicit none
    integer, parameter              :: dp = selected_real_kind(15, 300)

        ! Inputs and Error Checking
    character(len=1)                :: YN
    integer                         :: ierr
    character(len=50)               :: errmsg
    
        ! State array
    real(kind = dp), allocatable    :: u(:)
    integer                         :: size
    real(kind = dp)                 :: width, dx

        ! #######################
        ! # Initialising Arrays #
        ! #######################

        ! System state array (real(kind=dp))
    do
        write(*,*) "Would you like to adjust the size of the state array? Default is 100 positions over 0.5m"
        read(*,'(A)', iostat = ierr, iomsg = errmsg) YN
        if ((ierr .eq. 0) .and. (index('yY', YN) .ne. 0)) then

                ! Index count input
            do
                write(*,*) "How many indices should the state array contain?"
                read(*,'(I10)', iostat = ierr, iomsg = errmsg) size
                if (ierr .eq. 0) then
                    allocate(u(size))
                    u = 0
                    exit
                end if
                write(*,*) "Please enter an integer"
            end do

                ! Size input
            do
                write(*,*) "How long should the state array be (in metres)?"
                read(*,'(F10.10)', iostat = ierr, iomsg = errmsg) width
                if (ierr .eq. 0) then
                    dx = width/real(size, kind = dp)
                    exit
                end if
                write(*,*) "Please enter a float"
            end do
            exit
        else if ((ierr .eq. 0) .and. (index('nN', YN) .ne. 0)) then

                ! Default values
            size = 100
            allocate(u(size))
            u = 0
            width = 0.5_dp
            dx = width/real(size, kind = dp)
            exit
        end if

        write(*,*) "Please return Y/N"
    end do
end program