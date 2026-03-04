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

        ! Time stores
    real(kind = dp)                 :: dt, t, target_t

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
                write(*,*) trim(errmsg)
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
                write(*,*) trim(errmsg)
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
        write(*,*) trim(errmsg)
        write(*,*) "Please return Y/N"
    end do

        ! Start at time 0
    t = 0
    do
        write(*,*) "Would you like to adjust the timestep and target time? Default is a step of 0.01 seconds,&
            ! Line truncated
        & with a final time of 10s"
        read(*,'(A)', iostat = ierr, iomsg = errmsg) YN
        if ((ierr .eq. 0) .and. (index('yY', YN) .ne. 0)) then

                ! Timestep input
            do
                write(*,*) "How long should each timestep be?"
                read(*,'(F10.10)', iostat = ierr, iomsg = errmsg) dt
                if (ierr .eq. 0) exit
                write(*,*) trim(errmsg)
                write(*,*) "Please enter a float"
            end do

                ! Target time input
            do
                write(*,*) "How long should the code simulate?"
                read(*,'(F10.10)', iostat = ierr, iomsg = errmsg) target_t
                if (ierr .eq. 0) exit
                write(*,*) trim(errmsg)
                write(*,*) "Please enter a float"
            end do
            exit
        else if ((ierr .eq. 0) .and. (index('nN', YN) .ne. 0)) then

                ! Default values
            dt = 0.01_dp
            target_t = 10.0_dp
        end if
        write(*,*) "Please return Y/N"
    end do


end program