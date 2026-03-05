program heat
    use random

    implicit none
    integer, parameter              :: dp = selected_real_kind(15, 300)

        ! Inputs and Error Checking
    character(len=1)                :: u_input
    integer                         :: ierr
    character(len=50)               :: errmsg
    
        ! State array
    real(kind = dp), allocatable    :: u(:)
    integer                         :: len
    real(kind = dp)                 :: width, dx

        ! Time stores
    real(kind = dp)                 :: dt, t, target_t

        ! Step matrix
    real(kind = dp), allocatable    :: mat(:,:)
    real(kind = dp)                 :: alpha, lambda

        ! Loop counter
    integer                         :: count

        ! ################
        ! Setup Random Gen
        ! ################

    call init_random(0)


        ! #######################
        ! # Initialising Arrays #
        ! #######################

        ! System state array (real(kind=dp))
    do
        write(*,*) "Would you like to adjust the size of the state array? Default is 10 positions over 0.5m"
        read(*,'(A)', iostat = ierr, iomsg = errmsg) u_input
        if ((ierr .eq. 0) .and. (index('yY', u_input) .ne. 0)) then

                ! Index count input
            do
                write(*,*) "How many indices should the state array contain?"
                read(*,'(I10)', iostat = ierr, iomsg = errmsg) len
                if (ierr .eq. 0) then
                    allocate(u(len))
                    allocate(mat(len, len))
                    count = 0
                    do
                        count = count + 1
                        u(count) = gen_uniform_random()
                        if (count .eq. len) exit
                    end do
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
                    dx = width/real(len, kind = dp)
                    exit
                end if
                write(*,*) trim(errmsg)
                write(*,*) "Please enter a float"
            end do
            exit
        else if ((ierr .eq. 0) .and. (index('nN', u_input) .ne. 0)) then

                ! Default values
            len = 10
            allocate(u(len))
            allocate(mat(len, len))
            count = 0
            do
                count = count + 1
                u(count) = gen_uniform_random()
                if (count .eq. len) exit
            end do
            width = 0.5_dp
            dx = width/real(len, kind = dp)
            exit
        end if
        write(*,*) trim(errmsg)
        write(*,*) "Please enter Y/N"
    end do

    do
        write(*,*) "Would you like to adjust the timestep and target time? Default is a step of 0.01 seconds,&
            ! Line truncated
        & with a final time of 20s"
        read(*,'(A)', iostat = ierr, iomsg = errmsg) u_input
        if ((ierr .eq. 0) .and. (index('yY', u_input) .ne. 0)) then

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
        else if ((ierr .eq. 0) .and. (index('Nn', u_input) .ne. 0)) then

                ! Default values
            dt = 0.01_dp
            target_t = 20.0_dp
            exit
        end if
        write(*,*) "Please return Y/N"
    end do

    !do
    !    write(*,*) "Would you like to adjust the thermal permittivity of the system? Default is 0.2"
    !    read(*,'(A)', iostat = ierr, iomsg = errmsg) u_input
    !    if ((ierr .eq. 0) .and. (index('Yy', u_input) .ne. 0)) then
    !        do
    !            write(*,*) "What would you like the permittivity to be?"
    !            read(*,'(F10.10)', iostat = ierr, iomsg = errmsg) alpha
    !            if ((ierr .eq. 0) .and. (alpha .le. 1.0_dp) .and. (alpha .ge. 0)) exit
    !            write(*,*) trim(errmsg)
    !            write(*,*) "Please enter a float between 0 and 1"
    !        end do
    !        exit
    !    else if ((ierr .eq. 0) .and. (index('Nn', u_input) .ne. 0)) then
    !        alpha = 0.2_dp
    !        exit
    !    end if
    !    write(*,*) trim(errmsg)
    !    write(*,*) "Please return Y/N"
    !end do
    !lambda = alpha * dt / (dx**2)
    lambda = 0.4

        ! Set up timestep matrix
    do
        write(*,*) "Would you like FTCS or BTCS?"
        read(*,'(A)', iostat = ierr, iomsg = errmsg) u_input
        if ((ierr .eq. 0) .and. (index('FfBb', u_input) .ne. 0)) exit
        write(*,*) "Please enter FTCS or BTCS"
    end do
        ! Forwards time-step
    if (index('Ff', u_input) .ne. 0) then

            ! First column
        mat(1, 1) = 1.0_dp - (2.0_dp * lambda)
        mat(2, 1) = lambda
        mat(len, 1) = lambda

            ! Central diagonal
        count = 2
        do
            mat(count-1, count) = lambda
            mat(count, count) = 1.0_dp - (2.0_dp * lambda)
            mat(count+1, count) = lambda

            count = count + 1
            if (count .ge. len) exit
        end do

            ! Last column
        mat(1, count) = lambda
        mat(count-1, count) = lambda
        mat(count, count) = 1.0_dp - (2.0_dp * lambda)

        ! Backwards time-step
    else if (index('Bb', u_input) .ne. 0) then

                    ! First column
        mat(1, 1) = 1.0_dp + (2.0_dp * lambda)
        mat(2, 1) = -1.0_dp * lambda
        mat(len, 1) = -1.0_dp * lambda

            ! Central diagonal
        count = 2
        do
            mat(count-1, count) = -1.0_dp * lambda
            mat(count, count) = 1.0_dp + (2.0_dp * lambda)
            mat(count+1, count) = -1.0_dp + lambda

            count = count + 1
            if (count .ge. len) exit
        end do

            ! Last column
        mat(1, count) = -1.0_dp * lambda
        mat(count-1, count) = -1.0_dp * lambda
        mat(count, count) = 1.0_dp + (2.0_dp * lambda)

        call invert_matrix(mat)
    end if

        ! System evolution loop
    t = 0
    do
        t = t + dt
        u = matmul(mat, u)

        if (abs(t - int(t)) .lt. dt) then
            write(*,*) u
        end if

        if (t .ge. target_t) exit
    end do

    contains
        subroutine invert_matrix(matrix)! Invert the supplied matrix using LAPACK
            implicit none

            real(kind=dp),allocatable, dimension(:,:), intent(inout) :: matrix(:,:)
            integer :: N, LWORK, IERR
            integer, dimension(:), allocatable :: IPIV
            real(kind=dp), dimension(:), allocatable :: WORK
            
            if (size(matrix, 1) .ne. size(matrix, 2)) STOP "Matrix is not square"
            N = size(matrix, 1)
            allocate(IPIV(N),stat=IERR)
            if (IERR/=0) STOP "Failed to allocate IPIV"
            LWORK = N**2
            allocate(WORK(LWORK),stat=IERR)
            if (IERR/=0) STOP "Failed to allocate WORK"

            call dgetrf(N,N,matrix,N,IPIV,IERR)
            if (IERR/=0) STOP "Error in dgetrf: Matrix is singular"

            call dgetri(N,matrix,N,IPIV,WORK,LWORK,IERR)
            if (IERR/=0) STOP "Error in dgetri: Matrix is singular"
        end subroutine
end program