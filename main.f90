program heat
    use random

    implicit none
    integer, parameter              :: dp = selected_real_kind(15, 300)

        ! Inputs and Error Checking
    character(len=1)                :: u_input
    integer                         :: ierr
    character(len=50)               :: errmsg = ''
    
        ! State array
    real(kind = dp), allocatable    :: u(:)
    real(kind = dp), allocatable    :: sinks(:)
    integer                         :: len
    real(kind = dp)                 :: width, dx

        ! Time stores
    real(kind = dp)                 :: dt, t, target_t

        ! Step matrix
    real(kind = dp), allocatable    :: mat(:,:)
    real(kind = dp)                 :: lambda, alpha

        ! Loop counter
    integer                         :: count

        ! File store
    integer                         :: unit

        ! ################
        ! Setup Random Gen
        ! ################

    call init_random(0)

        ! ###############
        ! # User Inputs #
        ! ###############

    write(*,*) "Ensure for whole number inputs which would not always be integers, &
        ! Line truncated
    &to include X.0 (e.g. 1 metre should be written as 1.0 metres)"

        ! System state array (real(kind=dp))
    do
        write(*,*) "Would you like to adjust the size of the state array? Default is 100 positions over 1m"
        read(*,'(A)', iostat = ierr, iomsg = errmsg) u_input
        if ((ierr .eq. 0) .and. (index('yY', u_input) .ne. 0)) then

                ! Index count input
            do
                write(*,*) "How many indices should the state array contain?"
                read(*,'(I10)', iostat = ierr, iomsg = errmsg) len
                if (ierr .eq. 0) then
                    allocate(u(len))
                    allocate(sinks(len))
                    allocate(mat(len, len))
                    mat = 0
                    sinks = 0
                    count = 0
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
            len = 100
            allocate(u(len))
            allocate(sinks(len))
            allocate(mat(len, len))
            mat = 0
            sinks = 0
            count = 0
            do
                count = count + 1
                u(count) = gen_uniform_random()
                if (count .eq. len) exit
            end do
            width = 1.0_dp
            dx = width/real(len, kind = dp)
            exit
        end if
        write(*,*) trim(errmsg)
        write(*,*) "Please enter Y/N"
    end do

    do
        write(*,*) "Would you like to adjust the timestep and target time? Default is a step of 0.01 seconds,&
            ! Line truncated
        & with a final time of 120s"
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
            target_t = 120.0_dp
            exit
        end if
        write(*,*) "Please return Y/N"
    end do

    do
        write(*,*) "Would you like to adjust the thermal permittivity of the system? Default is 1E-4"
        read(*,'(A)', iostat = ierr, iomsg = errmsg) u_input
        if ((ierr .eq. 0) .and. (index('Yy', u_input) .ne. 0)) then
            do
                write(*,*) "What would you like the permittivity to be?"
                read(*,'(F10.10)', iostat = ierr, iomsg = errmsg) alpha
                if ((ierr .eq. 0) .and. (alpha .le. 1.0_dp) .and. (alpha .ge. 0)) exit
                write(*,*) trim(errmsg)
                write(*,*) "Please enter a float between 0 and 1"
            end do
            exit
        else if ((ierr .eq. 0) .and. (index('Nn', u_input) .ne. 0)) then
            alpha = 1.0E-4_dp
            exit
        end if
        write(*,*) trim(errmsg)
        write(*,*) "Please return Y/N"
    end do
    lambda = alpha * dt / (dx**2)

        ! Set up timestep matrix
    do
        write(*,*) "Would you like FTCS or BTCS?"
        read(*,'(A)', iostat = ierr, iomsg = errmsg) u_input
        if ((ierr .eq. 0) .and. (index('FfBb', u_input) .ne. 0)) exit
        write(*,*) "Please enter FTCS or BTCS"
    end do

    call init_heats(u, .false., 0.2_dp)
    call init_transformation(mat, lambda, u_input, '')
    call init_heatsinks(sinks, 1.0_dp, 0.0_dp)

        ! ###############
        ! File Management
        ! ###############

    unit = newunit()
    open(unit=unit, file="output.txt", action="write", iostat=ierr, iomsg=errmsg)
    if (ierr .ne. 0) then
        write(*,*) trim(errmsg)
        stop
    end if

    write(unit,*) width, ',', target_t

        ! ################
        ! System Evolution
        ! ################

    write(unit,*) u, sum(u)
    t = 0
    do
        t = t + dt
        u = matmul(mat, u)
        u = u + (lambda * sinks)

        write(unit,*) u, sum(u)

        if (t .ge. target_t) exit
    end do

    close(unit)

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

        function newunit(unit)

            integer, optional   :: unit
            integer             :: newunit
            logical             :: opened

                ! Start from given unit if supplied, else default to 10
            if (present(unit)) then
                newunit = unit
            else
                newunit = 10
            end if

            do
                    ! Iterate through units
                newunit = newunit + 1
                inquire(unit = newunit, opened = opened)

                    ! Leave loop if unit is unopened
                if (.not. opened) exit
                    ! Leave loop if all units are occupied
                if (newunit .ge. 100) exit
            end do

                ! Return -1 if all options 10-99 are opened
            if (newunit .ge. 100) newunit = -1

            return
        end function


        subroutine init_heats(u, random, initial)
            real(kind=dp), allocatable  :: u(:)
            real(kind=dp), optional     :: initial
            logical                     :: random

            u = 0
            if (present(initial)) u = initial

            if (random) then
                do
                    count = count + 1
                    u(count) = gen_uniform_random()
                    if (count .eq. len) exit
                end do
            end if
        end subroutine


        subroutine init_transformation(mat, lambda, timedir, boundary)
            real(kind=dp), allocatable  :: mat(:, :)
            real(kind=dp)               :: lambda
            integer                     :: length
            character(*)                :: timedir, boundary
            real(kind=dp)               :: val_diag, val_tridiag

            mat = 0

            length = size(mat, 1)

                ! Initialise variables based on FTCS or BTCS
            if (index('Ff', timedir) .ne. 0) then
                val_diag = 1.0_dp - (2.0_dp * lambda)
                val_tridiag = lambda
            else if (index('Bb', timedir) .ne. 0) then
                val_diag = 1.0_dp + (2.0_dp * lambda)
                val_tridiag = -1.0_dp * lambda
            else
                write(*,*) 'Invalid input for FTCS/BTCS when initialising transformation matrix'
                error stop
            end if

                ! First column
            mat(1, 1) = val_diag
            mat(2, 1) = val_tridiag
            if (boundary .eq. 'periodic') mat(len, 1) = val_tridiag

                ! Central diagonal
            count = 2
            do
                mat(count-1, count) = val_tridiag
                mat(count, count) = val_diag
                mat(count+1, count) = val_tridiag

                count = count + 1
                if (count .ge. len) exit
            end do

                ! Last column
            mat(count-1, count) = val_tridiag
            mat(count, count) = val_diag
            if (boundary .eq. 'periodic') mat(1, count) = val_tridiag

            if (index('Bb', timedir) .ne. 0) call invert_matrix(mat)
        end subroutine

        subroutine init_heatsinks(sinks, hot, cold)
            real(kind=dp), allocatable  :: sinks(:)
            real(kind=dp)               :: hot, cold
            integer                     :: length

                ! Places a heat source on the left and heat sink on the right
            length = size(sinks, 1)
            sinks(1) = hot
            sinks(length) = cold
        end subroutine
end program