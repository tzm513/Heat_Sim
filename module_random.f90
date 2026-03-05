module random
    implicit none
        ! Variables created here can be seen throughout the module but not outside of it
    integer, parameter :: dp = selected_real_kind(15, 300)
    integer :: A = 100, B = 104001, M = 714025, current_rand
    logical :: rand_init = .false.
    real(kind = dp) :: sigma = 1, y_bar = 0, pi = 3.14159
    private :: dp, A, B, M, current_rand, rand_init

    contains

        ! Sets a seed of the time in ms to 6 digits, unless an input is provided - then seed is set to input
    subroutine init_random(in)
        implicit none
        integer, optional :: in
        integer :: time(8)

            !Assign values depending on provided input
        if(present(in)) then
            current_rand = in
        else
            call date_and_time(values = time)
            current_rand = time(8) + (1000 * time(7))
        end if

            ! Store that the seed has been generated and does not need automatically calling
        rand_init = .true.
    end subroutine

    
        ! Initialises seed if uninitialised, then iterates the random sequence
    function gen_uniform_random() result(out)
        implicit none
        real (kind = dp) :: out

            ! Check if a seed has been generated, and generate one if not
        if (.not. rand_init) call init_random()

            ! Update current_rand with the next value in the sequence
        current_rand = mod((A * current_rand) + B, M)

            ! Scales the output to be (0 .le. out .lt. 1)
        out = current_rand / real(M, kind = dp)
        return
    end function

    function gen_box_muller() result(out)
        implicit none
        real(kind = dp) :: out(2), temp(2)

            ! Store 2 uniformly distributed random numbers 
        temp(1) = gen_uniform_random()
        temp(2) = gen_uniform_random()

            ! Perform the box-muller method
        out(1) = sigma * (-2.0 * log(temp(1)))**(0.5) * cos(2.0 * pi * temp(2)) + y_bar
        out(2) = sigma * (-2.0 * log(temp(1)))**(0.5) * sin(2.0 * pi * temp(2)) + y_bar

        return
    end function
end module