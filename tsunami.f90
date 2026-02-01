program tsunami

    implicit none

    integer :: i
    integer :: n
    integer, parameter :: grid_size = 100
    integer, parameter :: num_time_steps = 100

    real :: dt ! time step [s]
    real :: dx ! grid spacing [m]
    real :: c ! phase speed [m/s]
    real :: h(grid_size)
    real :: dh(grid_size)

    integer, parameter :: icenter = 25
    real, parameter :: decay = 0.02

    dt = 1
    dx = 1
    c = 1

    do i = 1, grid_size
        h(i) = exp(-decay * (i - icenter)**2)
    end do 

    time_loop: do n = 1, num_time_steps
        ! Handle left bound
        dh(1) = h(1) - h(grid_size)
        ! Continuous region and right bound
        do i = 2, grid_size
            dh(i) = h(i) - h(i-1)
        end do


    end do time_loop

    if (grid_size <= 0) stop "Grid Size <= 0"
    if (dt <= 0) stop "dt <= 0"
    if (dx <= 0) stop "dx <= 0"
    if (c <= 0) stop "c < 0"

end program tsunami