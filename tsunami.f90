program tsunami
    use mod_diff, only: diff
    use mod_initial, only: set_gaussian
    implicit none

    integer :: n
    integer, parameter :: grid_size = 100
    integer, parameter :: num_time_steps = 100

    real, parameter :: dt = 1 ! time step [s]
    real, parameter :: dx = 1 ! grid spacing [m]
    real, parameter :: c = 1 ! phase speed [m/s]

    real :: h(grid_size)
    real :: dh(grid_size)

    integer, parameter :: icenter = 25
    real, parameter :: decay = 0.02

    if (grid_size <= 0) stop "Grid Size <= 0"
    if (dt <= 0) stop "dt <= 0"
    if (dx <= 0) stop "dx <= 0"
    if (c <= 0) stop "c < 0"

    ! Set initial water height
    call set_gaussian(h, icenter, decay)

    print *, 0, h
    time_loop: do n = 1, num_time_steps        
        dh = diff(h)
        h = h - c * diff(h) / dx * dt
        print *, n, h
    end do time_loop

end program tsunami