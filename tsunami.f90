program tsunami
    use iso_fortran_env, only: int32, real32
    use mod_diff, only: diff => diff_centered
    use mod_initial, only: set_gaussian
    
    implicit none

    integer(int32) :: n
    integer(int32), parameter :: grid_size = 100
    integer(int32), parameter :: num_time_steps = 5000

    real(real32), parameter :: dt = 0.02 ! time step [s]
    real(real32), parameter :: dx = 1 ! grid spacing [m]
    real(real32), parameter :: g = 9.8 ! gravity [m/s]
    real(real32), parameter :: hmean = 10 ! mean depth of water [m]


    real(real32) :: h(grid_size)
    real(real32) :: u(grid_size)

    integer(int32), parameter :: icenter = 25
    real(real32), parameter :: decay = 0.02

    if (grid_size <= 0) stop "Grid Size <= 0"
    if (dt <= 0) stop "dt <= 0"
    if (dx <= 0) stop "dx <= 0"

    ! Set initial water height
    call set_gaussian(h, icenter, decay)

    print *, 0, h
    time_loop: do n = 1, num_time_steps        
        u = u - (u * diff(u) + g * diff(h)) / dx * dt
        h = h - diff(u * (hmean + h)) / dx * dt
       
        print *, n, h
    end do time_loop

end program tsunami