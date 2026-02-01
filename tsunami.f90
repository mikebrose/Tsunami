program tsunami

    implicit none

    integer :: i
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

contains

    function diff(x) result (dx)
        real, intent(in) :: x(:)  ! Tells compiler this is array of some length
        real :: dx(size(x))
        integer :: im

        im = size(x)
        dx(1) = x(1) - x(im)
        ! Array oriented syntax here
        dx(2:im) = x(2:im) - x(1:im-1) 
    end function diff

    ! There is likely bug in this that doesnt not handle periodic boundary conditions
    subroutine set_gaussian(x, icenter, decay)
        real, intent(inout) :: x(:)
        integer, intent(in) :: icenter
        real, intent(in) :: decay
        integer :: i
        
        do concurrent (i = 1 : size(x))
            h(x) = exp(-decay * (i - icenter)**2)
        end do
    end subroutine set_gaussian



end program tsunami