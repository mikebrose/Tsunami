program tsunami
    use iso_fortran_env, only: int32, real32
    use mod_diff, only: diff => diff_centered
    use mod_initial, only: set_gaussian
    use mod_parallel, only: tile_neighbors, tile_indices
    
    implicit none

    integer(int32) :: n, i
    integer(int32), parameter :: grid_size = 100
    integer(int32), parameter :: num_time_steps = 5000
    
    real(real32), parameter :: dt = 0.02 ! time step [s]
    real(real32), parameter :: dx = 1 ! grid spacing [m]
    real(real32), parameter :: g = 9.8 ! gravity [m/s]
    
    
    real(real32), parameter :: decay = 0.02
    integer(int32), parameter :: icenter = 25
    
    integer(int32) :: neighbors(2), left, right 
    integer(int32) :: indices(2) ! Temp to store tile indices
    
    real(real32), allocatable :: h(:)[:], u(:)[:]
    real(real32), allocatable :: gather(:)[:]
    real(real32), allocatable :: hmean(:) ! mean depth of water [m]
    
    integer(int32) :: is, ie 
    integer(int32) :: ils, ile ! Local Indices exluding halo [1,tile_size], do work
    integer(int32) :: ims, ime ! Local Indices including halo, mainly used to update and read from

    integer(int32) :: current_image, n_images
    integer(int32) :: tile_size

    character(*), parameter :: fmt = '(i0,*(1x,es15.8e2))'

    current_image = this_image()
    n_images = num_images()

    if (mod(grid_size, n_images) > 0) then
        error stop 'Error: grid_size must be divisible by number of images'
    end if
    if (n_images == 1) then
        error stop 'Error: must be > 1 image'
    end if

    neighbors = tile_neighbors(current_image, n_images)
    left = neighbors(1)
    right = neighbors(2)

    ! These are in global context, 1-100
    indices = tile_indices(current_image, n_images, grid_size)
    is = indices(1)
    ie = indices(2)

    tile_size = grid_size / n_images
    ! Setting Local Indices from local perspective
    ils = 1
    ile = tile_size
    ! these extend by one on each side for halo cells
    ims = ils - 1 ! can be zero, because will allocate with zero idx
    ime = ile + 1

    allocate(h(ims:ime)[*]) ! Allocate for all images
    allocate(u(ims:ime)[*]) ! Allocate for all images
    allocate(hmean(ims:ime))

    allocate(gather(grid_size)[*])


    if (grid_size <= 0) stop "Grid Size <= 0"
    if (dt <= 0) stop "dt <= 0"
    if (dx <= 0) stop "dx <= 0"


    ! Set initial water height
    do i = is - 1, ie + 1
        h(i-is+1) = exp(-decay * (i - icenter)**2)
    end do

    u = 0
    hmean = 10
    
    gather(is:ie)[1] = h(ils:ile)
    sync all
    if (current_image == 1) print fmt, 0, gather

    time_loop: do n = 1, num_time_steps

        
    ! Update neighbors halo cells
        h(ime)[left] = h(ils) ! update left neighbors right halo
        h(ims)[right] = h(ile) ! update right neighbor halo
        sync all

        u = u - (u * diff(u) + g * diff(h)) / dx * dt
        sync all

        u(ime)[left] = u(ils)
        u(ims)[right] = u(ile)
        sync all

        h = h - diff(u * (hmean + h)) / dx * dt
        
        gather(is:ie)[1] = h(ils:ile)
        sync all
        if (current_image == 1) print fmt, n,  gather
       
    end do time_loop

end program tsunami