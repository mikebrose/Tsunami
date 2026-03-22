module mod_parallel

implicit none

contains
    function tile_neighbors(current_image, n_images) result(res)
        integer, intent(in) :: current_image, n_images
        integer :: res(2)
        if (current_image == 1) then
            res = [n_images, 2]
        else if (current_image == n_images) then
            res = [n_images-1,1]
        else 
            res = [current_image - 1, current_image + 1]
        end if
        
    end function tile_neighbors


    pure function tile_indices(current_image,n_images,dims)

    integer, intent(in) :: current_image, n_images, dims
    integer :: tile_indices(2)
    integer :: offset, tile_size

    ! First guess at tile size, only correct if modulo 0
    tile_size = dims / n_images
    ! First guess for start index
    tile_indices(1) = (current_image - 1) * tile_size + 1
    ! First guess for end index
    tile_indices(2) = tile_indices(1) + tile_size - 1

    ! if dims not mod 0, distribute remainder of tasks
    ! to the trailing images
    offset = n_images - mod(dims, n_images)
    if (current_image > offset) then 
        ! Expand to right
        tile_indices(1) = tile_indices(1) &
                        + current_image - offset - 1
        tile_indices(2) = tile_indices(2) &
                        + current_image - offset
    end if     
end function



end module mod_parallel