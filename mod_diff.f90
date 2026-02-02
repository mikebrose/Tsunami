module mod_diff
    use iso_fortran_env, only : int32, real32
    implicit none
    
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

    
end module mod_diff