module mod_diff
    use iso_fortran_env, only : int32, real32
    implicit none
    
    private
    public :: diff_upwind, diff_centered
contains


    pure function diff_upwind(x) result(dx)
        real, intent(in) :: x(:)  ! Tells compiler this is array of some length
        real :: dx(size(x))
        integer :: im

        im = size(x)
        dx(1) = x(1) - x(im)
        ! Array oriented syntax here
        dx(2:im) = x(2:im) - x(1:im-1) 
    end function diff_upwind

    pure function diff_centered(x) result(dx)
        real, intent(in) :: x(:)  ! Tells compiler this is array of some length
        real :: dx(size(x))
        integer :: im

        im = size(x)
        ! Centered diff
        dx(1) = x(2) - x(im)
        dx(im) = x(1) - x(im-1)
        dx(2:im-1) = x(3:im) - x(1:im-2)

        ! Centered diff requires scaling
        dx = 0.5*dx
      
    end function diff_centered

    
end module mod_diff