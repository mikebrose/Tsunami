module mod_initial
    implicit none

contains

    ! There is likely bug in this that doesnt not handle periodic boundary conditions
    subroutine set_gaussian(x, icenter, decay)
       real, intent(inout) :: x(:)
       integer, intent(in) :: icenter
       real, intent(in) :: decay
       integer :: i
       
       do concurrent (i = 1 : size(x))
           x(i) = exp(-decay * (i - icenter)**2)
       end do
    end subroutine set_gaussian

end module mod_initial