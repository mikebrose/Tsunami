module mod_field
    use iso_fortran_env, only: real32, int32
    use mod_parallel, only: tile_indices
    implicit none

    type :: Field 
        character(:), allocatable :: name
        integer(int32) :: dims(2), lb(2), ub(2)
        real(real32), allocatable :: data(:,:)
    end type Field

    interface Field
        module procedure :: field_constructor
    end interface Field

    contains
        type(Field) function field_constructor(name, dims) result(res)
            character(*), intent(in) :: name
            integer(int32) :: dims(2)
            integer(int32) :: indices(4)

            res%name = name
            res%dims = dims
            indices = tile_indices(dims)
            res%lb = indices([1,3])
            res%ub = indices([2,4])
            ! Extra row and column for halo exchange
            allocate(res%data(res%lb(1)-1 : res%ub(1)+1, &              
                              res%lb(2)-1 : res%ub(2)+1))
            res%data = 0.0





        end function field_constructor



end module mod_field