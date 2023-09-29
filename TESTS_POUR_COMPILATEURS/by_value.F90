module nv_by_value
    implicit none
    interface
        module subroutine print_val(val)
            implicit none
            integer, intent(in), value :: val  ! <-- Compilation error
            ! integer, intent(in) :: val       ! <-- This would be OK
        end subroutine
    end interface
end module

submodule(nv_by_value) nv_submodule
    implicit none
    contains
        module procedure print_val
            implicit none
            print *, 'val = ', val
        end procedure
end submodule

program test
    use nv_by_value
    implicit none

    call print_val(4)
    
end program test
