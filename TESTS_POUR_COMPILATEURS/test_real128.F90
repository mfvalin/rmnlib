subroutine xreal128 (x, n)
    use,intrinsic :: iso_fortran_env, only: real128
    real(real128),dimension(*) :: x
    integer, intent(out) :: n

    n = x(1)/8
end subroutine xreal128
