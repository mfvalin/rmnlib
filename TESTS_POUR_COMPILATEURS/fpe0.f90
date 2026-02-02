  subroutine test()
    real :: a, b, c
    integer :: i
    a = 1e-10
    b = 1e-11
    i = 0
    do while(.true.)
      i = i + 1
      c = a * b
      if(c .eq. 0.0) exit
      b = b / 1.00001
    enddo

    write (*, '(i8,x,3(g15.8,x))') i, a, b, c
  end subroutine test

  program fpe0

  call test()


  end program fpe0
