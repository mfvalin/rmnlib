      include 'convip_plus.inc'

      interface

      subroutine convip_plus( ip, p, kind, mode, string, flagv )
        integer, intent(INOUT) :: ip, kind
        integer, intent(IN) :: mode
        real, intent(INOUT) :: p
        character *(*), intent(OUT) :: string 
        logical, intent(IN) :: flagv
      end subroutine convip_plus

      subroutine c_conv_ip( ip, p, kind, mode ) BIND(C,name='ConvertIp') ! reduced inteerface with no string option
        use ISO_C_BINDING
        implicit none
          integer(C_INT), intent(INOUT) :: ip, kind
          integer(C_INT), intent(IN), value :: mode
          real(C_FLOAT), intent(INOUT) :: p
      end subroutine c_conv_ip

      end interface
