module sfclayer_mod
  use sfclayer_funcs
  implicit none
  procedure(stability_function), pointer, save :: &
       sf_stable => sf_stable_delage97, &
       sf_unstable => sf_unstable_delage92
end module sfclayer_mod

