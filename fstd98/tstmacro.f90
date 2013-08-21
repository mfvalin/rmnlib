program tst
character *8 etik8
character*12 etik12
character *16 etik16
character *2 typvar
character *4 nomvar

nomvar = 'TT'
typvar = 'P'
etik8 = '12345678'
etik12 = '123456789ABC'
etik16 = 'pas complet'

call tstcp(etik8,typvar,nomvar)
call tstcp(etik12,'VV','C')
call tstcp(etik16,'ABCD','YY')
stop
end
