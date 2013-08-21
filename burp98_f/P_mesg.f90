Program P_mesg
integer iout,warnin,nonoftb

iout = 0
WARNIN = 3
nonoftb = 42
        WRITE(IOUT, 700)
         WRITE(IOUT, 703)
         MRFOPN = QDFERR('MRFOPN','FICHIER CREE AVEC TABLEBURP NON-OFFICIELLE',WARNIN,NONOFTB)
         WRITE(IOUT, 703)
         WRITE(IOUT, 702)
 700  FORMAT('0***********************ATTENTION***********************')
 702  FORMAT(' *******************************************************')
 703  FORMAT(' *')
stop
end
