subroutine zz(f, ni, nj, k)
integer, intent(IN) :: ni, nj, k
integer, dimension(merge(ni,nj,k>0),5) :: f
print *,'first dimension =',size(f,1),'f(1,2) =', f(1,2)
return
end
program tmerge
call zz([1,2,3,4,5,6,7,8,9,10],2,3,0)
call zz([1,2,3,4,5,6,7,8,9,10],2,3,1)
end
