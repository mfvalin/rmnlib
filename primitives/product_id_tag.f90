character *128 function product_id_tag(product,datetag,rev)
character *(*) product, datetag, rev
integer s1,s2,e1,e2

s1 = index(datetag,'$LastChangedDate:')
e1 = s1+16
s2 = index(rev,'$LastChangedRevision:')
e2 = s2+22
if ((ind1 .ne. 0) .and. (ind2 .ne. 0)) then
  product_id_tag = product//' '//datetag(s1:e1)//' '//rev(s2:e2)
else
  product_id_tag = product
endif
return
end