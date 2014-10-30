***s/p rmnlib_version
*
	subroutine rmnlib_version(version,prnt)
*
*Auteur M. Lepine - RPN - Octobre 1998
*
*Objet
*  Retourner un identificateur de version de la programmatheque RMNLIB
*  qui servira de signature pour les differents programmes qui utilisent 
*  la programmatheque.
*
	character *(*) version
	logical prnt
*
        version = 
     %  "  RMNLIB  -  Release:"//
     %  " 015 (Rev 967)"//
     %  " Linux_x86-64/pgi1401 Tue 30 Sep 2014 03:01:23 PM"
	if (prnt) print *,version
	return
	end
