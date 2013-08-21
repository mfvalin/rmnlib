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
     %  " 014_rc1 (Rev 819)"//
     %  " Linux_x86-64/pgi1301 Fri 26 Jul 2013 11:33:29 AM EDT"
	if (prnt) print *,version
	return
	end
