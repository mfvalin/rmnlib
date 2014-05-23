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
     %  " rmn_014_rc2b (Rev 821b)"//
     %  " Linux_x86-64/intel13sp1 Wed 20 Nov 2013 02:31:58 PM EST"
	if (prnt) print *,version
	return
	end
