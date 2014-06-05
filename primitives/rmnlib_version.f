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
     %  " beta_015 (Rev 959)"//
     %  " Linux_x86-64/intel13sp1 Mon 02 Jun 2014 11:26:21 AM"
	if (prnt) print *,version
	return
	end
