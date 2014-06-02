!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer, 
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms 
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer 
!version 3 or (at your option) any later version that should be found at: 
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html 
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software; 
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec), 
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
!**s/r itf_phy_inincr - define physics surface forcing increments
!
!
      subroutine itf_phy_inincr
!
      use ISO_C_BINDING
      implicit none
!
!author   
!     Bernard Dugas - july 97
!
!revision
! v2_21 - Bernard Dugas     - Adaption to GEM DM v2.2 and to physics v3.67:
!                             1) change all includes and variables to DM specs;
!                             2) each PE does the interpolations for its own area;
!                             3) INCRTS is only used for water points (TWATER);
!                             4) INCRTG is only used for glacier points (TGLACIER(2));
!                             5) INCRTP is only used for deep soil points when the
!                                force-restore surface scheme is selected (TSOIL(2));
!                             6) INCRICD is added for sea ice depht increments (ICEDP);
!                             7) INCRGL is now used with GLSEAS0, as GLSEA can be
!                                modified to account for lake ice evolution;
!                             8) INCRAL is no longer defined, since CALCALB
!                                is used instead (called by CLIMPHS2).
! v2_31 - Desgagne M.       - remove stkmemw
! v2_31 - Bernard Dugas     - adapt to new climatological file descriptors
! v2_32 - Bernard Dugas     - correct exit condition codes
! v3_00 - Desgagne & Lee    - Lam configuration
! v3_01 - Bernard Dugas     - add call to INTOZON
! v3_02 - Lubos Spacek      - remplacer l_ni et l_nj par p_ni et p_nj
! v3_02 - Bernard Dugas     - remettre le code de controle d'interpolation O3
!                           - toujours initialiser incrtp (independamment de soli_L)
! v3_10 - Lee V.            - RPN_bcastc for bcast on MPI_CHARACTER
! v3_11 - Bernard Dugas     - use glacier AND (maybe) sea ice SNODP to define INCRNE
!                           - Account for P_pbl_iceme_L = .true.
! v3_22 - Bernard Dugas     - add check for Done_inincr and correct xncibl(5) = 12
! v3_22 - Katja Winger      - make sure that BC are read twice in first month
!                             (at Lctl_step=2 and in the middle of the month)
! v3_30 - Dugas B.          - new itf_phy interface
! v3_32 - Winger K.         - allow minutely SST and sea ice BC's
!                             IP3:YYYY, IP2:MM, IP1:DDhhmm
!                           - assume monthly values at middle of month instead of 15th 12h
! v3_32 - Winger K.         - Change 'xnflds' -> 'Cfld_nbr' and 'clim_S' -> 'Cfld_list_S'.
!                             Put 'Cfld_nbr', 'Cfld_list_S', and 'Cfld_date' (coded 'Done_Inincr')
!                             in 'cfld.cdk' to write/read them in/from restart files.
! v3_33 - Dugas B.          - IP1 no longer used for DDhhmm info. DATEO used instead
!                           - support analysis data (deet > 0) in imanclima file
! v3_34 - Valin M.          - loop and wait for actual data to show up in climato files (Feb 2014)
!                           
!                             
!
!object
!
!       Initialize the climatological increments field that
!       are in the permanent physics bus. Care has to be taken
!       with the active physics configuration, especially with
!       regards to the active surface parameterization.
!
!       The increments are those required by the routine CLIMPHS2
!       to simulate the evolution of some of the surface forcing
!       terms accounted for in the RPN/CMC physics package.
!
! ***   It is important to note that the physics package is
! ***   supposed to have been initialized by a previous call
! ***   to phy_init.
!
!       Climatology and analysed climatology file:
!       IP2 is always supposed to contain the month MM
!
!       Analysed climatology file:
!       IP3 is always supposed to contain the year YYYY
!       If there is more than 1 date per month day, hour and minute 
!       are to be found is the decoded DATEO descriptor
!       
!
!implicits
!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer, 
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms 
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer 
!version 3 or (at your option) any later version that should be found at: 
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html 
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software; 
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec), 
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
*
      real*8  Dcst_cpd_8  , Dcst_cpv_8  , Dcst_rgasd_8, Dcst_rgasv_8,
     $        Dcst_trpl_8 , Dcst_tcdk_8 , Dcst_rauw_8 , Dcst_eps1_8 ,
     $        Dcst_eps2_8 , Dcst_delta_8, Dcst_cappa_8, Dcst_tgl_8  ,
     $        Dcst_conso_8, Dcst_grav_8 , Dcst_rayt_8 , Dcst_stefn_8,
     $        Dcst_pi_8   , Dcst_omega_8, Dcst_knams_8, Dcst_stlo_8 ,
     $        Dcst_karmn_8, Dcst_ric_8  , Dcst_chlc_8 , Dcst_chlf_8 ,
     $        Dcst_t1s_8  , Dcst_t2s_8  , Dcst_aw_8   , Dcst_bw_8   ,
     $        Dcst_ai_8   , Dcst_bi_8   , Dcst_slp_8
*
      
      integer Dcst_r_first(-1:0)
      common /Dcst_r/Dcst_r_first
      common / Dcst_r /
     $       Dcst_cpd_8  , Dcst_cpv_8  , Dcst_rgasd_8, Dcst_rgasv_8,
     $       Dcst_trpl_8 , Dcst_tcdk_8 , Dcst_rauw_8 , Dcst_eps1_8 ,
     $       Dcst_eps2_8 , Dcst_delta_8, Dcst_cappa_8, Dcst_tgl_8  ,
     $       Dcst_conso_8, Dcst_grav_8 , Dcst_rayt_8 , Dcst_stefn_8,
     $       Dcst_pi_8   , Dcst_omega_8, Dcst_knams_8, Dcst_stlo_8 ,
     $       Dcst_karmn_8, Dcst_ric_8  , Dcst_chlc_8 , Dcst_chlf_8 ,
     $       Dcst_t1s_8  , Dcst_t2s_8  , Dcst_aw_8   , Dcst_bw_8   ,
     $       Dcst_ai_8   , Dcst_bi_8   , Dcst_slp_8
      
      integer Dcst_r_last
      common /Dcst_r/Dcst_r_last
*
!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer, 
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms 
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer 
!version 3 or (at your option) any later version that should be found at: 
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html 
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software; 
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec), 
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
      character * 256 Lun_sortie_s,Lun_outgem_s
      logical Lun_debug_L
      integer Lun_in   , Lun_out  , Lun_lab ,
     $        Lun_tsrs , Lun_zonl , Lun_cte , Lun_rstrt,
     $        Lun_waphy, Lun_wapta, Lun_pilot
*
      common / Lunt / Lun_sortie_s,Lun_outgem_s
      common / Lunl / Lun_debug_L
      common / Luni / Lun_in   , Lun_out  , Lun_lab ,
     %                Lun_tsrs , Lun_zonl , Lun_cte , Lun_rstrt,
     %                Lun_waphy, Lun_wapta, Lun_pilot
*
!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer, 
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms 
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer 
!version 3 or (at your option) any later version that should be found at: 
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html 
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software; 
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec), 
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
      logical Mem_phyncore_L
      integer Mem_minmem,Mem_mx3db,Mem_pslic
*
      
      integer Meml_first(-1:0)
      common /Meml/Meml_first
      common / Meml/ Mem_phyncore_L
      
      integer Meml_last
      common /Meml/Meml_last
      
      integer Memi_first(-1:0)
      common /Memi/Memi_first
      common / Memi/ Mem_minmem,Mem_mx3db,Mem_pslic
      
      integer Memi_last
      common /Memi/Memi_last
*
!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer, 
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms 
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer 
!version 3 or (at your option) any later version that should be found at: 
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html 
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software; 
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec), 
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
      character*1024    Path_nml_S,Path_basedir_S,Path_input_S,Path_ind_
     %S,Path_output_S,Path_work_S,Path_xchg_S
      common / Path_s / Path_nml_S,Path_basedir_S,Path_input_S,Path_ind_
     %S,Path_output_S,Path_work_S,Path_xchg_S
*
      integer MAXBUS
      parameter (MAXBUS = 1000)
      integer p_bdyn_top,p_bper_top,p_bvol_top,p_bent_top,
     $        p_bdyn_siz,p_bper_siz,p_bvol_siz,p_bent_siz,
     $        p_bphy_top,p_nmp,p_ni,p_nj,p_offi,p_offj,
     $        p_bdyn_out,p_bper_out,p_bvol_out,p_bent_out,
     $        p_bent_idx(maxbus),p_bper_idx(maxbus),
     $        p_bdyn_idx(maxbus),p_bvol_idx(maxbus)
      integer entpar(maxbus,8),dynpar(maxbus,8),
     $        perpar(maxbus,8),volpar(maxbus,8)
      common / p_bus /
     $        p_bdyn_top,p_bper_top,p_bvol_top,p_bent_top,
     $        p_bdyn_siz,p_bper_siz,p_bvol_siz,p_bent_siz,
     $        p_bphy_top,p_nmp,p_ni,p_nj,p_offi,p_offj,
     $        p_bdyn_out,p_bper_out,p_bvol_out,p_bent_out,
     $        entpar,dynpar,perpar,volpar,
     $        p_bent_idx,p_bper_idx,
     $        p_bdyn_idx,p_bvol_idx
*
      character*1  p_outbus_s(maxbus)
      character*16  entnm(maxbus),dynnm(maxbus),
     $              pernm(maxbus),volnm(maxbus)
      character*8   enton(maxbus),dynon(maxbus),
     $              peron(maxbus),volon(maxbus)
      character*60  entdc(maxbus),dyndc(maxbus),
     $              perdc(maxbus),voldc(maxbus)
      common /busdync/ entnm, dynnm, pernm, volnm,
     $                 entdc, dyndc, perdc, voldc,
     $                 enton, dynon, peron, volon,p_outbus_s
*
      real, dimension(:), pointer :: Phy_busper3D, Phy_busper3D_digf
      common/ Phy_busper /   Phy_busper3D, Phy_busper3D_digf
*
      integer MAXGEO
      parameter (MAXGEO=1000)
*
      character*16 geonm(MAXGEO,6)
      common /busgeoc/ geonm
*
      integer p_bgeo_top,p_bgeo_siz,geopar(MAXGEO,4)
      common /busgeoi/ p_bgeo_top,p_bgeo_siz,geopar
*
      real, dimension(:), pointer ::  geobus, geofld
      common /busgeor/ geobus, geofld
!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer, 
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms 
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer 
!version 3 or (at your option) any later version that should be found at: 
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html 
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software; 
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec), 
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
!
!busdyn
      integer umoins,vmoins,tmoins,gzmoins6,humoins,pmoins
      integer uplus ,vplus ,tplus ,huplus ,pplus ,phis
      integer omegap,sigm,sigt,dxdy,dz,eponmod,fcpf,fcpw,vlsp
      integer fccpl,fvcpl,mccpl,stplus
!busvol
      integer km,kt,bm,bt,flusolis,rt,pr,fc,fv,tdew,uphytd
      integer vphytd,tphytd,hucond,dm_c,dm_r,dm_i,dm_s,dm_g
      integer dm_h,slw,vis,zet,ze_r,ze_i,ze_s,ze_g,ze_h
!busper
      integer dlat,dlon,mg,ml,z0,lhtg,alvis,twater,tglacier,tsoil
      integer wsoil,snodp,icedp,glsea,glsea0,ufcp,vfcp
      integer incrne,incrhs,incricd,incrgl,incrts,incrtg,incrtp
      integer fdsi, tss, cosz, tdiag, udiag, vdiag
      integer qdiag,tmice,icel
!
      common /dynbusid/ umoins,vmoins,tmoins,gzmoins6,humoins,pmoins
      common /dynbusid/ uplus ,vplus ,tplus ,huplus ,pplus ,phis
      common /dynbusid/ omegap,sigm,sigt,dxdy,dz,eponmod,fcpf,fcpw,vlsp
      common /dynbusid/ fccpl,fvcpl,mccpl,stplus
      common /dynbusid/ km,kt,bm,bt,flusolis,rt,pr,fc,fv,tdew
      common /dynbusid/ uphytd,vphytd,tphytd,hucond
      common /dynbusid/ dlat,dlon,mg,ml,z0,lhtg,alvis,twater,tglacier
      common /dynbusid/ tsoil,wsoil,snodp,icedp,glsea,glsea0,ufcp,vfcp
      common /dynbusid/ incrne,incrhs,incricd,incrgl,incrts,incrtg
      common /dynbusid/ incrtp,fdsi, tss, cosz, tdiag, udiag, vdiag
      common /dynbusid/ qdiag,tmice,dm_c,dm_r,dm_i,dm_s,dm_g,dm_h,slw
      common /dynbusid/ vis,zet,ze_r,ze_i,ze_s,ze_g,ze_h,icel
      integer tr3d_maxn
      parameter (tr3d_maxn=25)
      integer h2o_maxn,h2o_ntr
      parameter (h2o_maxn=8)
      character*8 h2o_liste_S(h2o_maxn),h2o_name_S(h2o_maxn)
      integer h2o_ind(3,h2o_maxn)
      common /h2otr_i/ h2o_ntr,h2o_ind
      common /h2otr_s/ h2o_name_S
      data h2o_liste_S / 'HU','QC','QB','QL','QI','QN','QJ','QH' /
*
      integer phyt_maxn,phyt_ntr
      parameter (phyt_maxn=12)
      character*8 phyt_liste_S(phyt_maxn)
      character*8 phyt_name_S(h2o_maxn+phyt_maxn+tr3d_maxn)
      integer phyt_ind(3,h2o_maxn+phyt_maxn+tr3d_maxn)
      common /phytr_i/ phyt_ntr,phyt_ind
      common /phytr_s/ phyt_name_S
      data phyt_liste_S(1:6) /'EN','NC','NR','NI','NN','NG'/
      data phyt_liste_S(7:12)/'NH','ZR','ZI','ZN','ZG','ZH'/
!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer, 
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms 
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer 
!version 3 or (at your option) any later version that should be found at: 
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html 
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software; 
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec), 
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
      integer
     $        Ptopo_myproc    , Ptopo_myrow, Ptopo_mycol ,
     $        Ptopo_numproc   , Ptopo_npex , Ptopo_npey  ,
     $        Ptopo_npeOpenMP , Ptopo_npeOpenMP_resv     ,
     $        Ptopo_nblocx    , Ptopo_nblocy,Ptopo_mybloc,
     $        Ptopo_myblocx   , Ptopo_myblocy, Ptopo_blocme,
     $        Ptopo_numpe_perb, Ptopo_smtphy , Ptopo_smtdyn
      logical Ptopo_bind_L
      
      integer Ptopo_gindx(6,*)
      pointer(Ptopo_gindx_,Ptopo_gindx)
      common/Ptopo/Ptopo_gindx_
*
      
      integer Ptopo_i_first(-1:0)
      common /Ptopo_i/Ptopo_i_first
      common  / Ptopo_i /
     $        Ptopo_myproc    , Ptopo_myrow, Ptopo_mycol ,
     $        Ptopo_numproc   , Ptopo_npex , Ptopo_npey  ,
     $        Ptopo_npeOpenMP , Ptopo_npeOpenMP_resv     ,
     $        Ptopo_nblocx    , Ptopo_nblocy,Ptopo_mybloc,
     $        Ptopo_myblocx   , Ptopo_myblocy, Ptopo_blocme,
     $        Ptopo_numpe_perb, Ptopo_smtphy , Ptopo_smtdyn,
     $        Ptopo_bind_L
      
      integer Ptopo_i_last
      common /Ptopo_i/Ptopo_i_last
*
      namelist /ptopo/
     $     Ptopo_npex  , Ptopo_npey  , Ptopo_nblocx, Ptopo_nblocy,
     $     Ptopo_smtphy, Ptopo_smtdyn, Ptopo_bind_L
!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer, 
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms 
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer 
!version 3 or (at your option) any later version that should be found at: 
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html 
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software; 
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec), 
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
      logical Lctl_r8stat_L, Lctl_debug_L
      integer Lctl_step, Lctl_reset
*
      common / Lctl / Lctl_step
      
      integer lctl_l_first(-1:0)
      common /lctl_l/lctl_l_first
      common / Lctl_l / Lctl_r8stat_L, Lctl_debug_L
      
      integer lctl_l_last
      common /lctl_l/lctl_l_last
      
      integer lctl_i_first(-1:0)
      common /lctl_i/lctl_i_first
      common / Lctl_i / Lctl_reset
      
      integer lctl_i_last
      common /lctl_i/lctl_i_last
!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer, 
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms 
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer 
!version 3 or (at your option) any later version that should be found at: 
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html 
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software; 
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec), 
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
      logical Step_cliptraj_L
      integer Step_total, Step_rsti, Step_bkup, Step_spinphy
      integer Step_gstat, Step_cleanup, Step_maxcfl
*
      common / Stepi / Step_total, Step_rsti, Step_bkup, Step_spinphy
      common / Stepi / Step_gstat, Step_cleanup, Step_maxcfl, Step_clipt
     %raj_L
*
!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer, 
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms 
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer 
!version 3 or (at your option) any later version that should be found at: 
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html 
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software; 
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec), 
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
      logical Rstri_rstn_L, Rstri_idon_L, Rstri_half_L, Rstri_glbcol_L
*
      integer Rstri_sdon
*
      common / Rstri / Rstri_sdon
      common / Rstrl / Rstri_rstn_L,  Rstri_idon_L, Rstri_half_L,
     &                 Rstri_glbcol_L
*
!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer, 
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms 
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer 
!version 3 or (at your option) any later version that should be found at: 
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html 
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software; 
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec), 
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
      character*16 Mod_runstrt_S
*
      common / modc / Mod_runstrt_S
*
      character*16  P_pbl_schsl_S,P_pbl_schurb_S,
     $              P_pbl2_schsl_S,P_cond_stcon_s,
     $              P_pbl_vegfld_S,P_pbl_lakes_s
*
      logical P_cond_satu_L,   P_pbl_snoalb_L, P_pbl_iceme_L,
     $        P_pbl_icelac_L,  P_pbl_ocean_L ,  P_pbl2_iceme_L,
     $        P_pbl_mesh_L
*
      integer P_fcpkuo_xofset, P_fcpkuo_xblnd, P_out_moyhr,
     $        P_fcpkuo_yofset, P_fcpkuo_yblnd, P_pbd_dumpbus,
     $        P_pbl_nmos,      P_pbl_schsl_IG
      real*8  p_lmvd_valml_8  ,p_lmvd_mllat_8,
     $        p_lmvd_valeq_8  ,p_lmvd_eqlat_8
*
      common /phy_s/ P_pbl_schsl_S,P_pbl_schurb_S,
     $               P_pbl2_schsl_S,P_cond_stcon_s,
     $               P_pbl_vegfld_S,P_pbl_lakes_s
      common /phy_l/ P_cond_satu_L, P_pbl_snoalb_L, P_pbl_iceme_L,
     $               P_pbl_icelac_L,P_pbl_ocean_L, P_pbl2_iceme_L,
     $               P_pbl_mesh_L
      common /phy_i/ P_fcpkuo_xofset, P_fcpkuo_xblnd, P_out_moyhr,
     $               P_fcpkuo_yofset, P_fcpkuo_yblnd, P_pbd_dumpbus,
     $               p_pbl_nmos,      P_pbl_schsl_IG
      common /phy_r8/ p_lmvd_valml_8  ,p_lmvd_mllat_8,
     $                p_lmvd_valeq_8  ,p_lmvd_eqlat_8
*
      
      real P_fcpkuo_fcpf(l_ni,l_nj)
      pointer(P_fcpkuo_fcpf_,P_fcpkuo_fcpf)
      common/P_fcpkuo/P_fcpkuo_fcpf_
      
      real P_fcpkuo_fcpw(l_ni,l_nj)
      pointer(P_fcpkuo_fcpw_,P_fcpkuo_fcpw)
      common/P_fcpkuo/P_fcpkuo_fcpw_
      
      real P_lmvd_vlsp(l_ni,l_nj)
      pointer(P_lmvd_vlsp_,P_lmvd_vlsp)
      common/P_lmvd/P_lmvd_vlsp_
*
*---------------------|-------------------------------------------------|
* P_pset_second_L     | .TRUE. implies that a second call to the        |
*                     | physics package will be done                    |
*---------------------|-------------------------------------------------|
* The following parametres are ignored when P_pset_second_L is .FALSE.  |
* ************* but they MUST the otherwise specified ***************** |
*          otherwise, only the first physics will be called             |
*---------------------|-------------------------------------------------|
* P_pset_xofset       | number of columns from the edges of the grid    |
*                     | where only the second physics set is requested. |
*                     | This is applied both on the left and right sides|
*                     | when P_pset_xofsetr is undefined                |
* P_pset_xofsetr      | columns for the right side of the grid where    |
*                     | only the second physics is requested. Defaults  |
*                     | to P_pset_xofset                                |
* P_pset_yofset       | number of rows from the edges of the grid where |
*                     | only the second physics set is requested. Again,|
*                     | this is appled both on the bottom and top sides |
*                     | when P_pset_yofsett is undefined                |
* P_pset_yofsett      | # of columns for the top side of the grid where |
*                     | only the second physics is requested. Defaults  |
*                     | to P_pset_yofset                                |
* P_pset_xblnd        | number of columns where both physics are        |
*                     | requested + 1 (this is the blending region)     |
* P_pset_yblnd        | number of rows where both physics are requested |
*                     | + 1 (this is the blending region)               |
*---------------------|-------------------------------------------------|
*                                                                       |
* The lower left corner point of the model domain where only the first  |
* physics is applied is thus...                                         |
*                                                                       |
*      ( P_pset_xofset+P_pset_xblnd , P_pset_yofset+P_pset_yblnd )      |
*                                                                       |
* and the corresponding upper right corner point is...                  |
*                                                                       |
* ( G_ni-P_pset_xofsetr-P_pset_xblnd+1 ,                                |
*                              G_nj-P_pset_yofsett-P_pset_yblnd+1 )     |
*                                                                       |
*---------------------|-------------------------------------------------|
* P_pset_secondi      | integer indices at each latitude of the:        |
*                     | 1) first longitudes where the first physics set |
*                     |    starts to be applied                         |
*                     | 2) first longitudes where only the first physics|
*                     |    set is applied                               |
*                     | 3) last longitudes where only the first physics |
*                     |    set is applied                               |
*                     | 4) last longitudes where the first physics set  |
*                     |    is applied                                   |
* P_pset_secondw      | real*8 weights at each longitude and latitude   |
*                     | that apply to the first physics set             |
*-----------------------------------------------------------------------
*
*
      logical  P_pset_second_L
      integer  P_pset_xofset  ,P_pset_xofsetr ,
     $         P_pset_yofset  ,P_pset_yofsett ,
     $         P_pset_xblnd   ,P_pset_yblnd
*
      common /ppset_i/ P_pset_xofset ,P_pset_xofsetr ,
     $                 P_pset_yofset ,P_pset_yofsett ,
     $                 P_pset_xblnd  ,P_pset_yblnd
*
      common /ppset_l/ P_pset_second_L
*
      
      integer P_pset_secondi(  4 ,l_nj)
      pointer(P_pset_secondi_,P_pset_secondi)
      common/P_pset/P_pset_secondi_
      
      real*8 P_pset_secondw(l_ni,l_nj)
      pointer(P_pset_secondw_,P_pset_secondw)
      common/P_pset/P_pset_secondw_
!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer, 
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms 
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer 
!version 3 or (at your option) any later version that should be found at: 
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html 
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software; 
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec), 
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
      real*8    Cstv_dt_8, Cstv_tau_8, Cstv_tstr_8,
     %          Cstv_pitop_8, Cstv_pisrf_8, Cstv_uvdf_8, Cstv_phidf_8,
     %          Cstv_hco0_8 , Cstv_hco1_8
*
      
      integer Cstv_first(-1:0)
      common /Cstv/Cstv_first
      common / Cstv / Cstv_dt_8, Cstv_tau_8, Cstv_tstr_8,
     %                Cstv_pitop_8, Cstv_pisrf_8, Cstv_uvdf_8,
     $                Cstv_phidf_8, Cstv_hco0_8 , Cstv_hco1_8
      
      integer Cstv_last
      common /Cstv/Cstv_last
*
      
      real*8 Cstvr_fistr_8(*)
      pointer(Cstvr_fistr_8_,Cstvr_fistr_8)
      common/Cstvr/Cstvr_fistr_8_
!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer, 
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms 
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer 
!version 3 or (at your option) any later version that should be found at: 
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html 
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software; 
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec), 
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
*
      logical G_lam, G_periodx, G_periody
      integer G_ni, G_nj, G_nk, G_niu, G_njv, G_lnimax, G_lnjmax,
     $        G_halox, G_haloy
      real*8 G_xg_8, G_yg_8
      pointer (paxg_8, G_xg_8(1-G_ni:2*G_ni)),
     $        (payg_8, G_yg_8(1-G_nj:2*G_nj))
      common / G_p / paxg_8,payg_8
*
      
      integer G_first(-1:0)
      common /G/G_first
      common / G / G_ni, G_nj, G_nk, G_niu, G_njv, G_lnimax, G_lnjmax,
     $             G_halox, G_haloy
      
      integer G_last
      common /G/G_last
      
      integer G_l_first(-1:0)
      common /G_l/G_l_first
      common / G_l / G_lam,G_periodx,G_periody
      
      integer G_l_last
      common /G_l/G_l_last
*
      logical l_north, l_south, l_east, l_west, l_mesg_proc
      integer l_ni, l_nj, l_nk, l_niu, l_njv, l_i0, l_j0,
     $        l_minx, l_maxx, l_miny, l_maxy,
     $        l_dimmsg, l_dim2d, l_dim3d,
     $        pil_n,pil_s,pil_w,pil_e,north,south,east,west
      
      integer l_first(-1:0)
      common /l/l_first
      common / l / l_north, l_south, l_east, l_west, l_mesg_proc,
     $             l_ni, l_nj, l_nk, l_niu, l_njv, l_i0, l_j0,
     $             l_minx, l_maxx, l_miny, l_maxy,
     $             l_dimmsg, l_dim2d, l_dim3d,
     $             pil_n,pil_s,pil_w,pil_e,north,south,east,west
      
      integer l_last
      common /l/l_last
*
!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer, 
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms 
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer 
!version 3 or (at your option) any later version that should be found at: 
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html 
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software; 
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec), 
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
*
      integer Glb_pil_n, Glb_pil_s, Glb_pil_w, Glb_pil_e,
     $        Lam_pil_n, Lam_pil_s, Lam_pil_w, Lam_pil_e
*
      
      integer Glb_pil_i_first(-1:0)
      common /Glb_pil_i/Glb_pil_i_first
      common /Glb_pil_i/ Glb_pil_n, Glb_pil_s, Glb_pil_w, Glb_pil_e
      
      integer Glb_pil_i_last
      common /Glb_pil_i/Glb_pil_i_last
*
      common /Lam_pil_i/ Lam_pil_n, Lam_pil_s, Lam_pil_w, Lam_pil_e
!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer, 
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms 
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer 
!version 3 or (at your option) any later version that should be found at: 
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html 
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software; 
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec), 
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
      integer Hgc_ig1ro, Hgc_ig2ro, Hgc_ig3ro, Hgc_ig4ro,
     $        dstf_gid, dstu_gid, dstv_gid,
     $        dstf_gis, dstu_gis, dstv_gis
      character*4 Hgc_gxtyp_s
*
      
      integer Hgc_first(-1:0)
      common /Hgc/Hgc_first
      common /Hgc/ Hgc_ig1ro, Hgc_ig2ro, Hgc_ig3ro, Hgc_ig4ro,
     $             dstf_gid, dstu_gid, dstv_gid,
     $             dstf_gis, dstu_gis, dstv_gis
      
      integer Hgc_last
      common /Hgc/Hgc_last
      
      integer Hgc_s_first(-1:0)
      common /Hgc_s/Hgc_s_first
      common /Hgc_s/ Hgc_gxtyp_s
      
      integer Hgc_s_last
      common /Hgc_s/Hgc_s_last
!
      integer, parameter     :: Cfld_nbr=6
      integer                :: Cfld_date(Cfld_nbr)
      character(len=4), save :: Cfld_list_S(Cfld_nbr)=(/
     + 'SD  '  ,'HS  '  ,'I8  '  ,'LG  '  ,'TP  '  ,'TM  '  /)
      character(len=6), save :: Cfld_interp_S(Cfld_nbr)=(/
     + 'VOISIN','VOISIN','VOISIN','VOISIN','LINEAR','LINEAR'/)
!
      
      integer cfld_I_first(-1:0)
      common /cfld_I/cfld_I_first
      common /cfld_I/  Cfld_date
      
      integer cfld_I_last
      common /cfld_I/cfld_I_last
!
!
!modules
      integer, parameter :: SLEEP_PERIOD=1
      interface
        subroutine sleep_a_bit(duration) bind(C,name='sleep')
        use ISO_C_BINDING
        integer(C_INT), intent(IN), value :: duration
        integer(C_INT) :: status
        end subroutine sleep_a_bit
      end interface
      INTEGER,EXTERNAL :: newdate,fnom,fclos,wkoffit,fstopc,fstouv
      INTEGER,EXTERNAL :: fstinf,fstsui,fstlir,fstlirx,fstprm,fstfrm,fst
     %nbr
      INTEGER,EXTERNAL :: ezgdef_fmem,ezsetopt,ezsint,ezdefset,ip1_all
      INTEGER,EXTERNAL :: create_flag_file, wait_for_flag_file, remove_f
     %lag_file
!*
!
      REAL(8)      days_8,hour_8,rad_8
      character(16) xndate
!
      CHARACTER    gtyp_S(Cfld_nbr),gtypz_S(Cfld_nbr)
      INTEGER      xnzig1(Cfld_nbr),xnzig2(Cfld_nbr),xnzig3(Cfld_nbr)
      INTEGER      xnig1(Cfld_nbr), xnig2(Cfld_nbr), xnig3(Cfld_nbr)
      INTEGER      xnig4(Cfld_nbr), xnzig4(Cfld_nbr)
      INTEGER      xnilu(Cfld_nbr), xnjlu(Cfld_nbr)
!
      LOGICAL      IntSol_L,IntIce_L,NoLeapYears_L
      LOGICAL,     SAVE :: acli_L(Cfld_nbr)
      INTEGER      xniz,xnjz
      INTEGER      Lun_clim,Lun_acli,Lun_activ,ppjour,aujour
      INTEGER      xnerr,xncle,xnhold,xncoupe
      INTEGER      xni,xnj,xnk,xnim,xnjm, i,j,k,ik,n,x, lowb,upb
      INTEGER,     SAVE :: xnzm(Cfld_nbr),xnmxfld(Cfld_nbr)
      INTEGER      xnmxfld_max,xnzm_max
      CHARACTER(2) typvar_S,vide_S,hold_S*12
      CHARACTER(512) clima_S,anclima_S
      CHARACTER(8) soli_S
!
      REAL(8)      scal_8(Cfld_nbr)
      integer(8)   Done_Inincr(Cfld_nbr), Do_Inincr(Cfld_nbr)
!
      integer      offi,offj,indx,dgid,sgid
      real         neige, lxfi(l_ni),lyfi(l_nj), aucun(1)
      integer(8),  dimension (:,:)  , allocatable, save :: dates
      real,        dimension (:,:,:), allocatable :: xrwork
      real,        dimension (:,:)  , allocatable :: xrclim
      real,        dimension (:,:)  , allocatable :: xrtic,xrtac
      real,        dimension (:)    , pointer     :: busper
!
      LOGICAL,     save :: VRAI=.true.
!
!     Variables needed to read minutely SST & sea ice BC's
      INTEGER(8),  PARAMETER :: factor10=10000000000_8,
     +                          factor8=100000000_8,
     +                          factor6=1000000_8,
     +                          factor4=10000_8
      INTEGER(8)   current_date
      INTEGER      curyy,curmo,curdd,curhh,curmm,curss,cursign
      INTEGER      handle, iip1,iip2,iip3, gii,gji,gif,gjf
      INTEGER      ip2(Cfld_nbr),ip3(Cfld_nbr),datev(Cfld_nbr)
      INTEGER      nrec_clim,nrec_acli,nrec_max
      INTEGER,     SAVE :: nrec(Cfld_nbr)
      INTEGER      year, month, day, hour, zulu
      INTEGER      dateo,deet,npas, datec, mo1
      INTEGER      date1,date2,time1,time2,stamp1,stamp2
      LOGICAL      open_acli_L, open_clim_L
      LOGICAL,     SAVE :: monthly_L(Cfld_nbr)
      CHARACTER(2),SAVE :: typv_S(Cfld_nbr)
      INTEGER      momid_day(12), momid_day_leap(12)
!
      DATA         momid_day      / 1612,1500,1612,1600,1612,1600,
     %                              1612,1612,1600,1612,1600,1612 /
      DATA         momid_day_leap / 1612,1512,1612,1600,1612,1600,
     %                              1612,1612,1600,1612,1600,1612 /
!
!*    Don't do anything at timestep 0
      if ( Lctl_step <= 0 ) return
!
      rad_8 = 45./atan( 1.0_8 )
!
!
!
!*       -------------------------------------------------
!*    1. Get the current date and time
!*       -------------------------------------------------
!
      days_8 = ( Lctl_step * (Cstv_dt_8 / 3600._8 ) ) / 24._8
      call incdatsd( xndate,Mod_runstrt_S,days_8 )
!
!*    internal dates are saved in a YYYY/MO/DD/HH/MM/SS format
      call prsdate( curyy,curmo,curdd,curhh,curmm,curss,cursign, xndate 
     %)
      Current_Date = curyy*factor10 + curmo*factor8
     +             + curdd*factor6  + curhh*factor4
     +             + curmm*100      + curss
!
!*    At very first time step initialize Done_Inincr to start of run
      if ( Lctl_step == 1 ) then
         call prsdate( curyy,curmo,curdd,curhh,curmm,curss,cursign, Mod_
     %runstrt_S )
         Done_Inincr = curyy*factor10 + curmo*factor8
     +               + curdd*factor6  + curhh*factor4
     +               + curmm*100      + curss
!
!*    At all other time steps set Done_Inincr to Cfld_date (saved coded Done_Inincr)
      else
         do x=1,Cfld_nbr
!*          Decode date from 'Cfld_date' in 'Done_Inincr'
            if ( Cfld_date(x) > 0 ) then
               xnerr = newdate( Cfld_date(x), date1, time1, -3 )
               if ( xnerr > 0 ) call gem_stop( 'itf_phy_inincr',-8 )
               Done_Inincr(x) = date1*factor6 + time1/100
            else
               Done_Inincr(x) = -1
            end if
         end do
      end if
!
!
!*    Update the climatological ozone field every 24 hours
!*    (and note that it is first done at timestep 1)
!
      if (Cstv_dt_8 < 43200. ) then
!
         ppjour = nint( 86400. / Cstv_dt_8 )
         aujour = mod( Lctl_step, ppjour )
!
         if (Rstri_sdon > 0 .and.
     %       aujour    == 1 )
     %       call intozon( curdd,curmo, lun_out>0 )
!
      endif
!
      call Get_LeapYear_Status( NoLeapYears_L )
!
!
!*    Determine whether the surface land scheme is fully
!*    interactive or not. If it is, the TP increments will
!*    only be used for TGLACIER(2)
!
      call low2up( P_pbl_schsl_S,soli_S )
      if (soli_S  == 'CLASS' .or. soli_S == 'ISBA') then
         IntSol_L = .true.
      else
         IntSol_L = .false.
      endif
!
!*    Check for interactive snow (in glacier & sea ice
!*    modules) and ice thichness (in sea ice module)
!
      IntIce_L = P_pbl_iceme_L
!
!C    print *,'Dans itf_phy_inincr: soli_S ',soli_S
!C    call flush( Lun_out )
!
!
!
!*       -------------------------------------------------
!*    2. If 'Rstri_sdon=1' processor 0 reads all dates 
!*       in BC files imclima and imanclima
!*       -------------------------------------------------
!
      RSTRI_SDON_1 : if ( Rstri_sdon == 1 ) then
!
         PTOPO_MYPROC_0_1 : if ( Ptopo_myproc == 0 ) then
!
            if (Lun_out > 0) write( Lun_out,'(/A)')
     %      ' ITF_PHY_ININCR: checking content of forcing files'
            xnim    = 0
            xnjm    = 0
!
            Lun_clim = 0
            Lun_acli = 0
!
            nrec_clim = 0
            nrec_acli = 0
!
!*          Open file imanclima
            anclima_S =  trim(Path_input_S)//'/imanclima'
!
            xnerr = wkoffit( anclima_S )
!
            if ( xnerr == 1 .or. xnerr == 33 ) then
!
               xnerr = fnom( Lun_acli, anclima_S , 'STD+RND+OLD+R/O', 0 
     %)
               xnerr = fstouv( Lun_acli, 'RND' )
!
               if ( xnerr >= 0 ) then
!
                  if (Lun_out > 0) write( Lun_out,* )
     %            'Opening file 1: ', trim( anclima_S )
!
!*                Get total number of records in file
                  nrec_acli = fstnbr (Lun_acli)
!
               end if
            end if
!
!
!*          Open file imclima
            clima_S = trim(Path_input_S)//'/imclima'
!
            xnerr = wkoffit( clima_S )
!
            if ( xnerr == 1 .or. xnerr == 33 ) then
!
               xnerr = fstopc( 'MSGLVL','INFORM', 0 )
               xnerr = fstopc( 'TOLRNC','INFORM', 0 )
               xnerr = fnom( Lun_clim, clima_S , 'STD+RND+OLD+R/O', 0 )
               xnerr = fstouv( Lun_clim, 'RND' )
!
               if ( xnerr >= 0 ) then
!
                  if (Lun_out > 0) write( Lun_out,* )
     %            'Opening file 2: ', trim( clima_S )
!
!*                Get total number of records in file
                  nrec_clim = fstnbr (Lun_clim)
!
               end if
            end if
!
!
!*          Get maximum number of records/date in files for allocation
            nrec_max = max( nrec_acli,nrec_clim )
!
         end if PTOPO_MYPROC_0_1
!
         call RPN_COMM_bcast( nrec_max,1      ,"MPI_INTEGER",0,"grid",xn
     %err )
         if ( nrec_max == 0 ) call gem_stop( 'itf_phy_inincr',-9 )
!
!*       All processors allocate 'dates'
         allocate ( dates(Cfld_nbr,nrec_max), stat=xnerr )
         If (xnerr > 0)  call gem_stop( 'itf_phy_inincr',-1 )
         dates = -1
!                
!*       Processor 0 reads all dates
         PTOPO_MYPROC_0_2 : if ( Ptopo_myproc == 0 ) then
!
!*          Read dates of records in file imanclimat
            if ( nrec_acli > 0 ) then
!
               do i=1,Cfld_nbr
!
                  if (    (Cfld_list_S(i) == 'SD' .and. IntIce_L .and. I
     %ntSol_L)
     %               .or. (Cfld_list_S(i) == 'I8' .and. IntIce_L)
     %               .or. (Cfld_list_S(i) == 'HS' .and. IntSol_L)) cycle
                  nrec(i) = 0
                  handle = fstinf( Lun_acli,xni,xnj,xnk,
     %                             -1,' ',-1,-1,-1,' ',Cfld_list_S(i) )
!
                  do while ( handle >= 0 )
!
                     xnim  = max( xnim,xni )
                     xnjm  = max( xnjm,xnj )
!
                     xnerr = fstprm( handle,dateo,deet,npas,
     %                               xnhold,xnhold,xnhold,
     %                               xnhold,xnhold,xnhold,iip2,iip3,
     %                               typv_S(i),hold_S,hold_S,hold_S,
     %                               xnhold,xnhold,xnhold,xnhold,
     %                               xnhold,xnhold,xnhold,xnhold,
     %                               xnhold,xnhold,xnhold )
!
                     nrec(i) = nrec(i) + 1
!
                     year = iip3 ; month = iip2 ; day = 0  ; zulu = 0 ; 
     %hour = 0
                     if ( deet > 0 .and. npas > 0 ) then 
                        hour_8 = ( deet * 1_8 * npas ) / 3600.0_8 ; hour
     %  = nint( hour_8 )
                        call incdatr( datec,dateo, hour_8 )
                     else
                        datec = dateo
                     endif
!
                     xnerr = newdate( datec,date1,time1,-3 )
!
                     if (     date1/10000     == year  .and.
     +                   mod( date1/100,100 ) == month) then 
                        day   = mod( date1,100 )
                        zulu  = time1/factor6
                     else if ( (hour == iip2 .and. deet > 0) .or.
     +                         month < 1 ) then 
                        year  = date1/10000
                        month = mod( date1/100,100 )
                        day   = mod( date1,100 )
                        zulu  = time1/factor6
                     endif
!
!*                   Check if file contains monthly data (where
!*                   each monthly value is ordered consecutively)
                     if ( nrec(i) == 1 ) then
                        mo1 = month
                        monthly_L(i) = .true.
                     elseif ( monthly_L(i) ) then
                        if ( mod(mo1,12)+1 /= month ) monthly_L(i) = .fa
     %lse.
                        mo1 = month
                     end if
!
                     dates(i,nrec(i)) = year*factor10 + month*factor8
     +                                + day *factor6  + zulu *factor4
!
                     handle = fstsui( Lun_acli, xni,xnj,xnk )
!
                  enddo
!
                  if (nrec(i) > 0) acli_L(i) = .true.
!
                  xnmxfld(i) = xnim*xnjm
                  xnzm(i)    = max( xnim,xnjm )
!
!*                For monthly data set date to the middle of the month
                  if ( monthly_L(i) ) then
                     do n=1,nrec(i)
                        year  = int(  dates(i,n) / factor10 )
                        month = int( (dates(i,n) - year*factor10) / fact
     %or8 )
!
!*                      If leap year
                        if ( .not. NoLeapYears_L     .and.
     %                      ( mod( year,4 )   == 0   .and.
     %                        mod( year,100 ) /= 0 ) .or.
     %                        mod( year,400 ) == 0 ) then
                           dates(i,n) = year*factor10 + month*factor8
     %                                + momid_day_leap(month)*factor4
!*                      If non-leap year
                        else
                           dates(i,n) = year*factor10 + month*factor8
     %                                +      momid_day(month)*factor4
                        end if
!
                     end do
                  end if
!
               end do
!
               xnerr = fstfrm( Lun_acli )
               xnerr = fclos ( Lun_acli )
!
            end if
!
!*          Read dates of records in file imclimat
            if ( nrec_clim > 0 ) then
!
               do i=1,Cfld_nbr
!
                  if (    (Cfld_list_S(i) == 'SD' .and. IntIce_L .and. I
     %ntSol_L)
     %               .or. (Cfld_list_S(i) == 'I8' .and. IntIce_L)
     %               .or. (Cfld_list_S(i) == 'HS' .and. IntSol_L)) cycle
!*                Only try to read variable if not already found in imanclimat
!*                Don't need to read dates since it is a climatological file.
                  if ( .not. acli_L(i) ) then
!
                     handle = fstinf( Lun_clim,xni,xnj,xnk,
     %                                -1,' ',-1,-1,-1,' ',Cfld_list_S(i)
     % )
!
                     do while ( handle >= 0 )
!
                        xnim  = max( xnim,xni )
                        xnjm  = max( xnjm,xnj )
!
                        handle = fstsui( Lun_clim, xni,xnj,xnk )
!
                     enddo
!
                     xnmxfld(i) = xnim*xnjm
                     xnzm(i)    = max( xnim,xnjm )
!
                     typv_S(i)  = 'C'
!
                  end if
               end do
!
               xnerr = fstfrm( Lun_clim )
               xnerr = fclos ( Lun_clim )
!
            end if
!
         end if PTOPO_MYPROC_0_2
!
!
!        Broadcast a few variables
         call RPN_COMM_bcast ( nrec     ,Cfld_nbr           ,"MPI_INTEGE
     %R"  ,0,"grid",xnerr )
         call RPN_COMM_bcast ( xnmxfld  ,Cfld_nbr           ,"MPI_INTEGE
     %R"  ,0,"grid",xnerr )
         call RPN_COMM_bcast ( xnzm     ,Cfld_nbr           ,"MPI_INTEGE
     %R"  ,0,"grid",xnerr )
         call RPN_COMM_bcast ( dates    ,Cfld_nbr*nrec_max*2,"MPI_INTEGE
     %R"  ,0,"grid",xnerr )
         call RPN_COMM_bcast ( acli_L   ,Cfld_nbr           ,"MPI_LOGICA
     %L"  ,0,"grid",xnerr )
         call RPN_COMM_bcast ( monthly_L,Cfld_nbr           ,"MPI_LOGICA
     %L"  ,0,"grid",xnerr )
         call RPN_COMM_bcastc( typv_S   ,Cfld_nbr*2         ,"MPI_CHARAC
     %TER",0,"grid",xnerr )
!
      end if RSTRI_SDON_1
!
!
!*       -------------------------------------------------
!*    3. Check for each variable if a new date needs to be read
!*       -------------------------------------------------
!*
!*       Done_Inincr: Last date read
!*       Do_Inincr  : Next date to be read
!*       in a YYYY/MO/DD/HH/MM/SS format
!
      open_acli_L = .false.
      open_clim_L = .false.
      Do_Inincr   = Done_Inincr
!
!*    Loop over all variables
      LOOP_OVER_ALL_VARIABLES_1 : do i=1,Cfld_nbr
!
         if (    (Cfld_list_S(i) == 'SD' .and. IntIce_L .and. IntSol_L)
     %      .or. (Cfld_list_S(i) == 'I8' .and. IntIce_L)
     %      .or. (Cfld_list_S(i) == 'HS' .and. IntSol_L)) cycle
!
!*       If next date got already read, do nothing
         if ( Do_Inincr(i) >= current_date
     %        .and. Lctl_step .ne. 1 ) cycle
!
!*       Find next date in analysed climatology imanclimat
         if ( acli_L(i) ) then
!
!*          Find the next date to read
            do n=1,nrec(i)
               if ( Do_Inincr(i) < dates(i,n) ) then
                  Do_Inincr(i) = dates(i,n)
                  exit
               end if
            end do
!
!*          Stop the run if new date was not found
            if ( Do_Inincr(i) < current_date ) then
               if (Lun_out > 0) write( Lun_out,1009 ) current_date
               call flush( Lun_out )
               call gem_stop('itf_phy_inincr',-2)
            end if
!
!*          Set DATEV,IP2-3 to date to be read
!
            If ( monthly_L(i) ) then
               ip3(i) =   Do_Inincr(i) /        factor10
               ip2(i) = ( Do_Inincr(i) - ip3(i)*factor10 ) / factor8
               datev(i) = -1
            Else
               date1 =      Do_Inincr(i)/factor6
               time1 = mod( Do_Inincr(i),factor6 )*100
               xnerr = newdate( datev(i),date1,time1,+3 )
               ip3(i) = -1 ; ip2(i) = -1
            End If
!
!*          Set flag that imanclimat needs to be opened
            open_acli_L = .true.
!
!        Set next date to be read in climatology imclimat
         else
!
            year  =   Do_Inincr(i) / factor10
            month = ( Do_Inincr(i) - year*factor10 ) / factor8
!
!*          Set day and time
!*          If leap year
 0050       if ( .not. NoLeapYears_L     .and.
     %          ( mod( year,4 )   == 0   .and.
     %            mod( year,100 ) /= 0 ) .or.
     %            mod( year,400 ) == 0 ) then
               Do_Inincr(i) = year*factor10 + month *factor8
     %                      +  momid_day_leap(month)*factor4
!*          If non-leap year
            else
               Do_Inincr(i) = year*factor10 + month *factor8
     %                      +       momid_day(month)*factor4
            end if
!             
!           Increment Do_Inincr if < current_date
            if ( Do_Inincr(i) < current_date ) then
               month = month + 1
               if ( month == 13 ) then
                  month = 01
                  year = year + 1
               end if
               goto 0050
            end if
! 
!*          Set DATEV,IP2-3 to date to be read
            datev(i) = -1
            ip3(i)   = -1
            ip2(i)   = month
!
!*          Set flag that imclimat needs to be opened
            open_clim_L = .true.
!
         end if
!
      end do LOOP_OVER_ALL_VARIABLES_1
!
!c    if ( Rstri_sdon == 15 ) then
!c    if ( Rstri_sdon == 408 ) then
!c    if ( Rstri_sdon == 408 ) then
!c       call gem_stop( 'itf_phy_inincr',-3 )
!c    end if
!
!
!     Return if no new fields need to be read
      if ( .not. open_acli_L .and. .not. open_clim_L ) return
!
!
!
!*       -------------------------------------------------
!*    4. Processor 0 opens imanclimat and/or imclimat
!*       -------------------------------------------------
!
      PTOPO_MYPROC_0_3 : if ( Ptopo_myproc == 0 ) then
!
         if (Lun_out > 0) write( Lun_out,'(/A)')
     %   ' ITF_PHY_ININCR: defining new physics forcing increments'
!*       Open analysied climatology imanclima if needed
         if ( open_acli_L ) then
!
            Lun_acli = 0
!
            anclima_S = trim(Path_input_S)//'/imanclima'
!
            xnerr = wkoffit( anclima_S )
!
            if ( xnerr == 1 .or. xnerr == 33 ) then
!
               xnerr = fnom( Lun_acli, anclima_S , 'STD+RND+OLD+R/O', 0 
     %)
               xnerr = fstouv( Lun_acli, 'RND' )
!
               if ( xnerr >= 0 ) then
                  if (Lun_out > 0) write( Lun_out,* )
     %            'Opening file 3: ', trim( anclima_S )
               else
                  if (Lun_out > 0) write( Lun_out,1002 )
                  Lun_acli = 0
               end if
!
            else
               if (Lun_out > 0) write( Lun_out,1002 )
               Lun_acli = 0
            end if
! 
         end if
!
!
!*       Open climatology imclima if needed
         if ( open_clim_L ) then
!
            Lun_clim = 0
!
            clima_S = trim(Path_input_S)//'/imclima'
!
            xnerr = wkoffit( clima_S )
!
            if ( xnerr == 1 .or. xnerr == 33 ) then
!
               xnerr = fnom( Lun_clim, clima_S , 'STD+RND+OLD+R/O', 0 )
               xnerr = fstouv( Lun_clim, 'RND' )
!
               if ( xnerr >= 0 ) then
                  if (Lun_out > 0) write( Lun_out,* )
     %            'Opening file 4: ', trim( clima_S )
               else
                  if (Lun_out > 0) write( Lun_out,1001 ) trim( clima_S )
                  Lun_clim = 0
               end if
!
            else
               if (Lun_out > 0) write( Lun_out,1001 ) trim( clima_S )
               Lun_clim = 0
            end if
! 
         end if
!
      end if PTOPO_MYPROC_0_3
!
!     Broadcast a few variables
      call RPN_COMM_bcast( Lun_acli    ,1,"MPI_INTEGER",0,"grid",xnerr )
      call RPN_COMM_bcast( Lun_clim    ,1,"MPI_INTEGER",0,"grid",xnerr )
!
!*    Stop run if one of the files could not be opened
      if (( open_acli_L .and. Lun_acli == 0 ) .or.
     %    ( open_clim_L .and. Lun_clim == 0 ))
     %   call gem_stop( 'itf_phy_inincr',-4 )
!
!
!*       -------------------------------------------------
!*    5. Read fields for new dates
!*       -------------------------------------------------
!
!
!*    Every one allocates the Cfld_nbr 2D climatology fields
      xnzm_max    = maxval( xnzm(1:Cfld_nbr) )
      xnmxfld_max = maxval( xnmxfld(1:Cfld_nbr) )
!      
      allocate ( xrtic(xnzm_max,Cfld_nbr), xrtac(xnzm_max,Cfld_nbr), sta
     %t=xnerr )
      If (xnerr > 0)  call gem_stop( 'itf_phy_inincr',-5 )
      allocate ( xrclim(xnmxfld_max,Cfld_nbr),xrwork(l_ni,l_nj,Cfld_nbr)
     %, stat=xnerr )
      If (xnerr > 0) call gem_stop( 'itf_phy_inincr',-6 )
!
!
!*    Processor 0 reads fields for new date
      PTOPO_MYPROC_0_4 : if ( Ptopo_myproc == 0 ) then
!
         xnerr = fstopc( 'MSGLVL','INFORM', 0 )
         xnerr = fstopc( 'TOLRNC','INFORM', 0 )
!
!*       Loop over all variables
         LOOP_OVER_ALL_VARIABLES_2 : do i=1,Cfld_nbr
!
!         print *,"INFO: i=",i
!*          Only read new date if needed
            if ( Done_Inincr(i) >= current_date
     %           .and. Lctl_step /= 1 ) cycle
!
!*          Only read new date if needed
            if (    (Cfld_list_S(i) == 'SD' .and. IntIce_L .and. IntSol_
     %L)
     %         .or. (Cfld_list_S(i) == 'I8' .and. IntIce_L)
     %         .or. (Cfld_list_S(i) == 'HS' .and. IntSol_L)) then
!
               if (Lun_out > 0) write( Lun_out,1004 ) Cfld_list_S(i)
!
            else
!
               xncle = -1
!
!              Read fields from analysed climatology imanclimat
               if ( acli_L(i) ) then
!
                 xni = 2
                 xnj = 2
                 xnk = 3
                 do while(xni==2 .and. xnj==2 .and. xnk==3)
                    xncle = fstlir( xrclim(1,i), Lun_acli,
     %                            xni,xnj,xnk,  datev(i),' ',
     %                            -1,ip2(i),ip3(i),typv_S(i),
     %                            Cfld_list_S(i) )
!
                    if ( xncle < 0 ) then
                      if (Lun_out > 0) write( Lun_out,1005 ) Cfld_list_S
     %(i)
                      goto 0040
                    endif
                    if(xni==2 .and. xnj==2 .and. xnk==3)then   
                      xnerr = fstfrm(Lun_acli)                 
                      xnerr = fclos(Lun_acli)
                      call sleep_a_bit(SLEEP_PERIOD)                   
         write( Lun_out,*)'INFO: reopening '//trim(anclima_S)
                      xnerr = fnom( Lun_acli, anclima_S ,
     %                        'STD+RND+OLD+R/O', 0 )   
                      xnerr = fstouv( Lun_acli, 'RND' )
                    endif
                 enddo
!
!              Read fields from climatology imclimat
               else
!
                 xni = 2
                 xnj = 2
                 xnk = 3
                 do while(xni==2 .and. xnj==2 .and. xnk==3)
                    xncle = fstlir( xrclim(1,i), Lun_clim,
     %                            xni,xnj,xnk,  -1,' ',-1,ip2(i),
     %                            -1,typv_S(i),Cfld_list_S(i) )
!
                    if ( xncle < 0 ) then
                      if (Lun_out > 0) write( Lun_out,1005 ) Cfld_list_S
     %(i)
                      goto 0040
                    endif
                    if(xni==2 .and. xnj==2 .and. xnk==3)then   
                      xnerr = fstfrm(Lun_clim)                 
                      xnerr = fclos(Lun_clim)
                      call sleep_a_bit(SLEEP_PERIOD)                   
         write( Lun_out,*)'INFO: reopening '//trim(clima_S)
                      xnerr = fnom( Lun_clim, clima_S ,
     %                        'STD+RND+OLD+R/O', 0 )   
                      xnerr = fstouv( Lun_clim, 'RND' )
                    endif
                 enddo
!
               end if
!
!
!*             Save the grid description parameters
               xnerr = fstprm( xncle, xnhold,xnhold,xnhold,
     %                         xnilu(i),xnjlu(i),xnk,
     %                         xnhold,xnhold,xnhold,xnhold,xnhold,
     %                         hold_S,hold_S,hold_S,
     %                         gtyp_S(i),xnig1(i),xnig2(i),
     %                                   xnig3(i),xnig4(i),
     %                         xnhold,xnhold,xnhold,xnhold,
     %                         xnhold,xnhold,xnhold )
!
!*             Retreive necessary Z-grid descriptors
               if (gtyp_S(i) == 'Z') then
!
                  if ( acli_L(i) ) then
                     Lun_activ = Lun_acli
                  else
                     Lun_activ = Lun_clim
                  endif
!
                  xncle = fstlir( xrtic(1,i), Lun_activ,
     %                            xniz,xnjz,xnhold, -1,' ',
     %                            xnig1(i),xnig2(i),xnig3(i),
     %                            ' ','>>')
!
                  if ( xncle < 0 .or.
     %                 xniz /= xnilu(i) ) then
                     write( Lun_out,1006 ) Lun_activ,xnig1(i),xnig2(i)
                     xncle = -1
                     goto 0040
                  endif
!                       
                  xncle = fstlir( xrtac(1,i), Lun_activ,
     %                            xniz,xnjz,xnhold, -1,' ',
     %                            xnig1(i),xnig2(i),xnig3(i),
     %                            ' ','^^')
!
                  if ( xncle < 0 .or.
     %                 xnjz /= xnjlu(i) ) then
                     write( Lun_out,1007 ) Lun_activ,xnig1(i),xnig2(i)
                     xncle = -1
                     goto 0040
                  endif
!
                  xnerr = fstprm( xncle, xnhold,xnhold,xnhold,
     %                            xnhold,xnhold,xnhold,
     %                            xnhold,xnhold,xnhold,xnhold,xnhold,
     %                            hold_S,hold_S,hold_S,
     %                            gtypz_S(i),xnzig1(i),xnzig2(i),
     %                                       xnzig3(i),xnzig4(i),
     %                            xnhold,xnhold,xnhold,xnhold,
     %                            xnhold,xnhold,xnhold )
!              end retreive necessary Z-grid descriptors
               endif
!
            end if
!
         End Do LOOP_OVER_ALL_VARIABLES_2
!
         xnerr = fstopc( 'MSGLVL','SYSTEM', 0 )
         xnerr = fstopc( 'TOLRNC','SYSTEM', 0 )
!
         if ( open_acli_L ) then
            xnerr = fstfrm( Lun_acli )
            xnerr = fclos ( Lun_acli )
         end if
         if ( open_clim_L ) then
            xnerr = fstfrm( Lun_clim )
            xnerr = fclos ( Lun_clim )
         end if
      end if PTOPO_MYPROC_0_4
!
!
!     Broadcast a few variables
 0040 continue
      if ( Ptopo_myproc == 0 ) then
         xnerr = create_flag_file("itf_phy_iniincr"//achar(0))      
      else
         xnerr =  wait_for_flag_file("itf_phy_iniincr"//achar(0))   
      endif
      call rpn_comm_Barrier("grid", xnerr)
      if ( Ptopo_myproc == 0 ) then
          xnerr = remove_flag_file("itf_phy_iniincr"//achar(0))     
      endif
      call RPN_COMM_bcast( xncle  ,1               ,"MPI_INTEGER",0,"gri
     %d",xnerr)
      if (xncle < 0) call gem_stop( 'itf_phy_inincr',-7 )
!
      call RPN_COMM_bcast (xrtic  ,Cfld_nbr*xnzm_max   ,"MPI_REAL" ,0,"g
     %rid",xnerr)
      call RPN_COMM_bcast (xrtac  ,Cfld_nbr*xnzm_max   ,"MPI_REAL" ,0,"g
     %rid",xnerr)
      call RPN_COMM_bcast (xrclim ,Cfld_nbr*xnmxfld_max,"MPI_REAL" ,0,"g
     %rid",xnerr)
      call RPN_COMM_bcast (xnilu  ,Cfld_nbr    ,"MPI_INTEGER"  ,0,"grid"
     %,xnerr)
      call RPN_COMM_bcast (xnjlu  ,Cfld_nbr    ,"MPI_INTEGER"  ,0,"grid"
     %,xnerr)
      call RPN_COMM_bcast (xnig1  ,Cfld_nbr    ,"MPI_INTEGER"  ,0,"grid"
     %,xnerr)
      call RPN_COMM_bcast (xnig2  ,Cfld_nbr    ,"MPI_INTEGER"  ,0,"grid"
     %,xnerr)
      call RPN_COMM_bcast (xnig3  ,Cfld_nbr    ,"MPI_INTEGER"  ,0,"grid"
     %,xnerr)
      call RPN_COMM_bcast (xnig4  ,Cfld_nbr    ,"MPI_INTEGER"  ,0,"grid"
     %,xnerr)
      call RPN_COMM_bcastc(gtyp_S ,Cfld_nbr    ,"MPI_CHARACTER",0,"grid"
     %,xnerr)
      call RPN_COMM_bcast (xnzig1 ,Cfld_nbr    ,"MPI_INTEGER"  ,0,"grid"
     %,xnerr)
      call RPN_COMM_bcast (xnzig2 ,Cfld_nbr    ,"MPI_INTEGER"  ,0,"grid"
     %,xnerr)
      call RPN_COMM_bcast (xnzig3 ,Cfld_nbr    ,"MPI_INTEGER"  ,0,"grid"
     %,xnerr)
      call RPN_COMM_bcast (xnzig4 ,Cfld_nbr    ,"MPI_INTEGER"  ,0,"grid"
     %,xnerr)
      call RPN_COMM_bcastc(gtypz_S,Cfld_nbr    ,"MPI_CHARACTER",0,"grid"
     %,xnerr)
      call RPN_COMM_bcastc(Cfld_list_S,Cfld_nbr*4,"MPI_CHARACTER",0,"gri
     %d",xnerr)
!
!
!*       -------------------------------------------------
!*    6. Interpolate what was just read to the local grid
!*       -------------------------------------------------
!
!*    Start by defining the local grid
!
      offi = Ptopo_gindx(1,Ptopo_myproc+1)-1
      offj = Ptopo_gindx(3,Ptopo_myproc+1)-1
      Do i=1,l_ni
         indx = offi + i
         lxfi(i) = G_xg_8(indx)*rad_8
      End Do
      Do i=1,l_nj
         indx = offj + i
         lyfi(i) = G_yg_8(indx)*rad_8
      End Do
!
      dgid  = ezgdef_fmem( l_ni , l_nj , 'Z', 'E',
     %         Hgc_ig1ro, Hgc_ig2ro, Hgc_ig3ro, Hgc_ig4ro,
     %         lxfi , lyfi )
!
      INTERPOLATE_TO_LOCAL_GRID : Do x=1,Cfld_nbr
!        Only interpolate if new date got read
         If ( Done_Inincr(x) == Do_Inincr(x)
     %        .and. Lctl_step .ne. 1 ) cycle
!
!        Only interpolate increment if needed
         if ( Cfld_list_S(x) == 'SD' .and. IntIce_L .and. IntSol_L ) cyc
     %le
         if ( Cfld_list_S(x) == 'HS' .and. IntSol_L ) cycle
         if ( Cfld_list_S(x) == 'I8' .and. IntIce_L ) cycle
         If ( gtyp_S(x) == 'Z' ) then
            sgid = ezgdef_fmem( xnilu(x), xnjlu(x), gtyp_S(x),
     %              gtypz_S(x), xnzig1(x),xnzig2(x),xnzig3(x),xnzig4(x),
     %              xrtic(1,x), xrtac(1,x) )
         Else
            sgid = ezgdef_fmem( xnilu(x), xnjlu(x), gtyp_S(x),
     %              Vide_S,     xnig1(x),xnig2(x),xnig3(x),xnig4(x),
     %              Aucun,Aucun )
         End If
!
         xnerr = ezdefset( dgid, sgid )
         xnerr = ezsetopt('INTERP_DEGREE', Cfld_interp_S(x) )
!
         xnerr = ezsint( xrwork(1,1,x), xrclim(1,x) )
!            
!*       And also calculate scaling factors scal_8
         date1 =      Done_Inincr(x) / factor6
         time1 = mod( Done_Inincr(x) , factor6 )*100
         date2 =      Do_Inincr(x)   / factor6
         time2 = mod( Do_Inincr(x)   , factor6 )*100
         xnerr = newdate( stamp1, date1, time1, 3 )
         xnerr = newdate( stamp2, date2, time2, 3 )
         call difdatr( stamp2,stamp1,hour_8 )
         scal_8(x) = 1./(hour_8*(3600./Cstv_dt_8))
!
      End Do INTERPOLATE_TO_LOCAL_GRID
!
!*       -------------------------------------------------
!*    7. We can now update the increments
!*       -------------------------------------------------
!
!*    Loop on the p_nj physics slabs.
!*    The increments are updated in place
      LOOP_ON_THE_P_NJ_PHYSICS_SLABS : do xncoupe=1,p_nj
!
         lowb = (xncoupe-1)*p_bper_siz+1
         upb  =  xncoupe   *p_bper_siz
         busper => Phy_busper3D( lowb : upb )
!
         j = xncoupe+p_offj
!
!        Loop over all variables
         LOOP_OVER_ALL_VARIABLES_3 : do x=1,Cfld_nbr
!
!           Only update increment if new date got read
            if ( Done_Inincr(x) == Do_Inincr(x)
     %           .and. Lctl_step .ne. 1 ) cycle
!
!           Only update increment if needed
            if ( Cfld_list_S(x) == 'SD' .and. IntIce_L .and. IntSol_L ) 
     %cycle
            if ( Cfld_list_S(x) == 'HS' .and. IntSol_L ) cycle
            if ( Cfld_list_S(x) == 'I8' .and. IntIce_L ) cycle
!
            ik = 0
!
!*          Treat each variable different
            select case ( Cfld_list_S(x) )
!
!*          Snow depth. It is used over glaciers and sea-ice when
!*          ICEMELT is false. Soil uses it only with the Force-restore
!*          scheme. Either the maximum GLACIER and GLSEA snow depth (which
!*          are currently in the second and fourth rows) or the soil snow
!*          depth is used here.
            case ( 'SD' )
!
               if (.not.IntIce_L) then
                  do i=1,p_ni
                     neige             = max( busper(snodp+  p_ni+ik) ,
     %                                        busper(snodp+3*p_ni+ik) )
                     busper(incrne+ik) =
     %              (xrwork(p_offi+i,j,x) - neige) * scal_8(x)
                     xrwork(p_offi+i,j,x) = busper(incrne+ik)
                     ik = ik+1
                  enddo
               else
                  do i=1,p_ni
                     busper(incrne+ik) =
     %              (xrwork(p_offi+i,j,x) - busper(snodp +ik)) * scal_8(
     %x)
                     xrwork(p_offi+i,j,x) = busper(incrne+ik)
                     ik = ik+1
                  enddo
               endif
!
!
!*          Soil moisture (again, only for Force-restore) ...
            case ( 'HS' )
!
               do i=1,p_ni
                  busper(incrhs+ik) =
     %           (xrwork(p_offi+i,j,x) - busper(wsoil +ik)) * scal_8(x)
                  xrwork(p_offi+i,j,x) = busper(incrhs+ik)
                  ik = ik+1
               enddo
!
!
!*          Sea ice thickness (if ICEMELT is false) ...
            case ( 'I8' )
!
               do i=1,p_ni
                  busper(incricd+ik) =
     %           (xrwork(p_offi+i,j,x) - busper(icedp +ik)) * scal_8(x)
                  xrwork(p_offi+i,j,x) = busper(incricd+ik)
                  ik = ik+1
               enddo
!
!
!*          Sea ice mask
            case ( 'LG' )
!
               do i=1,p_ni
                  busper(incrgl+ik) =
     %           (xrwork(p_offi+i,j,x) - busper(glsea0+ik)) * scal_8(x)
                  xrwork(p_offi+i,j,x) = busper(incrgl+ik)
                  ik = ik+1
               enddo
!
!
!*          Define the two temperature increments
!*          Busp_incrts is used for SSTs, Busp_incrtg is used in the
!*          second layer Continental Ice temperature and Busp_incrtp
!*          is used with the Force-Restore.
!
!
!*          TP will be used by Force-restore 
!*          and by the glacier module.
            case ( 'TP' )
!
               do i=1,p_ni
                  
                  xrwork(p_offi+i,j,x) = xrwork(p_offi+i,j,x) + Dcst_tcd
     %k_8
!
                  busper(incrtg+ik) = (min( Dcst_trpl_8, 1.0_8*
     %            xrwork(p_offi+i,j,x)) - busper(tglacier+p_ni+ik)) * sc
     %al_8(x)
                  busper(incrtp+ik) =
     %           (xrwork(p_offi+i,j,x)  - busper(tsoil   +p_ni+ik)) * sc
     %al_8(x)
                  xrwork(p_offi+i,j,x)  = busper(incrtp       +ik)
                  ik = ik+1
!
               enddo
!
!
!*          The sea surface temperature TM
            case ( 'TM' )
!
               do i=1,p_ni
               
                  xrwork(p_offi+i,j,x) = xrwork(p_offi+i,j,x) + Dcst_tcd
     %k_8
!
                  busper(incrts+ik) =
     %           (xrwork(p_offi+i,j,x) - busper(twater+ik)) * scal_8(x)
                  xrwork(p_offi+i,j,x) = busper(incrts+ik)
                  ik = ik+1
               enddo
!
            end select
!
!
!
         end do LOOP_OVER_ALL_VARIABLES_3
!
      end do LOOP_ON_THE_P_NJ_PHYSICS_SLABS
!
!
!*       -------------------------------------------------
!*    8. Optional diagnostics of the increments
!*       -------------------------------------------------
!
      INCREMENT_DIAGNOSTICS : If (Lctl_debug_L .or. VRAI) then
!
         if (G_lam) then
            gii = Glb_pil_e - 2 ; gif = G_ni - Glb_pil_w + 3
            gji = Glb_pil_s - 2 ; gjf = G_nj - Glb_pil_n + 3
         else
            gii = 1 ; gif = G_ni
            gji = 1 ; gjf = G_nj
         endif
!
         Do x=1,Cfld_nbr
!           Only do diagnostics if new date got read
            If ( Done_Inincr(x) == Do_Inincr(x)
     %           .and. Lctl_step /= 1 ) cycle
!
!           Only update increment if needed
            if ( Cfld_list_S(x) == 'SD' .and. IntIce_L .and. IntSol_L ) 
     %cycle
            if ( Cfld_list_S(x) == 'HS' .and. IntSol_L ) cycle
            if ( Cfld_list_S(x) == 'I8' .and. IntIce_L ) cycle
!
            call statf_dm( xrwork(1,1,x),Cfld_list_S(x),k+1,
     %       'INCREMENTS',VRAI, 1,l_ni,1,l_nj,1, gii,gji,1, gif,gjf, 1 )
!
         End Do
!
      End If INCREMENT_DIAGNOSTICS
!
!*    De-allocate working memory
      deallocate (xrclim, xrwork, xrtic, xrtac)
!
!
!*    Set flag that new date got read
      do x=1,Cfld_nbr
         Done_Inincr(x) = Do_Inincr(x)
!*       Code date from 'Done_Inincr' in 'Cfld_date' to write in restart file
         date1 =      Done_Inincr(x)/factor6
         time1 = mod( Done_Inincr(x),factor6  ) * 100
         xnerr = newdate( Cfld_date(x), date1, time1, 3 )
      end do
!
!*    De-allocate variable containing all dates in imanclim
!*    at the end of a job
      if ( Lctl_step == Step_total ) deallocate( dates )
!
!
 1001 format(' Unable to open climatological file ',A)
 1002 format(' Analysed/climatological file not opened')
 1004 format(' Climatological ',A4,' will not be read')
 1005 format(' Unable to read climatological ',A4)
 1006 format(' Unable to read >> record on unit ',I3,' for IP1,IP2= ',2I
     %10)
 1007 format(' Unable to read ^^ record on unit ',I3,' for IP1,IP2= ',2I
     %10)
 1009 format(' Date ',I12,' not found in analysed file')
!
      return
      end
