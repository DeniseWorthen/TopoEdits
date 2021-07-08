module charstrings

  implicit none

#if defined output_grid_3deg || defined output_grid_072deg || defined output_grid_twelfdeg
!set temporary locations
# ifdef output_grid_3deg
  character(len=256) :: dirsrc = &
   '/scratch2/NCEPDEV/climate/Denise.Worthen/Huiskamp/INPUT/'
# endif
# ifdef output_grid_072deg
  character(len=256) :: dirsrc = &
   '/scratch2/NCEPDEV/climate/Denise.Worthen/Hae-Cheol/INPUT/'
# endif
# ifdef output_grid_twelfdeg
  character(len=256) :: dirsrc = &
   '/scratch2/NCEPDEV/climate/Denise.Worthen/Hae-Cheol/INPUT/'
# endif
#else
  character(len=256) :: dirsrc = &
   '/scratch2/NCEPDEV/climate/climpara/S2S/FIX/fix_UFSp4/fix_mom6/'
#endif

#ifdef output_grid_qdeg
  character(len= 10) :: res = '025'
  character(len=100) :: bathfile = 'ocean_topog'
  character(len=100) :: topoeditfile = 'All_edits.nc'
#endif
#ifdef output_grid_hdeg
  character(len= 10) :: res = '050'
  character(len=100) :: bathfile = 'ocean_topog'
  character(len=100) :: topoeditfile = ''
#endif
#ifdef output_grid_1deg
  character(len= 10) :: res = '100'
  character(len=100) :: bathfile = 'topog'
  character(len=100) :: topoeditfile = 'topo_edits_011818.nc'
#endif
#ifdef output_grid_072deg
  character(len= 10) :: res = '072'
#endif
#ifdef output_grid_3deg
  character(len= 10) :: res = '300'
#endif
#ifdef output_grid_twelfdeg
  character(len= 10) :: res = '008'
#endif

  character(len=100) :: maskfile = 'ocean_mask'
  character(len= 12) :: maskname = 'mask'
  character(len= 12) :: bathname = 'depth'

  !character(len=256) :: dirout = '/scratch2/NCEPDEV/climate/Denise.Worthen/grids-20210223/'
  character(len=256) :: dirout = '/scratch2/NCEPDEV/climate/Denise.Worthen/GTMP/'
  character(len=256) :: history
  character(len=  8) :: cdate

end module charstrings
