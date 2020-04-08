raw_data_path = '' 
raw_data_tclim_coarse = 'absolute_hadcrut3.nc'
raw_data_tclim_fine = 'CRU_T2M_SURF_clim.nc'
buffer_tclim = 'CRU_BUFFER.nc'
external_topo = 'topography_ICON.nc'

# 1: use fine 
# 2: use fine over land, coarse over sea, correct with better
# topography from extpar_topo_to_buffer
itype_cru = 1
