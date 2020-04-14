input_tclim = {
        'raw_data_t_clim_path': '',
        'raw_data_tclim_coarse': 'absolute_hadcrut3.nc',
        'raw_data_tclim_fine': 'CRU_T2M_SURF_clim.nc',
        't_clim_buffer_file': 'cru_buffer.nc',
        'it_cl_type': 1

input_alb = {
        'ialb_type': 1,
        'raw_data_alb_path': '',
        'raw_data_alb_filename': 'month_alb_new.nc',
        'raw_data_alnid_filename': 'month_alnid_new.nc',
        'raw_data_aluvd_filename': 'month_aluvd_new.nc',
        'alb_buffer_file': 'albedo_buffer.nc',
        'alb_output_file': 'albedo_cosmo.nc',
        'alb_source': 'al',
        'alnid_source': 'alnid',
        'aluvd_source': 'aluvd'
        }

input_grid = {
        'igrid_type': 2,
        'icon_grid': '',
        'pollon':-170.0,
        'pollat':43.0,
        'startlon_tot':-18.0,
        'startlat_tot':-12.9,
        'dlon':0.06,
        'dlat':0.06,
        'ie_tot':601,
        'je_tot':421
        }

input_ndvi = {
        'raw_data_ndvi_path': '',
        'raw_data_ndvi_filename': 'NDVI_1998_2003.nc',
        'ndvi_buffer_file': 'ndvi_buffer.nc',
        'ndvi_output_file': 'ndvi_extpar_cosmo.nc'
        }

