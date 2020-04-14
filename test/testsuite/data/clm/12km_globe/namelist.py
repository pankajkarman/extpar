input_tclim = {
        'raw_data_path': '',
        'raw_data_tclim_coarse': 'absolute_hadcrut3.nc',
        'raw_data_tclim_fine': 'CRU_T2M_SURF_clim.nc',
        'buffer_tclim': 'CRU_BUFFER',
        'it_cl_type': 2
        }

input_alb = {
        'ialb_type': 2,
        'raw_data_alb_path': '',
        'raw_data_alb_filename': 'global_soil_albedo.nc',
        'raw_data_alnid_filename': '',
        'raw_data_aluvd_filename': '',
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
        'startlon_tot':-42.25,
        'startlat_tot':-36.52,
        'dlon':0.11,
        'dlat':0.11,
        'ie_tot':771,
        'je_tot':771
        }

input_ndvi = {
        'raw_data_ndvi_path': '',
        'raw_data_ndvi_filename': 'NDVI_1998_2003.nc',
        'ndvi_buffer_file': 'ndvi_buffer.nc',
        'ndvi_output_file': 'ndvi_extpar_cosmo.nc'
        }
