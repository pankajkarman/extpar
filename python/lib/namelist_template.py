'''
This is a template for the namelist -> rename to namelist.py for use

Make sure, the namelist.py is contained in the run-directory of Extpar.
All Python scripts use the same namelist.py file, but use different
dictionaries:

    -all extpar_programmes:       input_grid

    -extpar_alb_to_buffer.py:     input_alb

    -extpar_cru_to_buffer.py:     input_tclim

    -extpar_emiss_to_buffer.py:   input_emiss

    -extpar_ndvi_to_buffer.py:    input_ndvi
'''

input_tclim = {
        'raw_data_t_clim_path': '',
        'raw_data_tclim_coarse': 'absolute_hadcrut3.nc',
        'raw_data_tclim_fine': 'CRU_T2M_SURF_clim.nc',
        't_clim_buffer_file': 'cru_buffer.nc',
        'it_cl_type': 1
        }

input_alb = {
        'ialb_type': 2,
        'raw_data_alb_path': '',
        'raw_data_alb_filename': 'global_soil_albedo.nc',
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
#        'icon_grid': 'icon_grid_0013_R02B04_G.nc',
#        'pollon':-170.0,
#        'pollat':43.0,
#        'startlon_tot':-18.00,
#        'startlat_tot':-12.9,
#        'dlon':0.06,
#        'dlat':0.06,
#        'ie_tot':601,
#        'je_tot':421,
        }

input_emiss = { 
        'iemiss_type': 1, 
        'raw_data_emiss_path': '',
        'raw_data_emiss_filename': 'CAM_bbe_int_2010-2015_full.nc',
        'emiss_buffer_file': 'emiss_buffer.nc',
        'emiss_output_file': 'emiss_icon.nc'
        }

input_ndvi = {
        'raw_data_ndvi_path': '',
        'raw_data_ndvi_filename': 'NDVI_1998_2003.nc',
        'ndvi_buffer_file': 'ndvi_buffer.nc',
        'ndvi_output_file': 'ndvi_extpar_cosmo.nc'
        }
