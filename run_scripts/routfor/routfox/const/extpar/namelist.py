'''
This is a template for the namelist -> rename to namelist.py for use

Make sure, the namelist.py is contained in the run-directory of Extpar.
All Python scripts use the same namelist.py file, but use different
dictionaries:

    -extpar_alb_to_buffer.py:     input_alb

    -extpar_cru_to_buffer.py:     input_tclim

    -extpar_emiss_to_buffer.py:   input_emiss

    -extpar_ndvi_to_buffer.py:    input_ndvi

    -extpar_era_to_buffer.py:     input_era

    -extpar_ahf_to_buffer.py:     input_ahf

    -extpar_isa_to_buffer.py:     input_isa
'''

input_tclim = {
    'raw_data_t_clim_path': '/hpc/rhome/routfor/routfox/extpar/rawdata/',
    'raw_data_tclim_coarse': 'absolute_hadcrut3.nc',
    'raw_data_tclim_fine': 'CRU_T2M_SURF_clim.nc',
    't_clim_buffer_file': 'cru_buffer.nc',
    'it_cl_type': 2
}

input_alb = {
    'ialb_type': 1,
    'raw_data_alb_path': '/hpc/rhome/routfor/routfox/extpar/rawdata/',
    'raw_data_alb_filename': 'month_alb_new.nc',
    'raw_data_alnid_filename': 'month_alnid_new.nc',
    'raw_data_aluvd_filename': 'month_aluvd_new.nc',
    'alb_buffer_file': 'albedo_buffer.nc',
    'alb_output_file': 'albedo_cosmo.nc',
}

input_emiss = {
    'iemiss_type': 1,
    'raw_data_emiss_path': '/hpc/rhome/routfor/routfox/extpar/rawdata/',
    'raw_data_emiss_filename': 'CAM_bbe_int_2010-2015_full.nc',
    'emiss_buffer_file': 'emiss_buffer.nc',
    'emiss_output_file': 'emiss_icon.nc'
}

input_ndvi = {
    'raw_data_ndvi_path': '/hpc/rhome/routfor/routfox/extpar/rawdata/',
    'raw_data_ndvi_filename': 'NDVI_1998_2003.nc',
    'ndvi_buffer_file': 'ndvi_buffer.nc',
    'ndvi_output_file': 'ndvi_extpar_cosmo.nc'
}

input_era = {
    'iera_type': 2,
    'raw_data_era_path': './',
    'raw_data_era_ORO': 'ea_oro_1986.nc',
    'raw_data_era_T2M': 'ei_an1986-2015_mean_2t.nc',
    'raw_data_era_SST': 'ei_an1986-2015_mean_sst.nc',
    'raw_data_era_SD': 'ei_an1986-2015_mean_sd.nc',
    'era_buffer_file': 'era_buffer.nc',
}

input_ahf = {
    'raw_data_ahf_path': '',
    'raw_data_ahf_filename': 'AHF_2006_CDO.nc',
    'ahf_buffer_file': 'AHF_BUFFER.nc',
}

input_isa = {
    'raw_data_isa_path': '',
    'raw_data_isa_filename': 'NOAA_ISA_CDO.nc',
    'isa_buffer_file': 'ISA_BUFFER.nc',
}
