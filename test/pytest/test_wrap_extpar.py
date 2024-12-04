from extpar.WrapExtpar import *
import os
import pytest


def test_setup_flake_namelist():
    args = {'raw_data_path': '/path/to/raw/data'}
    expected_namelist = {
        'raw_data_flake_path': '/path/to/raw/data',
        'raw_data_flake_filename': 'GLDB_lakedepth.nc',
        'flake_buffer_file': 'flake_buffer.nc'
    }

    result = setup_flake_namelist(args)

    assert result == expected_namelist


def test_setup_tclim_namelist():
    args = {'raw_data_path': '/path/to/raw/data'}
    expected_output = {
        'it_cl_type': 1,
        'raw_data_t_clim_path': '/path/to/raw/data',
        'raw_data_tclim_coarse': 'absolute_hadcrut3.nc',
        'raw_data_tclim_fine': 'CRU_T_SOIL_clim.nc',
        't_clim_buffer_file': 'tclim_buffer.nc'
    }

    result = setup_tclim_namelist(args)
    assert result == expected_output


def test_setup_albedo_namelist_type_1():
    args = {'raw_data_path': '/path/to/data', 'ialb_type': 1}
    expected_namelist = {
        'raw_data_alb_path': '/path/to/data',
        'ialb_type': 1,
        'alb_buffer_file': 'alb_buffer.nc',
        'raw_data_alb_filename': 'alb_new.nc',
        'raw_data_alnid_filename': 'alnid_new.nc',
        'raw_data_aluvd_filename': 'aluvd_new.nc'
    }
    assert setup_albedo_namelist(args) == expected_namelist


def test_setup_albedo_namelist_type_2():
    args = {'raw_data_path': '/path/to/data', 'ialb_type': 2}
    expected_namelist = {
        'raw_data_alb_path': '/path/to/data',
        'ialb_type': 2,
        'alb_buffer_file': 'alb_buffer.nc',
        'raw_data_alb_filename': 'global_soil_albedo.nc'
    }
    assert setup_albedo_namelist(args) == expected_namelist


def test_setup_albedo_namelist_type_3():
    args = {'raw_data_path': '/path/to/data', 'ialb_type': 3}
    expected_namelist = {
        'raw_data_alb_path': '/path/to/data',
        'ialb_type': 3,
        'alb_buffer_file': 'alb_buffer.nc',
        'raw_data_alb_filename': 'alb_new.nc'
    }
    assert setup_albedo_namelist(args) == expected_namelist


def test_setup_ndvi_namelist():
    args = {'raw_data_path': '/path/to/data'}
    expected_namelist = {
        'raw_data_ndvi_path': '/path/to/data',
        'raw_data_ndvi_filename': 'NDVI_1998_2003.nc',
        'ndvi_buffer_file': 'ndvi_buffer.nc'
    }
    assert setup_ndvi_namelist(args) == expected_namelist


def test_setup_urban_namelist():
    args = {'raw_data_path': '/path/to/data'}
    expected_namelist = {
        'iahf_type': 1,
        'raw_data_ahf_path': '/path/to/data',
        'raw_data_ahf_filename': 'AHF_2006_2.5min_lonlat.nc',
        'ahf_buffer_file': 'ahf_buffer.nc',
        'isa_type': 1,
        'raw_data_isa_path': '/path/to/data',
        'raw_data_isa_filename': 'NOAA_ISA_16bit_lonlat.nc',
        'isa_buffer_file': 'isa_buffer.nc'
    }
    assert setup_urban_namelist(args) == expected_namelist


def test_setup_check_namelist():
    args = {}
    expected_namelist = {
        'netcdf_output_filename': 'external_parameter.nc',
        'i_lsm_data': 1,
        'land_sea_mask_file': "",
        'number_special_points': 0,
        'lflake_correction': ".TRUE."
    }
    assert setup_check_namelist(args) == expected_namelist


def test_setup_lu_namelist_type_1():
    args = {'ilu_type': 1, 'raw_data_path': '/path/to/data'}
    expected_namelist = {
        'i_landuse_data':
        1,
        'ilookup_table_lu':
        1,
        'raw_data_lu_path':
        '/path/to/data',
        'raw_data_glcc_path':
        '/path/to/data',
        'lu_buffer_file':
        'lu_buffer.nc',
        'raw_data_glcc_filename':
        'GLCC_usgs_class_byte.nc',
        'glcc_buffer_file':
        'glcc_buffer.nc',
        'l_use_corine':
        ".FALSE.",
        'raw_data_lu_filename': [
            "'GLOBCOVER_0_16bit.nc' ", "'GLOBCOVER_1_16bit.nc' ",
            "'GLOBCOVER_2_16bit.nc' ", "'GLOBCOVER_3_16bit.nc' ",
            "'GLOBCOVER_4_16bit.nc' ", "'GLOBCOVER_5_16bit.nc' "
        ]
    }
    assert setup_lu_namelist(args) == expected_namelist


def test_setup_lu_namelist_type_2():
    args = {'ilu_type': 2, 'raw_data_path': '/path/to/data'}
    expected_namelist = {
        'i_landuse_data': 2,
        'ilookup_table_lu': 2,
        'raw_data_lu_path': '/path/to/data',
        'raw_data_glcc_path': '/path/to/data',
        'lu_buffer_file': 'lu_buffer.nc',
        'raw_data_glcc_filename': 'GLCC_usgs_class_byte.nc',
        'glcc_buffer_file': 'glcc_buffer.nc',
        'l_use_corine': ".FALSE.",
        'raw_data_lu_filename': "'GLC2000_byte.nc'"
    }
    assert setup_lu_namelist(args) == expected_namelist


def test_setup_aot_namelist_type_1():
    args = {'iaot_type': 1, 'raw_data_path': '/path/to/data'}
    expected_namelist = {
        'iaot_type': 1,
        'raw_data_aot_path': '/path/to/data',
        'aot_buffer_file': 'aot_buffer.nc',
        'raw_data_aot_filename': 'aot_GACP.nc'
    }
    assert setup_aot_namelist(args) == expected_namelist


def test_setup_aot_namelist_type_2():
    args = {'iaot_type': 2, 'raw_data_path': '/path/to/data'}
    expected_namelist = {
        'iaot_type': 2,
        'raw_data_aot_path': '/path/to/data',
        'aot_buffer_file': 'aot_buffer.nc',
        'raw_data_aot_filename': 'aod_AeroCom1.nc'
    }
    assert setup_aot_namelist(args) == expected_namelist


def test_setup_aot_namelist_type_5():
    args = {'iaot_type': 5, 'raw_data_path': '/path/to/data'}
    expected_namelist = {
        'iaot_type': 5,
        'raw_data_aot_path': '/path/to/data',
        'aot_buffer_file': 'aot_buffer.nc',
        'raw_data_aot_filename': 'aot_CAMS_2003-2013.nc'
    }
    assert setup_aot_namelist(args) == expected_namelist


def test_generate_globe_filenames():
    expected_filenames = [
        "'GLOBE_A10.nc' ", "'GLOBE_B10.nc' ", "'GLOBE_C10.nc' ",
        "'GLOBE_D10.nc' ", "'GLOBE_E10.nc' ", "'GLOBE_F10.nc' ",
        "'GLOBE_G10.nc' ", "'GLOBE_H10.nc' ", "'GLOBE_I10.nc' ",
        "'GLOBE_J10.nc' ", "'GLOBE_K10.nc' ", "'GLOBE_L10.nc' ",
        "'GLOBE_M10.nc' ", "'GLOBE_N10.nc' ", "'GLOBE_O10.nc' ",
        "'GLOBE_P10.nc' "
    ]
    assert generate_globe_filenames() == expected_filenames


def test_setup_runscript_with_urban_cosmo():
    args = {'account': 'test_account', 'lurban': True, 'igrid_type': 2}
    expected_runscript = {
        'account':
        'test_account',
        'pythonpath':
        os.path.join(os.getcwd(), 'lib'),
        'extpar_executables': [
            '"extpar_landuse_to_buffer.exe" ', '"extpar_topo_to_buffer.exe" ',
            '"extpar_cru_to_buffer.py" ', '"extpar_aot_to_buffer.exe" ',
            '"extpar_flake_to_buffer.exe" ', '"extpar_soil_to_buffer.exe" ',
            '"extpar_alb_to_buffer.py" ', '"extpar_ndvi_to_buffer.py" ',
            '"extpar_ahf_to_buffer.py" ', '"extpar_isa_to_buffer.py" ',
            '"extpar_consistency_check.exe" '
        ]
    }
    assert setup_runscript(args) == expected_runscript


def test_setup_runscript_without_urban_icon():
    args = {'account': 'test_account', 'lurban': False, 'igrid_type': 1}
    expected_runscript = {
        'account':
        'test_account',
        'pythonpath':
        os.path.join(os.getcwd(), 'lib'),
        'extpar_executables': [
            '"extpar_landuse_to_buffer.exe" ', '"extpar_topo_to_buffer.exe" ',
            '"extpar_cru_to_buffer.py" ', '"extpar_aot_to_buffer.exe" ',
            '"extpar_flake_to_buffer.exe" ', '"extpar_soil_to_buffer.exe" ',
            '"extpar_alb_to_buffer.py" ', '"extpar_ndvi_to_buffer.py" ',
            '"extpar_era_to_buffer.py" ', '"extpar_consistency_check.exe" '
        ]
    }
    assert setup_runscript(args) == expected_runscript


def test_compute_aster_tiles_1_1():
    lonmax = 30.0
    lonmin = 29.0
    latmax = 30.0
    latmin = 31.0
    lsgsl = False

    expected_namelist = {
        'ntiles_column': 1,
        'ntiles_row': 1,
        'topo_files': ["'ASTER_orig_T055.nc' "],
    }
    assert compute_aster_tiles(lonmax, lonmin, latmax, latmin,
                               lsgsl) == expected_namelist


def test_compute_aster_tiles_2_1():
    lonmax = 40.0
    lonmin = 29.0
    latmax = 30.0
    latmin = 31.0
    lsgsl = False

    expected_namelist = {
        'ntiles_column': 2,
        'ntiles_row': 1,
        'topo_files': ["'ASTER_orig_T055.nc' ", "'ASTER_orig_T056.nc' "],
    }
    assert compute_aster_tiles(lonmax, lonmin, latmax, latmin,
                               lsgsl) == expected_namelist


def test_compute_aster_tiles_2_1_lsgsl():
    lonmax = 40.0
    lonmin = 29.0
    latmax = 30.0
    latmin = 31.0
    lsgsl = True

    expected_namelist = {
        'ntiles_column': 2,
        'ntiles_row': 1,
        'topo_files': ["'ASTER_orig_T055.nc' ", "'ASTER_orig_T056.nc' "],
        'sgsl_files': ["'S_ORO_T055.nc' ", "'S_ORO_T056.nc' "],
        'lpreproc_oro': '.FALSE.',
    }
    assert compute_aster_tiles(lonmax, lonmin, latmax, latmin,
                               lsgsl) == expected_namelist


def test_compute_aster_tiles_2_2():
    lonmax = 40.0
    lonmin = 29.0
    latmax = 40.0
    latmin = 31.0
    lsgsl = False

    expected_namelist = {
        'ntiles_column':
        2,
        'ntiles_row':
        2,
        'topo_files': [
            "'ASTER_orig_T043.nc' ", "'ASTER_orig_T044.nc' ",
            "'ASTER_orig_T055.nc' ", "'ASTER_orig_T056.nc' "
        ],
    }
    assert compute_aster_tiles(lonmax, lonmin, latmax, latmin,
                               lsgsl) == expected_namelist


def test_compute_aster_tiles_12_20():
    lonmax = 180.0
    lonmin = -180.0
    latmax = 60.0
    latmin = -60.0
    lsgsl = False

    expected_namelist = {
        'ntiles_column':
        12,
        'ntiles_row':
        20,
        'topo_files':
        [f"'ASTER_orig_T{number:03d}.nc' " for number in range(1, 241)]
    }
    assert compute_aster_tiles(lonmax, lonmin, latmax, latmin,
                               lsgsl) == expected_namelist


def test_compute_merit_tiles_1_1():
    lonmax = 30.0
    lonmin = 29.0
    latmax = 30.0
    latmin = 31.0
    lsgsl = False

    expected_namelist = {
        'ntiles_column': 1,
        'ntiles_row': 1,
        'topo_files': ["'MERIT_N60-N30_E000-E030.nc' "],
    }
    assert compute_merit_tiles(lonmax, lonmin, latmax, latmin,
                               lsgsl) == expected_namelist


def test_compute_merit_tiles_2_1():
    lonmax = 30.0
    lonmin = -10.0
    latmax = 30.0
    latmin = 31.0
    lsgsl = False

    expected_namelist = {
        'ntiles_column':
        2,
        'ntiles_row':
        1,
        'topo_files':
        ["'MERIT_N60-N30_W030-E000.nc' ", "'MERIT_N60-N30_E000-E030.nc' "],
    }
    assert compute_merit_tiles(lonmax, lonmin, latmax, latmin,
                               lsgsl) == expected_namelist


def test_compute_merit_tiles_2_1_sgsl():
    lonmax = 30.0
    lonmin = -10.0
    latmax = 30.0
    latmin = 31.0
    lsgsl = True

    expected_namelist = {
        'ntiles_column':
        2,
        'ntiles_row':
        1,
        'topo_files':
        ["'MERIT_N60-N30_W030-E000.nc' ", "'MERIT_N60-N30_E000-E030.nc' "],
        'sgsl_files': ["'S_ORO_0' ", "'S_ORO_1' "],
        'lpreproc_oro':
        '.TRUE.',
    }
    assert compute_merit_tiles(lonmax, lonmin, latmax, latmin,
                               lsgsl) == expected_namelist


def test_compute_merit_tiles_2_3():
    lonmax = 30.0
    lonmin = -10.0
    latmax = 60.0
    latmin = 31.0
    lsgsl = False

    expected_namelist = {
        'ntiles_column':
        2,
        'ntiles_row':
        2,
        'topo_files': [
            "'MERIT_N90-N60_W030-E000.nc' ", "'MERIT_N90-N60_E000-E030.nc' ",
            "'MERIT_N60-N30_W030-E000.nc' ", "'MERIT_N60-N30_E000-E030.nc' "
        ],
    }
    assert compute_merit_tiles(lonmax, lonmin, latmax, latmin,
                               lsgsl) == expected_namelist


def test_setup_soil_namelist_type_1():
    args = {'isoil_type': 1, 'raw_data_path': '/path/to/raw_data'}
    expected_namelist = {
        'isoil_data': 1,
        'raw_data_soil_path': '/path/to/raw_data',
        'raw_data_soil_filename': 'FAO_DSMW_double.nc',
        'ldeep_soil': ".FALSE",
        'soil_buffer_file': 'soil_buffer.nc',
        'path_HWSD_index_files': os.path.join('/path/to/raw_data', '../soil'),
        'lookup_table_HWSD': 'LU_TAB_HWSD_UF.data',
        'HWSD_data': 'HWSD_DATA_COSMO.data',
        'HWSD_data_deep': 'HWSD_DATA_COSMO_S.data'
    }
    assert setup_soil_namelist(args) == expected_namelist


def test_setup_soil_namelist_type_2():
    args = {'isoil_type': 2, 'raw_data_path': '/path/to/raw_data'}
    expected_namelist = {
        'isoil_data': 2,
        'raw_data_soil_path': '/path/to/raw_data',
        'raw_data_soil_filename': 'HWSD0_30_topsoil.nc',
        'raw_data_deep_soil_filename': 'HWSD30_100_subsoil.nc',
        'ldeep_soil': ".TRUE",
        'soil_buffer_file': 'soil_buffer.nc',
        'path_HWSD_index_files': os.path.join('/path/to/raw_data', '../soil'),
        'lookup_table_HWSD': 'LU_TAB_HWSD_UF.data',
        'HWSD_data': 'HWSD_DATA_COSMO.data',
        'HWSD_data_deep': 'HWSD_DATA_COSMO_S.data'
    }
    assert setup_soil_namelist(args) == expected_namelist


def test_setup_soil_namelist_type_3():
    args = {'isoil_type': 3, 'raw_data_path': '/path/to/raw_data'}
    expected_namelist = {
        'isoil_data': 3,
        'raw_data_soil_path': '/path/to/raw_data',
        'raw_data_soil_filename': 'HWSD0_30_topsoil.nc',
        'raw_data_deep_soil_filename': 'HWSD30_100_subsoil.nc',
        'ldeep_soil': ".FALSE",
        'soil_buffer_file': 'soil_buffer.nc',
        'path_HWSD_index_files': os.path.join('/path/to/raw_data', '../soil'),
        'lookup_table_HWSD': 'LU_TAB_HWSD_UF.data',
        'HWSD_data': 'HWSD_DATA_COSMO.data',
        'HWSD_data_deep': 'HWSD_DATA_COSMO_S.data'
    }
    assert setup_soil_namelist(args) == expected_namelist


def test_setup_soil_namelist_unknown_type():
    args = {'isoil_type': 99, 'raw_data_path': '/path/to/raw_data'}
    with pytest.raises(ValueError, match='Unknown isoil_type 99'):
        setup_soil_namelist(args)
