#!/usr/bin/env python3
import logging
import os
import subprocess
import netCDF4 as nc
import numpy as np

# new imports 
import joblib
from joblib import Parallel, delayed
from joblib import dump, load
import xarray as xr
from tqdm import tqdm
from datetime import datetime
from sklearn.neighbors import BallTree

# extpar modules from lib
try:
    from extpar.lib import (
        grid_def,
        buffer,
        metadata,
        fortran_namelist,
        utilities as utils,
        environment as env,
    )
except ImportError:
    import grid_def
    import buffer
    import metadata
    import fortran_namelist
    import utilities as utils
    import environment as env
from namelist import input_hwsdART as ihwsdART

def index_from_latlon_new(lon_array, lat_array, lon_point, lat_point, metric='haversine'):
    lon_array, lat_array, lon_point, lat_point = [np.deg2rad(arr) for arr in [lon_array, lat_array, lon_point, lat_point]]
    lon_lat_array = np.column_stack((lon_array.flatten(), lat_array.flatten()))
    points = np.column_stack((lon_point, lat_point))
    indices = BallTree(lon_lat_array, metric= metric, leaf_size=3).query(points, k=1)[1].squeeze()    
    return indices

def get_index(index, lons, lats, hlon, hlat, idxs):
    xx = np.ones((hlon.size))
    idxs[index, :] = index_from_latlon_new(lons, lats, hlon, hlat[index]*xx)
    
def get_mmap(mat, filename_mmap):
    dump(mat, filename_mmap)
    mat = load(filename_mmap, mmap_mode='r+')
    return mat

def calculate_fraction(lons, lus, idxs):
    """
    lus: LU classes from HWSD data
    idxs: indices corrsponding to icon grid for each grid in HWSD data
    tg: ICON grid
    """
    luc = np.arange(-1, 14)
    fracs = np.zeros((lons.size, 15))
    unq, cnt = np.unique(idxs, return_counts=True)
    for i, lu in tqdm(enumerate(luc)):
        unq1, cnt1 = np.unique(np.where(lus==i, idxs, -1), return_counts=True)
        for cm in np.arange(lons.size):
            frac = np.array(cnt1[unq1==cm] / cnt[unq==cm]) 
            if len(frac)!=0:
                fracs[cm, i] = frac                 
    return fracs

def write_data_to_buffer(fracs, tg):
    fr_names = ['fr_hcla', 'fr0', 'fr_silc', 'fr_lcla', 'fr_sicl', 'fr_cloa', 'fr_silt', 'fr_silo', 
                'fr_scla', 'fr_loam', 'fr_sclo', 'fr_sloa', 'fr_lsan', 'fr_sand', 'fr_udef']
    
    std_names = ['fr_hcla.st', '', 'fr_silc.st', 'fr_lcla.st', 'fr_sicl.st', 'fr_cloa.st', 'fr_silt.st', 'fr_silo.st', 
                'fr_scla.st', 'fr_loam.st', 'fr_sclo.st', 'fr_sloa.st', 'fr_lsan.st', 'fr_sand.st', 'fr_udef.st'] 
    
    long_names = ['Fraction of Heavy Clay', '', 'Fraction of Silty Clay', 'Fraction of Light Clay', 
                  'Fraction of Silty Clay Loam', 'Fraction of Clay Loam', 'Fraction of Silt', 'Fraction of Silty Loam', 
                  'Fraction of Sandy Clay', 'Fraction of Loam', 'Fraction of Sandy Clay Loam', 'Fraction of Sandy Loam', 
                  'Fraction of Loamy Sand', 'Fraction of Sand', 'Fraction of Undefined or Water']

    xfr = xr.Dataset(attrs={'title':'Percentage of HWSD soil type', 'institution': 'KIT', 
                            'source': 'HWSD Harmonized World Soil Database', 
                            'history': '%s hwsdART_to_buffer'%(datetime.now().isoformat()),
                            'references': 'HWSD Harmonized World Soil Database', 
                            'comment': 'For generating mineral dust emissions in ICON-ART and COSMO-ART',
                            'number_of_grid_used': '', 'uuidOfHGrid': ''})

    for i, (name, std_name, long_name) in enumerate(zip(fr_names, std_names, long_names)):
        xfr[name] = xr.DataArray(fracs[:, i], dims={'cell':np.arange(tg.lons.size)}, 
                                 attrs={'standard_name':std_name, 'long_name':long_name, 'units':'1'})    
    xfr = xfr.drop_vars("fr0")
    
    grd = xr.open_dataset(tg.gridfile)    
    xfr['clon'] = xr.DataArray(tg.lons, dims={'cell':np.arange(tg.lons.size)},
                               attrs={'standard_name':'grid longitude', 'long_name':'longitude of icon grid cell centre', 'units':'radians'})    
    xfr['clat'] = xr.DataArray(tg.lats, dims={'cell':np.arange(tg.lons.size)},
                               attrs={'standard_name':'grid latitude', 'long_name':'latitude of icon grid cell centre', 'units':'radians'})       
    xfr['clon_vertices'] = xr.DataArray(grd.clon_vertices.values, dims={'cell':np.arange(tg.lons.size), 'nv':3},
                               attrs={'standard_name':'vertices longitude', 'long_name':'longitude of icon grid cell vertices', 'units':'radians'})
    xfr['clat_vertices'] = xr.DataArray(grd.clat_vertices.values, dims={'cell':np.arange(tg.lons.size), 'nv':3},
                               attrs={'standard_name':'vertices latitude', 'long_name':'latitude of icon grid cell vertices', 'units':'radians'})    
    xfr.attrs['uuidOfHGrid'] = grd.attrs['uuidOfHGrid']
    xfr.attrs['number_of_grid_used'] = grd.attrs['number_of_grid_used']
    return xfr.astype('float32')
    
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# initialize logger
logging.basicConfig(
    filename="extpar_hwsdART_to_buffer.log",
    level=logging.INFO,
    format="%(message)s",
    filemode="w",
)

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
logging.info("============= start extpar_hwsdART_to_buffer =======")
logging.info("")

# print a summary of the environment
env.check_environment_for_extpar(__file__)

# check HDF5
lock = env.check_hdf5_threadsafe()

# get number of OpenMP threads for CDO
omp = env.get_omp_num_threads()

# get total cores available on the node
num_cores = joblib.cpu_count()
n_jobs = 90

logging.info("============= No. of CPU Cores Availabe: %d ======="%num_cores)
logging.info("============= No. of CPU Cores to be used: %d ======="%n_jobs)
logging.info("")

# unique names for files written to system to allow parallel execution
grid = "grid_description_hwsdART"  # name for grid description file
reduced_grid = "reduced_icon_grid_hwsdART.nc"  # name for reduced icon grid
weights = "weights_hwsdART"  # name for weights of spatial interpolation

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
logging.info("")
logging.info("============= init variables from namelist =====")
logging.info("")

igrid_type, grid_namelist = utils.check_gridtype("INPUT_grid_org")

if igrid_type == 1:
    path_to_grid = fortran_namelist.read_variable(grid_namelist, "icon_grid_dir", str)
    icon_grid = fortran_namelist.read_variable(grid_namelist, "icon_grid_nc_file", str)
    icon_grid = utils.clean_path(path_to_grid, icon_grid)
    tg = grid_def.IconGrid(icon_grid)
    grid = tg.reduce_grid(reduced_grid)
elif igrid_type == 2:
    tg = grid_def.CosmoGrid(grid_namelist)
    tg.create_grid_description(grid)

ihwsdART['raw_data_hwsdART_path'] = fortran_namelist.read_variable('INPUT_hwsdART', 'raw_data_hwsdART_path', str)[:-1]
ihwsdART['raw_data_hwsdART_filename'] = fortran_namelist.read_variable('INPUT_hwsdART', 'raw_data_hwsdART_filename', str)[:-1]
ihwsdART['hwsdART_buffer_file'] = fortran_namelist.read_variable('INPUT_hwsdART', 'hwsdART_buffer_file', str)
raw_data_hwsdART = utils.clean_path(ihwsdART['raw_data_hwsdART_path'], ihwsdART['raw_data_hwsdART_filename'])
buffer_file = ihwsdART['hwsdART_buffer_file'].split('.')[0] + '.nc'

input_hwsdART = fortran_namelist.InputHwsdart()
fortran_namelist.write_fortran_namelist("INPUT_hwsdART", ihwsdART, input_hwsdART)

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
logging.info("")
logging.info("============= delete files from old runs =======")
logging.info("")

utils.remove(grid)
utils.remove(weights)
utils.remove(reduced_grid)
utils.remove(buffer_file)

logging.info('')
logging.info('raw hwsdART data used: %s'%raw_data_hwsdART)
logging.info('Buffer file: %s'%buffer_file)
logging.info('')

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
logging.info("")
logging.info("============= Open original HWSD dataset ========")
logging.info("")

raw = xr.open_mfdataset(raw_data_hwsdART, chunks='auto')['LU']
lons = np.array(tg.lons)
lats = np.array(tg.lats)

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
logging.info("")
logging.info("============= Mapping raw pixel data to memory map for efficient use ========")
logging.info("")

folder = '/tmp/joblib_memmap'
try:
    os.mkdir(folder)
except FileExistsError:
    pass

lus_filename_memmap = os.path.join(folder, 'lus_memmap')
lus = get_mmap(raw.values, lus_filename_memmap)

idxs = -1 * np.ones(raw.shape, int)
data_filename_memmap = os.path.join(folder, 'data_memmap')
idxs = get_mmap(idxs, data_filename_memmap)
=======
idxs = -1 * np.ones(raw.shape, int)
ndi = np.arange(raw.lat.size)

Parallel(n_jobs=45, max_nbytes='100M', mmap_mode='w+')(delayed(get_index)(i, lons, lats, hlon, hlat, idxs) for i in tqdm(ndi))
idxs = -1 * np.ones(raw.shape, int)
ndi = np.arange(raw.lat.size)

Parallel(n_jobs=45, max_nbytes='100M', mmap_mode='w+')(delayed(get_index)(i, lons, lats, hlon, hlat, idxs) for i in tqdm(ndi))
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
logging.info("")
logging.info(
    "============= Find cells in target grid nearest to grid boxes in original HWSD dataset ========"
)
logging.info("")

ndi = np.arange(raw.lat.size)   
Parallel(n_jobs=n_jobs, max_nbytes='100M', mmap_mode='w+')(delayed(get_index)(i, lons, lats, raw.lon.values, raw.lat.values, idxs) for i in tqdm(ndi))

# --------------------------------------------------------------------------
logging.info("")
logging.info("============= Calculate LU Fraction for target grid ========")
logging.info("")

fracs = calculate_fraction(lons, lus, idxs)
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
logging.info("")
logging.info("============= write to buffer file =============")
logging.info("")

xfr = write_data_to_buffer(fracs, tg)
xfr.to_netcdf(buffer_file)
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
logging.info("")
logging.info("============= clean up =============")
logging.info("")

utils.remove(weights)
os.system('rm -rf %s'%folder)
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
logging.info("")
logging.info("============= extpar_hwsdART_to_buffer done ========")
logging.info("")
