import numpy as np
'''
Module providing the Meta-Data classes for the buffer file,
it contains:

    -Parent:Coordinates -> Child: Lon, Lat

    -Parent:AlbMeta -> Child: AL, NI, UV

    -Parent:NdviMeta-> Child: NDVI, NdviMax, NdviMrat

    -Parent:ClimMeta-> Child: TempClim, HsurfClim

Meta-Data that is shared amongs all fields of an Extpar class is defined in
the parent class, for example CoordsMeta 
Meta-Data that is only valid for one specific field is defined 
in the respective child class

The following Meta-Data is required during the write_*d_fields functions:
    -name ( string)
    -short ( string)
    -standard ( string)
    -long ( string)
    -type ( basic datatype (float, integer,...)
    -dim ( dictionary for all dimensions of fields, ie is the last index)
'''

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# Coordinates
# ->Lon
# ->Lat


class CoordsMeta:
    def __init__(self):         
        self.type = np.float32
        self.dim = {0: 'ke',
                    1: 'je',
                    2: 'ie'}


class Lon(CoordsMeta):
    def __init__(self):         
        super().__init__()
        self.name = 'lon'
        self.short = '_'
        self.standard = 'longitude'
        self.long = 'geographical longitude'
        self.units = 'degrees_east'


class Lat(CoordsMeta):
    def __init__(self):         
        super().__init__()
        self.name = 'lat'
        self.short = '-'
        self.standard = 'latitude'
        self.long = 'geographical latitude'
        self.units = 'degrees_north'
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# Albedo
# ->AL
# ->NI
# ->UV


class AlbMeta:
    def __init__(self):
        self.type = np.float32
        self.units = '%'
        self.dim = { 0: 'time',
                     1: 'ke', 
                     2: 'je',
                     3: 'ie'}


class AL(AlbMeta):
    def __init__(self):         
        super().__init__()
        self.name = 'ALB_DIF12'
        self.long = 'Albedo'
        self.orig = 'al'


class NI(AlbMeta):
    def __init__(self):         
        super().__init__()
        self.name = 'ALNID12'
        self.long = 'NI_Albedo'
        self.orig = 'alnid'


class UV(AlbMeta):
    def __init__(self):         
        super().__init__()
        self.name = 'ALUVD12'
        self.long = 'UV_Albedo'
        self.orig = 'aluvd'
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# NDVI
# ->NDVI
# ->NdviMax
# ->NdviMrat


class NdviMeta:
    def __init__(self):
        self.type = np.float32
        self.units = '_'
        self.standard = '_'
        self.short = '_'


class NDVI(NdviMeta):
    def __init__(self):
        super().__init__()
        self.dim = { 0: 'time',
                     1: 'ke', 
                     2: 'je',
                     3: 'ie'}
        self.name = 'NDVI'
        self.long = 'monthly mean NDVI climatology 1998-2003'


class NdviMax(NdviMeta):
    def __init__(self):
        super().__init__()
        self.dim = { 0: 'ke', 
                     1: 'je',
                     2: 'ie'}
        self.name = 'NDVI_MAX'
        self.long = 'NDVI yearly maximum for climatology 1998-2003'


class NdviMrat(NdviMeta):
    def __init__(self):
        super().__init__()
        self.dim = { 0: 'time',
                     1: 'ke', 
                     2: 'je',
                     3: 'ie'}
        self.name = 'NDVI_MRAT'
        self.long = 'monthly proportion of actual value/maximum NDVI'

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
# TCLIM
# ->TempClim
# ->HsurfClim


class ClimMeta:
    def __init__(self):
        self.type = np.float32
        self.dim = { 0: 'ke', 
                     1: 'je',
                     2: 'ie'}


class TempClim(ClimMeta):
    def __init__(self):
        super().__init__()
        self.name = 'T_CL'
        self.standard = 'soil_temperature' 
        self.long = 'CRU near surface temperature climatology'
        self.units = 'K'


class HsurfClim(ClimMeta):
    def __init__(self):
        super().__init__()
        self.name = 'HSURF'
        self.standard = 'surface_altitude' 
        self.long = 'CRU grid elevation'
        self.units = 'm'
