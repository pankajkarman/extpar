class CoordsMeta:
    def __init__(self):         
        self.type=np.float32
        self.dimensions={1: 'ke'
                         2: 'je'
                         3: 'ie'}

        self.lat=self.LatMeta()
        self.lon=self.LonMeta()

    class LonMeta:
        def __init__(self):         
            self.short='lon'
            self.standard= 'longitude'
            self.long= 'geographical longitude'
            self.units= 'degrees_east'

    class LatMeta:
        def __init__(self):         
            self.short='lat'
            self.standard= 'latitude'
            self.long= 'geographical latitude'
            self.units= 'degrees_north'

class AlbMeta:
    def __init__(self):
        self.type=np.float32
        self.dimensions={ 1: 'time'
                          2: 'ke' 
                          3: 'je'
                          4: 'ie'}
        self.units='%'
        self.number_of_fields=3

        self.subfields=[]
        self.subfields.append(self.AL())
        self.subfields.append(self.NI())
        self.subfields.append(self.UV())

        class AL
            def __init__(self):         
                self.name='ALB_DIF12'
                self.long='Albedo'
                self.orig='al'

        class NI
            def __init__(self):         
                self.name='ALNID12'
                self.long='NI_Albedo'
                self.orig='alnid'
                
        class UV
            def __init__(self):         
                self.name='ALUVD12'
                self.long='UV_Albedo'
                self.orig='aluvd'




