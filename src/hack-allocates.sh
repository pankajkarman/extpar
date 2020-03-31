#! /usr/bin/env python3

import re

files = [ "mo_soil_tg_fields.f90",
          "mo_glcc_tg_fields.f90", 
          "mo_landuse_tg_fields.f90",       
          "mo_albedo_tg_fields.f90",   
          "mo_isa_tg_fields.f90",      
          "mo_ahf_tg_fields.f90",      
          "mo_ndvi_tg_fields.f90",     
          "mo_emiss_tg_fields.f90",    
          "mo_era_tg_fields.f90",      
          "mo_topo_tg_fields.f90",     
          "mo_aot_target_fields.f90",  
          "mo_cru_target_fields.f90",  
          "mo_flake_tg_fields.f90"
        ]

leading_pattern = re.compile(r'^allocate *\(', re.I)
trailing_pattern = re.compile(r', *stat *= *errorcode\) *$', re.I)
             
for file in files:
    print("[INFO] {}".format(file))
    with open(file, "r") as a_file:
        for line in a_file:
            stripped_line = line.strip()
            if re.match(r'[\s]*allocate', line, re.I):
                print("[INFO]", stripped_line)
                # strip leading 'allocate *(' and trailing ', *STAT=errorcode)'
                stripped_line = re.sub(leading_pattern, '', stripped_line)
                stripped_line = re.sub(trailing_pattern, '', stripped_line)                
                arrays = re.findall(r'([\w_%]+\([\w,:%]+\))', stripped_line)
                for array in arrays:
                    name = re.search(r'([\w_%]+)', array)[1]
                    dims = re.search(r'[\w_%]+\(([\w,:%]+)\)', array)[1]
                    #print("[INFO]", array, " -> ", name, re.sub(r'1:', '', dims))
                    print("[CODE]", "if (l_use_array_cache) then")
                    print("[CODE]", "   ! new code")
                    print("[CODE]", "else")
                    print("[CODE]", "   allocate({}({}), stat=errorcode)".format(name, re.sub(r'1:', '', dims)))
                    print("[CODE]", "endif")                                        
