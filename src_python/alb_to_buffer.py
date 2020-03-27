import logging
import os
import sys
import subprocess

import INPUT_ALB as nl
import io_utilities as io_utils


# initialize logger
logging.basicConfig(filename='extpar_alb_to_buffer.log',\
                    level=logging.INFO, \
                    format='%(levelname)s:%(message)s', \
                    filemode='w')
                    

logging.info( '============= start alb_to_buffer ==============')
logging.info( '')
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= read namelist ====================')
logging.info( '')


io_utils.check_existence(nl.raw_data_path, nl.raw_data_alb)
io_utils.check_existence(nl.raw_data_path, nl.raw_data_aluvd)
io_utils.check_existence(nl.raw_data_path, nl.raw_data_alnid)

omp_num_threads=8

if os.path.exists("weights.nc"):
    os.remove("weights.nc")


file="test.nc"
subprocess.check_output(["ls", "-l"])
try:
    shell_message=subprocess.check_output(["cdo", "info",file], stderr=subprocess.STDOUT)
except subprocess.CalledProcessError:
    logging.error('Shell error', exc_info=True)
    sys.exit(1)

logging.info(shell_message)

