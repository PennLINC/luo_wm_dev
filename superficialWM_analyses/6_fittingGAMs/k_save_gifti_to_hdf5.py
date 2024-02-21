from collections import defaultdict
import csv
import h5py
import json
import nibabel as nib
import numpy as np
import os
from os.path import join as ospj
import pandas as pd
import re
import shutil
import sys
from tqdm import tqdm

# This script does a few things: 
# 1) saves out dsistudio scalars and GMprobseg for all subjects to h5 file WITHOUT excluding vertices
# 2) saves out dsistudio scalars to h5 file for data that has vertices with GM probability >50% removed and has medial wall removed 

# script adapted from Chenying's concifti code :) 

########################################
# Set directories
########################################
config_file = "/cbica/projects/luo_wm_dev/code/superficialWM_analyses/config_HCPD.json"

# Read config from the specified file
with open(config_file, "rb") as f:
    config = json.load(f)

dataset = config['dataset']
data_root = config['data_root']
outputs_root = config['outputs_root']
relative_root = ospj(f"{outputs_root}", "all_subjects")
h5_dir = ospj(f"{relative_root}", "h5_files")

if not os.path.exists(f"{h5_dir}"):
        os.makedirs(f"{h5_dir}")
        print(f"Directory 'h5_dir' created.")
else:
        print(f"Directory 'h5_dir' already exists.")

 
########################################
# Define functions
########################################

# write hdf5 files for non-filtered data
def write_hdf5(type, depth, relative_root):
    """
    Load all gifti data.
    Parameters
    -----------
    type: str
        either "GMprobseg" or the name of the DSIStudio scalar
    depth: str
        indicates the depth that vol_to_surf sampled at (e.g. "depth_1p25" or "depth_1")
    relative_root: str
        path to which index_file, directions_file and cohort_file (and its contents) are relative
    """

    # define cohort filename
    cohort_file = f"{type}_{depth}_cohortfile.csv"

    # load cohort csv
    if type == "GMprobseg":
        cohort_df = pd.read_csv(ospj(relative_root, "cohortfiles","GMprobseg",cohort_file))
    else: # use scalar name to get cohort file
        cohort_df = pd.read_csv(ospj(relative_root, "cohortfiles", "dsistudio_scalars", type, cohort_file))
        
    # upload each subject's data
    scalars = defaultdict(list)
    sources_lists = defaultdict(list)
    
    print("Loading gifti for each subject")
    for ix, row in tqdm(cohort_df.iterrows(), total=cohort_df.shape[0]):   # ix: index of row (start from 0); row: one row of data
        scalar_file = row['source_file']
        gifti_data = nib.load(scalar_file).darrays[0].data
        scalars[row['scalar_name']].append(gifti_data)   # append to specific scalar_name
        #pattern = r'sub-\d+_[a-zA-Z_]*\d*[a-z]*_depth_\d+[a-zA-Z]*\d*'
        #match = re.search(pattern, row['source_file'])
        #if match:
           # gifti_file_info = match.group(0)
        #sources_lists[row['scalar_name']].append(gifti_file_info)  # append source gifti file info to specific scalar_name
        sources_lists[row['scalar_name']].append(row['source_file'])  # append source gifti filename to specific scalar_name

    # Write the output
    output_h5 = ospj(f"{type}_{depth}.h5")
    h5_finaldir = ospj(f"{h5_dir}", f"{type}")

    if not os.path.exists(f"{h5_finaldir}"):
            os.makedirs(f"{h5_finaldir}")
            print(f"Directory {h5_finaldir} created.")
    else:
            print(f"Directory {h5_finaldir} already exists.")

    output_file = ospj(h5_finaldir,output_h5)
    f = h5py.File(output_file, "w")

    for scalar_name in scalars.keys():  # in the cohort.csv, two or more scalars in one sheet is allowed, and they can be separated to different scalar group.
        one_scalar_h5 = f.create_dataset('scalars/{}/values'.format(scalar_name),
                         data=np.row_stack(scalars[scalar_name]))
         
        one_scalar_h5.attrs['column_names'] = list(sources_lists[scalar_name])  # column names: list of source gifti filenames
    f.close()
    print(f"{type} h5 saved")
    return int(not os.path.exists(output_file))



# write hdf5 files for FILTERED data
def write_hdf5_GMfiltered(type, depth, relative_root):
    """
    Load all gifti data.
    Parameters
    -----------
    type: str
        either "GMprobseg" or the name of the DSIStudio scalar
    depth: str
        indicates the depth that vol_to_surf sampled at (e.g. "depth_1p25" or "depth_1")
    relative_root: str
        path to which index_file, directions_file and cohort_file (and its contents) are relative
    """

    # define cohort filename
    cohort_file = f"{type}_{depth}_cohortfile_GMfiltered_noMW.csv"

    # load cohort csv
    cohort_df = pd.read_csv(ospj(relative_root, "cohortfiles", "dsistudio_scalars", type, cohort_file))
        
    # upload each subject's data
    scalars = defaultdict(list)
    sources_lists = defaultdict(list)
    
    print("Loading gifti for each subject")
    for ix, row in tqdm(cohort_df.iterrows(), total=cohort_df.shape[0]):   # ix: index of row (start from 0); row: one row of data
        scalar_file = row['source_file']
        gifti_data = nib.load(scalar_file).darrays[0].data
        scalars[row['scalar_name']].append(gifti_data)   # append to specific scalar_name
        sources_lists[row['scalar_name']].append(row['source_file'])  # append source gifti filename to specific scalar_name

    # Write the output
    output_h5 = ospj(f"{type}_{depth}_GMfiltered_noMW.h5")
    h5_finaldir = ospj(f"{h5_dir}", f"{type}")

    if not os.path.exists(f"{h5_finaldir}"):
            os.makedirs(f"{h5_finaldir}")
            print(f"Directory {h5_finaldir} created.")
    else:
            print(f"Directory {h5_finaldir} already exists.")

    output_file = ospj(h5_finaldir,output_h5)
    f = h5py.File(output_file, "w")

    for scalar_name in scalars.keys():  # in the cohort.csv, two or more scalars in one sheet is allowed, and they can be separated to different scalar group.
        one_scalar_h5 = f.create_dataset('scalars/{}/values'.format(scalar_name),
                         data=np.row_stack(scalars[scalar_name]))
         
        one_scalar_h5.attrs['column_names'] = list(sources_lists[scalar_name])  # column names: list of source gifti filenames
    f.close()
    print(f"{type} h5 saved")
    return int(not os.path.exists(output_file))




##################################################
# NO VERTEX EXCLUSION: Convert gifti's to h5 files 
##################################################
 
# need to load giftis for depth = 1 and 1.25 for each scalar and for GM probseg
# need to make hdf5 files for depth = 1 and 1.25 for each scalar and for GM probseg

write_hdf5("GMprobseg", "depth_1", relative_root)
write_hdf5("GMprobseg", "depth_1p25", relative_root)

# set dsistudio calars
dsistudio_scalars = ["ad", "dti_fa", "gfa", "ha", "iso", "md", "qa", "rd", "rd1", "rd2", "rdi"]

for dsistudio_scalar in dsistudio_scalars:
    write_hdf5(dsistudio_scalar, "depth_1", relative_root)
    write_hdf5(dsistudio_scalar, "depth_1p25", relative_root)




##################################################
# WITH VERTEX EXCLUSION: Convert gifti's to h5 files 
##################################################
   
# set dsistudio calars
dsistudio_scalars = ["ad", "dti_fa", "gfa", "ha", "iso", "md", "qa", "rd", "rd1", "rd2", "rdi"]

for dsistudio_scalar in dsistudio_scalars:
    #write_hdf5_GMfiltered(dsistudio_scalar, "depth_1", relative_root)
    #write_hdf5_GMfiltered(dsistudio_scalar, "depth_1p25", relative_root)
    write_hdf5_GMfiltered(dsistudio_scalar, "depth_3", relative_root)