
import copy
import h5py
import json
import matplotlib
from matplotlib import pyplot as plt
import nibabel as nib
from nilearn.plotting import show
from nilearn import surface, datasets, plotting
import numpy as np
import os
from os.path import join as ospj
import pandas as pd
import sys


########################################
# Set directories
########################################
# Parse command-line arguments
if len(sys.argv) != 3:
    print("Usage: python i_filter_scalars_by_GMprobseg.py <config_file> <sub>")
    sys.exit(1)

config_file = sys.argv[1]
subject_id = sys.argv[2]

# Read config from the specified file
with open(config_file, "rb") as f:
    config = json.load(f)

 
 
dataset = config['dataset']
data_root = config['data_root']
outputs_root = config['outputs_root'] 
 
########################################
# Define functions
########################################

def filter_scalar_by_GMprobseg(depth, subject_id):
    GMprobseg_gifti = nib.load(ospj(f"{outputs_root}", f"{subject_id}", "vol_to_surf/probseg_depths/fsaverage5", f"{subject_id}_GMprobseg_{depth}_fsaverage5.shape.gii"))

    # Iterate over scalars
    scalars = ["ad", "dti_fa", "gfa", "ha", "iso", "md", "qa", "rd", "rd1", "rd2", "rdi"]
    for scalar in scalars:
        print(scalar)
        scalar_gii = nib.load(ospj(f"{outputs_root}", f"{subject_id}", "vol_to_surf/dsistudio_scalars/fsaverage5", f"{scalar}", f"{subject_id}_{scalar}_{depth}_fsaverage5.shape.gii"))
        
        # Replace vertices in scalar_data with "NA" if corresponding GM probability at that vertex is > 50%
        filtered_scalar_data = np.where(GMprobseg_gifti.darrays[0].data > 0.5, np.nan, scalar_gii.darrays[0].data)
        
        filtered_scalar_data_gifti = filtered_scalar_data.astype(np.float32)
        gii_data = nib.gifti.gifti.GiftiDataArray(filtered_scalar_data_gifti)
        gii_array = nib.gifti.gifti.GiftiImage(darrays=[gii_data])
        nib.save(gii_array, ospj(f"{outputs_root}", f"{subject_id}", "vol_to_surf/dsistudio_scalars/fsaverage5", f"{scalar}", f"{subject_id}_{scalar}_{depth}_fsaverage5_GMfiltered.shape.gii"))


filter_scalar_by_GMprobseg("depth_1", subject_id)
filter_scalar_by_GMprobseg("depth_1p25", subject_id)



 
