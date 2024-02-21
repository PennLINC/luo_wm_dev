import csv
import os
from os.path import join as ospj
import pandas as pd
import re
import sys
import json
import nibabel as nib
import numpy as np

# combine lh and rh giftis for each scalar and for GM probseg
 
########################################
# Set directories
########################################
 
# Parse command-line arguments
if len(sys.argv) != 3:
    print("Usage: python combine_hemisphere_giftis.py <config_file> <subject_id>")
    sys.exit(1)

config_file = sys.argv[1]
subject_id = sys.argv[2]
 
# Read config from the specified file
with open(config_file, "rb") as f:
    config = json.load(f)
 
outputs_root = config['outputs_root']
 
########################################
# Combine giftis
########################################

# Set scalars
scalars = ["ad", "dti_fa", "gfa", "ha", "iso", "md", "qa", "rd", "rd1", "rd2", "rdi"]

# Set depths (mm)
depths = [0, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3]

# define function for combining lh and rh giftis for each scalar and for GMprobseg
def combine_giftis(depth, type):
    if type == "scalar":
    
        # Iterate over scalars
        for scalar in scalars:
            print(scalar)

            # load lh and rh gifti's
            lh = nib.load(f"{outputs_root}/{subject_id}/vol_to_surf/dsistudio_scalars/fsaverage5/{scalar}/{subject_id}_{scalar}_lh_{depth}_fsaverage5.shape.gii")
            rh = nib.load(f"{outputs_root}/{subject_id}/vol_to_surf/dsistudio_scalars/fsaverage5/{scalar}/{subject_id}_{scalar}_rh_{depth}_fsaverage5.shape.gii")

            combined_data = np.concatenate((lh.darrays[0].data, rh.darrays[0].data), axis=0)
            
            # Create a new GiftiImage object with the combined data
            combined_gifti = nib.gifti.GiftiImage(darrays=[nib.gifti.GiftiDataArray(combined_data)])

            # Save the combined Gifti file
            nib.save(combined_gifti, f"{outputs_root}/{subject_id}/vol_to_surf/dsistudio_scalars/fsaverage5/{scalar}/{subject_id}_{scalar}_{depth}_fsaverage5.shape.gii")
  
            
    else:
        print("GM_probseg")

        # load lh and rh gifti's
        lh = nib.load(f"{outputs_root}/{subject_id}/vol_to_surf/probseg_depths/fsaverage5/lh.GMprobseg_{depth}_fsaverage5.shape.gii")
        rh = nib.load(f"{outputs_root}/{subject_id}/vol_to_surf/probseg_depths/fsaverage5/rh.GMprobseg_{depth}_fsaverage5.shape.gii")

        combined_data = np.concatenate((lh.darrays[0].data, rh.darrays[0].data), axis=0)
        
        # Create a new GiftiImage object with the combined data
        combined_gifti = nib.gifti.GiftiImage(darrays=[nib.gifti.GiftiDataArray(combined_data)])

        # Save the combined Gifti file
        nib.save(combined_gifti, f"{outputs_root}/{subject_id}/vol_to_surf/probseg_depths/fsaverage5/{subject_id}_GMprobseg_{depth}_fsaverage5.shape.gii")

      

combine_giftis("depth_1", "scalar")
combine_giftis("depth_1", "GM_probseg")

combine_giftis("depth_1p25", "scalar")
combine_giftis("depth_1p25", "GM_probseg")

combine_giftis("depth_3", "scalar")
combine_giftis("depth_3", "GM_probseg")