import json
from matplotlib import colors as mcolors
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import matplotlib
from neuromaps.datasets import fetch_fslr
from neuromaps.images import construct_shape_gii, load_gifti, load_data
from neuromaps import images
from neuromaps.parcellate import Parcellater
import nibabel as nib
import numpy as np
import os
from os.path import join as ospj
import re
from surfplot import Plot
import sys
from tqdm import tqdm

"""
This script takes the binary, subject-level tract-to-cortex maps (in fsLR 32k) for each tract, and averages them across subjects. 
This produces a group-level tract-to-cortex map of the proportion of subjects that have a tract connecting to each cortical region.
Saves the maps out as giftis (no threshold applied)
"""

###########################
## Set variables and dirs #
###########################
dataset = sys.argv[1]
depth = float(sys.argv[2])
 
config_file = f"/cbica/projects/luo_wm_dev/two_axes_manuscript/code/config/config_{dataset}.json"

with open(config_file, "rb") as f:
    config = json.load(f)

data_root = config['data_root']
derivs_dir = ospj(data_root, f"derivatives/vol_to_surf")
out_dir = ospj(derivs_dir, "group")
#os.makedirs(out_dir, exist_ok=True)


###################
# Define functions 
###################
def load_tract_maps(subject_dir, filename):
    """Load the tract-to-cortex map for a given tract from a subject directory."""
    file_path = ospj(subject_dir, filename)
    if os.path.exists(file_path):
        tract_gifti = nib.load(file_path)
        tract_data = tract_gifti.darrays[0].data
        return tract_data
    else:
        raise FileNotFoundError(f'Tract file {file_path} not found.')

def average_tract_maps(derivs_dir, depth):
    """Average tract-to-cortex maps across subjects to get population probability maps for each tract.
    Remember that each subject-level map is already binarized. 
    """
    subject_dirs = [os.path.join(derivs_dir, d, "fslr_32k") for d in os.listdir(derivs_dir) if d.startswith('sub-') and os.path.isdir(os.path.join(derivs_dir, d))]
    subject_ids = [os.path.basename(d) for d in os.listdir(derivs_dir) if d.startswith('sub-') and os.path.isdir(os.path.join(derivs_dir, d))]
    
    # Assume all subjects have the same tract names
    filenames = os.listdir(subject_dirs[0])
    tract_names = set()  # Use a set to avoid duplicates
    
    for filename in filenames:
        if filename.endswith('.shape.gii'):  # Ensure you're looking at the correct files
            parts = filename.split('.')
            if len(parts) > 2:
                tract_name_part = parts[1]
                tract_name = tract_name_part.split('_')[0]
                tract_names.add(tract_name)

    # Initialize dictionary to hold the sum of tract maps for each tract
    tract_sums = {tract: None for tract in tract_names}
    tract_counts = {tract: 0 for tract in tract_names}  # To keep track of how many subjects have each tract

    for subject_dir in tqdm(subject_dirs, desc="Processing subjects"):
        for filename in os.listdir(subject_dir):
            pattern = rf'sub-[^.]+\.([^.]+)_{depth}\.shape\.gii'
            match = re.search(pattern, filename)
            if match:
                tract = match.group(1)
                if tract in tract_names:
                    try:
                        tract_map = load_tract_maps(subject_dir, filename)
                        if tract_sums[tract] is None:
                            tract_sums[tract] = np.zeros_like(tract_map)
                        tract_sums[tract] += tract_map
                        tract_counts[tract] += 1
                    except FileNotFoundError:
                        continue

    # Average the tract maps
    print("Averaging across subjects")
    tract_averages = {tract: tract_sums[tract] / tract_counts[tract] if tract_counts[tract] > 0 else None for tract in tract_names}
    
    return tract_averages

def save_group_maps(tract_averages, output_dir, depth):
    """Save the group-level tract-to-cortex maps."""
    os.makedirs(output_dir, exist_ok=True)
    
    for tract, avg_map in tract_averages.items():
        if avg_map is not None:
            filename = f"group_{tract}_{depth}.shape.gii"
            output_file = ospj(output_dir, filename)
            data = avg_map.astype(np.float32)
            gii_data = nib.gifti.gifti.GiftiDataArray(data)
            gii_array = nib.gifti.gifti.GiftiImage(darrays=[gii_data])
            nib.save(gii_array, output_file)
            print(f"Saved GIFTI file for {tract}")


#########################################################
# Compute population probability maps for each tract
#########################################################
tract_averages = average_tract_maps(derivs_dir, depth)
save_group_maps(tract_averages, out_dir, depth)
print("Group-level tract-to-cortex maps have been saved.")
