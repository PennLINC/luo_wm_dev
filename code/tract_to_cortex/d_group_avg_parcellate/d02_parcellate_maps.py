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
import pandas as pd
import re
from surfplot import Plot
import sys

"""
This script takes the population probability tract-to-cortex maps and parcellates them to Glasser. 
Saves out csv's of the maps (no threshold applied).
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
manuscript_data_root = config['manuscript_input_root']
dataset = config['dataset']
derivs_dir = ospj(manuscript_data_root, f"derivatives/vol_to_surf")
out_dir = ospj(derivs_dir, "group")

###################
# Define functions 
###################
 
def parcellate_tract_maps(parcellator_object, tract):
    """
    Apply a cortical parcellation to tract-to-region probability maps
    """
    tract_lh = f"Left{tract}"
    tract_rh = f"Right{tract}"
    left_gii = tract_averages[f"{tract_lh}"]
    right_gii = tract_averages[f"{tract_rh}"]
    combined_gii = [left_gii, right_gii]
    parcellated_maps = parcellator_object.fit_transform(combined_gii, 'fsLR') 
    return parcellated_maps

def save_parcellated_maps(parcellator_object, tract, atlas, depth):
    parcellated_maps = parcellate_tract_maps(parcellator_object,  tract)
    print(f"{tract} parcellated")

    lh_parcellated = parcellated_maps[:180]  # first 180 parcels for lh
    rh_parcellated = parcellated_maps[180:]  # next 180 parcels for rh

    # save parcellated lh
    lh_labels = np.unique(lh_glasser.darrays[0].data)[1:]  # exclude 0 (medial wall)
    lh_data = np.column_stack((lh_parcellated, lh_labels))
    lh_df = pd.DataFrame(lh_data, columns=['probability', 'regionID'])
    lh_df.to_csv(ospj(out_dir, f'Left{tract}_{depth}_{atlas}.csv'), index=False)
    
    # save parcellated rh
    rh_labels = np.unique(rh_glasser.darrays[0].data)[1:]  
    rh_data = np.column_stack((rh_parcellated, rh_labels))
    rh_df = pd.DataFrame(rh_data, columns=['probability', 'regionID'])
    rh_df.to_csv(ospj(out_dir, f'Right{tract}_{depth}_{atlas}.csv'), index=False)

    # create vertex-level data from the parcellated data
    lh_vertex_data = np.zeros(32492)
    rh_vertex_data = np.zeros(32492)

    # assign the parcellated values to the corresponding vertices for plotting
    for idx, label in enumerate(np.unique(lh_glasser.darrays[0].data)):
        if label != 0:  # don't map the medial wall  
            lh_vertex_data[lh_glasser.darrays[0].data == label] = lh_parcellated[idx - 1] # identify which vertices belong to the current parcel

    for idx, label in enumerate(np.unique(rh_glasser.darrays[0].data)):
        if label != 0:   
            rh_vertex_data[rh_glasser.darrays[0].data == label] = rh_parcellated[idx - 1]

    vertex_data = np.concatenate((lh_vertex_data, rh_vertex_data))
    print(f"{tract} parcellated data saved")
    return vertex_data


##############################################################################
# Load vertex-level population probability tract maps for my depth of interest
##############################################################################
tract_averages = {}
for filename in os.listdir(out_dir):
    pattern = rf'group_([A-Za-z]+)_{depth}\.shape\.gii'
    match = re.search(pattern, filename)
    if match:
        tract = match.group(1)
        file_path = os.path.join(out_dir, filename)
        tract_averages[tract] = file_path # save the gifti filepath into a dictionary {tractname:tractmap}

print(tract_averages.keys())

tracts = {tract for tract in tract_averages.keys()}
tracts = set(tract[4:] if tract.startswith('Left') else (tract[5:] if tract.startswith('Right') else tract) for tract in tracts) # get the tract root name

#########################################################
# Load glasser
#########################################################
lh_glasser = nib.load("/cbica/projects/luo_wm_dev/atlases/glasser/FSLRVertex/glasser_360_L.label.gii")
rh_glasser = nib.load("/cbica/projects/luo_wm_dev/atlases/glasser/FSLRVertex/glasser_360_R.label.gii")

# generate neuromaps fsLR based Glasser 360 parcellation (needs relabeling to have consecutive region IDs)
glasser = images.relabel_gifti((lh_glasser, rh_glasser), background=['Medial_wall'])

# let's assign these labels so that we can call them later for plotting
lh_glasser = glasser[0]
rh_glasser = glasser[1]

# create parcellater object
glasser_parc = Parcellater(glasser, 'fsLR').fit()
print(glasser_parc)

#########################################################
# Parcellate and save!
#########################################################
for tract in tracts:
    save_parcellated_maps(glasser_parc, tract, "glasser", depth) 