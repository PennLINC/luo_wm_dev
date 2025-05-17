import os
import re
import sys
import glob
import json
import numpy as np
import os.path as op
from os.path import join as ospj
import matplotlib.pyplot as plt
import nibabel as nib
from dipy.io.image import load_nifti
from dipy.io.streamline import load_trk
from dipy.data import fetch_bundle_atlas_hcp842, get_bundle_atlas_hcp842

from dipy.tracking.streamline import (
    transform_streamlines,
    set_number_of_points,
    values_from_volume
)
import dipy.stats.analysis as dsa
import dipy.tracking.streamline as dts
from dipy.segment.clustering import QuickBundles
from dipy.segment.featurespeed import ResampleFeature
from dipy.segment.metricspeed import AveragePointwiseEuclideanMetric
from dipy.tracking.streamline import Streamlines
import pandas as pd

"""
This script maps NODDI nifti files to tract profiles following this DIPY tutorial:
https://workshop.dipy.org/documentation/1.7.0/examples_built/17_streamline_analysis/afq_tract_profiles/
* note: no need to reorient streamlines since they have already been reoriented in pyAFQ v. 2.0

input: 
- trk's for the given subject
- noddi nifti (ICVF is our measure of interest)

output:
- ICVF tract profiles for each tract for this subject
"""

###########################
## Set variables and dirs #
###########################
subject = sys.argv[1]
config_file = sys.argv[2]
pyafq_dir = sys.argv[3]

# Read config from the specified file
with open(config_file, "rb") as f:
    config = json.load(f)

dataset = config['dataset']
data_root = config['data_root']

pattern = ospj(pyafq_dir, f"qsirecon-PYAFQ/{subject}/ses-*/dwi/{subject}_ses-*_space-T1w_desc-preproc_dwimap/bundles/")
matching_dirs = glob.glob(pattern)
if matching_dirs:
    trks_dir = matching_dirs[0]
else:
    raise FileNotFoundError("No matching trk directory found.")

out_dir = ospj(data_root, f"derivatives/noddi", subject, "wmNODDI") # can save tract profiles into same folder as volumetric data

########################################
# Load Files
########################################
# load reference image
search_ref_anat = glob.glob(ospj(data_root, f"raw/datalad_qsiprep/qsiprep/{subject}", "se*", f"dwi/{subject}_*_space-T1w_dwiref.nii.gz"))
ref_anat = search_ref_anat[0]
ref_img = nib.load(ref_anat)

# load trks for my subject from pyAFQ outputs
trks_files = os.listdir(trks_dir)  
trks = {}
for file in trks_files: # loop through each file in my trks dir, extract the tract name, and load it
    if file.endswith('.trk'):
        match = re.search(r"desc-([^-_]+)_tractography", file)
        if match:
            tract_name = match.group(1)
            print(f"{tract_name} trk loaded")
            file_path = ospj(trks_dir, file)
            trks[tract_name] = load_trk(file_path, "same", bbox_valid_check=False).streamlines
        
########################################
# Read volumetric data
########################################
print("reading volumetric data")
search_pattern = ospj(out_dir,
                      f"{subject}_*.nii.gz")
volumetric_files = glob.glob(search_pattern)

# load data
noddi_dict = {}
for fpath in volumetric_files:
    # extract parameter name from filename
    # e.g., "param-icvf" --> "icvf"
    match = re.search(r'mdp-([a-zA-Z0-9]+)_', os.path.basename(fpath))
    if match:
        param = match.group(1)
        data, affine = load_nifti(fpath)
        noddi_dict[param] = {'data': data, 'affine': affine}
    else:
        print(f"Could not find mdp in file: {fpath}")

########################################
# Calculate weights for each tract:
########################################
print("Calculating gaussian weights for each tract")
weights_dict = {}
for tract_name, streamlines in trks.items():
    print(tract_name)
    weights = dsa.gaussian_weights(streamlines)
    weights_dict[tract_name] = weights # weights_dict['tractname']


########################################
# And then use the weights to calculate the tract profiles for each bundle
########################################
# This will store all profiles: nested dict
# Format: profiles[param][tract_name] = afq_profile array
profiles = {}

for param, noddi_data in noddi_dict.items():
    scalar_volume = noddi_data['data']
    scalar_affine = noddi_data['affine']
    profiles[param] = {}

    for tract_name, streamlines in trks.items():
        if tract_name not in weights_dict:
            print(f"Skipping {param} x {tract_name}: No weights.")
            continue
        if len(streamlines) < 1:
            print(f"Skipping {param} x {tract_name}: No streamlines.")
            continue

        weights = weights_dict[tract_name]
        print(f"Computing {param} profile for {tract_name}...")
        profile = dsa.afq_profile(scalar_volume, streamlines, scalar_affine, weights=weights)
        profiles[param][tract_name] = profile


########################################
# save out tract profiles for subject
########################################
rows = []

for param, tract_profiles in profiles.items():
    for tract_name, profile in tract_profiles.items():
        for node_id, value in enumerate(profile):
            rows.append({
                'tractID': tract_name,
                'nodeID': node_id,
                'index': param,
                'value': value
            })

# Convert to DataFrame
df_profiles_long = pd.DataFrame(rows)

# Pivot to wide format: rows = tract + nodeID, columns = parameters
df_profiles_wide = df_profiles_long.pivot_table(
    index=['tractID', 'nodeID'],
    columns='index',
    values='value'
).reset_index()

df_profiles_wide.to_csv(ospj(out_dir, f"{subject}_noddi_desc-profiles_tractography.csv"), index=False)
