import csv
import json
import nibabel as nib
from nilearn.plotting import show
from nilearn import surface, datasets, plotting
import numpy as np
import os
from os.path import join as ospj
import pandas as pd
import sys

"""
This script applies nilearn.vol_to_surf to GMprobseg and WMprobseg. This is done for 1 subject at a time.
GM and WM probseg will tell us the the probability of sampling gray and 
white matter respectively at specific depths from the white matter surface.
The point of this analysis is to figure out how deep to sample into white matter 
(using binarized TDIs) from the cortical surface to determine the cortical endpoints for each tract (tract_to_region scripts a-d).
If we sample too deep (say 3 mm), we have a large chance of sampling gray matter across the gyrus. 
If we sample to shallowly, we may not have a high enough probability of hitting white matter. 

ingredients (for each subject):
- qsiprep GM_probseg file in ACPC
- qsiprep WM_probseg file in ACPC
- freesurfer lh.white and rh.white in ACPC 

output:
- a csv per subject that tallies the number of vertices that have <5% probability of hitting gray matter when sampling in to WM at a specified depth
- a csv per subject that tallies the number of vertices that have >95% probability of hitting white matter when sampling in to WM at a specified depth
"""

########################################
# Set directories
########################################
subject = sys.argv[1]
config_file = sys.argv[2]

# Read config from the specified file
with open(config_file, "rb") as f:
    config = json.load(f)

data_root = config['data_root']
manuscript_data_root = config['manuscript_input_root']
dataset = config['dataset']
xfm_dir = ospj(data_root, "derivatives", "fs_qsiprep_xfm", subject)
#xfm_dir2 = ospj(manuscript_data_root, "derivatives", "fs_qsiprep_xfm", subject)
probseg_dir =  ospj(manuscript_data_root, "derivatives", "gyral_hops", subject)


if not os.path.exists(probseg_dir):  
    os.makedirs(probseg_dir)
    print(f"Directory {probseg_dir} created.")
else:
    print(f"Directory {probseg_dir} already exists.")
    
########################################
# Functions
########################################
# Define function for extracting probabilities at different depths
def apply_vol_to_surf(img, depth, surf_mesh):
    surf_data = surface.vol_to_surf(img, 
                        surf_mesh, 
                        kind='line', 
                        radius=1,
                        n_samples=None, 
                        mask_img=None, 
                        inner_mesh=None, 
                        depth=[depth])
    return(surf_data)
# Notes: 
# radius = size in mm of the neighborhood around each vertex in which to draw samples
# depth = expressed as a fraction of radius (default = 3)

# Define function for formatting depths (for key names)
def format_depth(depth):
    # Replace negative sign with "neg", remove decimals, and convert to string
    formatted_depth = str(abs(depth)).replace('.', 'p')
    return f"depth_{formatted_depth}"

# Define function for counting vertices at <5% and >95% probability and determine which depth has lowest probability of hitting gray matter (GM) or
# highest probability of staying in white matter (WM)
def calculate_probability_counts(probseg_data, hemi, type):
        
    depths = []
    less_than_5_percent_counts = []
    greater_than_95_percent_counts = []

    # Iterate over each depth
    for depth, probabilities in probseg_data.items():
        # Count vertices with less than 5% probability
        less_than_5_percent_count = np.sum(probabilities < 0.05)
        less_than_5_percent_counts.append(less_than_5_percent_count)
        
        # Count vertices with greater than 95% probability
        greater_than_95_percent_count = np.sum(probabilities > 0.95)
        greater_than_95_percent_counts.append(greater_than_95_percent_count)
        
        # Append depth name
        depths.append(depth)

    # Create DataFrame
    data = {
        "Depth": depths,
        "lessthan_5percent_Probability_Count": less_than_5_percent_counts,
        "greaterthan_95percent_Probability_Count": greater_than_95_percent_counts,
    }
    df = pd.DataFrame(data)

    if type == "GM":
        # Determine the depth with the maximum number of <5% probability of GM
        max_5_percent_depth = df.loc[df['lessthan_5percent_Probability_Count'].idxmax(), 'Depth']

        # Add additional column indicating which depth has the maximum number of <5% probability
        df['Lowest_GM_probability'] = (df['Depth'] == max_5_percent_depth).astype(int)
    else:
        # Determine the depth with the maximum number of >95% probability of WM
        max_95_percent_depth = df.loc[df['greaterthan_95percent_Probability_Count'].idxmax(), 'Depth']

        # Add additional column indicating which depth has the maximum number of >95% probability
        df['Highest_WM_probability'] = (df['Depth'] == max_95_percent_depth).astype(int)
    
    # Save DataFrame to CSV
    df.to_csv(ospj(probseg_dir, f"{hemi}_{type}probseg_diffdepths.csv"), index=False)
    
    return(df)

# Define function for saving gifti files for vol_to_surf output for probseg
def save_gifti_file(values, hemi, type, outdir):
    for depth, value in values.items():
        filename = f"{hemi}.{type}_{depth}.shape.gii"
        file_path = os.path.join(outdir, filename)
        value = value.astype(np.float32)
        
        gifti_image = nib.gifti.GiftiImage(darrays=[nib.gifti.GiftiDataArray(value)])
        nib.save(gifti_image, file_path)
        
        print(f"Saved GIFTI file for probseg '{depth}'")


########################################
# Load Files
########################################
# load GM and WM probseg files
acpc_files = os.listdir(os.path.join(xfm_dir, "transforms", "freesurfer-to-native_acpc"))  
gm_probseg = [acpc_file for acpc_file in acpc_files if "GM_probseg" in acpc_file]
wm_probseg = [acpc_file for acpc_file in acpc_files if "WM_probseg" in acpc_file]
gm_probseg = nib.load(os.path.join(xfm_dir, "transforms", "freesurfer-to-native_acpc", gm_probseg[0]))
wm_probseg = nib.load(os.path.join(xfm_dir, "transforms", "freesurfer-to-native_acpc", wm_probseg[0]))
 
# load white matter surfaces (freesurfer > native acpc)
surfmesh_files = os.listdir(os.path.join(xfm_dir, "surfaces", "native_acpc"))  
lh_surf_mesh = [surfmesh_file for surfmesh_file in surfmesh_files if "lh.white" in surfmesh_file]
rh_surf_mesh = [surfmesh_file for surfmesh_file in surfmesh_files if "rh.white" in surfmesh_file]
lh_surf_mesh = os.path.join(xfm_dir, "surfaces", "native_acpc", lh_surf_mesh[0])
rh_surf_mesh = os.path.join(xfm_dir, "surfaces", "native_acpc", rh_surf_mesh[0])
 

########################################
# Get GM probabilities at specific depths 
########################################
# depths (mm)
depths = [0, 0.5, 1, 1.5, 2]

# Get GM probabilities at each depth
lh_depth_GMprobseg = {format_depth(depth): apply_vol_to_surf(gm_probseg, depth, lh_surf_mesh) for depth in depths}
rh_depth_GMprobseg = {format_depth(depth): apply_vol_to_surf(gm_probseg, depth, rh_surf_mesh) for depth in depths}

# Save out csv: counts of vertices that have <5%, and >95% GM probability at each depth
calculate_probability_counts(lh_depth_GMprobseg, "lh", "GM")
calculate_probability_counts(rh_depth_GMprobseg, "rh", "GM")

# save out GMprobseg numpy arrays at each depth as gifti's
save_gifti_file(lh_depth_GMprobseg, 'lh', 'GMprobseg_native_acpc', probseg_dir)
save_gifti_file(rh_depth_GMprobseg, 'rh', 'GMprobseg_native_acpc', probseg_dir)

print("GMprobseg saved")

########################################
# Get WM probabilities at specific depths
######################################## 
# depths (mm)
depths = [0, 0.5, 1, 1.5, 2]

# Get WM probabilities at each depth
lh_depth_WMprobseg = {format_depth(depth): apply_vol_to_surf(wm_probseg, depth, lh_surf_mesh) for depth in depths}
rh_depth_WMprobseg = {format_depth(depth): apply_vol_to_surf(wm_probseg, depth, rh_surf_mesh) for depth in depths}
 
# Save out csv: counts of vertices that have <5%, and >95% WM probability at each depth
calculate_probability_counts(lh_depth_WMprobseg, "lh", "WM")
calculate_probability_counts(rh_depth_WMprobseg, "rh", "WM")

# save out WMprobseg numpy arrays at each depth as gifti's
save_gifti_file(lh_depth_WMprobseg, 'lh', 'WMprobseg_native_acpc', probseg_dir)
save_gifti_file(rh_depth_WMprobseg, 'rh', 'WMprobseg_native_acpc', probseg_dir)

print("WMprobseg saved")


 