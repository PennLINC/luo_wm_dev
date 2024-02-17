import csv
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


# This script applies nilearn.vol_to_surf to GMprobseg, WMprobseg, and DSIStudio scalars
## GM and WM probseg will tell us the the probability of sampling gray and white matter respectively at specific depths from the white matter surface 
## DSIStudio scalars will be sampled at specific depths from the white matter surface 


########################################
# Set directories
########################################
# Parse command-line arguments
if len(sys.argv) != 3:
    print("Usage: python d_vol_to_surf_probseg_scalars.py <config_file> <subject_id>")
    sys.exit(1)

config_file = sys.argv[1]
subject_id = sys.argv[2]

# Read config from the specified file
with open(config_file, "rb") as f:
    config = json.load(f)

dataset = config['dataset']
outputs_root = config['outputs_root']

inputs_dir=ospj(outputs_root, subject_id, "aligned_scalars")
outputs_dir=ospj(outputs_root, subject_id, "vol_to_surf")

# Create directory for vol_to_surf outputs
if not os.path.exists(outputs_dir):
    os.makedirs(outputs_dir)

if not os.path.exists(ospj(outputs_dir, "probseg_depths", "native")):
    os.makedirs(ospj(outputs_dir, "probseg_depths", "native"))
 
if not os.path.exists(ospj(outputs_dir, "dsistudio_scalars", "native")):
        os.makedirs(ospj(outputs_dir, "dsistudio_scalars", "native"))
        print(f"Directory 'dsistudio_scalars/native' created.")
else:
        print(f"Directory 'dsistudio_scalars/native' already exists.")

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


# Define function for counting vertices at <5% and >95% probability and determine which depth has lowest GM probability
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
    df.to_csv(ospj(outputs_dir, "probseg_depths", f"{hemi}_{type}probseg_diffdepths.csv"), index=False)
    
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



# Define function for saving out giftis for vol_to_surf output for scalars
def save_scalar_as_gifti(vol_to_surf_output, subject_id, hemi, scalar_name, depth, outdir):
    data = vol_to_surf_output.astype(np.float32)
    filename = f"{subject_id}_{scalar_name}_{hemi}_{depth}.shape.gii"

    # makes scalar directory in outdir
    if not os.path.exists(ospj(outdir, scalar_name)):
        os.makedirs(ospj(outdir, scalar_name))
        print(f"Directory '{ospj(outdir, scalar_name)}' created.")
    else:
        print(f"Directory '{ospj(outdir, scalar_name)}' already exists.")


    output_path = output_path = ospj(outdir, scalar_name, filename)
    gii_data = nib.gifti.gifti.GiftiDataArray(data)
    gii_array = nib.gifti.gifti.GiftiImage(darrays=[gii_data])
    nib.save(gii_array, output_path)
    print(f"Saved: {output_path}")



########################################
# Load Files
########################################

# load GM and WM probseg files
acpc_files = os.listdir(os.path.join(outputs_root, subject_id, "transforms", "freesurfer-to-native_acpc"))  
gm_probseg = [acpc_file for acpc_file in acpc_files if "GM_probseg" in acpc_file]
wm_probseg = [acpc_file for acpc_file in acpc_files if "WM_probseg" in acpc_file]
gm_probseg = nib.load(os.path.join(outputs_root, subject_id, "transforms", "freesurfer-to-native_acpc", gm_probseg[0]))
wm_probseg = nib.load(os.path.join(outputs_root, subject_id, "transforms", "freesurfer-to-native_acpc", wm_probseg[0]))
 
# load white matter surfaces (freesurfer > native acpc)
surfmesh_files = os.listdir(os.path.join(outputs_root, subject_id, "surfaces", "native_acpc"))  
lh_surf_mesh = [surfmesh_file for surfmesh_file in surfmesh_files if "lh.white" in surfmesh_file]
rh_surf_mesh = [surfmesh_file for surfmesh_file in surfmesh_files if "rh.white" in surfmesh_file]
lh_surf_mesh = os.path.join(outputs_root, subject_id, "surfaces", "native_acpc", lh_surf_mesh[0])
rh_surf_mesh = os.path.join(outputs_root, subject_id, "surfaces", "native_acpc", rh_surf_mesh[0])
 


########################################
# Get GM probabilities at depths
########################################

# Set depths (mm)
depths = [0, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3]

# Get GM probabilities at each depth
lh_depth_GMprobseg = {format_depth(depth): apply_vol_to_surf(gm_probseg, depth, lh_surf_mesh) for depth in depths}
rh_depth_GMprobseg = {format_depth(depth): apply_vol_to_surf(gm_probseg, depth, rh_surf_mesh) for depth in depths}


# Save out csv: counts of vertices that have <5%, and >95% GM probability at each depth
# Initialize lists to store counts
calculate_probability_counts(lh_depth_GMprobseg, "lh", "GM")
calculate_probability_counts(rh_depth_GMprobseg, "rh", "GM")


# save out GMprobseg numpy arrays at each depth as gifti's
outputs_probseg_dir = ospj(outputs_dir, "probseg_depths", "native")
save_gifti_file(lh_depth_GMprobseg, 'lh', 'GMprobseg', outputs_probseg_dir)
save_gifti_file(rh_depth_GMprobseg, 'rh', 'GMprobseg', outputs_probseg_dir)

print("GMprobseg saved")

########################################
# Get WM probabilities at depths
######################################## 

# Set depths (mm)
depths = [0, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3]

# Get WM probabilities at each depth
lh_depth_WMprobseg = {format_depth(depth): apply_vol_to_surf(wm_probseg, depth, lh_surf_mesh) for depth in depths}
rh_depth_WMprobseg = {format_depth(depth): apply_vol_to_surf(wm_probseg, depth, rh_surf_mesh) for depth in depths}
 
# Save out csv: counts of vertices that have <5%, and >95% WM probability at each depth
# remember that these counts are all in native acpc 
# Initialize lists to store counts
calculate_probability_counts(lh_depth_WMprobseg, "lh", "WM")
calculate_probability_counts(rh_depth_WMprobseg, "rh", "WM")

# save out WMprobseg numpy arrays at each depth as gifti's
outputs_probseg_dir = ospj(outputs_dir, "probseg_depths", "native")
save_gifti_file(lh_depth_WMprobseg, 'lh', 'WMprobseg', outputs_probseg_dir)
save_gifti_file(rh_depth_WMprobseg, 'rh', 'WMprobseg', outputs_probseg_dir)

print("WMprobseg saved")

######################################## 
# Sample DSIStudio Scalars into White Matter
######################################## 

scalar_names=['ad','dti_fa','gfa','ha','iso','md','qa','rd','rd1','rd2', 'rdi'] 
scalars_dir=ospj(outputs_root, subject_id, "aligned_scalars")

# load scalars
dsistudio_scalars = {}
for scalar_name in scalar_names:
    filename = f"{subject_id}_ses-V1_space-T1w_desc-preproc_gqiscalar.{scalar_name}.fixhdr_LAS.nii.gz"
    scalar_files = os.listdir(scalars_dir)
    filename = [scalar_file for scalar_file in scalar_files if f"{scalar_name}" in scalar_file]
    file_path = os.path.join(scalars_dir, filename[0])
    dsistudio_scalars[scalar_name] = nib.load(file_path)
    print(scalar_name)
 
 
# Create a nested dictionary comprehension for all scalars
depths = [0, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3]
scalars_vol2surf = {scalar_name: {
                    'lh': {format_depth(depth): apply_vol_to_surf(dsistudio_scalars[scalar_name], depth, lh_surf_mesh)
                           for depth in depths},
                    'rh': {format_depth(depth): apply_vol_to_surf(dsistudio_scalars[scalar_name], depth, rh_surf_mesh)
                           for depth in depths}
                }
               for scalar_name in scalar_names}
 
print("scalars_vol2surf made")

# save out scalar numpy arrays at each depth as gifti's
outputs_scalar_dir = ospj(outputs_dir, "dsistudio_scalars", "native")

 
# save each scalar at various depths for both hemispheres
depths = ["depth_0", "depth_0p5", "depth_0p75", "depth_1", "depth_1p25", "depth_1p5", "depth_1p75", "depth_2", "depth_2p25", "depth_2p5", "depth_2p75", "depth_3"]  
for scalar_name in scalar_names:
    for hemi in ["lh", "rh"]:
        for depth in depths:
            scalar_data = scalars_vol2surf[scalar_name][hemi][depth]
            save_scalar_as_gifti(scalar_data, subject_id, hemi, scalar_name, depth, outputs_scalar_dir)


 