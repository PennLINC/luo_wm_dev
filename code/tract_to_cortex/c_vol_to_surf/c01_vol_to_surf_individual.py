import glob
import json
import nibabel as nib
from nilearn import surface, datasets, plotting
from nilearn.plotting import show
import numpy as np
import os
from os.path import join as ospj
import re
import sys

"""
This script uses nilearn vol_to_surf to sample the binarized TDI map (in LAS+ orientation, native acpc space) for each tract to the cortical surface.
So the non-zero regions on the surface correspond to the cortical endpoints of that bundle.
 
ingredients:
- tdi maps in LAS for each subject  
- freesurfer lh.white, rh.white in acpc and LAS

recipe:
- load those files for my subject
- do vol_to_surf for each tract 
- save out.

output: 
- binary maps of cortical endpoints for each tract (subject-level)
- The goal later on is to get a group average for each tract -- cortical maps of tract endpoints that are weighted
by the proportion of subjects whose tract terminates at region X
"""

subject = sys.argv[1]
config_file = sys.argv[2]
depth = float(sys.argv[3])


# Read config from the specified file
with open(config_file, "rb") as f:
    config = json.load(f)

data_root = config['data_root']
dataset = config['dataset']
derivs_dir = ospj(data_root, f"derivatives/vol_to_surf")
out_dir = ospj(derivs_dir, subject, "native_acpc")

# Create directory for vol_to_surf outputs
if not os.path.exists(ospj(derivs_dir, subject, "native_acpc")):
        os.makedirs(ospj(derivs_dir, subject, "native_acpc"))
        print(f"Directory derivatives/vol_to_surf/{subject}/native_acpc created.")
else:
        print(f"Directory derivatives/vol_to_surf/{subject}/native_acpc already exists.")

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


# Define function for saving gifti files for vol_to_surf output
def save_gifti_file(vol_to_surf_output, subject, tract, depth, outdir):
    data = vol_to_surf_output.astype(np.float32)
    filename = f"{subject}_{tract}_{depth}.shape.gii"
    file_path = ospj(outdir, filename)
    gii_data = nib.gifti.gifti.GiftiDataArray(data)
    gii_array = nib.gifti.gifti.GiftiImage(darrays=[gii_data])
    nib.save(gii_array, file_path)
    print(f"Saved GIFTI file for {subject} {tract}")

 
########################################
# Load Files
########################################
# load tdi maps (LAS) for each subject = img 
tdi_maps_path = ospj(data_root, "derivatives", f"tdi_maps", subject, "tdi_binarized")
tdi_files = os.listdir(tdi_maps_path)  
tdi_maps = {}
for file in tdi_files: # loop through each file in my tdi_binarzed dir, extract the tract name, and load it
    if file.endswith('.nii.gz'):
        tract_name = file.split('_')[1]
        file_path = ospj(tdi_maps_path, file)
        tdi_maps[tract_name] = nib.load(file_path)

# load freesurfer lh.white, rh.white in native acpc (LAS) = surfmesh
surfs_path = ospj(data_root, "derivatives", f"fs_qsiprep_xfm", subject, "surfaces/native_acpc")
surfmesh_files = os.listdir(surfs_path)  
lh_surf_mesh = [surfmesh_file for surfmesh_file in surfmesh_files if "lh.white" in surfmesh_file]
rh_surf_mesh = [surfmesh_file for surfmesh_file in surfmesh_files if "rh.white" in surfmesh_file]
lh_surf_mesh = ospj(surfs_path, lh_surf_mesh[0])
rh_surf_mesh = ospj(surfs_path, rh_surf_mesh[0])


###############################################
# Map Binarized Tract Density Maps to Surface!
###############################################
lh_tracts = [key for key in tdi_maps.keys() if key.startswith('Left')]
rh_tracts = [key for key in tdi_maps.keys() if key.startswith('Right')]
bilat_tracts = [key for key in tdi_maps.keys() if 'Callosum' in key]

threshold = 0.5 # even though the TDI maps are binarized, it seems like when we do vol_to_surf, we get a few vertices
# where it's between 0 and 1 (majority are still either 0 or 1). Going to binarize again just to ensure
# that our data is truly binary even after vol_to_surf
 
# do vol_to_surf for lh tracts
for tract in lh_tracts:
        cortical_map = apply_vol_to_surf(tdi_maps[tract], depth = depth, surf_mesh = lh_surf_mesh)
        cortical_map = (cortical_map > threshold).astype(int)
        save_gifti_file(cortical_map, subject, tract, depth = f"{depth}", outdir=out_dir)

# do vol_to_surf for rh tracts
for tract in rh_tracts:
        cortical_map = apply_vol_to_surf(tdi_maps[tract], depth = depth, surf_mesh = rh_surf_mesh)
        cortical_map = (cortical_map > threshold).astype(int)
        save_gifti_file(cortical_map, subject, tract, depth = f"{depth}", outdir=out_dir)


# do vol_to_surf for bilateral tracts
for tract in bilat_tracts:
    cortical_map_lh = apply_vol_to_surf(tdi_maps[tract], depth = depth, surf_mesh = lh_surf_mesh)
    cortical_map_lh = (cortical_map_lh > threshold).astype(int)
    save_gifti_file(cortical_map_lh, subject, f"Left{tract}", depth = f"{depth}", outdir=out_dir) # indicate it's lh

    cortical_map_rh = apply_vol_to_surf(tdi_maps[tract], depth = depth, surf_mesh = rh_surf_mesh)
    cortical_map_rh = (cortical_map_rh > threshold).astype(int)
    save_gifti_file(cortical_map_rh, subject, f"Right{tract}", depth = f"{depth}", outdir=out_dir) # indicate it's rh

 