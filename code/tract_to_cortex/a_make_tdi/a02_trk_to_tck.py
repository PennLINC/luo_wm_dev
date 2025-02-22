from dipy.io.streamline import load_tractogram, save_tractogram
import glob
import json
import nibabel as nib
import numpy as np
import os
from os.path import join as ospj
import re
import sys


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
manuscript_data_root = config['manuscript_input_root']
derivs_dir = ospj(manuscript_data_root, "derivatives", f"tck_temp", subject)

if not os.path.exists(derivs_dir):
        os.makedirs(derivs_dir)
        print(f"Directory {derivs_dir} created.")
else:
        print(f"Directory {derivs_dir} already exists.")

 
###################
# Define function #
###################
def convert_trk(trk_file, ref_img):
    print(f"Processing {trk_file}")
    tract_trk = load_tractogram(trk_file, ref_img)
    pattern = r'_desc-([A-Za-z0-9]+)_tractography\.trk'
    match = re.search(pattern, trk_file)    
    tract = match.group(1)
    save_tractogram(tract_trk, ospj(derivs_dir, f"{tract}_tractography.tck"))


############## 
# Load files #
##############
# load reference image
search_ref_anat = glob.glob(ospj(data_root, f"raw/datalad_qsiprep/qsiprep/{subject}", "se*", f"dwi/{subject}_*_space-T1w_dwiref.nii.gz"))
ref_anat = search_ref_anat[0]
ref_img = nib.load(ref_anat)

    
######################
# Convert trk to tck #
######################
# get the trk files
search_pattern = ospj(pyafq_dir, "qsirecon-PYAFQ", subject, "*", "dwi", 
                      f"{subject}_*T1w_desc-preproc*/bundles/{subject}_*RASMM*_tractography.trk")
trk_files = glob.glob(search_pattern)
excluded_tracts = ["Cingulum", "Thalamic"]
trk_files = [f for f in trk_files if not any(exclude in f for exclude in excluded_tracts)]

# Process each .trk file
for trk_file in trk_files:
    convert_trk(trk_file, ref_img)

 