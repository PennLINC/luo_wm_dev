import os
import glob
import json
import sys
import shutil
from os.path import join as ospj
from smriprep.interfaces.surf import normalize_surfs
 
########################################
# Set variables and dirs
########################################
# Parse command-line arguments
subject = sys.argv[1]
config_file = sys.argv[2]

# Read config from the specified file
with open(config_file, "rb") as f:
    config = json.load(f)

data_root = config['data_root']
dataset = config['dataset']
derivs_dir = ospj(data_root, f"derivatives/fs_qsiprep_xfm")
 
########################################
# Check for required files
########################################
# Check for pial surface files (now giftis)
pial_files = [ospj(derivs_dir, subject, 'surfaces', 'freesurfer', f) for f in os.listdir(ospj(derivs_dir, subject, 'surfaces', 'freesurfer')) if f.endswith('pial.freesurfer.surf.gii')]
if len(pial_files) != 2:
    print('Missing pial surface files for %s' % subject)
    exit(1)

# Check for white surface files
white_files = [ospj(derivs_dir, subject, 'surfaces', 'freesurfer', f) for f in os.listdir(ospj(derivs_dir, subject, 'surfaces', 'freesurfer')) if f.endswith('white.freesurfer.surf.gii')]
if len(white_files) != 2:
    print('Missing white surface files for %s' % subject)
    exit(1)

# Check for .lta transformation file
lta_files = [ospj(derivs_dir, subject, 'transforms', 'freesurfer-to-native_acpc', f) for f in os.listdir(ospj(derivs_dir, subject, 'transforms', 'freesurfer-to-native_acpc')) if f.endswith('.lta')]
if len(lta_files) != 1:
    print('Missing .lta transformation file for %s' % subject)
    exit(1)

########################################
# Create output directories
########################################
# Create directory for transformed surfaces (into QSIPrep space)
if not os.path.exists(ospj(derivs_dir, subject, 'surfaces', 'native_acpc')):
    os.makedirs(ospj(derivs_dir, subject, 'surfaces', 'native_acpc'))
outputs_dir = ospj(derivs_dir, subject, 'surfaces', 'native_acpc')

########################################
# Apply Freesurfer to native AC-PC volume transformation to surfaces
########################################
print('Transforming pial surfaces...')

# Apply transformation to pial surfaces
for pial_file in pial_files:

    # Get hemisphere
    hemi = pial_file.split('/')[-1].split('.')[1]

    # Apply transformation
    converted_surf = normalize_surfs(pial_file, lta_files[0], os.path.dirname(pial_file))

    # Move the converted surface to the same directory as the original surface
    shutil.copy(converted_surf, ospj(outputs_dir, f"{subject}.{hemi}.pial.native_acpc.surf.gii"))

print('Transforming white surfaces...')
# Apply transformation to white surfaces
for white_file in white_files:

    # Get hemisphere
    hemi = white_file.split('/')[-1].split('.')[1]

    # Apply transformation
    converted_surf = normalize_surfs(white_file, lta_files[0], os.path.dirname(white_file))

    # Move the converted surface to the same directory as the original surface
    shutil.copy(converted_surf, ospj(outputs_dir, f"{subject}.{hemi}.white.native_acpc.surf.gii"))


########################################
# Clean up
########################################
folder_path = ospj(derivs_dir, subject)
gii_files = glob.glob(os.path.join(folder_path, f'{subject}*.gii')) # not sure why these gifti files appeared - might be something to do with normalize_surfs. but we don't need them - they are copies

for file_path in gii_files:
    try:
        os.remove(file_path)
        print(f"Deleted file: {file_path}")
    except Exception as e:
        print(f"Error deleting file {file_path}: {e}")

folder_path = "/cbica/projects/luo_wm_dev/two_axes_manuscript/code/tract_to_cortex"
gii_files = glob.glob(os.path.join(folder_path, f'{subject}*.gii')) # i think these are some tmp files that normalize_surfs forgot to delete lol *shrug*

for file_path in gii_files:
    try:
        os.remove(file_path)
        print(f"Deleted file: {file_path}")
    except Exception as e:
        print(f"Error deleting file {file_path}: {e}")