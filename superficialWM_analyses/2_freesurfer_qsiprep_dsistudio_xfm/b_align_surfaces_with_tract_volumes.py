import os
import json
import sys
import shutil
from os.path import join as ospj
from smriprep.interfaces.surf import normalize_surfs
 


 
########################################
# Set directories
########################################
# Parse command-line arguments
if len(sys.argv) != 3:
    print("Usage: python b_align_surfaces_with_tract_volumes.py <config_file> <subject_id>")
    sys.exit(1)

config_file = sys.argv[1]
subject_id = sys.argv[2]

# Read config from the specified file
with open(config_file, "rb") as f:
    config = json.load(f)

outputs_root = config['outputs_root']

########################################
# Process subject ID
########################################
sub = subject_id
print(sub)

########################################
# Check for required files
########################################

# Check for pial surface files (now giftis)
pial_files = [ospj(outputs_root, sub, 'surfaces', 'freesurfer', f) for f in os.listdir(ospj(outputs_root, sub, 'surfaces', 'freesurfer')) if f.endswith('pial.freesurfer.surf.gii')]
if len(pial_files) != 2:
    print('Missing pial surface files for %s' % sub)
    exit(1)

# Check for white surface files
white_files = [ospj(outputs_root, sub, 'surfaces', 'freesurfer', f) for f in os.listdir(ospj(outputs_root, sub, 'surfaces', 'freesurfer')) if f.endswith('white.freesurfer.surf.gii')]
if len(white_files) != 2:
    print('Missing white surface files for %s' % sub)
    exit(1)

# Check for midthickness surface files
midthickness_files = [ospj(outputs_root, sub, 'surfaces', 'freesurfer', f) for f in os.listdir(ospj(outputs_root, sub, 'surfaces', 'freesurfer')) if f.endswith('midthickness.freesurfer.surf.gii')]
if len(midthickness_files) != 2:
    print('Missing midthickness surface files for %s' % sub)
    exit(1)

# Check for inflated surface files
inflated_files = [ospj(outputs_root, sub, 'surfaces', 'freesurfer', f) for f in os.listdir(ospj(outputs_root, sub, 'surfaces', 'freesurfer')) if f.endswith('inflated.freesurfer.surf.gii')]
if len(inflated_files) != 2:
    print('Missing inflated surface files for %s' % sub)
    exit(1)

# Check for .lta transformation file
lta_files = [ospj(outputs_root, sub, 'transforms', 'freesurfer-to-native_acpc', f) for f in os.listdir(ospj(outputs_root, sub, 'transforms', 'freesurfer-to-native_acpc')) if f.endswith('.lta')]
if len(lta_files) != 1:
    print('Missing .lta transformation file for %s' % sub)
    exit(1)



########################################
# Create output directories
########################################

# Create directory for transformed surfaces (into QSIPrep space)
if not os.path.exists(ospj(outputs_root, sub, 'surfaces', 'native_acpc')):
    os.makedirs(ospj(outputs_root, sub, 'surfaces', 'native_acpc'))
outputs_dir = ospj(outputs_root, sub, 'surfaces', 'native_acpc')



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
    shutil.copy(converted_surf, ospj(outputs_dir, f"{sub}.{hemi}.pial.native_acpc.surf.gii"))





print('Transforming white surfaces...')
# Apply transformation to white surfaces
for white_file in white_files:

    # Get hemisphere
    hemi = white_file.split('/')[-1].split('.')[1]

    # Apply transformation
    converted_surf = normalize_surfs(white_file, lta_files[0], os.path.dirname(white_file))

    # Move the converted surface to the same directory as the original surface
    shutil.copy(converted_surf, ospj(outputs_dir, f"{sub}.{hemi}.white.native_acpc.surf.gii"))



print('Transforming midthickness surfaces...')

# Apply transformation to white surfaces
for midthickness_file in midthickness_files:

    # Get hemisphere
    hemi = midthickness_file.split('/')[-1].split('.')[1]

    # Apply transformation
    converted_surf = normalize_surfs(midthickness_file, lta_files[0], os.path.dirname(midthickness_file))

    # Move the converted surface to the same directory as the original surface
    shutil.copy(converted_surf, ospj(outputs_dir, f"{sub}.{hemi}.midthickness.native_acpc.surf.gii"))



print('Transforming inflated surfaces...')

# Apply transformation to white surfaces
for inflated_file in inflated_files:

    # Get hemisphere
    hemi = inflated_file.split('/')[-1].split('.')[1]

    # Apply transformation
    converted_surf = normalize_surfs(inflated_file, lta_files[0], os.path.dirname(inflated_file))

    # Move the converted surface to the same directory as the original surface
    shutil.copy(converted_surf, ospj(outputs_dir, f"{sub}.{hemi}.inflated.native_acpc.surf.gii"))
