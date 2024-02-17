import json
import os
from os.path import join as ospj
import sys
import numpy as np
import nibabel as nb
from nipype.utils.filemanip import fname_presuffix
from shutil import copy


# This script aligns DSI Studio scalar exports' affines to that of the transformed dwiref

 
########################################
# Set directories
########################################
# Parse command-line arguments
if len(sys.argv) != 3:
    print("Usage: python c_align_scalars_with_dwiref_affines_LAS.py <config_file> <subject_id>")
    sys.exit(1)

config_file = sys.argv[1]
subject_id = sys.argv[2]

# Read config from the specified file
with open(config_file, "rb") as f:
    config = json.load(f)

dataset = config['dataset']
data_root = config['data_root']
outputs_root = config['outputs_root']

inputs_dir=ospj(data_root, f"{dataset}_autotrack")
outputs_dir=ospj(outputs_root, subject_id, "aligned_scalars")

# Create directory for transformed surfaces (into QSIPrep space)
if not os.path.exists(outputs_dir):
    os.makedirs(outputs_dir)

# Define function to align DSIStudio scalars to transformed qsiprep dwiref
# following code is adapted from https://github.com/PennLINC/qsiprep/blob/master/qsiprep/interfaces/dsi_studio.py#L664
def align_scalars(subject, scalar): 
        """ Aligns DSI Studio scalar exports' affines to that of the transformed dwiref (LAS+, ACPC)
        Parameters
        ----------
            subject : str
                Subject ID
            scalar : str
                Name of DSI Studio scalar (e.g. dti_fa)
            Returns
            -------
            subject, scalar : tuple
                    
        """
        # want to extract the filename with correct scalar
        files = os.listdir(os.path.join(inputs_dir, subject))
        dsi_studio_file = [file for file in files if scalar in file]
        dsi_studio_file = os.path.join(inputs_dir, subject, dsi_studio_file[0]) # the DSI studio scalar file
        new_file = fname_presuffix(dsi_studio_file, suffix=".fixhdr_LAS", newpath=outputs_dir) # name of the correctly aligned file that we'll save out

        dsi_img = nb.load(dsi_studio_file)

        transformed_files = os.listdir(os.path.join(outputs_root, subject, "transforms", "freesurfer-to-native_acpc")) # qsiprep and freesurfer files that have been transformed to acpc
        
        correct_img = [trans_file for trans_file in transformed_files if "dwiref" in trans_file] # the dwiref in acpc

        correct_img = os.path.join(outputs_root, subject, "transforms", "freesurfer-to-native_acpc", correct_img[0]) # full path of dwiref in acpc
        correct_img = nb.load(correct_img)

        new_axcodes = nb.aff2axcodes(correct_img.affine) # should be LAS
        input_axcodes = nb.aff2axcodes(dsi_img.affine) # is LPS 



        # Is the input image oriented how we want?
        if not input_axcodes == new_axcodes:
            # Re-orient
            input_orientation = nb.orientations.axcodes2ornt(input_axcodes)
            desired_orientation = nb.orientations.axcodes2ornt(new_axcodes)
            transform_orientation = nb.orientations.ornt_transform(input_orientation,
                                                                desired_orientation)
            reoriented_img = dsi_img.as_reoriented(transform_orientation)

        else:
            reoriented_img = dsi_img

        # No matter what, still use the correct affine
        nb.Nifti1Image(
            reoriented_img.get_fdata(),
            correct_img.affine).to_filename(new_file) # saves out the newly aligned scalar (now in LAS)
        print(subject, scalar)

        # rename all files
        # Iterate through files in the directory
        for filename in os.listdir(outputs_dir):
            if ".fib.gz" in filename:
                # Create the new filename with ".scalar" instead of ".fib.gz"
                new_filename = filename.replace(".fib.gz", "scalar")
                
                # Rename the file
                os.rename(os.path.join(outputs_dir, filename), os.path.join(outputs_dir, new_filename))


        return subject, scalar


# Define scalars
scalars=['ad','dti_fa','gfa','ha','iso','md','qa','rd.','rd1','rd2', 'rdi'] 

# align DSIStudio scalars in subject
for scalar in scalars:
    align_scalars(subject_id, scalar)