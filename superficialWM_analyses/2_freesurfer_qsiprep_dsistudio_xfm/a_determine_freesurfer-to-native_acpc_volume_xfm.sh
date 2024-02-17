#!/bin/bash


##############################################
# Set config file (based on dataset to analyze)
##############################################
 
# Parse command-line arguments
while getopts ":c:" opt; do
  case $opt in
    c) config_file="$OPTARG"
    ;;
    \?) echo "Invalid option: -$OPTARG" >&2
    ;;
  esac
done

# Check if the config_file variable is set
if [ -z "$config_file" ]; then
  echo "Error: Please specify a config file using the -c option." >&2
  exit 1
fi

 
# qsub your_script.sh -c config_dataset.json


########################################
# Set directories
########################################
dataset=$(jq -r '.dataset' "$config_file")

data_root=$(jq -r '.data_root' "$config_file")
outputs_root=$(jq -r '.outputs_root' "$config_file")

SUBJECTS_DIR="${data_root}/${dataset}_freesurfer"


########################################
# Read in subject ID
########################################
sub=${@:OPTIND}
 
########################################
# Check for required files
########################################

# Check for a Freesurfer nu.mgz image
if [ ! -f ${SUBJECTS_DIR}/${sub}/mri/nu.mgz ]; then
    echo "No Freesurfer nu.mgz image for ${sub}"
    continue
fi

# Check for a QSIPrep T1w image
if [ ! -f ${data_root}/${dataset}_qsiprep/${sub}/${sub}_desc-preproc_T1w.nii.gz ]; then
    echo "No QSIPrep T1w image for ${sub}"
    continue
fi

# Check for a QSIPrep dwiref image
if [ ! -f ${data_root}/${dataset}_qsiprep/${sub}/${sub}_*-T1w_dwiref.nii.gz ]; then
    echo "No QSIPrep dwiref image for ${sub}"
    continue
fi

########################################
# Create output directories
########################################   

# Create output directories for transforms (including reference volumes) and surfaces converted to GIFTIs
if [ ! -d ${outputs_root}/${sub}/transforms ]; then
    mkdir -p ${outputs_root}/${sub}/transforms/freesurfer-to-native_acpc
    mkdir -p ${outputs_root}/${sub}/surfaces/freesurfer
fi
outputs_dir_xfm=${outputs_root}/${sub}/transforms/freesurfer-to-native_acpc
outputs_dir_surf=${outputs_root}/${sub}/surfaces/freesurfer



########################################
# Harmonize filetypes and orientations of Freesurfer and QSIPrep images with voxelized tracts
########################################

###############
# Freesurfer  
###############
# set variables for singularity
freesurfer_license="/cbica/projects/luo_wm_dev/software/freesurfer/license.txt"
freesurfer_sif="/cbica/projects/luo_wm_dev/software/freesurfer/fmriprep-20.2.3.sif"
 

# Convert Freesurfer reference volumes (nu.mgz) to NIFTIs
singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${SUBJECTS_DIR}/${sub}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mri_convert --in_type mgz --out_type nii \
  -i /mnt/mri/nu.mgz -o ${outputs_dir_xfm}/${sub}.freesurfer.nu.LIA.nii.gz
             
 

# Change orientation of Freesurfer reference volumes to LAS+
singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${outputs_dir_xfm}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mri_convert --in_type nii --out_type nii --out_orientation LAS+\
  -i /mnt/${sub}.freesurfer.nu.LIA.nii.gz -o ${outputs_dir_xfm}/${sub}.freesurfer.nu.nii.gz
rm ${outputs_dir_xfm}/${sub}.freesurfer.nu.LIA.nii.gz          



# Convert Freesurfer surfaces to GIFTIs
singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${SUBJECTS_DIR}/${sub}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mris_convert --to-scanner \
  /mnt/surf/lh.pial ${outputs_dir_surf}/${sub}.lh.pial.freesurfer.surf.gii

singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${SUBJECTS_DIR}/${sub}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mris_convert --to-scanner \
  /mnt/surf/rh.pial ${outputs_dir_surf}/${sub}.rh.pial.freesurfer.surf.gii
 
 
singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${SUBJECTS_DIR}/${sub}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mris_convert --to-scanner \
  /mnt/surf/lh.white ${outputs_dir_surf}/${sub}.lh.white.freesurfer.surf.gii

singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${SUBJECTS_DIR}/${sub}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mris_convert --to-scanner \
  /mnt/surf/rh.white ${outputs_dir_surf}/${sub}.rh.white.freesurfer.surf.gii


singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${SUBJECTS_DIR}/${sub}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mris_convert --to-scanner \
  /mnt/surf/lh.sphere.reg ${outputs_dir_surf}/${sub}.lh.sphere.freesurfer.surf.gii

singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${SUBJECTS_DIR}/${sub}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mris_convert --to-scanner \
  /mnt/surf/rh.sphere.reg ${outputs_dir_surf}/${sub}.rh.sphere.freesurfer.surf.gii
 

singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${SUBJECTS_DIR}/${sub}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mris_convert --to-scanner \
  /mnt/surf/lh.midthickness ${outputs_dir_surf}/${sub}.lh.midthickness.freesurfer.surf.gii

singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${SUBJECTS_DIR}/${sub}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mris_convert --to-scanner \
  /mnt/surf/rh.midthickness ${outputs_dir_surf}/${sub}.rh.midthickness.freesurfer.surf.gii
 
singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${SUBJECTS_DIR}/${sub}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mris_convert --to-scanner \
  /mnt/surf/lh.inflated ${outputs_dir_surf}/${sub}.lh.inflated.freesurfer.surf.gii

singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${SUBJECTS_DIR}/${sub}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mris_convert --to-scanner \
  /mnt/surf/rh.inflated ${outputs_dir_surf}/${sub}.rh.inflated.freesurfer.surf.gii
 
     
    
###############
# QSIPrep
###############

# Change orientation of QSIPrep T1w files to LAS+
 singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${data_root}/${dataset}_qsiprep/${sub}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mri_convert --in_type nii --out_type nii --out_orientation LAS+\
  -i /mnt/${sub}_desc-preproc_T1w.nii.gz  -o ${outputs_dir_xfm}/${sub}.native_acpc.desc-preproc_T1w.nii.gz
 

# Change orientation of QSIPrep dwiref files to LAS+
singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${data_root}/${dataset}_qsiprep/${sub}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mri_convert --in_type nii --out_type nii --out_orientation LAS+\
  -i /mnt/${sub}_ses-V1_space-T1w_dwiref.nii.gz  -o ${outputs_dir_xfm}/${sub}.native_acpc.T1_dwiref.nii.gz
 


# Change orientation of QSIPrep WMprobseg and GMprobseg files to LAS+
singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${data_root}/${dataset}_qsiprep/${sub}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mri_convert --in_type nii --out_type nii --out_orientation LAS+\
  -i /mnt/${sub}_label-GM_probseg.nii.gz  -o ${outputs_dir_xfm}/${sub}.native_acpc.GM_probseg.nii.gz
 

singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${data_root}/${dataset}_qsiprep/${sub}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mri_convert --in_type nii --out_type nii --out_orientation LAS+\
  -i /mnt/${sub}_label-WM_probseg.nii.gz  -o ${outputs_dir_xfm}/${sub}.native_acpc.WM_probseg.nii.gz

 

 
########################################
# Warp Freesurfer volume to QSIPrep volume
########################################

# Compute affine
flirt -in ${outputs_dir_xfm}/${sub}.freesurfer.nu.nii.gz \
    -ref ${outputs_dir_xfm}/${sub}.native_acpc.desc-preproc_T1w.nii.gz \
    -out ${outputs_dir_xfm}/${sub}.native_acpc.nu.nii.gz \
    -omat ${outputs_dir_xfm}/${sub}.freesurfer-to-native_acpc.xfm.mat

# Convert affine to lta format
singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${outputs_dir_xfm}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  lta_convert --infsl /mnt/${sub}.freesurfer-to-native_acpc.xfm.mat \
            --src /mnt/${sub}.freesurfer.nu.nii.gz \
            --trg /mnt/${sub}.native_acpc.desc-preproc_T1w.nii.gz \
            --outlta /mnt/${sub}.freesurfer-to-native_acpc.xfm.lta
 
rm ${outputs_dir_xfm}/${sub}.freesurfer-to-native_acpc.xfm.mat