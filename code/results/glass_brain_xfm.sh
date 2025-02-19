#!/bin/bash

# transformed a freesurfer brain to match qsiprep headers
# for glass brain plotting
 
########################################
# Set directories
########################################
dataset="PNC"
config_file="/cbica/projects/luo_wm_dev/two_axes_manuscript/code/config/config_${dataset}.json"

data_root=$(jq -r '.data_root' "$config_file")

freesurfer_dir="${data_root}/raw/datalad_freesurfer/inputs/data"
qsiprep_dir="${data_root}/raw/datalad_qsiprep"
SUBJECTS_DIR="${freesurfer_dir}/freesurfer"


########################################
# Read in subject ID
########################################
subject="sub-1000393599"

########################################
# Get required files
########################################
# Freesurfer
cd ${freesurfer_dir}
datalad get ${subject}*.zip
mkdir -p "freesurfer/${subject}/mri"
echo "Freesurfer ${subject} zip successfully gotten"
unzip -j ${subject}*freesurfer*.zip "freesurfer/${subject}/mri/brain.mgz" -d "freesurfer/${subject}/mri" # unzip and copy over just this file
find . -name "${subject}*.zip" -exec datalad drop --nocheck {} \; # drop zip to save space
echo "Freesurfer ${subject} zip successfully dropped"

# QSIPrep
cd ${qsiprep_dir}
datalad get ${subject}*.zip
echo "Qsiprep ${subject} zip successfully gotten"
mkdir -p "qsiprep/${subject}/anat/"
unzip -j ${subject}*.zip "qsiprep/${subject}/anat/${subject}_desc-preproc_T1w.nii.gz" -d "qsiprep/${subject}/anat"
unzip -j ${subject}*.zip "qsiprep/${subject}/anat/${subject}_desc-brain_mask.nii.gz" -d "qsiprep/${subject}/anat"
find . -name "${subject}*.zip" -exec datalad drop --nocheck {} \; # drop zip to save space
echo "Qsiprep ${subject} zip successfully dropped" 

########################################
# Transformation time!
########################################  
# set variables
run_qsiprep_cmd="singularity exec /cbica/projects/luo_wm_dev/software/qsiprep/qsiprep-0.21.4.sif"
workdir="${data_root}/glass_brain"
 
if [ ! -d ${workdir}/${subject} ]; then
    mkdir -p ${workdir}/${subject}
fi

# Convert FS brain from .mgz to .nii
${run_qsiprep_cmd} mrconvert -strides -1,-2,3 \
    ${SUBJECTS_DIR}/${subject}/mri/brain.mgz ${workdir}/${subject}/fs_brain.nii


# Register FreeSurfer brain to QSIPrep T1w
${run_qsiprep_cmd} antsRegistration --collapse-output-transforms 1 \
    --dimensionality 3 --float 0 \
    --initial-moving-transform [ ${qsiprep_dir}/qsiprep/${subject}/anat/${subject}_desc-preproc_T1w.nii.gz, ${workdir}/${subject}/fs_brain.nii, 1 ] \
    --initialize-transforms-per-stage 0 --interpolation BSpline \
    --output [ ${workdir}/${subject}/transform, ${workdir}/${subject}/transform_Warped.nii.gz ] \
    --transform Rigid[ 0.1 ] \
    --metric Mattes[ ${qsiprep_dir}/qsiprep/${subject}/anat/${subject}_desc-preproc_T1w.nii.gz, ${workdir}/${subject}/fs_brain.nii, 1, 32, Random, 0.25 ] \
    --convergence [ 1000x500x250x100, 1e-06, 10 ] \
    --smoothing-sigmas 3.0x2.0x1.0x0.0mm --shrink-factors 8x4x2x1 \
    --use-histogram-matching 0 \
    --masks [ ${qsiprep_dir}/qsiprep/${subject}/anat/${subject}_desc-brain_mask.nii.gz, NULL ] \
    --winsorize-image-intensities [ 0.002, 0.998 ] \
    --write-composite-transform 0
echo "${subject} registered FreeSurfer brain to QSIPrep T1w"

# Convert ANTs .mat transform to .txt, and rename it
${run_qsiprep_cmd} ConvertTransformFile 3 ${workdir}/${subject}/transform0GenericAffine.mat ${workdir}/${subject}/${subject}_from-FS_to-T1wACPC_mode-image_xfm.txt

  
 
${run_qsiprep_cmd} antsApplyTransforms \
    -d 3 \
    -i ${workdir}/${subject}/fs_brain.nii \
    -r ${qsiprep_dir}/qsiprep/${subject}/anat/${subject}_desc-preproc_T1w.nii.gz \
    -o ${workdir}/${subject}/fs_brain_transformed.nii.gz \
    -n NearestNeighbor \
    -t ${workdir}/${subject}/transform0GenericAffine.mat
echo "${subject} brain.mgz transformed to to QSIPrep T1w"
 