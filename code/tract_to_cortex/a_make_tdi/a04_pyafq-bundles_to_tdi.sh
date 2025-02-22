#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1                           
#SBATCH --cpus-per-task=1                   
#SBATCH --time=01:00:00                    
#SBATCH --output=/dev/null    # suppress default output file
#SBATCH --error=/dev/null     # suppress default error file
 
######################
# setup for job array 
######################
# define variables passed from the submission script
subjects_file=$1  
config_file=$2
logs_dir=$3
job_name=$4

data_root=$(jq -r '.data_root' ${config_file})
manuscript_data_root=$(jq -r '.manuscript_input_root' ${config_file})
dataset=$(jq -r '.dataset' ${config_file})

mapfile -t subjects_array < <(tail -n +2 ${subjects_file}) # skip header
for i in "${!subjects_array[@]}"; do
    subjects_array[$i]=$(echo "${subjects_array[$i]}" | tr -d '"') # remove quotes
done


# get the subject corresponding to my job array element
subject=${subjects_array[$SLURM_ARRAY_TASK_ID]}

# define the log filenames dynamically based on subject
output_file="${logs_dir}/${job_name}_${subject}_${SLURM_JOB_ID}_${SLURM_ARRAY_TASK_ID}.out"
error_file="${logs_dir}/${job_name}_${subject}_${SLURM_JOB_ID}_${SLURM_ARRAY_TASK_ID}.err"

# Redirect stdout and stderr to the desired log files
exec > "${output_file}"
exec 2> "${error_file}"
 
########################################
# Load modules
########################################
module load gcc/5.2.0  # gcc/9.4.0 has some issues on cubic
module load mrtrix/3.0.4 

########################################
# Check for required files
########################################
echo "Converting bundles to TDI for ${subject}"

# Check if converted .tck files exist
if ! find "${manuscript_data_root}/derivatives/tck_temp/${subject}" -name "*.tck" -print -quit | grep -q .; then
    echo "No tract (.tck) files for ${subject}"
    exit 1 
fi

########################################
# Create output directories
########################################
# Create output directories for tract density maps
if [ ! -d ${manuscript_data_root}/derivatives/tdi_maps/${subject} ]; then
    mkdir -p ${manuscript_data_root}/derivatives/tdi_maps/${subject}/mgz
    mkdir -p ${manuscript_data_root}/derivatives/tdi_maps/${subject}/nifti
    mkdir -p ${manuscript_data_root}/derivatives/tdi_maps/${subject}/tdi_binarized
fi
outputs_dir_mgz=${manuscript_data_root}/derivatives/tdi_maps/${subject}/mgz
outputs_dir_nifti=${manuscript_data_root}/derivatives/tdi_maps/${subject}/nifti
outputs_dir_tdi=${manuscript_data_root}/derivatives/tdi_maps/${subject}/tdi_binarized

########################################
# Convert tck to TDI maps
########################################
# Iterate over tract (.tck) files
for tract in ${manuscript_data_root}/derivatives/tck_temp/${subject}/*.tck; do
    
    # Extract file name (without .tck extension)
    tract_fname=$(basename ${tract} | sed 's/.tck//g')
    tract_label=$(echo ${tract_fname} | cut -d'-' -f6 | cut -d'_' -f1)

    template=${data_root}/raw/datalad_qsiprep/qsiprep/${subject}/*/dwi/${subject}_*-T1w_dwiref.nii.gz

    # Convert tck files to TDI maps
    if [ ! -f ${outputs_dir_mgz}/${tract_fname}.mgz ]; then
        tckmap ${tract} -template ${template} -contrast tdi ${outputs_dir_mgz}/${subject}_${tract_label}.mgz
    fi

    # Convert TDI maps to nifti
    mri_convert --in_type mgz \
        --out_type nii \
        ${outputs_dir_mgz}/${subject}_${tract_label}.mgz \
        ${outputs_dir_nifti}/${subject}_${tract_label}.LPS.nii.gz

    # Change orientation of NIFTIs to LAS+  
    mri_convert --in_type nii \
                --out_type nii \
                --out_orientation LAS+ \
                ${outputs_dir_nifti}/${subject}_${tract_label}.LPS.nii.gz \
                ${outputs_dir_nifti}/${subject}_${tract_label}.LAS.nii.gz

    mrcalc ${outputs_dir_nifti}/${subject}_${tract_label}.LAS.nii.gz 0 -gt 1 0 -if ${outputs_dir_tdi}/${subject}_${tract_label}_binarized.nii.gz
    # all voxels with nonzero TDI are set to 1. All voxels with TDI = 0 are set to 0. 
done
 
