#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1                           
#SBATCH --cpus-per-task=1                   
#SBATCH --time=00:30:00                    
#SBATCH --output=/dev/null    # suppress default output file
#SBATCH --error=/dev/null     # suppress default error file

# code for transforms adapted from Marc Jaskir (https://github.com/marcjaskir/tracts/blob/main/04-map_tract_volumes_to_surfaces.sh). you're my hero!!

######################
# setup for job array 
######################
# define variables passed from the submission script
subjects_file=$1  
config_file=$2
logs_dir=$3
job_name=$4

data_root=$(jq -r '.data_root' ${config_file})
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
module load fsl/6.0.4

########################################
# Set directories
########################################
qsiprep_dir="${data_root}/raw/datalad_qsiprep/"
freesurfer_dir="${data_root}/raw/datalad_freesurfer/inputs/data/"
SUBJECTS_DIR="${data_root}/raw/datalad_freesurfer/inputs/data/freesurfer/"
derivs_dir="${data_root}/derivatives/fs_qsiprep_xfm"

if [ ! -d "${derivs_dir}" ]; then
  echo "Missing ${derivs_dir}"
  #exit 1
fi

########################################
# Create deriv directories
########################################   
# Create derivs directories for transforms (including reference volumes) and surfaces converted to GIFTIs
if [ ! -d ${derivs_dir}/${subject}/transforms ]; then
   echo "Missing ${derivs_dir}/${subject}/transforms/"
   mkdir -p ${derivs_dir}/${subject}/transforms/
   #exit 1
fi

derivs_dir_xfm=${derivs_dir}/${subject}/transforms/freesurfer-to-native_acpc
derivs_dir_surf=${derivs_dir}/${subject}/surfaces/freesurfer
 
if [ ! -d ${derivs_dir_xfm} ]; then
   mkdir -p ${derivs_dir_xfm} 
fi

if [ ! -d ${derivs_dir_surf} ]; then
   mkdir -p ${derivs_dir_surf} 
fi
###############
# Freesurfer  
###############
# set variables for singularity
freesurfer_license="/cbica/projects/luo_wm_dev/software/freesurfer/license.txt"
freesurfer_sif="/cbica/projects/luo_wm_dev/software/freesurfer/fmriprep-20.2.3.sif"
 
# Change orientation of QSIPrep WMprobseg and GMprobseg files to LAS+
singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${qsiprep_dir}/qsiprep/${subject}/anat/:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mri_convert --in_type nii --out_type nii --out_orientation LAS+\
  -i /mnt/${subject}_label-GM_probseg.nii.gz  -o ${derivs_dir_xfm}/${subject}.native_acpc.GM_probseg.nii.gz
 

 singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${qsiprep_dir}/qsiprep/${subject}/anat/:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mri_convert --in_type nii --out_type nii --out_orientation LAS+\
  -i /mnt/${subject}_label-WM_probseg.nii.gz  -o ${derivs_dir_xfm}/${subject}.native_acpc.WM_probseg.nii.gz
  