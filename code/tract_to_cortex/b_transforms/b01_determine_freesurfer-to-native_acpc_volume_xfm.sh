#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1                           
#SBATCH --cpus-per-task=1                   
#SBATCH --time=00:30:00                    
#SBATCH --output=/dev/null    # suppress default output file
#SBATCH --error=/dev/null     # suppress default error file

# code for transforms adapted from Marc Jaskir (https://github.com/marcjaskir/tracts/blob/main/04-map_tract_volumes_to_surfaces.sh). thanks Marc!!

 
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
  mkdir -p ${derivs_dir}
fi

########################################
# Create deriv directories
########################################   
# Create derivs directories for transforms (including reference volumes) and surfaces converted to GIFTIs
if [ ! -d ${derivs_dir}/${subject}/transforms ]; then
    mkdir -p ${derivs_dir}/${subject}/transforms/freesurfer-to-native_acpc
    mkdir -p ${derivs_dir}/${subject}/surfaces/freesurfer
fi
derivs_dir_xfm=${derivs_dir}/${subject}/transforms/freesurfer-to-native_acpc
derivs_dir_surf=${derivs_dir}/${subject}/surfaces/freesurfer


########################################
# Datalad get required files 
########################################
# unzip -l zipfile.zip shows you the contents of the zip without unzipping it

cd ${freesurfer_dir}

if [[ "$dataset" == *PNC* ]]; then 
   zip_file_pattern="${subject}_freesurfer-20.2.3.zip" # PNC's freesurfer clone has extra stuff in it. merp.
   existing_zip_file=$(find . -name "$zip_file_pattern" | head -n 1)
   if [ -z "$existing_zip_file" ]; then 
        echo "${participant} has no zip file in the folder."  
   else
        datalad get "${zip_file_pattern}"
        echo "freesurfer ${subject} zip successfully gotten"
        unzip -o ${zip_file_pattern} "freesurfer/${subject}/mri/*" "freesurfer/${subject}/surf/*" # -o flag to automatically overwrite
        find . -name ${zip_file_pattern} -exec datalad drop --nocheck {} \; # drop zip to save space
        echo "freesurfer ${subject} zip successfully dropped"
   fi
else 
  if [ ! -d "freesurfer/${subject}" ]; then
      datalad get ${subject}*.zip
      echo "freesurfer ${subject} zip successfully gotten"
      unzip -o ${subject}*.zip "freesurfer/${subject}/mri/*" "freesurfer/${subject}/surf/*" # -o flag to automatically overwrite
      find . -name "${subject}*.zip" -exec datalad drop --nocheck {} \; # drop zip to save space
      echo "freesurfer ${subject} zip successfully dropped"
  fi
fi
 
cd ${qsiprep_dir}
if [ ! -f "qsiprep/${subject}/anat/${subject}_desc-preproc_T1w.nii.gz" ]; then
    datalad get ${subject}*.zip
    echo "qsiprep ${subject} zip successfully gotten"
    unzip -o ${subject}*.zip "qsiprep/${subject}/anat/${subject}_desc-preproc_T1w.nii.gz" # -o flag to automatically overwrite
    # find . -name "${subject}*.zip" -exec datalad drop --nocheck {} \; # drop zip to save space
    echo "qsiprep ${subject} zip successfully dropped"
fi

########################################
# Check for required files
########################################
# Check for a Freesurfer nu.mgz image
if [ ! -f ${SUBJECTS_DIR}/${subject}/mri/nu.mgz ]; then
    echo "No Freesurfer nu.mgz image for ${subject}"
    continue
fi

# Check for a QSIPrep T1w image
if [ ! -f ${qsiprep_dir}/qsiprep/${subject}/anat/${subject}_desc-preproc_T1w.nii.gz ]; then
    echo "No QSIPrep T1w image for ${subject}"
    continue
fi
 
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
  -B ${SUBJECTS_DIR}/${subject}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mri_convert --in_type mgz --out_type nii \
  -i /mnt/mri/nu.mgz -o ${derivs_dir_xfm}/${subject}.freesurfer.nu.LIA.nii.gz
             
 
# Change orientation of Freesurfer reference volumes to LAS+
singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${derivs_dir_xfm}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mri_convert --in_type nii --out_type nii --out_orientation LAS+\
  -i /mnt/${subject}.freesurfer.nu.LIA.nii.gz -o ${derivs_dir_xfm}/${subject}.freesurfer.nu.nii.gz
rm ${derivs_dir_xfm}/${subject}.freesurfer.nu.LIA.nii.gz          


# Convert Freesurfer surfaces to GIFTIs
singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${SUBJECTS_DIR}/${subject}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mris_convert --to-scanner \
  /mnt/surf/lh.pial ${derivs_dir_surf}/${subject}.lh.pial.freesurfer.surf.gii

singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${SUBJECTS_DIR}/${subject}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mris_convert --to-scanner \
  /mnt/surf/rh.pial ${derivs_dir_surf}/${subject}.rh.pial.freesurfer.surf.gii
 
 
singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${SUBJECTS_DIR}/${subject}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mris_convert --to-scanner \
  /mnt/surf/lh.white ${derivs_dir_surf}/${subject}.lh.white.freesurfer.surf.gii

singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${SUBJECTS_DIR}/${subject}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mris_convert --to-scanner \
  /mnt/surf/rh.white ${derivs_dir_surf}/${subject}.rh.white.freesurfer.surf.gii


singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${SUBJECTS_DIR}/${subject}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mris_convert \
  /mnt/surf/lh.sphere.reg ${derivs_dir_surf}/${subject}.lh.sphere.freesurfer.surf.gii

singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${SUBJECTS_DIR}/${subject}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mris_convert \
  /mnt/surf/rh.sphere.reg ${derivs_dir_surf}/${subject}.rh.sphere.freesurfer.surf.gii 
  # note that for sphere.reg, we DON'T want the --to-scanner flag!!
  # it messes up wb_shortcuts -freesurfer-resample-prep down the line
  # see Marc's slack channel 
 
 
###############
# QSIPrep
###############
# Change orientation of QSIPrep T1w files to LAS+
singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${qsiprep_dir}/qsiprep/${subject}/anat/:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  mri_convert --in_type nii --out_type nii --out_orientation LAS+\
  -i /mnt/${subject}_desc-preproc_T1w.nii.gz  -o ${derivs_dir_xfm}/${subject}.native_acpc.desc-preproc_T1w.nii.gz
 

########################################
# Warp Freesurfer volume to QSIPrep volume
########################################

# Compute affine
flirt -in ${derivs_dir_xfm}/${subject}.freesurfer.nu.nii.gz \
    -ref ${derivs_dir_xfm}/${subject}.native_acpc.desc-preproc_T1w.nii.gz \
    -out ${derivs_dir_xfm}/${subject}.native_acpc.nu.nii.gz \
    -omat ${derivs_dir_xfm}/${subject}.freesurfer-to-native_acpc.xfm.mat

# Convert affine to lta format
singularity exec --writable-tmpfs \
  -B ${SUBJECTS_DIR}:/opt/freesurfer/subjects \
  -B ${derivs_dir_xfm}:/mnt \
  -B ${freesurfer_license}:/opt/freesurfer/license.txt \
  $freesurfer_sif \
  lta_convert --infsl /mnt/${subject}.freesurfer-to-native_acpc.xfm.mat \
            --src /mnt/${subject}.freesurfer.nu.nii.gz \
            --trg /mnt/${subject}.native_acpc.desc-preproc_T1w.nii.gz \
            --outlta /mnt/${subject}.freesurfer-to-native_acpc.xfm.lta
 
rm ${derivs_dir_xfm}/${subject}.freesurfer-to-native_acpc.xfm.mat

########################################
# Clean up files
########################################   
rm -rf ${freesurfer_dir}/freesurfer/${subject}
