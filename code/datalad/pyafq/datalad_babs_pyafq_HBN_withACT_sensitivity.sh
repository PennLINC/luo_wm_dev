#!/bin/bash
#SBATCH --job-name=babs_mergeds_tractprofiles
#SBATCH --nodes=1
#SBATCH --ntasks=1                           
#SBATCH --cpus-per-task=5                   
#SBATCH --array=1-3     
#SBATCH --time=3:00:00                    
#SBATCH --output=/dev/null    # suppress default output file
#SBATCH --error=/dev/null     # suppress default error file

datasets=("HBN")

# get the dataset corresponding to this task
dataset=${datasets[$SLURM_ARRAY_TASK_ID-1]}
 
# Define the log filenames dynamically based on the dataset
output_file="/cbica/projects/luo_wm_dev/two_axes/code/logs/datalad/${dataset}/babs_mergeds_tractprofiles_${dataset}_${SLURM_JOB_ID}_${SLURM_ARRAY_TASK_ID}_withACT.out"
error_file="/cbica/projects/luo_wm_dev/two_axes/code/logs/datalad/${dataset}/babs_mergeds_tractprofiles_${dataset}_${SLURM_JOB_ID}_${SLURM_ARRAY_TASK_ID}_withACT.err"

# Redirect stdout and stderr to the desired log files
exec > "${output_file}"
exec 2> "${error_file}"

dest_dir="/cbica/projects/luo_wm_dev/input/${dataset}/derivatives/tract_profiles_ACT_v2"
src_dir="/cbica/projects/luo_wm_dev/input/${dataset}/derivatives/babs_qsirecon_pyafq_act_v2/merge_ds"

echo "Processing dataset: ${dataset}"
echo "Source directory: ${src_dir}"
echo "Destination directory: ${dest_dir}"

# create directory if it doesn't exist
mkdir -p "${dest_dir}" || { echo "Failed to create destination directory"; exit 1; }

cd "${src_dir}" || { echo "Failed to change directory to ${src_dir}"; exit 1; }

export dest_dir

# parallel processing 
find . -name 'sub*zip' | parallel -j 5 '
    file={}
    sub=$(basename ${file%_*})

    echo "Processing file: $file"
    echo "Destination directory: ${dest_dir}"
    echo "Subject: ${sub}"

    
    if ! [ -d "${dest_dir}/${sub}" ]; then

        echo "Getting ${file}"
        datalad get "${file}"
        mkdir -p "${dest_dir}/${sub}"
        
        # unzip only the tract profiles csv
        unzip -j "${file}" "qsirecon-PYAFQ/${sub}/*/dwi/*/*profiles_tractography.csv" -d "${dest_dir}/${sub}"
        
        datalad drop "${file}"  # drop the zip
    fi
' dest_dir="${dest_dir}"
 
 