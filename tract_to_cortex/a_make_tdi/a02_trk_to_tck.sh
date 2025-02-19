#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1                           
#SBATCH --cpus-per-task=1                
#SBATCH --time=02:00:00                    
#SBATCH --output=/dev/null    # suppress default output file
#SBATCH --error=/dev/null     # suppress default error file
 
######################
# setup for job array 
######################
# define variables passed from the submission script
subjects_file=$1  
pyafq_dir=$2
logs_dir=$3
config_file=$4
job_name=$5

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
 
echo "Converting trk to tck for ${subject}"

######################
# run python script 
######################
 
source /cbica/projects/luo_wm_dev/miniconda3/etc/profile.d/conda.sh
conda activate luo_wm_dev

# run python script
python a02_trk_to_tck.py ${subject} ${config_file} ${pyafq_dir} || { echo "Python script failed for ${subject}"; exit 1; }
