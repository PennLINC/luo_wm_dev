#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1                           
#SBATCH --cpus-per-task=1                  
#SBATCH --time=00:30:00                    
#SBATCH --output=/dev/null    # suppress default output file
#SBATCH --error=/dev/null     # suppress default error file

######################
# setup for job array 
######################
subjects_file=$1 
pyafq_dir=$2
logs_dir=$3
job_name=$4
 
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
 
echo "Cleaning up trks for ${subject}"

######################
# Delete trks 
######################
if [ -d "${pyafq_dir}/qsirecon-PYAFQ/${subject}" ]; then
    rm -rf ${pyafq_dir}/qsirecon-PYAFQ/${subject}
    echo "Deleted directory: ${pyafq_dir}/qsirecon-PYAFQ/${subject}"
else
    echo "Directory not found: ${pyafq_dir}/qsirecon-PYAFQ/${subject}"
fi

if [ -f "${pyafq_dir}/qsirecon-PYAFQ/${subject}.html" ]; then
    rm ${pyafq_dir}/qsirecon-PYAFQ/${subject}.html
    echo "Deleted file: ${pyafq_dir}/qsirecon-PYAFQ/${subject}.html"
else
    echo "File not found: ${pyafq_dir}/qsirecon-PYAFQ/${subject}.html"
fi
