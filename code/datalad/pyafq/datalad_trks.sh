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
# define variables passed from the submission script
subjects_file=$1  
pyafq_dir=$2
#qsiprep_dir=$3
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
 
echo "Datalad getting pyafq trk's for ${subject}"

################# 
# get trk files #
################# 

cd ${pyafq_dir}
echo ${pyafq_dir}
if [ ! -d "qsirecon-PYAFQ/${subject}" ]; then
    if datalad get ${subject}*.zip; then
        echo "PyAFQ ${subject} zip successfully gotten"
        unzip -o ${subject}*.zip "qsirecon-PYAFQ/${subject}/*/dwi/${subject}*/bundles/*trk" -d ${pyafq_dir} 
        if [ $? -ne 0 ]; then
            echo "Error: Failed to unzip ${subject} files"
            exit 1  
        fi
        find . -name "${subject}*.zip" -exec datalad drop --nocheck {} \;
        echo "PyAFQ ${subject} zip successfully dropped"
    else
        echo "Error: Failed to get PyAFQ ${subject} zip"
        exit 1   
    fi
fi
