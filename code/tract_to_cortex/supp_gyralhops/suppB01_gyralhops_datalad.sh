#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1                           
#SBATCH --cpus-per-task=1                   
#SBATCH --time=00:30:00                    
#SBATCH --output=/dev/null    # suppress default output file
#SBATCH --error=/dev/null     # suppress default error file
 
# datalad get t1w, dwiref, GM_probseg, WM_probseg from qsiprep repo

######################
# setup for job array 
######################
# define variables passed from the submission script
subjects_file=$1  
qsiprep_dir=$2
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
 
echo "Datalad getting qsiprep probseg files for ${subject}"

######################
# get required files #
######################
cd ${qsiprep_dir}

# function to check if a file exists
check_file_exists() {
    local pattern=$1
    find "qsiprep/${subject}" -type f -path "$pattern" | grep -q ".*"
}

# required files and their paths
files_to_check=(
    "*/*/dwi/${subject}*T1w_dwiref.nii.gz"
    "*/anat/${subject}_desc-preproc_T1w.nii.gz"
    "*/anat/${subject}_label-GM_probseg.nii.gz"
    "*/anat/${subject}_label-WM_probseg.nii.gz"
)

# datalad get necessary files
for filepath in "${files_to_check[@]}"; do
    if ! check_file_exists "$filepath"; then
    
        echo "File $filepath is missing. Attempting to retrieve..."

        # get the qsiprep zip file
        if datalad get ${subject}*.zip; then
            echo "Qsiprep ${subject} zip successfully gotten"

            zip_file=$(find . -name "${subject}*.zip")
            unzip -o "$zip_file" "$filepath"  
      
        else
            echo "Error: Failed to get qsiprep ${subject} zip"
            exit 1
        fi
    else
        echo "File $filepath already exists. Skipping..."
    fi
done

# drop the zip file
find . -name "${subject}*.zip" -exec datalad drop --nocheck {} \;
echo "Qsiprep ${subject} zip successfully dropped"
 