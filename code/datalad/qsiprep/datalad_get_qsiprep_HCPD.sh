#!/bin/bash
#SBATCH --job-name=datalad_get_qsiprep_HCPD
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --time=12:00:00
#SBATCH --propagate=NONE
#SBATCH --output=/cbica/projects/luo_wm_dev/two_axes_manuscript/code/logs/datalad/HCPD/qsiprep_%j.out
#SBATCH --error=/cbica/projects/luo_wm_dev/two_axes_manuscript/code/logs/datalad/HCPD/qsiprep_%j.err

cd /cbica/projects/luo_wm_dev/input/HCPD/raw/datalad_qsiprep
missing_files=$(git annex find --not --in here)

# in case missing qsiprep outputs
no_zip_file_list="/cbica/projects/luo_wm_dev/two_axes_manuscript/input/HCPD/sample_selection_files/missing_qsiprep.txt"
> $no_zip_file_list

while IFS= read -r participant; do
    zip_file_pattern="${participant}_qsiprep-0.16.1.zip"
    existing_zip_file=$(find . -name "$zip_file_pattern" | head -n 1)
    if [ -z "$existing_zip_file" ]; then 
        echo "${participant} has no zip file in the folder." >> $no_zip_file_list; 
    elif echo "$missing_files" | grep -q "$zip_file_pattern"; then
        echo "${participant}'s needs to be gotten. Attempting to download..."
        datalad get "$zip_file_pattern"
    fi
done < /cbica/projects/luo_wm_dev/two_axes_manuscript/input/HCPD/subject_list/HCPD_subject_list_babs.txt

echo "Participants with no zip file have been logged in: $no_zip_file_list"
