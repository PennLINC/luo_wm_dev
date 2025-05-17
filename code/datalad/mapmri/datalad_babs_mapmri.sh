#!/bin/bash
#SBATCH --job-name=babs_mergeds_mapmri
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=5
#SBATCH --array=1-2
#SBATCH --time=6:00:00
#SBATCH --output=/dev/null
#SBATCH --error=/dev/null

# Pick dataset based on array ID
datasets=("HCPD" "HBN")
dataset=${datasets[$SLURM_ARRAY_TASK_ID-1]}

# Redirect stdout and stderr
output_file="/cbica/projects/luo_wm_dev/two_axes/code/logs/datalad/${dataset}/babs_mergeds_mapmri_${dataset}_${SLURM_JOB_ID}_${SLURM_ARRAY_TASK_ID}.out"
error_file="/cbica/projects/luo_wm_dev/two_axes/code/logs/datalad/${dataset}/babs_mergeds_mapmri_${dataset}_${SLURM_JOB_ID}_${SLURM_ARRAY_TASK_ID}.err"
exec > "${output_file}"
exec 2> "${error_file}"

# Directories
dest_dir="/cbica/projects/luo_wm_dev/input/${dataset}/derivatives/mapmri"
src_dir="/cbica/projects/luo_wm_dev/input/${dataset}/derivatives/babs_mapmri/merge_ds"

echo "Processing dataset: ${dataset}"
echo "Source directory: ${src_dir}"
echo "Destination directory: ${dest_dir}"

# Create destination and cd to source
mkdir -p "${dest_dir}" || { echo "Failed to create destination directory"; exit 1; }
cd "${src_dir}" || { echo "Failed to change directory to ${src_dir}"; exit 1; }

# CSV for missing files
missing_log="${dest_dir}/missing_files_${dataset}.csv"
echo "subject,missing_file" > "${missing_log}"

# Adjust these patterns to match the actual paths in your ZIP files

echo "Searching for .zip files in ${src_dir}..."

# Loop over each sub*zip found
for zip_file in $(find . -name 'sub*zip'); do
    sub_id=$(basename "${zip_file%_*}")
    echo
    echo "======================================"
    echo "Processing subject: ${sub_id}"
    echo "Zip path: ${zip_file}"

    sub_dir="${dest_dir}/${sub_id}"
    mkdir -p "${sub_dir}"

    # 1. datalad get
    echo "Running datalad get on ${zip_file}..."
    datalad get "${zip_file}" || {
        echo "  ERROR: datalad_get_failed for ${sub_id}"
        echo "${sub_id},ERROR_datalad_get_failed" >> "${missing_log}"
        continue
    }
    declare -a patterns=(
    "TORTOISE_MAPMRI:qsirecon-TORTOISE/${sub_id}/ses-[^/]+/dwi/.*mapmri_mdp-RTAP_dwimap\.nii\.gz"
    "TORTOISE_MAPMRI:qsirecon-TORTOISE/${sub_id}/ses-[^/]+/dwi/.*mapmri_mdp-RTOP_dwimap\.nii\.gz"
    "TORTOISE_MAPMRI:qsirecon-TORTOISE/${sub_id}/ses-[^/]+/dwi/.*mapmri_mdp-RTPP_dwimap\.nii\.gz"
  )

    # 2. Check each pattern
    for pat in "${patterns[@]}"; do
        modality="${pat%%:*}"
        zip_path="${pat#*:}"
        outdir="${sub_dir}/${modality}"
        mkdir -p "${outdir}"

        # Show which pattern we're looking for
        echo "  Looking for pattern: ${zip_path}"

        # Use unzip -l + grep to see if it exists
        files_found=$(unzip -l "${zip_file}" | awk '{print $4}' | grep -E "${zip_path}")

        if [ -z "${files_found}" ]; then
            # Not found
            echo "    => Missing file for ${modality}: $(basename "${zip_path}")"
            echo "${sub_id},${modality}_$(basename "${zip_path}")" >> "${missing_log}"
        else
            # Found one or more matches
            echo "    => Found file(s):"
            echo "${files_found}" | while read -r f; do
                echo "       ${f}"
            done

            # 3. Unzip each found file into outdir
            #    If your pattern only matches a single file each time,
            #    this will still work for that single match
            for f in ${files_found}; do
                unzip -j "${zip_file}" "${f}" -d "${outdir}" \
                    || echo "${sub_id},ERROR_unzipping_${modality}_$(basename "${f}")" >> "${missing_log}"
            done
        fi
    done

    # 4. Drop the zip from local storage
    echo "Dropping ${zip_file} from datalad..."
    datalad drop "${zip_file}" || echo "${sub_id},ERROR_datalad_drop_failed" >> "${missing_log}"

    echo "Done processing subject: ${sub_id}"
    echo "======================================"
done

echo
echo "All done. Missing files recorded in: ${missing_log}"

 