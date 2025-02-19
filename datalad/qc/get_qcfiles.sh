#!/bin/bash

# dataset = first argument
dataset=$1  

# make qc dir
qc_dir="/cbica/projects/luo_wm_dev/input/${dataset}/raw/qc_files"
mkdir -p "${qc_dir}"

# go to qsiprep clone
cd /cbica/projects/luo_wm_dev/input/${dataset}/raw/datalad_qsiprep

# identify files already downloaded and get the qc csv
downloaded_files=$(git annex find --in here)
for file in ${downloaded_files}; do
    sub=${file%_*}

    # unzip only the qc csv files
    echo "Extracting qc files from: $file"
    unzip -j "$file" "qsiprep/${sub}/ses-*/dwi/*csv" -d "${qc_dir}"
done

# identify missing zip files
missing_files=$(git annex find --not --in here)

if [[ -z "$missing_files" ]]; then
    echo "No missing zip files to retrieve."
    exit 0
fi

# get qc files from each zip 
for file in ${missing_files}; do
    echo "Retrieving: $file"
    if ! datalad get "$file"; then
        echo "Failed to retrieve $file. Skipping..." >&2
        continue
    fi

    sub=${file%_*}

    # unzip only the qc csv files
    echo "Extracting qc files from: $file"
    unzip -j "$file" "qsiprep/${sub}/ses-*/dwi/*csv" -d "${qc_dir}"

    # drop after extracting
    datalad drop --nocheck "$file"
    
done

echo "Datalad getting qc file and extraction complete."