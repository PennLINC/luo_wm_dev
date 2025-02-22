#!/bin/bash

# submit this with ./c00_wrapper_vol_to_surf.sh
datasets=("PNC" "HCPD" "HBN")  

for dataset in "${datasets[@]}"; do
    config_file="/cbica/projects/luo_wm_dev/two_axes_manuscript/code/config/config_${dataset}.json"
    
    # Set where to save out error/output logs
    logs_dir="/cbica/projects/luo_wm_dev/two_axes_manuscript/code/logs/tract_to_cortex/c_vol_to_surf/${dataset}"
    if [ ! -d "${logs_dir}" ]; then
        mkdir -p ${logs_dir}
    fi

    # set dir
    data_root=$(jq -r '.data_root' ${config_file})
    manuscript_data_root=$(jq -r '.manuscript_input_root' ${config_file})

    # Create directory for vol_to_surf outputs
    if [ ! -d "${manuscript_data_root}/derivatives/vol_to_surf" ]; then
        mkdir -p ${manuscript_data_root}/derivatives/vol_to_surf
    fi

    # subjects file
    subjects_file="${manuscript_data_root}/subject_list/final_sample/${dataset}_WMDev_FinalSample.txt"
    mapfile -t subjects_array < <(tail -n +2 ${subjects_file}) # skip header
    for i in "${!subjects_array[@]}"; do
        subjects_array[$i]=$(echo "${subjects_array[$i]}" | tr -d '"') # remove quotes
    done
    subject_count=${#subjects_array[@]} 

    jid_c01=$(sbatch --parsable \
        --array=0-$((subject_count-1)) \
        --job-name="c01_vol_to_surf_individual_${dataset}" \
        ./c01_vol_to_surf_individual.sh ${subjects_file} ${config_file} ${logs_dir} "c01_vol_to_surf_individual_${dataset}")

    sbatch --parsable \
        --dependency=afterok:${jid_c01} \
        --array=0-$((subject_count-1)) \
        --job-name="c02_native_to_fsLR_${dataset}" \
        ./c02_native_to_fsLR.sh ${subjects_file} ${config_file} ${logs_dir} "c02_native_to_fsLR_${dataset}"

done
