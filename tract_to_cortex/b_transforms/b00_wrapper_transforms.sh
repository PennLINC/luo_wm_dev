#!/bin/bash

# submit this with ./b00_wrapper_transforms.sh
datasets=( "PNC" "HCPD" "HBN") 

for dataset in "${datasets[@]}"; do
    config_file="/cbica/projects/luo_wm_dev/two_axes_manuscript/code/config/config_${dataset}.json"
    
    # where to save output and error logs
    logs_dir="/cbica/projects/luo_wm_dev/two_axes_manuscript/code/logs/tract_to_cortex/b_transforms/${dataset}"
    if [ ! -d "${logs_dir}" ]; then
        mkdir -p ${logs_dir}
    fi

    # set dir
    data_root=$(jq -r '.data_root' ${config_file})
    
    # subjects file
    subjects_file="${data_root}/subject_list/final_sample/${dataset}_WMDev_FinalSample.txt"
    mapfile -t subjects_array < <(tail -n +2 ${subjects_file}) # skip header
    for i in "${!subjects_array[@]}"; do
        subjects_array[$i]=$(echo "${subjects_array[$i]}" | tr -d '"') # remove quotes
    done
    subject_count=${#subjects_array[@]} 
    
    jid_b01=$(sbatch --parsable \
        --array=0-$((subject_count-1))%2 \
        --job-name="b01_determine_freesurfer-to-native_acpc_volume_xfm_${dataset}" \
        ./b01_determine_freesurfer-to-native_acpc_volume_xfm.sh ${subjects_file} ${config_file} ${logs_dir} "b01_determine_freesurfer-to-native_acpc_volume_xfm_${dataset}")

    sbatch --parsable \
        --dependency=afterok:${jid_b01} \
        --array=0-$((subject_count-1)) \
        --job-name="b02_align_surfaces_with_tract_volumes_${dataset}" \
        ./b02_align_surfaces_with_tract_volumes.sh ${subjects_file} ${config_file} ${logs_dir} "b02_align_surfaces_with_tract_volumes_${dataset}"

done
