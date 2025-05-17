#!/bin/bash

datasets=("HCPD" "HBN")    


# submit this with ./submit_datalad_trks.sh

for dataset in "${datasets[@]}"; do
    config_file="/cbica/projects/luo_wm_dev/two_axes/code/config/config_${dataset}.json"
    
    # where to save output and error logs
    logs_dir="/cbica/projects/luo_wm_dev/two_axes/code/logs/scalars_to_tractprofiles/noddi/${dataset}"
    if [ ! -d "${logs_dir}" ]; then
        mkdir -p ${logs_dir}
    fi

    # set dir
    data_root=$(jq -r '.data_root' ${config_file})
    manuscript_data_root=$(jq -r '.manuscript_input_root' ${config_file})
    #qsiprep_dir="${data_root}/raw/datalad_qsiprep"
    
    if [ "${dataset}" = "HBN" ]; then
        pyafq_dir="${data_root}/derivatives/babs_qsirecon_pyafq_allsubs_noACT_v2/merge_ds"
    else
        pyafq_dir="${data_root}/derivatives/babs_qsirecon_pyafq_act_v2/merge_ds"
    fi

    # subjects file
    #subjects_file="${manuscript_data_root}/subject_list/final_sample/${dataset}_WMDev_FinalSample.txt"
    subjects_file="${manuscript_data_root}/subject_list/${dataset}_subject_list_babs.txt"
    mapfile -t subjects_array < <(tail -n +2 ${subjects_file}) # skip header
    for i in "${!subjects_array[@]}"; do
        subjects_array[$i]=$(echo "${subjects_array[$i]}" | tr -d '"') # remove quotes
    done
    subject_count=${#subjects_array[@]} 

    sbatch --parsable \
        --array=0-$((subject_count-1))%10 \
        --job-name="datalad_trks_${dataset}" \
        ./datalad_trks.sh ${subjects_file} ${pyafq_dir} ${logs_dir} "datalad_trks_${dataset}"

done
