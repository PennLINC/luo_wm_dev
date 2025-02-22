#!/bin/bash

datasets=( "PNC" "HCPD" "HBN")    

# submit this with ./a00_wrapper_make_tdi.sh

for dataset in "${datasets[@]}"; do
    config_file="/cbica/projects/luo_wm_dev/two_axes_manuscript/code/config/config_${dataset}.json"
    
    # where to save output and error logs
    logs_dir="/cbica/projects/luo_wm_dev/two_axes_manuscript/code/logs/tract_to_cortex/a_make_tdi/${dataset}"
    if [ ! -d "${logs_dir}" ]; then
        mkdir -p ${logs_dir}
    fi

    # set dir
    data_root=$(jq -r '.data_root' ${config_file})
    manuscript_data_root=$(jq -r '.manuscript_input_root' ${config_file})
    qsiprep_dir="${data_root}/raw/datalad_qsiprep"
    
    if [ "${dataset}" = "HBN" ]; then
        pyafq_dir="${data_root}/derivatives/babs_qsirecon_pyafq_allsubs_noACT/merge_ds"
    else
        pyafq_dir="${data_root}/derivatives/babs_qsirecon_pyafq_act/merge_ds"
    fi

    # subjects file
    subjects_file="${manuscript_data_root}/subject_list/final_sample/${dataset}_WMDev_FinalSample.txt"
    mapfile -t subjects_array < <(tail -n +2 ${subjects_file}) # skip header
    for i in "${!subjects_array[@]}"; do
        subjects_array[$i]=$(echo "${subjects_array[$i]}" | tr -d '"') # remove quotes
    done
    subject_count=${#subjects_array[@]} 

    jid_a01=$(sbatch --parsable \
        --array=0-$((subject_count-1))%2 \
        --job-name="a01_datalad_get_trks_${dataset}" \
        ./a01_datalad_get_trks.sh ${subjects_file} ${pyafq_dir} ${qsiprep_dir} ${logs_dir} "a01_datalad_get_trks_${dataset}")

    jid_a02=$(sbatch --parsable \
        --dependency=afterok:${jid_a01} \
        --array=0-$((subject_count-1)) \
        --job-name="a02_trk_to_tck_${dataset}" \
        ./a02_trk_to_tck.sh ${subjects_file} ${pyafq_dir} ${logs_dir} ${config_file} "a02_trk_to_tck_${dataset}")

    jid_a03=$(sbatch --parsable \
        --dependency=afterok:${jid_a02} \
        --array=0-$((subject_count-1)) \
        --job-name="a03_delete_trks_${dataset}" \
        ./a03_delete_trks.sh ${subjects_file} ${pyafq_dir} ${logs_dir} "a03_delete_trks_${dataset}")

    jid_a04=$(sbatch --parsable \
        --dependency=afterok:${jid_a03} \
        --array=0-$((subject_count-1)) \
        --job-name="a04_pyafq-bundles_to_tdi_${dataset}" \
        ./a04_pyafq-bundles_to_tdi.sh ${subjects_file} ${config_file} ${logs_dir} "a04_pyafq-bundles_to_tdi_${dataset}")

    sbatch --parsable \
        --dependency=afterok:${jid_a04} \
        --array=0-$((subject_count-1)) \
        --job-name="a05_cleanup_${dataset}" \
        ./a05_cleanup.sh ${subjects_file} ${config_file} ${logs_dir} "a05_cleanup_${dataset}"

done
