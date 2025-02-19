#!/bin/bash

# qc files wrapper
# submit this with `./get_qc_files_wrapper.sh`

datasets=( "PNC" "HCPD" "HBN")  

for dataset in "${datasets[@]}"; do

    # make dataset-specific logs folders
    logs_dir="/cbica/projects/luo_wm_dev/two_axes_manuscript/code/logs/datalad/${dataset}/"
    if [ ! -d ${logs_dir} ]; then
        mkdir -p ${logs_dir}
    fi
    sbatch --parsable \
                --nodes=1 \
                --ntasks=1 \
                --cpus-per-task=1 \
                --mem=1G \
                --time=24:00:00 \
                --propagate=NONE \
                --output=${logs_dir}/get_qcfiles_%j.out \
                --error=${logs_dir}/get_qcfiles_%j.err \
                --job-name="get_qcfiles_${dataset}" \
                ./get_qcfiles.sh "${dataset}"

            echo "Submitted get_qcfiles.sh ${dataset}" 

 done 