#!/bin/bash

# submit this with ./c00_wrapper_vol_to_surf.sh

datasets=( "PNC" "HCPD" "HBN")  
depths=(1.5)

source /cbica/projects/luo_wm_dev/miniconda3/etc/profile.d/conda.sh
conda activate luo_wm_dev

for dataset in "${datasets[@]}"; do
    config_file="/cbica/projects/luo_wm_dev/two_axes_manuscript/code/config/config_${dataset}.json"
        
    # Set where to save out error/output logs
    logs_dir="/cbica/projects/luo_wm_dev/two_axes_manuscript/code/logs/tract_to_cortex/d_group_avg_parcellate/${dataset}"
    if [ ! -d "${logs_dir}" ]; then
        mkdir -p ${logs_dir}
    fi

    # set dir
    data_root=$(jq -r '.data_root' ${config_file})
    manuscript_data_root=$(jq -r '.manuscript_input_root' ${config_file})
    
    # Create directory for group vol_to_surf outputs
    if [ ! -d "${manuscript_data_root}/derivatives/vol_to_surf/group" ]; then
        mkdir -p ${manuscript_data_root}/derivatives/vol_to_surf/group
    fi

        
    for depth in "${depths[@]}"; do
        jid_d01=$(sbatch --parsable \
        --nodes=1 \
        --ntasks=1 \
        --cpus-per-task=1 \
        --time=1:00:00 \
        --output=${logs_dir}/d01_group_avg_map_fslr_${dataset}_${depth}_%j.out \
        --error=${logs_dir}/d01_group_avg_map_fslr_${dataset}_${depth}_%j.err \
        --job-name="d01_group_avg_map_fslr_${dataset}_${depth}" \
        --wrap="python ./d01_group_avg_map_fslr.py ${dataset} ${depth}")
        echo "Submitted d01 for ${dataset} at depth ${depth}, Job ID: ${jid_d01}"

        sbatch --parsable \
            --dependency=afterok:${jid_d01} \
            --nodes=1 \
            --ntasks=1 \
            --cpus-per-task=1 \
            --time=1:00:00 \
            --output=${logs_dir}/d02_parcellate_maps_${dataset}_${depth}_%j.out \
            --error=${logs_dir}/d02_parcellate_maps_${dataset}_${depth}_%j.err \
            --job-name="d02_parcellate_maps_${dataset}_${depth}" \
            --wrap="python ./d02_parcellate_maps.py ${dataset} ${depth}"
        echo "Submitted d02 for ${dataset} at depth ${depth}, dependent on Job ID: ${jid_d01})"

    done
done
