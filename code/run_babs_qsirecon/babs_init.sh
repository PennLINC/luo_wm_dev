
#!/bin/bash

source /cbica/projects/luo_wm_dev/miniconda3/etc/profile.d/conda.sh
conda activate babs

########################
# PNC - act-hsvs
########################
if [ ! -d /cbica/projects/luo_wm_dev/input/PNC/derivatives ]; then
        mkdir -p /cbica/projects/luo_wm_dev/input/PNC/derivatives
fi

babs-init --where_project /cbica/projects/luo_wm_dev/input/PNC/derivatives \
    --project_name babs_qsirecon_pyafq_act \
    --input qsiprep /cbica/projects/luo_wm_dev/input/PNC/raw/datalad_qsiprep \
    --input freesurfer /cbica/projects/luo_wm_dev/input/PNC/raw/datalad_freesurfer/inputs/data \
    --list_sub_file /cbica/projects/luo_wm_dev/two_axes_manuscript/input/PNC/subject_list/PNC_subject_list_babs.txt \
    --container_ds /cbica/projects/luo_wm_dev/two_axes_manuscript/software/qsiprep/qsiprep-container-0-22-0 \
    --container_name qsiprep-0-22-0 \
    --container_config_yaml_file /cbica/projects/luo_wm_dev/two_axes_manuscript/code/run_babs_qsirecon/babs_yaml_files/babs_qsiprep-0-22-0_qsirecon_mrtrix_ACT-hsvs_pyafq_singleshell_dti.yaml \
    --type_session single-ses --type_system slurm  

########################
# HCPD - act-hsvs
########################
if [ ! -d /cbica/projects/luo_wm_dev/input/HCPD/derivatives ]; then
        mkdir -p /cbica/projects/luo_wm_dev/input/HCPD/derivatives
fi

babs-init --where_project /cbica/projects/luo_wm_dev/input/HCPD/derivatives \
    --project_name babs_qsirecon_pyafq_act \
    --input qsiprep /cbica/projects/luo_wm_dev/input/HCPD/raw/datalad_qsiprep \
    --input freesurfer /cbica/projects/luo_wm_dev/input/HCPD/raw/datalad_freesurfer/inputs/data \
    --list_sub_file /cbica/projects/luo_wm_dev/two_axes_manuscript/input/HCPD/subject_list/HCPD_subject_list_babs.txt \
    --container_ds /cbica/projects/luo_wm_dev/two_axes_manuscript/software/qsiprep/qsiprep-container-0-22-0 \
    --container_name qsiprep-0-22-0 \
    --container_config_yaml_file /cbica/projects/luo_wm_dev/two_axes_manuscript/code/run_babs_qsirecon/babs_yaml_files/babs_qsiprep-0-22-0_qsirecon_mrtrix_ACT-hsvs_pyafq_dti.yaml \
    --type_session single-ses --type_system slurm 

########################
# HBN - no act-hsvs
########################
if [ ! -d /cbica/projects/luo_wm_dev/input/HBN/derivatives ]; then
        mkdir -p /cbica/projects/luo_wm_dev/input/HBN/derivatives
fi

babs-init --where_project /cbica/projects/luo_wm_dev/input/HBN/derivatives \
    --project_name babs_qsirecon_pyafq_allsubs_noACT \
    --input qsiprep /cbica/projects/luo_wm_dev/input/HBN/raw/datalad_qsiprep \
    --list_sub_file /cbica/projects/luo_wm_dev/two_axes_manuscript/input/HBN/subject_list/HBN_subject_list_babs.txt \
    --container_ds /cbica/projects/luo_wm_dev/two_axes_manuscript/software/qsiprep/qsiprep-container-0-22-0 \
    --container_name qsiprep-0-22-0 \
    --container_config_yaml_file /cbica/projects/luo_wm_dev/two_axes_manuscript/code/run_babs_qsirecon/babs_yaml_files/babs_qsiprep-0-22-0_qsirecon_mrtrix_msmt_pyafq_dti.yaml \
    --type_session single-ses --type_system slurm 
 


########################
# HBN - act-hsvs (sensitivity analysis)
########################
babs-init --where_project /cbica/projects/luo_wm_dev/input/HBN/derivatives \
    --project_name babs_qsirecon_pyafq_act \
    --input qsiprep /cbica/projects/luo_wm_dev/input/HBN/raw/datalad_qsiprep \
    --input freesurfer /cbica/projects/luo_wm_dev/input/HBN/raw/datalad_freesurfer/inputs/data \
    --list_sub_file /cbica/projects/luo_wm_dev/two_axes_manuscript/input/HBN/subject_list/HBN_subject_list_babs.txt \
    --container_ds /cbica/projects/luo_wm_dev/two_axes_manuscript/software/qsiprep/qsiprep-container-0-22-0 \
    --container_name qsiprep-0-22-0 \
    --container_config_yaml_file /cbica/projects/luo_wm_dev/two_axes_manuscript/code/run_babs_qsirecon/babs_yaml_files/babs_qsiprep-0-22-0_qsirecon_mrtrix_ACT-hsvs_pyafq_dti.yaml \
    --type_session single-ses --type_system slurm 
 