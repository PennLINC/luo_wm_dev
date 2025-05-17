
#!/bin/bash

source /cbica/projects/luo_wm_dev/miniconda3/etc/profile.d/conda.sh
conda activate babs
 
# ---------
# HCPD - noddi
# ---------
cd /cbica/projects/luo_wm_dev/input/HCPD/derivatives/

babs-init --where_project /cbica/projects/luo_wm_dev/input/HCPD/derivatives/ \
    --project_name babs_noddi \
    --input qsiprep /cbica/projects/luo_wm_dev/input/HCPD/raw/datalad_qsiprep \
    --list_sub_file /cbica/projects/luo_wm_dev/two_axes/input/HCPD/subject_list/HCPD_subject_list_babs.txt \
    --container_ds /cbica/projects/luo_wm_dev/two_axes/software/qsiprep/qsiprep-container-0-22-0 \
    --container_name qsiprep-0-22-0 \
    --container_config_yaml_file /cbica/projects/luo_wm_dev/two_axes/code/run_babs_qsirecon/babs_yaml_files/babs_qsiprep-0-22-0_noddi.yaml \
    --type_session single-ses --type_system slurm  

# ---------
# HCPD - mapmri
# ---------
babs-init --where_project /cbica/projects/luo_wm_dev/input/HCPD/derivatives/ \
    --project_name babs_mapmri \
    --input qsiprep /cbica/projects/luo_wm_dev/input/HCPD/raw/datalad_qsiprep \
    --list_sub_file /cbica/projects/luo_wm_dev/two_axes/input/HCPD/subject_list/HCPD_subject_list_babs.txt \
    --container_ds /cbica/projects/luo_wm_dev/two_axes/software/qsiprep/qsiprep-container-0-22-0 \
    --container_name qsiprep-0-22-0 \
    --container_config_yaml_file /cbica/projects/luo_wm_dev/two_axes/code/run_babs_qsirecon/babs_yaml_files/babs_qsiprep-0-22-0_mapmri.yaml \
    --type_session single-ses --type_system slurm  

# ---------
# HBN - noddi
# ---------
 babs-init --where_project /cbica/projects/luo_wm_dev/input/HBN/derivatives/ \
    --project_name babs_noddi \
    --input qsiprep /cbica/projects/luo_wm_dev/input/HBN/raw/datalad_qsiprep \
    --list_sub_file /cbica/projects/luo_wm_dev/two_axes/input/HBN/subject_list/HBN_subject_list_babs.txt \
    --container_ds /cbica/projects/luo_wm_dev/two_axes/software/qsiprep/qsiprep-container-0-22-0 \
    --container_name qsiprep-0-22-0 \
    --container_config_yaml_file /cbica/projects/luo_wm_dev/two_axes/code/run_babs_qsirecon/babs_yaml_files/babs_qsiprep-0-22-0_noddi.yaml \
    --type_session single-ses --type_system slurm  


# ---------
# HBN - mapmri
# ---------
babs-init --where_project /cbica/projects/luo_wm_dev/input/HBN/derivatives/ \
    --project_name babs_mapmri \
    --input qsiprep /cbica/projects/luo_wm_dev/input/HBN/raw/datalad_qsiprep \
    --list_sub_file /cbica/projects/luo_wm_dev/two_axes/input/HBN/subject_list/HBN_subject_list_babs.txt \
    --container_ds /cbica/projects/luo_wm_dev/two_axes/software/qsiprep/qsiprep-container-0-22-0 \
    --container_name qsiprep-0-22-0 \
    --container_config_yaml_file /cbica/projects/luo_wm_dev/two_axes/code/run_babs_qsirecon/babs_yaml_files/babs_qsiprep-0-22-0_mapmri.yaml \
    --type_session single-ses --type_system slurm  