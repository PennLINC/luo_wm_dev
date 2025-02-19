
#!/bin/bash

source /cbica/projects/luo_wm_dev/miniconda3/etc/profile.d/conda.sh
conda activate babs

########################
# PNC - act-hsvs
########################
cd /cbica/projects/luo_wm_dev/input/PNC/derivatives/babs_qsirecon_pyafq_act
# after test job finishes successfully: 
babs-submit --project-root $PWD --all 

########################
# HCPD - act-hsvs
########################
cd /cbica/projects/luo_wm_dev/input/HCPD/derivatives/babs_qsirecon_pyafq_act
# after test job finishes successfully: 
babs-submit --project-root $PWD --all 

########################
# HBN - no act-hsvs
########################
cd /cbica/projects/luo_wm_dev/input/HBN/derivatives/babs_qsirecon_pyafq_allsubs_noACT
# after test job finishes successfully: 
babs-submit --project-root $PWD --all 


########################
# HBN - act-hsvs, sensitivity analysis
########################
cd /cbica/projects/luo_wm_dev/input/HBN/derivatives/babs_qsirecon_pyafq_act
# after test job finishes successfully: 
babs-submit --project-root $PWD --all 