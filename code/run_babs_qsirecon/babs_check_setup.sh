
#!/bin/bash

source /cbica/projects/luo_wm_dev/miniconda3/etc/profile.d/conda.sh
conda activate babs

########################
# PNC - act-hsvs
########################
cd /cbica/projects/luo_wm_dev/input/PNC/derivatives/babs_qsirecon_pyafq_act
babs-status --project-root $PWD # check status - must be in root of babs project
head analysis/code/participant_job.sh
babs-check-setup --project-root ${PWD} --job-test # success!


########################
# HCPD - act-hsvs
########################
cd /cbica/projects/luo_wm_dev/input/HCPD/derivatives/babs_qsirecon_pyafq_act
babs-status --project-root $PWD # check status - must be in root of babs project
head analysis/code/participant_job.sh
babs-check-setup --project-root ${PWD} --job-test # success!

########################
# HBN - no act-hsvs
########################
cd /cbica/projects/luo_wm_dev/input/HBN/derivatives/babs_qsirecon_pyafq_allsubs_noACT
babs-status --project-root $PWD # check status - must be in root of babs project
head analysis/code/participant_job.sh
babs-check-setup --project-root ${PWD} --job-test # success!


########################
# HBN - act-hsvs, sensitivity analysis
########################
cd /cbica/projects/luo_wm_dev/input/HBN/derivatives/babs_qsirecon_pyafq_act
babs-status --project-root $PWD # check status - must be in root of babs project
head analysis/code/participant_job.sh
babs-check-setup --project-root ${PWD} --job-test # success!
