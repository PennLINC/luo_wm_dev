
#!/bin/bash

source /cbica/projects/luo_wm_dev/miniconda3/etc/profile.d/conda.sh
conda activate babs

########################
# PNC - act-hsvs
########################
cd /cbica/projects/luo_wm_dev/input/PNC/derivatives/babs_qsirecon_pyafq_act
babs-merge --project-root $PWD

cd /cbica/projects/luo_wm_dev/input/PNC/derivatives/
datalad clone \
    ria+file://${PWD}/babs_qsirecon_pyafq_act/output_ria#~data \
    qsirecon_pyafq

########################
# HCPD - act-hsvs
########################
cd /cbica/projects/luo_wm_dev/input/HCPD/derivatives/babs_qsirecon_pyafq_act
babs-merge --project-root $PWD

cd /cbica/projects/luo_wm_dev/input/HCPD/derivatives/
datalad clone \
    ria+file://${PWD}/babs_qsirecon_pyafq_act/output_ria#~data \
    qsirecon_pyafq

########################
# HBN - no act-hsvs
########################
cd /cbica/projects/luo_wm_dev/input/HBN/derivatives/babs_qsirecon_pyafq_allsubs_noACT
babs-merge --project-root $PWD

cd /cbica/projects/luo_wm_dev/input/HBN/derivatives/
datalad clone \
    ria+file://${PWD}/babs_qsirecon_pyafq_allsubs_noACT/output_ria#~data \
    qsirecon_pyafq


########################
# HBN - act-hsvs, sensitivity analysis
########################
cd /cbica/projects/luo_wm_dev/input/HBN/derivatives/babs_qsirecon_pyafq_act
babs-merge --project-root $PWD

cd /cbica/projects/luo_wm_dev/input/HBN/derivatives/
datalad clone \
    ria+file://${PWD}/babs_qsirecon_pyafq_act/output_ria#~data \
    qsirecon_pyafq

