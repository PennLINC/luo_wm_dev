
#!/bin/bash

source /cbica/projects/luo_wm_dev/miniconda3/etc/profile.d/conda.sh
conda activate babs

 
########################
# HCPD - noddi
########################
cd /cbica/projects/luo_wm_dev/input/HCPD/derivatives/babs_noddi
babs-merge --project-root $PWD

 
########################
# HCPD - mapmri
########################
cd /cbica/projects/luo_wm_dev/input/HCPD/derivatives/babs_mapmri
babs-merge --project-root $PWD

 
########################
# HBN - noddi
########################
cd /cbica/projects/luo_wm_dev/input/HBN/derivatives/babs_noddi
babs-merge --project-root $PWD

 
########################
# HBN - mapmri
########################
cd /cbica/projects/luo_wm_dev/input/HBN/derivatives/babs_mapmri
babs-merge --project-root $PWD

 
 