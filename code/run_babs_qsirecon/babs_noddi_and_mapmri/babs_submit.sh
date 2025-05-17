
#!/bin/bash

source /cbica/projects/luo_wm_dev/miniconda3/etc/profile.d/conda.sh
conda activate babs

########################
# HCPD - noddi
########################
cd /cbica/projects/luo_wm_dev/input/HCPD/derivatives/babs_noddi
# after test job finishes successfully: 
babs-submit --project-root $PWD --all 

########################
# HCPD - mapmri
########################
cd /cbica/projects/luo_wm_dev/input/HCPD/derivatives/babs_mapmri
# after test job finishes successfully: 
babs-submit --project-root $PWD --all 
 


########################
# HBN - noddi
########################
cd /cbica/projects/luo_wm_dev/input/HBN/derivatives/babs_noddi
# after test job finishes successfully: 
babs-submit --project-root $PWD --all 

########################
# HBN - mapmri
########################
cd /cbica/projects/luo_wm_dev/input/HBN/derivatives/babs_mapmri
# after test job finishes successfully: 
babs-submit --project-root $PWD --all 