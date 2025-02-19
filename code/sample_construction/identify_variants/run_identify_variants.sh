#!/bin/bash

######################
# run python script 
######################
source /cbica/projects/luo_wm_dev/miniconda3/etc/profile.d/conda.sh
conda activate luo_wm_dev
 
python ./identify_variants.py "PNC" "/cbica/projects/luo_wm_dev/two_axes_manuscript/code/config/config_PNC.json"
python ./identify_variants.py "HCPD" "/cbica/projects/luo_wm_dev/two_axes_manuscript/code/config/config_HCPD.json"
python ./identify_variants.py "HBN" "/cbica/projects/luo_wm_dev/two_axes_manuscript/code/config/config_HBN.json"