import csv
import json
import nibabel as nib
import numpy as np
import os
from os.path import join as ospj
import pandas as pd
import re
from scipy.stats import spearmanr
import shutil
import sys
 

# S-A axis files downloaded from https://github.com/PennLINC/S-A_ArchetypalAxis.git
# left and right hemispheres have whole brain S-A axis rankings
# need to combine left and right and save out
SAaxis_left = nib.load("/cbica/projects/luo_wm_dev/SAaxis/SensorimotorAssociation_Axis_LH.fsaverage5.func.gii")
SAaxis_right = nib.load("/cbica/projects/luo_wm_dev/SAaxis/SensorimotorAssociation_Axis_RH.fsaverage5.func.gii")
 
SAaxis_wholebrain = np.concatenate((SAaxis_left.darrays[0].data, SAaxis_right.darrays[0].data))
SAaxis_wholebrain = pd.DataFrame(SAaxis_wholebrain)

# save out to csv
SAaxis_wholebrain.to_csv('/cbica/projects/luo_wm_dev/SAaxis/SensorimotorAssociation_Axis.fsaverage5.csv', index=False)
  

 