#!/bin/bash

# PNC
cd /cbica/projects/luo_wm_dev/input/PNC/derivatives/babs_qsirecon_pyafq_act/analysis/code
datalad save -m "add custom recon"   
datalad push --to input
datalad push --to output  

# HCPD
cd /cbica/projects/luo_wm_dev/input/HCPD/derivatives/babs_qsirecon_pyafq_act/analysis/code
datalad save -m "add custom recon"   
datalad push --to input
datalad push --to output  

# HBN
cd /cbica/projects/luo_wm_dev/input/HBN/derivatives/babs_qsirecon_pyafq_allsubs_noACT/analysis/code
datalad save -m "add custom recon"   
datalad push --to input
datalad push --to output  

# HBN - with ACT, sensitivity analysis
cd /cbica/projects/luo_wm_dev/input/HBN/derivatives/babs_qsirecon_pyafq_act/analysis/code
datalad save -m "add custom recon"   
datalad push --to input
datalad push --to output  