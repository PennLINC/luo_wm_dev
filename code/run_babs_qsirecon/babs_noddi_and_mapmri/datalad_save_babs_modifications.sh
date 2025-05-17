#!/bin/bash


# HCPD
cd /cbica/projects/luo_wm_dev/input/HCPD/derivatives/babs_noddi/analysis/code
datalad save -m "add custom recon"   
datalad push --to input
datalad push --to output  

cd /cbica/projects/luo_wm_dev/input/HCPD/derivatives/babs_mapmri/analysis/code
datalad save -m "add custom recon"   
datalad push --to input
datalad push --to output  

# HBN
cd /cbica/projects/luo_wm_dev/input/HBN/derivatives/babs_noddi/analysis/code
datalad save -m "add custom recon"   
datalad push --to input
datalad push --to output  

cd /cbica/projects/luo_wm_dev/input/HBN/derivatives/babs_mapmri/analysis/code
datalad save -m "add custom recon"   
datalad push --to input
datalad push --to output  

 