

# BABS installation notes

## 1. make python env

cd /cbica/projects/luo_wm_dev/two_axes_manuscript/software/
conda create -n babs python=3.9.16
conda activate babs

### Install DataLad, Git, and git-annex:
conda install -c conda-forge datalad git git-annex

### Install datalad-container:
pip install datalad_container
 

## 2. check dependencies

 
### required dependencies:
datalad --version # datalad 0.19.6
git --version # git version 2.40.1
git-annex version # git-annex version: 10.20230408-g5b1e8ba77
datalad containers-add --version # datalad_container 1.2.5
 

## 3. install BABS

pip install babs

### show version
pip show babs # Version: 0.0.8

 