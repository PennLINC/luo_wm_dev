
# I. Project Information

**Abstract**

Despite decades of neuroimaging research, it remains unknown how white matter develops along the length of major tracts in the human brain. Here, we identify fundamental patterns of white matter maturation by examining developmental variation along cortico-cortical tracts in youth ages 5-23 using diffusion MRI from three large-scale, cross-sectional datasets (total N = 2,710). Across all three datasets, we delineate two replicable axes of white matter development. First, we delineate a deep-to-superficial axis of maturation along tracts. Second, we demonstrate that development of superficial tract regions near a tract’s cortical endpoints aligns with the cortical hierarchy defined by the sensorimotor-association axis. These results challenge the longstanding assumption of synchronous maturation along white matter tracts. Rather than being uniform conduits between cortical regions, white matter tracts exhibit developmental variability along their length that may be adaptive for refining neural transmission in youth.


**Team**

| **Role** | **Name** |
| --- | --- |
| **Project Lead** | Audrey C. Luo |
| **Faculty Lead** | Theodore D. Satterthwaite |
| **Analytic Replicator** | Steven L. Meisler |
| **Collaborators** | Valerie J. Sydnor, Aaron Alexander-Bloch, Joëlle Bagautdinova, Deanna M. Barch, Dani S. Bassett, Christos Davatzikos, Alexandre R. Franco, Jeff Goldsmith, Raquel E. Gur, Ruben C. Gur, Fengling Hu, Marc Jaskir, Gregory Kiar, Arielle S. Keller, Bart Larsen, Allyson P. Mackey, Michael P. Milham, David Roalf, Golia Shafiei, Russell T. Shinohara, Leah H. Somerville, Sarah M. Weinstein, Jason Yeatman, Matthew Cieslak, Ariel Rokem |

**Project Timeline**

| **Project Start Date** | February 2024 |
| --- | --- |
| **Current Project Status** | In prep |

**Code and Communication**

| **Github Repository** | [https://github.com/PennLINC/luo_wm_dev/tree/main](https://github.com/PennLINC/luo_wm_dev/tree/main) |
| --- | --- |
| **Slack Channel** | #luo_wm_dev |

**Datasets**

- RBC PNC (health exclude) as discovery dataset, HCP-D (replication), and HBN (replication)

**Conference Presentations**

- Poster presented at The Organization for Human Brain Mapping Annual Meeting, June 2024
- Poster presented at Flux Congress, September 2024
- Poster to be presented at The Society of Biological Psychiatry Annual Meeting, April 2025 as a Predoctoral Travel Awardee
- Talk entitled *Axes of Hierarchical Brain Development* to be given at the Gradients of Brain Organization Workshop, Brisbane, Australia, June 2025

---

# II. CUBIC Project Directory Structure

The project directory on CUBIC is `/cbica/projects/luo_wm_dev` . 

- Code for the final manuscript is in `/cbica/projects/luo_wm_dev/two_axes_manuscript`

| **Directory** | **Description** |
| --- | --- |
| `~/two_axes_manuscript/code` | where manuscript code lives |
| `~/two_axes_manuscript/input` | for each dataset: has sample selection files, babs projects, and tract profiles for test subjects  |
| `~/two_axes_manuscript/output` | for each dataset: tract profiles, GAM outputs, NEST, spin test results |
| `~/two_axes_manuscript/software` | singularity images for various softwares and code for installing them and setting up the project folder |
|  |  |
| `~/input/` | This directory is OUTSIDE of the manuscript directory. 

For each dataset: includes “raw” data (datalad clones for qsiprep and freesurfer), ‘derivatives’ (pyAFQ output from BABS), sample selection files (demographics csv’s), tract profiles for ALL subjects.   |

# III. Code Documentation

***Overview of Analytic Workflow***

| **Step** | **Task** | **Notes** |
| --- | --- | --- |
| 0 | Install required software and packages |  |
| 1 | Construct initial sample | This multi-step task creates the subject list for BABS |
| 2 | Download QSIPrep and Freesurfer data |  |
| 3 | Use BABS to run `qsirecon` and `pyAFQ`   |  |
| 4 | Get tract profiles from `babs` | `datalad` get the merged babs output and copy over the tract profiles csv to `input/<dataset>/derivatives/tract_profiles_dti` |
| 5 | Prepare data for final sample selection | `prep_data`: finds subjects that are missing data and saves out a list of them for exclusion. This step needs to be done before final sample selection |
| 6 | Construct final sample | after `prep_data` , I did my final sample construction and excluded subjects that failed babs, etc. |
| 7 | Harmonize multi-site data using CovBat | Only HCP-D and HBN need to be harmonized within each dataset |
| 8 | Fit GAMs | Compute magnitude of the age effect and age of maturation, as well as GAM fits for each age |
| 9 | Tract-to-cortex mapping | Lots of steps here! Goal is to get tract-to-cortex probability maps for each dataset and parcellate to HCP-MMP atlas |
| 10 | Significance testing with NEST: deep-to-superficial | Are age effects enriched in superficial compared to deep tract regions? **(For Figures 1 and 2)**  |
| 11 | Significance testing with NEST: tract-to-cortex | Age age effects different on each end of callosum motor and IFOF? **(For Figures 3 and 4)** |
| 12 | Significance testing with spin tests: delta-delta analysis and parcel-level age of maturation vs. S-A rank | Are the ages of maturation different between tracts with large vs. small differences in S-A rank? **(For Figure 5)** Collapsing across tracts, does the age of maturation of superficial tract regions correlate with S-A axis rank? **(For Figure 6)** |
| 13 | Visualize results! |  |

### 0. Install required software and packages

- `~/two_axes_manuscript/software/installation` contains code for:
    - setting up the project directory
    - building the required singularity images for fMRIPrep and QSIPrep
    - pulling the Docker container for R packages used on CUBIC
    - information on how to install BABS
    - creating the Python environment used in this project (`luo_wm_dev`)
    - and cloning the version of pyAFQ used in this project

### 1. Construct initial sample

1. Identify non-variant diffusion MRI data in all 3 datasets
    1. Get QC files from each dataset’s QSIPrep clone
        - `~/two_axes_manuscript/code/datalad/qc/get_qcfiles.sh` gets the qc csv’s and copies them to `~/input/${dataset}/raw/qc_files`
        - `./submit_get_qcfiles.sh` submits the above script as jobs for PNC, HCP-D, and HBN
    2. Make list of subjects with variant and non-variant dMRI data
        
        ```bash
        cd ~/two_axes_manuscript/code/sample_construction/identify_variants/ 
        ./run_identify_variants.sh # this scripts runs identify_variants.py 
        # for all 3 datasets and saves out variant and non-variant lists 
        # in the two_axes_manuscript input folder for each dataset. 
        ```
        

1. Construct initial sample 
    1.  `${dataset}_InitialSampleSelection.Rmd` saves out temporary subject lists, which will be used for BABS
        
        ```bash
        cd ~/two_axes_manuscript/code/sample_construction/construct_initial_sample/
        ```
        

### 2. Download QSIPrep and Freesurfer data

1. Get QSIPrep data for each dataset 
    
    ```bash
    cd ~/two_axes_manuscript/code/datalad/qsiprep/
    sbatch ./datalad_get_qsiprep_PNC.sh # will get 3 subjects per datasets
    sbatch ./datalad_get_qsiprep_HCPD.sh
    sbatch ./datalad_get_qsiprep_HBN.sh
    ```
    
2. Get Freesurfer data, required for anatomically constrained tractography (ACT/HSVS) in QSIRecon
    1. Note that in the main analyses, only PNC and HCP-D use ACT. HBN loses too many subjects with ACT (noisy dataset), but we ran HBN with ACT as a sensitivity analysis.
    
    ```bash
    cd ~/two_axes_manuscript/code/datalad/freesurfer/
    sbatch ./datalad_get_freesurfer_PNC.sh  
    sbatch ./datalad_get_freesurfer_HCPD.sh
    ```
    

### 3. Use BABS to run `qsirecon` and `pyAFQ`

1. Initialize babs project
    
    ```bash
    cd ~/two_axes_manuscript/code/run_babs_qsirecon/
    ./babs_init.sh # initializes babs for PNC, HCP-D, and HBN for 3 test subjects 
    # in each dataset 
    ```
    
    - [FYI]: These are the custom BABS YAML files and custom QSIPrep json files used for each dataset (all files located in `~/two_axes_manuscript/code/run_babs_qsirecon/babs_yaml_files/` and `./qsirecon_json_files` )
    
    | **Dataset** | **QSIRecon** | **YAML file** | **JSON file** | **Notes** |
    | --- | --- | --- | --- | --- |
    | PNC | MRTrix: ss3t,
    ACT + HSVS
    and pyAFQ | babs_qsiprep-0-22-0_qsirecon_mrtrix_ACT-hsvs_pyafq_singleshell_dti.yaml | mrtrix_singleshell_ss3t_ACT-hsvs_pyafq_dti.json |  |
    | HCP-D | MRTrix: msmt,
    ACT + HSVS
    and pyAFQ | babs_qsiprep-0-22-0_qsirecon_mrtrix_ACT-hsvs_pyafq_dti.yaml | mrtrix_multishell_msmt_ACT-hsvs_pyafq_dti.json | JSON file contains modification: `"max_bval": 1500`. HCP-D is multi-shell. We use ALL shells for CSD in MRTrix. But we exclude the highest b-value for DTI in pyAFQ.  |
    | HBN | MRTrix: msmt,
    and pyAFQ. (No ACT+HSVS) | babs_qsiprep-0-22-0_qsirecon_mrtrix_msmt_pyafq_dti.yaml | mrtrix_multishell_msmt_pyafq_tractometry_dti.json | Same `"max_bval": 1500` modification as HCP-D |
2. Copy custom QSIRecon JSON file for each dataset to the corresponding BABS project (described in table above):
    
    ```bash
    cd ~/two_axes_manuscript/code/run_babs_qsirecon/
    ./copy_json_files.sh # run code to copy json files to respective BABS project
    ```
    
3. Datalad save all these modifications to each BABS project
    
    ```bash
    cd ~/two_axes_manuscript/code/run_babs_qsirecon/
    ./datalad_save_babs_modifications.sh # runs datalad save and push for each dataset's BABS project
    ```
    
4. Do a test job for each dataset’s BABS project (per BABS instructions)
    
    ```bash
    cd ~/two_axes_manuscript/code/run_babs_qsirecon/
    ./babs_check_setup.sh # runs babs-check-setup
    ```
    
5. `babs-submit` subjects for each dataset after the test jobs above finish successfully for all datasets. 
    
    ```bash
    cd ~/two_axes_manuscript/code/run_babs_qsirecon/
    ./babs_submit.sh # runs babs-submit  
    ```
    
    - you can check the status of the jobs by going to the BABS project folder (`babs_qsirecon_pyafq_act`) and running the following (remember to `conda activate babs`)
        
        ```bash
        babs-status --project-root $PWD
        ```
        
    - to resubmit failed subjects, go to BABS project folder and do:
        
        ```bash
        babs-status \
            --project-root $PWD \
            --resubmit failed 
        ```
        

### 4. Get tract profiles from `babs` project

1. Consume results!
    
    ```bash
    cd ~/two_axes_manuscript/code/run_babs_qsirecon/
    ./babs_merge_consume_results.sh # run code to babs-merge and datalad clone the results for 
    # each dataset
    ```
    
2. `Datalad get` the tract profiles for each test subject and copy them to a `tract_profiles` directory in `~/two_axes_manuscript/input/${dataset}/derivatives`
    
    ```bash
    cd ~/two_axes_manuscript/code/datalad/pyafq
    sbatch datalad_babs_pyafq.sh
    ```
    

### 5. Prepare data for final sample selection

1. From the QSIRecon/pyAFQ output, find subjects that are missing data and save out a list of them for exclusion. This step needs to be done before final sample selection.
    
    ```bash
    cd ~/two_axes_manuscript/code/prep_data/
    python prep_tract_profiles_data.py
    ```
    

### 6. Construct final sample

1. `~/two_axes_manuscript/code/sample_construction/construct_final_sample/${dataset}_FinalSampleSelection.Rmd` creates the final sample and tract profiles for each dataset

### 7. Harmonize multi-site data using CovBat

```bash
cd ~/two_axes_manuscript/code/covbat_harmonization/
./submit_covbat.sh # submit covbat for HCPD and HBN
```

### 8. Fit GAMs

```bash
cd ~/two_axes_manuscript/code/fit_GAMs/
./submit_fit_GAMs_development.sh  # fit GAMs for all 3 datasets
```

### 9. Tract-to-cortex mapping

- Code for tract-to-cortex mapping is in `~/two_axes_manuscript/code/tract_to_cortex`.
- The scripts in the subfolders of **`~/two_axes_manuscript/code/tract_to_cortex/`** need to be run sequentially.
1. **The first set of scripts makes the tract density images: `~/two_axes_manuscript/code/tract_to_cortex/a_make_tdi`**
    
    The wrapper will run all scripts a01 to a05 as jobs with dependencies. Each script will automatically run as soon as the one before it finishes. It basically runs job arrays that are dependent on each other (each element of array = 1 subject)
    
    ```bash
    cd ~/two_axes_manuscript/code/tract_to_cortex/a_make_tdi
    ./a00_wrapper_make_tdi.sh
    ```
    
    - `a01_datalad_get_trks.sh` : datalad gets the required files: QSIPrep T1w (from each qsiprep clone) and each .trk file for each person in each dataset (from each dataset’s babs project)
    - `a02_trk_to_tck.py` and `a02_trk_tck.sh`:  converts the trk files to tck and saves the newly converted tck files to a temporary directory
    - `a03_delete_trks.sh`: cleans up trk’s to save space
    - `a04_pyafq-bundles_to_tdi.sh`: converts tck files to tract density images for each subject and binarizes the TDI image
    - `a05_cleanup.sh`: deletes temporary directories and unneeded files to save space
2. **Next, we need to make sure each subject’s Freesurfer surfaces are aligned to their QSIPrep T1w’s and tracts. Scripts found here:**
    
    **`~/two_axes_manuscript/code/tract_to_cortex/b_transforms`**
    
    As above, the wrapper in this directory runs all the scripts from b01 to b02.
    
    ```bash
    cd ~/two_axes_manuscript/code/tract_to_cortex/b_transforms
    ./b00_wrapper_transforms.sh
    ```
    
    - Huge thanks to Marc Jaskir for his code, from which my code is heavily adapted: [https://github.com/marcjaskir/tracts/tree/main](https://github.com/marcjaskir/tracts/tree/main)
    - `b01_determine_freesurfer-to-native_acpc_volume_xfm.sh` : datalad gets Freesurfer files for each subject. Harmonizes file types and orientations of Freesurfer and QSIPrep images with voxelized tracts.
    - `b02_align_surfaces_with_tract_volumes.py` and `b02_align_surfaces_with_tract_volumes.sh` : Pial and white surfaces get transformed to native ACPC (QSIPrep space)
3. **Now we will use nilearn’s vol_to_surf to sample the binarized TDI map for each tract to the cortical surface. The non-zero regions on the surface correspond to the cortical endpoints of that tract.** 
    
    Scripts can be found at `~/two_axes_manuscript/code/tract_to_cortex/c_vol_to_surf`
    
    The wrapper runs c01 and c02.
    
    ```bash
    cd ~/two_axes_manuscript/code/tract_to_cortex/c_vol_to_surf
    ./c00_wrapper_vol_to_surf.sh
    ```
    
    - `c01_vol_to_surf_individual.py` and `c01_vol_to_surf_individual.sh` does the vol_to_surf mapping for each subject and outputs binary maps of cortical endpoints for each tract for each subject. For main analyses, a depth of 1.5mm is used (determined to be optimal depth in discovery dataset for sampling white matter and avoiding gray matter).
    - `c02_native_to_fsLR.sh` : warps the vol_to_surf outputs to fsLR
4. **Lastly, subject-level binarized tract-to-cortex maps are averaged to get a population probability map for each tract. Then we parcellate each map (1 map per tract per dataset) with Glasser.**
    
    **Scripts can be found at `~/two_axes_manuscript/code/tract_to_cortex/d_group_avg_parcellate`**
    
    ```bash
    cd ~/two_axes_manuscript/code/tract_to_cortex/d_group_avg_parcellate
    ./d00_wrapper_group_avg_parcellate.sh
    ```
    
    - `d01_group_avg_map_fslr.py` : This script takes the binary, subject-level tract-to-cortex maps (in fsLR 32k) for each tract, and averages them across subjects. This produces a group-level tract-to-cortex map of the proportion of subjects that have a tract connecting to each cortical region.
    Then saves the maps out as giftis (no threshold applied).
    - `d02_parcellate_maps.py`: This script takes the population probability tract-to-cortex maps and parcellates them to Glasser. Saves out csv's of the maps (no threshold applied).

### 10. Significance testing with NEST: deep-to-superficial

- This step checks — are age effects enriched in superficial compared to deep tract regions? **(For Figures 1 and 2).** Here, we use network enrichment significance testing  (NEST) ([https://github.com/smweinst/NEST/tree/main](https://github.com/smweinst/NEST/tree/main))
1. First, we need to format the tract profiles data for NEST. `~/two_axes_manuscript/code/significance_testing/NEST/prep_data_for_NEST.Rmd` prepares the data for all 3 datasets. Run through this first. This saves out `tract_profiles_for_NEST.RData` for each dataset in `~/two_axes_manuscript/output/${dataset}/tract_profiles/`
2. Run NEST for each tract in each dataset:

```bash
	cd ~/two_axes_manuscript/code/significance_testing/NEST/deep_to_superficial/
	./submit_NEST_wrapper_clipEnds_clip5.sh
```

- This wrapper submits a singularity call that uses my docker image for R-packages and the R script for NEST in the same directory (`NEST_wrapper_clipEnds_clip5.R`). This submits a job array where each element = 1 tract
- This may take several hours since NEST does 10,000 permutations

### 11. Significance testing with NEST: tract-to-cortex

- This step checks — Age age effects different on each end of callosum motor and IFOF? **(For Figures 3 and 4)**
- code is set up similarly to step 10
1. Run NEST for callosum motor and inferior fronto-occipital fasc

```bash
cd ~/two_axes_manuscript/code/significance_testing/NEST/tract_to_cortex/
./submit_NEST_wrapper_end_compare_clip5.sh
```

### 12. Significance testing with spin tests: delta-delta analysis and parcel-level age of maturation vs. S-A rank

- This step checks two things:
    - 1) Are the ages of maturation different between tracts with large vs. small differences in S-A rank? **(For Figure 5)**
    - 2) Collapsing across tracts, does the age of maturation of superficial tract regions correlate with S-A axis rank? **(For Figure 6)**
    - The scripts compute a spun t-test (t-value, dof, p-value) and Pearson’s correlations (p-value, correlation) for each dataset and averaged across datasets

```bash
cd /cbica/projects/luo_wm_dev/two_axes_manuscript/code/significance_testing/spin_tests
./submit_main_figures_spintests.sh
```

### 13. Visualize results!

- Main figures: `~/two_axes_manuscript/code/results/main_figures.Rmd`
    - this Rmd uses functions from `~/two_axes_manuscript/code/results/main_figures_functions.R`
- Supp figures: `~/two_axes_manuscript/code/results/supp_figures.Rmd`
    - this Rmd uses functions from `~/two_axes_manuscript/code/results/supp_figures_functions.R`
- Plotting glass brains:
    - `~/two_axes_manuscript/code/results/fig1and2_glass_brain.py`
    - `~/two_axes_manuscript/code/results/fig5_glass_brain.py`
    - `~/two_axes_manuscript/code/results/glass_brain_xfm.sh` — necessary transforms for a freesurfer brain to match qsiprep headers for glass brain plotting