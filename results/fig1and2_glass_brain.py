from dipy.io.streamline import load_trk
from fury.colormap import create_colormap
from fury import actor, window, colormap as cmap
import matplotlib.pyplot as plt
import nibabel as nib
import numpy as np
from os.path import join as ospj 
import pandas as pd
from scipy.ndimage import gaussian_filter, zoom

# This script takes average across-subject tract profile for left CST and plots it on a tract from an example subject

####################
# Set directory/variables
####################
subject = "sub-1000393599"
study_path = f"/Users/audluo/PennLINC/luowm_local/tract_profiles/qsirecon/{subject}/"
tract_profiles_path = f"{study_path}/average_tract_profiles/"

# Load md image
md_img = nib.load(ospj(study_path, f"{subject}_ses-PNC1_odfmodel-DTI_desc-MD_dwi.nii.gz"))
md = md_img.get_fdata()

# Load Freesurfer brain image -- note that this image had to be transformed to Native ACPC space with xfm_fs_brain.sh
brain_img = nib.load(ospj(study_path, "fs_brain_transformed.nii"))
brain_data = brain_img.get_fdata()
original_affine = brain_img.affine
# Zoom factor
zoom_factor = 2.0

# Interpolation step with zoom factor
brain_interpolated = zoom(brain_data, zoom=zoom_factor, order=1)

# Adjust the affine to account for the zoom
scaled_affine = original_affine.copy()
scaled_affine[:3, :3] *= (1 / zoom_factor)  # Scale the voxel size component by 1 / zoom_factor


def plot_tract_profile(tract_name, subject, study_path, tract_profiles_path, output_folder, view):
    print(tract_name)
    tract_filename = tract_name.replace("_", "")
   
    # Load the CSV file for the tract
    csv_file = ospj(tract_profiles_path, f"{tract_name}_avg_dev.csv")
    data = pd.read_csv(csv_file)
    normalized_values = data['normalized_values'].values
    normalized_values = np.abs(normalized_values)

    # Load tractography
    tract_filename = tract_filename.replace("Fronto.occipital", "Frontooccipital") # need to update for IFO
    sft = load_trk(ospj(study_path, "trk", f"{subject}_ses-PNC1_coordsys-RASMM_trkmethod-probCSD_recogmethod-AFQ_desc-{tract_filename}_tractography.trk"), md_img)
    tract_brain = sft.streamlines

   # Threshold and smooth brain image
    sigma = 1.5
    brain_interpolated_smooth = gaussian_filter(brain_interpolated, sigma=sigma)
    threshold = 60
    brain_interpolated_smooth[brain_interpolated_smooth < threshold] = 0

    brain_glass_brain_actor = actor.contour_from_roi(
    brain_interpolated_smooth, scaled_affine, color=[0, 0, 0], opacity=0.01)
    brain_glass_brain_actor.GetProperty().EdgeVisibilityOn()
    brain_glass_brain_actor.GetProperty().SetEdgeColor(0.5, 0.5, 0.5)
    brain_glass_brain_actor.GetProperty().SetSpecular(1.0)
    brain_glass_brain_actor.GetProperty().SetSpecularPower(150.0)

    
    # Create a function to generate colors
    def create_colormap(values, name='Spectral'):
        cmap = plt.get_cmap(name)
        norm_values = np.clip(values, 0, 1)  # Normalize between 0 and 1
        colors = cmap(norm_values)
        colors[values == 0] = [1, 1, 1, 0] # Set 0 values to white
        #return colors[:, :3]
        return colors

    # Visualize the tract with interpolated colors
    scene = window.Scene()
    scene.add(brain_glass_brain_actor)

    for sl in tract_brain:
        num_points = len(sl)
        interpolated_values = np.interp(np.linspace(0, 1, num_points), np.linspace(0, 1, len(normalized_values)), normalized_values)
        interpolated_values[np.isnan(interpolated_values)] = 0

        # Generate colormap for the streamline
        colors = create_colormap(interpolated_values, name='Spectral_r')
        line_actor = actor.line([sl], colors=colors)
        line_actor.GetProperty().SetLineWidth(3)
        scene.add(line_actor)

    # Set background and camera view
    scene.background((1, 1, 1))
    if view == "coronal": 
        scene.set_camera(position=(0, 400, 0), focal_point=(0, 0, 0), view_up=(0, 0, 1))  # Coronal view
        output_path = ospj(output_folder, f"{subject}_{tract_name}_normalized_coronal.png")
        window.record(scene, out_path=output_path, size=(1200, 1200))

    elif view == "axial":
        scene.set_camera(position=(0, 0, 400), focal_point=(0, 0, 0), view_up=(0, 1, 0)) # axial
         
        output_path = ospj(output_folder, f"{subject}_{tract_name}_normalized_axial.png")
        window.record(scene, out_path=output_path, size=(1200, 1200))

    elif view == "sagittal":
        scene.set_camera(position=(-400, 0, 0), focal_point=(0, 0, 0), view_up=(0, 0, 1)) #sagittal left
         
        output_path = ospj(output_folder, f"{subject}_{tract_name}_normalized_sagittal.png")
        window.record(scene, out_path=output_path, size=(1200, 1200))



# Figure 2: Callosum bundles - plot for both coronal and axial view
tract_list = ["Callosum_Anterior_Frontal", "Callosum_Motor", "Callosum_Occipital", "Callosum_Orbital",
               "Callosum_Posterior_Parietal", "Callosum_Superior_Frontal", 
               "Callosum_Superior_Parietal",  "Callosum_Temporal"]  

for tract in tract_list:
    plot_tract_profile(tract, subject=subject, study_path=study_path, tract_profiles_path=tract_profiles_path, output_folder="/Users/audluo/PennLINC/luowm_local/output/tract_profiles_testing/all_datasets/glass_brain/ageeffects", view = "axial")

for tract in tract_list:
    plot_tract_profile(tract, subject=subject, study_path=study_path, tract_profiles_path=tract_profiles_path, output_folder="/Users/audluo/PennLINC/luowm_local/output/tract_profiles_testing/all_datasets/glass_brain/ageeffects", view = "coronal")


tract_list = ["Callosum_Motor"]  
for tract in tract_list:
    plot_tract_profile(tract, subject=subject, study_path=study_path, tract_profiles_path=tract_profiles_path, output_folder="/Users/audluo/PennLINC/luowm_local/output/tract_profiles_testing/all_datasets/glass_brain/ageeffects", view = "axial")



# Figure 3: Association bundles - plot for sagittal view
tract_list = ["Left_Arcuate", "Left_Inferior_Fronto.occipital",  "Left_Inferior_Longitudinal", "Left_Posterior_Arcuate",
              "Left_Superior_Longitudinal", "Left_Uncinate", "Left_Vertical_Occipital", "Left_Corticospinal"]  
 
for tract in tract_list:
    plot_tract_profile(tract, subject=subject, study_path=study_path, tract_profiles_path=tract_profiles_path, output_folder="/Users/audluo/PennLINC/luowm_local/output/tract_profiles_testing/all_datasets/glass_brain/ageeffects", view = "sagittal")





 