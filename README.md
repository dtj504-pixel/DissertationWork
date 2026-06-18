This repository builds off the Using history matching to speed up management strategy evaluation grid searches paper (https://cdnsciencepub.com/doi/10.1139/cjfas-2024-0191) to create a framework that speeds up the evaluation of Harvest Control Rules. Then, this framework is applied to mixed fisheries using the MixME package.

The final report can be found at https://github.com/dtj504-pixel/DissertationWork/blob/main/An%20Efficient%20Framework%20for%20Management%20Strategy%20Evaluation%20Grid-Searches.pdf. This provides an in depth explanation of the project.

The original example drawn from the Using history matching to speed up management strategy evaluation grid searches paper can be found at https://github.com/dtj504-pixel/DissertationWork/blob/main/case_study8.R.

Then, we have added the respective acquisition functions into the files below:
    - Expected Improvement: https://github.com/dtj504-pixel/DissertationWork/blob/main/looped_ver_case_study8_multi_point_EI.R
    - Augmented Expected Improvement: https://github.com/dtj504-pixel/DissertationWork/blob/main/looped_ver_case_study8_multi_point_AEI.R
    - Knowledge Gradient: https://github.com/dtj504-pixel/DissertationWork/blob/main/looped_ver_case_study8_KG.R

There are some further experimentations for the first half of the project in the Changing GP kernel and Gaussian Process Visualisation and Experimentation folders 
as well as in the https://github.com/dtj504-pixel/DissertationWork/blob/main/Documents/Kernel%20and%20Mean%20Experiments.qmd and 
https://github.com/dtj504-pixel/DissertationWork/blob/main/Documents/Deciding%20which%20acquisition%20function%20is%20best.qmd files.

Any experimentation with the MixME package can be found in the MixME_experimentation folder.

Results can be found in the Grid_Search_Results folders.
