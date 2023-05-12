# Blood-cancer-risk-and-prediction
This is a project for the Translational Data Science module

Background:
- Altered blood cell counts can show early signs of disease
- Blood cenn counts vary in a multivariate way based on different demographic and population variables 
- Disentangling disease-led variation from normal variation from variation has a strong prognostic potential 


Possible research questions: 
- Blood cell phenotypes: can we identify clusters of people with shared blood characteristics?
- Risk Partitioning: can we estimate thee contribution of each group to blood cancer (or other disease) risk?
- Targeted analyses: can we identify lifestyle variables and/or other biomarkers that account for a large porpotion of (specific) blood phenotypes?


Expected outcomes: 
- Clustering results: determine and quantify the blood cell cluster groups and report theit main characteristics
- Biological integration: idenitfy possible lifestyle and biomarkers associated with cluster membership
- Disease risk: quantify the blood cancer risk and predictive power for each group


Possible approaches:
- Clustering: advanced clustering algorithms to detect groups that are not necessarily strongly separable
- Statistical models: stbaility-based models for variable selection in the biological integration step and disease risk estimation
- Machine learning: blood cancer prediction using blood phenotypes

Folders:
- Pre-processing scripts are in matching and imputation folders
- Final models contains the final random forest, logistic regression and clustering scripts
- Cluster characteristics are described in 'clustering' and 'cluster_descriptives' folders

<img width="648" alt="Screenshot 2023-02-19 at 10 01 02 PM" src="https://user-images.githubusercontent.com/111628669/219977741-8ae477e1-973a-4af8-bbf7-0b99954e5ba3.png">

Dataset used: UK Biobank dataset
