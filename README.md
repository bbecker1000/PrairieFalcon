# PrairieFalcon

Ben Becker, CeeCee Chen, and Noor Wahl
Summer 2022 - UC Berkeley - Pinnacles National Park Prairie Falcon Occupancy Models


Code files:
- Part1_InitialCleaning.R: code takes 2022 Raptor Observations dataset, conducts initial filtering & cleaning, and adds necessary columns for subsequent analysis and graphing.

- Part2_DatasetPrep.R: code prepares PEFA dataset and PRFA dectection history dataset for subsequent analysis, visuaization and modeling.

- Part3_EDAandVisualization.R: code conducts exploratory data analysis and visualization.

- Part4_GetWeatherData.R: Code prepares weather data for modeling as covariates.

- Part5_CreatUMF.R:
    a: Code prepares STACKED Unmarked Frame for modeling.
    b: Code prepares unstacked Unmarked Frame for modeling.
    
- Part6_FitModels.R:
    a: Code builds and runs Static Multi-State Models with stacked data with Unmarked.
    b: Code builds and runs Dynamic Multi-State Models with unstacked data with Unmarked.
    c: Code builds and runs BIG Static Multi-State Models with stacked data with Unmarked using conditional binomial parameterization.
    d: Code builds and runs Big Dynamic Multi-State Models with unstacked data with Unmarked using multinomial parameterization.
    e: Code builds and runs Big Static Multi-State Models with stacked data with Unmarked using multinomial parameterization.
    
- Part7_ParamVisualization.R: Code visualizes predicted parameter values.



Model fit naming convention:

  mod(BBB)NN(X)(P)_(SSSSSSS)_fit
  
  Where:
    - () means the element may be empty
    
    - mod = model
    
    - BBB = "Big" if model is a big model
    
    - NN = two-digit model number
    
    - X = a letter further specifying different models under the same model number
    
    - P = model parameterization ("m" if multinomial)
    
    - SSSSSSS = static/dynamic ("stacked" if static)
    
  Example: mod04b_stacked_fit, modBig04m_fit



Data files:
- PRFASurveys.csv: only PRFA entries from FalconObservations.csv with formated Start_Date, 2003-2019, Survey_Pupose is NOT Casual or Unknown, Territory_Type is "Territory", and Dectection_Result includes "Yes".

- PRFADetectHistory.csv: detection history for each territory, includes all years and all surveys. Includes 0, 1, 2, and . for no survey.

