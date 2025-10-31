# Simulation-Study-for-Comparison-of-Methods-for-Incorporating-Related-Data-when-Developing-CPMs
Comparison of Methods for Incorporating Related Data when Developing Clinical Prediction Models: A Simulation Study Paper


* R version 4.2.2
* Required packages:
    * require(c( "ROCR", "MLmetrics", "rms", "dplyr", "pmsampsize”,  "MASS", "pROC", "Metrics", "matrixcalc",
 	 "DescTools", "predtools", "tidyr", "dma", "magrittr", "ggplot2",  "wacolors"))

* Simulation parameters:
    * "X1_m_delta"=c(0, 0.8, 4), "X1_v_delta"=c(0, 0.2, 2),
	"X2_m_delta"=c(0, 1), "X2_v_delta"=0, "X3_m_delta"=0,
	"X3_v_delta"=0, "X4_m_delta"=0, "X4_v_delta"=0,
	"X5_m_delta"=0, "X5_v_delta"=0, "X6_m_delta"=0,
	"X6_v_delta"=0, "X7_m_delta"=0, "X7_v_delta"=0,
	"X8_m_delta"=0, "X8_v_delta"=0, "X9_m_delta"=0, 
	"X9_v_delta"=0, "X10_m_delta"=0, "X10_v_delta"=0,
	"X11_m_delta"=0, "X11_v_delta"=0, "X12_m_delta"=0,
	"X12_v_delta"=0, "X13_m_delta"=0, "X13_v_delta"=0,
	"X14_m_delta"=0, "X14_v_delta"=0, "U_m_delta"=0,
	"U_v_delta"=0, "X10_p_delta"=c(0, 0.1), "X11_p_delta"=0,
	"X12_p_delta"=0, "X13_p_delta"=0, "X14_p_delta"=0,
	"B0_delta"=c(0,0.9), "B1_delta"=c(0,0.4), "B2_delta"=0,
	"B3_delta"=0, "B4_delta"=0, "B5_delta"=0, "B6_delta"=0, 
	"B7_delta"=0, "B8_delta"=0, "B9_delta"=0, "B10_delta"=0,
	"B11_delta"=0, "B12_delta"=0, "B13_delta"=0, "B14_delta"=0,
	"BU_delta"=c(0,0.4) "target_split"=c(0.25, 0.5, 0.75), 
	"dev_samp_size"=c(1,2,3,4), "iteration" = seq(1,200)

* Simulation parameters combinations are stored in two files:
    *  SOURCE_PARAM.rds for development sample sizes 1,2 and 3 -> “dev_samp_size"=c(1,2,3)
    * SOURCE_PARAM_sampsize4_csf.rds for development sample size 4 -> “dev_samp_size"=4

* In these files, each scenario has 200 rows equivalent to 200 iterations.

* To get raw results for each scenario, run Main_all_models.R file wile passing the row numbers for the desired scenario from SOURCE_PARAM.rds or SOURCE_PARAM_sampsize4_csf.rds. You can switch between SOURCE_PARAM.rds or SOURCE_PARAM_sampsize4_csf.rds by comment/uncomment the desired files from SOURCE_PARAMETERS.R (top two lines) 

# Example to run No shift scenario, with development sample size 4 times the minimum required sample size (4xN_min) and target dataset proportion split 0.25:

* Use R version 4.2.2
* Load Required packages:
    * require(c( "ROCR", "MLmetrics", "rms", "dplyr", "pmsampsize”,  "MASS", "pROC", "Metrics", "matrixcalc",
 	 "DescTools", "predtools", "tidyr", "dma", "magrittr", "ggplot2",  "wacolors"))
* Get row numbers for the desired scenario :
    * run SOURCE.PARAM <- readRDS('SOURCE_PARAM_sampsize4_csf.rds')
    * rows<-which(
                 SOURCE.PARAM$X1_m_delta==0& SOURCE.PARAM$X1_v_delta==0&
                 SOURCE.PARAM$X2_m_delta==0& SOURCE.PARAM$X2_v_delta==0&
                 SOURCE.PARAM$X3_m_delta==0& SOURCE.PARAM$X3_v_delta==0&
                 SOURCE.PARAM$X4_m_delta==0& SOURCE.PARAM$X4_v_delta==0&
                 SOURCE.PARAM$X5_m_delta==0& SOURCE.PARAM$X5_v_delta==0&
                 SOURCE.PARAM$X6_m_delta==0& SOURCE.PARAM$X6_v_delta==0&
                 SOURCE.PARAM$X7_m_delta==0& SOURCE.PARAM$X7_v_delta==0&
                 SOURCE.PARAM$X8_m_delta==0& SOURCE.PARAM$X8_v_delta==0&
                 SOURCE.PARAM$X9_m_delta==0& SOURCE.PARAM$X9_v_delta==0&
                 SOURCE.PARAM$X10_m_delta==0& SOURCE.PARAM$X10_v_delta==0&
                 SOURCE.PARAM$X11_m_delta==0& SOURCE.PARAM$X11_v_delta==0&
                 SOURCE.PARAM$X12_m_delta==0& SOURCE.PARAM$X12_v_delta==0&
                 SOURCE.PARAM$X13_m_delta==0& SOURCE.PARAM$X13_v_delta==0&
                 SOURCE.PARAM$X14_m_delta==0& SOURCE.PARAM$X14_v_delta==0&
                 SOURCE.PARAM$U_m_delta==0& SOURCE.PARAM$U_v_delta==0&
                 SOURCE.PARAM$X10_p_delta==0& SOURCE.PARAM$X11_p_delta==0&
                 SOURCE.PARAM$X12_p_delta==0& SOURCE.PARAM$X13_p_delta==0&
                 SOURCE.PARAM$X14_p_delta==0& SOURCE.PARAM$B0_delta==0&
                 SOURCE.PARAM$B1_delta==0& SOURCE.PARAM$B2_delta==0&
                 SOURCE.PARAM$B3_delta==0& SOURCE.PARAM$B4_delta==0&
                 SOURCE.PARAM$B5_delta==0& SOURCE.PARAM$B6_delta==0&
                 SOURCE.PARAM$B7_delta==0& SOURCE.PARAM$B8_delta==0&
                 SOURCE.PARAM$B9_delta==0& SOURCE.PARAM$B10_delta==0&
                 SOURCE.PARAM$B11_delta==0& SOURCE.PARAM$B12_delta==0&
                 SOURCE.PARAM$B13_delta==0& SOURCE.PARAM$B14_delta==0&
                 SOURCE.PARAM$BU_delta==0& SOURCE.PARAM$target_split==0.25)
      
* Make sure that this line “SOURCE.PARAM <- readRDS('SOURCE_PARAM_sampsize4_csf.rds')” is active in SOURCE_PARAMETERS.R
* If you are using a system that can pass argument to the code file to run multiple jobs at the same time, pass the row numbers to Main_all_models.R
    * These two lines at top of Main_all_models.R file will catch the row number 
        * args <- commandArgs(trailingOnly = T) 
        * row <- as.numeric(args[1]) 
* Otherwise, you need to run each iteration on its Owen while passing a single row number to “row“ object, such as row <- 1
* Running Main_all_models.R will produce three results file for each iteration: 
    * cal_plot_results: results to plot calibration curves 
    * validation_results: for validation metrics results (AUC, CITL, CSLOPE, BrierScore, MER)
    * indv_preds_results: predicted values for specific patients to test individual instability
* Run csv_validation_metrics_group.R to group the validation metrics across 200 iterations per scenario and generate 3 validation results files grouped by the outcome-generating model model15, model5, model4
* Run Performance_metrics_plot.R to get the final plots of Performance metrics presented in the manuscript.
* Run proportion_split_plot.R to get Proportion split of source and target three plots for No shift.
* Run dev_sample_size_comparsion_plot.R to get Performance metrics with different sample size three plots for No shift.
* Run num_of_preds_comparison.R to get Performance metrics with different number of predictor variables three plots for No shift.
* Run tipping_point_analysis.R to get Tipping-point Analysis of Ancillary data benefit across Development Sample Sizes plot for No shift. 
* Run calibration_MER_IndvPred_plots.R to get Instability of estimated risks for nine individuals, Individual Calibration, and Instability in Mean Estimated Risk plots
* Run mean_calibration_curves.R to get Mean Calibration Instability Curves across 200 iterations for different scenarios.

Other Supplementary material plots:
* Run misspecification_factor_plot.R to get Correctly Specified vs. Misspecified Weighting Schemes results plot
* Run num_of_preds_103scenario_comparison.R to get Performance metrics with different number of predictor variables for Combination of Single Case-mix, Event rate and Predictor-outcome association shifts
* Run Delta_AUC.R to get Comparison of Discrimination Performance Across Recalibration Methods for Event rate shift table.
