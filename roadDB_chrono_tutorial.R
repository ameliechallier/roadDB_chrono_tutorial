### How to deal with large datasets using roadDB ###

# Load the roadDB and tidyverse libraries
library(roadDB)
library(tidyverse)

# Set your working directory (where your output files will be stored)
# Also possible from the 'Session' tab
setwd('C:/Users/Amelie/Documents/Presentations/Workshops')

### We want to query all radiocarbon dates associated with Aurignacian assemblages ###

# Check the argument "technocomplex" to find the exact denomination for Aurignacian
road_list_argument_values("technocomplex")

# Retrieve all Aurignacian assemblages
Aurignacian <- road_get_assemblages(technocomplex = "UP/ Aurignacian")

# Check the argument "dating_method" to target radiocarbon dating
road_list_argument_values("dating_method")

# Retrieve all radiocarbon dates associated with Aurignacian assemblages
C14_Aurignacian <- road_get_dates(Aurignacian)|>filter(
  dating_method == "14C (radiocarbon) dating")

# Load radiocarbon library
library(rcarbon)

# Try to calibrate the data
calibrate(x=C14_Aurignacian$age,
          errors=C14_Aurignacian$positive_standard_deviation,
          calCurves="intcal20")

### CALIBRATION FAIL ###
### We need to further filter the data in order to calibrate it ###

# Check the max and min values for the positive and negative standard deviation
# in your dataframe by clicking on the corresponding tabs in the data frame (accessible in the Environment) 
# Filter the data to remove minimum age values (negative sd of 0 and positive sd of 1000000)
# Remove NA values 
Filter1 <- road_get_dates(Aurignacian)|>filter(
  dating_method == "14C (radiocarbon) dating")|>filter(
  positive_standard_deviation != "NA") |> filter(
    negative_standard_deviation != 0)

# Remove uncalibrated ages > 50000 BP beyond the range of calibration
Filter2 <- road_get_dates(Aurignacian)|>filter(
  dating_method == "14C (radiocarbon) dating")|>filter(
  age <= 50000)

# We want to use the IntCal20 calibration curve.
# Marine shells should be calibrated using Marine20 => remove them.
# Unknown material_dated should also be removed for quality control. 
Filter3 <- road_get_dates(Aurignacian)|>filter(
  dating_method == "14C (radiocarbon) dating")|>
  filter(material_dated != "shell marine" | material_dated != "NA")

# Let's say we only want 'recent' data from the past 15 years
Filter4 <- road_get_dates(Aurignacian)|>filter(
  dating_method == "14C (radiocarbon) dating")|>
  filter(date_of_analysis >= "2010")

# Apply all filters at once to get a clear dataset
## Note that previous functions are for the sake of the examples
## You can always apply all filters at once as shown below
C14_Aurignacian_filtered <- road_get_dates(Aurignacian)|>filter(
  dating_method == "14C (radiocarbon) dating")|>filter(
    positive_standard_deviation != "NA")|>filter(
      negative_standard_deviation != 0)|>filter(
        age <= 50000)|>filter(age != 0)|>filter(
          material_dated != "shell marine" | material_dated != "NA")|>filter(
            date_of_analysis >= "2010")

# Save the road_get_dates() data frame with uncalibrated ages in a .csv file
write.csv(C14_Aurignacian_filtered, file = "C14_Aurignacian_uncal.csv")######

# Calibrate
Aurignacian_cal<- calibrate(x=C14_Aurignacian_filtered$age,
          errors=C14_Aurignacian_filtered$positive_standard_deviation,
          calCurves="intcal20")

# Display the results
summary(Aurignacian_cal)

# We need to convert the results into a data frame before saving it as a .csv file
make_summary_table <- function(Aurignacian_cal, C14_Aurignacian_filtered) {
  
  df1 <- summary(Aurignacian_cal) %>% 
    as.data.frame()
  
  # add the "geolayer", "archlayer", "assemblage_id" and "id" columns from
  # road_get_dates() output
  df1$geolayer <- C14_Aurignacian_filtered$geolayer
  df1$archlayer <- C14_Aurignacian_filtered$archlayer
  df1$assemblage_id <- C14_Aurignacian_filtered$assemblage_id
  df1$id <- C14_Aurignacian_filtered$id
  
  # Identify sigma columns
  sigma_cols <- names(df1)[str_detect(names(df1), "^(One|Two)Sigma_BP_\\d+$")]
  
  # Split all sigma columns into start / end
  for (col in sigma_cols) {
    df1 <- df1 %>%
      separate(
        !!sym(col),
        into = c(paste0(col, "_start"), paste0(col, "_end")),
        sep = " to ",
        convert = TRUE,
        remove = TRUE
      )
  }
  
  # Identify start/end columns
  one_start <- names(df1)[str_detect(names(df1), "^OneSigma_BP_\\d+_start$")]
  one_end   <- names(df1)[str_detect(names(df1), "^OneSigma_BP_\\d+_end$")]
  two_start <- names(df1)[str_detect(names(df1), "^TwoSigma_BP_\\d+_start$")]
  two_end   <- names(df1)[str_detect(names(df1), "^TwoSigma_BP_\\d+_end$")]
  
  # Compute row-wise summary values
  df1 <- df1 %>%
    rowwise() %>%
    mutate(
      OneSigma_start_max = if (length(one_start) > 0)
        max(c_across(all_of(one_start)), na.rm = TRUE) else NA_real_,
      
      OneSigma_end_min = if (length(one_end) > 0)
        min(c_across(all_of(one_end)), na.rm = TRUE) else NA_real_,
      
      TwoSigma_start_max = if (length(two_start) > 0)
        max(c_across(all_of(two_start)), na.rm = TRUE) else NA_real_,
      
      TwoSigma_end_min = if (length(two_end) > 0)
        min(c_across(all_of(two_end)), na.rm = TRUE) else NA_real_
    ) %>%
    ungroup()
  
  df1
}

table1 <- make_summary_table(Aurignacian_cal, C14_Aurignacian_filtered)

# Save the output of the calibrate() function + added columns
write.csv(table1,'C14_Aurignacian_cal.csv')

### If you want to use it in Chronomodel ###

## Now we have all the information for the dates associated with Aurignacian assemblages
# stored in a .csv file
## To use the data for age modelling in the software Chronomodel, we need to slightly transform it

# Keep only the columns useful for the Chronomodel software
# Add a column 'Method' (14C) and a column for the calibration curve to be used in Chronomodel
Chronomodel_csv <- C14_Aurignacian_filtered|>select(locality_id,
                                            analysis_number,
                                            age,
                                            positive_standard_deviation,)|> mutate(
                                            Method = "14C", .before = "analysis_number")|>
                                            mutate(method_param_c = "intcal20.14c")

# Save the .csv file to be used in Chronomodel
write.csv(Chronomodel_csv, file = "Chronomodel_Aurignacian.csv", sep = ",",
          row.names = FALSE,
          col.names = FALSE,
          quote = FALSE)

### Download the Chronomodel software from their Webpage ###
