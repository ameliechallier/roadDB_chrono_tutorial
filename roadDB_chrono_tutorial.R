### How to deal with large datasets using roadDB ###

#Load the roadDB and tidyverse libraries
library(roadDB)
library(tidyverse)

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
# Remove minimum age values (negative sd of 0 and positive sd of 1000000)
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
C14_Aurignacian_filtered <- road_get_dates(Aurignacian)|>filter(
  dating_method == "14C (radiocarbon) dating")|>filter(
    positive_standard_deviation != "NA")|>filter(
      negative_standard_deviation != 0)|>filter(
        age <= 50000)|>filter(age != 0)|>filter(
          material_dated != "shell marine" | material_dated != "NA")|>filter(
            date_of_analysis >= "2010")

# Calibrate
Aurignacian_cal<- calibrate(x=C14_Aurignacian_filtered$age,
          errors=C14_Aurignacian_filtered$positive_standard_deviation,
          calCurves="intcal20")

# Display the results
summary(Aurignacian_cal)

# Save the road_get_dates() data frame with uncalibrated ages in a .csv file
write.csv(C14_Aur_filtered, file = "C14_Aurignacian.csv")

### Now we have all information for the dates associated with Aurignacian assemblages
# stored in a .csv file
### To use the data for age modelling in the software Chronomodel, we need to slightly transform it

# Keep only the columns useful for the Chronomodel software
# Add a column 'Method' (14C) and a column for the caibration curve to be used in Chronomodel
Chronomodel_csv <- C14_Aur_filtered|>select(locality_id,
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
