
# R Script for Processing Sap Flow Data

# David Moore
# davidblakneymoore@gmail.com
# April 2022


# Explanation

# This script should be used in conjunction with the 'CRBasic Script for
# Collecting Sap Flow Data.CR1' CRBasic script
# (https://github.com/davidblakneymoore/Sap-Flow-Stuff/blob/main/CRBasic%20Script%20for%20Collecting%20Sap%20Flow%20Data.CR1).
# Please refer to that script for specific instructions about sensors.


# Constants and Metadata

# Constants

Working_Directory <- "C:/Sap Flow Data"
Unique_Station_Names <- c("Station_1", "Station_2", "Station_3")
Number_of_Ports_Read_in_Each_Station <- 64
Thermal_Diffusivity_of_Green_Wood <- 0.0019
Wood_Density <- 700 # Units: kg * m ^ -3
Specific_Heat_Capacity_of_Wood <- 1200 # Units: J * kg ^ -1 * º C ^ -1 (at 20 º C)
Water_Content_of_Sapwood <- 0.7
Specific_Heat_Capacity_of_Sap <- 4182 # Units: J * kg ^ -1 * º C ^ -1 (at 20 º C)
Density_of_Water <- 1000 # Units: kg * m ^ -3
Distance_Between_Heater_and_Probes <- 0.5 # Units: cm
Number_of_Sensors_in_Each_Tree <- 1
Number_of_Thermocouples_Per_Sensor <- 3
Thermocouple_Depths <- c(0.5, 1.0, 1.5) # Units: cm # If you're using 1.5-in sensors, this should be 'c(1.0, 2.25, 3.5)'
Thermocouple_Positions <- c("Outer", "Middle", "Inner")
b <- 1.6821 # This constant is for the wound-correction equation that I didn't include. See Burgess et al. (2001) for more information.
c <- -0.0015 # This constant is for the wound-correction equation that I didn't include. See Burgess et al. (2001) for more information.
d <- 0.0002 # This constant is for the wound-correction equation that I didn't include. See Burgess et al. (2001) for more information.
Time_Between_Measurements <- 15 # Units: min
Wire_Colors <- c("blue", "brown", "black")

# Station 'Station_1'

Ports_Used_in_Station_1 <- c(1:24)

# Station 'Station_2'

Ports_Used_in_Station_2 <- c(1:48)

# Station 'Station_3'

Ports_Used_in_Station_3 <- c(1:22, 25:26)

# Functions

Heat_Pulse_Velocity_Function <- function (x) {
  ((Thermal_Diffusivity_of_Green_Wood / Distance_Between_Heater_and_Probes) * log(x)) * 3600
}
Sap_Flux_Density_Function <- function (x) {
  (Wood_Density * x / Density_of_Water) * (Water_Content_of_Sapwood + (Specific_Heat_Capacity_of_Wood / Specific_Heat_Capacity_of_Sap))
}
Sap_Velocity_Function <- function (x) {
  (x * Wood_Density * (Specific_Heat_Capacity_of_Wood + (Water_Content_of_Sapwood * Specific_Heat_Capacity_of_Sap))) / (Density_of_Water * Specific_Heat_Capacity_of_Sap)
}

# Other constants

Unique_Station_Names <- Unique_Station_Names[order(Unique_Station_Names)]
Number_of_Stations <- length(Unique_Station_Names)
Number_of_Thermocouples_Per_Tree <- Number_of_Sensors_in_Each_Tree * Number_of_Thermocouples_Per_Sensor
Number_of_Hours_in_a_Day <- 24 # Units: hr
Number_of_Minutes_in_an_Hour <- 60
Number_of_Measurements_in_a_Day <- Number_of_Hours_in_a_Day * Number_of_Minutes_in_an_Hour / Time_Between_Measurements

# Generating a list of station metadata

Ports_Used_in_Station_1 <- I(list(Ports_Used_in_Station_1))
Ports_Used_in_Station_2 <- I(list(Ports_Used_in_Station_2))
Ports_Used_in_Station_3 <- I(list(Ports_Used_in_Station_3))
Station_1 <- data.frame(Ports_Used = Ports_Used_in_Station_1)
Station_2 <- data.frame(Ports_Used = Ports_Used_in_Station_2)
Station_3 <- data.frame(Ports_Used = Ports_Used_in_Station_3)
Station_Data_List <- list(Station_1 = Station_1, Station_2 = Station_2, Station_3 = Station_3)
Station_Data_List <- Station_Data_List[order(names(Station_Data_List))]


# Processing the Raw Data

# Upload the sap flow data

setwd(Working_Directory)
Heat_Ratio_Method_Files <- file.info(grep(".dat", list.files(Working_Directory, full.names = T), value = T))
Heat_Ratio_Method_Files$Names <- rownames(Heat_Ratio_Method_Files)
rownames(Heat_Ratio_Method_Files) <- NULL
Sap_Flow_Data_List <- lapply(Unique_Station_Names, function (x) {
  Sap_Flow_Data_List <- Heat_Ratio_Method_Files[grep(x, Heat_Ratio_Method_Files$Names), ]
})
names(Sap_Flow_Data_List) <- Unique_Station_Names
Sap_Flow_Data_List <- lapply(Sap_Flow_Data_List, function (x) {
  x[, c("Names")]
})
Sap_Flow_Data_List <- lapply(Sap_Flow_Data_List, function (x) {
  x <- lapply(x, function (y) {
    read.table(y, skip = 1, stringsAsFactors = F, sep = ",", na.strings = "NAN", header = T)
  })
})

# Format the sap flow data

Sap_Flow_Data_List <- lapply(Sap_Flow_Data_List, function (x) {
  x <- lapply(x, function (y) {
    y[-c(1:2), ]
  })
})
Sap_Flow_Data_List <- lapply(Sap_Flow_Data_List, function (x) {
  do.call("rbind", x)
})
Sap_Flow_Data_List <- lapply(Sap_Flow_Data_List, function (x) {
  if (any(duplicated(x))) {
    x[-which(duplicated(x)), ]
  } else {
    return (x)
  }
})
Sap_Flow_Data_List <- lapply(Sap_Flow_Data_List, function (x) {
  x$TIMESTAMP <- as.POSIXct(x$TIMESTAMP, format = c("%Y-%m-%d %H:%M"), tz = "UTC")
  x[, 2:ncol(x)] <- lapply(x[, 2:ncol(x)], as.numeric)
  x
})
Sap_Flow_Data_List <- lapply(Sap_Flow_Data_List, function (x) {
  y <- x[order(x$TIMESTAMP), ]
  rownames(y) <- NULL
  y
})
Sap_Flow_Data_List <- lapply(Sap_Flow_Data_List, function (x) {
  colnames(x) <- gsub("_Avg|_Min", "", colnames(x))
  colnames(x)[which(colnames(x) == "TIMESTAMP")] <- "Time"
  colnames(x)[which(colnames(x) == "RECORD")] <- "Record_Number"
  x
})
Sap_Flow_Data_List <- mapply(function (x, y) {
  z <- x[, grep("Wood_Temperature", colnames(x))[unlist(y$Ports_Used)]]
  x1 <- cbind(x[, c("Time", "Record_Number", "Panel_Temperature", "Big_Battery_Voltage", "Small_Battery_Voltage")], z, x[, grep("After_Heat_Pulse_Temperature", colnames(x))])
  z <- x1[, setdiff(grep("After_Heat_Pulse_Temperature", colnames(x1)), grep("Max_After_Heat_Pulse_Temperature", colnames(x1)))[unlist(y$Ports_Used)]]
  x2 <- cbind(x1[, c("Time", "Record_Number", "Panel_Temperature", "Big_Battery_Voltage", "Small_Battery_Voltage")], x1[, grep("Wood_Temperature", colnames(x1))], z, x1[, grep("Max_After_Heat_Pulse_Temperature", colnames(x1))])
  z <- x2[, setdiff(grep("Max_After_Heat_Pulse_Temperature", colnames(x2)), grep("Max_After_Heat_Pulse_Temperature_MA", colnames(x2)))[unlist(y$Ports_Used)]]
  x3 <- cbind(x2[, c("Time", "Record_Number", "Panel_Temperature", "Big_Battery_Voltage", "Small_Battery_Voltage")], x2[, grep("Wood_Temperature", colnames(x2))], x2[, setdiff(grep("After_Heat_Pulse_Temperature", colnames(x2)), grep("Max_After_Heat_Pulse_Temperature", colnames(x2)))], z, x2[, grep("Max_After_Heat_Pulse_Temperature_MA", colnames(x2))])
  z <- x3[, grep("Max_After_Heat_Pulse_Temperature_MA", colnames(x3))[unlist(y$Ports_Used)]]
  x4 <- cbind(x3[, c("Time", "Record_Number", "Panel_Temperature", "Big_Battery_Voltage", "Small_Battery_Voltage")], x3[, grep("Wood_Temperature", colnames(x3))], x3[, setdiff(grep("After_Heat_Pulse_Temperature", colnames(x3)), grep("Max_After_Heat_Pulse_Temperature_MA", colnames(x3)))], z)
  x4
}, x = Sap_Flow_Data_List, y = Station_Data_List, SIMPLIFY = F)
Sap_Flow_Data_List <- lapply(Sap_Flow_Data_List, function (x) {
  colnames(x) <- gsub("[.]$", "", colnames(x))
  colnames(x) <- gsub("[.]", "_Multiplexer_Port_", colnames(x))
  colnames(x)[grep("Max_After_Heat_Pulse_Temperature", colnames(x))] <- gsub("Max", "Maximum", colnames(x)[grep("Max_After_Heat_Pulse_Temperature", colnames(x))])
  colnames(x)[grep("Maximum_After_Heat_Pulse_Temperature_MA", colnames(x))] <- gsub("MA", "Moving_Average", colnames(x)[grep("Maximum_After_Heat_Pulse_Temperature_MA", colnames(x))])
  x
})

# Calculate the deltas

Sap_Flow_Data_List <- lapply(Sap_Flow_Data_List, function (x) {
  Heat_Ratio_Method_Deltas <- x[, setdiff(grep("After_Heat_Pulse_Temperature", colnames(x)), grep("Maximum_After_Heat_Pulse_Temperature", colnames(x)))] - x[, grep("Wood_Temperature", colnames(x))]
  colnames(Heat_Ratio_Method_Deltas) <- gsub("Wood_Temperature", "Heat_Ratio_Method_Delta", colnames(x)[grep("Wood_Temperature", colnames(x))])
  Maximum_Heat_Ratio_Method_Deltas <- x[, setdiff(grep("Maximum_After_Heat_Pulse_Temperature", colnames(x)), grep("Maximum_After_Heat_Pulse_Temperature_Moving_Average", colnames(x)))] - x[, grep("Wood_Temperature", colnames(x))]
  colnames(Maximum_Heat_Ratio_Method_Deltas) <- gsub("Wood_Temperature", "Maximum_Heat_Ratio_Method_Delta", colnames(x)[grep("Wood_Temperature", colnames(x))])
  Maximum_Heat_Ratio_Method_Moving_Average_Deltas <- x[, grep("Maximum_After_Heat_Pulse_Temperature_Moving_Average", colnames(x))] - x[, grep("Wood_Temperature", colnames(x))]
  colnames(Maximum_Heat_Ratio_Method_Moving_Average_Deltas) <- gsub("Wood_Temperature", "Maximum_Heat_Ratio_Method_Moving_Average_Delta", colnames(x)[grep("Wood_Temperature", colnames(x))])
  cbind(x, Heat_Ratio_Method_Deltas, Maximum_Heat_Ratio_Method_Deltas, Maximum_Heat_Ratio_Method_Moving_Average_Deltas)
})

# Calculate the ratios

Upper_Thermocouples_Only <- rep(c(T, F), each = Number_of_Thermocouples_Per_Sensor)
Lower_Thermocouples_Only <- rep(c(F, T), each = Number_of_Thermocouples_Per_Sensor)
Thermocouple_Indices <- lapply(seq_len(Number_of_Thermocouples_Per_Sensor), function (x) {
  y <- rep(F, Number_of_Thermocouples_Per_Sensor)
  y[x] <- T
  y
})
Multiplexer_Ports <- lapply(Station_Data_List, function (x) {
  unlist(lapply(seq_len(length(unlist(x$Ports_Used)) / 2 / Number_of_Thermocouples_Per_Sensor), function (y) {
    sapply(seq_len(length(Thermocouple_Indices)), function (z) {
      paste(unlist(x$Ports_Used)[Thermocouple_Indices[[z]]][(2 * (y - 1)) + 1], "and", unlist(x$Ports_Used)[Thermocouple_Indices[[z]]][(2 * y)], sep = "_")
    })
  }))
})
Sap_Flow_Data_List <- mapply(function (x, y) {
  Heat_Ratio_Method_Ratios <- x[, setdiff(grep("Heat_Ratio_Method_Delta", colnames(x)), grep("Maximum_Heat_Ratio_Method_Delta", colnames(x)))][Upper_Thermocouples_Only] / x[, setdiff(grep("Heat_Ratio_Method_Delta", colnames(x)), grep("Maximum_Heat_Ratio_Method_Delta", colnames(x)))][Lower_Thermocouples_Only]
  colnames(Heat_Ratio_Method_Ratios) <- gsub("Multiplexer_Port_[[:digit:]]*$", "Multiplexer_Ports", gsub("Delta", "Ratio", colnames(Heat_Ratio_Method_Ratios)))
  colnames(Heat_Ratio_Method_Ratios) <- paste(colnames(Heat_Ratio_Method_Ratios), y, sep = "_")
  Maximum_Heat_Ratio_Method_Ratios <- x[, setdiff(grep("Maximum_Heat_Ratio_Method_Delta", colnames(x)), grep("Maximum_Heat_Ratio_Method_Moving_Average_Delta", colnames(x)))][Upper_Thermocouples_Only] / x[, setdiff(grep("Maximum_Heat_Ratio_Method_Delta", colnames(x)), grep("Maximum_Heat_Ratio_Method_Moving_Average_Delta", colnames(x)))][Lower_Thermocouples_Only]
  colnames(Maximum_Heat_Ratio_Method_Ratios) <- gsub("Multiplexer_Port_[[:digit:]]*$", "Multiplexer_Ports", gsub("Delta", "Ratio", colnames(Maximum_Heat_Ratio_Method_Ratios)))
  colnames(Maximum_Heat_Ratio_Method_Ratios) <- paste(colnames(Maximum_Heat_Ratio_Method_Ratios), y, sep = "_")
  Maximum_Heat_Ratio_Method_Moving_Average_Ratios <- x[, grep("Maximum_Heat_Ratio_Method_Moving_Average_Delta", colnames(x))][Upper_Thermocouples_Only] / x[, grep("Maximum_Heat_Ratio_Method_Moving_Average_Delta", colnames(x))][Lower_Thermocouples_Only]
  colnames(Maximum_Heat_Ratio_Method_Moving_Average_Ratios) <- gsub("Multiplexer_Port_[[:digit:]]*$", "Multiplexer_Ports", gsub("Delta", "Ratio", colnames(Maximum_Heat_Ratio_Method_Moving_Average_Ratios)))
  colnames(Maximum_Heat_Ratio_Method_Moving_Average_Ratios) <- paste(colnames(Maximum_Heat_Ratio_Method_Moving_Average_Ratios), y, sep = "_")
  cbind(x, Heat_Ratio_Method_Ratios, Maximum_Heat_Ratio_Method_Ratios, Maximum_Heat_Ratio_Method_Moving_Average_Ratios)
}, x = Sap_Flow_Data_List, y = Multiplexer_Ports, SIMPLIFY = F)

# Calculate the heat pulse velocities

Sap_Flow_Data_List <- lapply(Sap_Flow_Data_List, function (x) {
  Heat_Pulse_Velocities <- as.data.frame(lapply(x[grep("Ratio_Multiplexer_Ports", colnames(x))], Heat_Pulse_Velocity_Function))
  colnames(Heat_Pulse_Velocities) <- gsub("Ratio_Multiplexer_Ports", "Heat_Pulse_Velocity_Multiplexer_Ports", colnames(Heat_Pulse_Velocities))
  cbind(x, Heat_Pulse_Velocities)
})

# Here, you will need to baseline your data. You could also calculate moving
# averages or combine heat-ratio-method data with maximum-heat-ratio-method
# data.

# Calculate the sap flux densities

Sap_Flow_Data_List <- lapply(Sap_Flow_Data_List, function (x) {
  Sap_Flux_Densities <- as.data.frame(lapply(x[grep("Heat_Pulse_Velocity", colnames(x))], Sap_Flux_Density_Function))
  colnames(Sap_Flux_Densities) <- gsub("Heat_Pulse_Velocity", "Sap_Flux_Density", colnames(Sap_Flux_Densities))
  cbind(x, Sap_Flux_Densities)
})

# Calculate the sap velocities

Sap_Flow_Data_List <- lapply(Sap_Flow_Data_List, function (x) {
  Sap_Velocities <- as.data.frame(lapply(x[grep("Heat_Pulse_Velocity", colnames(x))], Sap_Velocity_Function))
  colnames(Sap_Velocities) <- gsub("Heat_Pulse_Velocity", "Sap_Velocity", colnames(Sap_Velocities))
  cbind(x, Sap_Velocities)
})

# Here, you can calculate total tree water use ('sap flow') using sap
# velocities by taking into account the areas of influence of each
# thermocouple.


# Works Cited

# Burgess, S.S.O., M.A. Adams, N.C. Turner, C.R. Beverly, C.K. Ong, A.A.H.
# Khan, and T.M. Bleby. 2001. An improved heat pulse method to measure low and
# reverse rates of sap flow in woody plants. Tree Physiol. 21:589-598.
