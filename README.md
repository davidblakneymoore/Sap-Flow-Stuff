# Sap-Flow-Stuff

This repository contains two CRBasic programs that collects sap flow measurements using heat-pulse sap flow sensors read in single-ended mode. In this program, both the heat-ratio method (Burgess et al., 2001) and the maximum-heat-ratio method (Gutierrez Lopez et al., 2021) are used to measure sap flow. This program does not record raw sap flow data.

These two CRBasic scripts are to be used with Campbell Scientific, Inc. dataloggers and multiplexers. Sap flow sensors should be connected to one [AM16/32B multiplexer](https://www.campbellsci.com/am16-32b?gad_source=1&gclid=CjwKCAjwps-zBhAiEiwALwsVYT2r0gWfglJF-BsIHIBeonoB4e-BOCLsAIeSysO8C6i1qrUywr73TxoCBTUQAvD_BwE) in '4X16' mode which is in turn connected to a [CR1000 datalogger](https://www.campbellsci.com/cr1000). More information can be found in the comments in the scripts.

One CRBasic program has a scan rate of 1 s and is to be used when fewer than 33 sap flow thermocouples are being measured (when fewer than 33 multiplexer ports are being read). The other CRBasic program has a scan rate of 3 s and is to be used when more than 32 sap flow thermocouples are being measured (when more than 32 multiplexer ports are being read). Additional modifications to these scripts would be needed if a second multiplexer is added (for increasing the number of thermocouples that can be measured), if the sap flow sensors are wired directly to the datalogger (if no multiplexers are used), and if differential-mode thermocouples are used instead of single-ended thermocouples.

This repository also contains an R script for processing these data. This R script still needs to be updated to include code to process the times at which the maximum temperatures occur for the maximum-heat-ratio method!

<b>Works Cited</b>

Burgess, S.S.O., M.A. Adams, N.C. Turner, C.R. Beverly, C.K. Ong, A.A.H. Khan, and T.M. Bleby. 2001. An improved heat pulse method to measure low and reverse rates of sap flow in woody plants. Tree Physiol. 21:589-598.

Gutierrez Lopez, J., T. Pypker, J. Licata, S.O. Burgess, and H. Asbjornsen. 2021. Maximum heat ratio: bi-directional method for fast and slow sap flow measurements. Plant Soil. 469:503-523.

Marshall, D.C. 1958. Measurement of sap flow in conifers by heat transport. Plant Physiol. 33:385-396.
