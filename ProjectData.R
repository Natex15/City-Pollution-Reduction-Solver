project_full_names <- c(
  "Large Solar Park", "Small Solar Installations", "Wind Farm", 
  "Gas-to-renewables conversion", "Boiler Retrofit", 
  "Catalytic Converters for Buses", "Diesel Bus Replacement", 
  "Traffic Signal/Flow Upgrade", "Low-Emission Stove Program", 
  "Residential Insulation/Efficiency", "Industrial Scrubbers", 
  "Waste Methane Capture System", "Landfill Gas-to-energy", 
  "Reforestation (acre-package)", "Urban Tree Canopy Program (street trees)", 
  "Industrial Energy Efficiency Retrofit", "Natural Gas Leak Repair", 
  "Agricultural Methane Reduction", "Clean Cookstove & Fuel Switching", 
  "Rail Electrification", "EV Charging Infrastructure", 
  "Biochar for soils (per project unit)", "Industrial VOC", 
  "Heavy-Duty Truck Retrofit", "Port/Harbor Electrification", 
  "Black Carbon reduction", "Wetlands restoration", 
  "Household LPG conversion program", "Industrial process change", 
  "Behavioral demand-reduction program"
)

data_vector <- c(
  4000, 60,   0,    0,    0,    0,    0,    0,    0,     0,     0,
  1200, 18,   0,    0,    0,    0,    0,    0,    0,     0,     0,
  3800, 55,   0,    0,    0,    0,    0,    0,    0,     0,     0,
  3200, 25,   1,    0.2,  0.1,  1.5,  0.5,  2,    0.05,  0.01,  0.3,
  1400, 20,   0.9,  0.4,  0.2,  0.1,  0.05, 1.2,  0.02,  0.01,  0.05,
  2600, 30,   2.8,  0.6,  0.8,  0,    0.5,  5,    0.01,  0.05,  0.02,
  5000, 48,   3.2,  0.9,  1,    0,    0.7,  6,    0.02,  0.08,  0.03,
  1000, 12,   0.6,  0.1,  0.4,  0.05, 0.2,  3,    0.02,  0.02,  0.01,
  180,  2,    0.02, 0.01, 0.7,  0,    0.01, 1.5,  0.03,  0.2,   0,
  900,  15,   0.1,  0.05, 0.05, 0.02, 0.02, 0.5,  0,     0,     0.01,
  4200, 6,    0.4,  6,    0.4,  0,    0.1,  0.6,  0.01,  0.01,  0,
  3600, 28,   0.2,  0.1,  0.05, 8,    0.2,  0.1,  0,     0,     0.05,
  3400, 24,   0.15, 0.05, 0.03, 6.5,  0.1,  0.05, 0,     0,     0.03,
  220,  3.5,  0.04, 0.02, 0.01, 0.8,  0.03, 0.1,  0.01,  0.005, 0.005,
  300,  4.2,  0.06, 0.01, 0.03, 0.6,  0.02, 0.15, 0.005, 0.02,  0.002,
  1600, 22,   0.5,  0.3,  0.15, 0.2,  0.1,  1,    0.01,  0.01,  0.03,
  1800, 10,   0.05, 0.01, 0.01, 4,    0.02, 0.02, 0,     0,     0.01,
  2800, 8,    0.02, 0.01, 0.02, 7.2,  0.05, 0.02, 0.1,   0,     0.05,
  450,  3.2,  0.04, 0.02, 0.9,  0.1,  0.02, 2,    0.05,  0.25,  0,
  6000, 80,   2,    0.4,  1.2,  0,    0.6,  10,   0.02,  0.1,   0.05,
  2200, 20,   0.3,  0.05, 0.1,  0,    0.05, 0.5,  0.01,  0.01,  0.01,
  1400, 6,    0.01, 0,    0.01, 2.5,  0.01, 0.01, 0.2,   0,     0.02,
  2600, 2,    0.01, 0,    0,    0,    6.5,  0.1,  0,     0,     0,
  4200, 36,   2.2,  0.6,  0.6,  0,    0.3,  4.2,  0.01,  0.04,  0.02,
  4800, 28,   1.9,  0.8,  0.7,  0,    0.2,  3.6,  0.01,  0.03,  0.02,
  600,  1.8,  0.02, 0.01, 0.6,  0.05, 0.01, 1,    0.02,  0.9,   0,
  1800, 10,   0.03, 0.02, 0.02, 3.2,  0.01, 0.05, 0.15,  0.02,  0.04,
  700,  2.5,  0.03, 0.01, 0.4,  0.05, 0.02, 1.2,  0.03,  0.1,   0,
  5000, 3,    0.02, 0.01, 0,    0,    0,    0,    0,     0,     1.5,
  400,  9,    0.4,  0.05, 0.05, 0.01, 0.3,  2.5,  0.01,  0.01,  0.01
)

#Matrix
projects_matrix = matrix(data_vector, nrow = 30, ncol = 11, byrow = TRUE)
colnames(projects_matrix) = c("Costs", "CO2", "NOx", "SO2", "PM2.5", "CH4", "VOC", "CO", "NH3", "BC", "N2O")
rownames(projects_matrix) = project_full_names

# Pollutants
pollutant_names = c("CO2", "NOx", "SO2", "PM2.5", "CH4","VOC", "CO", "NH3", "BC", "N2O")
target_values = c(1000, 35, 25, 20, 60, 45, 80, 12, 6, 10)
pollutant_targets = setNames(target_values, pollutant_names)
