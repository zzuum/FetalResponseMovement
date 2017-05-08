# MAE 207 Project
# Fetal Movement Analysis
# Niklas Braun
####
# Data import ----
classical.silence <- readxl::read_excel(
  'Test_Data_May7.xlsx', 
  sheet = 1,
  skip = 2)[, -7] # Extra Unknown column
colnames(classical.silence) <- c(
  'Time',
  'Ch. A',
  'Ch. B',
  'Ch. C',
  'Move #',
  'Move Duration'
)
classical.65 <- readxl::read_excel(
  'Test_Data_May7.xlsx', 
  sheet = 2,
  skip = 2)[, -7] # Extra Unknown column
colnames(classical.65) <- c(
  'Time',
  'Ch. A',
  'Ch. B',
  'Ch. C',
  'Move #',
  'Move Duration'
)
classical.75 <- readxl::read_excel(
  'Test_Data_May7.xlsx', 
  sheet = 3,
  skip = 2)[, -7] # Extra Unknown column
colnames(classical.75) <- c(
  'Time',
  'Ch. A',
  'Ch. B',
  'Ch. C',
  'Move #',
  'Move Duration'
)
classical.85 <- readxl::read_excel(
  'Test_Data_May7.xlsx', 
  sheet = 4,
  skip = 2)[, -7] # Extra Unknown column
colnames(classical.85) <- c(
  'Time',
  'Ch. A',
  'Ch. B',
  'Ch. C',
  'Move #',
  'Move Duration'
)
