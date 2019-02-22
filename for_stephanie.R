library(tidyverse)
library(readxl)

inputfile = as.character(fs::file_info("/home/rob/Downloads/2019-02-22 MH Drug List.xlsx")$path)
fs::file_exists(inputfile)

readxl::excel_sheets(inputfile)

getdrugdata <- function(path, sheetnum, target1, target2_col, target2_start_row, target2_stop_row, drug_class, ...){
  print(path)
  print("-------------")
  print(class(path))
  print("-------------")
  print(sheetnum)
  
  print(target1)
  print(target2_col)
  print(target2_start_row)
  print(target2_stop_row)
  print(drug_class)
  target1m=paste0(target1, ":", target1)
  
  print(target1m)
  
  drug_name <-  readxl::read_xlsx(path=path, sheet=sheetnum, range=target1m)
  print(drug_name)
  AHFS_range = paste0(target2_col,4 )
  print(AHFS_range)
  
  AHFS_designation <- readxl::read_xlsx(path=path, sheet=sheetnum, range=AHFS_range)
  
  ATC_range = paste0(target2_col,6 )
  print(ATC_range)
  ATC_designation <- readxl::read_xlsx(path=path, sheet=sheetnum, range=ATC_range)
  
  
  AIG_range = paste0(target2_col,8 )
  print(AIG_range)
  AIG_designation <- readxl::read_xlsx(path=path, sheet=sheetnum, range=AIG_range)
  
  dinrange = paste0(target2_col, target2_start_row, ":",target2_col, target2_stop_row )
  print(dinrange)
  
  drug  <-  readxl::read_xlsx(path, sheet=sheetnum, range=dinrange, 
                              col_names="DIN") %>% 
    mutate(
      drugname =  make.names(str_to_lower(colnames(drug_name))), 
      AHFS     =  make.names(colnames(AHFS_designation)), 
      ATC      =  make.names(colnames(ATC_designation)), 
      AIG      =  make.names(colnames(AIG_designation)), 
      drug_type = drug_class
    )
}

calling_dataset <- tibble(
  path = inputfile,
  sheetnum=3, 
  target1=c("A2", "C2", "E2", "G2", "I2", "K2", "M2", "O2"),
  target2_col = c("B", "D", "F", "H", "J", "L", "N", "P"),
  target2_start_row = 12,
  target2_stop_row=c(98, 146, 103, 20, 92, 16, 62, 42),
  drug_class="anti_depressants"
)
calling_dataset
rm(anti_depressants)

calling_dataset1<- calling_dataset %>% slice(1)
calling_dataset1

old_fashioned <- getdrugdata(calling_dataset1$path,
                             calling_dataset1$sheetnum, 
                             calling_dataset1$target1, 
                             calling_dataset1$target2_col, 
                             calling_dataset1$target2_start_row, 
                             calling_dataset1$target2_stop_row, 
                             calling_dataset1$drug_class
)
nrow(old_fashioned)

rm(anti_depressants)
anti_depressants <- calling_dataset1 %>%  map_df(.,
                                                getdrugdata,
                                                path = .$path,
                                                sheetnum   = .$sheetnum, 
                                                target1 = .$target1, 
                                                target2_col = .$target2_col, 
                                                target2_start_row = .$target2_start_row, 
                                                target2_stop_row = .$target2_stop_row, 
                                                drug_class = .$drug_class
                                                )
nrow(anti_depressants)





