# dplyr and tidyr packages
library(dplyr)
library(tidyr)

# Read CSV file 
refine_original <- read.csv("refine_original.csv",fileEncoding="UTF-8-BOM")

# Convert company column to lowercase
refine_original$company <- tolower(refine_original$company)

# Convert all characters containing 'ak' and 'ph' to 'akzo' and 'philips' 
refine_original$company <- sub(pattern="ak.*",replacement = "akzo",refine_original$company)
refine_original$company <- sub(pattern="ph.*",replacement = "philips",refine_original$company)
refine_original$company <- sub(pattern="un.*",replacement = "unilever",refine_original$company)

# Separate product code column to 'product_code' and 'product_number'
refine_original <- separate(refine_original,col=Product.code...number,into=c("product_code","product_number"),sep="-")

# Create 'product_category' column to exactly replicate 'product_code'
refine_original$product_category <- refine_original$product_code

# In product_category column change "p", "v", "x" and "q" to "Smartphone", "TV", "Laptop" and "Tablet using a function()
Sub_product_category_func <- function(x,y){sub(pattern = x,replacement=y,refine_original$product_category)}

refine_original$product_category <- Sub_product_category_func("p","Smartphone")
refine_original$product_category <- Sub_product_category_func("v","TV")
refine_original$product_category <- Sub_product_category_func("x","Laptop")
refine_original$product_category <- Sub_product_category_func("q","Tablet")

# Add column named full_address where address, city and country are pasted together using comma as separator
refine_original <- refine_original %>% 
                    mutate(full_address = paste(refine_original$address,refine_original$city,refine_original$country,sep=","))

# Create new columns for company dummy variable using mutate. For values that have the respective company names, substitute 1. For other values, substitute 0
refine_original <- refine_original %>% mutate(company_philips = ifelse(company=="philips",1,0))
refine_original <- refine_original %>% mutate(company_akzo = ifelse(company=="akzo",1,0))
refine_original <- refine_original %>% mutate(company_van_houten = ifelse(company=="van houten",1,0))
refine_original <- refine_original %>% mutate(company_unilever = ifelse(company=="unilever",1,0))

# Create new columns for product dummy variable using mutate. For values that have the respective product names, substitute 1. For other values, substitute 0
refine_original <- refine_original %>% mutate(product_smartphone = ifelse(product_category=="Smartphone",1,0))
refine_original <- refine_original %>% mutate(product_tv = ifelse(product_category=="TV",1,0))
refine_original <- refine_original %>% mutate(product_laptop = ifelse(product_category=="Laptop",1,0))
refine_original <- refine_original %>% mutate(product_tablet = ifelse(product_category=="Tablet",1,0))

# Save new file called refine_clean      
refine_clean <- refine_original

# Write final csv output file to working directory 
write.csv(refine_clean,file="refine_clean.csv") 
