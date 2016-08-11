### Read data frame ###
refine_original <- data.frame(refine_original)
head(refine_original)

### change company values to all lowercase ###
refine_original$company[c(1,2,3,4,5,6,14,15,16)] = "philips"
refine_original$company[c(7:13)] = "akzo"
refine_original$company[c(17:21)] = "van houten"
refine_original$company[c(22:24)] = "unilever"
head(refine_original)

### split up product code and product number ###
product_code_and_number <- refine_original[-c(1,3,4,5,6)]
product_code_and_number <- data.frame(do.call('rbind', strsplit(as.character(product_code_and_number$Product.code...number),'-',fixed=TRUE)))
colnames(product_code_and_number) <- c("product_code", "product_number")
refine_original <- refine_original[-c(2)]
refine_original <- data.frame(append(refine_original,list(product_code_and_number),after=match("company",names(refine_original))))
head(refine_original)

### Add product categories ###
refine_original$product_category[refine_original$product_code=="p"] <- "Smartphone"
refine_original$product_category[refine_original$product_code=="v"] <- "TV"
refine_original$product_category[refine_original$product_code=="x"] <- "Laptop"
refine_original$product_category[refine_original$product_code=="q"] <- "Tablet"
head(refine_original)

### Concatenate the three address fields ###
refine_original$full_address <- paste(refine_original$address,",",refine_original$city,",",refine_original$country)
refine_original <- refine_original[-c(4,5,6)]
refine_original <- refine_original[c(1,2,3,6,4,5)]
head(refine_original)

### 4 dummy varibales for company and product category ###
refine_original$company_philips <- ifelse(refine_original$company == "philips", 1, 0)
refine_original$company_akzo <- ifelse(refine_original$company == "akzo", 1, 0)
refine_original$company_van_houten <- ifelse(refine_original$company == "van houten", 1, 0)
refine_original$company_unilever <- ifelse(refine_original$company == "unilever", 1, 0)

refine_original$product_smartphone <- ifelse(refine_original$product_category == "Smartphone", 1, 0)
refine_original$product_tv <- ifelse(refine_original$product_category == "TV", 1, 0)
refine_original$product_laptop <- ifelse(refine_original$product_category == "Laptop", 1, 0)
refine_original$product_tablet <- ifelse(refine_original$product_category == "Tablet", 1, 0)

refine_original

