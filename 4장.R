library(tidyverse)

lung = read_csv(r"(/Volumes/Samsung_T5/S4DS/practical-statistics-for-data-scientists/data/LungDisease.csv)")

model = lm(PEFR ~ Exposure, data = lung)
model

lung %>% 
  ggplot() +
  geom_point(aes(x = Exposure, y = PEFR))

fitted = predict(model)
resid = residuals(model)

house = read.delim(r"(/Volumes/Samsung_T5/S4DS/practical-statistics-for-data-scientists/data/house_sales.csv)")

library(tidymodels)

lm_model = linear_reg() %>% 
  set_mode("regression") %>%
  set_engine("lm")

recipe(data = house,
       formula = AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + BldgGrade, na.action = na.omit)

library(MASS)

house_full = lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + BldgGrade +
                  PropertyType + NbrLivingUnits + SqFtFinBasement + YrBuilt + YrRenovated + NewConstruction,
                data = house, na.action = na.omit)
step = stepAIC(house_full, direction = "both")
step

