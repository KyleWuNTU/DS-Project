session_times <- read.csv("/Users/kylewu/Downloads/practical-statistics-for-data-scientists/data/web_page_data.csv")
lung <- read.csv("/Users/kylewu/Downloads/practical-statistics-for-data-scientists/data/LungDisease.csv")


#Simple Linear Regression
model <- lm(PEFR ~Exposure, data = lung)
print(model)

print(fitted <- predict(model))
print(resid <- residuals(model))

#Multi-Regression
house <- read.csv("/Users/kylewu/Downloads/practical-statistics-for-data-scientists/data/house_sales.csv", header = TRUE, sep='\t')
house_lm <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade,  
               data=house, na.action=na.omit)

print(summary(house_lm))

### Model Selection and Stepwise Regression
house_full <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                   Bedrooms + BldgGrade + PropertyType + NbrLivingUnits + 
                   SqFtFinBasement + YrBuilt + YrRenovated + NewConstruction,
                 data=house, na.action=na.omit)

### Weighted regression

house$Year = year(house$DocumentDate)
house$Weight = house$Year - 2005

house_wt <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade,
               data=house, weight=Weight, na.action=na.omit)
round(cbind(house_lm=house_lm$coefficients, 
            house_wt=house_wt$coefficients), digits=3)

#Dummy Variables
prop_type_dummies <- model.matrix(~PropertyType -1, data=house)
print(head(prop_type_dummies))

print(table(house$ZipCode))
zip_groups <- house %>%
  mutate(resid = residuals(house_lm)) %>%
  group_by(ZipCode) %>%
  summarize(med_resid = median(resid),
            cnt = n()) %>%
  # sort the zip codes by the median residual
  arrange(med_resid) %>%
  mutate(cum_cnt = cumsum(cnt),
         ZipGroup = factor(ntile(cum_cnt, 5)))
house <- house %>%
  left_join(select(zip_groups, ZipCode, ZipGroup), by='ZipCode')
print(table(zip_groups[c('ZipGroup')]))

house_98105 <- house[house$ZipCode == 98105,]
lm_98105 <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade, data=house_98105)

summary(lm_98105)

#Ploynomial Regression
lm_poly <- lm(AdjSalePrice ~  poly(SqFtTotLiving, 2) + SqFtLot + 
                BldgGrade +  Bathrooms +  Bedrooms,
              data=house_98105)
terms <- predict(lm_poly, type='terms')
partial_resid <- resid(lm_poly) + terms
print(lm_poly)


#Splines
knots <- quantile(house_98105$SqFtTotLiving, p=c(.25, .5, .75))
lm_spline <- lm(AdjSalePrice ~ bs(SqFtTotLiving, knots=knots, degree=3) +  SqFtLot +  
                  Bathrooms + Bedrooms + BldgGrade,  data=house_98105)
print(lm_spline)