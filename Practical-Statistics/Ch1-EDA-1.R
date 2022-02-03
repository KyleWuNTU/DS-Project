#multiple variable
kc_tax <- read.csv("/Users/kylewu/Downloads/practical-statistics-for-data-scientists/data/kc_tax.csv.gz")
kc_tax0 <- subset(kc_tax, TaxAssessedValue < 750000 & SqFtTotLiving >100 & SqFtTotLiving <3500)
print(nrow(kc_tax0))

#hex plot
print(ggplot(kc_tax0, (aes(x = SqFtTotLiving, y = TaxAssessedValue)))+stat_binhex(color = "white")+theme_bw()+scale_fill_gradient(low = "white", high ="black")+ labs(x= "Finished Square Feet", y = "Tax-Assessed Value"))

#Contour Plot
print(ggplot(kc_tax0, (aes(x = SqFtTotLiving, y = TaxAssessedValue)))+theme_bw()+geom_point(alpha = 0.1)+geom_density2d(color = "white")+labs(x= "Finished Square Feet", y = "Tax-Assessed Value"))

#pivot table
lc_loan <- read.csv("/Users/kylewu/Downloads/practical-statistics-for-data-scientists/data/lc_loans.csv")
print(x_tab <- CrossTable(lc_loan$grade, lc_loan$status, prop.c = FALSE, prop.chisq = FALSE, prop.t = FALSE))

#分組
graph <- ggplot(subset(kc_tax0, ZipCode %in% c(98188, 98105, 98108, 98126)),
                aes(x=SqFtTotLiving, y=TaxAssessedValue)) + 
  stat_binhex(colour='white') + 
  theme_bw() + 
  scale_fill_gradient(low='gray95', high='black') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(x='Finished Square Feet', y='Tax-Assessed Value') +
  facet_wrap('ZipCode')
  
print(graph)