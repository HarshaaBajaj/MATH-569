library(readr)
Grocery <- read_delim("E:/SUBJECTS/569 MATH SL S17--/hw2/Grocery.txt", "\t",
                      escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
View(Grocery)
names(Grocery)<-c("total_labor_hours","number_of_cases_shipped","indirect_costs_of_the_total_labor_hours_as_percentage","holiday")
#linear model
fit<-lm(total_labor_hours~.,data=Grocery)
summary(fit)
#best subset
library(leaps)
subset<-leaps(x=as.matrix(Grocery[,2:4]),y=as.matrix(Grocery[,1]))
plot(x=subset$size,y=subset$Cp,xlab='size',ylab='Cp')
subset$which[which(subset$Cp==(min(subset$Cp))),]
fit_bestsub<-lm(total_labor_hours~number_of_cases_shipped+holiday,data = Grocery)
summary(fit_bestsub)
#forward selection
fit0<-lm(total_labor_hours~1,data=Grocery)
fit.forward<-step(fit0,scope=list(lower=total_labor_hours~1, upper=total_labor_hours~holiday
+number_of_cases_shipped+indirect_costs_of_the_total_labor_hours_as_percentage),direction='forward')
summary(fit.forward)
#backward selection
fit.backward<-step(fit,scope=list(lower=total_labor_hours~1, upper=total_labor_hours~holiday
+number_of_cases_shipped+indirect_costs_of_the_total_labor_hours_as_percentage),direction='backward')
summary(fit.backward)
#f test with null hypo that models are same
anova(fit,fit_bestsub)
