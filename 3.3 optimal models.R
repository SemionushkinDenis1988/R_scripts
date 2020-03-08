rm(swiss)
swiss <- data.frame(swiss)

fit_full <- lm(Fertility ~ ., data = swiss)
summary(fit_full)

fit_reduced1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, data = swiss)
summary(fit_reduced1)

anova(fit_full, fit_reduced1)

fit_reduced2 <- lm(Fertility ~ Infant.Mortality + Catholic + Education +Agriculture, data = swiss)
summary(fit_reduced2)

anova(fit_full, fit_reduced2)

optimal_fit <- step(fit_full, direction = "backward")

summary(optimal_fit)

#задача1
df <- attitude
model_full <- lm(rating ~ ., data = attitude) 

model_null <- lm(rating ~ 1, data = attitude)

scope = list(lower = model_null, upper = model_full)

ideal_model <- step(object = model_null, scope = list(lower = model_null, upper = model_full), direction = "forward")

summary(ideal_model)

#задача2

summary(anova(ideal_model, model_null))
anova(ideal_model, model_full)

#задача3

model <- lm(sr ~.^2, data = LifeCycleSavings)
