library(fuzzr)
library(sets)

## Podstawowe implementacje ###################################################
sets_options("universe", seq(from = 0, to = 1, by = 0.001)) 

fuzzy_variable_value = function(x_, fvar) {
  atrib <- attributes(fvar)
  idx <- which.min(abs(unlist(fvar) - x_))
  return( round(atrib$memberships[idx], 3) )
}

GetMembership = function(x, fvar) {
  hidden_ = function(x, fvar) { # Zwraca listê=
    return ( sapply( as.double(x), function(val) { return (fuzzy_variable_value(val, fvar)) }) )
  }
  return ( unlist(sapply( hidden_(x, fvar), FUN=function(x) ifelse(!is.numeric(x), 0, x))) ) # Lista na Array z zamian¹ NULL na 0
}

AccuracyRateMatrix = function(in_F, fvar) {
  MTX_ <- matrix(rep(0, length(in_F)*5), nrow = length(in_F), ncol = 5)
  for (i in seq(1, length(in_F))) {
    MTX_[i, 1] <- GetMembership(in_F[i], fvar$One)
    MTX_[i, 2] <- GetMembership(in_F[i], fvar$Two)
    MTX_[i, 3] <- GetMembership(in_F[i], fvar$Three)
    MTX_[i, 4] <- GetMembership(in_F[i], fvar$Four)
    MTX_[i, 5] <- GetMembership(in_F[i], fvar$Five)
  }
  return (MTX_)
}
# 
# BaseVariable <- fuzzy_variable(One   = fuzzy_trapezoid(corners=c(-0.01, 0.0, 0.1, 0.3)),
#                                Two     = fuzzy_triangular(corners=c(0.1, 0.3, 0.5)),
#                                Three     = fuzzy_triangular(corners=c(0.3, 0.5, 0.7)),
#                                Four     = fuzzy_triangular(corners=c(0.5, 0.7, 0.9)),
#                                Five    = fuzzy_trapezoid(corners=c(0.7, 0.9, 1, 1.01)))

BaseVariable <- fuzzy_variable(One   = fuzzy_two_normals(mean = c(0.0, 0.1), sd = c(0.1, 0.1)),
                               Two     = fuzzy_normal(mean = 0.3, sd = 0.1),
                               Three     = fuzzy_normal(mean = 0.5, sd = 0.1),
                               Four     = fuzzy_normal(mean = 0.7, sd = 0.1),
                               Five    = fuzzy_two_normals(mean = c(0.9, 1), sd = c(0.1, 0.1)))

plot(BaseVariable)

## Ibrahim A. Hameed - Using Gaussian membership functions for improving the reliability
## and robustness of students’ evaluation systems - IMPLEMENTACJA
A <- matrix( # Wyniki 10 studentów na 5 pytañ
  c(0.59, 0.35, 1, 0.66, 0.11, 0.08, 0.84, 0.23, 0.04, 0.24,
    0.01, 0.27, 0.14, 0.04, 0.88, 0.16, 0.04, 0.22, 0.81, 0.53,
    0.77, 0.69, 0.97, 0.71, 0.17, 0.86, 0.87, 0.42, 0.91, 0.74,
    0.73, 0.72, 0.18, 0.16, 0.5, 0.02, 0.32, 0.92, 0.9, 0.25,
    0.93, 0.49, 0.08, 0.81, 0.65, 0.93, 0.39, 0.51, 0.97, 0.61),
  nrow=5,
  ncol=10,
  byrow = TRUE
)

T <- matrix( # Czas odpowiedzi 10 studentów na 5 pytañ
  c(0.7, 0.4, 0.1, 1, 0.7, 0.2, 0.7, 0.6, 0.4, 0.9,
      1, 0, 0.9, 0.3, 1, 0.3, 0.2, 0.8, 0, 0.3,
      0, 0.1, 0, 0.1, 0.9, 1, 0.2, 0.3, 0.1, 0.4,
      0.2, 0.1, 0, 1, 1, 0.3, 0.4, 0.8, 0.7, 0.5,
      0, 0.1, 1, 1, 0.6, 1, 0.8, 0.2, 0.8, 0.2),
  nrow=5,
  ncol=10,
  byrow = TRUE
)

G <- matrix( # Wagi 5 pytañ
  c(10, 15, 20, 25, 30),
  nrow=5,
  ncol=1,
  byrow = TRUE
)

S <- t(A)%*%G # Classical  Total score for each individual student

P_ <- c(0.9, 0.434, 0.87, 0.1, 0.486) # degree of importance of each question in the fuzzy domain
P <- AccuracyRateMatrix(P_, BaseVariable)

# complexity matrix, which is an important factor indicating the ability of students to give correct answers of complex questions
Co_ <- c(0.33, 0.634, 0.762, 0.188, 0.56)
Co <- AccuracyRateMatrix(Co_, BaseVariable)

A_ <- round(rowMeans (A), 3)
T_ <- round(rowMeans (T), 3)


FA <- AccuracyRateMatrix(A_, BaseVariable)
FT <- AccuracyRateMatrix(T_, BaseVariable)

Accuracy <- BaseVariable
Timerate <- BaseVariable
Difficulty <- BaseVariable

# plot(Accuracy) ## 3str. w artukule ##################

# Difficulty node
variables1st <- set(
  Accuracy,
  Timerate,
  Difficulty
)

# Fuzzy rules
rules1st <- set(
  fuzzy_rule(Accuracy %is% One && Timerate %is% One,   Difficulty %is% Three),
  fuzzy_rule(Accuracy %is% One && Timerate %is% Two,   Difficulty %is% Four),
  fuzzy_rule(Accuracy %is% One && Timerate %is% Three, Difficulty %is% Four),
  fuzzy_rule(Accuracy %is% One && Timerate %is% Four,  Difficulty %is% Five),
  fuzzy_rule(Accuracy %is% One && Timerate %is% Five,  Difficulty %is% Five),
  
  fuzzy_rule(Accuracy %is% Two && Timerate %is% One,   Difficulty %is% Two),
  fuzzy_rule(Accuracy %is% Two && Timerate %is% Two,   Difficulty %is% Three),
  fuzzy_rule(Accuracy %is% Two && Timerate %is% Three, Difficulty %is% Four),
  fuzzy_rule(Accuracy %is% Two && Timerate %is% Four,  Difficulty %is% Four),
  fuzzy_rule(Accuracy %is% Two && Timerate %is% Five,  Difficulty %is% Five),
  
  fuzzy_rule(Accuracy %is% Three && Timerate %is% One,   Difficulty %is% Two),
  fuzzy_rule(Accuracy %is% Three && Timerate %is% Two,   Difficulty %is% Two),
  fuzzy_rule(Accuracy %is% Three && Timerate %is% Three, Difficulty %is% Three),
  fuzzy_rule(Accuracy %is% Three && Timerate %is% Four,  Difficulty %is% Four),
  fuzzy_rule(Accuracy %is% Three && Timerate %is% Five,  Difficulty %is% Four),
  
  fuzzy_rule(Accuracy %is% Four && Timerate %is% One,   Difficulty %is% One),
  fuzzy_rule(Accuracy %is% Four && Timerate %is% Two,   Difficulty %is% Two),
  fuzzy_rule(Accuracy %is% Four && Timerate %is% Three, Difficulty %is% Two),
  fuzzy_rule(Accuracy %is% Four && Timerate %is% Four,  Difficulty %is% Three),
  fuzzy_rule(Accuracy %is% Four && Timerate %is% Five,  Difficulty %is% Four),
  
  fuzzy_rule(Accuracy %is% Five && Timerate %is% One,   Difficulty %is% One),
  fuzzy_rule(Accuracy %is% Five && Timerate %is% Two,   Difficulty %is% One),
  fuzzy_rule(Accuracy %is% Five && Timerate %is% Three, Difficulty %is% Two),
  fuzzy_rule(Accuracy %is% Five && Timerate %is% Four,  Difficulty %is% Two),
  fuzzy_rule(Accuracy %is% Five && Timerate %is% Five,  Difficulty %is% Three)
)

model1st <- fuzzy_system(variables1st, rules1st)
print(model1st)
plot(model1st)

tmpObj1 <- lapply(1:length(A_), FUN = function(i) { return ( fuzzy_inference(model1st, list(Accuracy = A_[i], Timerate = T_[i])) ) })
plot(tmpObj1[[1]]) ## 5 str. w artykule ###################

D_ <- sapply(tmpObj1, function(o) {return ( gset_defuzzify(o, "centroid") )})
FD <- AccuracyRateMatrix(D_, BaseVariable)

# Cost node
Complexity <- BaseVariable
Cost <- BaseVariable

variables2st <- set(
  Difficulty,
  Complexity,
  Cost
)

rules2st <- set(
  fuzzy_rule(Difficulty %is% One && Complexity %is% One,   Cost %is% One),
  fuzzy_rule(Difficulty %is% One && Complexity %is% Two,   Cost %is% One),
  fuzzy_rule(Difficulty %is% One && Complexity %is% Three, Cost %is% Two),
  fuzzy_rule(Difficulty %is% One && Complexity %is% Four,  Cost %is% Two),
  fuzzy_rule(Difficulty %is% One && Complexity %is% Five,  Cost %is% Three),
  
  fuzzy_rule(Difficulty %is% Two && Complexity %is% One,   Cost %is% One),
  fuzzy_rule(Difficulty %is% Two && Complexity %is% Two,   Cost %is% Two),
  fuzzy_rule(Difficulty %is% Two && Complexity %is% Three, Cost %is% Two),
  fuzzy_rule(Difficulty %is% Two && Complexity %is% Four,  Cost %is% Three),
  fuzzy_rule(Difficulty %is% Two && Complexity %is% Five,  Cost %is% Four),
  
  fuzzy_rule(Difficulty %is% Three && Complexity %is% One,   Cost %is% Two),
  fuzzy_rule(Difficulty %is% Three && Complexity %is% Two,   Cost %is% Two),
  fuzzy_rule(Difficulty %is% Three && Complexity %is% Three, Cost %is% Three),
  fuzzy_rule(Difficulty %is% Three && Complexity %is% Four,  Cost %is% Four),
  fuzzy_rule(Difficulty %is% Three && Complexity %is% Five,  Cost %is% Four),
  
  fuzzy_rule(Difficulty %is% Four && Complexity %is% One,   Cost %is% Two),
  fuzzy_rule(Difficulty %is% Four && Complexity %is% Two,   Cost %is% Three),
  fuzzy_rule(Difficulty %is% Four && Complexity %is% Three, Cost %is% Four),
  fuzzy_rule(Difficulty %is% Four && Complexity %is% Four,  Cost %is% Four),
  fuzzy_rule(Difficulty %is% Four && Complexity %is% Five,  Cost %is% Five),
  
  fuzzy_rule(Difficulty %is% Five && Complexity %is% One,   Cost %is% Three),
  fuzzy_rule(Difficulty %is% Five && Complexity %is% Two,   Cost %is% Four),
  fuzzy_rule(Difficulty %is% Five && Complexity %is% Three, Cost %is% Four),
  fuzzy_rule(Difficulty %is% Five && Complexity %is% Four,  Cost %is% Five),
  fuzzy_rule(Difficulty %is% Five && Complexity %is% Five,  Cost %is% Five)
)

model2st <- fuzzy_system(variables2st, rules2st)
# print(model2st)
# plot(model2st)

tmpObj2 <- lapply(1:length(D_), FUN = function(i) { return ( fuzzy_inference(model2st, list(Difficulty = round(D_[i],3), Complexity = Co_[i])) ) })
plot(tmpObj2[[1]]) ## 5 str. w artykule ###################

C_ <- sapply(tmpObj2, function(o) {return ( gset_defuzzify(o, "centroid") )})
FC <- AccuracyRateMatrix(C_, BaseVariable)


# Adjustment node
Importance <- BaseVariable
Adjustment <- BaseVariable

variables3st <- set(
  Cost,
  Importance,
  Adjustment
)

rules3st <- set(
  fuzzy_rule(Cost %is% One && Importance %is% One,   Adjustment %is% One),
  fuzzy_rule(Cost %is% One && Importance %is% Two,   Adjustment %is% One),
  fuzzy_rule(Cost %is% One && Importance %is% Three, Adjustment %is% Two),
  fuzzy_rule(Cost %is% One && Importance %is% Four,  Adjustment %is% Two),
  fuzzy_rule(Cost %is% One && Importance %is% Five,  Adjustment %is% Three),
  
  fuzzy_rule(Cost %is% Two && Importance %is% One,   Adjustment %is% One),
  fuzzy_rule(Cost %is% Two && Importance %is% Two,   Adjustment %is% Two),
  fuzzy_rule(Cost %is% Two && Importance %is% Three, Adjustment %is% Two),
  fuzzy_rule(Cost %is% Two && Importance %is% Four,  Adjustment %is% Three),
  fuzzy_rule(Cost %is% Two && Importance %is% Five,  Adjustment %is% Four),
  
  fuzzy_rule(Cost %is% Three && Importance %is% One,   Adjustment %is% Two),
  fuzzy_rule(Cost %is% Three && Importance %is% Two,   Adjustment %is% Two),
  fuzzy_rule(Cost %is% Three && Importance %is% Three, Adjustment %is% Three),
  fuzzy_rule(Cost %is% Three && Importance %is% Four,  Adjustment %is% Four),
  fuzzy_rule(Cost %is% Three && Importance %is% Five,  Adjustment %is% Four),
  
  fuzzy_rule(Cost %is% Four && Importance %is% One,   Adjustment %is% Two),
  fuzzy_rule(Cost %is% Four && Importance %is% Two,   Adjustment %is% Three),
  fuzzy_rule(Cost %is% Four && Importance %is% Three, Adjustment %is% Four),
  fuzzy_rule(Cost %is% Four && Importance %is% Four,  Adjustment %is% Four),
  fuzzy_rule(Cost %is% Four && Importance %is% Five,  Adjustment %is% Five),
  
  fuzzy_rule(Cost %is% Five && Importance %is% One,   Adjustment %is% Three),
  fuzzy_rule(Cost %is% Five && Importance %is% Two,   Adjustment %is% Four),
  fuzzy_rule(Cost %is% Five && Importance %is% Three, Adjustment %is% Four),
  fuzzy_rule(Cost %is% Five && Importance %is% Four,  Adjustment %is% Five),
  fuzzy_rule(Cost %is% Five && Importance %is% Five,  Adjustment %is% Five)
)

model3st <- fuzzy_system(variables3st, rules3st)
# print(model2st)
# plot(model2st)

tmpObj3 <- lapply(1:length(D_), FUN = function(i) { return ( fuzzy_inference(model3st, list(Cost = round(C_[i],3), Importance = P_[i])) ) })
# plot(tmpObj2[[1]]) ## 5 str. w artykule ###################

W_ <- sapply(tmpObj3, function(o) {return ( gset_defuzzify(o, "centroid") )})
FW <- AccuracyRateMatrix(W_, BaseVariable)


G_ <- G*(1+W_)
G_ <- G_*sum(G)/sum(G_)

S_ <- round(t(A)%*%G_, digits = 2) # New Total score for each individual student

print("Zdobyte punkty:")
print(A)
print("--------------------------------------------------------------")
print("Czas odpowiedzi:")
print(T)
print("--------------------------------------------------------------")
print("Wagi:")
print(t(G))
print("--------------------------------------------------------------")
print("Importance:")
print(P)
print("--------------------------------------------------------------")
print("Complexity:")
print(Co)
print("--------------------------------------------------------------")
print("Œrednia punktów:")
print(A_)
print("--------------------------------------------------------------")
print("Œrednia czasów:")
print(T_)
print("--------------------------------------------------------------")
print("fuzzy accuracy rate matrix:")
print(FA)
print("--------------------------------------------------------------")
print("fuzzy time rate matrix:")
print(FT)
print("--------------------------------------------------------------")
print("crisp value of the difficulty:")
print(D_)
print("--------------------------------------------------------------")
print("fuzzy difficulty matrix:")
print(FD)
print("--------------------------------------------------------------")
print("crisp values of cost:")
print(C_)
print("--------------------------------------------------------------")
print("fuzzy cost matrix:")
print(FC)
print("--------------------------------------------------------------")
print("crisp values of adjustment:")
print(W_)
print("--------------------------------------------------------------")
print("fuzzified adjustment matrix:")
print(FW)
print("--------------------------------------------------------------")
print("adjusted grades:")
print(t(G_))
print("--------------------------------------------------------------")
print("Total score:")
print(t(S))
print("--------------------------------------------------------------")
print("new total score:")
print(t(S_))
print("--------------------------------------------------------------")













