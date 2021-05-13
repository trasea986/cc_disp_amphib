#r script for building dispersal kernals for MIGCLIM
library(tidyverse)

#The following section is from Jesse's class
NegEx <- function(x, a =1, b = 1) {
  stopifnot(x>=0, a>=0, b>=0)
  y <- a*exp(-b*x)
  return(y)
}

NegEx_b <- function(x, a = 1){
  y <- exp(-a * x)
  return(y)
}
p + stat_function(fun=NegEx_b, args=list(a=1), aes(color="a=1, b=1")) +
  stat_function(fun=NegEx_b, args=list(a=2), aes(color="a=1, b=3")) +
  stat_function(fun=NegEx_b, args=list(a=3), aes(color="a=1, b=6"))+
                  labs(title = "Negative Exponential") + 
                  #scale_y_log10() +
                  theme_bw() +
                  scale_color_discrete("Parameters")

#this uses the default value for a and b, alternatively:
p + stat_function(fun=NegEx, args=list(b=3))


#if we want to plot multiple line estimates, you add multiple stat_functions, also adding color coding. scale_color_discrete adds legend

#can also log transfaorm using scale_y_log10()

p + stat_function(fun=NegEx, args=list(a=1, b=1), aes(color="a=1, b=1")) +
  stat_function(fun=NegEx, args=list(a=1, b=(2)), aes(color="a=1, b=3")) +
  stat_function(fun=NegEx, args=list(a=1, b=(3)), aes(color="a=1, b=6")) +
  stat_function(fun=NegEx, args=list(a=1, b=(.5)), aes(color="a=1/2, b=9")) +
  #stat_function(fun=NegEx, args=list(a=1, b=(1/12)), aes(color="a=1/2, b=12")) +
  #stat_function(fun=NegEx, args=list(a=1, b=(1/22)), aes(color="a=1/2, b=22")) +
  labs(title = "Negative Exponential") + 
  #scale_y_log10() +
  theme_bw() +
  scale_color_discrete("Parameters")


p + stat_function(fun=NegEx, args=list(a=1, b=1), aes(color="a=1, b=1")) +
  stat_function(fun=NegEx, args=list(a=1/2, b=1), aes(color="a=1/2, b=1")) +
  stat_function(fun=NegEx, args=list(a=1/4, b=1), aes(color="a=1/4, b=1")) +
  stat_function(fun=NegEx, args=list(a=2, b=1), aes(color="a=2, b=1")) +
  stat_function(fun=NegEx, args=list(a=4, b=1), aes(color="a=4, b=1")) +
  stat_function(fun=NegEx, args=list(a=6, b=1), aes(color="a=6, b=1")) +
  labs(title = "Negative Exponential") + 
  #scale_y_log10() +
  theme_bw() +
  scale_color_discrete("Parameters")


#adding lots of stat_functions is workable, but tedious. Alternative is to just create a data frame with columns of x and the a and b parameters, and then calculate y.

#create a data frame with all combinations
df <- expand.grid(x=seq(0,10, length=101),
                  a=c(0.5,1),
                  b=c(0.5,1,2))
#add prediction column
df$y <- with(df, NegEx(x=x, a=a, b=b))
head(df)

#add column with levels of the parameters
df$parameters <- with(df, paste("a=", a, ", b=", b, sep=""))

#ggplot(data=df, aes(x=x, y=y, color=parameters)) +
# geom_line() +
#labs(title="Negative Exponential") +
#theme_bw()

#or alternatively
ggplot(data=df, aes(x=x, y=y, linetype=factor(a), color=factor(b), group=parameters)) +
  geom_line() +
  scale_linetype("a = ") +
  scale_color_brewer("b = ",palette="Set1") +
  labs(title = "Negative Exponenetial") +
  theme_bw()


#Now to incorporate some amphibian numbers here
#Here, X is the distance, and Y is the proportion of individuals

#create a data frame with all combinations
df <- expand.grid(x=seq(0,10, length=11),
                  a=c(1),
                  b=c(0.75,1,1.5,2,2.5))
#add prediction column
df$y <- with(df, NegEx(x=x, a=a, b=b))
head(df)

#add column with levels of the parameters
df$parameters <- with(df, paste("a=", a, ", b=", b, sep=""))



#or alternatively
ggplot(data=df, aes(x=x, y=y, color=factor(a), linetype=factor(b), group=parameters)) +
  geom_line() +
  scale_linetype("b = ") +
  scale_color_brewer("a = ",palette="Set1") +
  labs(title = "Negative Exponenetial") +
  theme_classic()

#this looks like a good set, but we need to flip the proportions around for the dispersal kernal, so going to create that and then export to CSV

df$inverse <- 1 - df$y
head(df)

write.csv(df, "Curve_Param.csv")


######now to work through distance, in a similar fashion
df_d <- expand.grid(x=seq(0,10, length=11),
                  a=c(1),
                  b=c(0.75,1,1.5,2,2.5))
#add prediction column
df_d$y <- with(df_d, NegEx(x=x, a=a, b=b))
head(df)

#add column with levels of the parameters
df_d$parameters <- with(df_d, paste("a=", a, ", b=", b, sep=""))



#or alternatively
ggplot(data=df_d, aes(x=x, y=y, color=factor(a), linetype=factor(b), group=parameters)) +
  geom_line() +
  scale_linetype("b = ") +
  scale_color_brewer("a = ",palette="Set1") +
  labs(title = "Negative Exponenetial") +
  theme_classic()

#this looks like a good set, but we need to flip the proportions around for the dispersal kernal, so going to create that and then export to CSV

df_d$inverse <- 1 - df_d$y
head(df_d)

write.csv(df_d, "Distance_Param.csv")