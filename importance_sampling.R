#### importance sampling example


library(ggplot2)

# from this paper http://iopscience.iop.org/0143-0807/22/4/315/pdf/0143-0807_22_4_315.pdf



## first simulate 100000 draws from the exp distribution with rate 1

# how many random samples to generate
number_samples<-10e5

# 
random_numbers<-runif(number_samples)

# converts 0-1 variate to the corresponding quantile 
# in the exponetial distribution with rate = 1
# same functionality as qexp; written for transparency
quantile_function<-function(x){
  return(-log(1-x))
}

#
exp_samples<-quantile_function(random_numbers)

#hist(exp_samples, freq=F)
exp_hist<-qplot(exp_samples, geom="histogram")


## Compare this to the theoretical distribution 

xs<-seq(0,15,length.out=10e3)

## this is a simplified version of the (probably much faster) internal pexp
pdf_exp<-function(x){
  return(exp(-x))
} 

p_xs<-pdf_exp(xs)

points(xs, p_xs, type="l")


# consider we are interested in the rare event that our samples exceed threshold 
# T
# probability of this is the integral from p to infinity of the pdf
# or alternatively, the proportion of our random samples that are greater the T
Ts<-seq(1,8,1)

log_proportions<-lapply(Ts, function(T) {log(sum(exp_samples>T)/number_samples)})

plot(Ts, log_proportions)

# because as T increases we have fewer and fewer samples above T, we have larger
# errors in our numerical approximations of the integral.

# instead sample from g(x) (not p(x))  and then reweight these new values 
# according to the sample weight it should have in p

calculate_a<- function(Tee){
  a<- 0.5 * ( 1+Tee + sqrt(1+Tee**2))
  return(a)
}

g_pdf_creator<-function(Tee){
  a<-calculate_a(Tee)
  g_pdf<-function(x){
    p_x<- (1 / a) *  exp(-x/a)
    return(p_x)
  }
  return(g_pdf)
}

Tee<-8
g_pdf<-g_pdf_creator(Tee)


g_quantile_function_creator<-function(a){
  q_quantile_function<-function(x){
    return(-a*log(1-x))
  }
  return(q_quantile_function)
}

g_quantile_function<- g_quantile_function_creator(calculate_a(Tee))

hist(g_quantile_function(random_numbers), freq=F)
us<-seq(0,120,length.out=100)
points(us, g_pdf(us),type="l")


random_numbers<-runif(number_samples)
# sample from g instead
g_samples<- g_quantile_function(random_numbers)
# weight by the ratio of g to p
Iks<- pdf_exp(g_samples)/g_pdf(g_samples)
# include only those areas above the threshold
Iks<-Iks * (g_samples>Tee)

# average to get integral estimatew
sum(Iks)/number_samples
