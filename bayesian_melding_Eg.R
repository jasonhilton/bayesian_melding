###########

# bayesian melding example.
# using the simple example in Poole and Raftery 2000 JASA

# I have included dummy functions for likelihoods and for priors even when
# these are constant over the inputs for the example
# to ensure the logic of the method is clear
# and to enable easy extension to real examples.




get_z<-function(x,y){
  "stand in for the model"
  return(y/x)
}

n_samples<- 100000

xs_prior<-runif(n_samples, 2, 4)
ys_prior<-runif(n_samples, 6, 9)

thetas<- data.frame(xs_prior,ys_prior)

zs_prior<-runif(n_samples, 0, 5)

get_zs_prior<-function(z){
  return (1/5)
}


plot(density(xs_prior))
plot(density(ys_prior))


zs<-get_z(xs_prior, ys_prior)

zs_induced<-density(zs, n= n_samples)


zs_sampled_density<-sapply(zs,function(z) {
  zs_induced$y[which(zs_induced$x>z)[1]]})



like1<-function(theta){
  "dummy function"
  return(1)
}

like2<-function(z){
  "dummy function"
  return(1)
}

weights<-mapply(function(zs, thetas, zs_sampled_density,alpha) { 
     ( (zs_sampled_density/ get_zs_prior(zs))**(1-alpha) ) * 
        like1(thetas) * like2(zs)
     }, 
     zs, thetas, zs_sampled_density, MoreArgs=list(alpha=0.5))



l_samples<-100000


posterior_indices<-sample.int(length(xs_prior), l_samples,prob=1/weights,replace=T)

posterior_theta<-thetas[posterior_indices,]

hist(posterior_theta[,1],freq=F)

hist(posterior_theta[,2],freq=F)

posterior_zs<- zs[posterior_indices]









