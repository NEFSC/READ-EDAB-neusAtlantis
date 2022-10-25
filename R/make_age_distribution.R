#Function to generate an age distrubtion

# peak.age = 7
# steepness = 10
make_age_distribution = function(peak.age,steepness){
  
  peak.age2 = peak.age/10
  ages = seq(0.01,0.99,length.out = 10)
  
  alpha = ((2*peak.age2)-1-(peak.age2*steepness))/(peak.age2 - 1)
  
  age.props = dbeta(ages,alpha,steepness)
  age.props = age.props/sum(age.props)
  
  # plot(ages,age.props,type = 'l')
  return(age.props)
}
# 
# plot(0,0,xlim=c(1,10),ylim=c(0,0.5),type = 'n')
# for(i in 1:10){
#   lines(1:10,make_age_distribution(peak.age = 5,steepness = i),col = i)
# }
