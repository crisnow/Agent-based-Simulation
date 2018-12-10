
##----set the file dir


###--- we will set transmission rate, p = 0.1, 0.25, 0.4

transmission_rate = 0.5

coins = c(1, 0) 
probabilities = c(transmission_rate, 1-transmission_rate )         
sample(coins, 1, rep=TRUE, prob=probabilities) # Generate a sequence
?sample

x <- 1:3
y <- c(50, 104, 0.3)
# a random permutation
sample(x, 3, prob = y)

# toss the coins unfairly

# why we need this function? so confused, why not use random?
toss = function(freq) {
  tossing = NULL
  for (i in 1:freq )
  tossing[i] = sample(coins, 1, rep=TRUE, prob=probabilities)
  tossing = sum(tossing)
  return (tossing)
}

toss(10000000)

#### to figure out how this code works!!!!!


seed_num = 1
set.seed(0); 
diffusers = sample(V(g),seed_num) # pick the first diffuer:897

update_diffusers = function(diffusers){
  # find the first level neighbo of the diffuers node
  neighbors<- ego(g, 1, diffusers)
  # cannot coerce class ""igraph.vs"" to a data.frame
  nearest_neighbors = data.frame(table(unlist(neighbors)))
  # exclude itself
  nearest_neighbors = subset(nearest_neighbors, !(nearest_neighbors[,1]%in%diffusers))
  # find out the adoption rate under the probability
  utility = unlist(lapply(nearest_neighbors[,2], toss))
  
  # the utility function that whether the utility is greater than 1
  # if utility is greater than 1, it is active, if not, inactive.
  new_infected =as.numeric(as.character(nearest_neighbors[,1][utility >= 1]))
  class(new_infected) <- "igraph.vs"
  diffusers = unique(c(diffusers, new_infected))
  return(diffusers)
}


###----------------------------------------------
#  Experiment 2: node_number = 100, 1000, 2000
#------------------------------------------------


library(igraph)
node_number = 100
node_number

#--------- random network -----------
diffusers = sample(V(g.random),seed_num)
g.random = sample_gnm(node_number, node_number)  # set the node number is equal to the edge number
graph_name = "Random network"
plot(g.random, main = graph_name)

set.seed(0); 
layout.old = layout_with_fr(g.random, niter = 1000)
layout.old = layout_on_grid(g.random)
?layout_with_fr
# par(mfrow=c(1,1))   # set the window of plot
V(g.random)$color[V(g.random)%in%diffusers] = "red"
plot(g.random, layout =layout.old, vertex.label=NA)

g<-g.random

#--------- Scale-free network -----------

g.pa = sample_pa(node_number) 
graph_name = "Scale-free network"
plot(g.pa, main = graph_name)

diffusers = sample(V(g.pa),seed_num)


layout.old = layout_with_fr(g.pa, niter = 1000)
layout.old = layout_on_grid(g.pa)
?layout_with_fr
V(g.pa)$color[V(g.pa)%in%diffusers] = "red"
plot(g.pa, layout =layout.old, vertex.label=NA)


#--------- Small World Network -----------
?sample_smallworld
g.sw = sample_smallworld(2, 10, 1, 0.005)
graph_name = "Small World network"
?plot
plot(g.sw, main = graph_name)

set.seed(0); 
layout.old = layout_with_fr(g.random, niter = 1000)
layout.old = layout_on_grid(g.random)
?layout_with_fr
V(g.sw)$color[V(g.sw)%in%diffusers] = "red"
plot(g.sw, layout =layout.old, vertex.label=NA)

#g<-experiment.pa
### here I only use the betweenness, but should use more

#--------------------------------------------------
# use centerality measure to select the seeds
#---------------------------------------------------


between <- betweenness(g, normalized = T) 
close <- closeness(g, normalized = T)
page <- page_rank(g, directed=F)$vector

topnv <- function(graph, values, n=1) {
  return(V(graph)[sort.list(values, decreasing=TRUE)[1:n]])
}



# the top 10 early seeds.
diffusers<-topnv(g, between, n=1)
diffusers<-topnv(g, close, n=10)
diffusers<-topnv(g, page, n=1)

seed_num = 1
set.seed(0); 
#diffusers = sample(V(g),seed_num) # the naive method
infected =list()
infected[[1]]= diffusers
# set the color
E(g)$color = "grey"
V(g)$color = "white"

set.seed(0); 
#layout.old = layout.fruchterman.reingold(g, niter = 1000)
layout.old = layout_on_grid(g.random)
V(g)$color[V(g)%in%diffusers] = "red"
plot(g, layout =layout.old, vertex.label=NA)

count = 1

# simulate 1000 times and get the average step.

#for (count in 1:1000) {

total_time = 1
while(length(infected[[total_time]]) < node_number){ 
  infected[[total_time+1]] = sort(update_diffusers(infected[[total_time]]))
  cat(length(infected[[total_time+1]]), "-->")
  total_time = total_time + 1
  if (length(infected[[total_time]]) == length(infected[[total_time-1]])){break} 
  }
#}

plot_time_series = function(infected, m){
  num_cum = unlist(lapply(1:m, 
                          function(x) length(infected[[x]]) ))
  p_cum = num_cum/node_number
  p = diff(c(0, p_cum))
  time = 1:m
  plot(p_cum~time, type = "b", 
       ylab = "CDF", xlab = "Time",
       xlim = c(0,total_time), ylim =c(0,1))
  plot(p~time, type = "h", frame.plot = FALSE,
       ylab = "PDF", xlab = "Time",
       xlim = c(0,total_time), ylim =c(0,1))
}
par(mfrow=c(1,2)) 
plot_time_series(infected, 10)

g<-g.random
source("new_window.R")



plot_gif = function(infected){
  m = 2
  while(m <= length(infected)){
    layout(matrix(c(1, 2, 1, 3), 2,2, byrow = TRUE), widths=c(3,1), heights=c(1, 1))
    V(g)$color = "white"
    V(g)$color[V(g)%in%infected[[m]]] = "red"
    plot(g, layout =layout.old, edge.arrow.size=0.2)
    title(paste(graph_name, "\n Transmission Rate =", transmission_rate, ", Day", m))
    plot_time_series(infected, m)
    dev.copy(jpeg,'myplot%03d.jpeg')
    dev.off()
    m = m + 1
    }
}

plot_gif(infected)

