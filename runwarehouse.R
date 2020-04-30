library(tidyverse)
library(ggthemes)
# Set parameters for warehouse
aisles <- 10
tiers <- 5
slots <- 100
height_cost <- 1.0 #ratio: (cost of going up a tier)/(cost of going out an aisle)
aisle_cost <- 10 #ratio: (cost of going to next aisle vs cost of going to next bin)
mean_demand <- 50 #mean product stock for inital loading
product_range <- 100 #number of distinct products
warehouse_loading <- 0.8 #load factor
gamma_shape <-5 #skewness of demand distribution
capacity <- aisles * tiers * slots
fast_access_limit <- 5 # easy access bins reserved for each product
age_limit <- 2000 # cycle age beyond which stock not permitted to sit

#
# Build warehouse
# Warehouse consists of a table of bins, with associated cost, 
# a product in the bin (or 0), and a cycle number when product was placed.
# Bins are ordered from lowest to highest cost
build_it <- function(){
  wh <- expand.grid(aisle = seq(1,aisles), tier = seq(1,tiers), slot = seq(1,slots))
  wh <- wh %>% mutate(cost = aisle + height_cost*tier - 1 - height_cost + slots/aisle_cost, cycle_in = 0)
  wh <- wh %>% arrange(cost)
  wh <- wh %>% mutate(bin_no = as.integer(rownames(wh)))
  #wh %>% ggplot(aes(x=cost)) + geom_histogram(binwidth = 1)
  return (wh)
}
#Alternative warehouse with mlore complicated geometry
build_it2 <- function (){
  wh <- expand.grid(aisle = seq(1,aisles), tier = seq(1,tiers), slot = seq(1,slots))
  wh <- wh %>% mutate(cost = aisle + height_cost*tier - 1 - height_cost +
      (2/aisle_cost) * ifelse(slot > slots/2,slot-slots/2,0), cycle_in = 0)
  wh <- wh %>% arrange(cost)
  wh <- wh %>% mutate(bin_no = as.integer(rownames(wh)))
  #wh %>% ggplot(aes(x=cost)) + geom_histogram(binwidth = 1)
  return (wh)
}#Alternative warehouse with more complicated geometry and full costing
build_it3 <- function (){
  aisle_cost <- 10
  wh <- expand.grid(aisle = seq(1,aisles), tier = seq(1,tiers), slot = seq(1,slots))
  wh <- wh %>% mutate(cost = aisle + height_cost*tier - 1 - height_cost + slots/(2*aisle_cost) +
                        (2/aisle_cost) * ifelse(slot > slots/2,slot-slots/2,0), cycle_in = 0)
  wh <- wh %>% arrange(cost)
  wh <- wh %>% mutate(bin_no = as.integer(rownames(wh)))
  #wh %>% ggplot(aes(x=cost)) + geom_histogram(binwidth = 1)
  return (wh)
}
#
#Generate products with gamma distribution of demand 
catalogue <- function() {
# create a catalogue of random demand values for full product range 
# with a gamma distribution, sorted from highest demand to lowest
  prod_list <- data.frame(dem = sort(rgamma(product_range,shape=gamma_shape,
                                       scale=mean_demand/gamma_shape), decreasing=T))
  
# prod_list %>% ggplot(aes(x=dem)) + geom_histogram(binwidth = 1)
# normalise the demand and generate a cumulative demand from highest to lowest
  sum_d <- sum(prod_list$dem)
  prod_list <- prod_list %>% mutate(c_dem = cumsum(dem)/sum_d, dem = dem/sum_d)
  return (prod_list)
}

#
# Function to populate warehouse
#
pop_warehouse <- function(quantity_rule, placement, prod_list) {
#
#Quantities of stock set at random levels
  if(quantity_rule == "random") {
    prod_pop <- sample(seq(1,product_range), replace=T, size = capacity*warehouse_loading)
#
#Quantities of stock set probabilistically according to demand levels
  } else {
    prod_pop <- runif(capacity*warehouse_loading, 0, 1.0)
#Map random input from 0-1 onto previosuly generated demand curve
    prod_pop <- sapply(prod_pop, function(x) {
      return (prod_list %>% filter(c_dem <= x) %>% count() %>% pull() + 1)
    })
  }
#
# Product placed randomly
  
  if (placement %in% c("random", "near")){
    prod_pop <- sample(c(prod_pop, rep(0,capacity-length(prod_pop))), size=capacity, replace = F)
#  
# Product placement prioritised for big movers 
  } else if (placement == "prioritise") {
    prod_pop <- c(sort(prod_pop), rep(0,capacity-length(prod_pop)))
# Place for fast access strategy
  } else if (placement == "fast") {
    prod_tab <- table(prod_pop)
    prod_pop <- c()
# Populate fast access bins with up to 'fast_access_limit' units of each product 
    for (i in 1:product_range)  {
        prod_pop <- c(prod_pop, head(c(rep(i, min(fast_access_limit, prod_tab[i])), 
            rep(0,fast_access_limit)), fast_access_limit))
    }
# Populate the rest of the warehouse with remainder of each product
    for (i in 1:product_range) {
      prod_pop <- c(prod_pop, rep(i, max(0, prod_tab[i]-fast_access_limit)))
    }
    prod_pop <- c(prod_pop, rep(0,capacity-length(prod_pop)))
  } else {
    return ("bin_rule not recognised")
  }
  return (prod_pop)
}

despatch <- function(pr, picking, wh, cycle_no) {
  tr_new <- data.frame(cycle=integer(), trans=character(), product=integer(), 
                       bin=integer(), cost=integer(),stringsAsFactors= F)
  r <- wh %>% filter(product == pr)
  if (nrow(r)==0) {
    return (list(tr_new,wh)) 
  }
  if (picking == 'fifo') {
    r <- (r %>% arrange(cycle_in))[1,"bin_no"]
  } else if (picking == 'mod fifo') {
    r <- (rbind(r %>% filter(cycle_in < cycle_no - age_limit), r))[1,"bin_no"]
  } else if (picking == "near") {
    r <- r[1,"bin_no"]
  } else if (picking == "mod fifo baseline") {
    rc <- r %>% filter(cycle_in < cycle_no - age_limit)
    if (nrow(rc) > 0) {
      r <- sample_n(rc,1)[1,"bin_no"]
    } else {
      r <- sample_n(r,1)[1,"bin_no"]
    }
    
  } else {
    r <- sample_n(r,1)[1,"bin_no"]
  }
  wh[r,"product"] <- 0
  tr_new <- rbind(tr_new, list(cycle=cycle_no, trans="Despatch", product=pr, bin=r, cost=wh[r,"cost"]))
  return (list(tr_new,wh))
}

receive <- function(pr, placement, wh, prod_list, prod_arrival,cycle_no) {
  empty_bins <- wh %>% filter(product == 0)
  tr_new <- data.frame(cycle=integer(), trans=character(), product=integer(), 
                                    bin=integer(), cost=integer(),stringsAsFactors= F)
  if (nrow(empty_bins) == 0) {
    return (list(tr_new,wh))
  }
  prod_arrival <- min(prod_arrival, nrow(empty_bins))
  if (placement == 'random') {
    r <- sample_n(empty_bins,prod_arrival) 
  } else if (placement == 'near') {
    r <- empty_bins %>% head(prod_arrival)
  } else if (placement == 'prioritise') {
    m_cost <- prod_list[pr,"min_cost"]
    r <- rbind(empty_bins %>% filter(cost >= m_cost),
                   empty_bins %>% filter(cost < m_cost) %>% arrange(desc(cost))) 
    r <- head(r, prod_arrival)
  } else if (placement ==  "fast") {
    r <- empty_bins %>% filter(bin_no > fast_access_limit*(pr-1) &
                  bin_no <= fast_access_limit * pr )
    r <- rbind(r, empty_bins %>% filter(bin_no > fast_access_limit*product_range))
    r <- head(r,prod_arrival)
  } else {
    return (list(tr_new,wh))
  }
  r <- r$bin_no
  wh[r,"product"] <- rep(pr, prod_arrival)
  cycle_ind <- seq(cycle_no, cycle_no + prod_arrival - 1)
  wh[r,"cycle_in"] <- cycle_ind
  tr_new <- rbind(tr_new, list(cycle=cycle_ind, trans=rep("Receive",prod_arrival), product=rep(pr,prod_arrival), bin=r, cost=wh[r,"cost"]))
  return (list(tr_new,wh))
}

stock_move <- function(placement, picking, cycle_no, wh, prod_list, prod_arrival) {
  pr <- prod_list %>% filter(c_dem <= runif(1,0,1)) %>% count() %>% pull() + 1
  direction <- sample(c(rep("despatch", prod_arrival),"receive"), replace=T, size=1)
  if (direction == "receive") {
    return(receive(pr,placement, wh, prod_list, prod_arrival,cycle_no))
  } else {
    return (despatch(pr,picking, wh, cycle_no))
  }
}


# Build a warehouse, populate it and run 10000 cycles - repeat n times
run_trial <- function(stock_quantity_rule = "match demand",
                      placement = "random",
                      picking = "random",
                      n = 1,
                      warehouse = "basic",
                      prod_arrival = 1,
                      title) {
  prod_list <- catalogue()
  costs <- c()
  for (i in 1:n) {
    if (warehouse == "basic") {
      wh <- build_it()
    } else if (warehouse == "basic") {
      wh <- build_it2()
    } else if (warehouse == "full cost") {
      wh <- build_it3()
    }
    
    # add a minimum cost column to ensure the lowest cost bins are reserved for high demand products
    min_c <- sapply(c(0,prod_list$c_dem[1:product_range-1]), function(cp) {
      wh[cp * capacity + 1,"cost"]
    })
    prod_list <- prod_list %>% mutate(min_cost = min_c)
    wh <- wh %>% mutate(product = pop_warehouse(stock_quantity_rule,placement,prod_list))
    b <- cycle(placement, picking,wh, prod_list, prod_arrival)
    wh <- b[[1]]
    costs <- c(costs, b[[2]])
    tr_hist <- b[[3]]
    cat("cycle ", i, "...cost=", b[[2]],"\n") #Progress monitor
  }
  tr_summary <- tr_hist%>%group_by(cost = floor(cost)) %>% summarise(n=n())
  tr_summary["legend"] <- paste(title, ":", costs[1])
  tr_summary["total_cost"] <- costs[1]
  return (tr_summary)
}

#Run 10000 stock movements
cycle <- function (placement, picking, wh, prod_list, prod_arrival) {
  tr_hist <- data.frame(cycle=integer(), trans=character(), product=integer(), 
                        bin=integer(), cost=integer(),stringsAsFactors= F)
  while (nrow(tr_hist) < 10000) {
    b <- stock_move(placement,picking, nrow(tr_hist)+1, wh, prod_list, prod_arrival)
    tr_hist <- rbind(tr_hist,b[[1]])
    wh <- b[[2]]
  }
  return(list(wh,sum(tr_hist$cost), tr_hist))
}

#Show warehouse bin costs

wh1 <- build_it() %>% mutate(layout = "Original")
wh3 <- build_it3() %>% mutate(layout = "Modified")
colours <- c("Original" = "blue", "Modified" = "bred")
wh_graph1<- wh1 %>% group_by(cost=ceiling(cost)) %>% summarize(count=n()) %>% 
  ggplot(aes(x=cost, y=count)) + 
  geom_bar(stat="identity", fill="blue", color="black", alpha=0.4) + xlab("Bin cost") +
  xlim(0,30) +
  ggtitle("Warehouse geometry: distribution of bin cost")

wh4 <- rbind(wh1, wh3) %>% group_by(bin_cost=ceiling(cost),layout) %>%
  summarise(count=n())  
wh_graph3 <- wh4 %>% ggplot(aes(x=bin_cost,y=count)) +
  
  geom_bar(data=subset(wh4,layout == 'Original'), aes(fill="Original"), color="black", alpha=0.8, stat = "identity") +
  geom_bar(data=subset(wh4,layout == 'Modified'), aes(fill="Modified"), color="black", alpha=0.6, stat = "identity") +
  ggtitle("Bin cost distribution for modified layout")+
  labs(colour = "Modified layout") +
  theme(legend.position = c(0.8, 0.9)) +
  labs(fill = "Layout") +
  xlab("Bin cost")

#Sample product demand distribution
pr_list <- catalogue()
pr_graph <-pr_list %>% group_by(Demand=round(1000*dem)/1000) %>% 
  summarize(Frequency=n()) %>%
  ggplot(aes(x=Demand, y=Frequency)) + geom_bar(stat="identity", color="black", fill="green", alpha=0.4) + 
  xlab("Product demand (normalised)") +
  ylab("Frequency") +
  ggtitle("Gamma distributed product demand")

#Combine results into one graph
build_graph <- function(t_list, g_title) {
  show_hist <- do.call(rbind,t_list)
  show_hist %>% ggplot(aes(x=floor(cost), y=n)) + geom_line(aes(color=legend, linetype=legend), size=1) +
    xlab("bin cost") + ylab("No of movements") + 
    #xlim(4,30) + ylim(-100,5500) +
    labs(colour = "Strategy", linetype = "Strategy") +
    scale_linetype_manual(values=c("solid", "dashed", "dotted", "twodash")) +
    ggtitle(g_title)
  }

cost <- function(ds) {
  return (ds[1,"total_cost"] %>% pull())
}
coststr <- function(ds) {
  return (format(ds[1,"total_cost"] %>% pull(),big.mark=","))
}
trial1 <- run_trial(title="Baseline")
trial2 <- run_trial(placement="near", picking="near", title="Easy")
trial3 <- run_trial(placement="prioritise", picking="near", title="Prioritise")
trial4 <- run_trial(placement="fast", picking="near", title="Fast access")
bg1 <- build_graph(list(trial1, trial2, trial3, trial4), "No stock rotation")

trial5 <- run_trial(title="Baseline", picking="fifo")
trial6 <- run_trial(placement="near", picking="fifo", title="Easy")
trial7 <- run_trial(placement="prioritise", picking="fifo", title="Prioritise")
trial8 <- run_trial(placement="fast", picking="fifo", title="Fast access")
bg2 <- build_graph(list(trial5, trial6, trial7, trial8), "FIFO stock rotation")

trial9 <- run_trial(title="Baseline", picking="mod fifo baseline")
trial10 <- run_trial(placement="near", picking="mod fifo", title="Easy")
trial11 <- run_trial(placement="prioritise", picking="mod fifo", title="Prioritise")
trial12 <- run_trial(placement="fast", picking="mod fifo", title="Fast access")
bg3 <- build_graph(list(trial9, trial10, trial11, trial12), "Modified FIFO stock rotation")

trial13 <- run_trial(warehouse="full cost",title="Baseline", picking="mod fifo baseline")
trial14 <- run_trial(warehouse="full cost",placement="near", picking="mod fifo", title="Easy")
trial15 <- run_trial(warehouse="full cost",placement="prioritise", picking="mod fifo", title="Prioritise")
trial16 <- run_trial(warehouse="full cost",placement="fast", picking="mod fifo", title="Fast access")
bg4 <- build_graph(list(trial13, trial14, trial15, trial16), "Modified warehouse layout")

trial17 <- run_trial(warehouse="full cost",title="Baseline", picking="mod fifo baseline",prod_arrival = 20)
trial18 <- run_trial(warehouse="full cost",placement="near", picking="mod fifo", title="Easy",prod_arrival = 20)
trial19 <- run_trial(warehouse="full cost",placement="prioritise", picking="mod fifo", title="Prioritise",prod_arrival = 20)
trial20 <- run_trial(warehouse="full cost",placement="fast", picking="mod fifo", title="Fast access",prod_arrival = 20)
bg5 <- build_graph(list(trial17, trial18, trial19, trial20), "Container delivery")

trial21 <- run_trial(warehouse="full cost",title="Baseline+FIFO", picking="fifo", prod_arrival = 20)
trial22 <- run_trial(warehouse="full cost",placement="near", picking="near", title="Easy",prod_arrival = 20)
trial23 <- run_trial(warehouse="full cost",placement="near", picking="fifo", title="Easy+FIFO",prod_arrival = 20)
bg6 <- build_graph(list(trial21, trial22, trial23), "Range of possible FIFO modification")



trial1 <- run_trial(warehouse="full cost", title="Fully random")
trial2 <- run_trial(warehouse="full cost", placement="near", picking = "near", title = "Nearest bin")
trial3 <- run_trial(warehouse="full cost", placement="prioritise", picking = "near", title = "Prioritise")
trial4 <- run_trial(warehouse="full cost", placement="fast", picking="near", title = "Fast access bins")
bg1<-build_graph(list(trial1, trial2, trial3, trial4), "Full costing, no stock rotation")

trial5 <- run_trial(warehouse="full cost", title="Fully random",prod_arrival = 20)
trial6 <- run_trial(warehouse="full cost", placement="near", picking = "near", title = "Nearest bin",prod_arrival = 20)
trial7 <- run_trial(warehouse="full cost", placement="prioritise", picking = "near", title = "Prioritise",prod_arrival = 20)
trial8 <- run_trial(warehouse="full cost", placement="fast", picking="near", title = "Fast access bins",prod_arrival = 20)
bg2<-build_graph(list(trial5, trial6, trial7, trial8), "Full costing, no stock rotation, bulk arrival")

trial9 <- run_trial(warehouse="full cost", picking = "fifo", title="Fully random")
trial10 <- run_trial(warehouse="full cost", placement="near", picking = "fifo", title = "Nearest bin")
trial11 <- run_trial(warehouse="full cost", placement="prioritise", picking = "fifo", title = "Prioritise")
trial12 <- run_trial(warehouse="full cost", placement="fast", picking="fifo", title = "Fast access bins")
bg3<-build_graph(list(trial9, trial10, trial11, trial12), "Full costing, FIFO")

trial13 <- run_trial(warehouse="full cost", picking = "fifo", title="Fully random",prod_arrival = 20)
trial14 <- run_trial(warehouse="full cost", placement="near", picking = "fifo", title = "Nearest bin",prod_arrival = 20)
trial15 <- run_trial(warehouse="full cost", placement="prioritise", picking = "fifo", title = "Prioritise",prod_arrival = 20)
trial16 <- run_trial(warehouse="full cost", placement="fast", picking="fifo", title = "Fast access bins",prod_arrival = 20)
bg4<-build_graph(list(trial13, trial14, trial15, trial16), "Full costing, FIFO, bulk arrival")

trial17 <- run_trial(warehouse="full cost", placement="near", picking = "mod fifo", title = "Nearest bin")
trial18 <- run_trial(warehouse="full cost", placement="prioritise", picking = "mod fifo", title = "Prioritise")
trial19 <- run_trial(warehouse="full cost", placement="fast", picking="mod fifo", title = "Fast access bins")
bg5<-build_graph(list(trial17, trial18, trial19), "Full costing, mod FIFO")

trial20 <- run_trial(warehouse="full cost", placement="near", picking = "mod fifo", title = "Nearest bin",prod_arrival = 20)
trial21 <- run_trial(warehouse="full cost", placement="prioritise", picking = "mod fifo", title = "Prioritise",prod_arrival = 20)
trial22 <- run_trial(warehouse="full cost", placement="fast", picking="mod fifo", title = "Fast access bins",prod_arrival = 20)
bg5<-build_graph(list(trial20, trial21, trial22), "Full costing, mod FIFO, bulk arrival")






trial1 <- run_trial(despatch_mode = "random",title = "Baseline")
trial3 <- run_trial(bin_rule="prioritise",receive_mode = "min-cost", despatch_mode = "random", title = "Prioritisation")
trial4 <- run_trial(bin_rule = "fast access", receive_mode = "fast access", despatch_mode = "random", title = "Fast access")
bg1<-build_graph(list(trial1,  trial3, trial4), "Simple geometry, no stock rotation")

trial2 <- run_trial(receive_mode = "easiest", title = "Baseline + FIFO")
trial5 <- run_trial(bin_rule = "fast access", receive_mode = "fast access", title="Fast access + FIFO")
trial6 <- run_trial(bin_rule = "fast access", receive_mode = "fast access", despatch_mode = "mod fifo",title =  "Fast access + mod FIFO")
trial7 <- run_trial(receive_mode = "easiest", despatch_mode = "mod fifo",title= "Baseline + mod FIFO")
bg2 <-build_graph(list(trial2, trial5, trial6, trial7), "Simple geometry, stock rotation")

trial8 <- run_trial(despatch_mode = "random",title = "Baseline", simple_warehouse = F)
trial9 <- run_trial(bin_rule="prioritise",receive_mode = "min-cost", despatch_mode = "random", title = "Prioritisation", simple_warehouse = F)
trial10 <- run_trial(bin_rule = "fast access", receive_mode = "fast access", despatch_mode = "random", title = "Fast access", simple_warehouse = F)
bg3<-build_graph(list(trial8,  trial9, trial10), "Realistic geometry, no stock rotation")

trial11 <- run_trial(receive_mode = "easiest", title = "Baseline + FIFO", simple_warehouse = F)
trial12 <- run_trial(bin_rule = "fast access", receive_mode = "fast access", title="Fast access + FIFO", simple_warehouse = F)
trial13 <- run_trial(bin_rule = "fast access", receive_mode = "fast access", despatch_mode = "mod fifo",title =  "Fast access + mod FIFO", simple_warehouse = F)
trial14 <- run_trial(receive_mode = "easiest", despatch_mode = "mod fifo",title= "Baseline + mod FIFO", simple_warehouse = F)
bg4 <-build_graph(list(trial11, trial12, trial13, trial14), "Realistic geometry, stock rotation")

trial15 <- run_trial(despatch_mode = "random",title = "Baseline", prod_arrival = 20)
trial16 <- run_trial(bin_rule="prioritise",receive_mode = "min-cost", despatch_mode = "random", title = "Prioritisation", prod_arrival = 20)
trial17 <- run_trial(bin_rule = "fast access", receive_mode = "fast access", despatch_mode = "random", title = "Fast access", prod_arrival = 20)
bg5<-build_graph(list(trial15,  trial16, trial17), "Simple geometry, no stock rotation, bulk arrival")

trial18 <- run_trial(receive_mode = "easiest", title = "Baseline + FIFO", prod_arrival = 20)
trial19 <- run_trial(bin_rule = "fast access", receive_mode = "fast access", title="Fast access + FIFO", prod_arrival = 20)
trial20 <- run_trial(bin_rule = "fast access", receive_mode = "fast access", despatch_mode = "mod fifo",title =  "Fast access + mod FIFO", prod_arrival = 20)
trial21 <- run_trial(receive_mode = "easiest", despatch_mode = "mod fifo",title= "Baseline + mod FIFO", prod_arrival = 20)
bg6 <-build_graph(list(trial18, trial19, trial20, trial21), "Simple geometry, stock rotation, bulk arrival")
