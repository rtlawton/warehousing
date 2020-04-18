library(tidyverse)
# Set parameters for warehouse
aisles <- 10
tiers <- 5
slots <- 100
height_cost <- 1.0 #ratio: (cost of going up a tier)/(cost of going out an aisle)
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
  wh <- wh %>% mutate(cost = aisle + height_cost*tier - 1 - height_cost, cycle_in = 0)
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
# add a minimum cost column to ensure the lowest cost bins are reserved for high demand products
  min_c <- sapply(c(0,prod_list$c_dem[1:product_range-1]), function(cp) {
    wh[cp * capacity + 1,"cost"]
  })
  prod_list <- prod_list %>% mutate(min_cost = min_c)
  return (prod_list)
}

#
# Function to populate warehouse
#
pop_warehouse <- function(quantity_rule, bin_rule, prod_list) {
  prod_pop <- runif(capacity * warehouse_loading, 0, 1.0)
#
#Quantities of stock set at random levels
  if(quantity_rule == "random") {
    prod_pop <- sample(seq(1,product_range), replace=T, size = capacity*warehouse_loading)
#
#Quantities of stock set probabilistically according to demand levels
  } else if(quantity_rule == "match demand") {
    prod_pop <- runif(capacity*warehouse_loading, 0, 1.0)
#Map random input from 0-1 onto previosuly generated demand curve
    prod_pop <- sapply(prod_pop, function(x) {
      return (prod_list %>% filter(c_dem <= x) %>% count() %>% pull() + 1)
    })
  } else {
    return ("Quantity rule not recognised")
  }
#
# Product placed randomly
  
  if (bin_rule == "random-position"){
    prod_pop <- sample(c(prod_pop, rep(0,capacity-length(prod_pop))))
#  
# Product placement prioritised for big movers 
  } else if (bin_rule == "prioritise") {
    prod_pop <- c(sort(prod_pop), rep(0,capacity-length(prod_pop)))
# Place for fast access strategy
  } else if (bin_rule == "fast access") {
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

despatch <- function(pr, rule='fifo', tr_hist, wh, prod_list) {
  if (rule == 'fifo') {
    prod_bins <- wh %>% filter(product == pr) %>% arrange(cycle_in)
  } else if (rule == 'mod fifo') {
    prod_bins <- wh %>% filter(product == pr & cycle_in < nrow(tr_hist) - age_limit)
    if (nrow(prod_bins) == 0) {
      prod_bins <- wh %>% filter(product == pr)
    }
  } else {
    prod_bins <- wh %>% filter(product == pr)
  }
  if (nrow(prod_bins)==0) {
    tr_hist[nrow(tr_hist),"trans"] = "Stockout"
    tr_hist[nrow(tr_hist),"bin"] = 0
    tr_hist[nrow(tr_hist),"cost"] = 0
    return (list(tr_hist,wh))
  } else {
    r <-  prod_bins %>% head(1)["bin_no"]
    wh[r,"product"] <- 0
    tr_hist[nrow(tr_hist),"trans"] = "Despatch"
    tr_hist[nrow(tr_hist),"bin"] = r
    tr_hist[nrow(tr_hist),"cost"] = wh[r,"cost"]
    return (list(tr_hist,wh))
  }
}

receive <- function(pr, rule='random', tr_hist, wh, prod_list) {
  empty_bins <- wh %>% filter(product == 0)
  if (nrow(empty_bins) == 0) {
    tr_hist[nrow(tr_hist),"trans"] = "Overflow"
    tr_hist[nrow(tr_hist),"bin"] = 0
    tr_hist[nrow(tr_hist),"cost"] = 0
    return (list(tr_hist,wh))
  } 
  if (rule == 'random') {
    r <- sample(empty_bins$bin_no,1) 
  } else if (rule == 'easiest') {
    r <- (empty_bins %>% head(1))$bin_no
  } else if (rule == 'min-cost') {
    m_cost <- prod_list[pr,"min_cost"]
    next_bins <- empty_bins %>% filter(cost >= m_cost)
    if (nrow(next_bins) == 0) {
      r <- (empty_bins %>% filter(cost < m_cost) %>% tail(1))$bin_no
    } else {
      r <- (next_bins %>% head(1))$bin_no 
    }
  } else if (rule ==  "fast access") {
    fast_bins <- empty_bins %>% filter(bin_no > fast_access_limit*(pr-1) &
                  bin_no <= fast_access_limit * pr )
    if (nrow(fast_bins) == 0) {
      r <- (empty_bins %>% filter(bin_no > fast_access_limit*product_range)%>% head(1))$bin_no
    } else {
      r <- (fast_bins %>% head(1))$bin_no
    }
  } else {
    return (list(tr_hist,wh))
  }
  wh[r,"product"] <- pr
  wh[r,"cycle_in"] <- nrow(tr_hist)
  tr_hist[nrow(tr_hist),"trans"] = "Receive"
  tr_hist[nrow(tr_hist),"bin"] = r
  tr_hist[nrow(tr_hist),"cost"] = wh[r,"cost"]
  return (list(tr_hist,wh))
}

stock_move <- function(receive_mode, despatch_mode, tr_hist, wh, prod_list) {
  pr <- prod_list %>% filter(c_dem <= runif(1,0,1)) %>% count() %>% pull() + 1
  direction <- sample(c("receive","despatch"), replace=T, size=1)
  tr_hist[nrow(tr_hist)+1,"product"] = pr
  tr_hist[nrow(tr_hist),"cycle"] = nrow(tr_hist)
  tr_hist[nrow(tr_hist),"trans"] = direction
  if (direction == "receive") {
    return(receive(pr,receive_mode, tr_hist, wh, prod_list))
  } else {
    return (despatch(pr,despatch_mode, tr_hist, wh, prod_list))
  }
}


# Build a warehouse, populate it and run 10000 cycles - repeat n times
run_trial <- function(stock_quantity_rule, bin_rule, receive_mode, despatch_mode,n, title) {
  prod_list <- catalogue()
  costs <- c()
  for (i in 1:n) {
    wh <- build_it()
    wh <- wh %>% mutate(product = pop_warehouse(stock_quantity_rule,bin_rule,prod_list))
    b <- cycle(receive_mode, despatch_mode,wh, prod_list)
    wh <- b[[1]]
    costs <- c(costs, b[[2]])
    tr_hist <- b[[3]]
    cat("cycle ", i, "...cost=", b[[2]],"\n") #Progress monitor
  }
  tr_summary <- tr_hist%>%group_by(cost) %>% summarise(n=n())
  tr_summary["legend"] <- paste(title, ":", costs[1])
  tr_summary["total_cost"] <- costs[1]
  return (tr_summary)
}

#Run 10000 stock movements
cycle <- function (receive_mode, despatch_mode, wh, prod_list) {
  tr_hist <- data.frame(cycle=integer(), trans=character(), product=integer(), 
                        bin=integer(), cost=integer(),stringsAsFactors= F)
  for (i in 1:10000) {
    b <- stock_move(receive_mode,despatch_mode, tr_hist, wh, prod_list)
    tr_hist <- b[[1]]
    wh <- b[[2]]
  }
  return(list(wh,sum(tr_hist$cost), tr_hist))
}

#Show warehouse bin costs

wh <- build_it()
wh_graph <- wh %>% group_by(cost) %>% summarize(n=n()) %>% ggplot(aes(x=cost, y=n)) + 
  geom_bar(stat="identity") + xlab("Marginal bin cost") +
  ylab("Number of bins") +
  ggtitle("Warehouse geometry: distribution of bin cost")

#Sample product demand distribution
pr_list <- catalogue()
pr_graph <-pr_list %>% ggplot(aes(x=dem)) + geom_histogram(bins=30) + 
  xlab("Product demand (normalised)") +
  ylab("Frequency") +
  ggtitle("Gamma distributed product demand")

#Combine results into one graph
build_graph <- function(t_list, g_title) {
  show_hist <- do.call(rbind,t_list)
  show_hist %>% ggplot(aes(x=cost, y=n)) + geom_line(aes(color=legend, linetype=legend)) +
    xlab("bin cost") + ylab("No of movements") +
    scale_linetype_manual(values=c("solid", "dashed", "dotted", "twodash")) +
    ggtitle(g_title)
  }


trial1 <- run_trial("match demand", "random-position", "easiest", "random",1, "Baseline")
trial3 <- run_trial("match demand", "prioritise", "min-cost", "random",1, "Prioritisation")
trial4 <- run_trial("match demand", "fast access", "fast access", "random",1, "Fast access")
bg1<-build_graph(list(trial1,  trial3, trial4), "Cost of warehouse strategies: no stock rotation")

trial2 <- run_trial("match demand", "random-position", "easiest", "fifo",1, "Baseline + FIFO")
trial5 <- run_trial("match demand", "fast access", "fast access", "fifo",1, "Fast access + FIFO")
trial6 <- run_trial("match demand", "fast access", "fast access", "mod fifo",1, "Fast access + mod FIFO")
trial7 <- run_trial("match demand", "random-position", "easiest", "mod fifo",1, "Baseline + mod FIFO")
bg2 <-build_graph(list(trial2, trial5, trial6, trial7), "Cost of warehouse strategies: stock rotation")

