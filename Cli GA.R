
##Initial selected gene
##-----------------
data <- data.frame(cli = sort(sample(500:14000,35000,replace = T)))
static_grp_cut <- c(499,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000,13000,14000)
static_grp <-static_grp_cut[-length(static_grp_cut)]

data$vec <- as.numeric(as.character(cut(data$cli,breaks = static_grp_cut,labels = static_grp  )))
sqrt(sum((data$cli-data$vec)*(data$cli-data$vec)))

##final selected gene
##-----------------
final_gene <- round(gene_mat[,all_generation_min_cost_ind[gen]],-2)
final_gene_grp <-final_gene[-length(final_gene)]

data$vec_fin <- as.numeric(as.character(cut(data$cli,breaks = final_gene,labels = final_gene_grp  )))
sqrt(sum((data$cli-data$vec_fin)*(data$cli-data$vec_fin)))


## Initialization
##-----------------
k <- .7
f <- .6
cr <- .1
gen <- 200
no_gene <- 200

gene_mat <- matrix(sample(500:14000,(15*no_gene),replace = F),ncol = no_gene)

for (ii in 1 : gen) {
ptm <- proc.time()
for (i in 1:no_gene){

## Mutation
##-----------------
pos <- sample(1:no_gene,3,replace = F)
mutated_vec <- gene_mat[,i]+k*(gene_mat[,pos[1]]-gene_mat[,i])+f*(gene_mat[,pos[2]]-gene_mat[,pos[3]])

parent_vec_scaled <- sort(499+((gene_mat[,i] - min(gene_mat[,i]) )/(max(gene_mat[,i]) - min(gene_mat[,i]) ))*(14000-499))
mutated_vec_scaled <- sort(499+((mutated_vec - min(mutated_vec) )/(max(mutated_vec) - min(mutated_vec) ))*(14000-499))

## Cross over
##-----------------
trial_vec <- parent_vec_scaled
rn = sample(1:15,1,replace = F)

for (j in 1:length(parent_vec_scaled)){
  rnj <- runif(1, 0, 1) 
  if (j == rn || rnj <= cr ) {
    trial_vec[j] <- mutated_vec_scaled[j]
  }
}

trial_vec_scaled <- sort(trial_vec)
if(length(unique(trial_vec_scaled)) < 15) {
  trial_vec_scaled <- parent_vec_scaled  
}

## Selection
##-----------------
parent_vec_scaled_grp <- parent_vec_scaled[-length(parent_vec_scaled)]
trial_vec_scaled_grp <- trial_vec_scaled[-length(trial_vec_scaled)]

parent_cost_data <- as.numeric(as.character(cut(data$cli,breaks = parent_vec_scaled,labels = parent_vec_scaled_grp  )))
trial_cost_data <- as.numeric(as.character(cut(data$cli,breaks = trial_vec_scaled,labels = trial_vec_scaled_grp  )))

parent_cost <- sqrt(sum((data$cli-parent_cost_data)*(data$cli-parent_cost_data)))
trial_cost <- sqrt(sum((data$cli-trial_cost_data)*(data$cli-trial_cost_data)))

if (parent_cost <= trial_cost) {
  selected_vec <- parent_vec_scaled
  selected_cost <- parent_cost 
}else {
  selected_vec <- trial_vec_scaled
  selected_cost <- trial_cost 
}

if (i== 1) {
  next_gen_gene_pool <- selected_vec 
  generation_cost <- selected_cost 
}else {
  next_gen_gene_pool <- cbind(next_gen_gene_pool,selected_vec) 
  generation_cost <- cbind(generation_cost,selected_cost)
}
min_generation_cost <-min(generation_cost)
min_generation_cost_ind <-which.min(generation_cost)
}
  
  gene_mat <- next_gen_gene_pool
  
  
  if (ii== 1) {
    all_generation_min_cost <- min_generation_cost 
    all_generation_min_cost_ind <- min_generation_cost_ind 
  }else {
    all_generation_min_cost <- cbind(all_generation_min_cost,min_generation_cost) 
    all_generation_min_cost_ind <- cbind(all_generation_min_cost_ind,min_generation_cost_ind) 
  }  
  
  print(paste('Generation number : ',ii))
  print(paste('Error : ' ,min_generation_cost))
  print(paste('Time Taken : ' , (proc.time() - ptm)[1]))
  print('-------------------------------')
}



# Plot error 
x <- 1:gen
plot(x,
     all_generation_min_cost,
     main="Error By generation",
     xlab="Generation",
     ylab="SSE",
     type="l",
     col="blue")


