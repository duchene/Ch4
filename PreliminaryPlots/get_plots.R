source("fixorders.R")

setwd("hetrun2")

res_files <- list.dirs()
res_files <- gsub("./", "", res_files[2:length(res_files)]) 

summary_matrix <- matrix(NA, length(res_files), 3)

for(i in 1:length(res_files)){
      setwd(res_files[i])
      print(paste("I am processing data set", res_files[i]))
      est_tre <- read.annotated.nexus("estimated.tree")
      sim_tre <- read.tree(grep("*tre$", dir(), value = T))
      node_data <- get.time.coverage(tr.est = est_tre, tr.sim = sim_tre)$coverage.nodes
      summary_matrix[i, ] <- c(res_files[i] , sum(node_data), length(node_data))
      print("The coverage is:")
      print(summary_matrix[i, ])
      setwd("..")
}

setwd("..")


summary_matrix <- summary_matrix[as.numeric(summary_matrix[, 2]) > 10, ]


# ~~ Deep magic begins here... ~~

summary_matrix[, 1] <- gsub("_[0-9]$", "", summary_matrix[, 1])

map_matrix <- matrix(NA, 9, 3)
colnames(map_matrix) <- c("highbal", "midbal", "lowbal")
rownames(map_matrix) <- c(paste(rep("clock", 3), c("deep", "median", "shallow"), sep = "_"), paste(rep("uncorlog", 3), c("deep", "median", "shallow"), sep = "_"), paste(rep("autocor", 3), c("deep", "median", "shallow"), sep = "_"))



for(i in 1:nrow(map_matrix)){
      for(k in 1:ncol(map_matrix)){
      	    name_1 <- strsplit(rownames(map_matrix)[i], "_")[[1]]
	    name_2 <- colnames(map_matrix)[k]
	    name_search <- paste(c(name_1[1], name_2, name_1[2]), collapse = "_")
	    print(name_search)
	    matrix_location <- grep(name_search, summary_matrix[, 1])
      	    map_matrix[i, k] <- sum(as.numeric(summary_matrix[matrix_location, 2])) / sum(as.numeric(summary_matrix[matrix_location, 3])) 
      }
}

corrplot(1 - map_matrix, is.cor = F, cl.pos = "n", method = "color", tl.col = "black")
