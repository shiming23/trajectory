# modified from 'functionsV2.R'
# 21 July, 2021

Endness_multicomponents <- function(x0, X, knei=10, L = 0.2, FIG = F, FIGcomp = F){
  
  # Added at 21 July, 2021
  
  distMat <- base::as.matrix(dist(X, method="euclidean")) # distance matrix for the points, symmetric matrix
  
  knnNei <- knnFinder(distMat,round(knei)) # find knn for each of the point
  
  library(igraph)
  
  G <- graph_from_data_frame(knnNei, directed = F)
  
  simpG <- simplify(G, remove.loops = FALSE, edge.attr.comb = "mean")
  
  Gcomponentslist <- decompose(simpG)
  
  if(FIGcomp == TRUE){
    
  }
  
  for( ii in 1:length(Gcomponentslist))
    if(as.character(x0) %in% as_ids(V(Gcomponentslist[[ii]])))
      break;
  
  Nodescomponent <- as.numeric(as_ids(V(Gcomponentslist[[ii]])))
  Xnew <- X[Nodescomponent,]
  x0new <- which(x0 == Nodescomponent)
  
  Endness(x0new, Xnew, knei, L, FIG)
  
}

Endness_allpoints <- function(X, knei=10, L = 0.2, FIG = F){
  # Initilization
  endv <- vector(mode = 'numeric', length = nrow(X))
  
  # Step 1 graph construction
  # distance matrix for the points, symmetric matrix
  distMat <- base::as.matrix(dist(X, method="euclidean")) 
  knnNei <- knnFinder(distMat,round(knei)) # find knn for each of the point
  
  library(igraph)
  G <- graph_from_data_frame(knnNei, directed = F)
  simpG <- simplify(G, remove.loops = FALSE, edge.attr.comb = "mean")
  distAll = distances(simpG, weights = NULL) #The distance beteen each node pair in the graph
  
  # Step 2: find the intrinsic width in the point set X with respect the point id x0
  for(x0 in 1:nrow(distAll)){
    print(paste0(x0,'...'))
    
    intriWidth=c() # initialization
    intriRadius = c() 
    
    dist0 = distAll[x0,]# The distance from each node to the start point x0
    #errL = err* max(dist0)
    for(ii in seq(0, 1, 1/100 ) ){ #100 is the number of path points to calculate the intrinsic width
      iiSet = (dist0<= ii*max(distAll))
      iiWidth = max(distAll[iiSet, iiSet])
      intriWidth = c(intriWidth,iiWidth)
      iiRadius = max(dist0[iiSet])
      intriRadius = c(intriRadius, iiRadius)
    }
    
    vind <- !is.na(intriWidth/intriRadius) & intriRadius/max(distAll)<= L
    library('pracma')
    curveDv <- cbind((intriRadius/max(distAll))[vind], (intriWidth/intriRadius)[vind])
    endv[x0] <- 2- trapz(c(0, curveDv[,1]), c(1, curveDv[,2]))/max(curveDv[,1])
  }
  return(endv)
    
}

Endness_allpoints_paral <- function(X, knei=10, L = 0.2, FIG = F){
  # Initilization

  # Step 1 graph construction
  # distance matrix for the points, symmetric matrix
  distMat <- base::as.matrix(dist(X, method="euclidean")) 
  knnNei <- knnFinder(distMat,round(knei)) # find knn for each of the point
  
  library(igraph)
  G <- graph_from_data_frame(knnNei, directed = F)
  simpG <- simplify(G, remove.loops = FALSE, edge.attr.comb = "mean")
  distAll = distances(simpG, weights = NULL) #The distance beteen each node pair in the graph
  
  # Step 2: find the intrinsic width in the point set X with respect the point id x0
  # Parallel computing by 'foreach' package, with reference
  # https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
  library(foreach)
  library(doParallel)
  registerDoParallel(8)

  endv <- foreach (x0 = 1:nrow(distAll), .combine = c)  %dopar% {
    intriWidth=c() # initialization
    intriRadius = c() 
    
    dist0 = distAll[x0,]# The distance from each node to the start point x0
    #errL = err* max(dist0)
    for(ii in seq(0, 1, 1/100 ) ){ #100 is the number of path points to calculate the intrinsic width
      iiSet = (dist0<= ii*max(distAll))
      iiWidth = max(distAll[iiSet, iiSet])
      intriWidth = c(intriWidth,iiWidth)
      iiRadius = max(dist0[iiSet])
      intriRadius = c(intriRadius, iiRadius)
    }
    
    vind <- !is.na(intriWidth/intriRadius) & intriRadius/max(distAll)<= L
    library('pracma')
    curveDv <- cbind((intriRadius/max(distAll))[vind], (intriWidth/intriRadius)[vind])
    2- trapz(c(0, curveDv[,1]), c(1, curveDv[,2]))/max(curveDv[,1])
  }
  # When you're done, clean up the cluster
  stopImplicitCluster()
  
  return(endv)
  
}


Endness.lmax <- function(x0, X, knei = 10, L = 0.2, FIG = F){
  # Edited on April 26, 2022
  # Calculate the endness of the data set. 
  # The endness of the whole data set is defined as the maximum endnesses of endpoints
  # in the longest path of the whole nearest neighborhood graph 
}

Endness <- function(x0, X, knei=10, L = 0.2, FIG = F){
  
  # Version 1.5, 2021-3-16; Correct local iiSet.
  # Function for intrinsic width. 
  # x0: start point to calculate the intrinsic width, must belong to X
  # X:the locations for all the nodes in the feature space.
  
  # Step 1: calcuate the geometric distance matrix for each point pair in X.
  
  # Step 1-1: Build a knn graph
  # G = knn(X)
  
  distMat <- base::as.matrix(dist(X, method="euclidean")) # distance matrix for the points, symmetric matrix
  
  knnNei <- knnFinder(distMat,round(knei)) # find knn for each of the point
  
  library(igraph)
  
  G <- graph_from_data_frame(knnNei, directed = F)
  
  simpG <- simplify(G, remove.loops = FALSE, edge.attr.comb = "mean")
  
  distAll = distances(G, weights = NULL) #The distance beteen each node pair in the graph
  
  # Step 2: find the intrinsic width in the point set X with respect the point id x0
  
  intriWidth=c() # initialization
  
  intriRadius = c() 
  
  dist0 = distAll[x0,]# The distance from each node to the start point x0
  
  #errL = err* max(dist0)
  
  for(ii in seq(0, 1, 1/100 ) ){ #100 is the number of path points to calculate the intrinsic width
    
    iiSet = (dist0<= ii*max(distAll))
    
    iiWidth = max(distAll[iiSet, iiSet])
    
    intriWidth = c(intriWidth,iiWidth)
    
    iiRadius = max(dist0[iiSet])
    
    intriRadius = c(intriRadius, iiRadius)
    
  }
  
  vind <- !is.na(intriWidth/intriRadius) & intriRadius/max(distAll)<= L
  
  library('pracma')
  
  curveDv <- cbind((intriRadius/max(distAll))[vind], (intriWidth/intriRadius)[vind])
  
  if(FIG){
    plot(c(0, curveDv[,1]), c(1, curveDv[,2])
         , type = 'l'
         , xlim = c(0,1), ylim = c(1,2))
    
  }
  
  2- trapz(c(0, curveDv[,1]), c(1, curveDv[,2]))/max(curveDv[,1])  
  
}




Endness_endnesscurve <- function(x0, X, knei=10, L = 0.2, FIG = F){
  
  # Added 12-Oct, 2021
  # Function for endcurve. 
  # x0: start point to calculate the intrinsic width, must belong to X
  # X:the locations for all the nodes in the feature space.
  
  # Step 1: calcuate the geometric distance matrix for each point pair in X.
  
  # Step 1-1: Build a knn graph
  # G = knn(X)
  
  distMat <- base::as.matrix(dist(X, method="euclidean")) # distance matrix for the points, symmetric matrix
  
  knnNei <- knnFinder(distMat,round(knei)) # find knn for each of the point
  
  library(igraph)
  
  G <- graph_from_data_frame(knnNei, directed = F)
  
  simpG <- simplify(G, remove.loops = FALSE, edge.attr.comb = "mean")
  
  distAll = distances(G, weights = NULL) #The distance beteen each node pair in the graph
  
  # Step 2: find the intrinsic width in the point set X with respect the point id x0
  
  intriWidth=c() # initialization
  
  intriRadius = c() 
  
  dist0 = distAll[x0,]# The distance from each node to the start point x0
  
  #errL = err* max(dist0)
  
  for(ii in seq(0, 1, 1/100 ) ){ #100 is the number of path points to calculate the intrinsic width
    
    iiSet = (dist0<= ii*max(distAll))
    
    iiWidth = max(distAll[iiSet, iiSet])
    
    intriWidth = c(intriWidth,iiWidth)
    
    iiRadius = max(dist0[iiSet])
    
    intriRadius = c(intriRadius, iiRadius)
    
  }
  
  vind <- !is.na(intriWidth/intriRadius) & intriRadius/max(distAll)<= L
  
  library('pracma')
  
  curveDv <- cbind((intriRadius/max(distAll))[vind], (intriWidth/intriRadius)[vind])
  
  if(FIG){
    plot(c(0, curveDv[,1]), c(1, curveDv[,2])
         , type = 'l'
         , xlim = c(0,1), ylim = c(1,2))
    
  }
  
  curveDv <- rbind( t(c(0,1)), curveDv)
  
  # 2- trapz(c(0, curveDv[,1]), c(1, curveDv[,2]))/max(curveDv[,1])  
  
}

knnFinder <- function(distMat, k = 10){
  
  # a self defined funciton to find the nearest neighbors for each point. 
  # distMat: distMatrix for each point pairs
  # An alternative for the function dist_to_knn in scanstatistics
  
  # Attention:
  # 1) Undirected distance matrix!!! 
  # 2) Exclude selfloops!!!
  
  knnMat <- apply(distMat, 1, function(x) order(x)[2:(k+1)]) ## knn matrix for the values
  
  eLis <- cbind(rep(1:ncol(knnMat), each = k ), as.vector(knnMat))
  
  eLis_ind <- (eLis[,2]-1)*nrow(distMat)+ eLis[,1]
  
  weight <- distMat[eLis_ind]
  
  return(cbind(eLis, weight))
}

Endness_FIGmdf <- function(x0, X, knei=10, L = 0.2
                           , FIG = F
                           , addFIG=F
                           , FIGcol = 'black'
                           , FIGlwd = 1
                           , FIGpch = 20
                           , FIGlty = 1
                           , FIGtype ='l'){
  
  # Modified from Endness at 2021-3-27;
  # Ad R(l) line in a existed figure
  
  distMat <- base::as.matrix(dist(X, method="euclidean")) # distance matrix for the points, symmetric matrix
  
  knnNei <- knnFinder(distMat,round(knei)) # find knn for each of the point
  
  library(igraph)
  
  G <- graph_from_data_frame(knnNei, directed = F)
  
  simpG <- simplify(G, remove.loops = FALSE, edge.attr.comb = "mean")
  
  distAll = distances(G, weights = NULL) #The distance beteen each node pair in the graph
  
  # Step 2: find the intrinsic width in the point set X with respect the point id x0
  
  intriWidth=c() # initialization
  
  intriRadius = c() 
  
  dist0 = distAll[x0,]# The distance from each node to the start point x0
  
  #errL = err* max(dist0)
  
  for(ii in seq(0, 1, 1/100 ) ){ #100 is the number of path points to calculate the intrinsic width
    
    iiSet = (dist0<= ii*max(distAll))
    
    iiWidth = max(distAll[iiSet, iiSet])
    
    intriWidth = c(intriWidth,iiWidth)
    
    iiRadius = max(dist0[iiSet])
    
    intriRadius = c(intriRadius, iiRadius)
    
  }
  
  vind <- !is.na(intriWidth/intriRadius) & intriRadius/max(distAll)<= L
  
  library('pracma')
  
  curveDv <- cbind((intriRadius/max(distAll))[vind], (intriWidth/intriRadius)[vind])
  
  if(FIG){
    if(addFIG){
      lines(c(0, curveDv[,1]), c(1, curveDv[,2])
            , col = FIGcol #= 'black'
            , lwd = FIGlwd #= 1
            , pch = FIGpch #= 20
            , lty = FIGlty #= 1
            , type = FIGtype# ='l'
            #, type = 'l'
            , xlim = c(0,1), ylim = c(1,2))
    }
    else{
      plot(c(0, curveDv[,1]), c(1, curveDv[,2])
           , col = FIGcol #= 'black'
           , lwd = FIGlwd #= 1
           , pch = FIGpch #= 20
           , lty = FIGlty #= 1
           , type = FIGtype# ='l' 
           #, type = 'l'
           , xlim = c(0,1), ylim = c(1,2))
    }
    
  }
  
  2- trapz(c(0, curveDv[,1]), c(1, curveDv[,2]))/max(curveDv[,1])  
  
}
