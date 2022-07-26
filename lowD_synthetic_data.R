# Regeneration of line in toydata
# 28 July, 2021
{
  set.seed(123)
  source('toy_data.R')
  source('functionsV3.R')
  
  X <- toy.line(1000)+ matrix( rnorm(1000*2, sd = 0.025), ncol =2, byrow = T)
  library(ggplot2)
  ggplot(data.frame(x = X[,1], y = X[,2]),aes(x,y))+geom_point()+#coord_fixed()+
    coord_cartesian(xlim = range(X[,1]),y=c(0,1)-0.5)+
    theme_classic()
  endv_line <- Endness_allpoints_paral(X,knei = 10,L = 1,FIG = F)
  
  Xf <- data.frame(x = X[,1], y = X[,2], endv=endv_line)
  library(ggplot2)
  #tiff(filename = 'line.tif',units = 'in', width = 4,height = 3, res = 300)
  ggplot(Xf, aes(x,y))+
    geom_point(aes(colour = endv))+
    #scale_color_gradientn( colours = c( "lightgreen","yellow","red"),limits = c(0,1) )+
    scale_color_gradientn( colours = c( "green",'yellow',"red"),limits = c(0,1) )+
    coord_cartesian(xlim = range(Xf[,1]),y=range(X[,1])-0.5)+
    #coord_fixed()+
    theme_classic()#coord_fixed()
  #dev.off()
}

# Regeneration of cross. 
# 28 July, 2021
{
  source('toy_data.R')
  source('functionsV3.R')
  # For a two dimesional independent noise (e1,e2)~I(1,1)
  # Then the resulting mean deviation from the original point on the 2d plane is
  # E (sqrt(e1^2+e2^2)) =1/3(sqrt(2)+log(1+sqrt(2)))
  X <- toy.cross(1000)+matrix( rnorm(1000*2, sd = 0.025), ncol =2, byrow = T)-0.5
  plot(X[,1],X[,2],asp = 1)
  X <- X %*% matrix(c(sqrt(3),1,sqrt(3),-1)/2/sqrt(2), ncol = 2, byrow = T)
  plot(X[,1],X[,2],asp = 1)
  endv_cross <- Endness_allpoints_paral(X,knei = 10,L = 1,FIG = F)
  
  Xf <- data.frame(x = X[,1], y = X[,2], endv=endv_cross)
  library(ggplot2)
  #tiff(filename = 'cross.tif',units = 'in', width = 4,height = 3, res = 300)
  ggplot(Xf, aes(x,y))+
    geom_point(aes(colour = endv))+
    #scale_color_gradientn( colours = c( "lightgreen","yellow","red"),limits = c(0,1) )+
    scale_color_gradientn( colours = c( "green",'yellow',"red"),limits = c(0,1) )+
    theme_classic()+
    coord_cartesian(xlim = range(Xf[,1]),y=range(X[,1]))
    #coord_fixed()
  #dev.off()
}

# Regeneration of tree. 
# Date: 28 July, 2021
{
  source('toy_data.R')
  source('functionsV3.R')
  
  X <- toy.tree(1000)
  endv_tree <- Endness_allpoints_paral(X,knei = 10,L = 1,FIG = F)
  
  Xf <- data.frame(x = X[,1], y = X[,2], endv=endv_tree)
  library(ggplot2)
  tiff(filename = 'tree.tif',units = 'in', width = 4,height = 3, res = 300)
  ggplot(Xf, aes(x,y))+
    geom_point(aes(colour = endv))+
    #scale_color_gradientn( colours = c( "lightgreen","yellow","red"),limits = c(0,1) )+
    scale_color_gradientn( colours = c( "green",'yellow',"red"),limits = c(0,1) )+
    coord_fixed()
  dev.off()
}

# Simulation of a whole circle
{
  source('toy_data.R')
  source('functionsV3.R')
  
  X <- circle.synthesis.data(1000)
  endv_cir <- Endness_allpoints_paral(X, knei = 12, L=1, FIG = T)
  
  Xf <- data.frame(x = X[,1], y = X[,2], endv=endv_cir)
  
  library(ggplot2)
  #tiff(filename = 'circle.tif',units = 'in', width = 4,height = 3, res = 300)
  library(ggplot2)
  ggplot(Xf, aes(x,y))+
    geom_point(aes(colour = endv))+
    #scale_color_gradientn( colours = c( "lightgreen","yellow","red"),limits = c(0,1) )+
    scale_color_gradientn( colours = c( "green",'yellow',"red"),limits = c(0,1) )+
    #theme_bw()+
    theme_classic()
    coord_fixed()
  #dev.off()
  
  colv <- colorRampPalette(c('green', 'yellow','red'))(10)[round(endv_cir*(10-1)+1)]
  library(plotly)
  f <- plot_ly(x = X[,1],y= X[,2], mode ='markers', type = 'scatter'
          , color = endv_cir, colors=colv) %>%
    # Set the aspect ratio, referred from
    # https://stackoverflow.com/questions/51031095 
    layout(yaxis = list(scaleanchor = "x")) %>%
    # layout(scene = list(aspectratio=list(x=1,y=2))) %>%
    hide_legend()
  f
  # library(processx)
  # orca(f, file = 'endv_circle.png', width = 1200, height = 900)
  plotly_IMAGE(f, width = 1200, height = 900, format = 'png', out_file = 'endv_circle.png')
}

# Regeneration of sector1. 
# Date: 28 July, 2021
{
  source('toy_data.R')
  source('functionsV3.R')
  X <- toy.segment1(theta = 1/6*pi, 1000)
  # if a two dimensional independent noise (e1,e2)~I(1,1)
  # Then E ( sqrt(e1^2+e2^2) ) =1/3(sqrt(2)+ln(sqrt(2)+1)
  X <- X + matrix(rnorm(nrow(X)*2, sd = 0.025), ncol =2, byrow = T)
  tm <- matrix(c(cos(pi/12),-sin(pi/12), sin(pi/12), cos(pi/12)), ncol =2, byrow=T)
  X <- X%*% tm
  print(Sys.time())
  endv_sect1 <- Endness_allpoints_paral(X,knei = 10,L = 1,FIG = F)
  print(Sys.time())
  
  Xf <- data.frame(x = X[,1], y = X[,2], endv=endv_sect1)
  library(ggplot2)
  #tiff(filename = 'sector1.tif',units = 'in', width = 4,height = 3, res = 300)
  #library(ggplot2)
  ggplot(Xf, aes(x,y))+
    geom_point(aes(colour = endv))+
    #scale_color_gradientn( colours = c( "lightgreen","yellow","red"),limits = c(0,1) )+
    scale_color_gradientn( colours = c( "green",'yellow',"red"),limits = c(0,1) )+
    theme_classic()+
    coord_cartesian(xlim = range(Xf[,1]),ylim=range(X[,1])-0.5)
  
    #coord_fixed()
  #dev.off()
  
}

# Regeneration of sector2. 
# Date: 28 July, 2021
{
  source('toy_data.R')
  source('functionsV3.R')
  X <- toy.segment1(theta = 2/3*pi*3/2, 1000)
  X <- X+rnorm(nrow(X)*2, sd = 0.025)
  print(Sys.time())
  endv_sect2 <- Endness_allpoints_paral(X,knei = 10,L = 1,FIG = F)
  print(Sys.time())
  
  Xf <- data.frame(x = X[,1], y = X[,2], endv=endv_sect2)
  library(ggplot2)
  ggplot(Xf,aes(x,y))+geom_point()+coord_fixed()
  

  library(ggplot2)
  #tiff(filename = 'sector2.tif',units = 'in', width = 4,height = 3, res = 300)
  ggplot(Xf, aes(x,y))+
    geom_point(aes(colour = endv))+
    #scale_color_gradientn( colours = c( "lightgreen","yellow","red"),limits = c(0,1) )+
    scale_color_gradientn( colours = c( "green",'yellow',"red"),limits = c(0,1) )+
    coord_cartesian(xlim = range(X[,1]), ylim = range(X[,1])+0.5)+
    #coord_fixed()+
    theme_classic()
  #dev.off()
}


# Regeneration of sector3. 
# Date: 28 July, 2021
{
  source('toy_data.R')
  source('functionsV3.R')
  X <- toy.segment2(theta = 4/3*pi, 1000)
  print(Sys.time())
  endv_sect3 <- Endness_allpoints_paral(X,knei = 10,L = 1,FIG = F)
  print(Sys.time())
  
  
  Xf <- data.frame(x = X[,1], y = X[,2], endv=endv_sect3)
  
  range(endv_sect3)
  # Fixed  continuous colour values in ggplot2, referred
  # https://stackoverflow.com/questions/21537782/how-to-set-fixed-continuous-colour-values-in-ggplot2
  library(ggplot2)
  tiff(filename = 'sector3.tif',units = 'in', width = 4,height = 3, res = 300)
  library(ggplot2)
  ggplot(Xf, aes(x,y))+
    geom_point(aes(colour = endv))+
    #scale_color_gradientn( colours = c( "lightgreen","yellow","red"),limits = c(0,1) )+
    scale_color_gradientn( colours = c( "green","red"),limits = c(0,1) )+
    coord_cartesian(xlim = range(X[,1]), ylim = range(X[,1]))
  dev.off()
  
}

# Regeneration of sector4. 
# Date: 28 July, 2021
{
  source('toy_data.R')
  source('functionsV3.R')
  num = 1000
  x <- runif(num*8/(2*pi), -1, 1)
  y <- runif(num*8/(2*pi), -1, 1)
  ind <- (x^2+y^2<=1)
  X <- cbind(x[ind],y[ind])
  X <- X+rnorm(nrow(X)*2, sd = 0.025)
  print(Sys.time())
  endv_sect4 <- Endness_allpoints_paral(X,knei = 10,L = 1,FIG = F)
  print(Sys.time())
  
  Xf <- data.frame(x = X[,1], y = X[,2], endv=endv_sect4)
  
  # Fixed  continuous colour values in ggplot2, referred
  # https://stackoverflow.com/questions/21537782/how-to-set-fixed-continuous-colour-values-in-ggplot2
  #tiff(filename = 'sector4.tif',units = 'in', width = 4,height = 3, res = 300)
  library(ggplot2)
  ggplot(Xf, aes(x,y))+
    geom_point(aes(colour = endv))+
    #scale_color_gradientn( colours = c( "lightgreen","yellow","red"),limits = c(0,1) )+
    scale_color_gradientn( colours = c("green",'yellow',"red"),limits = c(0,1) )+
    coord_cartesian(xlim = range(X[,1]), ylim = range(X[,1]))+
    #coord_fixed()+
    theme_classic()
  #dev.off()
  
}

# Two 3d corssing curves
{
  x <- seq(-pi,pi,length.out = 10000)
  xs <- sample(x,500, replace = F,prob = sqrt(1+cos(x)^2))
  plot(xs, sin(xs), pch =20, asp = 1)
  
  y <- seq(-pi,pi,length.out = 10000)
  ys <- sample(y,500, replace = F,prob = sqrt(1+cos(y)^2))
  plot(ys, sin(ys), pch =20, asp = 1)
  
  Xf <- rbind( cbind(xs, 0, sin(xs)), cbind(0, ys, sin(ys)))
  Xf <- Xf+rnorm(nrow(Xf)*3,sd = 0.025)
  Xf <- as.data.frame(Xf)
  names(Xf) <- c('x','y', 'z')
  
  fig <- plot_ly(Xf, x=~x, y = ~y, z = ~z) %>%
    add_markers() %>%
    layout(scene= list( aspectmode = 'manual', aspectratio = list(x=pi, y=pi, z= 1/2)))
    
  fig
  
  endv_csin3d <- Endness_allpoints_paral(as.matrix(Xf),knei = 10,L = 1,FIG = F)
  Xf$endv <- endv_csin3d
  
  col_min = round(range(Xf$endv)[1]*9)+1
  col_max = round(range(Xf$endv)[2]*9)+1
  colv <- colorRampPalette(c('green','yellow','red'))(10)[col_min:col_max]
  fig <-plot_ly(Xf, x=~x, y=~y, z= ~z, color = ~endv, colors = colv, size = 1, sizes = 36*1.5,alpha =0.5) %>%
    add_markers() %>%
    layout( scene = list(aspectmode = "manual", aspectratio = list(x=2*pi, y=2*pi, z= 2)) ) # sqrt(3)/3
  fig
  #orca(p = fig, file = 'hat_sphere', format = 'pdf', width = 4, height = 3)
  
  
}
# 3D caps of spheres
{
  # phi <- runif(1000)*2*pi
  # theta <- runif(1000)*2*pi
  # 
  # x <- rnorm(1000)
  # y <- sin(theta)*sin(phi)
  # z <- cos(theta)
  # 
  # Xf <- data.frame(x,y,z)
  # library(plotly)
  # fig <-plot_ly(Xf, x=~x, y=~y, z= ~z, color = ~z, colors = c('#BF382A'),size = 1, sizes = 16, alpha =0.5) %>%
  #   add_markers()
  # fig
  x <- rnorm(1000*2*4*2)
  y <- rnorm(1000*2*4*2)
  z <- rnorm(1000*2*4*2)
  
  r <- sqrt(x^2+y^2+z^2)
  
  Xf <- data.frame(x = x/r, y = y/r, z= z/r)
  library(plotly)
  fig <-plot_ly(Xf, x=~x, y=~y, z= ~z, color = ~z, colors = c('#BF382A'),size = 1, sizes = 16, alpha =0.5) %>%
    add_markers()
  fig
  
  theta <- pi/3
  exp_id <- sqrt(Xf$x^2 +Xf$y^2)<= tan(theta/2)*Xf$z
  Xf <- Xf[exp_id,]
  fig <-plot_ly(Xf, x=~x, y=~y, z= ~z, color = ~z, colors = c('#BF382A'),size = 1, sizes = 16, alpha =0.5) %>%
    add_markers() %>%
    layout( scene = list(aspectmode = "manual", aspectratio = list(x=1, y=1, z=1-sqrt(3)/2)) )#range = c(-1,1)))
  fig
  
  endv_hatsph <- Endness_allpoints_paral(as.matrix(Xf),knei = 10,L = 1,FIG = F)
  
  col_min = round(range(endv_hatsph)[1]*9)+1
  col_max = round(range(endv_hatsph)[2]*9)+1
  colv <- colorRampPalette(c('green','yellow','red'))(10)[col_min:col_max]
  
  Xf$endv <- endv_hatsph
  fig <-plot_ly(Xf, x=~x, y=~y, z= ~z, color = ~endv, colors = colv, size = 1, sizes = 36*1.5,alpha =0.5) %>%
    add_markers() %>%
    layout( scene = list(aspectmode = "manual", aspectratio = list(x=1, y=1, z=1-sqrt(3)/2)) )#range = c(-1,1)))
  fig
  #orca(p = fig, file = 'hat_sphere', format = 'pdf', width = 4, height = 3)
  
  
  ## phat of sphere
  x <- rnorm(1000*2*1/2)
  y <- rnorm(1000*2*1/2)
  z <- rnorm(1000*2*1/2)
  
  r <- sqrt(x^2+y^2+z^2)
  
  Xf <- data.frame(x = x/r, y = y/r, z= z/r)
  
  #theta <- pi/6; exp_id <- x>=z*tan(theta) & (z>=0) # pi/3 of sphere 
  exp_id <- (z>=0)        # Half sphere
  exp_id <- 1:nrow(Xf)    # ALL sphere
  
  Xf <- Xf[exp_id,]
  endv_phatsph <- Endness_allpoints_paral(as.matrix(Xf),knei = 10,L = 1,FIG = F)
  Xf$endv <- endv_phatsph
  
  
  col_min = round(range(endv_phatsph)[1]*9)+1
  col_max = round(range(endv_phatsph)[2]*9)+1
  colv <- colorRampPalette(c('green','yellow','red'))(10)[col_min:col_max]
  fig <-plot_ly(Xf, x=~x, y=~y, z= ~z, color = ~endv, colors = colv, size = 1, sizes = 36*1.5,alpha =0.5) %>%
    add_markers() %>%
    layout( scene = list(aspectmode = "manual", aspectratio = list(x=2, y=2, z= 2)) ) # sqrt(3)/3
  fig
  #orca(p = fig, file = 'hat_sphere', format = 'pdf', width = 4, height = 3)
  
  
}
#3-D cones
{
  x <- runif(1000*2*1,-1,1)
  y <- runif(1000*2*1,-1,1)
  z <- runif(1000*2*1,-1,1)
  
  r <- sqrt(x^2+y^2+z^2)
  # theta <- pi/3 ; exp_id <- (r<=1) & ( sqrt(x^2+y^2)<= z*tan(theta/2) ) # pi/3 cone
  # exp_id <- (r<=1) & z>=0 # pi cone
  exp_id <-(r<=1) &  (z>=-1)
  
  Xf <- data.frame(x = x, y = y, z= z)
  Xf <- Xf[exp_id,]
  
  endv_cone <- Endness_allpoints_paral(as.matrix(Xf),knei = 10,L = 1,FIG = F)
  Xf$endv <- endv_cone
  
  col_min = round(range(Xf$endv)[1]*9)+1
  col_max = round(range(Xf$endv)[2]*9)+1
  colv <- colorRampPalette(c('green','yellow','red'))(10)[col_min:col_max]
  fig <-plot_ly(Xf, x=~x, y=~y, z= ~z, color = ~endv, colors = colv, size = 1, sizes = 36*2,alpha =0.5) %>%
    add_markers() %>%
    layout( scene = list(aspectmode = "manual", aspectratio = list(x=2, y=2, z= 2)) ) # sqrt(3)/3
  fig
  #orca(p = fig, file = 'hat_sphere', format = 'pdf', width = 4, height = 3)


}

# mesoderm development with UMAP 18-aug-2021
{
  # Load data
  mesoderm.raw <- readRDS("../dynverse/mesoderm-development_loh.rds")
  expr_mat <- ((mesoderm.raw$expression))
  str(expr_mat)
  
  # pca and elbow fixing
  expr_pca=prcomp(expr_mat,scale.  = F, retx = T)
  library(PCAtools)
  npc <- findElbowPoint(expr_pca$sdev^2)
  X <- expr_pca$x[,1:npc]
  
  endpoints <- unlist(mesoderm.raw$prior_information[c('start_id','end_id')])
  waypoints <- mesoderm.raw$waypoint_cells
  plot(expr_pca$x[,1], -expr_pca$x[,2],pch=20, asp = 1)
  points(expr_pca$x[waypoints,1], -expr_pca$x[waypoints,2], pch = 20, col = 'blue')
  #points(expr_pca$x[endpoints$start_id,1], -expr_pca$x[endpoints$start_id,2], pch = 20, col = 'red')
  #points(expr_pca$x[endpoints$end_id,1], -expr_pca$x[endpoints$end_id,2], pch = 20, col = 'red')
  
  # UMAP projection
  library(umap)
  library(reticulate)
  Xumap = umap(X, method="umap-learn", densemap = T)$layout
  plot(Xumap[,1], Xumap[,2],pch = 20, asp = 1)
  points(Xumap[endpoints,1], Xumap[endpoints,2], pch = 20, col = 'red')
  points(Xumap[waypoints,1], Xumap[waypoints,2], pch = 20, col = 'blue')

}

# Mesoderm development with PCA - 19-Oct 2021
{
  # Load data
  mesoderm.raw <- readRDS("../dynverse/mesoderm-development_loh.rds")
  # Structure of the raw data 
  names(mesoderm.raw)
  expr_mat <- ((mesoderm.raw$expression))
  str(expr_mat)
  
  # pca and elbow fixing
  expr_pca=prcomp(expr_mat,scale.  = F, retx = T)
  library(PCAtools)
  npc <- findElbowPoint(expr_pca$sdev^2)
  X <- expr_pca$x[,1:npc]
  X[,2] <- -X[,2]
  
  plot(expr_pca$x[,1], -expr_pca$x[,2],pch=20, asp = 1)
  source('./functionsV3.R')
  endv_mesoderm <- Endness_allpoints_paral(X[,1:2], L = 1)
  
  # Plot the graph constructed on the single cell data set.
  # distance matrix for the points, symmetric matrix
  knei=10; L = 1; FIG = F
  distMat <- base::as.matrix(dist(X[,1:2], method="euclidean")) 
  knnNei <- knnFinder(distMat,round(knei)) # find knn for each of the point
  library(igraph)
  G <- graph_from_data_frame(knnNei, directed = F)
  simpG <- simplify(G, remove.loops = FALSE, edge.attr.comb = "mean")
  distAll = distances(simpG, weights = NULL) #The distance beteen each node pair in the graph
  plot.igraph(simpG, axes = FALSE
              , vertex.label = NA
              , vertex.size = 3
              , layout = X[,1:2]/100
              , rescale = F
              #, ylim = c(-1,1)*50
              #, xlim = c(-2,2)*50
              , asp = T
              , edge.width = 3)
  
  Xf <- data.frame(x = X[,1], y = X[,2], endv = endv_mesoderm)
  col_min = round(range(Xf$endv)[1]*9)+1
  col_max = round(range(Xf$endv)[2]*9)+1
  colv <- colorRampPalette(c('green','yellow','red'))(10)[col_min:col_max]
  fig <-plot_ly(Xf, x=~x, y=~y, color = ~endv, colors = colv, size = 1, sizes = 36*1.5,alpha =1) %>%
    add_markers() %>%
    layout( scene = list(aspectmode = 'mannual', aspectratio = list(x=1, y=1)) ) # sqrt(3)/3
  fig
  
  # Plot the distribution of Endness in the single cell dataset.
  library(ggplot2)
  #tiff(filename = 'line.tif',units = 'in', width = 4,height = 3, res = 300)
  ggplot(Xf, aes(x,y))+
    geom_point(aes(colour = endv), size = 3)+
    #scale_color_gradientn( colours = c( "lightgreen","yellow","red"),limits = c(0,1) )+
    scale_color_gradientn( colours = c( "green",'yellow',"red"),limits = c(0,1) )+
    #coord_cartesian(xlim = range(Xf[,1]),y=range(X[,1])-0.5)+
    coord_fixed(ratio = 1)+
    theme_classic()#coord_fixed()
  
  ggplot(data = Xf)+
    geom_boxplot(aes(y = endv, ymin = 0, ymax = 1.0))+
    #geom_jitter(aes(x = y, y=x))+
    theme_classic()
  
  #dev.off()
  # Plot the single cell data set on a 2 dimensional PCA plane
  ggplot(Xf, aes(x,y))+
    geom_point(size = 3)+
    coord_fixed(ratio = 1)+
    theme_classic()#coord_fixed()
  #dev.off()
  
  plot(X[,1],X[,2], col = 'white',asp = 1)
  text(X[,1],X[,2], labels = 1:nrow(X),cex = 0.5)
  # topmin <- order(endv_mesoderm, decreasing = FALSE)[1:80]
  # text(X[topmin,1],X[topmin,2], labels = topmin, col = 'red', cex = 0.5)
  
  selectpoints <- c(83, 176, 302, 251, 303, 247) # change E from 316 to 303, F from 450 to 247
  text(X[selectpoints,1],X[selectpoints,2], labels = selectpoints, col = 'red', cex = 0.5)
  
  curveA <- Endness_endnesscurve(x0 = 83, X[,1:2], L = 1,FIG = F) # A 83, B 176 , C 302,  D 251, E 316, F 450 
  curveB <- Endness_endnesscurve(x0 = 176, X[,1:2], L = 1,FIG = F) # A 83, B 176 , C 302,  D 251, E 316, F 450  
  curveC <- Endness_endnesscurve(x0 = 302, X[,1:2], L = 1,FIG = F) # A 83, B 176 , C 302,  D 251, E 316, F 450 
  curveD <- Endness_endnesscurve(x0 = 251, X[,1:2], L = 1,FIG = F) # A 83, B 176 , C 302,  D 251, E 316, F 450 
  curveE <- Endness_endnesscurve(x0 = 303, X[,1:2], L = 1,FIG = F) # A 83, B 176 , C 302,  D 251, E 316, F 450 
  curveF <- Endness_endnesscurve(x0 = 247, X[,1:2], L = 1,FIG = F) # A 83, B 176 , C 302,  D 251, E 316, F 450 
  
  library(RColorBrewer)
  #all palette available from RColorBrewer
  #display.brewer.all()
  #we will select the first 4 colors in the Set1 palette
  linecol <-brewer.pal(n=6,name="Set1")
  plot(curveA[,1], curveA[,2 ],type = 'l', col = linecol[1], xlim = c(0,1), ylim = c(1,2), lwd = 3, xlab = 'l', ylab = 'R(l)',asp = 1)
  lines(curveB[,1], curveB[,2 ],type = 'l', col = linecol[2], lwd = 3)
  lines(curveC[,1], curveC[,2 ],type = 'l', col = linecol[3], lwd = 3)
  lines(curveD[,1], curveD[,2 ],type = 'l', col = linecol[4], lwd = 3)
  lines(curveE[,1], curveE[,2 ],type = 'l', col = linecol[5], lwd = 3)
  lines(curveF[,1], curveF[,2 ],type = 'l', col = linecol[6], lwd = 3)
  legend('topright',legend = c('A', 'B', 'C', 'D', 'E','F'),inset = .05, lty = 1,lwd = 3,col = linecol)
  
  
  # Infer the developmental directions
  X <- Xf[,1:2]
  colv <- colorRampPalette(c("green",'yellow','red'))(20)
  start.point <- which.max(Xf$endv)
  endv <- distAll[start.point,]/max(distAll[start.point,])
  colvv <- colv[20-round(endv*(20-1))+1]
  
  plot(X[,1], X[,2],pch=20, asp = 1, col = colvv, axes = F)
  xcut <- seq(range(X[,1])[1], range(X[,1])[2], length.out = 20)
  ycut <- seq(range(X[,2])[1],range(X[,2])[2],length.out = length(xcut))
  ncuts <- length(xcut)
  endv.bin <- vector(mode = 'numeric', length = (ncuts-1)^2)
  center.bin <- matrix(rep(endv.bin,2), ncol = 2, nrow = (ncuts-1)^2,byrow = F
                       , dimnames = list(NULL,c('x','y')))
  # center.bin <- vector(mode = 'numeric', length = (ncuts-1)^2)
  # grad.bin <- vector(mode = 'numeric', length = (ncuts-1)^2)
  len.diag.bin <- sqrt( (xcut[2]-xcut[1])^2+ (ycut[2]-ycut[1])^2)
  
  for(ii in 1:(ncuts-1)){
    for(jj in 1:(ncuts-1)){
      x0 <- xcut[ii]
      x1 <- xcut[ii+1]
      y0 <- ycut[jj]
      y1 <- ycut[jj+1]
      point.id.bin <- which ( (X[,1]>x0)& (X[,1]<=x1)& (X[,2]>y0)& (X[,2]<=y1))
      
      endv.bin[(ii-1)*(ncuts-1)+jj] <- mean(endv[point.id.bin])
      center.bin[(ii-1)*(ncuts-1)+jj,] <- apply(X[point.id.bin, ,drop = FALSE],2,mean)
      # arrows(x0,y0,x1,y1,length = 0.05)
    }
  } 
  
  for(ii in 1:(ncuts-1)){
    for(jj in 1:(ncuts-1)){
      x0 <- xcut[ii]
      x1 <- xcut[ii+1]
      y0 <- ycut[jj]
      y1 <- ycut[jj+1]
      xcenter <- center.bin[(ii-1)*(ncuts-1)+jj,1] #0.5*(x0+x1)
      ycenter <- center.bin[(ii-1)*(ncuts-1)+jj,2] #0.5*(y0+y1)
      #points(xcenter,ycenter,pch = 20, col = 'black')
      
      id0 <- (ii-1)*(ncuts-1)+jj
      
      idx1 <- (min(ii+1,ncuts-1) - 1)*(ncuts-1)+jj
      idxm1 <- (max(ii-1,1)-1)*(ncuts-1)+jj
      
      idy1 <- (ii-1)*(ncuts-1)+min(jj+1,ncuts-1)
      idym1  <- (ii-1)*(ncuts-1)+max(jj-1,1)
      
      fx1 <- endv.bin[idx1]-endv.bin[id0]
      fxm1 <- endv.bin[id0]-endv.bin[idxm1]
      fy1 <- endv.bin[idy1]-endv.bin[id0]
      fym1 <- endv.bin[id0]-endv.bin[idym1]
      
      fxfinal <- mean(c(fx1,fxm1),na.rm = T)  
      fyfinal <- mean(c(fy1,fym1),na.rm = T)
      
      if(is.nan(fxfinal)) 
        fxfinal <- 0
      if(is.nan(fyfinal)) 
        fyfinal <- 0
      
      
      grad.bin <- (1)*apply(cbind(fxfinal*c(1,0),fyfinal*c(0,1)),1,sum) 
      len.grad.bin <- sqrt(sum(grad.bin^2))*300
      
      if(!is.na(endv.bin[(ii-1)*(ncuts-1)+jj])){
        len.arrow <- 0.5*len.grad.bin #len.diag.bin
        ori.arrow <- as.numeric( scale(grad.bin, center = F, scale = T))
        
        arrows(xcenter,ycenter,xcenter+ori.arrow[1]*len.arrow, ycenter+ori.arrow[2]*len.arrow
               ,lwd = 2,length = 0.05,col = 'black')
      }
      
    }
  }
}


# Aging hsc with PCA 19-Oct 2021
{
  # Load data
  aging_hsc.raw <- readRDS("../dynverse//aging-hsc-old_kowalczyk.rds")
  # Structure of the raw data 
  names(aging_hsc.raw)
  expr_mat <- ((aging_hsc.raw$expression))
  str(expr_mat)
  
  # pca and elbow fixing
  expr_pca=prcomp(expr_mat,scale.  = F, retx = T)
  library(PCAtools)
  npc <- findElbowPoint(expr_pca$sdev^2)
  X <- expr_pca$x[,1:npc]
  
  plot(expr_pca$x[,1], expr_pca$x[,2],pch=20, asp = 1)
  source('./functionsV3.R')
  endv_aging_hsc <- Endness_allpoints_paral(X[,1:2], L = 1)
  
  # Plot the graph constructed on the single cell data set.
  # distance matrix for the points, symmetric matrix
  knei=10; L = 1; FIG = F
  distMat <- base::as.matrix(dist(X[,1:2], method="euclidean")) 
  knnNei <- knnFinder(distMat,round(knei)) # find knn for each of the point
  library(igraph)
  G <- graph_from_data_frame(knnNei, directed = F)
  simpG <- simplify(G, remove.loops = FALSE, edge.attr.comb = "mean")
  distAll = distances(simpG, weights = NULL) #The distance beteen each node pair in the graph
  plot.igraph(simpG, axes = FALSE
              , vertex.label = NA
              , vertex.size = 3
              , layout = X[,1:2]/100
              , rescale = F
              #, ylim = c(-1,1)*50
              , xlim = c(-1.5,2.0)
              , asp = T
              , edge.width = 3)
  
  
  Xf <- data.frame(x = X[,1], y = X[,2], endv = endv_aging_hsc)
  col_min = round(range(Xf$endv)[1]*9)+1
  col_max = round(range(Xf$endv)[2]*9)+1
  colv <- colorRampPalette(c('green','yellow','red'))(10)[col_min:col_max]
  library(plotly)
  fig <-plot_ly(Xf, x=~x, y=~y, color = ~endv, colors = colv, size = 1, sizes = 36*1.5,alpha =1) %>%
    add_markers() %>%
    layout(scene = list(aspectmode = "manual", aspectratio = list(x=1, y=1))) # sqrt(3)/3
  fig
  
  # Plot the distribution of Endness in the single cell dataset.
  library(ggplot2)
  #tiff(filename = 'line.tif',units = 'in', width = 4,height = 3, res = 300)
  ggplot(Xf, aes(x,y))+
    geom_point(aes(colour = endv), size = 3)+
    #scale_color_gradientn( colours = c( "lightgreen","yellow","red"),limits = c(0,1) )+
    scale_color_gradientn( colours = c( "green",'yellow',"red"),limits = c(0,1) )+
    #coord_cartesian(xlim = range(Xf[,1]),y=range(X[,1])-0.5)+
    coord_fixed(ratio = 1)+
    theme_classic()#coord_fixed()
  
  ggplot(data = Xf)+
    geom_boxplot(aes(y = endv))+
    #geom_jitter(aes(x = y, y=x))+
    theme_classic()
  
  #dev.off()
  # Plot the single cell data set on a 2 dimensional PCA plane
  ggplot(Xf, aes(x,y))+
    geom_point(size = 3)+
    coord_fixed(ratio = 1)+
    theme_classic()#coord_fixed()
  #dev.off()
  
  plot(X[,1],X[,2], col = 'white',asp = 1)
  text(X[,1], X[,2], labels = 1:nrow(X),cex = 1)
  #text(X[249,1], X[249,2], labels = '249', col = 'red')
  curveA <- Endness_endnesscurve(x0 = 87, X[,1:2], L = 1,FIG = T) # B 432 , C 206,  D 249
  curveB <- Endness_endnesscurve(x0 = 432, X[,1:2], L = 1,FIG = T) # B 432 , C 206,  D 249
  curveC <- Endness_endnesscurve(x0 = 206, X[,1:2], L = 1,FIG = T) # B 432 , C 206,  D 249
  curveD <- Endness_endnesscurve(x0 = 249, X[,1:2], L = 1,FIG = T) # B 432 , C 206,  D 249
  
  # Curve of Rx(l) on different points 
  library(RColorBrewer)
  #all palette available from RColorBrewer
  display.brewer.all()
  #we will select the first 4 colors in the Set1 palette
  linecol <-brewer.pal(n=4,name="Set1")
  plot(curveA[,1], curveA[,2 ],type = 'l', col = linecol[1], xlim = c(0,1)
       , ylim = c(1,2), lwd = 3, xlab = 'l', ylab = 'R(l)',asp =1)
  lines(curveB[,1], curveB[,2 ],type = 'l', col = linecol[2], lwd = 3)
  lines(curveC[,1], curveC[,2 ],type = 'l', col = linecol[3], lwd = 3)
  lines(curveD[,1], curveD[,2 ],type = 'l', col = linecol[4], lwd = 3)
  legend('topright',inset = .05,legend = c('A', 'B', 'C', 'D'),lty = 1,lwd = 3,col = linecol)
  

  # Infer the developmental directions
  X <- Xf[,1:2]
  colv <- colorRampPalette(c("green",'yellow','red'))(20)
  start.point <- which.max(Xf$endv)
  endv <- distAll[start.point,]/max(distAll[start.point,])
  colvv <- colv[20-round(endv*(20-1))+1]
  
  plot(X[,1], X[,2],pch=20, asp = 1, col = colvv, axes = F)
  xcut <- seq(range(X[,1])[1], range(X[,1])[2], length.out = 20)
  ycut <- seq(range(X[,2])[1],range(X[,2])[2],length.out = length(xcut))
  ncuts <- length(xcut)
  endv.bin <- vector(mode = 'numeric', length = (ncuts-1)^2)
  center.bin <- matrix(rep(endv.bin,2), ncol = 2, nrow = (ncuts-1)^2,byrow = F
                       , dimnames = list(NULL,c('x','y')))
  # center.bin <- vector(mode = 'numeric', length = (ncuts-1)^2)
  # grad.bin <- vector(mode = 'numeric', length = (ncuts-1)^2)
  len.diag.bin <- sqrt( (xcut[2]-xcut[1])^2+ (ycut[2]-ycut[1])^2)
  
  for(ii in 1:(ncuts-1)){
    for(jj in 1:(ncuts-1)){
      x0 <- xcut[ii]
      x1 <- xcut[ii+1]
      y0 <- ycut[jj]
      y1 <- ycut[jj+1]
      point.id.bin <- which ( (X[,1]>x0)& (X[,1]<=x1)& (X[,2]>y0)& (X[,2]<=y1))
      
      endv.bin[(ii-1)*(ncuts-1)+jj] <- mean(endv[point.id.bin])
      center.bin[(ii-1)*(ncuts-1)+jj,] <- apply(X[point.id.bin, ,drop = FALSE],2,mean)
      # arrows(x0,y0,x1,y1,length = 0.05)
    }
  } 
  
  for(ii in 1:(ncuts-1)){
    for(jj in 1:(ncuts-1)){
      x0 <- xcut[ii]
      x1 <- xcut[ii+1]
      y0 <- ycut[jj]
      y1 <- ycut[jj+1]
      xcenter <- center.bin[(ii-1)*(ncuts-1)+jj,1] #0.5*(x0+x1)
      ycenter <- center.bin[(ii-1)*(ncuts-1)+jj,2] #0.5*(y0+y1)
      #points(xcenter,ycenter,pch = 20, col = 'black')
      
      id0 <- (ii-1)*(ncuts-1)+jj
      
      idx1 <- (min(ii+1,ncuts-1) - 1)*(ncuts-1)+jj
      idxm1 <- (max(ii-1,1)-1)*(ncuts-1)+jj
      
      idy1 <- (ii-1)*(ncuts-1)+min(jj+1,ncuts-1)
      idym1  <- (ii-1)*(ncuts-1)+max(jj-1,1)
      
      fx1 <- endv.bin[idx1]-endv.bin[id0]
      fxm1 <- endv.bin[id0]-endv.bin[idxm1]
      fy1 <- endv.bin[idy1]-endv.bin[id0]
      fym1 <- endv.bin[id0]-endv.bin[idym1]
      
      fxfinal <- mean(c(fx1,fxm1),na.rm = T)  
      fyfinal <- mean(c(fy1,fym1),na.rm = T)
      
      if(is.nan(fxfinal)) 
        fxfinal <- 0
      if(is.nan(fyfinal)) 
        fyfinal <- 0
      
      
      grad.bin <- (1)*apply(cbind(fxfinal*c(1,0),fyfinal*c(0,1)),1,sum) 
      len.grad.bin <- sqrt(sum(grad.bin^2))*200
      
      if(!is.na(endv.bin[(ii-1)*(ncuts-1)+jj])){
        len.arrow <- 0.5*len.grad.bin #len.diag.bin
        ori.arrow <- as.numeric( scale(grad.bin, center = F, scale = T))
        
        arrows(xcenter,ycenter,xcenter+ori.arrow[1]*len.arrow, ycenter+ori.arrow[2]*len.arrow
               ,lwd = 2,length = 0.05,col = 'black')
      }
      
    }
  }
} 

