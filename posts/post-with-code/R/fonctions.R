
#' milieu d'un segment
#'
#' @param a la mesure d'un côte du triangle
#' @param b la mesure d'un côte du triangle
#'
#' @return le milieu d'un segment 
#' 
#'
#' @examples
divide_milieu_segment=function(a,b){
        return((a+b)/2)
}

#' diviser un triangle en petit
#'
#' @param xa l'abscisse du premier point
#' @param ya l'ordoner du premier point
#' @param xb l'abscisse du deuxième un point
#' @param yb l'ordonner du deuxième point 
#' @param xc l'abscisse du troisième point 
#' @param yc l'ordonnee du troisième point
#'
#' @return cette fonction retourne une liste de triangle issue de la prémière ittération  Sierpiński
#' @export
#'
#' @examples
divide_triangle=function(xa,ya,xb,yb,xc,yc){
    xE=divide_milieu_segment(xa,xb);yE=divide_milieu_segment(ya,yb)
    xF=divide_milieu_segment(xb,xc);yF=divide_milieu_segment(yb,yc)
    xG=divide_milieu_segment(xa,xc);yG=divide_milieu_segment(ya,yc)
    #triangle_inte=c(xE,yE,xF,yF,xG,yG)
    triangle2=c(xa,ya,xE,yE,xG,yG)
    triangle3=c(xE,yE,xb,yb,xF,yF)
    triangle4=c(xc,yc,xF,yF,xG,yG)
    liste=list(triangle2,triangle3,triangle4)
    return(liste)
}

#' divide liste des triangle
#'
#' @param liste l'entrer est une liste de triangle issue de la première ittération de  Sierpiński
#'
#' @return retourne une autre liste de triangle après avoir appliquer la fonction divide triangle
#' 
#'

divide_list_triangle=function(liste){
  l=list(1:length(liste))
  total_air=0
  for (i in 1:length(liste)){
        xa=liste[[i]][1];ya=liste[[i]][2];xb=liste[[i]][3];yb=liste[[i]][4];xc=liste[[i]][5];yc=liste[[i]][6]
        l[[i]]=divide_triangle(xa,ya,xb,yb,xc,yc)
        print(plot_triangle(xa,ya,xb,yb,xc,yc))
       total_air=total_air +air_total_tiangle(xa,ya,xb,yb,xc,yc)
  }
  print(total_air)
  print(l)
  #liste=list(l,total_air)
  #return(liste)
} 

#' Plot
#'
#' @param xa 
#' @param ya 
#' @param xb 
#' @param yb 
#' @param xc 
#' @param yc 
#'
#' @return retourne le graphique de la première ittération de  Sierpiński
#' @export
#'
#' @examples
plot_triangle=function(xa,ya,xb,yb,xc,yc){
  
  corner.points = data.frame(x=c(xa, xb, xc),
                             y=c(ya, yb, yc))
  liste=divide_triangle(xa,ya,xb,yb,xc,yc)
  
  new.points = data.frame(x=c(liste[[1]][1],liste[[1]][3],liste[[1]][5]),
                          y=c(liste[[1]][2],liste[[1]][4],liste[[1]][6]))
  table1 <-as.data.frame(setNames(replicate(2,numeric(0), simplify = F),c("x","y") ))
  for( i in 2:3){
    table1[1,1]=liste[[i]][1]
    table1[1,2]=liste[[i]][2]
    table1[2,1]=liste[[i]][3]
    table1[2,2]=liste[[i]][4]
    table1[3,1]=liste[[i]][5]
    table1[3,2]=liste[[i]][6]
    new.points=rbind(new.points,table1)
  }
  #plot sierpinski
  plot.new()
  legend("left",legend=c("iter.1"))
  my_return=list(polygon(corner.points) ,polygon(new.points[1:3,],col = "black"),
                 polygon(new.points[4:6,],col = "black"),polygon(new.points[7:9,],col = "black"))
  #return(polygon(corner.points))
  #return(polygon(new.points[1:3,],col = "black"))
  #return( polygon(new.points[4:6,],col = "black"))
  #return(polygon(new.points[7:9,],col = "black"))
  return(my_return)
}


#' air 
#'
#' @param xa 
#' @param ya 
#' @param xb 
#' @param yb 
#' @param xc 
#' @param yc 
#'
#' @return l'air d'un triangle après avoir calculer la meseure de chaque côte d'un traingle à l'aide de ses coordonner
#' @export
#'
#' @examples
air_total_tiangle=function(xa,ya,xb,yb,xc,yc){
    AB=sqrt((xb-xa)^2+(yb-ya)^2)
    BC=sqrt((xc-xb)^2+(yc-yb)^2)
    AC=sqrt((xc-xa)^2+(yc-ya)^2)
    return(air_triangle(AB,BC,AC))
}

#air_total_tiangle(1,0,1,1,0.5,1)


#plot_triangle(1,0,1,1,0.5,1)

#liste=divide_triangle(1,0,1,1,0.5,1)

#divide_list_triangle(liste)


#Z=divide_triangle(1 ,0 ,1 ,1, 0.5,1)
