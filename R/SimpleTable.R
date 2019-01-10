#simple_table
SimpleTable <- function(x1,x2){

  f1 <- factor(x1)
  name1 <- levels(f1)
  f2 <- factor(x2)
  name2 <- levels(f2)


  dims = c(length(name1),length(name2))
  dimname = list(name1,name2)

  y<-array(0,dims,dimname)

  n1=length(f1)

  for (i in 1:n1) {
    y[f1[i],f2[i]] = y[f1[i],f2[i]] + 1
  }
  y
}
