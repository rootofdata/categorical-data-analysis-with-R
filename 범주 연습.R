#각 행과 열의 합을 구하는 함수(생각1)
KCH.sumX <- function(x)  # x : 범주형 자료의 행렬
{
  r <- nrow(x);
  c <- ncol(x);
  sum_x <- matrix(c(0), nrow =(r+1),ncol=(c+1));
  for(i in 1:r)
  {
    sum_x[i,(c+1)] <- sum(x[i,]);
  }
  
  
  for(j in 1:c)
  {
    sum_x[(r+1),j] <- sum(x[,j]);
  }
  
  sum_x[(r+1),(c+1)] <- sum(sum_x[(r+1),]);
  
  return (sum_x);
}
#기대도수를 구하는 함수(생각2)
KCH.expectation <- function(sumx)   #sumx : 각 열의 합을 저장한 행렬
{
  r <- nrow(sumx);
  c <- ncol(sumx);
  
  #기대도수만 있는 행렬을 만들기 위해서
  expec <- matrix(c(-1), nrow =(r-1),ncol=(c-1));
  
  
  for(i in 1:(r-1))
  {
    for(j in 1:(c-1))
    {
      expec[i,j] <- (sumx[i,c]*sumx[r,j])/sumx[r,c] ;
    }
  }
  
  return (expec);
}
#x²를 구하는 함수
KCH.X2 <- function(x,ux)   # x : 범주형 자료의 행렬 , ux : 기대도수
{
  x2 <- sum((x - ux)^2/ux);
  return (x2);
}

#G²를 구하는 함수
KCH.G2 <- function(x,ux)   # x : 범주형 자료의 행렬 , ux : 기대도수
{
  g2 <- 2*sum(x*log(x/ux));
  return (g2);
}
#x²를 출력하는 함수
KCH.X2print <- function(x)   # x : 범주형 자료의 행렬
{
  
  sx <- KCH.sumX(x)
  ux <- KCH.expectation(sx)
  
  cat("x²값을 출력 합니다.\n");
  cat("============================================================\n\n"); 
  cat("교차 분할표\n");
  print(x);
  cat("‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥\n\n"); 
  cat("기대 도수\n");
  print(ux);
  cat("‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥\n\n"); 
  cat("x² 값 : ",KCH.X2(x,ux),"\n");
  cat("============================================================\n");
  
}
#G²를 출력하는 함수
KCH.G2print <- function(x)   # x : 범주형 자료의 행렬
{
  
  sx <- KCH.sumX(x)
  ux <- KCH.expectation(sx)
  
  cat("G²값을 출력 합니다.\n");
  cat("============================================================\n\n"); 
  cat("교차 분할표\n");
  print(x);
  cat("‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥\n\n"); 
  cat("기대 도수\n");
  print(ux);
  cat("‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥‥\n\n"); 
  cat("G² 값 : ",KCH.G2(x,ux),"\n");
  cat("============================================================\n");
  
}
A = array(c(871, 302, 444, 80, 873, 43), dim = c(2, 3), 
          dimnames = list(Race = c("w", "b"), Party = c("Dem", "Indep", "Rep")))
A
mosaicplot(t(A), shade = TRUE, main = 'Party Identification by Race')
KCH.G2print(A)
KCH.X2print(A)

#TABLE 2.5
A = array(c(762, 482, 327, 239, 468, 477), dim = c(2, 3), 
          dimnames = list(Race = c("w", "b"), Party = c("Dem", "Indep", "Rep")))
A
mosaicplot(t(A), shade = TRUE, main = 'Party Identification by Race')
KCH.G2print(A)
KCH.X2print(A)
