# Function to compute species contribution to temporal beta diversity (SCTBD)

SCTBD <- function(mat1, mat2, pa.tr=F)
{
  #note : production de NA
  bcd.stat = data.frame()
  
  SCTBD.a = SCTBD.b = SCTBD.c = SCTBD.a.den = SCTBD.b.den = SCTBD.c.den = data.frame()
  
  
  if(pa.tr) { mat1 <- ifelse(mat1>0, 1, 0) ; mat2 <- ifelse(mat2>0, 1, 0) }
  
  
  # Sum of all values by rows to get 2A+B+C
  # den.tmp <- apply(cbind(mat1, mat2), 1, sum) 
  
  # Compute the reference values of statistics D, B and C
  tmp = mat1 - mat2
  
  # bcd.stat = cbind("B" = B.tmp, "C" = C.tmp, "D" = D.tmp, "den" = den.tmp)
  
  # Species contribution                                      
  b = c = tmp
  # b = loss
  b[b<0] = 0
  # c = gain
  c[c>0] = 0 ; c = abs(c)
  # a = common
  mat.diff <- mat1<=mat2
  a <- as.matrix(mat2)
  a[mat.diff] <- as.matrix(mat1)[mat.diff]
  
  # Sum of the species losses between T1 and T2
  B.tmp = apply(b,1,sum)       
  # Sum of the species gains between T1 and T2
  C.tmp = apply(c,1,sum)
  # Dissimilarity
  D.tmp = B.tmp + C.tmp
  # Common
  A.tmp = apply(a,1,sum)

  # Denominator = A+B+C
  den.tmp = A.tmp + D.tmp
  
  vec.b.B= b/B.tmp
  vec.c.C = c/C.tmp 
  vec.a.A = (1-(b+c))/(1-D.tmp)
  vec.b.den = b/den.tmp 
  vec.c.den = c/den.tmp 
  vec.d.den = (b+c)/den.tmp
  vec.a.den = a/den.tmp
  
  list(SCTBD.a.raw = a, SCTBD.b.raw = b, SCTBD.c.raw = c,
       SCTBD.a=vec.a.A, SCTBD.b=vec.b.B, SCTBD.c=vec.c.C, 
       SCTBD.a.den = vec.a.den, SCTBD.b.den=vec.b.den, SCTBD.c.den=vec.c.den, SCTBD.d.den = vec.d.den)
}
     


#test=SCTBD(mat1=sp.mat1[1:20,MySpecies], mat2 = sp.mat4[1:20,MySpecies])
# Bouya!


