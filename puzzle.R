# Teile nach rechts drehen

rechts <- function(A){
  Ar <- array(0, dim=c(2,2,4))
  for(i in 1:4){
    Ar[1,2,i] <- A[1,1,i]
    Ar[1,1,i] <- A[2,1,i]
    Ar[2,2,i] <- A[1,2,i]
    Ar[2,1,i] <- A[2,2,i]
  }
  return(Ar)
}

# Teile umdrehen

drehen <- function(A){
  Ad <- array(0, dim=c(2,2,4))
  for(i in 1:2){
    Ad[i,1,1] <- A[i,2,4]
    Ad[i,2,1] <- A[i,1,4]
    Ad[i,1,2] <- A[i,2,3]
    Ad[i,2,2] <- A[i,1,3]
    Ad[i,1,3] <- A[i,2,2]
    Ad[i,2,3] <- A[i,1,2]
    Ad[i,1,4] <- A[i,2,1]
    Ad[i,2,4] <- A[i,1,1]
  }
  return(Ad)
}

# Teile definieren

Av1_1 <- array(c(0,1,0,1,0,0,0,1,0,0,1,1,1,1,1,1), dim=c(2,2,4))
Av2_1 <- array(c(0,1,0,1,0,1,0,1,1,1,1,1,0,1,0,1), dim=c(2,2,4))
Av3_1 <- array(c(0,1,0,1,0,1,0,0,0,1,0,0,0,1,0,1), dim=c(2,2,4))
Av4_1 <- array(c(0,1,0,1,0,1,0,1,0,0,0,1,0,1,0,1), dim=c(2,2,4))
Av5_1 <- array(c(0,1,1,1,0,0,1,1,0,0,0,1,0,1,0,1), dim=c(2,2,4))

Av1_2 <- rechts(Av1_1)
Av2_2 <- rechts(Av2_1)
Av3_2 <- rechts(Av3_1)
Av4_2 <- rechts(Av4_1)
Av5_2 <- rechts(Av5_1)

Av1_3 <- rechts(Av1_2)
Av2_3 <- rechts(Av2_2)
Av3_3 <- rechts(Av3_2)
Av4_3 <- rechts(Av4_2)
Av5_3 <- rechts(Av5_2)

Av1_4 <- rechts(Av1_3)
Av2_4 <- rechts(Av2_3)
Av3_4 <- rechts(Av3_3)
Av4_4 <- rechts(Av4_3)
Av5_4 <- rechts(Av5_3)

Ar1_1 <- drehen(Av1_1)
Ar2_1 <- drehen(Av2_1)
Ar3_1 <- drehen(Av3_1)
Ar4_1 <- drehen(Av4_1)
Ar5_1 <- drehen(Av5_1)

Ar1_2 <- rechts(Ar1_1)
Ar2_2 <- rechts(Ar2_1)
Ar3_2 <- rechts(Ar3_1)
Ar4_2 <- rechts(Ar4_1)
Ar5_2 <- rechts(Ar5_1)

Ar1_3 <- rechts(Ar1_2)
Ar2_3 <- rechts(Ar2_2)
Ar3_3 <- rechts(Ar3_2)
Ar4_3 <- rechts(Ar4_2)
Ar5_3 <- rechts(Ar5_2)

Ar1_4 <- rechts(Ar1_3)
Ar2_4 <- rechts(Ar2_3)
Ar3_4 <- rechts(Ar3_3)
Ar4_4 <- rechts(Ar4_3)
Ar5_4 <- rechts(Ar5_3)

A <- array(c(Av1_1,Av2_1,Av3_1,Av4_1,Av5_1,
             Av1_2,Av2_2,Av3_2,Av4_2,Av5_2,
             Av1_3,Av2_3,Av3_3,Av4_3,Av5_3,
             Av1_4,Av2_4,Av3_4,Av4_4,Av5_4,
             Ar1_1,Ar2_1,Ar3_1,Ar4_1,Ar5_1,
             Ar1_2,Ar2_2,Ar3_2,Ar4_2,Ar5_2,
             Ar1_3,Ar2_3,Ar3_3,Ar4_3,Ar5_3,
             Ar1_4,Ar2_4,Ar3_4,Ar4_4,Ar5_4), dim=c(2,2,4,5,4,2))

# Anker setzen

x <- 1

Box <- array(0, dim=c(4,4,4))

for(i in 1:4){
  for(j in 1:2){
    for(k in 2:3){
      Box[k,j,i] <- 1
    }
  }
}

Boxa <- Box

# Einsetzen der Teile

# Umdrehen

for(ad in 1:2){
  for(bd in 1:2){
    for(cd in 1:2){
      for(dd in 1:2){
        for(ed in 1:2){
          
          # Drehungen nach rechts
          for(ar in 1:4){
            for(br in 1:4){
              for(cr in 1:4){
                for(dr in 1:4){
                  for(er in 1:4){
                    
                    
                    # Position a
                    for(a in 1:5){
                      print((x/3932160)*100)
                      Box <- Boxa
                      for(i in 1:4){
                        for(j in 3:4){
                          for(k in 2:3){
                            Box[k,j,i] <- Box[k,j,i] + A[k-1,j-2,i,a,ar,ad]
                          }
                        }
                      }
                      if(2 %in% Box){
                        x <- x + 24
                        next
                      }
                      Boxb <- Box
                      
                      # Position b  
                      for(b in 1:5){
                        Box <- Boxb
                        if(b == a){next}
                        for(i in 1:2){
                          for(j in 2:3){
                            for(k in 1:4){
                              Box[k,j,i] <- Box[k,j,i] + A[3-i,j-1,k,b,br,bd]
                            }
                          }
                        }
                        if(2 %in% Box){
                          x <- x + 6
                          next
                        }
                        Boxc <- Box
                        
                        # Position c  
                        for(c in 1:5){
                          Box <- Boxc
                          if(c == a || c == b){next}
                          for(i in 3:4){
                            for(j in 2:3){
                              for(k in 1:4){
                                Box[k,j,i] <- Box[k,j,i] + A[5-i,j-1,k,c,cr,cd]
                              }
                            }
                          }
                          if(2 %in% Box){
                            x <- x + 2
                            next
                          }
                          Boxd <- Box
                          
                          # Position d  
                          for(d in 1:5){
                            Box <- Boxd
                            if(d == a || d == b || d == c){next}
                            for(i in 2:3){
                              for(j in 1:4){
                                for(k in 1:2){
                                  Box[k,j,i] <- Box[k,j,i] + A[k,4-i,j,d,dr,dd]
                                }
                              }
                            }
                            if(2 %in% Box){
                              x <- x + 1
                              next
                            }
                            Boxe <- Box
                            
                            # Position e  
                            for(e in 1:5){
                              Box <- Boxe
                              if(e == a || e == b || e == c || e == d){next}
                              x <- x+1
                              for(i in 2:3){
                                for(j in 1:4){
                                  for(k in 3:4){
                                    Box[k,j,i] <- Box[k,j,i] + A[k-2,4-i,j,e,er,ed]
                                    }
                                  }
                                }
                              if(2 %in% Box){
                                next
                              }
                              if(sum(Box)==56){
                                print("sum(Box)")
                                print(sum(Box))
                                print("a")
                                print(a)
                                print("ar")
                                print(ar)
                                print("ad")
                                print(ad)
                                print("b")
                                print(b)
                                print("br")
                                print(br)
                                print("bd")
                                print(bd)
                                print("c")
                                print(c)
                                print("cr")
                                print(cr)
                                print("cd")
                                print(cd)
                                print("d")
                                print(d)
                                print("dr")
                                print(dr)
                                print("dd")
                                print(dd)
                                print("e")
                                print(e)
                                print("er")
                                print(er)
                                print("ed")
                                print(ed)
                                stop("Berechnung beendet.")
                              }
                            }  
                          }  
                        }  
                      }  
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
