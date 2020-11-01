library(rgl)
library(pracma)
library(gplots)

setwd("") #set own patch to dataSet

results = "" #set own patch for results

pliki <- list.files()
czas<-array(0,dim=c(250,3))
pozostale <-rep(0,150)
podwojne <-rep(0,150)
poz=0


for (iter in 1:1){
  
  wyniki <- array(0, dim=c(2000 ,length(pliki)))
  gif_1=0
  
  #parameters
  dyst=c(0,0,0)
  
  a <- 1
  b <- 1
  c <- 1
  x_1 <- 0
  y_1 <- 0
  z_1 <- 0
  k = 0
  czas_1=0
  for (i in pliki){
    gif_1=gif_1+1
    
    k=k+1
    zbior <- read.csv(i,header=TRUE, sep = ";", dec=",")
    dl_zb <- length(zbior[,1])
    lb_gr <- length(unique(zbior[,4]))
    wyn <- array(0, dim=c(lb_gr,12))
    
    
    #Calculate the coefficient system with regard to the group center for all groups
    g=1
    dl_gr <- rep(0, times=lb_gr)
    
    
    for (j in 1:lb_gr){
      w_min<-as.data.frame(array(0,dim=c(12*lb_gr,7)))
      w_min2<-as.data.frame(array(0,dim=c(12*lb_gr,7)))
      w_min3<-as.data.frame(array(0,dim=c(12*lb_gr,16)))
      
      colnames(w_min2)<-c('min', 'max', 'id_Con', 'group', 'No_Con','% of occupancy','color')
      colnames(w_min3)<-c('min', 'max', 'id_Con', 'group', 'No_Con','% of occuapncy','k_10', 'k_20','k_30','k_40', 'k_50', 'k_60','k_70','k_80','k_90','k_100')
      
      
      start.time <- Sys.time()
      czas_1=czas_1+1
      #obliczenie dlugosci kazdej z grup:
      for (v in 1:lb_gr){
        dl_gr[v]<-nrow(subset(zbior, (zbior[,4]==v )))
      }
      
      
      
      kol_n=array(0, dim=c(12,lb_gr))
      h=j
      zb_2 <- zbior
      zb_2[,5] <- "white"
      zb_2[,6] <- 0
      zb_3 <- subset(zb_2, (zb_2[,4]==h))
      sr_x <- mean(zb_3[,1])
      sr_y <- mean(zb_3[,2])
      sr_z <- mean(zb_3[,3])
      zb_2[,1] <- zb_2[,1]-sr_x
      zb_2[,2] <- zb_2[,2]-sr_y
      zb_2[,3] <- zb_2[,3]-sr_z
      
      min_x <- min(zb_2[,1])
      min_y <- min(zb_2[,2])
      min_z <- min(zb_2[,3])
      max_x <- max(zb_2[,1])
      max_y <- max(zb_2[,2])
      max_z <- max(zb_2[,3])
      
  
      
      
      
      kol=c("yellow", "blue", "cyan", "green", "red", "brown", "chartreuse", "darkslategrey", "bisque3", "darkviolet", "coral4", "deepskyblue")
      cwiartki <- c('TL', 'TF', 'TR', 'LF', 'L', 'LB', 'RF','R','RB','BL','BB', 'BR' )
      cwiartki2<- c('T2', 'T3', 'MR1', 'MR2', 'MR3', 'B1', 'B2','B3','ML1','ML2','ML3', 'T1' )
      # wzdluz osi OX, promie? okr?gu 0.5
      
      sph_rad <- 1
      a_sph<- c(1, 0,- 1, -1, -2, -1, 1, 2, 1, 1, 0, -1)
      b_sph <- c(-0.58, 1.16, -0.58, -1.73, 0, 1.73, -1.73, 0, 1.73, 0.58, -1.16, 0.58)
      c_sph <- c(1.63, 1.63, 1.63, 0, 0, 0, 0, 0, 0, -1.63, -1.63, -1.63)
      tu_1 <-0
     
      
      
      for (fg in 1:12){
        for (z in 1:(dl_zb)){
          b_delta <- (-2*a_sph[fg]*zb_2[z,1]-2*b_sph[fg]*zb_2[z,2]-2*c_sph[fg]*zb_2[z,3]) #(x+a_sph)^2+(y+b_sph)^2+(z+c_sph)=1
          a_delta<- zb_2[z,1]^2+zb_2[z,2]^2+zb_2[z,3]^2
          c_delta <- a_sph[fg]^2+b_sph[fg]^2+c_sph[fg]^2-sph_rad^2
          
          if ( b_delta^2 - 4 * a_delta * c_delta >= 0 & zb_2[z,5]!="white" & (((fg==1 | fg==2 | fg==3)& zb_2[z,3]>0) | ((fg==4 | fg==5 | fg==6) & zb_2[z,1]<=0 ) | ((fg==7 | fg==8 | fg==9) & zb_2[z,1]>0) | ((fg==10 | fg==11 | fg==12) & zb_2[z,3]<=0 )  ) ) {
            tu_1=tu_1+1
          }
          
          
          if ( b_delta^2 - 4 * a_delta * c_delta >= 0 & zb_2[z,5]=="white" & (((fg==1 | fg==2 | fg==3)& zb_2[z,3]>0) | ((fg==4 | fg==5 | fg==6) & zb_2[z,1]<=0 ) | ((fg==7 | fg==8 | fg==9) & zb_2[z,1]>0) | ((fg==10 | fg==11 | fg==12) & zb_2[z,3]<=0 )  ) ) {
            zb_2[z,5]=kol[fg]
            
            
            zb_2[z,6] = fg
            
           
            kol_n[fg,(zbior[z,4])]=kol_n[fg,(zbior[z,4])]+1
            
            
            
          }
        }
      }
      
      tu<-0
      for (wer in 1:dl_zb){
        if (zb_2[wer,5]=="white"){
          tu<-tu+1
        }
      }
      sub.time<-Sys.time()
      czas[czas_1,1]<-sub.time-start.time
      
      poz=poz+1
      pozostale[poz] <- tu
      podwojne[poz] <- tu_1
      
      
      kol_n5 <-kol_n
      kol_n4 <- kol_n/dl_gr*100
      kol_n3 <-kol_n4
      kol_n4 <- ((kol_n4-1)/(100-1))*(10-1)+1
      kol_n4 <- round(kol_n4)
      kol_n2 <-kol_n4
      kol_n4 <- -kol_n4
      kol_n4 <- kol_n4+11
      
      
      
      
      
      if (j==2){
        zb_15<-zb_2
        zb_16<-subset(zb_15, (zb_15[,4]==4 | zb_15[,4]==14 ) )
      }
      zb_22<-zb_2
      #assign whites to one of the slopes
      kol=c("yellow", "blue", "cyan", "green", "red", "brown", "chartreuse", "darkslategrey", "bisque3", "darkviolet", "coral4", "deepskyblue") #Color of Con
      
      licz_white=0
      for (o in 1:dl_zb){
        haus <- 12300000
        if (zb_2[o,5]=="white"){
          licz_white=licz_white+1
          rut=0
          for (t in 1:12){
            
            zm_1=zb_2[o,4]
            
            zb_6 <- subset(zb_2, (zb_2[,6]==t & (zb_2[,4]==zm_1) ) )
            zb_6 <- zb_6[,1:3]
            t_6 <- data.matrix(zb_6, rownames.force = NA)
            t_7 <- data.matrix(zb_2[o,1:3], rownames.force = NA)
            dyst <- hausdorff_dist(as.numeric(t_6), as.numeric(t_7) )
            #dyst <- min(sqrt((zb_2[o,1]-zb_6[,1])^2+(zb_2[o,2]-zb_6[,2])^2+(zb_2[o,3]-zb_6[,3])^2 ))
            if (haus>dyst & dyst!=0 & dyst!=Inf){
              haus=dyst
              zb_2[o,5] <- kol[t]
              zb_2[o,6] <- t
              if (rut!=0){kol_n[rut,(zbior[o,4])]=kol_n[rut,(zbior[o,4])]-1}
              kol_n[t,(zbior[o,4])]=kol_n[t,(zbior[o,4])]+1
              rut=t
              
              
            }
            
          }
        
        }
        
      }
      print(licz_white)
      end.time<-Sys.time()
      czas[czas_1,2]<-end.time-start.time
      
      for (hj in 1:12){
        kol_n[hj,] <- kol_n[hj,]/dl_gr*100
      }
      
      kol_n7 <- rep(0, lb_gr*12)
      kol_i=1
      for (b in 1:lb_gr){
        for (bn in 1:12){
          kol_n7[kol_i]=kol_n[bn, b]
          kol_i=kol_i+1
        }
      }
      
      
      kol_n3 <-kol_n
      kol_n <- ((kol_n-0)/(100-0))*(9-0)+0
      kol_n <- kol_n + 1
      kol_n <- round(kol_n)
      kol_n2 <-kol_n
      kol_n <- -kol_n
      kol_n <- kol_n+11
      kol_n6<- rep(0,lb_gr*12)
      kol_i=1
      for (b in 1:lb_gr){
        for (bn in 1:12){
          kol_n6[kol_i]=kol_n[bn, b]
          kol_i=kol_i+1
        }
      }
      
      
      #Zliczenie liczby wyst?pie? w poszczeg?lnych ?wiartkach
      #dla wszystkich sto?k?w
      
      for (hj in 1:lb_gr){
        for (t in 1:12){
          d_4 <- nrow(subset(zb_2, ((zb_2[,4]==(hj) ) & zb_2[,6]==t) ))
          wyn[hj,t] <- d_4
          
        }
      }
      
      nazwa<-paste(results,i," ",j,".pdf",sep="")
      pdf(nazwa,width=15,height=40)
      barplot(wyn, beside=TRUE, col=rainbow(lb_gr), names.arg=c("T1","T2","T3","ML1","ML2","ML3","MR1", "MR2", "MR3","B1", "B2", "B3"), ylim=c(0,max(wyn)+10))
      #legend("topright", legend=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"), fill = rainbow(lb_gr), ncol = 5, cex = 0.35)
      dev.off()
      
      
      
      
     
      
      #odleglosc min max w stozku dla grupy
      dyst_2 <- array(0, dim=c(lb_gr,24))
      
      r=1
      f=1
      bh=1
      w_tym <- c(2,3,7,8,9,12,11,10,6,5,4,1)
      for (o in 1:lb_gr){
        r=1
        for (t in 1:12){
          
          min_odl=12300
          max_odl=0
          for (e in 1:dl_zb){
            if (zb_2[e,4]==o  & zb_2[e,6]==t){
              odl <- sqrt((0-zb_2[e,1])^2+(0-zb_2[e,2])^2+(0-zb_2[e,3])^2)
              if (odl>max_odl){max_odl=odl}
              if (odl<min_odl){min_odl=odl}
            }
            
          }
          dyst_2[f,r] <- min_odl
          r=r+1
          dyst_2[f,r] <- max_odl
          r=r+1
          
          if (min_odl==12300){
            w_min[bh,1] <- 0
          } else {
            w_min[bh,1] <- min_odl
          }
          w_min[bh,2] <-max_odl
          w_min[bh,3] <-t
          w_min[bh,4] <-o
          w_min[bh,5] <-cwiartki[t]
          bh=bh+1
          
        }
        f=f+1
        
        
      }
      w_min[,6]<-kol_n7
      w_min[,7]<-kol_n6
      
      bm=1
      for (b in 1:12){
        for (bn in 1:lb_gr){
          for (bz in 1:(lb_gr*12)){
            if (w_tym[b]==w_min[bz,3] & w_min[bz,4]==bn){
              w_min2[bm,]<-w_min[bz,]
              bm=bm+1
              
            }  
          }
          
        }
        
      }
      
      naz <- paste(results,i,"\-kompl_min_max",j,".txt", sep="")
      write.csv(w_min2, file=naz, row.names=FALSE)
      
      for (b in 1:(lb_gr*12)){
        w_min3[b,1:6]<-w_min2[b,1:6]
        w_min3[b,w_min2[b,7]+6]<-w_min2[b,2]
      }
      naz <- paste(results,i,"_",j,".txt", sep="")
      write.csv(w_min3, file=naz, row.names=FALSE)
      
      
      
      #Zapid wyników stozkowania do pliku
      
      naz <- paste(results,i,"-Cone",j,".txt", sep="")
      write.csv(zb_2, file=naz)
      
      #odleglosc min max calego chromosomu od centromeru
      dyst_4 <- array(0, dim=c(lb_gr,2))
      
      r=1
      f=1
      for (o in 1:lb_gr){
        r=1
        min_odl=12300
        max_odl=0
        for (e in 1:dl_zb){
          if (zb_2[e,4]==o ){
            odl <- sqrt((0-zb_2[e,1])^2+(0-zb_2[e,2])^2+(0-zb_2[e,3])^2)
            if (odl>max_odl){max_odl=odl}
            if (odl<min_odl){min_odl=odl}
          }
          
        }
        dyst_4[f,r] <- min_odl
        r=r+1
        dyst_4[f,r] <- max_odl
        #r=r+1
        
        
        f=f+1
        
        
      }
      
      
      palette( rev(rich.colors(10)) )
      nazwa<-paste(reults, "graph min max ",i," ",j,".pdf",sep="")
      pdf(nazwa,width=15,height=40)
      #par(mfrow=c(5,2)) # all plots on one page 
      #x_y<-1:10
      #barplot(x_y, col=1:10)
      l_kol=6
      #l_kol=l_kol+1
      dyst_5 <- array(0, dim=c(2,lb_gr))
      r=1
      for (t in 1:lb_gr){
        if (dyst_4[t,1]==12300){dyst_5[1,t]<-0}  else{
          dyst_5[1,t] <- dyst_4[t,1]
        }
        dyst_5[2,t] <- dyst_4[t,2]
        r=r+2
      }
      boxplot(dyst_5, col=kol_n[l_kol,], ylim=c(0,max(dyst_5)+10),main=cwiartki[l_kol], xlab="chromosom", medcol =kol_n[l_kol,])
      #lines(h_dyst_2, type = "p", pch=23)
      
      dev.off()
      
      
      
      
      
      
      
      
      #palette( rev(heat.colors(10)) )
      palette(c("#FFFFE5", "#FFF7BC", "#FEE391", "#FEC44F", "#FE9929", "#EC7014", "#CC4C02", "#993404", "#662506", "#321000"))
      nazwa<-paste(results, "/DPC ",i," ",j,".pdf",sep="")
      pdf(nazwa,width=15,height=40)
      par(mfrow=c(4,3)) # all plots on one page 
      x_y<-1:10
      #barplot(x_y, col=1:10)
      l_kol=0
      for (n in seq(1, 24, by = 2)){
        l_kol=l_kol+1
        dyst_3 <- array(0, dim=c(2,lb_gr))
        r=1
        for (t in 1:lb_gr){
          if (dyst_2[t,n]==12300){dyst_3[1,t]<-0}  else{
            dyst_3[1,t] <- dyst_2[t,n]
            
          }
          dyst_3[2,t] <- dyst_2[t,n+1]
          r=r+2
        }
        boxplot(dyst_3, col=11-kol_n[l_kol,], ylim=c(0,round(max(dyst_5)*1.1)),main=cwiartki[l_kol], xlab="group", medcol =11-kol_n[l_kol,])
        #lines(h_dyst[1:lb_gr,l_kol], type = "p", pch=23)
      }
      tytul_wykresu = paste("DPC12 for mouse data set", ramiona_wykres[j], "\n (", "file_path_sans_ext(i)",")", sep="" ) #po wypuszczeniu wykresu w pliku nie by?o wida? co si? dzieje, wzgl?dem jakiego chromosomu s? wykresy
      mtext(tytul_wykresu, outer = T, cex=1.5)
      dev.off()
      nazwa=paste(results, "/min_max_",i,"_",j , ".csv", sep="")
      write.csv(dyst_2,file=nazwa)
      
      
      
    }
   
    
  }

  
  
}
write.csv(czas,file=paste(results, "/czas_chrom.txt", sep = ""))

nazwa=paste(results, "/unclassified_.txt", sep="")
write.csv(pozostale,nazwa)

nazwa=paste(results, "/double_classified.txt", sep="")
write.csv(podwojne,nazwa)
