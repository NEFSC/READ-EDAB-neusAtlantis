#model Atlantis' Beverton-Holt recruitment with fake numbers
#somewhat modelled off of HAD

#Fixed Parameters
XRS = 1.65
FSP = 0.5
KSPA = 10
FSPB = 1
bh.a = 1E4
bh.b = 5E3

#Individual Variables
SN = 100
RN = SN * XRS
N = 1

atlantis_BH = function(XRS=1.65,FSP,FSPB,KSPA,bh.a,bh.b,SN,RN,N){
  
  #optimal spawning weight
  w0 = (1+XRS)*SN
  #total weight
  w = (SN+RN)
  
  #Calculate spawning biomass
  Sp = ( ((w0*FSP)-KSPA)-(w0-w) )*FSPB*N
  
  #Calculate Recruitment
  Rt = Sp*bh.a/(w + bh.b)
  
  return(Rt)
} 

#Baseline
atlantis_BH(XRS = XRS,
            FSP = FSP,
            FSPB = FSPB,
            KSPA = KSPA,
            bh.a = bh.a,
            bh.b = bh.b,
            SN = SN,
            RN = RN, 
            N =N)

#vary alpha
bh.a.v = seq(0,1E4,2E3)
change.bh.a = sapply(bh.a.v,function(x) return(atlantis_BH(XRS = XRS,
                                                           FSP = FSP,
                                                           FSPB = FSPB,
                                                           KSPA = KSPA,
                                                           bh.a = x,
                                                           bh.b = bh.b,
                                                           SN = SN,
                                                           RN = RN, 
                                                           N =N)))
plot(bh.a.v,change.bh.a,type='l')

#vary beta
bh.b.v = seq(0,1E4,500)
change.bh.b = sapply(bh.b.v,function(x) return(atlantis_BH(XRS = XRS,
                                                           FSP = FSP,
                                                           FSPB = FSPB,
                                                           KSPA = KSPA,
                                                           bh.a = bh.a,
                                                           bh.b = x,
                                                           SN = SN,
                                                           RN = RN, 
                                                           N =N)))
plot(bh.b.v,change.bh.b,type='l')

#vary FSP
fsp.v = seq(0,0.5,0.01)
change.fsp = sapply(fsp.v,function(x) return(atlantis_BH(XRS = XRS,
                                                           FSP = x,
                                                           FSPB = FSPB,
                                                           KSPA = KSPA,
                                                           bh.a = bh.a,
                                                           bh.b = bh.b,
                                                           SN = SN,
                                                           RN = RN, 
                                                           N =N)))
plot(fsp.v,change.fsp,type='l')

#vary FSPB
fspb.v = seq(0,1,0.05)

change.fspb = sapply(fspb.v,function(x) return(atlantis_BH(XRS = XRS,
                                                          FSP = FSP,
                                                          FSPB = x,
                                                          KSPA = KSPA,
                                                          bh.a = bh.a,
                                                          bh.b = bh.b,
                                                          SN = SN,
                                                          RN = RN, 
                                                          N =N)))
plot(fspb.v,change.fspb,type='l')

#vary KSPA
kspa.v = seq(0,100,1)

change.kspa = sapply(kspa.v,function(x) return(atlantis_BH(XRS = XRS,
                                                           FSP = FSP,
                                                           FSPB = FSPB,
                                                           KSPA = x,
                                                           bh.a = bh.a,
                                                           bh.b = bh.b,
                                                           SN = SN,
                                                           RN = RN, 
                                                           N =N)))
plot(kspa.v,change.kspa,type='l')

#Vary conditions (RN:SN)
rn.sn.v = seq(0,1.65,0.05)
rn.v = rn.sn.v*SN

change.rn = sapply(rn.v,function(x) return(atlantis_BH(XRS = XRS,
                                                           FSP = FSP,
                                                           FSPB = FSPB,
                                                           KSPA = KSPA,
                                                           bh.a = bh.a,
                                                           bh.b = bh.b,
                                                           SN = SN,
                                                           RN = x, 
                                                           N =N)))
plot(rn.sn.v,change.rn,type='l')
rn.sn.v[which(change.rn>0)[1]]

#Vary alpha & beta

##Fixed alpha
a.b.v = seq(0,10,0.5)
bh.a.v = bh.a*a.b.v
bh.b.v = bh.b*a.b.v
change.a.b = sapply(1:length(a.b.v),function(x) return(atlantis_BH(XRS = XRS,
                                                           FSP = FSP,
                                                           FSPB = FSPB,
                                                           KSPA = KSPA,
                                                           bh.a = bh.a.v[x],
                                                           bh.b = bh.b.v[x],
                                                           SN = SN,
                                                           RN = RN, 
                                                           N =N)))
plot(a.b.v,change.a.b,type='l')

#Contours of alpha beta combos
bh.a.seq = seq(0,1E4,100)
bh.b.seq = seq(0,10100,100)

combs = expand.grid(bh.a.seq,bh.b.seq)
change.a.b =  sapply(1:nrow(combs),function(x) return(atlantis_BH(XRS = XRS,
                                                                    FSP = FSP,
                                                                    FSPB = FSPB,
                                                                    KSPA = KSPA,
                                                                    bh.a = combs[x,1],
                                                                    bh.b = combs[x,2],
                                                                    SN = SN,
                                                                    RN = RN, 
                                                                    N =N)))

change.a.b.mat = matrix(change.a.b,nrow = length(bh.b.seq), ncol = length(bh.a.seq),byrow = T)

filled.contour(x = bh.b.seq,y = bh.a.seq,z = log10(change.a.b.mat),ylab = 'BH-beta',xlab = 'BH-alpha')
