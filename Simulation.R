#####
library(truncnorm)
#width and height for plots
PlotWidth=25
PlotHeight=15

##Function used
randvector=function(MeanLbdUbdStd,distribution)
{
  rand=c()
  if(distribution==0)
  {
    for(i in 1:dim(MeanLbdUbdStd)[1])
    {
      rand=c(rand, runif(1,MeanLbdUbdStd[i,2],MeanLbdUbdStd[i,3]))
    }
  }
  if(distribution==1)
  {
    for(i in 1:dim(MeanLbdUbdStd)[1])
    {
      randtemp=0
      while(any(randtemp<MeanLbdUbdStd[i,2] || randtemp>MeanLbdUbdStd[i,3])) randtemp=rnorm(1,MeanLbdUbdStd[i,1],MeanLbdUbdStd[i,4])
      rand=c(rand,randtemp)			
    }
  }
  if(distribution==2)
  {
    for(i in 1:dim(MeanLbdUbdStd)[1])
    {
      randtemp=0
      while(any(randtemp<MeanLbdUbdStd[i,2] || randtemp>MeanLbdUbdStd[i,3])) randtemp=MeanLbdUbdStd[i,1]*exp(rnorm(1,0,MeanLbdUbdStd[i,4]))*exp(-(MeanLbdUbdStd[i,4]^2)/2) 
      rand=c(rand,randtemp)              
    }
  }	
  return(rand)
}

cumsumbydim=function(datamatrix, dimension) #dimention is how many you want to cumulate
{
  set=dim(datamatrix)[1]/dimension
  matrixdim=dim(datamatrix)[2]
  if(identical(set,numeric(0))) 
  {
    set=length(datamatrix)/dimension
    matrixdim=1
  }
  
  setnum=rep(1:set,each=dimension)
  datanum=data.frame(datamatrix,setnum)
  
  cumdata1=c()	
  for(i in 1:matrixdim)
  {
    cumdata2=c()
    for(j in 1:set)
    {
      temp=subset(datanum,setnum==j)[,i]
      cumdata2=c(cumdata2,cumsum(temp))
    }
    cumdata1=cbind(cumdata1,cumdata2)
  }
  return(cumdata1)
}

library(xlsx)
library(ggplot2)

if(ModelFunctionSwitch==3.2 & HistoricalSimulationYesNo==1)
{
  load("HistoricalSimulation.RData")
  ModelFunctionSwitch=4
  HistoricalSimulationYesNo=1
  BRPSwitches[c(1,2),2]=c(1,0)
  
}else if(ModelFunctionSwitch==4 & HistoricalSimulationYesNo==1)
{
  load("HistoricalSimulation.RData")
  ModelFunctionSwitch=5
  HistoricalSimulationYesNo=1
  ProjectionSwitches[1,2]=1
  
}else
{
  if(ModelFunctionSwitch==1)
  {
    TunningSwitches=data.frame(Switch="tunning phase.  0: off; 1: on",Value=0)
    BRPSwitchesTemp=data.frame(Switch=c("BRP phase.  0: off; 1: on","approach to BRP. 0:  theoretical; 1: ad hoc"),Value=c(0,0))
    ProjectionSwitchesTemp=data.frame(Switch="projection phase.  0: off; 1: on",Value=0)
  }
  if(ModelFunctionSwitch==2)
  {
    TunningSwitches=data.frame(Switch="tunning phase.  0: off; 1: on",Value=1)
    BRPSwitchesTemp=data.frame(Switch=c("BRP phase.  0: off; 1: on","approach to BRP. 0:  theoretical; 1: ad hoc"),Value=c(0,0))
    ProjectionSwitchesTemp=data.frame(Switch="projection phase.  0: off; 1: on",Value=0)
  }
  if(ModelFunctionSwitch==3.1)
  {
    TunningSwitches=data.frame(Switch="tunning phase.  0: off; 1: on",Value=0)
    BRPSwitchesTemp=data.frame(Switch=c("BRP phase.  0: off; 1: on","approach to BRP. 0:  theoretical; 1: ad hoc"),Value=c(1,1))
    ProjectionSwitchesTemp=data.frame(Switch="projection phase.  0: off; 1: on",Value=0)
  }	
  if(ModelFunctionSwitch==3.2)
  {
    TunningSwitches=data.frame(Switch="tunning phase.  0: off; 1: on",Value=0)
    BRPSwitchesTemp=data.frame(Switch=c("BRP phase.  0: off; 1: on","approach to BRP. 0:  theoretical; 1: ad hoc"),Value=c(1,0))
    ProjectionSwitchesTemp=data.frame(Switch="projection phase.  0: off; 1: on",Value=0)
  }	
  if(ModelFunctionSwitch==4)
  {
    TunningSwitches=data.frame(Switch="tunning phase.  0: off; 1: on",Value=0)
    BRPSwitchesTemp=data.frame(Switch=c("BRP phase.  0: off; 1: on","approach to BRP. 0:  theoretical; 1: ad hoc"),Value=c(0,0))
    ProjectionSwitchesTemp=data.frame(Switch="projection phase.  0: off; 1: on",Value=1)
  }
  
  PlotSwitches=data.frame(Switch="summary plots. 0: by size class and time step; 1: by size class and year; 2: plot both options 0 and 1; 3: no plots",Value=PlotSwitch)
  
  ##Read in Switches.xlsx
  
  LSwitches=read.xlsx("Switches.xlsx",sheetName="LifeHistoryControl") 
  MSwitches=read.xlsx("Switches.xlsx",sheetName="ManagementControl") 
  #PlotSwitches=read.xlsx("Switches.xlsx",sheetName="PlotControl") 
  #TunningSwitches=read.xlsx("Switches.xlsx",sheetName="TunningControl") 
  BRPSwitches=rbind(BRPSwitchesTemp,read.xlsx("Switches.xlsx",sheetName="BRPControl"))
  ProjectionSwitches=rbind(ProjectionSwitchesTemp,read.xlsx("Switches.xlsx",sheetName="ProjectionControl"))
  
  ##Read in LifeHistoryControl.xlsx
  
  TimestepSetting=read.xlsx("LifeHistoryControl.xlsx",sheetName="TimestepSetting") 
  SimulationSetting=read.xlsx("LifeHistoryControl.xlsx",sheetName="SimulationSetting") 
  InitialAbunRecruitSexRatio=read.xlsx("LifeHistoryControl.xlsx",sheetName="InitialAbun&Recruit&SexRatio")
  SizeComp=read.xlsx("LifeHistoryControl.xlsx",sheetName="SizeComp") 
  ###############
  TuneSizeComp = read.xlsx("LifeHistoryControl.xlsx",sheetName="TuningSizeComp") 
  ###############
  Movement=read.xlsx("LifeHistoryControl.xlsx",sheetName="Movement") 
  MPAOutIn=read.xlsx("LifeHistoryControl.xlsx",sheetName="MPAOutIn") 
  MPAInOut=read.xlsx("LifeHistoryControl.xlsx",sheetName="MPAInOut") 
  MPAMaxK=read.xlsx("LifeHistoryControl.xlsx",sheetName="MPAMaxK") 
  LifeHistoryPara=read.xlsx("LifeHistoryControl.xlsx",sheetName="LifeHistoryPara") 
  NaturalM=read.xlsx("LifeHistoryControl.xlsx",sheetName="NaturalM") 
  BerriedProb=read.xlsx("LifeHistoryControl.xlsx",sheetName="BerriedProb") 
  HatchProb=read.xlsx("LifeHistoryControl.xlsx",sheetName="HatchProb") 
  BerriedMoltRelation=read.xlsx("LifeHistoryControl.xlsx",sheetName="BerriedMoltRelation") 
  #?MoltProb=read.xlsx("LifeHistoryControl.xlsx",sheetName="MoltProb") 
  # MoltIncrement=read.xlsx("LifeHistoryControl.xlsx",sheetName="MoltIncrement") 
  
  MoltProb=read.csv(file="MoltProb.csv") 
  MoltIncrement=read.csv(file="MoltIncrement.csv") 
  
  ##Read in ManagementControl.xlsx
  
  ManagementSetting=read.xlsx("ManagementControl.xlsx",sheetName="ManagementSetting") 
  EncounterRate=read.csv(file="EncounterRate.csv")
  # EncounterRate=read.xlsx("ManagementControl.xlsx",sheetName="EncounterRate") 		
  ScalingTemporalEnRate=read.xlsx("ManagementControl.xlsx",sheetName="ScalingTemporalEnRate")
  MinMaxLegalSize=read.xlsx("ManagementControl.xlsx",sheetName="MinMaxLegalSize")
  HandlingM=read.xlsx("ManagementControl.xlsx",sheetName="HandlingM")
  FisheryProb=read.xlsx("ManagementControl.xlsx",sheetName="FisheryProb")
  TAC=read.xlsx("ManagementControl.xlsx",sheetName="TAC")
  ObservedCatchEffort=read.xlsx("ManagementControl.xlsx",sheetName="ObservedCatchEffort")
  
  ##Add data for burning years
  
  NumBurnInYears=SimulationSetting[9,2]
  if(NumBurnInYears>0)
  {
    InitialAbunRecruitSexRatioBurn=subset(InitialAbunRecruitSexRatio,Year==ManagementSetting[1,2])
    NumberOfInitials=length(which(InitialAbunRecruitSexRatio$InitialAbundance>0)) 
    InitialAbunRecruitSexRatioBurnRep=InitialAbunRecruitSexRatioBurn[rep(1:dim(InitialAbunRecruitSexRatioBurn)[1],time=NumBurnInYears),]
    # InitialAbunRecruitSexRatioBurnRep$Recruitment=0
    InitialAbunRecruitSexRatioBurnRep$Year=rep((ManagementSetting[1,2]-NumBurnInYears):(ManagementSetting[1,2]-1),each=dim(InitialAbunRecruitSexRatioBurn)[1])
    InitialAbunRecruitSexRatio=rbind(InitialAbunRecruitSexRatioBurnRep,InitialAbunRecruitSexRatio)
    InitialAbunRecruitSexRatio$InitialAbundance[which(InitialAbunRecruitSexRatio$InitialAbundance>0)[-c(1:NumberOfInitials)]]=0 #only has one initial abundance coming in so the rest that was repeted from previous code need to be set back to zero
    
    MovementBurn=subset(Movement,Year==ManagementSetting[1,2])
    MovementBurnRep=MovementBurn[rep(1:dim(MovementBurn)[1],time=NumBurnInYears),]
    MovementBurnRep$Year=rep((ManagementSetting[1,2]-NumBurnInYears):(ManagementSetting[1,2]-1),each=dim(MovementBurn)[1])
    Movement=rbind(MovementBurnRep,Movement)
    
    MPAOutInBurn=subset(MPAOutIn,Year==ManagementSetting[1,2])
    MPAOutInBurnRep=MPAOutInBurn[rep(1:dim(MPAOutInBurn)[1],time=NumBurnInYears),]
    MPAOutInBurnRep$Year=rep((ManagementSetting[1,2]-NumBurnInYears):(ManagementSetting[1,2]-1),each=dim(MPAOutInBurn)[1])
    MPAOutIn=rbind(MPAOutInBurnRep,MPAOutIn)
    
    MPAInOutBurn=subset(MPAInOut,Year==ManagementSetting[1,2])
    MPAInOutBurnRep=MPAInOutBurn[rep(1:dim(MPAInOutBurn)[1],time=NumBurnInYears),]
    MPAInOutBurnRep$Year=rep((ManagementSetting[1,2]-NumBurnInYears):(ManagementSetting[1,2]-1),each=dim(MPAInOutBurn)[1])
    MPAInOut=rbind(MPAInOutBurnRep,MPAInOut)
    
    MPAMaxKBurn=subset(MPAMaxK,Year==ManagementSetting[1,2])
    MPAMaxKBurnRep=MPAMaxKBurn[rep(1:dim(MPAMaxKBurn)[1],time=NumBurnInYears),]
    MPAMaxKBurnRep$Year=rep((ManagementSetting[1,2]-NumBurnInYears):(ManagementSetting[1,2]-1),each=dim(MPAMaxKBurn)[1])
    MPAMaxK=rbind(MPAMaxKBurnRep,MPAMaxK)
    
    NaturalMBurn=NaturalM[,paste("MWeight_",ManagementSetting[1,2],sep="")]
    NaturalMBurnRep=as.data.frame(matrix(rep(NaturalMBurn,NumBurnInYears),byrow=F,ncol=NumBurnInYears))
    names(NaturalMBurnRep)=paste("MWeight_",(ManagementSetting[1,2]-NumBurnInYears):(ManagementSetting[1,2]-1),sep="")
    NaturalM=cbind(NaturalM[,c("Timestep","Area","MPA","SizeClass","mean","lbd","ubd","sd")],NaturalMBurnRep,NaturalM[,paste("MWeight_",(ManagementSetting[1,2]:ManagementSetting[2,2]),sep="")])
    
    ScalingTemporalEnRateBurn=subset(ScalingTemporalEnRate,Year==ManagementSetting[1,2])
    ScalingTemporalEnRateBurnRep=ScalingTemporalEnRateBurn[rep(1:dim(ScalingTemporalEnRateBurn)[1],time=NumBurnInYears),]
    ScalingTemporalEnRateBurnRep$Year=rep((ManagementSetting[1,2]-NumBurnInYears):(ManagementSetting[1,2]-1),each=dim(ScalingTemporalEnRateBurn)[1])
    ScalingTemporalEnRate=rbind(ScalingTemporalEnRateBurnRep,ScalingTemporalEnRate)
    
    MinMaxLegalSizeBurn=MinMaxLegalSize[,paste("Size_",ManagementSetting[1,2],sep="")]
    MinMaxLegalSizeBurnRep=as.data.frame(matrix(rep(MinMaxLegalSizeBurn,NumBurnInYears),byrow=F,ncol=NumBurnInYears))
    names(MinMaxLegalSizeBurnRep)=paste("Size_",(ManagementSetting[1,2]-NumBurnInYears):(ManagementSetting[1,2]-1),sep="")
    MinMaxLegalSize=cbind(MinMaxLegalSize[,c("Legal","Fishery","Area")],MinMaxLegalSizeBurnRep,MinMaxLegalSize[,paste("Size_",(ManagementSetting[1,2]:ManagementSetting[2,2]),sep="")])	
    
    HandlingMBurn=HandlingM[,paste("HandlingM_",ManagementSetting[1,2],sep="")]
    HandlingMBurnRep=as.data.frame(matrix(rep(HandlingMBurn,NumBurnInYears),byrow=F,ncol=NumBurnInYears))
    names(HandlingMBurnRep)=paste("HandlingM_",(ManagementSetting[1,2]-NumBurnInYears):(ManagementSetting[1,2]-1),sep="")
    HandlingM=cbind(HandlingM[,c("Fishery","Area","SizeClass")],HandlingMBurnRep,HandlingM[,paste("HandlingM_",(ManagementSetting[1,2]:ManagementSetting[2,2]),sep="")])	
    
    FisheryProbBurn=FisheryProb[,paste("FisheryProb_",ManagementSetting[1,2],sep="")]
    # FisheryProbBurnRep=as.data.frame(matrix(rep(FisheryProbBurn,NumBurnInYears),byrow=F,ncol=NumBurnInYears))
    FisheryProbBurnRep=as.data.frame(matrix(rep(FisheryProbBurn,NumBurnInYears),byrow=F,ncol=NumBurnInYears))
    
    names(FisheryProbBurnRep)=paste("FisheryProb_",(ManagementSetting[1,2]-NumBurnInYears):(ManagementSetting[1,2]-1),sep="")
    FisheryProb=cbind(FisheryProb[,c("Timestep","Fishery","Area","SizeClass")],FisheryProbBurnRep,FisheryProb[,paste("FisheryProb_",(ManagementSetting[1,2]:ManagementSetting[2,2]),sep="")])	
    
    TACBurn=TAC[,paste("TAC_",ManagementSetting[1,2],sep="")]
    TACBurnRep=as.data.frame(matrix(rep(TACBurn,NumBurnInYears),byrow=F,ncol=NumBurnInYears))
    names(TACBurnRep)=paste("TAC_",(ManagementSetting[1,2]-NumBurnInYears):(ManagementSetting[1,2]-1),sep="")
    TAC=cbind(TAC[,c("Legal","Fishery","Area")],TACBurnRep,TAC[,paste("TAC_",(ManagementSetting[1,2]:ManagementSetting[2,2]),sep="")])	
    
    EncounterRateBurn=EncounterRate[,paste("EnRate_",ManagementSetting[1,2],sep="")]
    # EncounterRateBurn=ifelse(EncounterRateBurn>0,0,0)
    EncounterRateBurnRep=as.data.frame(matrix(rep(EncounterRateBurn,NumBurnInYears),byrow=F,ncol=NumBurnInYears))
    names(EncounterRateBurnRep)=paste("EnRate_",(ManagementSetting[1,2]-NumBurnInYears):(ManagementSetting[1,2]-1),sep="")
    EncounterRate=cbind(EncounterRate[,c("Timestep","Area","Sex","SizeClass")],EncounterRateBurnRep,EncounterRate[,paste("EnRate_",(ManagementSetting[1,2]:ManagementSetting[2,2]),sep="")])		
    
    ObservedCatchEffort=subset(ObservedCatchEffort,Year>=ManagementSetting[1,2] & Year<=ManagementSetting[2,2])
    ObservedCatchEffortBurn=subset(ObservedCatchEffort,Year==ManagementSetting[1,2])
    ObservedCatchEffortBurnRep=ObservedCatchEffortBurn[rep(1:dim(ObservedCatchEffortBurn)[1],time=NumBurnInYears),]
    ObservedCatchEffortBurnRep$Year=rep((ManagementSetting[1,2]-NumBurnInYears):(ManagementSetting[1,2]-1),each=dim(ObservedCatchEffortBurn)[1])
    ObservedCatchEffort=rbind(ObservedCatchEffortBurnRep,ObservedCatchEffort)
    
    ManagementSetting[1,2]=ManagementSetting[1,2]-NumBurnInYears
  }
  
  ##Change the unit of the data
  
  InitialAbunRecruitSexRatio$InitialAbundance=round(InitialAbunRecruitSexRatio$InitialAbundance/SimulationSetting[8,2],0)
  InitialAbunRecruitSexRatio$Recruitment=round(InitialAbunRecruitSexRatio$Recruitment/SimulationSetting[8,2],0)
  InitialAbunRecruitSexRatio$EndCatchAndShort=round(InitialAbunRecruitSexRatio$EndCatchAndShort/SimulationSetting[8,2],0)
  ObservedCatchEffort$ObsLandedCatch=round(ObservedCatchEffort$ObsLandedCatch,0)
  InitialAbunRecruitSexRatio$CumulativeAbundance=cumsum(InitialAbunRecruitSexRatio[,5]+InitialAbunRecruitSexRatio[,6])
  MPAMaxK[,-1]=MPAMaxK[,-1]/SimulationSetting[8,2]
  TAC[,-c(1,2)]=TAC[,-c(1,2)]/SimulationSetting[8,2]
  
  ##Simulation settings
  
  SimulationYear=seq(ManagementSetting[1,2],ManagementSetting[2,2])
  SimulationSizeClass=seq(SimulationSetting[2,2],SimulationSetting[3,2],length.out=SimulationSetting[1,2])
  NumSizeClass=SimulationSetting[1,2]
  NumTimestep=max(na.omit(TimestepSetting$Timestep))
  NumArea=SimulationSetting[4,2]
  
  ##Result summary
  
  SummaryLength1=NumArea*2*2 #Area/MPA/Sex for each bin
  SummaryLength2=ManagementSetting[7,2]*NumArea*2 #Fishery/Area/Sex for each bin
  SummaryList=c("Abundance (number)","Biomass (weight)","Legal Abundance (number)","Legal Biomass (weight)","Dead Legal Abundance (number)", "Dead Legal Biomass (weight)","Spawning Stock Abundance (number)","Spawning Stock Biomass (weight)","Recruitment (number)","Recruitment (weight)","Dead Lobster due to Natural M (number)","Dead Lobster due to Natural M (weight)","Dead Lobster due to Molt M (number)","Dead Lobster due to Molt M (weight)","Lobster caught with V-Notched (number)","Lobster caught with V-Notched (weight)","Landed Catch (number)","Landed Catch (weight)","Dead Lobster due to Handling M (number)","Dead Lobster due to Handling M (weight)","Exploitation rate (Landed Catch in number/Legal Abundance)","Exploitation rate (Landed Catch in weight/Legal Biomass)")
  SummaryListSheetName=c("Abundance","Biomass","LegalAbundance","LegalBiomass","DeadLegalAbundance", "DeadLegalBiomass","SpawningStockAbundance","SpawningStockBiomass","RecruitNumber","RecruitWeight","DeadNaturalMNumber","DeadNaturalMWeight","DeadMoltMNumber","DeadMoltMWeight","VNotchedNumber","VNotchedWeight","LandedCatchNumber","LandedCatchWeight","DeadHandlingMNumber","DeadHandlingMWeight","ExploitationRateNumber","ExploitationRateWeight")
  
  FisheryStatus=c("1-1","1-2","1-3","2-1","2-2","2-3","3-1","3-2","3-3")
  if(PlotSwitches[1,2]==0 || PlotSwitches[1,2]==2) windows(width=PlotWidth, height=PlotHeight, record=T)
}

Simulation=function(SimulationYeartemp,SimulationTimestepStarttemp,SimulationTimestepEndtemp,fBRPtemp,InitialAbuScalertemp,RecruitScalertemp,EnrateScaler_t1temp,EnrateScaler_t2temp,EnrateScaler_t3temp,EnrateScaler_t4temp,BRPSwitch1,BRPSwitch2,BRPSwitch3,ProjectionSwitch1, ProjectionRecruitmentSwitch)
{
  #message for incomplete years
  
  if(SimulationTimestepStarttemp!=1) print(paste("Warning: first simulation year is not a complete year. Starting timestep is", SimulationTimestepStarttemp))
  
  if(ManagementSetting[8,2]!=1 & SimulationTimestepEndtemp!=ManagementSetting[8,2]-1) print(paste("Warning: last simulation year is not a complete year. Ending timestep is", SimulationTimestepEndtemp))
  
  #calculate total timestep running, calculate EnRate and initialize some parameters 
  
  if(ProjectionSwitch1==0) 
  {
    TotalTimestep=(length(SimulationYeartemp)-2)*NumTimestep+(NumTimestep-SimulationTimestepStarttemp+1)+SimulationTimestepEndtemp
    
    TimestepSSBAbu=c() #used to store SSB for projection, if projection is on, this need not to be cleaned!
    TimestepSSBBio=c()
    
    EnRateD1=which(names(EncounterRate)==paste("EnRate_",ManagementSetting[1,2],sep=""))
    EnRateD2=which(names(EncounterRate)==paste("EnRate_",ManagementSetting[2,2],sep=""))
    
    if(MSwitches[4,2]==1)
    {
      MinScaleTemporalEnrate<<-min(ScalingTemporalEnRate$Catch.Effort[ScalingTemporalEnRate$Catch.Effort>0])			
      ScalingTemporalEnRate$Scale=ScalingTemporalEnRate$Catch.Effort/MinScaleTemporalEnrate
      #EncounterRate[,c(EnRateD1:EnRateD2)]=EncounterRate[,c(EnRateD1:EnRateD2)]*matrix(rep(ScalingTemporalEnRate$Scale,each=NumSizeClass*2),nrow=NumTimestep*NumSizeClass*2)*EnrateScalertemp #times all the scale values and then make sure that the max is not 
      
      #####################
      EncounterRate[EncounterRate$Timestep == 1,c(EnRateD1:EnRateD2)]=EncounterRate[EncounterRate$Timestep == 1,c(EnRateD1:EnRateD2)]*matrix(rep(ScalingTemporalEnRate$Scale[ScalingTemporalEnRate$Timestep==1],each=NumSizeClass*2),nrow=NumSizeClass*2)*EnrateScaler_t1temp #times all the scale values and then make sure that the max is not 
      EncounterRate[EncounterRate$Timestep == 2,c(EnRateD1:EnRateD2)]=EncounterRate[EncounterRate$Timestep == 2,c(EnRateD1:EnRateD2)]*matrix(rep(ScalingTemporalEnRate$Scale[ScalingTemporalEnRate$Timestep==2],each=NumSizeClass*2),nrow=NumSizeClass*2)*EnrateScaler_t2temp
      EncounterRate[EncounterRate$Timestep == 3,c(EnRateD1:EnRateD2)]=EncounterRate[EncounterRate$Timestep == 3,c(EnRateD1:EnRateD2)]*matrix(rep(ScalingTemporalEnRate$Scale[ScalingTemporalEnRate$Timestep==3],each=NumSizeClass*2),nrow=NumSizeClass*2)*EnrateScaler_t3temp 
      EncounterRate[EncounterRate$Timestep == 4,c(EnRateD1:EnRateD2)]=EncounterRate[EncounterRate$Timestep == 4,c(EnRateD1:EnRateD2)]*matrix(rep(ScalingTemporalEnRate$Scale[ScalingTemporalEnRate$Timestep==4],each=NumSizeClass*2),nrow=NumSizeClass*2)*EnrateScaler_t4temp 
      #######################
    }
    else {
      #EncounterRate[,c(EnRateD1:EnRateD2)]=EncounterRate[,c(EnRateD1:EnRateD2)]*EnrateScalertemp
      EncounterRate[EncounterRate$Timestep==1,c(EnRateD1:EnRateD2)]=EncounterRate[EncounterRate$Timestep==1,c(EnRateD1:EnRateD2)]*EnrateScaler_t1temp
      EncounterRate[EncounterRate$Timestep==2,c(EnRateD1:EnRateD2)]=EncounterRate[EncounterRate$Timestep==2,c(EnRateD1:EnRateD2)]*EnrateScaler_t2temp
      EncounterRate[EncounterRate$Timestep==3,c(EnRateD1:EnRateD2)]=EncounterRate[EncounterRate$Timestep==3,c(EnRateD1:EnRateD2)]*EnrateScaler_t3temp
      EncounterRate[EncounterRate$Timestep==4,c(EnRateD1:EnRateD2)]=EncounterRate[EncounterRate$Timestep==4,c(EnRateD1:EnRateD2)]*EnrateScaler_t4temp
    }
    
    
    if(max(EncounterRate[,c(EnRateD1:EnRateD2)])>1)
    {
      if(MSwitches[5,2]==0) EncounterRate[,c(EnRateD1:EnRateD2)][EncounterRate[,c(EnRateD1:EnRateD2)]>1]=1
      if(MSwitches[5,2]==1) EncounterRate[,c(EnRateD1:EnRateD2)]=EncounterRate[,c(EnRateD1:EnRateD2)]/max(EncounterRate[,c(EnRateD1:EnRateD2)])
    }
    
    write.csv(EncounterRate, file="EncounterRateScaled.csv",row.names=F)
  }
  
  if(ProjectionSwitch1==1) 
  {
    NumLiveLobsta=NumLiveLobstaProjection
    BioInMPA=BioInMPAProjection
    ChangeInManagementStrategy4NextYear=c()
    ChangeMinLegalSize4NextYear=0
    ChangeMaxLegalSize4NextYear=0
    ChangeMPAOutIn4NextYear=0
    ChangeMPAMaxK4NextYear=0
    ChangeTAC4NextYear=0
    ChangeEncounterRate4NextYear=0
    CloseFishingTimestep4NextYear=0
    PreviousBoxInSimtemp=0
    CumulativeBoxInSimtemp=0 #how many years in same box
    
    TotalTimestep=(length(SimulationYeartemp)-2)*NumTimestep+(NumTimestep-SimulationTimestepStarttemp+1)+SimulationTimestepEndtemp+ThisTimestep-1 #if projection the number will started from the previous ended timestep and total timestep will be historical+simulation timestep
    
    BoxInSimtemp=0 #trigger for management action
    
    EnRateD1=which(names(EncounterRateProject)==paste("EnRate_",1,sep=""))
    EnRateD2=which(names(EncounterRateProject)==paste("EnRate_",ProjectionSetting[1,2],sep=""))
    
    if(MSwitches[4,2]==1)
    {
      ScalingTemporalEnRateProject$Scale=ScalingTemporalEnRateProject$Catch.Effort/MinScaleTemporalEnrate
      #EncounterRateProject[,c(EnRateD1:EnRateD2)]=EncounterRateProject[,c(EnRateD1:EnRateD2)]*EnrateScalertemp*matrix(rep(ScalingTemporalEnRateProject$Scale,each=NumSizeClass*2),nrow=NumTimestep*NumSizeClass*2) #times all the scale values and then make sure that the max is not 
      #####################
      EncounterRateProject[EncounterRateProject$Timestep == 1,c(EnRateD1:EnRateD2)]=EncounterRateProject[EncounterRateProject$Timestep == 1,c(EnRateD1:EnRateD2)]*matrix(rep(ScalingTemporalEnRateProject$Scale[ScalingTemporalEnRateProject$Timestep==1],each=NumSizeClass*2),nrow=NumSizeClass*2)*EnrateScaler_t1temp #times all the scale values and then make sure that the max is not 
      EncounterRateProject[EncounterRateProject$Timestep == 2,c(EnRateD1:EnRateD2)]=EncounterRateProject[EncounterRateProject$Timestep == 2,c(EnRateD1:EnRateD2)]*matrix(rep(ScalingTemporalEnRateProject$Scale[ScalingTemporalEnRateProject$Timestep==2],each=NumSizeClass*2),nrow=NumSizeClass*2)*EnrateScaler_t2temp
      EncounterRateProject[EncounterRateProject$Timestep == 3,c(EnRateD1:EnRateD2)]=EncounterRateProject[EncounterRateProject$Timestep == 3,c(EnRateD1:EnRateD2)]*matrix(rep(ScalingTemporalEnRateProject$Scale[ScalingTemporalEnRateProject$Timestep==3],each=NumSizeClass*2),nrow=NumSizeClass*2)*EnrateScaler_t3temp 
      EncounterRateProject[EncounterRateProject$Timestep == 4,c(EnRateD1:EnRateD2)]=EncounterRateProject[EncounterRateProject$Timestep == 4,c(EnRateD1:EnRateD2)]*matrix(rep(ScalingTemporalEnRateProject$Scale[ScalingTemporalEnRateProject$Timestep==4],each=NumSizeClass*2),nrow=NumSizeClass*2)*EnrateScaler_t4temp 
      #######################
    }
    else {
      #EncounterRate[,c(EnRateD1:EnRateD2)]=EncounterRate[,c(EnRateD1:EnRateD2)]*EnrateScalertemp
      EncounterRateProject[EncounterRateProject$Timestep==1,c(EnRateD1:EnRateD2)]=EncounterRateProject[EncounterRateProject$Timestep==1,c(EnRateD1:EnRateD2)]*EnrateScaler_t1temp
      EncounterRateProject[EncounterRateProject$Timestep==2,c(EnRateD1:EnRateD2)]=EncounterRateProject[EncounterRateProject$Timestep==2,c(EnRateD1:EnRateD2)]*EnrateScaler_t2temp
      EncounterRateProject[EncounterRateProject$Timestep==3,c(EnRateD1:EnRateD2)]=EncounterRateProject[EncounterRateProject$Timestep==3,c(EnRateD1:EnRateD2)]*EnrateScaler_t3temp
      EncounterRateProject[EncounterRateProject$Timestep==4,c(EnRateD1:EnRateD2)]=EncounterRateProject[EncounterRateProject$Timestep==4,c(EnRateD1:EnRateD2)]*EnrateScaler_t4temp
    }
    
    if(max(EncounterRateProject[,c(EnRateD1:EnRateD2)])>1)
    {
      if(MSwitches[5,2]==0) EncounterRateProject[,c(EnRateD1:EnRateD2)][EncounterRateProject[,c(EnRateD1:EnRateD2)]>1]=1 
      if(MSwitches[5,2]==1) EncounterRateProject[,c(EnRateD1:EnRateD2)]=EncounterRateProject[,c(EnRateD1:EnRateD2)]/max(EncounterRateProject[,c(EnRateD1:EnRateD2)])
    }
    
    write.csv(EncounterRateProject, file="EncounterRateProjectScaled.csv",row.names=F)
  }	
  
  Summary1=c()
  Summary2=c()
  
  for(yr in 1:length(SimulationYeartemp))
  {   
    for(ThisTimestepWYear in 1:NumTimestep) 
    {
      ThisYear=SimulationYeartemp[yr]
      
      if(ThisYear==SimulationYeartemp[1] & ThisTimestepWYear<SimulationTimestepStarttemp) next;
      
      if(ThisYear==SimulationYeartemp[length(SimulationYeartemp)] & ThisTimestepWYear>SimulationTimestepEndtemp) break;
      
      if(ThisTimestep==1)
      {
        LobstaRecord=c()   		    
        write.table(LobstaRecord,file="LobstaRecord.txt",row.names=F,col.names=F, quote = F) 
        NumLiveLobsta=0
        BioInMPA=rep(0,NumArea)
      }
      
      #use to store catch during the fishing year so that the model can 
      if(ThisTimestepWYear==SimulationTimestepStarttemp & ProjectionSwitch1==1)
      {
        CatchAbuBRPInSimtemp=0
        CatchBioBRPInSimtemp=0
      }
      
      ##Form c++ input file
      
      SizeCompIniAbuRecruitTemp=c()
      SizeCompIniAbuRecruit=c()
      SizeCompFemale=c()
      SizeCompMale=c()
      
      for(area in 1:NumArea)
      {
        for(mpa in 1:2)
        {
          if(ProjectionSwitch1==1)
          {
            
            if(ProjectionRecruitmentSwitch==0) #random draw from historical recruitment
            {
              tempAll=subset(InitialAbunRecruitSexRatio,Timestep==ThisTimestepWYear & Area==area & MPA==mpa & Year!=SimulationYear[1] & Year!=tail(SimulationYear,1))
              RecruitAll=subset(tempAll,Year==sample(unique(tempAll$Year),1))
              RecruitFemale=RecruitAll$Recruitment*RecruitAll$RecruitmentSexRatio
              RecruitMale=RecruitAll$Recruitment-RecruitFemale
              
            }
            if(ProjectionRecruitmentSwitch==1) #recruitment from sheet (input)
            {
              RecruitAll=subset(RecruitProject,Timestep==ThisTimestepWYear & Area==area & MPA==mpa & Year==yr)
              RecruitFemale=RecruitAll$Recruitment*RecruitAll$RecruitmentSexRatio
              RecruitMale=RecruitAll$Recruitment-RecruitFemale
            }
            if(ProjectionRecruitmentSwitch==2)
            {
              if(yr>6){
                TimestepSummary1data=aggregate(Summary1$Quantity,by=as.list(Summary1[,c("Name","Area","MPA","Year","Timestep","ThisTimestep")]),sum)
                prediction_all <- subset(TimestepSummary1data[which(TimestepSummary1data$MPA=="Out MPA"),], Name=="Spawning Stock Biomass (weight)")
                prediction_time=aggregate(prediction_all$x,by=list(prediction_all$Year,prediction_all$Timestep),sum)
                colnames(prediction_time)[1] <- "Year"
                colnames(prediction_time)[2] <- "timestep"
                colnames(prediction_time)[3] <- "ssb"
                ssb <-prediction_time
                ssb <- ssb[with(ssb, order(Year, timestep)),]
                SSBthistimestep<- ssb[ssb$timestep==3,]
                SSBthistimestep<- SSBthistimestep$ssb[SSBthistimestep$Year==yr-6]
              }
              if(yr<7){
                TimestepSummary1data=aggregate(Summary1Base$Quantity,by=as.list(Summary1Base[,c("Name","Area","MPA","Year","Timestep","ThisTimestep")]),sum)
                prediction_all <- subset(TimestepSummary1data[which(TimestepSummary1data$MPA=="Out MPA"),], Name=="Spawning Stock Biomass (weight)")
                prediction_time=aggregate(prediction_all$x,by=list(prediction_all$Year,prediction_all$Timestep),sum)
                colnames(prediction_time)[1] <- "Year"
                colnames(prediction_time)[2] <- "timestep"
                colnames(prediction_time)[3] <- "ssb"
                ssb <-prediction_time
                ssb <- ssb[with(ssb, order(Year, timestep)),]
                SSBthistimestep<- ssb[ssb$timestep==3,]
                SSBthistimestep<- SSBthistimestep$ssb[SSBthistimestep$Year==tail(SSBthistimestep$Year,1)-6]
              }
              bootnls<-read.csv('bootnls.csv')
              alpha<-bootnls$a
              beta<-bootnls$b
              
              library(FSA)
              r1 <- srFuns("Ricker",msg=FALSE)
              
              if (ThisTimestepWYear==1 | ThisTimestepWYear==2 | mpa==1) {
                RecruitAll=0
                RecruitFemale=0
                RecruitMale=0
              }
              if (ThisTimestepWYear==3 & mpa==2){
                RecruitAll <- 0.66*(r1(SSBthistimestep,sample(alpha,1),sample(beta,1)))/10000
                RecruitmentSexRatio=0.5
                RecruitFemale=RecruitAll*RecruitmentSexRatio
                RecruitMale=RecruitAll-RecruitFemale
              }
              if (ThisTimestepWYear==4 & mpa==2){
                RecruitAll <- 0.33*(r1(SSBthistimestep,sample(alpha,1),sample(beta,1)))/10000
                RecruitmentSexRatio=0.5
                RecruitFemale=RecruitAll*RecruitmentSexRatio
                RecruitMale=RecruitAll-RecruitFemale
              }
            }	
            if(ProjectionRecruitmentSwitch==3)
            {
              temp<-read.csv('temp.csv')
              if(yr>6){
                TimestepSummary1data=aggregate(Summary1$Quantity,by=as.list(Summary1[,c("Name","Area","MPA","Year","Timestep","ThisTimestep","Sex")]),sum)
                prediction_all <- TimestepSummary1data[TimestepSummary1data$MPA=="Out MPA" & TimestepSummary1data$Name=="Spawning Stock Biomass (weight)" & TimestepSummary1data$Sex=="Female",]
                prediction_time=aggregate(prediction_all$x,by=list(prediction_all$Year,prediction_all$Timestep),sum)
                colnames(prediction_time)[1] <- "Year"
                colnames(prediction_time)[2] <- "timestep"
                colnames(prediction_time)[3] <- "ssb"
                ssb <-prediction_time
                ssb <- ssb[with(ssb, order(Year, timestep)),]
                SSBthistimestep<- ssb[ssb$timestep==3,]
                SSBthistimestep<- SSBthistimestep$ssb[SSBthistimestep$Year==yr-6]
                temp<-temp$temp[temp$Year==yr+2013]
              }
              if(yr<7){
                TimestepSummary1data=aggregate(Summary1Base$Quantity,by=as.list(Summary1Base[,c("Name","Area","MPA","Year","Timestep","ThisTimestep","Sex")]),sum)
                prediction_all <- TimestepSummary1data[TimestepSummary1data$MPA=="Out MPA" & TimestepSummary1data$Name=="Spawning Stock Biomass (weight)" & TimestepSummary1data$Sex=="Female",]
                prediction_time=aggregate(prediction_all$x,by=list(prediction_all$Year,prediction_all$Timestep),sum)
                colnames(prediction_time)[1] <- "Year"
                colnames(prediction_time)[2] <- "timestep"
                colnames(prediction_time)[3] <- "ssb"
                ssb <-prediction_time
                ssb <- ssb[with(ssb, order(Year, timestep)),]
                SSBthistimestep<- ssb[ssb$timestep==3,]
                SSBthistimestep<- SSBthistimestep$ssb[SSBthistimestep$Year==tail(SSBthistimestep$Year,1)-6]
                temp<-temp$temp[temp$Year==yr+2013]
              }
              
              if (ThisTimestepWYear==1 | ThisTimestepWYear==2 | mpa==1) {
                RecruitAll=0
                RecruitFemale=0
                RecruitMale=0
              }
              if (ThisTimestepWYear==3 & mpa==2){
                newdata<-as.data.frame(cbind(SSBthistimestep,temp))
                colnames(newdata)<-c('SSB.lag','avetemp')
                RecruitAll <- 0.66*predict(mod2,newdata=newdata,type='response')/10000
                if (RecruitAll>23862){
                  RecruitAll<-23862
                }
                RecruitmentSexRatio=0.5
                RecruitFemale=RecruitAll*RecruitmentSexRatio
                RecruitMale=RecruitAll-RecruitFemale
              }
              if (ThisTimestepWYear==4 & mpa==2){
                newdata<-as.data.frame(cbind(SSBthistimestep,temp))
                colnames(newdata)<-c('SSB.lag','avetemp')
                RecruitAll <- 0.33*predict(mod2,newdata=newdata,type='response')/10000
                if (RecruitAll>11931){
                  RecruitAll<-11931
                }
                RecruitmentSexRatio=0.5
                RecruitFemale=RecruitAll*RecruitmentSexRatio
                RecruitMale=RecruitAll-RecruitFemale
              }
            }
            if(ProjectionRecruitmentSwitch==4)
            {
              if(yr>6){
                TimestepSummary1data=aggregate(Summary1$Quantity,by=as.list(Summary1[,c("Name","Area","MPA","Year","Timestep","ThisTimestep")]),sum)
                prediction_all <- subset(TimestepSummary1data[which(TimestepSummary1data$MPA=="Out MPA"),], Name=="Spawning Stock Biomass (weight)")
                prediction_time=aggregate(prediction_all$x,by=list(prediction_all$Year,prediction_all$Timestep),sum)
                colnames(prediction_time)[1] <- "Year"
                colnames(prediction_time)[2] <- "timestep"
                colnames(prediction_time)[3] <- "ssb"
                ssb <-prediction_time
                ssb <- ssb[with(ssb, order(Year, timestep)),]
                SSBthistimestep<- ssb[ssb$timestep==3,]
                SSBthistimestep<- SSBthistimestep$ssb[SSBthistimestep$Year==yr-6]
              }
              if(yr<7){
                TimestepSummary1data=aggregate(Summary1Base$Quantity,by=as.list(Summary1Base[,c("Name","Area","MPA","Year","Timestep","ThisTimestep")]),sum)
                prediction_all <- subset(TimestepSummary1data[which(TimestepSummary1data$MPA=="Out MPA"),], Name=="Spawning Stock Biomass (weight)")
                prediction_time=aggregate(prediction_all$x,by=list(prediction_all$Year,prediction_all$Timestep),sum)
                colnames(prediction_time)[1] <- "Year"
                colnames(prediction_time)[2] <- "timestep"
                colnames(prediction_time)[3] <- "ssb"
                ssb <-prediction_time
                ssb <- ssb[with(ssb, order(Year, timestep)),]
                SSBthistimestep<- ssb[ssb$timestep==3,]
                SSBthistimestep<- SSBthistimestep$ssb[SSBthistimestep$Year==tail(SSBthistimestep$Year,1)-6]
              }
              
              if (ThisTimestepWYear==1 | ThisTimestepWYear==2 | mpa==1) {
                RecruitAll=0
                RecruitFemale=0
                RecruitMale=0
              }
              if (ThisTimestepWYear==3 & mpa==2){
                if (SSBthistimestep<=10000000) RecruitAll <- 0.66*(rnorm(1, mean=62050000, sd=25840418)/10000)
                if (SSBthistimestep>10000000 & SSBthistimestep<=12500000) RecruitAll <- 0.66*(rnorm(1,mean=69320000, sd=32570877)/10000)
                if (SSBthistimestep>12500000 & SSBthistimestep<=16000000) RecruitAll <- 0.66*(rnorm(1,mean=107180000, sd=21179282)/10000)
                if (SSBthistimestep>16000000 & SSBthistimestep<=19000000) RecruitAll <- 0.66*(rnorm(1,mean=161666667, sd=55148285)/10000)
                if (SSBthistimestep>19000000) RecruitAll <- 0.66*(rnorm(1,mean=274666667, sd=10969655)/10000)
                #if (SSBthistimestep<=mean_sd3$breaks[6] & SSBthistimestep>mean_sd3$breaks[5]) RecruitAll <- rnorm(1, mean=mean_sd3[6,1], sd=mean_sd3[6,2])
                RecruitmentSexRatio=0.5
                RecruitFemale=RecruitAll*RecruitmentSexRatio
                RecruitMale=RecruitAll-RecruitFemale
              }
              if (ThisTimestepWYear==4 & mpa==2){
                if (SSBthistimestep<=10000000) RecruitAll <- 0.33*(rnorm(1, mean=62050000, sd=25840418)/10000)
                if (SSBthistimestep>10000000 & SSBthistimestep<=12500000) RecruitAll <- 0.33*(rnorm(1,mean=69320000, sd=32570877)/10000)
                if (SSBthistimestep>12500000 & SSBthistimestep<=16000000) RecruitAll <- 0.33*(rnorm(1,mean=107180000, sd=21179282)/10000)
                if (SSBthistimestep>16000000 & SSBthistimestep<=19000000) RecruitAll <- 0.33*(rnorm(1,mean=161666667, sd=55148285)/10000)
                if (SSBthistimestep>19000000) RecruitAll <- 0.33*(rnorm(1,mean=274666667, sd=10969655)/10000)
                RecruitmentSexRatio=0.5
                RecruitFemale=RecruitAll*RecruitmentSexRatio
                RecruitMale=RecruitAll-RecruitFemale
              }
            }	
            SizeCompFemale=subset(SizeComp, Area==area & MPA==mpa & Sex==0)
            SizeCompMale=subset(SizeComp, Area==area & MPA==mpa & Sex==1)	
            
            SizeCompFemale=data.frame(RecruitSizeComp=round(RecruitFemale*SizeCompFemale$RecruitSizeComp),IniAbuSizeComp=0,Area=area,MPA=mpa,Sex=0,SizeClass=SimulationSizeClass)					
            
            SizeCompMale=data.frame(RecruitSizeComp=round(RecruitMale*SizeCompMale$RecruitSizeComp),IniAbuSizeComp=0,Area=area,MPA=mpa,Sex=1,SizeClass=SimulationSizeClass)
          }
          else #not projection phase
          {
            if(BRPSwitch1==0 || (BRPSwitch1==1 & BRPSwitch2==1)) temp=subset(InitialAbunRecruitSexRatio,Year==ThisYear & Timestep==ThisTimestepWYear & Area==area & MPA==mpa) #base case
            else if(BRPSwitch1==1 & BRPSwitch2==0 & BRPSwitch3==0) #BRP one cohort
            {
              temp=subset(RecruitTheoBRP,Timestep==ThisTimestepWYear & Area==area & MPA==mpa)
              if(ThisTimestep>NumTimestep) temp[,"Recruitment"]=0
            }
            else if(BRPSwitch1==1 & BRPSwitch2==0 & BRPSwitch3==1) temp=subset(RecruitTheoBRP,Timestep==ThisTimestepWYear & Area==area & MPA==mpa) #BRP multiple cohorts
            
            SizeCompFemale=subset(SizeComp, Area==area & MPA==mpa & Sex==0)
            SizeCompMale=subset(SizeComp, Area==area & MPA==mpa & Sex==1)					
            
            IniAbuFemale=temp$InitialAbundance*temp$InitialAbundanceSexRatio
            IniAbuMale=temp$InitialAbundance-IniAbuFemale
            SizeCompFemale$IniAbuSizeComp=round(IniAbuFemale*SizeCompFemale$SizeComp)
            SizeCompMale$IniAbuSizeComp=round(IniAbuMale*SizeCompMale$SizeComp)
            
            RecruitFemale=temp$Recruitment*temp$RecruitmentSexRatio
            RecruitMale=temp$Recruitment-RecruitFemale
            SizeCompFemale$RecruitSizeComp=round(RecruitFemale*SizeCompFemale$RecruitSizeComp) #recruit to the first size bin
            SizeCompMale$RecruitSizeComp=round(RecruitMale*SizeCompMale$RecruitSizeComp) #recruit to the first size bin
            
          }
          
          SizeCompIniAbuRecruitTemp=rbind(SizeCompIniAbuRecruitTemp,rbind(SizeCompFemale,SizeCompMale))
          
        }
      }
      
      SizeCompIniAbuRecruit[1]=paste(as.character(SizeCompIniAbuRecruitTemp[,"Area"]),collapse=" ")
      SizeCompIniAbuRecruit[2]=paste(as.character(SizeCompIniAbuRecruitTemp[,"MPA"]),collapse=" ")
      SizeCompIniAbuRecruit[3]=paste(as.character(SizeCompIniAbuRecruitTemp[,"Sex"]),collapse=" ") 
      SizeCompIniAbuRecruit[4]=paste(as.character(SizeCompIniAbuRecruitTemp[,"SizeClass"]),collapse=" ") 
      
      SizeCompIniAbuRecruit[5]=paste(as.character(round(SizeCompIniAbuRecruitTemp[,"IniAbuSizeComp"]*InitialAbuScalertemp+0.5)),collapse=" ") #initial population
      SizeCompIniAbuRecruit[6]=paste(as.character(round(SizeCompIniAbuRecruitTemp[,"RecruitSizeComp"]*RecruitScalertemp+0.5)),collapse=" ") #recruitment
      
      # if(BRPSwitch1==0 & ProjectionSwitch1==0) #base case or adhoc BRP
      # {
      # temp=subset(InitialAbunRecruitSexRatio,Year==ThisYear & Timestep==ThisTimestepWYear)
      # SizeCompIniAbuRecruit[7]=temp[1,"CumulativeAbundance"]-(temp[1,"InitialAbundance"]+temp[1,"Recruitment"])+1
      # }
      # else if(BRPSwitch1==1 & BRPSwitch2==1) #base case or adhoc BRP
      # {
      # temp=subset(InitialAbunRecruitSexRatio,Year==ThisYear & Timestep==ThisTimestepWYear)
      # SizeCompIniAbuRecruit[7]=temp[1,"CumulativeAbundance"]-(temp[1,"InitialAbundance"]+temp[1,"Recruitment"])+1
      # }			
      # else if(BRPSwitch1==1 & BRPSwitch2==0 & BRPSwitch3==0) SizeCompIniAbuRecruit[7]=1 #theorical BRP with one cohort
      # else if(BRPSwitch1==1 & BRPSwitch2==0 & BRPSwitch3==1) SizeCompIniAbuRecruit[7]=yr*sum(SizeCompIniAbuRecruitTemp[,7])+1 #theorical BRP with multiple cohorts
      # if(ProjectionSwitch1==0) 
      # {
      SizeCompIniAbuRecruit[7]=CumulativeIntitialRecruit+1
      CumulativeIntitialRecruit=CumulativeIntitialRecruit+sum(round(SizeCompIniAbuRecruitTemp[,"IniAbuSizeComp"]*InitialAbuScalertemp+0.5))+sum(round(SizeCompIniAbuRecruitTemp[,"RecruitSizeComp"]*RecruitScalertemp+0.5))
      # }	
      # else if(ProjectionSwitch1==1)
      # {
      #SizeCompIniAbuRecruit[7]=CumulativeRecruitProjection+1
      #CumulativeRecruitProjection=CumulativeRecruitProjection+sum(round(SizeCompIniAbuRecruitTemp[,"RecruitSizeComp"]*RecruitScalertemp+0.5))
      # }
      SizeCompIniAbuRecruit[8]=NumLiveLobsta
      SizeCompIniAbuRecruit[9]="!end"
      write.table(SizeCompIniAbuRecruit,file="SizeCompIniAbuRecruit.txt",row.names=F,col.names=F, quote = F) 
      
      LifeHistoryControl=c()
      LifeHistoryControl[1]=as.character(NumTimestep) 
      LifeHistoryControl[2]=as.character(NumSizeClass)
      LifeHistoryControl[3]=as.character(SimulationSetting[2,2]) 
      LifeHistoryControl[4]=as.character(SimulationSetting[3,2]) 
      LifeHistoryControl[5]=paste(as.character(LSwitches[,2]),collapse=" ") 
      LifeHistoryControl[6]=as.character(NumArea) 
      
      if(BRPSwitch1==0 & ProjectionSwitch1==0) LifeHistoryControl[7]=paste(as.character(matrix(cumsumbydim(subset(Movement, Year==ThisYear & Timestep==ThisTimestepWYear)[,-c(1:3)],NumArea),nrow=1)),collapse=" ") 
      else if(BRPSwitch1==1 & BRPSwitch2==1) LifeHistoryControl[7]=paste(as.character(matrix(cumsumbydim(subset(Movement, Year==ThisYear & Timestep==ThisTimestepWYear)[,-c(1:3)],NumArea),nrow=1)),collapse=" ")		
      else if(BRPSwitch1==1 & BRPSwitch2==0) LifeHistoryControl[7]=paste(as.character(matrix(cumsumbydim(subset(MovementTheoBRP,Timestep==ThisTimestepWYear)[,-c(1:2)],NumArea),nrow=1)),collapse=" ")
      else if(ProjectionSwitch1==1) LifeHistoryControl[7]=paste(as.character(matrix(cumsumbydim(subset(MovementProject, Year==ThisYear & Timestep==ThisTimestepWYear)[,-c(1:3)],NumArea),nrow=1)),collapse=" ")
      
      if(BRPSwitch1==0 & ProjectionSwitch1==0) LifeHistoryControl[8]=paste(as.character(subset(MPAOutIn, Year==ThisYear)[,-1]),collapse=" ")
      else if(BRPSwitch1==1 & BRPSwitch2==1) LifeHistoryControl[8]=paste(as.character(subset(MPAOutIn, Year==ThisYear)[,-1]),collapse=" ")
      else if(BRPSwitch1==1 & BRPSwitch2==0) LifeHistoryControl[8]=paste(as.character(MPAOutInTheoBRP),collapse=" ")
      else if(ProjectionSwitch1==1) LifeHistoryControl[8]=paste(as.character(subset(MPAOutInProject, Year==ThisYear)[,-1]),collapse=" ")
      else if(ProjectionSwitch1==1 & BoxInSimtemp==0) LifeHistoryControl[8]=paste(as.character(subset(MPAOutInProject, Year==ThisYear)[,-1]),collapse=" ")
      else if(ProjectionSwitch1==1 & BoxInSimtemp>0) LifeHistoryControl[8]=paste(as.character(subset(MPAOutInProject, Year==ThisYear)[,-1]+ChangeMPAOutIn4NextYear),collapse=" ")					
      
      if(BRPSwitch1==0 & ProjectionSwitch1==0) LifeHistoryControl[9]=paste(as.character(subset(MPAInOut, Year==ThisYear)[,-1]),collapse=" ")
      else if(BRPSwitch1==1 & BRPSwitch2==1) LifeHistoryControl[9]=paste(as.character(subset(MPAInOut, Year==ThisYear)[,-1]),collapse=" ")
      else if(BRPSwitch1==1 & BRPSwitch2==0) LifeHistoryControl[9]=paste(as.character(MPAInOutTheoBRP),collapse=" ")
      else if(ProjectionSwitch1==1) LifeHistoryControl[9]=paste(as.character(subset(MPAInOutProject, Year==ThisYear)[,-1]),collapse=" ")				
      
      if(BRPSwitch1==0 & ProjectionSwitch1==0) LifeHistoryControl[10]=paste(as.character(subset(MPAMaxK, Year==ThisYear)[,-1]),collapse=" ")
      else if(BRPSwitch1==1 & BRPSwitch2==1) LifeHistoryControl[10]=paste(as.character(subset(MPAMaxK, Year==ThisYear)[,-1]),collapse=" ")
      else if(BRPSwitch1==1 & BRPSwitch2==0) LifeHistoryControl[10]=paste(as.character(MPAMaxKTheoBRP),collapse=" ")
      else if(ProjectionSwitch1==1 & BoxInSimtemp==0) LifeHistoryControl[10]=paste(as.character(subset(MPAMaxKProject, Year==ThisYear)[,-1]),collapse=" ")
      else if(ProjectionSwitch1==1 & BoxInSimtemp>0) LifeHistoryControl[10]=paste(as.character(subset(MPAMaxKProject, Year==ThisYear)[,-1]+ChangeMPAMaxK4NextYear),collapse=" ")			
      
      LifeHistoryControl[11]=paste(as.character(BioInMPA),collapse=" ")
      
      LifeHistoryControl[12]=paste(as.character(subset(LifeHistoryPara, Functions=="W-L")[,"Values"]),collapse=" ")
      LifeHistoryControl[13]=paste(as.character(subset(LifeHistoryPara, Functions=="Mature")[,"Values"]),collapse=" ")
      if(ProjectionSwitch1==0) set.seed(7)
      
      if(BRPSwitch1==0 & ProjectionSwitch1==0) LifeHistoryControl[14]=paste(as.character(randvector(subset(NaturalM, Timestep==ThisTimestepWYear)[,c("mean","lbd","ubd","sd")],LSwitches[3,2])*subset(NaturalM, Timestep==ThisTimestepWYear)[,c(paste("MWeight_",ThisYear,sep=""))]),collapse=" ")
      else if(BRPSwitch1==1 & BRPSwitch2==1) LifeHistoryControl[14]=paste(as.character(randvector(subset(NaturalM, Timestep==ThisTimestepWYear)[,c("mean","lbd","ubd","sd")],LSwitches[3,2])*subset(NaturalM, Timestep==ThisTimestepWYear)[,c(paste("MWeight_",ThisYear,sep=""))]),collapse=" ")
      else if(BRPSwitch1==1 & BRPSwitch2==0) LifeHistoryControl[14]=paste(as.character(randvector(subset(NaturalMTheoBRP, Timestep==ThisTimestepWYear)[,c("mean","lbd","ubd","sd")],LSwitches[3,2])*subset(NaturalMTheoBRP, Timestep==ThisTimestepWYear)[,c("MWeight")]),collapse=" ")
      else if(ProjectionSwitch1==1) LifeHistoryControl[14]=paste(as.character(randvector(subset(NaturalMProject, Timestep==ThisTimestepWYear)[,c("mean","lbd","ubd","sd")],LSwitches[3,2])*subset(NaturalMProject, Timestep==ThisTimestepWYear)[,c(paste("MWeight_",ThisYear,sep=""))]),collapse=" ")
      
      LifeHistoryControl[15]=paste(as.character(subset(BerriedProb, Timestep==ThisTimestepWYear)[,"ProbBerried"]),collapse=" ")
      LifeHistoryControl[16]=paste(as.character(data.matrix(t(subset(HatchProb, Timestep==ThisTimestepWYear)[,4:(3+NumTimestep)]))),collapse=" ")
      LifeHistoryControl[17]=paste(as.character(BerriedMoltRelation[,3]),collapse=" ")
      LifeHistoryControl[18]=paste(as.character(BerriedMoltRelation[,4]),collapse=" ")
      LifeHistoryControl[19]=as.character(SimulationSetting[5,2])
      LifeHistoryControl[20]=paste(as.character(subset(MoltProb, Timestep==ThisTimestepWYear)[,5]),collapse=" ")
      for(i in 1:SimulationSetting[5,2]) LifeHistoryControl[20+i]=paste(as.character(subset(MoltProb, Timestep==ThisTimestepWYear)[,(6+i)]),collapse=" ")
      LifeHistoryControl[21+SimulationSetting[5,2]]=paste(as.character(subset(MoltProb, Timestep==ThisTimestepWYear)[,"ProbDoubleMolt"]),collapse=" ")
      LifeHistoryControl[22+SimulationSetting[5,2]]=as.character(SimulationSetting[6,2])
      LifeHistoryControl[23+SimulationSetting[5,2]]=as.character(SimulationSetting[7,2])
      temp=c()
      for(i in 1:SimulationSetting[7,2]) temp=rbind(temp,subset(MoltIncrement, Timestep==ThisTimestepWYear)[,(5+i)])
      temp=apply(temp,2,cumsum)
      for(i in 1:SimulationSetting[7,2]) LifeHistoryControl[23+SimulationSetting[5,2]+i]=paste(as.character(temp[i,]),collapse=" ")
      LifeHistoryControl[24+SimulationSetting[5,2]+SimulationSetting[7,2]]=ProjectionSwitch1		
      LifeHistoryControl[25+SimulationSetting[5,2]+SimulationSetting[7,2]]="!end"
      write.table(LifeHistoryControl,file="LifeHistoryControl.txt",row.names=F,col.names=F, quote = F)
      
      ManagementControl=c()
      ManagementControl[1]=as.character(ThisTimestep)
      ManagementControl[2]=as.character(ThisTimestepWYear)			
      ManagementControl[3]=paste(as.character(MSwitches[,2]),collapse=" ") 
      ManagementControl[4]=as.character(ManagementSetting[5,2])
      ManagementControl[5]=as.character(ManagementSetting[6,2])
      ManagementControl[6]=as.character(ManagementSetting[7,2])
      
      if(BRPSwitch1==0 & ProjectionSwitch1==0)
      {    
        ManagementControl[7]=paste(as.character(subset(EncounterRate,Timestep==ThisTimestepWYear)[,c(paste("EnRate_",ThisYear,sep=""))]),collapse=" ")
        ManagementControl[8]=paste(as.character(subset(MinMaxLegalSize,Legal=="Min")[,c(paste("Size_",ThisYear,sep=""))]),collapse=" ")
        ManagementControl[9]=paste(as.character(subset(MinMaxLegalSize,Legal=="Max")[,c(paste("Size_",ThisYear,sep=""))]),collapse=" ")
        ManagementControl[10]=paste(as.character(TAC[,c(paste("TAC_",ThisYear,sep=""))]),collapse=" ")
        
        ManagementControl[11]=paste(as.character(HandlingM[,c(paste("HandlingM_",ThisYear,sep=""))]),collapse=" ")
        ManagementControl[12]=paste(as.character(matrix(t(apply(matrix(FisheryProb[FisheryProb$Timestep==ThisTimestepWYear,c(paste("FisheryProb_",ThisYear,sep=""))],ncol=ManagementSetting[7,2]),1,cumsum)),ncol=1)),collapse=" ")
      }
      else if(BRPSwitch1==1 & BRPSwitch2==1)
      {    
        ManagementControl[7]=paste(as.character(subset(EncounterRate,Timestep==ThisTimestepWYear)[,c(paste("EnRate_",ThisYear,sep=""))]),collapse=" ")
        ManagementControl[8]=paste(as.character(subset(MinMaxLegalSize,Legal=="Min")[,c(paste("Size_",ThisYear,sep=""))]),collapse=" ")
        ManagementControl[9]=paste(as.character(subset(MinMaxLegalSize,Legal=="Max")[,c(paste("Size_",ThisYear,sep=""))]),collapse=" ")
        ManagementControl[10]=paste(as.character(TAC[,c(paste("TAC_",ThisYear,sep=""))]),collapse=" ")  			    
        ManagementControl[11]=paste(as.character(HandlingM[,c(paste("HandlingM_",ThisYear,sep=""))]),collapse=" ")
        ManagementControl[12]=paste(as.character(matrix(t(apply(matrix(FisheryProb[FisheryProb$Timestep==ThisTimestepWYear,c(paste("FisheryProb_",ThisYear,sep=""))],ncol=ManagementSetting[7,2]),1,cumsum)),ncol=1)),collapse=" ")
      }			
      else if(BRPSwitch1==1 & BRPSwitch2==0) 
      { 
        ManagementControl[7]=paste(as.character(rep(fBRPtemp*EnRateTimestepTheoBRP[ThisTimestepWYear,"Value"],NumArea*NumSizeClass*2)),collapse=" ")
        ManagementControl[8]=paste(as.character(subset(MinMaxLegalSizeTheoBRP,Legal=="Min")[,4]),collapse=" ")
        ManagementControl[9]=paste(as.character(subset(MinMaxLegalSizeTheoBRP,Legal=="Max")[,4]),collapse=" ")
        ManagementControl[10]=paste(as.character(rep(0,NumArea*ManagementSetting[7,2])),collapse=" ")  				
        ManagementControl[11]=paste(as.character(HandlingMTheoBRP[,c("HandlingM")]),collapse=" ")
        ManagementControl[12]=paste(as.character(matrix(t(apply(matrix(FisheryProbTheoBRP[FisheryProbTheoBRP$Timestep==ThisTimestepWYear,c("FisheryProb")],ncol=ManagementSetting[7,2]),1,cumsum)),ncol=1)),collapse=" ")
      }
      else if(ProjectionSwitch1==1 & BoxInSimtemp==0) 
      {
        ManagementControl[4]=as.character(VNotchProject[1,2])
        ManagementControl[7]=paste(as.character(subset(EncounterRateProject,Timestep==ThisTimestepWYear)[,c(paste("EnRate_",ThisYear,sep=""))]),collapse=" ")
        ManagementControl[8]=paste(as.character(subset(MinMaxLegalSizeProject,Legal=="Min")[,c(paste("Size_",ThisYear,sep=""))]),collapse=" ")
        ManagementControl[9]=paste(as.character(subset(MinMaxLegalSizeProject,Legal=="Max")[,c(paste("Size_",ThisYear,sep=""))]),collapse=" ")
        ManagementControl[10]=paste(as.character(TACProject[,c(paste("TAC_",ThisYear,sep=""))]),collapse=" ")			    
        ManagementControl[11]=paste(as.character(HandlingMProject[,c(paste("HandlingM_",ThisYear,sep=""))]),collapse=" ")
        ManagementControl[12]=paste(as.character(matrix(t(apply(matrix(FisheryProbProject[FisheryProbProject$Timestep==ThisTimestepWYear,c(paste("FisheryProb_",ThisYear,sep=""))],ncol=ManagementSetting[7,2]),1,cumsum)),ncol=1)),collapse=" ")			
      }
      else if(ProjectionSwitch1==1 & BoxInSimtemp>0) 
      {
        EncounterRateProjecttemp=subset(EncounterRateProject,Timestep==ThisTimestepWYear)[,c(paste("EnRate_",ThisYear,sep=""))]+ChangeEncounterRate4NextYear
        if(ThisTimestepWYear==CloseFishingTimestep4NextYear) EncounterRateProjecttemp=rep(0,length(EncounterRateProjecttemp))
        else if(max(EncounterRateProjecttemp)>1) temp=EncounterRateProjecttemp/max(EncounterRateProjecttemp)
        ManagementControl[7]=paste(as.character(EncounterRateProjecttemp),collapse=" ")
        ManagementControl[4]=as.character(VNotchProject[1,2])
        ManagementControl[8]=paste(as.character(subset(MinMaxLegalSizeProject,Legal=="Min")[,c(paste("Size_",ThisYear,sep=""))]+ChangeMinLegalSize4NextYear),collapse=" ")
        ManagementControl[9]=paste(as.character(subset(MinMaxLegalSizeProject,Legal=="Max")[,c(paste("Size_",ThisYear,sep=""))]+ChangeMaxLegalSize4NextYear),collapse=" ")
        ManagementControl[10]=paste(as.character(TACProject[,c(paste("TAC_",ThisYear,sep=""))]+ChangeTAC4NextYear),collapse=" ")					
        ManagementControl[11]=paste(as.character(HandlingMProject[,c(paste("HandlingM_",ThisYear,sep=""))]),collapse=" ")
        ManagementControl[12]=paste(as.character(matrix(t(apply(matrix(FisheryProbProject[FisheryProbProject$Timestep==ThisTimestepWYear,c(paste("FisheryProb_",ThisYear,sep=""))],ncol=ManagementSetting[7,2]),1,cumsum)),ncol=1)),collapse=" ")			
      }
      
      ManagementControl[13]="!end"
      write.table(ManagementControl,file="ManagementControl.txt",row.names=F,col.names=F, quote = F)
      
      if(BRPSwitch1==0 & ProjectionSwitch1==0 & ModelFunctionSwitch!=2) print(paste("Historical simulation:",ThisTimestep,"/",TotalTimestep,sep=""))
      else if(BRPSwitch1==1 & BRPSwitch2==1) print(paste("BRP AD HOC approach:",ThisTimestep,"/",TotalTimestep,sep=""))
      else if(BRPSwitch1==1 & BRPSwitch2==0 &  is.null(fBRPtemp)==TRUE) print(paste("BRP theoretical approach:",ThisTimestep,"/",TotalTimestep,sep=""))
      else if(BRPSwitch1==1 & BRPSwitch2==0 &  is.null(fBRPtemp)==FALSE) print(paste("BRP theoretical approach:",ThisTimestep,"/",TotalTimestep," F:",fBRPtemp,sep=""))			
      else if(ProjectionSwitch1==1) print(paste("Projection:",ThisTimestep,"/",TotalTimestep," Scenario:",pscen," Iteration:",piter,sep=""))
      #else if(ModelFunctionSwitch==2) print(paste("Tune model:",ThisTimestep,"/",TotalTimestep," InitialAbuScaler:",InitialAbuScalertemp," RecruitScaler:",RecruitScalertemp," EnrateScaler:",EnrateScalertemp," Combination:",tg,"/",NumOfTunning,sep=""))
      else if(ModelFunctionSwitch==2) print(paste("Tune model:",ThisTimestep,"/",TotalTimestep," InitialAbuScaler:",InitialAbuScalertemp," RecruitScaler:",RecruitScalertemp," EnrateScaler_t1:",EnrateScaler_t1temp,"EnrateScaler_t2:",EnrateScaler_t2temp,"EnrateScaler_t3:",EnrateScaler_t3temp, "EnrateScaler_t4",EnrateScaler_t4temp, " Combination:",tg,"/",NumOfTunning,sep=""))
      
      
      BoxInSimtemp=0 # the actions had applied to the fishery, clean it and evaluate whether the fishery status will trigger the action or not
      
      #　system("LobsterSimulator_v5.exe")
      system("test.exe")
      
      if(LSwitches[4,2]==0)
      {
        file.copy("LobstaRecord.txt", paste("LobstaRecord_",ThisTimestep,".txt",sep=""),overwrite = TRUE ) 
        file.rename("LobstaRecordLive.txt", paste("LobstaRecordLive_",ThisTimestep,".txt",sep="")) #lobsters that are alive at the beginning of timestep
        file.rename("LobstaRecordDie.txt", paste("LobstaRecordDie_",ThisTimestep,".txt",sep="")) #lobsters that die at the end of timestep
      }
      
      LobstaRecordSummary=scan(file="LobstaRecordSummary.txt",sep="\n",what="character",blank.lines.skip=T)
      LobstaRecordSummaryList=list()
      dim1=1
      dim2=1
      if(ManagementSetting[8,2]!=1 & ThisTimestepWYear<ManagementSetting[8,2]) FishingYear=ThisYear-1
      else FishingYear=ThisYear
      
      for(j in 1:22)
      {
        dim1=dim2+1
        
        if(j<=14)
        {
          dim2=dim1+SummaryLength1-1
          RestData=data.frame(Area=rep(paste("Area",1:NumArea),each=2*2*NumSizeClass),MPA=rep(c("In MPA","Out MPA"),time=NumArea,each=2*NumSizeClass),Sex=rep(c("Female","Male"),time=NumArea*2,each=NumSizeClass),SizeClass=SimulationSizeClass,Year=FishingYear,CalendarYear=ThisYear,Timestep=ThisTimestepWYear,ThisTimestep=ThisTimestep)
          LobstaRecordSummaryList[[j]]=data.frame(Quantity=as.numeric(unlist(lapply(strsplit(LobstaRecordSummary[dim1:dim2], " "),"[",-1)))*SimulationSetting[8,2],Name=SummaryList[j],RestData)
          
          if(ThisTimestepWYear==SimulationTimestepEndtemp & ProjectionSwitch1==1)
          {
            if(j==1 & BRPSettingProject[3,2]==0) #Abundance based BRP for projection				
            {
              AbuBRPInSimtemp=sum(subset(LobstaRecordSummaryList[[j]],SizeClass>=BRPSettingProject[1,2] & SizeClass<=BRPSettingProject[2,2])[,"Quantity"])
            }
            if(j==2 & BRPSettingProject[3,2]==1) #Biomass based BRP for projection				
            {
              BioBRPInSimtemp=sum(subset(LobstaRecordSummaryList[[j]],SizeClass>=BRPSettingProject[1,2] & SizeClass<=BRPSettingProject[2,2])[,"Quantity"])
            }
          }
          
          if(PlotSwitches[1,2]==0 || PlotSwitches[1,2]==2) 
          {
            if((j==13 || j==14) & sum(subset(MoltProb, Timestep==ThisTimestepWYear)[,"ProbDoubleMolt"])==0)
            {
              dim2=dim2+1
              next
            }
            else print(ggplot(LobstaRecordSummaryList[[j]],aes(SizeClass,Quantity,group=Sex,colour = Sex,linetype=Sex))+geom_line(size=1)+scale_color_manual(values=c("Red", "Blue"))+facet_grid(Area~MPA)+labs(x="Size Class",y=SummaryList[j])+labs(title=paste(SummaryList[j],"for",ThisYear,TimestepSetting[ThisTimestepWYear,2],"(Time step:",ThisTimestep,")"))+theme_bw())
          }
        }
        else
        {
          dim2=dim1+SummaryLength2-1
          RestData=data.frame(Fishery=rep(paste("Fishery",1:ManagementSetting[7,2]),each=NumArea*2*NumSizeClass),Area=rep(paste("Area",1:NumArea),time=ManagementSetting[7,2],each=2*NumSizeClass),Sex=rep(c("Female","Male"),time=ManagementSetting[7,2]*NumArea,each=NumSizeClass),SizeClass=SimulationSizeClass,Year=FishingYear,CalendarYear=ThisYear,Timestep=ThisTimestepWYear,ThisTimestep=ThisTimestep)
          LobstaRecordSummaryList[[j]]=data.frame(Quantity=as.numeric(unlist(lapply(strsplit(LobstaRecordSummary[dim1:dim2], " "),"[",-1)))*SimulationSetting[8,2],Name=SummaryList[j],RestData)
          
          if(ProjectionSwitch1==1)
          {
            if(j==17 & BRPSettingProject[3,2]==0) #Catch Abundance based BRP for projection				
            {
              CatchAbuBRPInSimtemp=CatchAbuBRPInSimtemp+sum(LobstaRecordSummaryList[[j]][,"Quantity"])
            }
            if(j==18 & BRPSettingProject[3,2]==1) #Catch Biomass based BRP for projection				
            {
              CatchBioBRPInSimtemp=CatchBioBRPInSimtemp+sum(LobstaRecordSummaryList[[j]][,"Quantity"])
            }
          }
          if(PlotSwitches[1,2]==0 || PlotSwitches[1,2]==2)
          {
            if((j==15 || j==16) & MSwitches[1,2]==0)
            {
              dim2=dim2+1
              next
            }
            else if((j==19 || j==20) & sum(as.numeric(strsplit(ManagementControl[9]," ")[[1]]))==0)
            {
              dim2=dim2+1
              next
            }
            else print(ggplot(LobstaRecordSummaryList[[j]],aes(SizeClass,Quantity,group=Sex,colour = Sex,linetype=Sex))+geom_line(size=1)+scale_color_manual(values=c("Red", "Blue"))+ labs(x="Size Class",y=SummaryList[j])+labs(title=paste(SummaryList[j],"for",ThisYear,TimestepSetting[ThisTimestepWYear,2],"(Time step:",ThisTimestep,")"))+theme_bw())
          }						
        }
        dim2=dim2+1
      }
      
      Summary1=rbind(Summary1,do.call("rbind", LobstaRecordSummaryList[c(1:14)]))
      Summary2=rbind(Summary2,do.call("rbind", LobstaRecordSummaryList[c(15:22)]))
      
      NumLiveLobsta=as.numeric(LobstaRecordSummary[[dim2+1]]) #count how many lobsters are alive
      BioInMPA=as.numeric(LobstaRecordSummary[[dim2+3]]) #Biomass inside MPA from the previous timestep
      
      if(ThisTimestepWYear==SimulationTimestepEndtemp & ProjectionSwitch1==1) #evaluate performance of management strategies in projections 
      {
        if(BRPSettingProject[4,2]==1)
        {			
          if(BRPSettingProject[3,2]==0) 
          {
            FMSYBRPInSimtemp=CatchAbuBRPInSimtemp/(AbuBRPInSimtemp+CatchAbuBRPInSimtemp)
            BBRPInSimtemp=AbuBRPInSimtemp
          }
          if(BRPSettingProject[3,2]==1) 
          {
            FMSYBRPInSimtemp=CatchBioBRPInSimtemp/(BioBRPInSimtemp+CatchBioBRPInSimtemp)
            BBRPInSimtemp=BioBRPInSimtemp
          }	
        }
        else if(BRPSettingProject[4,2]==0)
        {
          if(BRPSettingProject[3,2]==0) 
          {
            FMSYBRPInSimtemp=CatchAbuBRPInSimtemp
            BBRPInSimtemp=AbuBRPInSimtemp
          }
          if(BRPSettingProject[3,2]==1) 
          {
            FMSYBRPInSimtemp=CatchBioBRPInSimtemp
            BBRPInSimtemp=BioBRPInSimtemp
          }	
        }
        
        FMSYBRPDividTargetInSimtemp=FMSYBRPInSimtemp/BRPSettingProject[5,2]
        BBRPDividTargetInSimtemp=BBRPInSimtemp/BRPSettingProject[7,2]
        FMSYBRPDividLimitInSimtemp=FMSYBRPInSimtemp/BRPSettingProject[6,2]
        BBRPDividLimitInSimtemp=BBRPInSimtemp/BRPSettingProject[8,2]
        
        if(BBRPDividLimitInSimtemp<=1 & FMSYBRPDividLimitInSimtemp>=1) BoxInSimtemp=1 #1-1 
        if(BBRPDividLimitInSimtemp<=1 & FMSYBRPDividTargetInSimtemp>=1 & FMSYBRPDividLimitInSimtemp<1) BoxInSimtemp=2 #1-2
        if(BBRPDividLimitInSimtemp<=1 & FMSYBRPDividTargetInSimtemp<1) BoxInSimtemp=3 #1-3
        if(BBRPDividTargetInSimtemp<=1 & BBRPDividLimitInSimtemp>1 & FMSYBRPDividLimitInSimtemp>=1) BoxInSimtemp=4 #2-1
        if(BBRPDividTargetInSimtemp<=1 & BBRPDividLimitInSimtemp>1 & FMSYBRPDividTargetInSimtemp>=1 & FMSYBRPDividLimitInSimtemp<1) BoxInSimtemp=5 #2-2
        if(BBRPDividTargetInSimtemp<=1 & BBRPDividLimitInSimtemp>1 & FMSYBRPDividTargetInSimtemp<1) BoxInSimtemp=6 #2-3
        if(BBRPDividTargetInSimtemp>1 & FMSYBRPDividLimitInSimtemp>=1) BoxInSimtemp=7 #3-1
        if(BBRPDividTargetInSimtemp>1 & FMSYBRPDividTargetInSimtemp>=1 & FMSYBRPDividLimitInSimtemp<1) BoxInSimtemp=8 #3-2
        if(BBRPDividTargetInSimtemp>1 & FMSYBRPDividTargetInSimtemp<1) BoxInSimtemp=9 #3-3			
        
        if(PreviousBoxInSimtemp==BoxInSimtemp) CumulativeBoxInSimtemp=CumulativeBoxInSimtemp+1
        else CumulativeBoxInSimtemp=0
        
        if(CumulativeBoxInSimtemp==0 | CumulativeBoxInSimtemp==YearToApplyAdaptiveManagement[BoxInSimtemp,2])
        {
          ChangeMinLegalSize4NextYear=ChangeMinLegalSize4NextYear+AdaptiveManagementStrategy[BoxInSimtemp,2]
          if(AdaptiveManagementStrategy[BoxInSimtemp,2]!=0)
            print(paste("year:",yr," Fishery status:",FisheryStatus[BoxInSimtemp]," Change minimum legal size in projection by ",AdaptiveManagementStrategy[BoxInSimtemp,2],sep=""))
        }
        if(CumulativeBoxInSimtemp==0 | CumulativeBoxInSimtemp==YearToApplyAdaptiveManagement[BoxInSimtemp,3])
        {
          ChangeMaxLegalSize4NextYear=ChangeMaxLegalSize4NextYear+AdaptiveManagementStrategy[BoxInSimtemp,3]
          if(AdaptiveManagementStrategy[BoxInSimtemp,3]!=0)
            print(paste("year:",yr," Fishery status:",FisheryStatus[BoxInSimtemp]," Change maximum legal size in projection by ",AdaptiveManagementStrategy[BoxInSimtemp,3],sep=""))
        }	
        if(CumulativeBoxInSimtemp==0 | CumulativeBoxInSimtemp==YearToApplyAdaptiveManagement[BoxInSimtemp,4]) 
        {
          ChangeMPAOutIn4NextYear=ChangeMPAOutIn4NextYear+AdaptiveManagementStrategy[BoxInSimtemp,4]
          if(AdaptiveManagementStrategy[BoxInSimtemp,4]!=0)
            print(paste("year:",yr," Fishery status:",FisheryStatus[BoxInSimtemp]," Change probability of entering MPA by ",AdaptiveManagementStrategy[BoxInSimtemp,4],sep=""))
        }	
        if(CumulativeBoxInSimtemp==0 | CumulativeBoxInSimtemp==YearToApplyAdaptiveManagement[BoxInSimtemp,5]) 
        {
          ChangeMPAMaxK4NextYear=ChangeMPAMaxK4NextYear+AdaptiveManagementStrategy[BoxInSimtemp,5]
          if(AdaptiveManagementStrategy[BoxInSimtemp,5]!=0)
            print(paste("year:",yr," Fishery status:",FisheryStatus[BoxInSimtemp]," Change carrying capacity of MPA by ",AdaptiveManagementStrategy[BoxInSimtemp,5],sep=""))
        }	
        if(CumulativeBoxInSimtemp==0 | CumulativeBoxInSimtemp==YearToApplyAdaptiveManagement[BoxInSimtemp,6]) 
        {
          ChangeTAC4NextYear=ChangeTAC4NextYear+AdaptiveManagementStrategy[BoxInSimtemp,6]
          if(AdaptiveManagementStrategy[BoxInSimtemp,6]!=0)
            print(paste("year:",yr," Fishery status:",FisheryStatus[BoxInSimtemp]," Change TAC by ",AdaptiveManagementStrategy[BoxInSimtemp,6],sep=""))
        }	
        if(CumulativeBoxInSimtemp==0 | CumulativeBoxInSimtemp==YearToApplyAdaptiveManagement[BoxInSimtemp,7]) 
        {
          ChangeEncounterRate4NextYear=ChangeEncounterRate4NextYear+AdaptiveManagementStrategy[BoxInSimtemp,7]
          if(AdaptiveManagementStrategy[BoxInSimtemp,7]!=0)
            print(paste("year:",yr," Fishery status:",FisheryStatus[BoxInSimtemp]," Change encounter rate by ",AdaptiveManagementStrategy[BoxInSimtemp,7],sep=""))
        }	
        if(CumulativeBoxInSimtemp==0 | CumulativeBoxInSimtemp==YearToApplyAdaptiveManagement[BoxInSimtemp,8]) 
        {
          CloseFishingTimestep4NextYear=AdaptiveManagementStrategy[BoxInSimtemp,8]
          if(AdaptiveManagementStrategy[BoxInSimtemp,8]!=0)
            print(paste("year:",yr," Fishery status:",FisheryStatus[BoxInSimtemp]," Close fishing time step:",AdaptiveManagementStrategy[BoxInSimtemp,8],sep=""))
        }
        
        ChangeInManagementStrategy4NextYear=rbind(ChangeInManagementStrategy4NextYear,data.frame(Year=FishingYear,FisheryStatus=FisheryStatus[BoxInSimtemp],ChangeMinLegalSize4NextYear=ChangeMinLegalSize4NextYear,ChangeMaxLegalSize4NextYear=ChangeMaxLegalSize4NextYear,ChangeMPAOutIn4NextYear=ChangeMPAOutIn4NextYear,ChangeMPAMaxK4NextYear=ChangeMPAMaxK4NextYear,ChangeTAC4NextYear=ChangeTAC4NextYear,ChangeEncounterRate4NextYear=ChangeEncounterRate4NextYear,CloseFishingTimestep4NextYear=CloseFishingTimestep4NextYear))
        
        PreviousBoxInSimtemp=BoxInSimtemp
      }
      
      #get recruitment and SSB for each timestep until the end of last year
      if(yr!=length(SimulationYeartemp) || (ManagementSetting[8,2]!=1 && ThisTimestepWYear<ManagementSetting[8,2]-1) ||  (ManagementSetting[8,2]==1 && ThisTimestepWYear<ManagementSetting[8,2]))
      {		
        LobstaRecordSummaryListFemale=lapply(LobstaRecordSummaryList,subset,Sex=="Female")
        TimestepSSBAbu=rbind(TimestepSSBAbu,aggregate(LobstaRecordSummaryListFemale[[7]]["Quantity"],by=as.list(LobstaRecordSummaryListFemale[[7]][,c("Name","Area","MPA","Year","Timestep","ThisTimestep")]),sum))
        TimestepSSBBio=rbind(TimestepSSBBio,aggregate(LobstaRecordSummaryListFemale[[8]]["Quantity"],by=as.list(LobstaRecordSummaryListFemale[[8]][,c("Name","Area","MPA","Year","Timestep","ThisTimestep")]),sum))  
      }
      
      if(LSwitches[4,2]==0) file.rename("LobstaRecordSummary.txt", paste("LobstaRecordSummary_",ThisTimestep,".txt",sep=""))      		
      ThisTimestep<<-ThisTimestep+1
      CumulativeIntitialRecruit<<-CumulativeIntitialRecruit			
    }				
  }
  
  if(ProjectionSwitch1==0)
  {
    NumLiveLobstaProjection<<-NumLiveLobsta
    BioInMPAProjection<<-BioInMPA
  }
  
  if(ProjectionSwitch1==1) ChangeInManagementStrategy4NextYear<<-ChangeInManagementStrategy4NextYear	
  
  TimestepSSBAbu<<-TimestepSSBAbu
  TimestepSSBBio<<-TimestepSSBBio
  
  Summary1<<-Summary1
  Summary2<<-Summary2
  
}

SimulationRegularResult=function(TunningSwitch1)
{	
  
  ##Annual and timestep summary
  
  #Annual abundance, biomass, legal size, and ssb are represented using quantity at the end of the year and the rest are the sum over year
  Summary1Live=Summary1[Summary1$Name %in% SummaryList[c(1,2,3,4,7,8)],]
  Summary1Die=Summary1[!(Summary1$Name %in% SummaryList[c(1,2,3,4,7,8)]),]	
  
  AnnualSummary1Live=aggregate(Summary1Live$Quantity,by=as.list(Summary1Live[,c("Name","Area","MPA","Sex", "Year","Timestep")]),sum)
  if(ManagementSetting[8,2]>1) AnnualSummary1Live=subset(AnnualSummary1Live,Timestep==(ManagementSetting[8,2]-1))[,c("x","Name","Area","MPA","Sex", "Year")]
  else AnnualSummary1Live=subset(AnnualSummary1Live,Timestep==1)[,c("x","Name","Area","MPA","Sex", "Year")]	
  AnnualSummary1Die=aggregate(Summary1Die$Quantity,by=as.list(Summary1Die[,c("Name","Area","MPA","Sex", "Year")]),sum)
  
  AnnualSummary1=rbind(AnnualSummary1Live,AnnualSummary1Die)
  if(ManagementSetting[3,2]!=ManagementSetting[8,2]) AnnualSummary1=subset(AnnualSummary1,Year>min(Summary1$Year))
  if(ManagementSetting[8,2]>1 && ManagementSetting[4,2]!=(ManagementSetting[8,2]-1)) AnnualSummary1=subset(AnnualSummary1,Year<max(Summary1$Year))
  
  TimestepSummary1=aggregate(Summary1$Quantity,by=as.list(Summary1[,c("Name","Area","MPA","Sex","Year","Timestep","ThisTimestep")]),sum)
  
  AnnualSummary2=aggregate(Summary2$Quantity,by=as.list(Summary2[,c("Name","Fishery","Area","Sex","Year")]),sum)
  if(ManagementSetting[3,2]!=ManagementSetting[8,2]) AnnualSummary2=subset(AnnualSummary2,Year>min(Summary1$Year))
  if(ManagementSetting[8,2]>1 && ManagementSetting[4,2]!=(ManagementSetting[8,2]-1)) AnnualSummary2=subset(AnnualSummary2,Year<max(Summary1$Year))
  
  TimestepSummary2=aggregate(Summary2$Quantity,by=as.list(Summary2[,c("Name","Fishery","Area","Sex","Year","Timestep","ThisTimestep")]),sum)
  
  AnnualSummary1$Year=as.factor(AnnualSummary1$Year)
  AnnualSummary2$Year=as.factor(AnnualSummary2$Year)
  TimestepSummary1$Year=as.factor(TimestepSummary1$Year)
  TimestepSummary2$Year=as.factor(TimestepSummary2$Year)
  TimestepSummary1$Timestep=as.factor(TimestepSummary1$Timestep)
  TimestepSummary2$Timestep=as.factor(TimestepSummary2$Timestep)
  TimestepSummary1$ThisTimestep=as.factor(TimestepSummary1$ThisTimestep)
  TimestepSummary2$ThisTimestep=as.factor(TimestepSummary2$ThisTimestep)
  
  ##Exploitation rate
  
  AnnualLiveLegalAbuOutMPA=subset(AnnualSummary1,Name=="Legal Abundance (number)" & MPA=="Out MPA")
  names(AnnualLiveLegalAbuOutMPA)[names(AnnualLiveLegalAbuOutMPA)=="x"]="AnLiLegalAb"
  TimestepLiveLegalAbuOutMPA=subset(TimestepSummary1,Name=="Legal Abundance (number)" & MPA=="Out MPA")
  names(TimestepLiveLegalAbuOutMPA)[names(TimestepLiveLegalAbuOutMPA)=="x"]="TiLiLegalAb"
  AnnualLiveLegalBioOutMPA=subset(AnnualSummary1,Name=="Legal Biomass (weight)" & MPA=="Out MPA")
  names(AnnualLiveLegalBioOutMPA)[names(AnnualLiveLegalBioOutMPA)=="x"]="AnLiLegalBi"
  TimestepLiveLegalBioOutMPA=subset(TimestepSummary1,Name=="Legal Biomass (weight)" & MPA=="Out MPA")
  names(TimestepLiveLegalBioOutMPA)[names(TimestepLiveLegalBioOutMPA)=="x"]="TiLiLegalBi"
  
  AnnualDieLegalAbuOutMPA=subset(AnnualSummary1,Name=="Dead Legal Abundance (number)" & MPA=="Out MPA")
  names(AnnualDieLegalAbuOutMPA)[names(AnnualDieLegalAbuOutMPA)=="x"]="AnDiLegalAb"
  TimestepDieLegalAbuOutMPA=subset(TimestepSummary1,Name=="Dead Legal Abundance (number)" & MPA=="Out MPA")
  names(TimestepDieLegalAbuOutMPA)[names(TimestepDieLegalAbuOutMPA)=="x"]="TiDiLegalAb"
  AnnualDieLegalBioOutMPA=subset(AnnualSummary1,Name=="Dead Legal Biomass (weight)" & MPA=="Out MPA")
  names(AnnualDieLegalBioOutMPA)[names(AnnualDieLegalBioOutMPA)=="x"]="AnDiLegalBi"
  TimestepDieLegalBioOutMPA=subset(TimestepSummary1,Name=="Dead Legal Biomass (weight)" & MPA=="Out MPA")
  names(TimestepDieLegalBioOutMPA)[names(TimestepDieLegalBioOutMPA)=="x"]="TiDiLegalBi"
  
  AnnualLegalAbuOutMPA=merge(AnnualLiveLegalAbuOutMPA[,c("Area","Sex","Year","AnLiLegalAb")],AnnualDieLegalAbuOutMPA[,c("Area","Sex","Year","AnDiLegalAb")])
  TimestepLegalAbuOutMPA=merge(TimestepLiveLegalAbuOutMPA[,c("Area","Sex","Year","Timestep","ThisTimestep","TiLiLegalAb")],TimestepDieLegalAbuOutMPA[,c("Area","Sex","Year","Timestep","ThisTimestep","TiDiLegalAb")])
  
  AnnualLegalBioOutMPA=merge(AnnualLiveLegalBioOutMPA[,c("Area","Sex","Year","AnLiLegalBi")],AnnualDieLegalBioOutMPA[,c("Area","Sex","Year","AnDiLegalBi")])
  TimestepLegalBioOutMPA=merge(TimestepLiveLegalBioOutMPA[,c("Area","Sex","Year","Timestep","ThisTimestep","TiLiLegalBi")],TimestepDieLegalBioOutMPA[,c("Area","Sex","Year","Timestep","ThisTimestep","TiDiLegalBi")])
  
  AnnualLandedCatchNumber=subset(AnnualSummary2,Name=="Landed Catch (number)")
  TimestepLandedCatchNumber=subset(TimestepSummary2,Name=="Landed Catch (number)")	
  AnnualLandedCatchWeight=subset(AnnualSummary2,Name=="Landed Catch (weight)")
  TimestepLandedCatchWeight=subset(TimestepSummary2,Name=="Landed Catch (weight)")
  
  AnnualExploitNumber=merge(AnnualLandedCatchNumber,AnnualLegalAbuOutMPA)
  AnnualExploitNumber$LandedCatchNumber=AnnualExploitNumber$x
  AnnualExploitNumber$AnnualExploitNumber=AnnualExploitNumber$LandedCatchNumber/(AnnualExploitNumber$LandedCatchNumber+AnnualExploitNumber$AnLiLegalAb+AnnualExploitNumber$AnDiLegalAb)
  
  TimestepExploitNumber=merge(TimestepLandedCatchNumber,TimestepLegalAbuOutMPA)
  TimestepExploitNumber$LandedCatchNumber=TimestepExploitNumber$x
  TimestepExploitNumber$TimestepExploitNumber=TimestepExploitNumber$LandedCatchNumber/(TimestepExploitNumber$LandedCatchNumber+TimestepExploitNumber$TiLiLegalAb+TimestepExploitNumber$TiDiLegalAb)
  TimestepExploitNumber$TimestepExploitNumber[TimestepExploitNumber$TimestepExploitNumber=="NaN"]=0
  
  AnnualExploitWeight=merge(AnnualLandedCatchWeight,AnnualLegalBioOutMPA)
  AnnualExploitWeight$LandedCatchWeight=AnnualExploitWeight$x
  AnnualExploitWeight$AnnualExploitWeight=AnnualExploitWeight$LandedCatchWeight/(AnnualExploitWeight$LandedCatchWeight+AnnualExploitWeight$AnLiLegalBi+AnnualExploitWeight$AnDiLegalBi)
  
  TimestepExploitWeight=merge(TimestepLandedCatchWeight,TimestepLegalBioOutMPA)
  TimestepExploitWeight$LandedCatchWeight=TimestepExploitWeight$x
  TimestepExploitWeight$TimestepExploitWeight=TimestepExploitWeight$LandedCatchWeight/(TimestepExploitWeight$LandedCatchWeight+TimestepExploitWeight$TiLiLegalBi+TimestepExploitWeight$TiDiLegalBi)
  TimestepExploitWeight$TimestepExploitWeight[TimestepExploitWeight$TimestepExploitWeight=="NaN"]=0
  
  if(PlotSwitches[1,2]==1 || PlotSwitches[1,2]==2)
  {
    if(PlotSwitches[1,2]==1) windows(width=PlotWidth, height=PlotHeight, record=T)
    
    for(j in 1:22)
    {
      if(j<=14)
      {
        if((j==13 || j==14) & sum(subset(AnnualSummary1, Name==SummaryList[j])$Quantity)==0) next
        else
        {
          print(ggplot(subset(AnnualSummary1, Name==SummaryList[j]),aes(Year,x,group=Sex,colour = Sex,linetype=Sex))+geom_line(size=1)+scale_color_manual(values=c("Red", "Blue"))+facet_grid(Area~MPA)+labs(x="Year",y=SummaryList[j])+labs(title=SummaryList[j])+theme_bw()+theme(axis.text.x=element_text(angle=90)))
          savePlot(filename=paste(SummaryList[j],"by year"),type="png")
          print(ggplot(subset(TimestepSummary1, Name==SummaryList[j]),aes(ThisTimestep,x,group=Sex,colour = Sex,linetype=Sex))+geom_line(size=1)+scale_color_manual(values=c("Red", "Blue"))+facet_grid(Area~MPA)+labs(x="Time step",y=SummaryList[j])+labs(title=SummaryList[j])+theme_bw())
          savePlot(filename=paste(SummaryList[j],"by time step"),type="png")
        }
      }
      else if(j<=20)
      {
        if((j==15 || j==16) & MSwitches[1,2]==0) next
        else if((j==19 || j==20) & sum(subset(AnnualSummary2, Name==SummaryList[j])$Quantity)==0) next	
        else
        {
          print(ggplot(subset(AnnualSummary2, Name==SummaryList[j]),aes(Year,x,group=Sex,colour = Sex,linetype=Sex))+geom_line(size=1)+scale_color_manual(values=c("Red", "Blue"))+ labs(x="Year",y=SummaryList[j])+labs(title=SummaryList[j])+theme_bw()+theme(axis.text.x=element_text(angle=90)))
          savePlot(filename=paste(SummaryList[j],"by year"),type="png")			 
          print(ggplot(subset(TimestepSummary2, Name==SummaryList[j]),aes(ThisTimestep,x,group=Sex,colour = Sex,linetype=Sex))+geom_line(size=1)+scale_color_manual(values=c("Red", "Blue"))+ labs(x="Time step",y=SummaryList[j])+labs(title=SummaryList[j])+theme_bw())
          savePlot(filename=paste(SummaryList[j],"by time step"),type="png")
        }
      }
      else if(j==21)
      {
        print(ggplot(AnnualExploitNumber,aes(Year,AnnualExploitNumber,group=Sex,colour = Sex,linetype=Sex))+geom_line(size=1)+scale_color_manual(values=c("Red", "Blue"))+ labs(x="Year",y=SummaryList[j])+labs(title=SummaryList[j])+theme_bw()+theme(axis.text.x=element_text(angle=90)))
        savePlot(filename="Exploitation rate (number) by year.png")			 
        print(ggplot(TimestepExploitNumber,aes(ThisTimestep,TimestepExploitNumber,group=Sex,colour = Sex,linetype=Sex))+geom_line(size=1)+scale_color_manual(values=c("Red", "Blue"))+ labs(x="Time step",y=SummaryList[j])+labs(title=SummaryList[j])+theme_bw())
        savePlot(filename=paste("Exploitation rate (number) by time step"),type="png")			 
      }
      else if(j==22)
      {
        print(ggplot(AnnualExploitWeight,aes(Year,AnnualExploitWeight,group=Sex,colour = Sex,linetype=Sex))+geom_line(size=1)+scale_color_manual(values=c("Red", "Blue"))+ labs(x="Year",y=SummaryList[j])+labs(title=SummaryList[j])+theme_bw()+theme(axis.text.x=element_text(angle=90)))
        savePlot(filename=paste("Exploitation rate (weight) by year"),type="png")			 
        print(ggplot(TimestepExploitWeight,aes(ThisTimestep,TimestepExploitWeight,group=Sex,colour = Sex,linetype=Sex))+geom_line(size=1)+scale_color_manual(values=c("Red", "Blue"))+ labs(x="Time step",y=SummaryList[j])+labs(title=SummaryList[j])+theme_bw())
        savePlot(filename=paste("Exploitation rate (weight) by time step"),type="png")			 
      }
    }	
  }
  
  ##OBS vs PRED catch			
  ObservedCatchEffort$CalendarYear=ObservedCatchEffort$Year
  ObservedCatchEffort$Year=ifelse(ObservedCatchEffort$Timestep<ManagementSetting[8,2],ObservedCatchEffort$Year-1,ObservedCatchEffort$Year)
  
  ObservedCatch=ObservedCatchEffort[,c("Year","CalendarYear","Timestep","Fishery","ObsLandedCatch")]
  ObservedCatch$Fishery=paste("Fishery",ObservedCatch$Fishery)
  ObservedCatch$Year=as.factor(ObservedCatch$Year)
  ObservedCatch$Timestep=as.factor(ObservedCatch$Timestep)
  
  ObservedCatchFish=aggregate(ObservedCatch$ObsLandedCatch,by=as.list(ObservedCatch[,c("Year","CalendarYear","Timestep")]),sum) #Sum over fishery
  names(ObservedCatchFish)[dim(ObservedCatchFish)[2]]="ObsLandedCatch"
  ObservedCatchFish$Fishery="Combined"
  ObservedCatchFish=rbind(ObservedCatch,ObservedCatchFish)
  
  TimestepLandedCatchNumberAll=subset(TimestepSummary2,Name=="Landed Catch (number)")
  
  PredictedCatch=aggregate(TimestepLandedCatchNumberAll$x,by=as.list(TimestepLandedCatchNumberAll[,c("Fishery","Year","Timestep","ThisTimestep")]),sum) #Sum over area and sex
  names(PredictedCatch)[dim(PredictedCatch)[2]]="PredLandedCatch"
  
  PredictedCatchFish=aggregate(TimestepLandedCatchNumberAll$x,by=as.list(TimestepLandedCatchNumberAll[,c("Year","Timestep","ThisTimestep")]),sum) #Sum over fishery
  names(PredictedCatchFish)[dim(PredictedCatchFish)[2]]="PredLandedCatch"
  PredictedCatchFish$Fishery="Combined"
  
  PredictedCatchFish=rbind(PredictedCatch,PredictedCatchFish)
  
  ObsPredCatchFish=merge(PredictedCatchFish,ObservedCatchFish)
  if(ManagementSetting[3,2]!=ManagementSetting[8,2]) ObsPredCatchFish=subset(ObsPredCatchFish,Year!=head(Summary1$Year,1))
  if(ManagementSetting[8,2]>1 && ManagementSetting[4,2]!=(ManagementSetting[8,2]-1)) ObsPredCatchFish=subset(ObsPredCatchFish,Year!=tail(Summary1$Year,1))	
  
  ObservedCatchYear=aggregate(ObsPredCatchFish$ObsLandedCatch,by=as.list(ObsPredCatchFish[,c("Fishery","Year")]),sum) #Sum over year
  names(ObservedCatchYear)[dim(ObservedCatchYear)[2]]="ObsLandedCatch"
  ObservedCatchYear$Timestep="Annual"
  ObservedCatchYear$ThisTimestep="Annual"
  ObservedCatchYear$CalendarYear="None"
  
  PredictedCatchYear=aggregate(ObsPredCatchFish$PredLandedCatch,by=as.list(ObsPredCatchFish[,c("Fishery","Year")]),sum) #Sum over year
  names(PredictedCatchYear)[dim(PredictedCatchYear)[2]]="PredLandedCatch"
  PredictedCatchYear$Timestep="Annual"
  PredictedCatchYear$ThisTimestep="Annual" 
  PredictedCatchYear$CalendarYear="None"
  
  ObsPredCatchYear=merge(PredictedCatchYear,ObservedCatchYear)
  if(ManagementSetting[3,2]!=ManagementSetting[8,2]) ObsPredCatchYear=subset(ObsPredCatchYear,Year!=head(Summary1$Year,1))
  if(ManagementSetting[8,2]>1 && ManagementSetting[4,2]!=(ManagementSetting[8,2]-1)) ObsPredCatchYear=subset(ObsPredCatchYear,Year!=tail(Summary1$Year,1))	
  
  ObsPredCatch<<-rbind(ObsPredCatchYear,ObsPredCatchFish)
  ####
  ObsPredCatchFish <<- ObsPredCatchFish
  ####
  if(PlotSwitches[1,2]==1 || PlotSwitches[1,2]==2) 
  {   
    print(ggplot(ObsPredCatchYear,aes(Year,PredLandedCatch,group=Fishery))+geom_line(size=1)+geom_point(aes(Year,ObsLandedCatch),size=2,colour="red")+labs(x="Year",y="Landed Catch (number)")+labs(title="Observed (dot) and Prediced (line) Landed Catch (number)")+theme_bw()+theme(axis.text.x=element_text(angle=90)))
    savePlot(filename="Observed and Prediced Landed Catch (number) by year",type="png")	
    
    print(ggplot(ObsPredCatchFish,aes(Year,PredLandedCatch,group=Fishery))+geom_line(size=1)+geom_point(aes(Year,ObsLandedCatch),size=2,colour="red")+facet_grid(.~Timestep)+labs(x="Year",y="Landed Catch (number)")+labs(title="Observed (dot) and Prediced (line) Landed Catch (number)")+theme_bw()+theme(axis.text.x=element_text(angle=90)))
    savePlot(filename="Observed and Predicted Landed Catch (number) by time step",type="png")	
  }
  ####
  save(ObsPredCatchFish, file = "ObsPredCatch.RData")
  ####
  ##Size composition
  
  #size comp for each year
  SizeClassAbu<<-subset(Summary1,Name=="Abundance (number)")
  SizeClassAbuYear=aggregate(SizeClassAbu$Quantity, by=as.list(SizeClassAbu[,c("SizeClass","Area","MPA","Sex","Year","Timestep")]),sum)  # timestep by size class
  
  if(ManagementSetting[3,2]!=ManagementSetting[8,2]) SizeClassAbuYear=subset(SizeClassAbuYear,Year>min(Summary1$Year))
  if(ManagementSetting[8,2]>1 && ManagementSetting[4,2]!=(ManagementSetting[8,2]-1)) SizeClassAbuYear=subset(SizeClassAbuYear,Year<max(Summary1$Year))
  
  if(ManagementSetting[8,2]>1) SizeClassAbuYear=subset(SizeClassAbuYear,Timestep==(ManagementSetting[8,2]-1))[,c("x","SizeClass","Area","MPA","Sex","Year")] # use last timestep population to represent annual population size comp
  else SizeClassAbuYear=subset(SizeClassAbuYear,Timestep==1)[,c("x","SizeClass","Area","MPA","Sex","Year")] # use last timestep population to represent annual population size comp
  names(SizeClassAbuYear)[which(names(SizeClassAbuYear)=="x")]="SumQuantity"
  
  SizeClassAbuYear$SizeClassGroup=rep(1:(dim(SizeClassAbuYear)[1]/NumSizeClass),each=NumSizeClass) #for each set of data
  
  PredAbuSizeCompYear=c()
  for(i in 1:(dim(SizeClassAbuYear)[1]/NumSizeClass))
  {
    temp=subset(SizeClassAbuYear,SizeClassGroup==i)
    temp$PredSizeComp=temp$SumQuantity/sum(temp$SumQuantity)
    PredAbuSizeCompYear=rbind(PredAbuSizeCompYear,temp)
  }
  
  #size comp for each timestep or sex
  
  if(NumBurnInYears>0)
  {
    RowStart=min(which(SizeClassAbu$CalendarYear==(ManagementSetting[1,2]+NumBurnInYears) & SizeClassAbu$Timestep==ManagementSetting[3,2]))
    SizeClassAbuWithoutBurnIn=SizeClassAbu[RowStart:dim(SizeClassAbu)[1],]
    SizeClassAbuTimestep=aggregate(SizeClassAbuWithoutBurnIn$Quantity, by=as.list(SizeClassAbuWithoutBurnIn[,c("SizeClass","Area","MPA","Sex","Timestep")]),sum) #Sum over timestep
    ##################
    sizeClassAbuSex=aggregate(SizeClassAbuWithoutBurnIn$Quantity, by=as.list(SizeClassAbuWithoutBurnIn[,c("SizeClass","Area","MPA","Sex")]),sum) #Sum over timestep
    ##################
  }
  
  SizeClassAbuTimestep=aggregate(SizeClassAbu$Quantity, by=as.list(SizeClassAbu[,c("SizeClass","Area","MPA","Sex","Timestep")]),sum) #Sum over timestep
  names(SizeClassAbuTimestep)[dim(SizeClassAbuTimestep)[2]]="SumQuantity"
  SizeClassAbuTimestep$SizeClassGroup=rep(1:(dim(SizeClassAbuTimestep)[1]/NumSizeClass),each=NumSizeClass)
  ###########################
  sizeClassAbuSex=aggregate(SizeClassAbu$Quantity, by=as.list(SizeClassAbu[,c("SizeClass","Area","MPA","Sex")]),sum) #Sum over timestep
  names(sizeClassAbuSex)[dim(sizeClassAbuSex)[2]]="SumQuantity"
  sizeClassAbuSex$SizeClassGroup=rep(1:(dim(sizeClassAbuSex)[1]/NumSizeClass),each=NumSizeClass)
  ###########################
  PredAbuSizeCompTimestep=c()
  for(i in 1:(dim(SizeClassAbuTimestep)[1]/NumSizeClass))
  {
    temp=subset(SizeClassAbuTimestep,SizeClassGroup==i)
    temp$PredSizeComp=temp$SumQuantity/sum(temp$SumQuantity)
    PredAbuSizeCompTimestep=rbind(PredAbuSizeCompTimestep,temp)
  }
  #######################
  PredAbuSizeCompSex=c()
  for(i in 1:(dim(sizeClassAbuSex)[1]/NumSizeClass))
  {
    temp=subset(sizeClassAbuSex,SizeClassGroup==i)
    temp$PredSizeComp=temp$SumQuantity/sum(temp$SumQuantity)
    PredAbuSizeCompSex=rbind(PredAbuSizeCompSex,temp)
  }
  #######################
  ObsSizeComp=TuneSizeComp
  #ObsSizeComp=SizeComp
  ObsSizeComp$Area=paste("Area", ObsSizeComp$Area)
  ObsSizeComp$MPA[ObsSizeComp$MPA==1]="In MPA"
  ObsSizeComp$MPA[ObsSizeComp$MPA==2]="Out MPA"
  ObsSizeComp$Sex[ObsSizeComp$Sex==0]="Female"
  ObsSizeComp$Sex[ObsSizeComp$Sex==1]="Male"
  
  AbuSizeCompYear=merge(PredAbuSizeCompYear,ObsSizeComp)
  AbuSizeCompTimestep=merge(PredAbuSizeCompTimestep,ObsSizeComp)
  AbuSizeCompSex=merge(PredAbuSizeCompSex,ObsSizeComp)
  
  ########
  AbuSizeCompYear <<- AbuSizeCompYear
  AbuSizeCompTimestep <<- AbuSizeCompTimestep
  AbuSizeCompSex <<- AbuSizeCompSex
  write.csv(SizeClassAbu, "E:/Bai/ibm/size_comp/predicted_sizecomp.csv")
  ########
  
  
  if(PlotSwitches[1,2]==1 || PlotSwitches[1,2]==2) 
  {
    for(j in unique(AbuSizeCompYear$MPA))
    {
      for(k in unique(AbuSizeCompYear$Sex))
      {
        print(ggplot(subset(AbuSizeCompYear,MPA==j & Sex==k) ,aes(SizeClass,PredSizeComp))+geom_line(size=1)+geom_point(aes(SizeClass,SizeComp),size=2,colour="red")+facet_grid(.~Year)+labs(x="Size Class",y="Size Composition")+labs(title=paste("Observed (dot) and Prediced (line) Size Composition for",k,"(",j,")"))+theme_bw()+theme(axis.text.x=element_text(angle=90)))
        savePlot(filename=paste("Observed and Prediced Size Composition for",k,"(",j,") by year"),type="png")	
        
        print(ggplot(subset(AbuSizeCompTimestep,MPA==j & Sex==k) ,aes(SizeClass,PredSizeComp))+geom_line(size=1)+geom_point(aes(SizeClass,SizeComp),size=2,colour="red")+facet_grid(.~Timestep)+labs(x="Size Class",y="Size Composition")+labs(title=paste("Observed (dot) and Prediced (line) Size Composition for",k,"(",j,")"))+theme_bw())
        savePlot(filename=paste("Observed and Prediced Size Composition for",k,"(",j,") by timestep"),type="png")	
        
      }
    }
  }
  
  if(TunningSwitch1==1)
  {
    ##Objective functions
    
    #OBS and PRED size comp by year
    
    if(NumBurnInYears>0)
    {
      SizeClassAbuWithoutBurnIn=subset(SizeClassAbu,CalendarYear>=(ManagementSetting[1,2]+NumBurnInYears))
      SizeClassAbuTimestep=aggregate(SizeClassAbuWithoutBurnIn$Quantity, by=as.list(SizeClassAbuWithoutBurnIn[,c("SizeClass","Area","MPA","Sex","Timestep")]),sum) #Sum over timestep
      names(SizeClassAbuTimestep)[dim(SizeClassAbuTimestep)[2]]="SumQuantity"
      SizeClassAbuTimestep$SizeClassGroup=rep(1:(dim(SizeClassAbuTimestep)[1]/NumSizeClass),each=NumSizeClass)
      
      PredAbuSizeCompTimestep=c()
      for(i in 1:(dim(SizeClassAbuTimestep)[1]/NumSizeClass))
      {
        temp=subset(SizeClassAbuTimestep,SizeClassGroup==i)
        temp$PredSizeComp=temp$SumQuantity/sum(temp$SumQuantity)
        PredAbuSizeCompTimestep=rbind(PredAbuSizeCompTimestep,temp)
      }
      
      AbuSizeCompTimestep=merge(PredAbuSizeCompTimestep,ObsSizeComp)
    }
    
    colnames(AbuSizeCompTimestep)[which(colnames(AbuSizeCompTimestep)=="SizeComp")]="ObsSizeComp"
    AbuSizeCompTimestep<<-AbuSizeCompTimestep[with(AbuSizeCompTimestep,order(Timestep,Area,MPA,Sex,SizeClass)),c("SizeClass","Area","MPA","Sex","Timestep", "PredSizeComp","ObsSizeComp")]
    
    SSEAbuSizeCompTimestep=sqrt(sum((AbuSizeCompTimestep$ObsSizeComp[which(is.finite(AbuSizeCompTimestep$PredSizeComp)==TRUE)]-AbuSizeCompTimestep$PredSizeComp[which(is.finite(AbuSizeCompTimestep$PredSizeComp)==TRUE)])^2)/dim(AbuSizeCompTimestep[which(is.finite(AbuSizeCompTimestep$PredSizeComp)==TRUE),])[1])/mean(AbuSizeCompTimestep$ObsSizeComp[which(is.finite(AbuSizeCompTimestep$PredSizeComp)==TRUE)])
    
    #############################
    #OBS and PRED size comp by sex
    if(NumBurnInYears>0)
    {
      SizeClassAbuWithoutBurnIn=subset(SizeClassAbu,CalendarYear>=(ManagementSetting[1,2]+NumBurnInYears))
      SizeClassAbuSex=aggregate(SizeClassAbuWithoutBurnIn$Quantity, by=as.list(SizeClassAbuWithoutBurnIn[,c("SizeClass","Area","MPA","Sex")]),sum) #Sum by sex
      names(SizeClassAbuSex)[dim(SizeClassAbuSex)[2]]="SumQuantity"
      SizeClassAbuSex$SizeClassGroup=rep(1:(dim(SizeClassAbuSex)[1]/NumSizeClass),each=NumSizeClass)
      
      PredAbuSizeCompSex=c()
      for(i in 1:(dim(SizeClassAbuSex)[1]/NumSizeClass))
      {
        temp=subset(SizeClassAbuSex,SizeClassGroup==i)
        temp$PredSizeComp=temp$SumQuantity/sum(temp$SumQuantity)
        PredAbuSizeCompSex=rbind(PredAbuSizeCompSex,temp)
      }
      
      AbuSizeCompSex=merge(PredAbuSizeCompSex,ObsSizeComp)
    }
    
    colnames(AbuSizeCompSex)[which(colnames(AbuSizeCompSex)=="SizeComp")]="ObsSizeComp"
    AbuSizeCompSex<<-AbuSizeCompSex[with(AbuSizeCompSex,order(Area,MPA,Sex,SizeClass)),c("SizeClass","Area","MPA","Sex","PredSizeComp","ObsSizeComp")]
    
    SSEAbuSizeCompSex=sqrt(sum((AbuSizeCompSex$ObsSizeComp[which(is.finite(AbuSizeCompSex$PredSizeComp)==TRUE)]-AbuSizeCompSex$PredSizeComp[which(is.finite(AbuSizeCompSex$PredSizeComp)==TRUE)])^2)/dim(AbuSizeCompSex[which(is.finite(AbuSizeCompSex$PredSizeComp)==TRUE),])[1])/mean(AbuSizeCompSex$ObsSizeComp[which(is.finite(AbuSizeCompSex$PredSizeComp)==TRUE)])
    
    #############################
    
    #OBS and PRED catch by timestep
    
    CatchNumTimestep=subset(ObsPredCatch,Fishery!="Combined" & Timestep!="Annual" & CalendarYear>=(ManagementSetting[1,2]+NumBurnInYears))
    CatchNumTimestep=CatchNumTimestep[order(as.numeric(CatchNumTimestep$ThisTimestep)),]
    CatchNumTimestep<<-CatchNumTimestep
    # CatchNumTimestep=subset(CatchNumTimestep,CalendarYear>=1986)
    SSECatchNumTimestep=sqrt(sum((CatchNumTimestep$ObsLandedCatch-CatchNumTimestep$PredLandedCatch)^2)/dim(CatchNumTimestep)[1])/mean(CatchNumTimestep$ObsLandedCatch)
    
    #Relative difference in initial abundance of fishing time step (end abundance of the previous fishing time step)
    
    if(ManagementSetting[8,2]!=1) EndTimeStep=ManagementSetting[8,2]-1
    else EndTimeStep=NumTimestep
    
    TimestepSummary1$Year=as.numeric(as.character(TimestepSummary1$Year))
    
    PredAbuTimestepSex=subset(TimestepSummary1,Name=="Abundance (number)" & Timestep==EndTimeStep & Year>=(ManagementSetting[1,2]+NumBurnInYears-1)) #by sex only for Doug
    
    PredAbuTimestep=aggregate(PredAbuTimestepSex$x,by=as.list(PredAbuTimestepSex[,c("Area","MPA","Year")]),sum)
    colnames(PredAbuTimestep)[which(colnames(PredAbuTimestep)=="x")]="PredAbundance"
    PredAbuTimestep$PredProp=PredAbuTimestep$PredAbundance/mean(PredAbuTimestep$PredAbundance)
    
    InitialAbunRecruitSexRatio$CalendarYear=InitialAbunRecruitSexRatio$Year
    InitialAbunRecruitSexRatio$Year=ifelse(InitialAbunRecruitSexRatio$Timestep<ManagementSetting[8,2],InitialAbunRecruitSexRatio$Year-1,InitialAbunRecruitSexRatio$Year)
    
    ObsAbuTimestep=subset(InitialAbunRecruitSexRatio,Timestep==EndTimeStep)[,c("Year","CalendarYear","Area","MPA","EndCatchAndShort","EndCatchAndShortProp")]
    ObsAbuTimestep$Year=as.factor(ObsAbuTimestep$Year)
    ObsAbuTimestep$Area=paste("Area",ObsAbuTimestep$Area)
    ObsAbuTimestep$MPA[ObsAbuTimestep$MPA==1]="In MPA"
    ObsAbuTimestep$MPA[ObsAbuTimestep$MPA==2]="Out MPA"
    
    AbuTimestep<<-merge(PredAbuTimestep,ObsAbuTimestep)
    SSEAbuTimestep=sqrt(sum((AbuTimestep$EndCatchAndShortProp-AbuTimestep$PredProp)^2)/dim(AbuTimestep)[1])/mean(AbuTimestep$EndCatchAndShortProp)
    
    if(WeightObjFunction[1,2]==0) SSECatchNumTimestep=0
    if(WeightObjFunction[2,2]==0) SSEAbuSizeCompTimestep=0
    if(WeightObjFunction[3,2]==0) SSEAbuTimestep=0		
    #ObjFunction<<-SSEAbuSizeCompTimestep*WeightObjFunction[2,2]+SSECatchNumTimestep*WeightObjFunction[1,2]+SSEAbuTimestep*WeightObjFunction[3,2]	
    #############
    ObjFunction<<-SSEAbuSizeCompSex*WeightObjFunction[2,2]+SSECatchNumTimestep*WeightObjFunction[1,2]+SSEAbuTimestep*WeightObjFunction[3,2]
    #############
  }
}

##Phase plot function			

PhasePlotFunction=function(FBRP,BBRP,FBRPTarget,BBRPTarget,FBRPLimit,BBRPLimit,PlotYear,PlotFirstYear,PlotLastYear,PlotTitle,XLabel,YLabel,FBRPTargetLabel,BBRPTargetLabel,FBRPLimitLabel,BBRPLimitLabel,FMSY,BMSY,FMSYLabel,BMSYLabel)
{	
  PhasePlotData<<-data.frame(FBRP,BBRP,Year=PlotYear)
  if(PlotFirstYear==FALSE) PhasePlotData<<-PhasePlotData[-1,] #first year is not included
  if(PlotLastYear==FALSE) PhasePlotData<<-PhasePlotData[-dim(PhasePlotData)[1],] #last year is not included
  
  if(is.null(FMSY)==FALSE & is.null(BMSY)==FALSE)
  {
    PhasePlotPolygon<<-data.frame(xx=c(
      BBRPLimit,max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.1,max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.1,BBRPLimit,
      BBRPLimit,max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.1,max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.1,BBRPLimit,
      BBRPLimit,max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.1,max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.1,BBRPLimit,
      BBRPTarget,BBRPLimit,BBRPLimit,BBRPTarget,
      BBRPTarget,BBRPLimit,BBRPLimit,BBRPTarget,
      BBRPTarget,BBRPLimit,BBRPLimit,BBRPTarget,
      min(PhasePlotData$BBRP,BBRPLimit)*0.6,BBRPLimit,BBRPLimit,min(PhasePlotData$BBRP,BBRPLimit)*0.6,
      min(PhasePlotData$BBRP,BBRPLimit)*0.6,BBRPLimit,BBRPLimit,min(PhasePlotData$BBRP,BBRPLimit)*0.6,
      min(PhasePlotData$BBRP,BBRPLimit)*0.6,BBRPLimit,BBRPLimit,min(PhasePlotData$BBRP,BBRPLimit)*0.6),		
      yy=c(
        min(PhasePlotData$FBRP,FBRPTarget,FMSY)*0.8,min(PhasePlotData$FBRP,FBRPTarget,FMSY)*0.8,FBRPTarget,FBRPTarget,
        FBRPTarget,FBRPTarget,FBRPLimit,FBRPLimit,
        FBRPLimit,FBRPLimit,max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.16,max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.16,
        min(PhasePlotData$FBRP,FBRPTarget,FMSY)*0.8,min(PhasePlotData$FBRP,FBRPTarget,FMSY)*0.8,FBRPTarget,FBRPTarget,
        FBRPTarget,FBRPTarget,FBRPLimit,FBRPLimit,
        FBRPLimit,FBRPLimit,max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.16,max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.16,
        min(PhasePlotData$FBRP,FBRPTarget,FMSY)*0.8,min(PhasePlotData$FBRP,FBRPTarget,FMSY)*0.8,FBRPTarget,FBRPTarget,
        FBRPTarget,FBRPTarget,FBRPLimit,FBRPLimit,
        FBRPLimit,FBRPLimit,max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.16,max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.16
      ),polycol=c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4),rep(7,4),rep(8,4),rep(9,4)),polyid=factor(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4),rep(7,4),rep(8,4),rep(9,4))))
  }else
  {
    PhasePlotPolygon<<-data.frame(xx=c(
      BBRPLimit,max(PhasePlotData$BBRP,BBRPTarget)*1.1,max(PhasePlotData$BBRP,BBRPTarget)*1.1,BBRPLimit,
      BBRPLimit,max(PhasePlotData$BBRP,BBRPTarget)*1.1,max(PhasePlotData$BBRP,BBRPTarget)*1.1,BBRPLimit,
      BBRPLimit,max(PhasePlotData$BBRP,BBRPTarget)*1.1,max(PhasePlotData$BBRP,BBRPTarget)*1.1,BBRPLimit,
      BBRPTarget,BBRPLimit,BBRPLimit,BBRPTarget,
      BBRPTarget,BBRPLimit,BBRPLimit,BBRPTarget,
      BBRPTarget,BBRPLimit,BBRPLimit,BBRPTarget,
      min(PhasePlotData$BBRP,BBRPLimit)*0.6,BBRPLimit,BBRPLimit,min(PhasePlotData$BBRP,BBRPLimit)*0.6,
      min(PhasePlotData$BBRP,BBRPLimit)*0.6,BBRPLimit,BBRPLimit,min(PhasePlotData$BBRP,BBRPLimit)*0.6,
      min(PhasePlotData$BBRP,BBRPLimit)*0.6,BBRPLimit,BBRPLimit,min(PhasePlotData$BBRP,BBRPLimit)*0.6),
      yy=c(
        min(PhasePlotData$FBRP,FBRPTarget)*0.8,min(PhasePlotData$FBRP,FBRPTarget)*0.8,FBRPTarget,FBRPTarget,
        FBRPTarget,FBRPTarget,FBRPLimit,FBRPLimit,
        FBRPLimit,FBRPLimit,max(PhasePlotData$FBRP,FBRPLimit)*1.16,max(PhasePlotData$FBRP,FBRPLimit)*1.16,
        min(PhasePlotData$FBRP,FBRPTarget)*0.8,min(PhasePlotData$FBRP,FBRPTarget)*0.8,FBRPTarget,FBRPTarget,
        FBRPTarget,FBRPTarget,FBRPLimit,FBRPLimit,
        FBRPLimit,FBRPLimit,max(PhasePlotData$FBRP,FBRPLimit)*1.16,max(PhasePlotData$FBRP,FBRPLimit)*1.16,
        min(PhasePlotData$FBRP,FBRPTarget)*0.8,min(PhasePlotData$FBRP,FBRPTarget)*0.8,FBRPTarget,FBRPTarget,
        FBRPTarget,FBRPTarget,FBRPLimit,FBRPLimit,
        FBRPLimit,FBRPLimit,max(PhasePlotData$FBRP,FBRPLimit)*1.16,max(PhasePlotData$FBRP,FBRPLimit)*1.16
      ),polycol=c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4),rep(7,4),rep(8,4),rep(9,4)),polyid=factor(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4),rep(7,4),rep(8,4),rep(9,4))))
  }
  
  FBRPTarget<<-FBRPTarget
  BBRPTarget<<-BBRPTarget
  FBRPLimit<<-FBRPLimit
  BBRPLimit<<-BBRPLimit
  FBRPTargetLabel<<-FBRPTargetLabel
  BBRPTargetLabel<<-BBRPTargetLabel
  FBRPLimitLabel<<-FBRPLimitLabel
  BBRPLimitLabel<<-BBRPLimitLabel
  FMSY<<-FMSY
  BMSY<<-BMSY
  FMSYLabel<<-FMSYLabel
  BMSYLabel<<-BMSYLabel
  
  p=ggplot(PhasePlotData,aes(BBRP,FBRP))+
    geom_polygon(PhasePlotPolygon,mapping=aes(xx,yy,group=polyid,fill=polycol))+
    scale_fill_gradient(low = "#66FF33", high = "#FF3333")+
    geom_path(aes(colour=Year),size=1.5)+
    geom_point(aes(colour=Year),size=4)+
    geom_point(data=head(PhasePlotData, 1), colour="#000033", size=7) +
    geom_point(data=tail(PhasePlotData, 1), colour="#3399CC",size=7,shape=15)+
    geom_text(aes(label=Year),hjust=1.35, vjust=0.45,size=4)+
    geom_segment(aes(x=min(PhasePlotData$BBRP,BBRPLimit)*0.6, y=FBRPTarget,xend=max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.1,yend=FBRPTarget),size=1,linetype=2)+	
    geom_segment(aes(x=min(PhasePlotData$BBRP,BBRPLimit)*0.6, y=FBRPLimit,xend=max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.1,yend=FBRPLimit),size=1)+		
    geom_segment(aes(x=BBRPLimit, y=min(PhasePlotData$FBRP,FBRPTarget,FMSY)*0.8,xend=BBRPLimit,yend=max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.16),size=1)+
    geom_segment(aes(x=BBRPTarget, y=min(PhasePlotData$FBRP,FBRPTarget,FMSY)*0.8,xend=BBRPTarget,yend=max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.16),size=1,linetype=2)+
    #geom_segment(aes(x=BBRPLimit,y=min(PhasePlotData$FBRP,FBRPTarget)*0.8,xend=BBRPTarget, yend=FBRPTarget),size=1.5)+
    #geom_segment(aes(x=BBRPTarget, y=FBRPTarget,xend=max(PhasePlotData$BBRP,BBRPTarget)*1.1,yend=FBRPTarget),size=1.5)+
    #geom_segment(aes(x=BBRPLimit,y=min(PhasePlotData$FBRP,FBRPTarget)*0.8,xend=BBRPTarget, yend=FBRPLimit),size=1.5)+
    #geom_segment(aes(x=BBRPTarget, y=FBRPLimit,xend=max(PhasePlotData$BBRP,BBRPTarget)*1.1,yend=FBRPLimit),size=1.5)+
    coord_cartesian(xlim=c(min(PhasePlotData$BBRP,BBRPLimit)*0.6,max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.3),ylim=c(min(PhasePlotData$FBRP,FBRPTarget,FMSY)*0.8,max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.3))+
    theme(legend.position="none",panel.background=element_blank(),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),panel.border=element_rect(fill=NA))+
    labs(title=PlotTitle)+
    xlab(XLabel)+
    ylab(YLabel)
  
  if(is.null(FMSY)==FALSE & is.null(BMSY)==FALSE) 
  {
    if(FMSY==FBRPTarget | FMSY==FBRPLimit)
    {
      if(FMSY==FBRPTarget) p=p+
          annotate("text",x=max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.102,y=FBRPTarget,label=paste("Target (MSY):",FBRPTargetLabel),hjust=0,size=3)+
          annotate("text",x=max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.102,y=FBRPLimit,label=paste("Limit:",FBRPLimitLabel),hjust=0,size=3)
      
      if(FMSY==FBRPLimit) p=p+
          annotate("text",x=max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.102,y=FBRPLimit,label=paste("Limit (MSY):",FBRPLimitLabel),hjust=0,size=3)+
          annotate("text",x=max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.102,y=FBRPTarget,label=paste("Target:",FBRPTargetLabel),hjust=0,size=3)
    }	
    else
    {
      p=p+
        geom_segment(aes(x=min(PhasePlotData$BBRP,BBRPLimit)*0.6, y=FMSY,xend=max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.1,yend=FMSY),size=1,linetype=3)+
        annotate("text",x=max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.102,y=FMSY,label=paste("MSY:",FMSYLabel),hjust=0,size=3)+
        annotate("text",x=max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.102,y=FBRPTarget,label=paste("Target:",FBRPTargetLabel),hjust=0,size=3)+ 
        annotate("text",x=max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.102,y=FBRPLimit,label=paste("Limit:",FBRPLimitLabel),hjust=0,size=3)
    }
    
    if(BMSY==BBRPTarget | BMSY==BBRPLimit)
    {
      if(BMSY==BBRPTarget) p=p+
          annotate("text",x=BBRPTarget,y=max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.17,label=paste("Target (MSY) \n",BBRPTargetLabel),vjust=0,size=3)+
          annotate("text",x=BBRPLimit,y=max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.17,label=paste("Limit \n",BBRPLimitLabel),vjust=0,size=3)		
      
      if(BMSY==BBRPLimit) p=p+
          annotate("text",x=BBRPLimit,y=max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.17,label=paste("Limit (MSY) \n",BBRPLimitLabel),vjust=0,size=3)+
          annotate("text",x=BBRPTarget,y=max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.17,label=paste("Target \n",BBRPTargetLabel),vjust=0,size=3)		
    }	
    else 
    {
      p=p+
        geom_segment(aes(x=BMSY, y=min(PhasePlotData$FBRP,FBRPTarget,FMSY)*0.8,xend=BMSY,yend=max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.16),size=1,linetype=3)+
        annotate("text",x=BMSY,y=max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.17,label=paste("MSY \n",BMSYLabel),vjust=0,size=3)+
        annotate("text",x=BBRPLimit,y=max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.17,label=paste("Limit \n",BBRPLimitLabel),vjust=0,size=3)+
        annotate("text",x=BBRPTarget,y=max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.17,label=paste("Target \n",BBRPTargetLabel),vjust=0,size=3)		
    }
    
    p
  }
  else
  {
    p=p+
      annotate("text",x=max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.102,y=FBRPTarget,label=paste("Target:",FBRPTargetLabel),hjust=0,size=3)+ 
      annotate("text",x=max(PhasePlotData$BBRP,BBRPTarget,BMSY)*1.102,y=FBRPLimit,label=paste("Limit:",FBRPLimitLabel),hjust=0,size=3)+
      annotate("text",x=BBRPLimit,y=max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.17,label=paste("Limit \n",BBRPLimitLabel),vjust=0,size=3)+
      annotate("text",x=BBRPTarget,y=max(PhasePlotData$FBRP,FBRPLimit,FMSY)*1.17,label=paste("Target \n",BBRPTargetLabel),vjust=0,size=3)
    
  }
}

##Multiplot

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,layout.pos.col = matchidx$col))
    }
  }
}

##Base case simulation

if(BRPSwitches[1,2]==0 & ProjectionSwitches[1,2]==0 & TunningSwitches[1,2]==0) #base case
{
  ThisTimestep=1
  CumulativeIntitialRecruit=0
  Simulation(SimulationYear,ManagementSetting[3,2],ManagementSetting[4,2],NULL,SimulationSetting[10,2],SimulationSetting[11,2],SimulationSetting[12,2],SimulationSetting[13,2],SimulationSetting[14,2],SimulationSetting[15,2],0,0,0,0,0)
  
  SimulationRegularResult(0)
  
  save.image("HistoricalSimulation.RData")
  
  HistoricalDataInfo1=subset(Summary1,Name==SummaryList[[1]])[,c("Area","MPA","Sex","SizeClass","Year","CalendarYear","Timestep","ThisTimestep")]
  HistoricalDataInfo2=subset(Summary2,Name==SummaryList[[22]])[,c("Area","Fishery","Sex","SizeClass","Year","CalendarYear","Timestep","ThisTimestep")]
  
  HistoricalDataAll1=c()
  HistoricalDataAll2=c()
  
  #write the results
  for(i in 1:length(SummaryList))
  {
    if(i<=14)
    {
      temp=subset(Summary1,Name==SummaryList[[i]])
      tempdata1=temp$Quantity
      HistoricalDataAll1=cbind(HistoricalDataAll1,tempdata1)
    }
    else
    {
      temp=subset(Summary2,Name==SummaryList[[i]])
      tempdata2=temp$Quantity
      HistoricalDataAll2=cbind(HistoricalDataAll2,tempdata2)
    }
    
  }
  HistoricalDataAll1=as.data.frame(HistoricalDataAll1)
  names(HistoricalDataAll1)=SummaryListSheetName[1:14]
  HistoricalData1=cbind(HistoricalDataInfo1,HistoricalDataAll1)
  
  HistoricalDataAll2=as.data.frame(HistoricalDataAll2)
  names(HistoricalDataAll2)=SummaryListSheetName[15:22]
  HistoricalData2=cbind(HistoricalDataInfo2,HistoricalDataAll2)
  
  
  write.csv(HistoricalData1,file="HistoricalOutput1.csv", row.names=FALSE)
  write.csv(HistoricalData2,file="HistoricalOutput2.csv", row.names=FALSE)
}

##Tunning model

if(TunningSwitches[1,2]==1)
{
  #read in TunningControl.xlsx
  InitialAbuScaler=read.xlsx("TunningControl.xlsx",sheetName="InitialAbuScaler")
  RecruitScaler=read.xlsx("TunningControl.xlsx",sheetName="RecruitScaler")
  EnrateScaler_t1=read.xlsx("TunningControl.xlsx",sheetName="EnrateScaler_t1")
  EnrateScaler_t2=read.xlsx("TunningControl.xlsx",sheetName="EnrateScaler_t2")
  EnrateScaler_t3=read.xlsx("TunningControl.xlsx",sheetName="EnrateScaler_t3")
  EnrateScaler_t4=read.xlsx("TunningControl.xlsx",sheetName="EnrateScaler_t4")
  WeightObjFunction=read.xlsx("TunningControl.xlsx",sheetName="WeightObjFunction")
  
  InitialAbuScalerSeq=seq(InitialAbuScaler[1,2],InitialAbuScaler[2,2],by=InitialAbuScaler[3,2])
  RecruitScalerSeq=seq(RecruitScaler[1,2],RecruitScaler[2,2],by=RecruitScaler[3,2])
  EnrateScaler_t1Seq=seq(EnrateScaler_t1[1,2],EnrateScaler_t1[2,2],by=EnrateScaler_t1[3,2])
  EnrateScaler_t2Seq=seq(EnrateScaler_t2[1,2],EnrateScaler_t2[2,2],by=EnrateScaler_t2[3,2])
  EnrateScaler_t3Seq=seq(EnrateScaler_t3[1,2],EnrateScaler_t3[2,2],by=EnrateScaler_t3[3,2])
  EnrateScaler_t4Seq=seq(EnrateScaler_t4[1,2],EnrateScaler_t4[2,2],by=EnrateScaler_t4[3,2])
  NumOfTunning=length(InitialAbuScalerSeq)*length(RecruitScalerSeq)*length(EnrateScaler_t1Seq)*length(EnrateScaler_t2Seq)*length(EnrateScaler_t3Seq)*length(EnrateScaler_t4Seq)
  #?ScalerAll=data.frame(InitialAbuScalerSeq,RecruitScalerSeq,EnrateScalerSeq)
  
  TunningAbuSizeComp=c()
  TunningCatchNum=c()				
  TunningAbu=c()
  TunningObjFunction=c()
  
  #for(j in 1:dim(ScalerAll)[1])
  #{
  # i=ScalerAll[j,1]
  # r=ScalerAll[j,2]
  # e=ScalerAll[j,3]
  tg=1
  
  library("parallel")
  library("doSNOW")
  no_cores <- detectCores()
  cl <- makeCluster(no_cores)
  registerDoSNOW(cl)
  ptm <- proc.time()
  my.list=foreach (i=InitialAbuScalerSeq, .combine = rbind) %do%
  {
    for(r in RecruitScalerSeq)
    {
      for(e in EnrateScaler_t1Seq)
      {
        for(f in EnrateScaler_t2Seq)
        {
          for(g in EnrateScaler_t3Seq)
          {
            for(h in EnrateScaler_t4Seq)
            {
              #EncounterRate=EncounterRate
              ThisTimestep<<-1 #set the timestep to the beginning, run from no lobsters
              CumulativeIntitialRecruit<<-0
              #return(env1 <- as.list(Simulation(SimulationYear,ManagementSetting[3,2],ManagementSetting[4,2],NULL,i,r,e,0,0,0,0,0)))
              #list2env(env1 , envir = .GlobalEnv)
              Simulation(SimulationYear,ManagementSetting[3,2],ManagementSetting[4,2],NULL,i,r,e,f,g,h,0,0,0,0,0)  
              #env2 <- as.list(SimulationRegularResult(1))
              SimulationRegularResult(1)
              TunningAbuSizeComp<<-rbind(TunningAbuSizeComp,data.frame(AbuSizeCompTimestep,InitialAbuScaler=i,RecruitScaler=r,EnrateScaler_t1=e, EnrateScaler_t2=f, EnrateScaler_t3=g, EnrateScaler_t4=h,ObjectiveValue=ObjFunction))
              TunningCatchNum<<-rbind(TunningCatchNum,data.frame(CatchNumTimestep,InitialAbuScaler=i,RecruitScaler=r,EnrateScaler_t1=e, EnrateScaler_t2=f, EnrateScaler_t3=g, EnrateScaler_t4=h,ObjectiveValue=ObjFunction))
              TunningAbu<<-rbind(TunningAbu,data.frame(AbuTimestep,InitialAbuScaler=i,RecruitScaler=r,EnrateScaler_t1=e, EnrateScaler_t2=f, EnrateScaler_t3=g, EnrateScaler_t4=h,ObjectiveValue=ObjFunction))
              TunningObjFunction<<-rbind(TunningObjFunction,data.frame(InitialAbuScaler=i,RecruitScaler=r,EnrateScaler_t1=e, EnrateScaler_t2=f, EnrateScaler_t3=g, EnrateScaler_t4=h,ObjectiveValue=ObjFunction))
              tg<<-tg+1
            }
          }
          #return(TunningAbuSizeComp, TunningCatchNum, TunningAbu, TunningObjFunction)
        }
      }
    }
    write.csv(TunningObjFunction,file="TunningOutputRecord.csv", row.names=FALSE)
  }
  # }
  stopCluster(cl)
  print(proc.time()-ptm)
  write.csv(TunningAbuSizeComp,file="TunningOutputAbuSizeComp.csv", row.names=FALSE)
  names(TunningCatchNum)[which(names(TunningCatchNum)=="Year")]=c("FishingYear")
  write.csv(TunningCatchNum,file="TunningOutputCatchNum.csv", row.names=FALSE)
  
  names(TunningAbu)[which(names(TunningAbu)=="Year")]=c("FishingYear")
  write.csv(TunningAbu,file="TunningOutputAbu.csv", row.names=FALSE)
  write.csv(TunningObjFunction,file="TunningOutputObjFunction.csv", row.names=FALSE)
}			

##BRP simulation

if(BRPSwitches[1,2]==1 & BRPSwitches[2,2]==1) #adhoc BRP
{
  ThisTimestep=1
  CumulativeIntitialRecruit=0
  Simulation(SimulationYear,ManagementSetting[3,2],ManagementSetting[4,2],NULL,SimulationSetting[10,2],SimulationSetting[11,2],SimulationSetting[12,2],SimulationSetting[13,2],SimulationSetting[14,2],SimulationSetting[15,2],BRPSwitches[1,2],BRPSwitches[2,2],0,0,0)
  
  if(NumBurnInYears>0)
  {
    RowStart=min(which(Summary1$CalendarYear==(ManagementSetting[1,2]+NumBurnInYears) & Summary1$Timestep==ManagementSetting[3,2]))
    Summary1=Summary1[RowStart:dim(Summary1)[1],]
    RowStart=min(which(Summary2$CalendarYear==(ManagementSetting[1,2]+NumBurnInYears) & Summary2$Timestep==ManagementSetting[3,2]))
    Summary2=Summary2[RowStart:dim(Summary2)[1],]
  }
  
  SimulationFishingYear=c(min(Summary1$Year):max(Summary1$Year)) 
  if(ManagementSetting[3,2]!=ManagementSetting[8,2]) SimulationFishingYear=SimulationFishingYear[SimulationFishingYear>min(Summary1$Year)]
  if(ManagementSetting[8,2]!=1 && ManagementSetting[4,2]!=(ManagementSetting[8,2]-1)) SimulationFishingYear=SimulationFishingYear[SimulationFishingYear<max(Summary1$Year)]
  
  #read in BRPControl.xlsx   	
  AdHocBRP=read.xlsx("BRPControl.xlsx",sheetName="AdHocBRP")	
  
  #subset by referenced size classes
  Summary1AbuBio=subset(Summary1, SizeClass>=AdHocBRP[3,2] & SizeClass<=AdHocBRP[4,2])
  
  AnnualSummary1AbuBio=aggregate(Summary1AbuBio$Quantity,by=as.list(Summary1AbuBio[,c("Name","Area","Year","Timestep")]),sum) #sum by size class and timestep for each year and area
  
  if(ManagementSetting[3,2]!=ManagementSetting[8,2]) AnnualSummary1AbuBio=subset(AnnualSummary1AbuBio,Year>min(Summary1$Year))
  if(ManagementSetting[8,2]!=1 && ManagementSetting[4,2]!=(ManagementSetting[8,2]-1)) AnnualSummary1AbuBio=subset(AnnualSummary1AbuBio,Year<max(Summary1$Year))
  
  AnnualSummary2=aggregate(Summary2$Quantity,by=as.list(Summary2[,c("Name","Area","Year")]),sum)
  
  if(ManagementSetting[3,2]!=ManagementSetting[8,2]) AnnualSummary2=subset(AnnualSummary2,Year>min(Summary1$Year))
  if(ManagementSetting[8,2]!=1 && ManagementSetting[4,2]!=(ManagementSetting[8,2]-1)) AnnualSummary2=subset(AnnualSummary2,Year<max(Summary1$Year))
  
  #subset by each year and area
  if(ManagementSetting[8,2]==1) 
  {
    AbuBRP=subset(AnnualSummary1AbuBio,Name==SummaryList[1] & Timestep==ManagementSetting[8,2])[,c("Name","Area","Year","x")]
    BioBRP=subset(AnnualSummary1AbuBio,Name==SummaryList[2] & Timestep==ManagementSetting[8,2])[,c("Name","Area","Year","x")]
  }
  else
  {
    AbuBRP=subset(AnnualSummary1AbuBio,Name==SummaryList[1] & Timestep==(ManagementSetting[8,2]-1))[,c("Name","Area","Year","x")]
    BioBRP=subset(AnnualSummary1AbuBio,Name==SummaryList[2] & Timestep==(ManagementSetting[8,2]-1))[,c("Name","Area","Year","x")]
  }
  CatchAbuBRP=subset(AnnualSummary2,Name==SummaryList[17])
  CatchBioBRP=subset(AnnualSummary2,Name==SummaryList[18])
  
  #subset by reference year for each area
  RefAbuBRP=subset(AbuBRP, Year>=AdHocBRP[1,2] & Year<=AdHocBRP[2,2]) 
  RefBioBRP=subset(BioBRP, Year>=AdHocBRP[1,2] & Year<=AdHocBRP[2,2])
  RefCatchAbuBRP=subset(CatchAbuBRP, Year>=AdHocBRP[1,2] & Year<=AdHocBRP[2,2])
  RefCatchBioBRP=subset(CatchBioBRP, Year>=AdHocBRP[1,2] & Year<=AdHocBRP[2,2]) 
  RefFAbuBRP=data.frame(RefAbuBRP[,c("Name","Area","Year")],x=RefCatchAbuBRP$x/(RefAbuBRP$x+RefCatchAbuBRP$x))
  RefFBioBRP=data.frame(RefBioBRP[,c("Name","Area","Year")],x=RefCatchBioBRP$x/(RefBioBRP$x+RefCatchBioBRP$x))
  
  HistoricalCondition=c()
  BRP=c()
  
  #windows(width=PlotWidth, height=PlotHeight, record=T)
  
  if(NumArea>1)
  {
    #calculate target reference points for each area
    if(AdHocBRP[5,2]<=1 & AdHocBRP[5,2]>=0) FAbuBRPTarget=aggregate(RefFAbuBRP$x,by=as.list(RefFAbuBRP[,c("Name","Area")]),quantile,probs = AdHocBRP[5,2])
    else if(AdHocBRP[5,2]>1) FAbuBRPTarget=aggregate(RefFAbuBRP$x,by=as.list(RefFAbuBRP[,c("Name","Area")]),max)*AdHocBRP[5,2]
    else if(AdHocBRP[5,2]<0) FAbuBRPTarget=aggregate(RefFAbuBRP$x,by=as.list(RefFAbuBRP[,c("Name","Area")]),min)*(1+AdHocBRP[5,2])		
    if(AdHocBRP[5,2]<=1 & AdHocBRP[5,2]>=0) FBioBRPTarget=aggregate(RefFBioBRP$x,by=as.list(RefFBioBRP[,c("Name","Area")]),quantile,probs = AdHocBRP[5,2])
    else if(AdHocBRP[5,2]>1) FBioBRPTarget=aggregate(RefFBioBRP$x,by=as.list(RefFBioBRP[,c("Name","Area")]),max)*AdHocBRP[5,2]
    else if(AdHocBRP[5,2]<0) FBioBRPTarget=aggregate(RefFBioBRP$x,by=as.list(RefFBioBRP[,c("Name","Area")]),min)*(1+AdHocBRP[5,2])
    if(AdHocBRP[7,2]<=1 & AdHocBRP[7,2]>=0) BAbuBRPTarget=aggregate(RefAbuBRP$x,by=as.list(RefAbuBRP[,c("Name","Area")]),quantile,probs = AdHocBRP[7,2])
    else if(AdHocBRP[7,2]>1) BAbuBRPTarget=aggregate(RefAbuBRP$x,by=as.list(RefAbuBRP[,c("Name","Area")]),max)*AdHocBRP[7,2]
    else if(AdHocBRP[7,2]<0) BAbuBRPTarget=aggregate(RefAbuBRP$x,by=as.list(RefAbuBRP[,c("Name","Area")]),min)*(1+AdHocBRP[7,2])
    if(AdHocBRP[7,2]<=1 & AdHocBRP[7,2]>=0) BBioBRPTarget=aggregate(RefBioBRP$x,by=as.list(RefBioBRP[,c("Name","Area")]),quantile,probs = AdHocBRP[7,2])
    else if(AdHocBRP[7,2]>1) BBioBRPTarget=aggregate(RefBioBRP$x,by=as.list(RefBioBRP[,c("Name","Area")]),max)*AdHocBRP[7,2]
    else if(AdHocBRP[7,2]<0) BBioBRPTarget=aggregate(RefBioBRP$x,by=as.list(RefBioBRP[,c("Name","Area")]),min)*(1+AdHocBRP[7,2])
    if(AdHocBRP[9,2]<=1 & AdHocBRP[9,2]>=0) MSYAbuBRPPTarget=aggregate(RefCatchAbuBRP$x,by=as.list(RefCatchAbuBRP[,c("Name","Area")]),quantile,probs = AdHocBRP[9,2])
    else if(AdHocBRP[9,2]>1) MSYAbuBRPPTarget=aggregate(RefCatchAbuBRP$x,by=as.list(RefCatchAbuBRP[,c("Name","Area")]),max)*AdHocBRP[9,2]
    else if(AdHocBRP[9,2]<0) MSYAbuBRPPTarget=aggregate(RefCatchAbuBRP$x,by=as.list(RefCatchAbuBRP[,c("Name","Area")]),min)*(1+AdHocBRP[9,2])
    if(AdHocBRP[9,2]<=1 & AdHocBRP[9,2]>=0) MSYBioBRPPTarget=aggregate(RefCatchBioBRP$x,by=as.list(RefCatchBioBRP[,c("Name","Area")]),quantile,probs = AdHocBRP[9,2])
    else if(AdHocBRP[9,2]>1) MSYBioBRPPTarget=aggregate(RefCatchBioBRP$x,by=as.list(RefCatchBioBRP[,c("Name","Area")]),max)*AdHocBRP[9,2]
    else if(AdHocBRP[9,2]<0) MSYBioBRPPTarget=aggregate(RefCatchBioBRP$x,by=as.list(RefCatchBioBRP[,c("Name","Area")]),min)*(1+AdHocBRP[9,2])
    
    #calculate limit reference points from target reference points for each area
    if(AdHocBRP[6,2]<=1 & AdHocBRP[6,2]>=0) FAbuBRPLimit=aggregate(RefFAbuBRP$x,by=as.list(RefFAbuBRP[,c("Name","Area")]),quantile,probs = AdHocBRP[6,2])
    else if(AdHocBRP[6,2]>1) FAbuBRPLimit=aggregate(RefFAbuBRP$x,by=as.list(RefFAbuBRP[,c("Name","Area")]),max)*AdHocBRP[6,2]
    else if(AdHocBRP[6,2]<0) FAbuBRPLimit=aggregate(RefFAbuBRP$x,by=as.list(RefFAbuBRP[,c("Name","Area")]),min)*(1+AdHocBRP[6,2])
    if(AdHocBRP[6,2]<=1 & AdHocBRP[6,2]>=0) FBioBRPLimit=aggregate(RefFBioBRP$x,by=as.list(RefFBioBRP[,c("Name","Area")]),quantile,probs = AdHocBRP[6,2])
    else if(AdHocBRP[6,2]>1) FBioBRPLimit=aggregate(RefFBioBRP$x,by=as.list(RefFBioBRP[,c("Name","Area")]),max)*AdHocBRP[6,2]
    else if(AdHocBRP[6,2]<0) FBioBRPLimit=aggregate(RefFBioBRP$x,by=as.list(RefFBioBRP[,c("Name","Area")]),min)*(1+AdHocBRP[6,2])
    if(AdHocBRP[8,2]<=1 & AdHocBRP[8,2]>=0) BAbuBRPLimit=aggregate(RefAbuBRP$x,by=as.list(RefAbuBRP[,c("Name","Area")]),quantile,probs = AdHocBRP[8,2])
    else if(AdHocBRP[8,2]>1) BAbuBRPLimit=aggregate(RefAbuBRP$x,by=as.list(RefAbuBRP[,c("Name","Area")]),max)*AdHocBRP[8,2]
    else if(AdHocBRP[8,2]<0) BAbuBRPLimit=aggregate(RefAbuBRP$x,by=as.list(RefAbuBRP[,c("Name","Area")]),min)*(1+AdHocBRP[8,2])
    if(AdHocBRP[8,2]<=1 & AdHocBRP[8,2]>=0) BBioBRPLimit=aggregate(RefBioBRP$x,by=as.list(RefBioBRP[,c("Name","Area")]),quantile,probs = AdHocBRP[8,2])
    else if(AdHocBRP[8,2]>1) BBioBRPLimit=aggregate(RefBioBRP$x,by=as.list(RefBioBRP[,c("Name","Area")]),max)*AdHocBRP[8,2]
    else if(AdHocBRP[8,2]<0) BBioBRPLimit=aggregate(RefBioBRP$x,by=as.list(RefBioBRP[,c("Name","Area")]),min)*(1+AdHocBRP[8,2])
    if(AdHocBRP[10,2]<=1 & AdHocBRP[10,2]>=0) MSYAbuBRPPLimit=aggregate(RefCatchAbuBRP$x,by=as.list(RefCatchAbuBRP[,c("Name","Area")]),quantile,probs = AdHocBRP[10,2])
    else if(AdHocBRP[10,2]>1) MSYAbuBRPPLimit=aggregate(RefCatchAbuBRP$x,by=as.list(RefCatchAbuBRP[,c("Name","Area")]),max)*AdHocBRP[10,2]
    else if(AdHocBRP[10,2]<0) MSYAbuBRPPLimit=aggregate(RefCatchAbuBRP$x,by=as.list(RefCatchAbuBRP[,c("Name","Area")]),min)*(1+AdHocBRP[10,2])
    if(AdHocBRP[10,2]<=1 & AdHocBRP[10,2]>=0) MSYBioBRPPLimit=aggregate(RefCatchBioBRP$x,by=as.list(RefCatchBioBRP[,c("Name","Area")]),quantile,probs = AdHocBRP[10,2])
    else if(AdHocBRP[10,2]>1) MSYBioBRPPLimit=aggregate(RefCatchBioBRP$x,by=as.list(RefCatchBioBRP[,c("Name","Area")]),max)*AdHocBRP[10,2]
    else if(AdHocBRP[10,2]<0) MSYBioBRPPLimit=aggregate(RefCatchBioBRP$x,by=as.list(RefCatchBioBRP[,c("Name","Area")]),min)*(1+AdHocBRP[10,2])	
    
    for(i in 1:NumArea) #in different area
    { 
      AbuBRPArea=subset(AbuBRP,Area==paste("Area",i))
      BioBRPArea=subset(BioBRP,Area==paste("Area",i))
      CatchAbuBRPArea=subset(CatchAbuBRP,Area==paste("Area",i))
      CatchBioBRPArea=subset(CatchBioBRP,Area==paste("Area",i))
      
      #calculate F and B for each year by area
      FAbuBRPArea=CatchAbuBRPArea$x/(AbuBRPArea$x+CatchAbuBRPArea$x) #exploitation rate for each year and area
      FBioBRPArea=CatchBioBRPArea$x/(BioBRPArea$x+CatchBioBRPArea$x)
      BAbuBRPArea=AbuBRPArea$x
      BBioBRPArea=BioBRPArea$x
      MSYAbuBRPArea=CatchAbuBRPArea$x
      MSYBioBRPArea=CatchBioBRPArea$x
      
      FAbuBRPTargetArea=FAbuBRPTarget$x[i]
      FBioBRPTargetArea=FBioBRPTarget$x[i]
      BAbuBRPTargetArea=BAbuBRPTarget$x[i]
      BBioBRPTargetArea=BBioBRPTarget$x[i]
      MSYAbuBRPTargetArea=MSYAbuBRPPTarget$x[i]
      MSYBioBRPTargetArea=MSYBioBRPPTarget$x[i]
      
      FAbuBRPLimitArea=FAbuBRPLimit$x[i]
      FBioBRPLimitArea=FBioBRPLimit$x[i]
      BAbuBRPLimitArea=BAbuBRPLimit$x[i]
      BBioBRPLimitArea=BBioBRPLimit$x[i]
      MSYAbuBRPLimitArea=MSYAbuBRPPLimit$x[i]
      MSYBioBRPLimitArea=MSYBioBRPPLimit$x[i]
      
      if(BRPSwitches[4,2]==0 || BRPSwitches[4,2]==2)
      {
        HistoricalCondition=rbind(HistoricalCondition,data.frame(F=FAbuBRPArea,B=BAbuBRPArea,Catch=MSYAbuBRPArea,Year=SimulationFishingYear,Unit="Abundance",Area=paste("Area",i)))
        
        BRP=rbind(BRP,data.frame(BRP=c("FTarget","FLimit","BTarget","BLimit","CTarget","CLimit"),value=c(FAbuBRPTargetArea,FAbuBRPLimitArea,BAbuBRPTargetArea,BAbuBRPLimitArea,MSYAbuBRPTargetArea,MSYAbuBRPLimitArea),Area=paste("Area",i)))
        
        print(PhasePlotFunction(FAbuBRPArea/FAbuBRPTargetArea,BAbuBRPArea/BAbuBRPTargetArea,FAbuBRPTargetArea/FAbuBRPTargetArea,BAbuBRPTargetArea/BAbuBRPTargetArea,FAbuBRPLimitArea/FAbuBRPTargetArea,BAbuBRPLimitArea/BAbuBRPTargetArea,SimulationFishingYear,TRUE,TRUE,paste("Area ",i," (reference year: ",AdHocBRP[1,2]," to ",AdHocBRP[2,2],")",sep=""),"Abundance (A/ATarget)","Exploitation rate (F/FTarget)", paste(round(FAbuBRPTargetArea,4)," (",AdHocBRP[5,2]*100,"%)",sep=""),paste(format(BAbuBRPTargetArea,scientific=T,digits=2),"\n(",AdHocBRP[7,2]*100,"%)",sep=""),paste(round(FAbuBRPLimitArea,4)," (",AdHocBRP[6,2]*100,"%)",sep=""),paste(format(BAbuBRPLimitArea,scientific=T,digits=2),"\n(",AdHocBRP[8,2]*100,"%)",sep=""), NULL,NULL, NULL,NULL))
        savePlot(filename=paste("Phase plot (F Abundance AdHoc) for Area",i),type="png")
        
        print(PhasePlotFunction(MSYAbuBRPArea/MSYAbuBRPTargetArea,BAbuBRPArea/BAbuBRPTargetArea,MSYAbuBRPTargetArea/MSYAbuBRPTargetArea,BAbuBRPTargetArea/BAbuBRPTargetArea,MSYAbuBRPLimitArea/MSYAbuBRPTargetArea,BAbuBRPLimitArea/BAbuBRPTargetArea,SimulationFishingYear,TRUE,TRUE,paste("Area ",i," (reference year: ",AdHocBRP[1,2]," to ",AdHocBRP[2,2],")",sep=""),"Abundance (A/ATarget)","Catch (C/CTarget)",paste(format(MSYAbuBRPTargetArea,scientific=T,digits=2)," (",AdHocBRP[9,2]*100,"%)",sep=""),paste(format(BAbuBRPTargetArea,scientific=T,digits=2),"\n(",AdHocBRP[7,2]*100,"%)",sep=""),paste(format(MSYAbuBRPLimitArea,scientific=T,digits=2)," (",AdHocBRP[10,2]*100,"%)",sep=""),paste(format(BAbuBRPLimitArea,scientific=T,digits=2),"\n(",AdHocBRP[8,2]*100,"%)",sep=""), NULL,NULL, NULL,NULL))
        savePlot(filename=paste("Phase plot (Catch Abundance AdHoc) for Area",i),type="png")
        
      }
      if(BRPSwitches[4,2]==1 || BRPSwitches[4,2]==2)
      {
        HistoricalCondition=rbind(HistoricalCondition,data.frame(F=FBioBRPArea,B=BBioBRPArea,Catch=MSYBioBRPArea,Year=SimulationFishingYear,Unit="Biomass",Area=paste("Area",i)))
        
        BRP=rbind(BRP,data.frame(BRP=c("FTarget","FLimit","BTarget","BLimit","CTarget","CLimit"),value=c(FBioBRPTargetArea,FBioBRPLimitArea,BBioBRPTargetArea,BBioBRPLimitArea,MSYBioBRPTargetArea,MSYBioBRPLimitArea),Area=paste("Area",i)))
        
        print(PhasePlotFunction(FBioBRPArea/FBioBRPTargetArea,BBioBRPArea/BBioBRPTargetArea,FBioBRPTargetArea/FBioBRPTargetArea,BBioBRPTargetArea/BBioBRPTargetArea,FBioBRPLimitArea/FBioBRPTargetArea,BBioBRPLimitArea/BBioBRPTargetArea,SimulationFishingYear,TRUE,TRUE,paste("Area ",i," (reference year: ",AdHocBRP[1,2]," to ",AdHocBRP[2,2],")",sep=""),"Biomass (B/BTarget)","Exploitation rate (F/FTarget)",paste(round(FBioBRPTargetArea,4)," (",AdHocBRP[5,2]*100,"%)",sep=""),paste(format(BBioBRPTargetArea,scientific=T,digits=2),"\n(",AdHocBRP[7,2]*100,"%)",sep=""),paste(round(FBioBRPLimitArea,4)," (",AdHocBRP[6,2]*100,"%)",sep=""),paste(format(BBioBRPLimitArea,scientific=T,digits=2),"\n(",AdHocBRP[8,2]*100,"%)",sep=""), NULL,NULL, NULL,NULL))
        savePlot(filename=paste("Phase plot (F Biomass AdHoc) for Area",i),type="png")
        
        print(PhasePlotFunction(MSYBioBRPArea/MSYBioBRPTargetArea,BBioBRPArea/BBioBRPTargetArea,MSYBioBRPTargetArea/MSYBioBRPTargetArea,BBioBRPTargetArea/BBioBRPTargetArea,MSYBioBRPLimitArea/MSYBioBRPTargetArea,BBioBRPLimitArea/BBioBRPTargetArea,SimulationFishingYear,TRUE,TRUE,paste("Area ",i," (reference year: ",AdHocBRP[1,2]," to ",AdHocBRP[2,2],")",sep=""),"Biomass (B/BTarget)","Catch (C/CTarget)",paste(format(MSYBioBRPTargetArea,scientific=T,digits=2)," (",AdHocBRP[9,2]*100,"%)",sep=""),paste(format(BBioBRPTargetArea,scientific=T,digits=2),"\n(",AdHocBRP[7,2]*100,"%)",sep=""),paste(format(MSYBioBRPLimitArea,scientific=T,digits=2)," (",AdHocBRP[10,2]*100,"%)",sep=""),paste(format(BBioBRPLimitArea,scientific=T,digits=2),"\n(",AdHocBRP[8,2]*100,"%)",sep=""), NULL,NULL, NULL,NULL))
        savePlot(filename=paste("Phase plot (Catch Biomass AdHoc) for Area",i),type="png")
        
      }
    }
  }
  
  #calculate sum by year (sum over areas)
  AbuBRPAll=aggregate(AbuBRP$x,by=as.list(AbuBRP[,c("Name","Year")]),sum) #for whole area
  BioBRPAll=aggregate(BioBRP$x,by=as.list(BioBRP[,c("Name","Year")]),sum)
  CatchAbuBRPAll=aggregate(CatchAbuBRP$x,by=as.list(CatchAbuBRP[,c("Name","Year")]),sum)
  CatchBioBRPAll=aggregate(CatchBioBRP$x,by=as.list(CatchBioBRP[,c("Name","Year")]),sum)
  
  #calcualte F and B for each year for whole area
  FAbuBRPAll=CatchAbuBRPAll$x/(AbuBRPAll$x+CatchAbuBRPAll$x) #exploitation rate for each year and area
  FBioBRPAll=CatchBioBRPAll$x/(BioBRPAll$x+CatchBioBRPAll$x)
  BAbuBRPAll=AbuBRPAll$x
  BBioBRPAll=BioBRPAll$x	
  MSYAbuBRPAll=CatchAbuBRPAll$x
  MSYBioBRPAll=CatchBioBRPAll$x
  
  #calculate reference abundance/biomass and catch for whole area
  RefAbuBRPAll=aggregate(RefAbuBRP$x,by=as.list(RefAbuBRP[,c("Name","Year")]),sum)
  RefBioBRPAll=aggregate(RefBioBRP$x,by=as.list(RefBioBRP[,c("Name","Year")]),sum)
  RefCatchAbuBRPAll=aggregate(RefCatchAbuBRP$x,by=as.list(RefCatchAbuBRP[,c("Name","Year")]),sum)
  RefCatchBioBRPAll=aggregate(RefCatchBioBRP$x,by=as.list(RefCatchBioBRP[,c("Name","Year")]),sum)
  RefFAbuBRPAll=data.frame(RefAbuBRPAll[,c("Name","Year")],x=RefCatchAbuBRPAll$x/(RefAbuBRPAll$x+RefCatchAbuBRPAll$x))
  RefFBioBRPAll=data.frame(RefBioBRPAll[,c("Name","Year")],x=RefCatchBioBRPAll$x/(RefBioBRPAll$x+RefCatchBioBRPAll$x))
  
  if(AdHocBRP[5,2]<=1 & AdHocBRP[5,2]>=0) FAbuBRPTargetAll=quantile(RefFAbuBRPAll$x,probs = AdHocBRP[5,2],names=F)
  else if(AdHocBRP[5,2]>1) FAbuBRPTargetAll=max(RefFAbuBRPAll$x)*AdHocBRP[5,2]
  else if(AdHocBRP[5,2]<0) FAbuBRPTargetAll=min(RefFAbuBRPAll$x)*(1+AdHocBRP[5,2]) 
  if(AdHocBRP[5,2]<=1 & AdHocBRP[5,2]>=0) FBioBRPTargetAll=quantile(RefFBioBRPAll$x,probs = AdHocBRP[5,2],names=F)
  else if(AdHocBRP[5,2]>1) FBioBRPTargetAll=max(RefFBioBRPAll$x)*AdHocBRP[5,2]
  else if(AdHocBRP[5,2]<0) FBioBRPTargetAll=min(RefFBioBRPAll$x)*(1+AdHocBRP[5,2]) 	
  if(AdHocBRP[7,2]<=1 & AdHocBRP[7,2]>=0) BAbuBRPTargetAll=quantile(RefAbuBRPAll$x,probs = AdHocBRP[7,2],names=F)
  else if(AdHocBRP[7,2]>1) BAbuBRPTargetAll=max(RefAbuBRPAll$x)*AdHocBRP[7,2]
  else if(AdHocBRP[7,2]<0) BAbuBRPTargetAll=min(RefAbuBRPAll$x)*(1+AdHocBRP[7,2])	
  if(AdHocBRP[7,2]<=1 & AdHocBRP[7,2]>=0) BBioBRPTargetAll=quantile(RefBioBRPAll$x,probs = AdHocBRP[7,2],names=F)
  else if(AdHocBRP[7,2]>1) BBioBRPTargetAll=max(RefBioBRPAll$x)*AdHocBRP[7,2]
  else if(AdHocBRP[7,2]<0) BBioBRPTargetAll=min(RefBioBRPAll$x)*(1+AdHocBRP[7,2]) 	
  if(AdHocBRP[9,2]<=1 & AdHocBRP[9,2]>=0) MSYAbuBRPTargetAll=quantile(RefCatchAbuBRPAll$x,probs = AdHocBRP[9,2],names=F)
  else if(AdHocBRP[9,2]>1) MSYAbuBRPTargetAll=max(RefCatchAbuBRPAll$x)*AdHocBRP[9,2]
  else if(AdHocBRP[9,2]<0) MSYAbuBRPTargetAll=min(RefCatchAbuBRPAll$x)*(1+AdHocBRP[9,2]) 	
  if(AdHocBRP[9,2]<=1 & AdHocBRP[9,2]>=0) MSYBioBRPTargetAll=quantile(RefCatchBioBRPAll$x,probs = AdHocBRP[9,2],names=F)
  else if(AdHocBRP[9,2]>1) MSYBioBRPTargetAll=max(RefCatchBioBRPAll$x)*AdHocBRP[9,2]
  else if(AdHocBRP[9,2]<0) MSYBioBRPTargetAll=min(RefCatchBioBRPAll$x)*(1+AdHocBRP[9,2]) 	
  
  if(AdHocBRP[6,2]<=1 & AdHocBRP[6,2]>=0) FAbuBRPLimitAll=quantile(RefFAbuBRPAll$x,probs = AdHocBRP[6,2],names=F)
  else if(AdHocBRP[6,2]>1) FAbuBRPLimitAll=max(RefFAbuBRPAll$x)*AdHocBRP[6,2]
  else if(AdHocBRP[6,2]<0) FAbuBRPLimitAll=min(RefFAbuBRPAll$x)*(1+AdHocBRP[6,2])	
  if(AdHocBRP[6,2]<=1 & AdHocBRP[6,2]>=0) FBioBRPLimitAll=quantile(RefFBioBRPAll$x,probs = AdHocBRP[6,2],names=F)
  else if(AdHocBRP[6,2]>1)  FBioBRPLimitAll=max(RefFBioBRPAll$x)*AdHocBRP[6,2]
  else if(AdHocBRP[6,2]<0)  FBioBRPLimitAll=min(RefFBioBRPAll$x)*(1+AdHocBRP[6,2])	
  if(AdHocBRP[8,2]<=1 & AdHocBRP[8,2]>=0) BAbuBRPLimitAll=quantile(RefAbuBRPAll$x,probs = AdHocBRP[8,2],names=F)
  else if(AdHocBRP[8,2]>1)  BAbuBRPLimitAll=max(RefAbuBRPAll$x)*AdHocBRP[8,2]
  else if(AdHocBRP[8,2]<0)  BAbuBRPLimitAll=min(RefAbuBRPAll$x)*(1+AdHocBRP[8,2])	
  if(AdHocBRP[8,2]<=1 & AdHocBRP[8,2]>=0) BBioBRPLimitAll=quantile(RefBioBRPAll$x,probs = AdHocBRP[8,2],names=F)
  else if(AdHocBRP[8,2]>1)  BBioBRPLimitAll=max(RefBioBRPAll$x)*AdHocBRP[8,2]
  else if(AdHocBRP[8,2]<0)  BBioBRPLimitAll=min(RefBioBRPAll$x)*(1+AdHocBRP[8,2])	
  if(AdHocBRP[10,2]<=1 & AdHocBRP[10,2]>=0) MSYAbuBRPLimitAll=quantile(RefCatchAbuBRPAll$x,probs = AdHocBRP[10,2],names=F)
  else if(AdHocBRP[10,2]>1)  MSYAbuBRPLimitAll=max(RefCatchAbuBRPAll$x)*AdHocBRP[10,2]
  else if(AdHocBRP[10,2]<0)  MSYAbuBRPLimitAll=min(RefCatchAbuBRPAll$x)*(1+AdHocBRP[10,2])	
  if(AdHocBRP[10,2]<=1 & AdHocBRP[10,2]>=0) MSYBioBRPLimitAll=quantile(RefCatchBioBRPAll$x,probs = AdHocBRP[10,2],names=F)
  else if(AdHocBRP[10,2]>1)  MSYBioBRPLimitAll=max(RefCatchBioBRPAll$x)*AdHocBRP[10,2]
  else if(AdHocBRP[10,2]<0)  MSYBioBRPLimitAll=min(RefCatchBioBRPAll$x)*(1+AdHocBRP[10,2])
  
  if(BRPSwitches[4,2]==0 || BRPSwitches[4,2]==2) 
  {
    HistoricalCondition=rbind(HistoricalCondition,data.frame(F=FAbuBRPAll,B=BAbuBRPAll,Catch=MSYAbuBRPAll,Year=SimulationFishingYear,Unit="Abundance",Area="Whole area"))
    
    BRP=rbind(BRP,data.frame(BRP=c("FTarget","FLimit","BTarget","BLimit","CTarget","CLimit"),value=c(FAbuBRPTargetAll,FAbuBRPLimitAll,BAbuBRPTargetAll,BAbuBRPLimitAll,MSYAbuBRPTargetAll,MSYAbuBRPLimitAll),Unit="Abundance",Area="Whole area"))			
    
    print(PhasePlotFunction(FAbuBRPAll/FAbuBRPTargetAll,BAbuBRPAll/BAbuBRPTargetAll,FAbuBRPTargetAll/FAbuBRPTargetAll,BAbuBRPTargetAll/BAbuBRPTargetAll,FAbuBRPLimitAll/FAbuBRPTargetAll,BAbuBRPLimitAll/BAbuBRPTargetAll,SimulationFishingYear,TRUE,TRUE,paste("Whole area (reference year: ",AdHocBRP[1,2]," to ",AdHocBRP[2,2],")",sep=""),"Abundance (A/ATarget)","Exploitation rate (F/FTarget)",paste(round(FAbuBRPTargetAll,4)," (",AdHocBRP[5,2]*100,"%)",sep=""),paste(format(BAbuBRPTargetAll,scientific=T,digits=2),"\n(",AdHocBRP[7,2]*100,"%)",sep=""),paste(round(FAbuBRPLimitAll,4)," (",AdHocBRP[6,2]*100,"%)",sep=""),paste(format(BAbuBRPLimitAll,scientific=T,digits=2),"\n(",AdHocBRP[8,2]*100,"%)",sep=""), NULL,NULL, NULL,NULL))
    savePlot(filename="Phase plot (F Abundance AdHoc) for whole area",type="png")
    
    print(PhasePlotFunction(MSYAbuBRPAll/MSYAbuBRPTargetAll,BAbuBRPAll/BAbuBRPTargetAll,MSYAbuBRPTargetAll/MSYAbuBRPTargetAll,BAbuBRPTargetAll/BAbuBRPTargetAll,MSYAbuBRPLimitAll/MSYAbuBRPTargetAll,BAbuBRPLimitAll/BAbuBRPTargetAll,SimulationFishingYear,TRUE,TRUE,paste("Whole area (reference year: ",AdHocBRP[1,2]," to ",AdHocBRP[2,2],")",sep=""),"Abundance (A/ATarget)","Catch(C/CTarget)",paste(format(MSYAbuBRPTargetAll,scientific=T,digits=2)," (",AdHocBRP[9,2]*100,"%)",sep=""),paste(format(BAbuBRPTargetAll,scientific=T,digits=2),"\n(",AdHocBRP[7,2]*100,"%)",sep=""),paste(format(MSYAbuBRPLimitAll,scientific=T,digits=2)," (",AdHocBRP[10,2]*100,"%)",sep=""),paste(format(BAbuBRPLimitAll,scientific=T,digits=2),"\n(",AdHocBRP[8,2]*100,"%)",sep=""), NULL,NULL, NULL,NULL))
    savePlot(filename="Phase plot (Catch Abundance AdHoc) for whole area",type="png")
    
  }
  if(BRPSwitches[4,2]==1 || BRPSwitches[4,2]==2) 
  {
    HistoricalCondition=rbind(HistoricalCondition,data.frame(F=FBioBRPAll,B=BBioBRPAll,Catch=MSYBioBRPAll,Year=SimulationFishingYear,Unit="Biomass",Area="Whole area"))
    
    BRP=rbind(BRP,data.frame(BRP=c("FTarget","FLimit","BTarget","BLimit","CTarget","CLimit"),value=c(FBioBRPTargetAll,FBioBRPLimitAll,BBioBRPTargetAll,BBioBRPLimitAll,MSYBioBRPTargetAll,MSYBioBRPLimitAll),Unit="Biomass",Area="Whole area"))		
    
    print(PhasePlotFunction(FBioBRPAll/FBioBRPTargetAll,BBioBRPAll/BBioBRPTargetAll,FBioBRPTargetAll/FBioBRPTargetAll,BBioBRPTargetAll/BBioBRPTargetAll,FBioBRPLimitAll/FBioBRPTargetAll,BBioBRPLimitAll/BBioBRPTargetAll,SimulationFishingYear,TRUE,TRUE,paste("Whole area (reference year: ",AdHocBRP[1,2]," to ",AdHocBRP[2,2],")",sep=""),"Biomass (B/BTarget)","Exploitation rate (F/FTarget)",paste(round(FBioBRPTargetAll,4)," (",AdHocBRP[5,2]*100,"%)",sep=""),paste(format(BBioBRPTargetAll,scientific=T,digits=2),"\n(",AdHocBRP[7,2]*100,"%)",sep=""),paste(round(FBioBRPLimitAll,4)," (",AdHocBRP[6,2]*100,"%)",sep=""),paste(format(BBioBRPLimitAll,scientific=T,digits=2),"\n(",AdHocBRP[8,2]*100,"%)",sep=""), NULL,NULL, NULL,NULL))
    savePlot(filename="Phase plot (F Biomass AdHoc) for whole area",type="png")
    
    print(PhasePlotFunction(MSYBioBRPAll/MSYBioBRPTargetAll,BBioBRPAll/BBioBRPTargetAll,MSYBioBRPTargetAll/MSYBioBRPTargetAll,BBioBRPTargetAll/BBioBRPTargetAll,MSYBioBRPLimitAll/MSYBioBRPTargetAll,BBioBRPLimitAll/BBioBRPTargetAll,SimulationFishingYear,TRUE,TRUE,paste("Whole area (reference year: ",AdHocBRP[1,2]," to ",AdHocBRP[2,2],")",sep=""),"Biomass (B/BTarget)","Catch(C/CTarget)",paste(format(MSYBioBRPTargetAll,scientific=T,digits=2)," (",AdHocBRP[9,2]*100,"%)",sep=""),paste(format(BBioBRPTargetAll,scientific=T,digits=2),"\n(",AdHocBRP[7,2]*100,"%)",sep=""),paste(format(MSYBioBRPLimitAll,scientific=T,digits=2)," (",AdHocBRP[10,2]*100,"%)",sep=""),paste(format(BBioBRPLimitAll,scientific=T,digits=2),"\n(",AdHocBRP[8,2]*100,"%)",sep=""), NULL,NULL, NULL,NULL))
    savePlot(filename="Phase plot (Catch Biomass AdHoc) for whole area",type="png")
    
  }
  
  if(BRPSwitches[4,2]==0) HistoricalCondition=subset(HistoricalCondition,Unit=="Abundance")
  if(BRPSwitches[4,2]==1) HistoricalCondition=subset(HistoricalCondition,Unit=="Biomass")
  
  names(HistoricalCondition$Year)=c("FishingYear")
  write.xlsx(HistoricalCondition,file="BRPOutput.xlsx", sheetName="HistoricalCondition", row.names=FALSE)
  
  write.xlsx(BRP,file="BRPOutput.xlsx", sheetName="BRP", row.names=FALSE, append=TRUE)	
  
}else if(BRPSwitches[1,2]==1 & BRPSwitches[2,2]==0) #theorical BRP
{
  #run the base case
  if(HistoricalSimulationYesNo==0)
  {
    ThisTimestep=1	
    CumulativeIntitialRecruit=0
    Simulation(SimulationYear,ManagementSetting[3,2],ManagementSetting[4,2],NULL,SimulationSetting[10,2],SimulationSetting[11,2],SimulationSetting[12,2],0,0,0,0,0)  
  }
  
  if(NumBurnInYears>0)
  {
    RowStart=min(which(Summary1$CalendarYear==(ManagementSetting[1,2]+NumBurnInYears) & Summary1$Timestep==ManagementSetting[3,2]))
    Summary1=Summary1[RowStart:dim(Summary1)[1],]
    RowStart=min(which(Summary2$CalendarYear==(ManagementSetting[1,2]+NumBurnInYears) & Summary2$Timestep==ManagementSetting[3,2]))
    Summary2=Summary2[RowStart:dim(Summary2)[1],]
  }
  
  #need to change SimulationYear (which was inputed as CalendarYear) to FishingYear so that the results will be comparable
  SimulationFishingYear=c(min(Summary1$Year):max(Summary1$Year)) 
  if(ManagementSetting[3,2]!=ManagementSetting[8,2]) SimulationFishingYear=SimulationFishingYear[SimulationFishingYear>min(Summary1$Year)]
  if(ManagementSetting[8,2]!=1 && ManagementSetting[4,2]!=(ManagementSetting[8,2]-1)) SimulationFishingYear=SimulationFishingYear[SimulationFishingYear<max(Summary1$Year)]
  
  #read in BRPControl.xlsx
  TheoreticalBRP=read.xlsx("BRPControl.xlsx",sheetName="TheoreticalBRP")
  RecruitTheoBRP=read.xlsx("BRPControl.xlsx",sheetName="RecruitTheoBRP")
  MovementTheoBRP=read.xlsx("BRPControl.xlsx",sheetName="MovementTheoBRP")
  
  MPAOutInTheoBRP=read.xlsx("BRPControl.xlsx",sheetName="MPAOutInTheoBRP") 
  MPAInOutTheoBRP=read.xlsx("BRPControl.xlsx",sheetName="MPAInOutTheoBRP") 
  MPAMaxKTheoBRP=read.xlsx("BRPControl.xlsx",sheetName="MPAMaxKTheoBRP")
  MPAMaxKTheoBRP=MPAMaxKTheoBRP/SimulationSetting[8,2]
  
  NaturalMTheoBRP=read.xlsx("BRPControl.xlsx",sheetName="NaturalMTheoBRP")
  EnRateTimestepTheoBRP=read.xlsx("BRPControl.xlsx",sheetName="OpenFishingTimestepTheoBRP")
  MinMaxLegalSizeTheoBRP=read.xlsx("BRPControl.xlsx",sheetName="MinMaxLegalSizeTheoBRP")
  HandlingMTheoBRP=read.xlsx("BRPControl.xlsx",sheetName="HandlingMTheoBRP")
  FisheryProbTheoBRP=read.xlsx("BRPControl.xlsx",sheetName="FisheryProbTheoBRP")
  
  if(any(TheoreticalBRP[4,2]!= subset(MinMaxLegalSizeTheoBRP,Legal=="Min")[,"Size"]))
    print("Minimum size for calculating biomass/abundance BRP is different from minimum legal size")
  if(any(TheoreticalBRP[5,2]!= subset(MinMaxLegalSizeTheoBRP,Legal=="Max")[,"Size"]))
    print("Maximum size for calculating biomass/abundance BRP is different from maximum legal size")	
  
  #set simulation year and f
  SimulationYearBRP=c(1:TheoreticalBRP[1,2])
  fBRPSeq=seq(0,TheoreticalBRP[3,2],by=TheoreticalBRP[2,2])
  
  #subset by referenced size classes
  Summary1AbuBio=subset(Summary1, SizeClass>=TheoreticalBRP[4,2] & SizeClass<=TheoreticalBRP[5,2])
  
  #sum by size class and timestep for each year and area
  AnnualSummary1AbuBio=aggregate(Summary1AbuBio$Quantity,by=as.list(Summary1AbuBio[,c("Name","Area","Year","Timestep")]),sum)
  if(ManagementSetting[3,2]!=ManagementSetting[8,2]) AnnualSummary1AbuBio=subset(AnnualSummary1AbuBio,Year>min(Summary1$Year))
  if(ManagementSetting[8,2]!=1 && ManagementSetting[4,2]!=(ManagementSetting[8,2]-1)) AnnualSummary1AbuBio=subset(AnnualSummary1AbuBio,Year<max(Summary1$Year))
  
  AnnualSummary2=aggregate(Summary2$Quantity,by=as.list(Summary2[,c("Name","Area","Year")]),sum)
  if(ManagementSetting[3,2]!=ManagementSetting[8,2]) AnnualSummary2=subset(AnnualSummary2,Year>min(Summary1$Year))
  if(ManagementSetting[8,2]!=1 && ManagementSetting[4,2]!=(ManagementSetting[8,2]-1)) AnnualSummary2=subset(AnnualSummary2,Year<max(Summary1$Year))
  
  #subset the quantity by each year and area
  if(ManagementSetting[8,2]==1)
  {
    AbuBRP=subset(AnnualSummary1AbuBio,Name==SummaryList[1] & Timestep==ManagementSetting[8,2])[,c("Name","Area","Year","x")] 
    BioBRP=subset(AnnualSummary1AbuBio,Name==SummaryList[2] & Timestep==ManagementSetting[8,2])[,c("Name","Area","Year","x")]
  }else
  {
    AbuBRP=subset(AnnualSummary1AbuBio,Name==SummaryList[1] & Timestep==(ManagementSetting[8,2]-1))[,c("Name","Area","Year","x")] 
    BioBRP=subset(AnnualSummary1AbuBio,Name==SummaryList[2] & Timestep==(ManagementSetting[8,2]-1))[,c("Name","Area","Year","x")]
  }
  CatchAbuBRP=subset(AnnualSummary2,Name==SummaryList[17])
  CatchBioBRP=subset(AnnualSummary2,Name==SummaryList[18])
  
  #average recruitment by year to get the BMSY
  
  if(ManagementSetting[3,2]!=ManagementSetting[8,2])  Summary1NoFirstLastYear=subset(Summary1,Year>min(Summary1$Year))
  if(ManagementSetting[8,2]!=1 && ManagementSetting[4,2]!=(ManagementSetting[8,2]-1))   Summary1NoFirstLastYear=subset(Summary1NoFirstLastYear,Year<max(Summary1$Year))
  if(ManagementSetting[3,2]==ManagementSetting[8,2])
  {
    if(ManagementSetting[8,2]!=1 && ManagementSetting[4,2]==(ManagementSetting[8,2]-1))Summary1NoFirstLastYear=Summary1
    else if(ManagementSetting[8,2]==1) Summary1NoFirstLastYear=Summary1
    
  }
  Summary1NoFirstLastYear=Summary1
  AnnualSummary1NoFirstLastYear=aggregate(Summary1NoFirstLastYear$Quantity,by=as.list(Summary1NoFirstLastYear[,c("Name","Area","Year")]),sum) 
  AnnualAverageSummary1NoFirstLastYear=aggregate(AnnualSummary1NoFirstLastYear$x,by=as.list(AnnualSummary1NoFirstLastYear[,c("Name","Area")]),mean)
  AvgRecruitAbuBRP=subset(AnnualAverageSummary1NoFirstLastYear,Name==SummaryList[9])
  AvgRecruitBioBRP=subset(AnnualAverageSummary1NoFirstLastYear,Name==SummaryList[10])
  
  #run the BRP simulation case
  TheoBRPArea=c()
  TheoBRPAll=c()
  
  for(fBRP in fBRPSeq)
  {
    ThisTimestep=1
    CumulativeIntitialRecruit=0
    
    if(ManagementSetting[8,2]==1) Simulation(SimulationYearBRP,ManagementSetting[8,2],4,fBRP,SimulationSetting[10,2],SimulationSetting[11,2],SimulationSetting[12,2],BRPSwitches[1,2],BRPSwitches[2,2],BRPSwitches[3,2],0,0) #theoretical BRP calculation always starts at the first timestep and ends at the last timestep of the defined year
    else Simulation(SimulationYearBRP,ManagementSetting[8,2],4,fBRP,SimulationSetting[10,2],SimulationSetting[11,2],SimulationSetting[12,2],BRPSwitches[1,2],BRPSwitches[2,2],BRPSwitches[3,2],0,0) #theoretical BRP calculation always starts at the first timestep and ends at the last timestep of the defined year
    
    #recruitment: one cohort
    if(BRPSwitches[3,2]==0)
    {
      AllSummary1=aggregate(Summary1$Quantity,by=as.list(Summary1[,c("Name","Area")]),sum)
      AllSummary2=aggregate(Summary2$Quantity,by=as.list(Summary2[,c("Name","Area")]),sum)
      AllSummary1SSB=aggregate(Summary1$Quantity,by=as.list(Summary1[,c("Name","Area","Timestep")]),sum)
    }
    
    #recruitment: multiple cohort
    if(BRPSwitches[3,2]==1)
    {
      AnnualSummary1=aggregate(Summary1$Quantity,by=as.list(Summary1[,c("Name","Area","Year")]),sum)
      AnnualSummary1SSB=aggregate(Summary1$Quantity,by=as.list(Summary1[,c("Name","Area","Year","Timestep")]),sum)			
      AnnualSummary2=aggregate(Summary2$Quantity,by=as.list(Summary2[,c("Name","Area","Year")]),sum)    
      AllSummary1=aggregate(AnnualSummary1$x,by=as.list(AnnualSummary1[,c("Name","Area")]),mean)
      AllSummary1SSB=aggregate(AnnualSummary1SSB$x,by=as.list(AnnualSummary1SSB[,c("Name","Area","Timestep")]),mean)			
      AllSummary2=aggregate(AnnualSummary2$x,by=as.list(AnnualSummary2[,c("Name","Area")]),mean)
    }
    
    #more then one area
    if(NumArea>1)
    {
      for(j in 1:NumArea) #in different area
      { 		 
        CatchAbuTheoBRP=subset(AllSummary2, Name==SummaryList[17] & Area==paste("Area",j))$x
        CatchBioTheoBRP=subset(AllSummary2, Name==SummaryList[18] & Area==paste("Area",j))$x
        if(ManagementSetting[8,2]==1) 
        {
          SSBAbuTheoBRP=subset(AllSummary1SSB, Name==SummaryList[7]& Area==paste("Area",j) & Timestep==ManagementSetting[8,2])$x
          SSBBioTheoBRP=subset(AllSummary1SSB, Name==SummaryList[8]& Area==paste("Area",j) & Timestep==ManagementSetting[8,2])$x
        }
        else
        {
          SSBAbuTheoBRP=subset(AllSummary1SSB, Name==SummaryList[7]& Area==paste("Area",j) & Timestep==(ManagementSetting[8,2]-1))$x
          SSBBioTheoBRP=subset(AllSummary1SSB, Name==SummaryList[8]& Area==paste("Area",j) & Timestep==(ManagementSetting[8,2]-1))$x				 
        }
        RAbuTheoBRP=subset(AllSummary1, Name==SummaryList[9] & Area==paste("Area",j))$x
        RBioTheoBRP=subset(AllSummary1, Name==SummaryList[10] & Area==paste("Area",j))$x
        
        TheoBRPArea=rbind(TheoBRPArea,data.frame(SSBAbuTheoBRP,SSBBioTheoBRP,YPRAbu=CatchAbuTheoBRP/RAbuTheoBRP,YPRBio=CatchBioTheoBRP/RBioTheoBRP,SPRAbu=SSBAbuTheoBRP/RAbuTheoBRP,SPRBio=SSBBioTheoBRP/RBioTheoBRP,Area=paste("Area",j),FValue=fBRP))
      }
    }
    
    #for all
    CatchAbuTheoBRP=sum(subset(AllSummary2, Name==SummaryList[17])$x)
    CatchBioTheoBRP=sum(subset(AllSummary2, Name==SummaryList[18])$x)
    if(ManagementSetting[8,2]==1)
    {
      SSBAbuTheoBRP=sum(subset(AllSummary1SSB, Name==SummaryList[7] & Timestep==ManagementSetting[8,2])$x)
      SSBBioTheoBRP=sum(subset(AllSummary1SSB, Name==SummaryList[8] & Timestep==ManagementSetting[8,2])$x)
    }
    else
    {
      SSBAbuTheoBRP=sum(subset(AllSummary1SSB, Name==SummaryList[7] & Timestep==(ManagementSetting[8,2]-1))$x)
      SSBBioTheoBRP=sum(subset(AllSummary1SSB, Name==SummaryList[8] & Timestep==(ManagementSetting[8,2]-1))$x)		
    }
    RAbuTheoBRP=sum(subset(AllSummary1, Name==SummaryList[9])$x)
    RBioTheoBRP=sum(subset(AllSummary1, Name==SummaryList[10])$x)	
    
    TheoBRPAll=rbind(TheoBRPAll,data.frame(SSBAbuTheoBRP,SSBBioTheoBRP,YPRAbu=CatchAbuTheoBRP/RAbuTheoBRP,YPRBio=CatchBioTheoBRP/RBioTheoBRP,SPRAbu=SSBAbuTheoBRP/RAbuTheoBRP,SPRBio=SSBBioTheoBRP/RBioTheoBRP,Area="All",FValue=fBRP))
  }
  
  TheoBRP=rbind(TheoBRPAll,TheoBRPArea)
  
  #if there are more then one area
  if(NumArea==1) FromTo=1
  else FromTo=NumArea+1
  
  TheoBRPDataOutput=c()
  HistoricalCondition=c()
  BRP=c()
  fpercentcode=paste("F",TheoreticalBRP[6,2]*100,"%",sep="") #used for fpercent name
  
  windows(width=PlotWidth, height=PlotHeight, record=T)
  
  for(k in 1:FromTo)
  {
    if(FromTo==1 || k==NumArea+1)
    {
      TheoBRPtemp=subset(TheoBRP,Area=="All")
      AvgRecruitAbuBRPtemp=AvgRecruitAbuBRP$x #average recruitment from historical recruitment
      AvgRecruitBioBRPtemp=AvgRecruitBioBRP$x			
    }
    else 
    {
      TheoBRPtemp=subset(TheoBRP,Area==paste("Area",j))
      AvgRecruitAbuBRPtemp=subset(AvgRecruitAbuBRP,Area==paste("Area",k))$x #average recruitment from historical recruitment
      AvgRecruitBioBRPtemp=subset(AvgRecruitBioBRP,Area==paste("Area",k))$x		
    }	
    
    #calculate FMax and BMSY at FMax
    FMaxAbu=TheoBRPtemp$FValue[which.max(TheoBRPtemp$YPRAbu)]
    FMaxBio=TheoBRPtemp$FValue[which.max(TheoBRPtemp$YPRBio)]
    BMaxAbu=TheoBRPtemp$SPRAbu[which(TheoBRPtemp$FValue==FMaxAbu)]*AvgRecruitAbuBRPtemp
    BMaxBio=TheoBRPtemp$SPRBio[which(TheoBRPtemp$FValue==FMaxBio)]*AvgRecruitBioBRPtemp
    CatchMaxAbu=TheoBRPtemp$YPRAbu[which(TheoBRPtemp$FValue==FMaxAbu)]*AvgRecruitAbuBRPtemp
    CatchMaxBio=TheoBRPtemp$YPRBio[which(TheoBRPtemp$FValue==FMaxBio)]*AvgRecruitBioBRPtemp
    
    YPRAbuSlope=c()
    YPRBioSlope=c()
    for(l in 1:which(TheoBRPtemp$FValue==FMaxAbu)) YPRAbuSlope=c(YPRAbuSlope,(TheoBRPtemp$YPRAbu[l+1]-TheoBRPtemp$YPRAbu[l])/TheoreticalBRP[2,2])
    for(l in 1:which(TheoBRPtemp$FValue==FMaxBio)) YPRBioSlope=c(YPRBioSlope,(TheoBRPtemp$YPRBio[l+1]-TheoBRPtemp$YPRBio[l])/TheoreticalBRP[2,2])
    
    YPRAbuSlopeDiff=abs(max(YPRAbuSlope,na.omit=T)*0.1-YPRAbuSlope) #difference of slope and 10% of max slope 
    YPRBioSlopeDiff=abs(max(YPRBioSlope,na.rm=T)*0.1-YPRBioSlope)
    YPRAbuSlope01Pos=which.min(YPRAbuSlopeDiff)#find the smallest difference to determine where F0.1 is
    YPRBioSlope01Pos=which.min(YPRBioSlopeDiff)
    
    F01Abu=TheoBRPtemp$FValue[YPRAbuSlope01Pos]
    F01Bio=TheoBRPtemp$FValue[YPRBioSlope01Pos]
    B01Abu=TheoBRPtemp$SPRAbu[YPRAbuSlope01Pos]*AvgRecruitAbuBRPtemp
    B01Bio=TheoBRPtemp$SPRBio[YPRBioSlope01Pos]*AvgRecruitBioBRPtemp
    Catch01Abu=TheoBRPtemp$YPRAbu[YPRAbuSlope01Pos]*AvgRecruitAbuBRPtemp
    Catch01Bio=TheoBRPtemp$YPRBio[YPRBioSlope01Pos]*AvgRecruitBioBRPtemp
    
    TheoBRPtemp$SPRAbuRelativeMAXSPR=TheoBRPtemp$SPRAbu/max(TheoBRPtemp$SPRAbu)
    TheoBRPtemp$SPRBioRelativeMAXSPR=TheoBRPtemp$SPRBio/max(TheoBRPtemp$SPRBio)
    
    SPRAbuDiff=abs(TheoBRPtemp$SPRAbuRelativeMAXSPR-TheoreticalBRP[6,2])
    SPRBioDiff=abs(TheoBRPtemp$SPRBioRelativeMAXSPR-TheoreticalBRP[6,2])
    
    SPRAbuPercentPos=which.min(SPRAbuDiff)
    SPRBioPercentPos=which.min(SPRBioDiff)
    
    FPercentAbu=TheoBRPtemp$FValue[SPRAbuPercentPos]
    FPercentBio=TheoBRPtemp$FValue[SPRBioPercentPos]
    BPercentAbu=TheoBRPtemp$SPRAbu[SPRAbuPercentPos]*AvgRecruitAbuBRPtemp
    BPercentBio=TheoBRPtemp$SPRBio[SPRBioPercentPos]*AvgRecruitBioBRPtemp
    CatchPercentAbu=TheoBRPtemp$YPRAbu[SPRAbuPercentPos]*AvgRecruitAbuBRPtemp
    CatchPercentBio=TheoBRPtemp$YPRBio[SPRBioPercentPos]*AvgRecruitBioBRPtemp
    
    FMaxAbuTheoBRPTarget=TheoreticalBRP[7,2]*FMaxAbu
    FMaxBioTheoBRPTarget=TheoreticalBRP[7,2]*FMaxBio
    BMaxAbuTheoBRPTarget=TheoreticalBRP[9,2]*BMaxAbu
    BMaxBioTheoBRPTarget=TheoreticalBRP[9,2]*BMaxBio
    CatchMaxAbuTheoBRPTarget=TheoreticalBRP[11,2]*CatchMaxAbu
    CatchMaxBioTheoBRPTarget=TheoreticalBRP[11,2]*CatchMaxBio	
    FMaxAbuTheoBRPLimit=TheoreticalBRP[8,2]*FMaxAbu
    FMaxBioTheoBRPLimit=TheoreticalBRP[8,2]*FMaxBio
    BMaxAbuTheoBRPLimit=TheoreticalBRP[10,2]*BMaxAbu
    BMaxBioTheoBRPLimit=TheoreticalBRP[10,2]*BMaxBio
    CatchMaxAbuTheoBRPLimit=TheoreticalBRP[12,2]*CatchMaxAbu
    CatchMaxBioTheoBRPLimit=TheoreticalBRP[12,2]*CatchMaxBio
    
    TheoBRPDataOutput=rbind(TheoBRPDataOutput,data.frame(FAbuTheoBRPTarget=FMaxAbuTheoBRPTarget,FAbuTheoBRPLimit=FMaxAbuTheoBRPLimit,BAbuTheoBRPTarget=BMaxAbuTheoBRPTarget,BAbuTheoBRPLimit=BMaxAbuTheoBRPLimit,CatchAbuTheoBRPTarget=CatchMaxAbuTheoBRPTarget,CatchAbuTheoBRPLimit=CatchMaxAbuTheoBRPLimit,FBioTheoBRPTarget=FMaxBioTheoBRPTarget,FBioTheoBRPLimit=FMaxBioTheoBRPLimit,BBioTheoBRPTarget=BMaxBioTheoBRPTarget,BBioTheoBRPLimit=BMaxBioTheoBRPLimit,CatchBioTheoBRPTarget=CatchMaxBioTheoBRPTarget,CatchBioTheoBRPLimit=CatchMaxBioTheoBRPLimit,Area=paste("Area",k),FMSY="FMax"))
    
    F01AbuTheoBRPTarget=TheoreticalBRP[7,2]*F01Abu
    F01BioTheoBRPTarget=TheoreticalBRP[7,2]*F01Bio
    B01AbuTheoBRPTarget=TheoreticalBRP[9,2]*B01Abu
    B01BioTheoBRPTarget=TheoreticalBRP[9,2]*B01Bio
    Catch01AbuTheoBRPTarget=TheoreticalBRP[11,2]*Catch01Abu
    Catch01BioTheoBRPTarget=TheoreticalBRP[11,2]*Catch01Bio	
    F01AbuTheoBRPLimit=TheoreticalBRP[8,2]*F01Abu
    F01BioTheoBRPLimit=TheoreticalBRP[8,2]*F01Bio
    B01AbuTheoBRPLimit=TheoreticalBRP[10,2]*B01Abu
    B01BioTheoBRPLimit=TheoreticalBRP[10,2]*B01Bio
    Catch01AbuTheoBRPLimit=TheoreticalBRP[12,2]*Catch01Abu
    Catch01BioTheoBRPLimit=TheoreticalBRP[12,2]*Catch01Bio
    
    TheoBRPDataOutput=rbind(TheoBRPDataOutput,data.frame(FAbuTheoBRPTarget=F01AbuTheoBRPTarget,FAbuTheoBRPLimit=F01AbuTheoBRPLimit,BAbuTheoBRPTarget=B01AbuTheoBRPTarget,BAbuTheoBRPLimit=B01AbuTheoBRPLimit,CatchAbuTheoBRPTarget=Catch01AbuTheoBRPTarget,CatchAbuTheoBRPLimit=Catch01AbuTheoBRPLimit,FBioTheoBRPTarget=F01BioTheoBRPTarget,FBioTheoBRPLimit=F01BioTheoBRPLimit,BBioTheoBRPTarget=B01BioTheoBRPTarget,BBioTheoBRPLimit=B01BioTheoBRPLimit,CatchBioTheoBRPTarget=Catch01BioTheoBRPTarget,CatchBioTheoBRPLimit=Catch01BioTheoBRPLimit,Area=paste("Area",k),FMSY="F0.1"))
    
    FPercentAbuTheoBRPTarget=TheoreticalBRP[7,2]*FPercentAbu
    FPercentBioTheoBRPTarget=TheoreticalBRP[7,2]*FPercentBio
    BPercentAbuTheoBRPTarget=TheoreticalBRP[9,2]*BPercentAbu
    BPercentBioTheoBRPTarget=TheoreticalBRP[9,2]*BPercentBio
    CatchPercentAbuTheoBRPTarget=TheoreticalBRP[11,2]*CatchPercentAbu
    CatchPercentBioTheoBRPTarget=TheoreticalBRP[11,2]*CatchPercentBio	
    FPercentAbuTheoBRPLimit=TheoreticalBRP[8,2]*FPercentAbu
    FPercentBioTheoBRPLimit=TheoreticalBRP[8,2]*FPercentBio
    BPercentAbuTheoBRPLimit=TheoreticalBRP[10,2]*BPercentAbu
    BPercentBioTheoBRPLimit=TheoreticalBRP[10,2]*BPercentBio
    CatchPercentAbuTheoBRPLimit=TheoreticalBRP[12,2]*CatchPercentAbu
    CatchPercentBioTheoBRPLimit=TheoreticalBRP[12,2]*CatchPercentBio
    
    TheoBRPDataOutput=rbind(TheoBRPDataOutput,data.frame(FAbuTheoBRPTarget=FPercentAbuTheoBRPTarget,FAbuTheoBRPLimit=FPercentAbuTheoBRPLimit,BAbuTheoBRPTarget=BPercentAbuTheoBRPTarget,BAbuTheoBRPLimit=BPercentAbuTheoBRPLimit,CatchAbuTheoBRPTarget=CatchPercentAbuTheoBRPTarget,CatchAbuTheoBRPLimit=CatchPercentAbuTheoBRPLimit,FBioTheoBRPTarget=FPercentBioTheoBRPTarget,FBioTheoBRPLimit=FPercentBioTheoBRPLimit,BBioTheoBRPTarget=BPercentBioTheoBRPTarget,BBioTheoBRPLimit=BPercentBioTheoBRPLimit,CatchBioTheoBRPTarget=CatchPercentBioTheoBRPTarget,CatchBioTheoBRPLimit=CatchPercentBioTheoBRPLimit,Area=paste("Area",k),FMSY=fpercentcode))
    
    #YPR and SPR curve plot
    
    #phase plot for each Fmsy
    if(FromTo==1 || k==NumArea+1) #need plots for whole area
    {
      #calculate sum by year (sum over areas)
      AbuBRPtemp=aggregate(AbuBRP$x,by=as.list(AbuBRP[,c("Name","Year")]),sum) #for whole area
      BioBRPtemp=aggregate(BioBRP$x,by=as.list(BioBRP[,c("Name","Year")]),sum)
      CatchAbuBRPtemp=aggregate(CatchAbuBRP$x,by=as.list(CatchAbuBRP[,c("Name","Year")]),sum)$x
      CatchBioBRPtemp=aggregate(CatchBioBRP$x,by=as.list(CatchBioBRP[,c("Name","Year")]),sum)$x
      plottitle="Whole Area"			
    }
    else
    {		
      AbuBRPtemp=subset(AbuBRP,Area==paste("Area",k))
      BioBRPtemp=subset(BioBRP,Area==paste("Area",k))
      CatchAbuBRPtemp=subset(CatchAbuBRP,Area==paste("Area",k))$x
      CatchBioBRPtemp=subset(CatchBioBRP,Area==paste("Area",k))$x
      plottitle=paste("Area",k)
    }	
    
    #calcualte F and B for each year
    FAbuBRPtemp=CatchAbuBRPtemp/(AbuBRPtemp$x+CatchAbuBRPtemp)
    FBioBRPtemp=CatchBioBRPtemp/(BioBRPtemp$x+CatchBioBRPtemp)
    BAbuBRPtemp=AbuBRPtemp$x
    BBioBRPtemp=BioBRPtemp$x	
    
    if(BRPSwitches[4,2]==0 || BRPSwitches[4,2]==2)
    {
      HistoricalCondition=rbind(HistoricalCondition,data.frame(F=FAbuBRPtemp,B=BAbuBRPtemp,Catch=CatchAbuBRPtemp,Year=SimulationFishingYear,Unit="Abundance",Area=plottitle))
      
      BRP=rbind(BRP,data.frame(BRP=c("FMaxTarget","FMaxLimit","BTarget(FMax)","BLimit(FMax)","F0.1Target","F0.1Limit","BTarget(F0.1)","BLimit(F0.1)",paste(fpercentcode,"Target",sep=""),paste(fpercentcode,"Limit",sep=""),paste("BTarget(",fpercentcode,")",sep=""),paste("BLimit(",fpercentcode,")",sep=""),"CatchTarget(FMax)","CatchLimit(FMax)","CatchTarget(F0.1)","CatchLimit(F0.1)",paste("CatchTarget(",fpercentcode,")",sep=""),paste("CatchLimit(",fpercentcode,")",sep="")),value=c(FMaxAbuTheoBRPTarget,FMaxAbuTheoBRPLimit,BMaxAbuTheoBRPTarget,BMaxAbuTheoBRPLimit,F01AbuTheoBRPTarget,F01AbuTheoBRPLimit,B01AbuTheoBRPTarget,B01AbuTheoBRPLimit,FPercentAbuTheoBRPTarget,FPercentAbuTheoBRPLimit,BPercentAbuTheoBRPTarget,BPercentAbuTheoBRPLimit,CatchMaxAbuTheoBRPTarget,CatchMaxAbuTheoBRPLimit,Catch01AbuTheoBRPTarget,Catch01AbuTheoBRPLimit,CatchPercentAbuTheoBRPTarget,CatchPercentAbuTheoBRPLimit),Unit="Abundance",Area=plottitle))
      
      print(ggplot(TheoBRPtemp,aes(FValue,YPRAbu))+geom_line(size=1)+geom_point(data=subset(TheoBRPtemp,FValue==FMaxAbu),aes(FValue,YPRAbu),size=5,colour="red")+geom_point(data=subset(TheoBRPtemp,FValue==F01Abu),aes(FValue,YPRAbu),size=5,colour="blue")+labs(x="F",y="YPR (abundance)")+labs(title=paste("YPR curve with FMax=",FMaxAbu,"(red) and F0.1=",F01Abu,"(blue) for ",plottitle,sep=""))+theme_bw())
      savePlot(filename=paste("YPR curve (abundance) for",plottitle),type="png")
      
      print(ggplot(TheoBRPtemp,aes(FValue,SPRAbuRelativeMAXSPR))+geom_line(size=1)+geom_point(data=subset(TheoBRPtemp,FValue==FPercentAbu),aes(FValue,SPRAbuRelativeMAXSPR),size=5,colour="red")+labs(x="F",y="SPR (abundance)")+labs(title=paste("SPR curve with ",fpercentcode,"=",FMaxAbu,"(red) for ",plottitle,sep=""))+theme_bw())
      savePlot(filename=paste("SPR curve (abundance) for",plottitle),type="png")
      
      print(PhasePlotFunction(FAbuBRPtemp/FMaxAbuTheoBRPTarget,BAbuBRPtemp/BMaxAbuTheoBRPTarget,FMaxAbuTheoBRPTarget/FMaxAbuTheoBRPTarget,BMaxAbuTheoBRPTarget/BMaxAbuTheoBRPTarget,FMaxAbuTheoBRPLimit/FMaxAbuTheoBRPTarget,BMaxAbuTheoBRPLimit/BMaxAbuTheoBRPTarget,SimulationFishingYear,TRUE,TRUE,paste(plottitle," (use FMax=",FMaxAbu," as FMSY proxy)",sep=""),"Abundance (A/ATarget)","Exploitation rate (F/FTarget)",paste(round(FMaxAbuTheoBRPTarget,4)," (",TheoreticalBRP[7,2]*100,"%)",sep=""),paste(format(BMaxAbuTheoBRPTarget,scientific=T,digits=2),"\n(",TheoreticalBRP[9,2]*100,"%)",sep=""),paste(round(FMaxAbuTheoBRPLimit,4)," (",TheoreticalBRP[8,2]*100,"%)",sep=""),paste(format(BMaxAbuTheoBRPLimit,scientific=T,digits=2),"\n(",TheoreticalBRP[10,2]*100,"%)",sep=""),FMaxAbu/FMaxAbuTheoBRPTarget,BMaxAbu/BMaxAbuTheoBRPTarget,round(FMaxAbu,4),format(BMaxAbu,scientific=T,digits=2)))
      savePlot(filename=paste("Phase plot (abundance and FMax) for",plottitle),type="png")
      
      print(PhasePlotFunction(CatchAbuBRPtemp/CatchMaxAbuTheoBRPTarget,BAbuBRPtemp/BMaxAbuTheoBRPTarget,CatchMaxAbuTheoBRPTarget/CatchMaxAbuTheoBRPTarget,BMaxAbuTheoBRPTarget/BMaxAbuTheoBRPTarget,CatchMaxAbuTheoBRPLimit/CatchMaxAbuTheoBRPTarget,BMaxAbuTheoBRPLimit/BMaxAbuTheoBRPTarget,SimulationFishingYear,TRUE,TRUE,paste(plottitle," (use Catch=",round(CatchMaxAbu,2)," as MSY proxy) (FMax)",sep=""),"Abundance (A/ATarget)","Catch (C/CTarget)",paste(format(CatchMaxAbuTheoBRPTarget,scientific=T,digits=2)," (",TheoreticalBRP[11,2]*100,"%)",sep=""),paste(format(BMaxAbuTheoBRPTarget,scientific=T,digits=2),"\n(",TheoreticalBRP[9,2]*100,"%)",sep=""),paste(format(CatchMaxAbuTheoBRPLimit,scientific=T,digits=2)," (",TheoreticalBRP[12,2]*100,"%)",sep=""),paste(format(BMaxAbuTheoBRPLimit,scientific=T,digits=2),"\n(",TheoreticalBRP[10,2]*100,"%)",sep=""),CatchMaxAbu/CatchMaxAbuTheoBRPTarget,BMaxAbu/BMaxAbuTheoBRPTarget,format(CatchMaxAbu,scientific=T,digits=2),format(BMaxAbu,scientific=T,digits=2)))
      savePlot(filename=paste("Phase plot (abundance and catch-FMax) for",plottitle),type="png")
      
      print(PhasePlotFunction(FAbuBRPtemp/F01AbuTheoBRPTarget,BAbuBRPtemp/B01AbuTheoBRPTarget,F01AbuTheoBRPTarget/F01AbuTheoBRPTarget,B01AbuTheoBRPTarget/B01AbuTheoBRPTarget,F01AbuTheoBRPLimit/F01AbuTheoBRPTarget,B01AbuTheoBRPLimit/B01AbuTheoBRPTarget,SimulationFishingYear,TRUE,TRUE,paste(plottitle," (use F0.1=",F01Abu," as FMSY proxy)",sep=""),"Abundance (A/ATarget)","Exploitation rate (F/FTarget)",paste(round(F01AbuTheoBRPTarget,4)," (",TheoreticalBRP[7,2]*100,"%)",sep=""),paste(format(B01AbuTheoBRPTarget,scientific=T,digits=2),"\n(",TheoreticalBRP[9,2]*100,"%)",sep=""),paste(round(F01AbuTheoBRPLimit,4)," (",TheoreticalBRP[8,2]*100,"%)",sep=""),paste(format(B01AbuTheoBRPLimit,scientific=T,digits=2),"\n(",TheoreticalBRP[10,2]*100,"%)",sep=""),F01Abu/F01AbuTheoBRPTarget,B01Abu/B01AbuTheoBRPTarget,round(F01Abu,4),format(B01Abu,scientific=T,digits=2)))
      savePlot(filename=paste("Phase plot (abundance and F01) for",plottitle),type="png")
      
      print(PhasePlotFunction(CatchAbuBRPtemp/Catch01AbuTheoBRPTarget,BAbuBRPtemp/B01AbuTheoBRPTarget,Catch01AbuTheoBRPTarget/Catch01AbuTheoBRPTarget,B01AbuTheoBRPTarget/B01AbuTheoBRPTarget,Catch01AbuTheoBRPLimit/Catch01AbuTheoBRPTarget,B01AbuTheoBRPLimit/B01AbuTheoBRPTarget,SimulationFishingYear,TRUE,TRUE,paste(plottitle," (use catch=",round(Catch01Abu,2)," as MSY proxy) (F0.1)",sep=""),"Abundance (A/ATarget)","Catch (C/CTarget)",paste(format(Catch01AbuTheoBRPTarget,scientific=T,digits=2)," (",TheoreticalBRP[11,2]*100,"%)",sep=""),paste(format(B01AbuTheoBRPTarget,scientific=T,digits=2),"\n(",TheoreticalBRP[9,2]*100,"%)",sep=""),paste(format(Catch01AbuTheoBRPLimit,scientific=T,digits=2)," (",TheoreticalBRP[12,2]*100,"%)",sep=""),paste(format(B01AbuTheoBRPLimit,scientific=T,digits=2),"\n(",TheoreticalBRP[10,2]*100,"%)",sep=""),Catch01Abu/Catch01AbuTheoBRPTarget,B01Abu/B01AbuTheoBRPTarget,format(Catch01Abu,scientific=T,digits=2),format(B01Abu,scientific=T,digits=2)))
      savePlot(filename=paste("Phase plot (abundance and catch-F01) for ",plottitle),type="png")
      
      print(PhasePlotFunction(FAbuBRPtemp/FPercentAbuTheoBRPTarget,BAbuBRPtemp/BPercentAbuTheoBRPTarget,FPercentAbuTheoBRPTarget/FPercentAbuTheoBRPTarget,BPercentAbuTheoBRPTarget/BPercentAbuTheoBRPTarget,FPercentAbuTheoBRPLimit/FPercentAbuTheoBRPTarget,BPercentAbuTheoBRPLimit/BPercentAbuTheoBRPTarget,SimulationFishingYear,TRUE,TRUE,paste(plottitle," (use ",fpercentcode,"=",FPercentAbu," as FMSY proxy)",sep=""),"Abundance (A/ATarget)","Exploitation rate (F/FTarget)",paste(round(FPercentAbuTheoBRPTarget,4)," (",TheoreticalBRP[7,2]*100,"%)",sep=""),paste(format(BPercentAbuTheoBRPTarget,scientific=T,digits=2),"\n(",TheoreticalBRP[9,2]*100,"%)",sep=""),paste(round(FPercentAbuTheoBRPLimit,4)," (",TheoreticalBRP[8,2]*100,"%)",sep=""),paste(format(BPercentAbuTheoBRPLimit,scientific=T,digits=2),"\n(",TheoreticalBRP[10,2]*100,"%)",sep=""),FPercentAbu/FPercentAbuTheoBRPTarget,BPercentAbu/BPercentAbuTheoBRPTarget,round(FPercentAbu,4),format(BPercentAbu,scientific=T,digits=2)))
      savePlot(filename=paste("Phase plot (abundance and FPercent) for",plottitle),type="png")
      
      print(PhasePlotFunction(CatchAbuBRPtemp/CatchPercentAbuTheoBRPTarget,BAbuBRPtemp/BPercentAbuTheoBRPTarget,CatchPercentAbuTheoBRPTarget/CatchPercentAbuTheoBRPTarget,BPercentAbuTheoBRPTarget/BPercentAbuTheoBRPTarget,CatchPercentAbuTheoBRPLimit/CatchPercentAbuTheoBRPTarget,BPercentAbuTheoBRPLimit/BPercentAbuTheoBRPTarget,SimulationFishingYear,TRUE,TRUE,paste(plottitle," (use catch=",round(CatchPercentAbu,2)," as MSY proxy) (",fpercentcode,")",sep=""),"Abundance (A/ATarget)","Catch (C/CTarget)",paste(format(CatchPercentAbuTheoBRPTarget,scientific=T,digits=2)," (",TheoreticalBRP[11,2]*100,"%)",sep=""),paste(format(BPercentAbuTheoBRPTarget,scientific=T,digits=2),"\n(",TheoreticalBRP[9,2]*100,"%)",sep=""),paste(format(CatchPercentAbuTheoBRPLimit,scientific=T,digits=2)," (",TheoreticalBRP[12,2]*100,"%)",sep=""),paste(format(BPercentAbuTheoBRPLimit,scientific=T,digits=2),"\n(",TheoreticalBRP[10,2]*100,"%)",sep=""),CatchPercentAbu/CatchPercentAbuTheoBRPTarget,BPercentAbu/BPercentAbuTheoBRPTarget,format(CatchPercentAbu,scientific=T,digits=2),format(BPercentAbu,scientific=T,digits=2)))
      savePlot(filename=paste("Phase plot (abundance and catch-FPercent) for",plottitle),type="png")		
      
    }
    if(BRPSwitches[4,2]==1 || BRPSwitches[4,2]==2)
    {
      HistoricalCondition=rbind(HistoricalCondition,data.frame(F=FBioBRPtemp,B=BBioBRPtemp,Catch=CatchBioBRPtemp,Year=SimulationFishingYear,Unit="Biomass",Area=plottitle))
      
      BRP=rbind(BRP,data.frame(BRP=c("FMaxTarget","FMaxLimit","BTarget(FMax)","BLimit(FMax)","F0.1Target","F0.1Limit","BTarget(F0.1)","BLimit(F0.1)",paste(fpercentcode,"Target",sep=""),paste(fpercentcode,"Limit",sep=""),paste("BTarget(",fpercentcode,")",sep=""),paste("BLimit(",fpercentcode,")",sep=""),"CatchTarget(FMax)","CatchLimit(FMax)","CatchTarget(F0.1)","CatchLimit(F0.1)",paste("CatchTarget(",fpercentcode,")",sep=""),paste("CatchLimit(",fpercentcode,")",sep="")),value=c(FMaxBioTheoBRPTarget,FMaxBioTheoBRPLimit,BMaxBioTheoBRPTarget,BMaxBioTheoBRPLimit,F01BioTheoBRPTarget,F01BioTheoBRPLimit,B01BioTheoBRPTarget,B01BioTheoBRPLimit,FPercentBioTheoBRPTarget,FPercentBioTheoBRPLimit,BPercentBioTheoBRPTarget,BPercentBioTheoBRPLimit,CatchMaxBioTheoBRPTarget,CatchMaxBioTheoBRPLimit,Catch01BioTheoBRPTarget,Catch01BioTheoBRPLimit,CatchPercentBioTheoBRPTarget,CatchPercentBioTheoBRPLimit),Unit="Biomass",Area=plottitle))
      
      print(ggplot(TheoBRPtemp,aes(FValue,YPRBio))+geom_line(size=1)+geom_point(data=subset(TheoBRPtemp,FValue==FMaxBio),aes(FValue,YPRBio),size=5,colour="red")+geom_point(data=subset(TheoBRPtemp,FValue==F01Bio),aes(FValue,YPRBio),size=5,colour="blue")+labs(x="F",y="YPR (biomass)")+labs(title=paste("YPR curve with FMax=",FMaxBio,"(red) and F0.1=",F01Bio,"(blue) for ",plottitle,sep=""))+theme_bw())
      savePlot(filename=paste("YPR curve (biomass) for",plottitle),type="png")
      
      print(ggplot(TheoBRPtemp,aes(FValue,SPRBioRelativeMAXSPR))+geom_line(size=1)+geom_point(data=subset(TheoBRPtemp,FValue==FPercentBio),aes(FValue,SPRBioRelativeMAXSPR),size=5,colour="red")+labs(x="F",y="SPR (biomass)")+labs(title=paste("SPR curve with ",fpercentcode,"=",FMaxBio,"(red) for ",plottitle,sep=""))+theme_bw())
      savePlot(filename=paste("SPR curve (biomass) for",plottitle),type="png")				
      print(PhasePlotFunction(FBioBRPtemp/FMaxBioTheoBRPTarget,BBioBRPtemp/BMaxBioTheoBRPTarget,FMaxBioTheoBRPTarget/FMaxBioTheoBRPTarget,BMaxBioTheoBRPTarget/BMaxBioTheoBRPTarget,FMaxBioTheoBRPLimit/FMaxBioTheoBRPTarget,BMaxBioTheoBRPLimit/BMaxBioTheoBRPTarget,SimulationFishingYear,TRUE,TRUE,paste(plottitle," (use FMax=",FMaxBio," as FMSY proxy)",sep=""),"Biomass (B/BTarget)","Exploitation rate (F/FTarget)",paste(round(FMaxBioTheoBRPTarget,4)," (",TheoreticalBRP[7,2]*100,"%)",sep=""),paste(format(BMaxBioTheoBRPTarget,scientific=T,digits=2),"\n(",TheoreticalBRP[9,2]*100,"%)",sep=""),paste(round(FMaxBioTheoBRPLimit,4)," (",TheoreticalBRP[8,2]*100,"%)",sep=""),paste(format(BMaxBioTheoBRPLimit,scientific=T,digits=2),"\n(",TheoreticalBRP[10,2]*100,"%)",sep=""),FMaxBio/FMaxBioTheoBRPTarget,BMaxBio/BMaxBioTheoBRPTarget,round(FMaxBio,4),format(BMaxBio,scientific=T,digits=2)))
      savePlot(filename=paste("Phase plot (biomass and FMax) for",plottitle),type="png")			
      
      print(PhasePlotFunction(CatchBioBRPtemp/CatchMaxBioTheoBRPTarget,BBioBRPtemp/BMaxBioTheoBRPTarget,CatchMaxBioTheoBRPTarget/CatchMaxBioTheoBRPTarget,BMaxBioTheoBRPTarget/BMaxBioTheoBRPTarget,CatchMaxBioTheoBRPLimit/CatchMaxBioTheoBRPTarget,BMaxBioTheoBRPLimit/BMaxBioTheoBRPTarget,SimulationFishingYear,TRUE,TRUE,paste(plottitle," (use Catch=",round(CatchMaxBio,2)," as MSY proxy) (FMax)",sep=""),"Biomass (B/BTarget)","Catch (C/CTarget)",paste(format(CatchMaxBioTheoBRPTarget,scientific=T,digits=2)," (",TheoreticalBRP[11,2]*100,"%)",sep=""),paste(format(BMaxBioTheoBRPTarget,scientific=T,digits=2),"\n(",TheoreticalBRP[9,2]*100,"%)",sep=""),paste(format(CatchMaxBioTheoBRPLimit,scientific=T,digits=2)," (",TheoreticalBRP[12,2]*100,"%)",sep=""),paste(format(BMaxBioTheoBRPLimit,scientific=T,digits=2),"\n(",TheoreticalBRP[10,2]*100,"%)",sep=""),CatchMaxBio/CatchMaxBioTheoBRPTarget,BMaxBio/BMaxBioTheoBRPTarget,format(CatchMaxBio,scientific=T,digits=2),format(BMaxBio,scientific=T,digits=2)))
      savePlot(filename=paste("Phase plot (biomass and catch-FMax) for",plottitle),type="png")
      
      print(PhasePlotFunction(FBioBRPtemp/F01BioTheoBRPTarget,BBioBRPtemp/B01BioTheoBRPTarget,F01BioTheoBRPTarget/F01BioTheoBRPTarget,B01BioTheoBRPTarget/B01BioTheoBRPTarget,F01BioTheoBRPLimit/F01BioTheoBRPTarget,B01BioTheoBRPLimit/B01BioTheoBRPTarget,SimulationFishingYear,TRUE,TRUE,paste(plottitle," (use F0.1=",F01Bio," as FMSY proxy)",sep=""),"Biomass (B/BTarget)","Exploitation rate (F/FTarget)",paste(round(F01BioTheoBRPTarget,4)," (",TheoreticalBRP[7,2]*100,"%)",sep=""),paste(format(B01BioTheoBRPTarget,scientific=T,digits=2),"\n(",TheoreticalBRP[9,2]*100,"%)",sep=""),paste(round(F01BioTheoBRPLimit,4)," (",TheoreticalBRP[8,2]*100,"%)",sep=""),paste(format(B01BioTheoBRPLimit,scientific=T,digits=2),"\n(",TheoreticalBRP[10,2]*100,"%)",sep=""),F01Bio/F01BioTheoBRPTarget,B01Bio/B01BioTheoBRPTarget,round(F01Bio,4),format(B01Bio,scientific=T,digits=2)))
      savePlot(filename=paste("Phase plot (biomass and F01) for",plottitle),type="png")
      
      print(PhasePlotFunction(CatchBioBRPtemp/Catch01BioTheoBRPTarget,BBioBRPtemp/B01BioTheoBRPTarget,Catch01BioTheoBRPTarget/Catch01BioTheoBRPTarget,B01BioTheoBRPTarget/B01BioTheoBRPTarget,Catch01BioTheoBRPLimit/Catch01BioTheoBRPTarget,B01BioTheoBRPLimit/B01BioTheoBRPTarget,SimulationFishingYear,TRUE,TRUE,paste(plottitle," (use catch=",round(Catch01Bio,2)," as MSY proxy) (F0.1)",sep=""),"Biomass (B/BTarget)","Catch (C/CTarget)",paste(format(Catch01BioTheoBRPTarget,scientific=T,digits=2)," (",TheoreticalBRP[11,2]*100,"%)",sep=""),paste(format(B01BioTheoBRPTarget,scientific=T,digits=2),"\n(",TheoreticalBRP[9,2]*100,"%)",sep=""),paste(format(Catch01BioTheoBRPLimit,scientific=T,digits=2)," (",TheoreticalBRP[12,2]*100,"%)",sep=""),paste(format(B01BioTheoBRPLimit,scientific=T,digits=2),"\n(",TheoreticalBRP[10,2]*100,"%)",sep=""),Catch01Bio/Catch01BioTheoBRPTarget,B01Bio/B01BioTheoBRPTarget,format(Catch01Bio,scientific=T,digits=2),format(B01Bio,scientific=T,digits=2)))
      savePlot(filename=paste("Phase plot (biomass and catch-F01) for",plottitle),type="png")
      
      print(PhasePlotFunction(FBioBRPtemp/FPercentBioTheoBRPTarget,BBioBRPtemp/BPercentBioTheoBRPTarget,FPercentBioTheoBRPTarget/FPercentBioTheoBRPTarget,BPercentBioTheoBRPTarget/BPercentBioTheoBRPTarget,FPercentBioTheoBRPLimit/FPercentBioTheoBRPTarget,BPercentBioTheoBRPLimit/BPercentBioTheoBRPTarget,SimulationFishingYear,TRUE,TRUE,paste(plottitle," (use ",fpercentcode,"=",FPercentBio," as FMSY proxy)",sep=""),"Biomass (B/BTarget)","Exploitation rate (F/FTarget)",paste(round(FPercentBioTheoBRPTarget,4)," (",TheoreticalBRP[7,2]*100,"%)",sep=""),paste(format(BPercentBioTheoBRPTarget,scientific=T,digits=2),"\n(",TheoreticalBRP[9,2]*100,"%)",sep=""),paste(round(FPercentBioTheoBRPLimit,4)," (",TheoreticalBRP[8,2]*100,"%)",sep=""),paste(format(BPercentBioTheoBRPLimit,scientific=T,digits=2),"\n(",TheoreticalBRP[10,2]*100,"%)",sep=""),FPercentBio/FPercentBioTheoBRPTarget,BPercentBio/BPercentBioTheoBRPTarget,round(FPercentBio,4),format(BPercentBio,scientific=T,digits=2)))
      savePlot(filename=paste("Phase plot (biomass and FPercent) for",plottitle),type="png")
      
      print(PhasePlotFunction(CatchBioBRPtemp/CatchPercentBioTheoBRPTarget,BBioBRPtemp/BPercentBioTheoBRPTarget,CatchPercentBioTheoBRPTarget/CatchPercentBioTheoBRPTarget,BPercentBioTheoBRPTarget/BPercentBioTheoBRPTarget,CatchPercentBioTheoBRPLimit/CatchPercentBioTheoBRPTarget,BPercentBioTheoBRPLimit/BPercentBioTheoBRPTarget,SimulationFishingYear,TRUE,TRUE,paste(plottitle," (use catch=",round(CatchPercentBio,2)," as MSY proxy) (",fpercentcode,")",sep=""),"Biomass (B/BTarget)","Catch (C/CTarget)",paste(format(CatchPercentBioTheoBRPTarget,scientific=T,digits=2)," (",TheoreticalBRP[11,2]*100,"%)",sep=""),paste(format(BPercentBioTheoBRPTarget,scientific=T,digits=2),"\n(",TheoreticalBRP[9,2]*100,"%)",sep=""),paste(format(CatchPercentBioTheoBRPLimit,scientific=T,digits=2)," (",TheoreticalBRP[12,2]*100,"%)",sep=""),paste(format(BPercentBioTheoBRPLimit,scientific=T,digits=2),"\n(",TheoreticalBRP[10,2]*100,"%)",sep=""),CatchPercentBio/CatchPercentBioTheoBRPTarget,BPercentBio/BPercentBioTheoBRPTarget,format(CatchPercentBio,scientific=T,digits=2),format(BPercentBio,scientific=T,digits=2)))
      savePlot(filename=paste("Phase plot (biomass and catch-FPercent) for",plottitle),type="png")		
      
      
    }
  }
  
  if(BRPSwitches[4,2]==0) HistoricalCondition=subset(HistoricalCondition,Unit=="Abundance")
  if(BRPSwitches[4,2]==1) HistoricalCondition=subset(HistoricalCondition,Unit=="Biomass")
  
  names(HistoricalCondition$Year)=c("FishingYear")
  write.xlsx(HistoricalCondition,file="BRPOutput.xlsx", sheetName="HistoricalCondition", row.names=FALSE)
  
  write.xlsx(BRP,file="BRPOutput.xlsx", sheetName="BRP", row.names=FALSE, append=TRUE)
}

##Projection

if(ProjectionSwitches[1,2]==1)
{
  #run the base case
  
  if(HistoricalSimulationYesNo==0)
  {
    ThisTimestep=1
    CumulativeIntitialRecruit=0
    Simulation(SimulationYear,ManagementSetting[3,2],ManagementSetting[4,2],NULL,SimulationSetting[10,2],SimulationSetting[11,2],SimulationSetting[12,2],SimulationSetting[13,2],SimulationSetting[14,2],SimulationSetting[15,2],0,0,0,0,0) 
  }
  
  if(NumBurnInYears>0)
  {
    RowStart=min(which(Summary1$CalendarYear==(ManagementSetting[1,2]+NumBurnInYears) & Summary1$Timestep==ManagementSetting[3,2]))
    Summary1=Summary1[RowStart:dim(Summary1)[1],]
    RowStart=min(which(Summary2$CalendarYear==(ManagementSetting[1,2]+NumBurnInYears) & Summary2$Timestep==ManagementSetting[3,2]))
    Summary2=Summary2[RowStart:dim(Summary2)[1],]
  }
  #############################
  TimestepSummary1data=aggregate(Summary1$Quantity,by=as.list(Summary1[,c("Name","Area","MPA","Sex","Year","Timestep","ThisTimestep")]),sum)
  TimestepSummary2data=aggregate(Summary2$Quantity,by=as.list(Summary2[,c("Name","Fishery","Area","Sex","Year","Timestep","ThisTimestep")]),sum)
  TimestepSize2data=aggregate(Summary2$Quantity,by=as.list(Summary2[,c("Name","Fishery","Area","Sex","Year","Timestep","ThisTimestep", "SizeClass")]),sum)
  ##############################
  SimulationFishingYear=c(min(Summary1$Year):max(Summary1$Year)) 
  if(ManagementSetting[3,2]!=ManagementSetting[8,2]) SimulationFishingYear=SimulationFishingYear[SimulationFishingYear>min(Summary1$Year)]
  if(ManagementSetting[8,2]!=1 && ManagementSetting[4,2]!=(ManagementSetting[8,2]-1)) SimulationFishingYear=SimulationFishingYear[SimulationFishingYear<max(Summary1$Year)]
  
  file.rename("LobstaRecord.txt","LobstaRecordProjection.txt")
  ThisTimestepProjection=ThisTimestep
  CumulativeIntitialRecruitProjection=CumulativeIntitialRecruit
  Summary1Base=Summary1
  Summary2Base=Summary2
  
  #read in ProjectionSetting in ProjectionControlGeneralSetting.xlsx
  ProjectionSetting=read.xlsx("ProjectionControlGeneralSetting.xlsx",sheetName="ProjectionSetting")
  SimulationYearProjection=c(1:ProjectionSetting[1,2])
  
  #run the projection simulation case after the base case	    
  AnnualProject=c()
  AnnualCatchProject=c()
  LegalAbuProjectionMeasures=c()
  CatchAbuProjectionMeasures=c()
  LegalBioProjectionMeasures=c()
  CatchBioProjectionMeasures=c()
  AbuBRPAll=c()
  BioBRPAll=c()
  CatchAbuBRPAll=c()
  CatchBioBRPAll=c()
  ProbExceedReferencePoints=c()
  PhasePlotAll=c()
  ChangeInManagementStrategy4NextYearAll=c()
  
  windows(width=PlotWidth, height=PlotHeight, record=T)
  
  for(pscen in 1:ProjectionSetting[2,2])
  {
    #read in each scenario
    
    AdaptiveManagementStrategy=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="AdaptiveManagementStrategy")
    YearToApplyAdaptiveManagement=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="YearToApplyAdaptiveManagement")		
    BRPSettingProject=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="BRPSettingProject")
    BRPSettingProject[c(7,8),2]=BRPSettingProject[c(7,8),2]
    ProjectionRecruitType=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="RecruitSettingProject")[1,2]
    RecruitProject=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="RecruitProject")
    RecruitProject$Recruitment=RecruitProject$Recruitment/SimulationSetting[8,2]		
    RecruitParaProject=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="RecruitParaProject")		
    SSBParaProject=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="SSBParaProject")
    MovementProject=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="MovementProject")
    
    MPAOutInProject=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="MPAOutInProject") 
    MPAInOutProject=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="MPAInOutProject") 
    MPAMaxKProject=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="MPAMaxKProject")		
    MPAMaxKProject[,-1]=MPAMaxKProject[,-1]/SimulationSetting[8,2]
    
    NaturalMProject=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="NaturalMProject")
    EncounterRateProject=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="EncounterRateProject")
    MinMaxLegalSizeProject=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="MinMaxLegalSizeProject")
    TACProject=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="TACProject")
    TACProject[,-c(1,2)]=TACProject[,-c(1,2)]/SimulationSetting[8,2]
    VNotchProject=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="VNotchProject")
    HandlingMProject=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="HandlingMProject")
    FisheryProbProject=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="FisheryProbProject")
    
    ScalingTemporalEnRateProject=read.xlsx(paste("ProjectionControlScenario",pscen,".xlsx",sep=""),sheetName="ScalingTemporalEnRateProject")
    
    #summerize data from the base case base on the projection setting
    #subset by referenced size classes
    
    Summary1AbuBioBase=subset(Summary1Base,SizeClass>=BRPSettingProject[1,2] & SizeClass<=BRPSettingProject[2,2] & (Name==SummaryList[1] | Name==SummaryList[2]))
    
    AnnualSummary1AbuBioBase=aggregate(Summary1AbuBioBase$Quantity,by=as.list(Summary1AbuBioBase[,c("Name","Year","Timestep")]),sum) #sum by size class for each year and area
    
    if(ManagementSetting[8,2]==1) AnnualSummary1AbuBioBase=subset(AnnualSummary1AbuBioBase,Timestep==ManagementSetting[8,2])[,c("Name","Year","x")]
    else AnnualSummary1AbuBioBase=subset(AnnualSummary1AbuBioBase,Timestep==(ManagementSetting[8,2]-1))[,c("Name","Year","x")]
    
    if(ManagementSetting[3,2]!=ManagementSetting[8,2]) AnnualSummary1AbuBioBase=subset(AnnualSummary1AbuBioBase,Year>min(Summary1Base$Year))
    if(ManagementSetting[8,2]!=1 && ManagementSetting[4,2]!=(ManagementSetting[8,2]-1)) AnnualSummary1AbuBioBase=subset(AnnualSummary1AbuBioBase,Year<max(Summary1Base$Year))		
    
    AnnualSummary2Base=aggregate(Summary2Base$Quantity,by=as.list(Summary2Base[,c("Name","Year")]),sum)
    #subset by each year and area
    if(ManagementSetting[3,2]!=ManagementSetting[8,2]) AnnualSummary2Base=subset(AnnualSummary2Base,Year>min(Summary1Base$Year))
    if(ManagementSetting[8,2]!=1 && ManagementSetting[4,2]!=(ManagementSetting[8,2]-1)) AnnualSummary2Base=subset(AnnualSummary2Base,Year<max(Summary1Base$Year))
    
    AbuBRPBase=subset(AnnualSummary1AbuBioBase,Name==SummaryList[1])
    BioBRPBase=subset(AnnualSummary1AbuBioBase,Name==SummaryList[2])
    CatchAbuBRPBase=subset(AnnualSummary2Base,Name==SummaryList[17])
    CatchBioBRPBase=subset(AnnualSummary2Base,Name==SummaryList[18])		
    
    for(piter in 1:ProjectionSetting[3,2])
    {		
      file.copy("LobstaRecordProjection.txt","LobstaRecord.txt",overwrite = TRUE)
      ThisTimestep=ThisTimestepProjection
      CumulativeIntitialRecruit=CumulativeIntitialRecruitProjection
      if(ManagementSetting[8,2]==1) Simulation(SimulationYearProjection,ManagementSetting[8,2],4,NULL,SimulationSetting[10,2],SimulationSetting[11,2],SimulationSetting[12,2],SimulationSetting[13,2],SimulationSetting[14,2],SimulationSetting[15,2],0,0,0,ProjectionSwitches[1,2],ProjectionRecruitType) 
      else  Simulation(SimulationYearProjection,ManagementSetting[8,2],4,NULL,SimulationSetting[10,2],SimulationSetting[11,2],SimulationSetting[12,2],SimulationSetting[13,2],SimulationSetting[14,2],SimulationSetting[15,2],0,0,0,ProjectionSwitches[1,2],ProjectionRecruitType) 
      
      SimulationFishingYearProjection=c(min(Summary1$Year):max(Summary1$Year)) 
      
      #population summary of the projection period 
      Summary1Live=Summary1[Summary1$Name %in% SummaryList[c(1,2,3,4,7,8)],]
      Summary1Die=Summary1[!(Summary1$Name %in% SummaryList[c(1,2,3,4,7,8)]),]	
      AnnualSummary1Live=aggregate(Summary1Live$Quantity,by=as.list(Summary1Live[,c("Name","Area","Sex", "Year","Timestep")]),sum)
      if(ManagementSetting[8,2]==1) AnnualSummary1Live=subset(AnnualSummary1Live,Timestep==ManagementSetting[8,2])[,c("x","Name","Area","Sex", "Year")]
      else  AnnualSummary1Live=subset(AnnualSummary1Live,Timestep==(ManagementSetting[8,2]-1))[,c("x","Name","Area","Sex", "Year")]
      AnnualSummary1Die=aggregate(Summary1Die$Quantity,by=as.list(Summary1Die[,c("Name","Area","Sex", "Year")]),sum)
      AnnualSummary1=rbind(AnnualSummary1Live,AnnualSummary1Die)
      
      AnnualSummary2=aggregate(Summary2$Quantity,by=as.list(Summary2[,c("Name","Fishery","Sex","Year")]),sum)           
      AnnualSummary1$Year=as.factor(AnnualSummary1$Year)
      AnnualSummary2$Year=as.factor(AnnualSummary2$Year)	        
      
      AnnualProjecttemp=subset(AnnualSummary1,Name==SummaryList[7] | Name==SummaryList[8] | Name==SummaryList[3] | Name==SummaryList[4] | Name==SummaryList[9] | Name==SummaryList[10])	
      AnnualCatchProjecttemp=subset(AnnualSummary2,Name==SummaryList[17] | Name==SummaryList[18])
      
      AnnualProject=rbind(AnnualProject,data.frame(AnnualProjecttemp,Iteration=piter,Scenario=paste("Scenario",pscen)))
      AnnualCatchProject=rbind(AnnualCatchProject,data.frame(AnnualCatchProjecttemp,Iteration=piter,Scenario=paste("Scenario",pscen)))
      
      #summary of the four measures
      AnnualSummary1Area=aggregate(Summary1$Quantity,by=as.list(Summary1[,c("Name","Area","Year","Timestep")]),sum)
      if(ManagementSetting[8,2]==1) AnnualSummary1Area=subset(AnnualSummary1Area, Timestep==ManagementSetting[8,2])[,c("Name","Area","Year","x")]
      else  AnnualSummary1Area=subset(AnnualSummary1Area, Timestep==(ManagementSetting[8,2]-1))[,c("Name","Area","Year","x")]
      AnnualSummary2Fishery=aggregate(Summary2$Quantity,by=as.list(Summary2[,c("Name","Area","Fishery","Year")]),sum)
      
      AveAnnualCatchAbu=subset(aggregate(AnnualSummary2Fishery$x,by=as.list(AnnualSummary2Fishery[,c("Name","Area","Fishery")]),mean),Name==SummaryList[17])
      AveAnnualCatchAbu$Measure="AveAnnualCatch"
      AveAnnualCatchBio=subset(aggregate(AnnualSummary2Fishery$x,by=as.list(AnnualSummary2Fishery[,c("Name","Area","Fishery")]),mean),Name==SummaryList[18])	
      AveAnnualCatchBio$Measure="AveAnnualCatch"
      AnnualVarCatchAbu=subset(aggregate(AnnualSummary2Fishery$x,by=as.list(AnnualSummary2Fishery[,c("Name","Area","Fishery")]),var),Name==SummaryList[17])
      AnnualVarCatchAbu$Measure="AnnualVarCatch"
      AnnualVarCatchBio=subset(aggregate(AnnualSummary2Fishery$x,by=as.list(AnnualSummary2Fishery[,c("Name","Area","Fishery")]),var),Name==SummaryList[18])
      AnnualVarCatchBio$Measure="AnnualVarCatch"
      CatchAbuProjectionMeasurestemp=rbind(AveAnnualCatchAbu,AnnualVarCatchAbu)
      CatchBioProjectionMeasurestemp=rbind(AveAnnualCatchBio,AnnualVarCatchBio)
      
      MinAnnualLegalAbu=subset(aggregate(AnnualSummary1Area$x,by=as.list(AnnualSummary1Area[,c("Name","Area")]),min),Name==SummaryList[3])
      MinAnnualLegalAbu$Measure="MinAnnualLegal"
      MinAnnualLegalBio=subset(aggregate(AnnualSummary1Area$x,by=as.list(AnnualSummary1Area[,c("Name","Area")]),min),Name==SummaryList[4])
      MinAnnualLegalBio$Measure="MinAnnualLegal"
      EndAnnualLegalAbu=subset(aggregate(AnnualSummary1Area$x,by=as.list(AnnualSummary1Area[,c("Name","Area")]),tail,1),Name==SummaryList[3])
      EndAnnualLegalAbu$Measure="EndAnnualLegal"
      EndAnnualLegalBio=subset(aggregate(AnnualSummary1Area$x,by=as.list(AnnualSummary1Area[,c("Name","Area")]),tail,1),Name==SummaryList[4])
      EndAnnualLegalBio$Measure="EndAnnualLegal"
      LegalAbuProjectionMeasurestemp=rbind(MinAnnualLegalAbu,EndAnnualLegalAbu)
      LegalBioProjectionMeasurestemp=rbind(MinAnnualLegalBio,EndAnnualLegalBio)
      
      CatchAbuProjectionMeasures=rbind(CatchAbuProjectionMeasures,data.frame(CatchAbuProjectionMeasurestemp,Iteration=piter,Scenario=paste("Scenario",pscen),Unit="Abundance"))
      CatchBioProjectionMeasures=rbind(CatchBioProjectionMeasures,data.frame(CatchBioProjectionMeasurestemp,Iteration=piter,Scenario=paste("Scenario",pscen),Unit="Biomass"))			
      LegalAbuProjectionMeasures=rbind(LegalAbuProjectionMeasures,data.frame(LegalAbuProjectionMeasurestemp,Iteration=piter,Scenario=paste("Scenario",pscen),Unit="Abundance"))
      LegalBioProjectionMeasures=rbind(LegalBioProjectionMeasures,data.frame(LegalBioProjectionMeasurestemp,Iteration=piter,Scenario=paste("Scenario",pscen),Unit="Biomass"))			
      
      #phase plots
      
      #subset by referenced size classes
      Summary1AbuBioAll=subset(Summary1, SizeClass>=BRPSettingProject[1,2] & SizeClass<=BRPSettingProject[2,2])
      AnnualSummary1AbuBioAll=aggregate(Summary1AbuBioAll$Quantity,by=as.list(Summary1AbuBioAll[,c("Name","Year","Timestep")]),sum) #sum by size class and timestep for each year and area
      AnnualSummary2All=aggregate(Summary2$Quantity,by=as.list(Summary2[,c("Name","Year")]),sum)
      
      #subset by each year and area
      if(ManagementSetting[8,2]==1) 
      {
        AbuBRPiter=subset(AnnualSummary1AbuBioAll,Name==SummaryList[1] & Timestep==ManagementSetting[8,2])$x 
        BioBRPiter=subset(AnnualSummary1AbuBioAll,Name==SummaryList[2] & Timestep==ManagementSetting[8,2])$x
      }
      else
      {
        AbuBRPiter=subset(AnnualSummary1AbuBioAll,Name==SummaryList[1] & Timestep==(ManagementSetting[8,2]-1))$x 
        BioBRPiter=subset(AnnualSummary1AbuBioAll,Name==SummaryList[2] & Timestep==(ManagementSetting[8,2]-1))$x			
      }
      CatchAbuBRPiter=subset(AnnualSummary2All,Name==SummaryList[17])$x
      CatchBioBRPiter=subset(AnnualSummary2All,Name==SummaryList[18])$x
      AbuBRPAll=rbind(AbuBRPAll,data.frame(x=AbuBRPiter,Year=SimulationFishingYearProjection,Iteration=piter))
      BioBRPAll=rbind(BioBRPAll,data.frame(x=BioBRPiter,Year=SimulationFishingYearProjection,Iteration=piter))
      CatchAbuBRPAll=rbind(CatchAbuBRPAll,data.frame(x=CatchAbuBRPiter,Year=SimulationFishingYearProjection,Iteration=piter))
      CatchBioBRPAll=rbind(CatchBioBRPAll,data.frame(x=CatchBioBRPiter,Year=SimulationFishingYearProjection,Iteration=piter))		
      
      ChangeInManagementStrategy4NextYearAll=rbind(ChangeInManagementStrategy4NextYearAll,data.frame(ChangeInManagementStrategy4NextYear,Scenario=pscen,Iteration=piter))
    }
    
    #calcualte F and B for each year for whole area
    
    AbuBRPMedian=aggregate(AbuBRPAll$x,by=list(AbuBRPAll[,"Year"]),median)
    BioBRPMedian=aggregate(BioBRPAll$x,by=list(BioBRPAll[,"Year"]),median)
    CatchAbuBRPMedian=aggregate(CatchAbuBRPAll$x,by=list(CatchAbuBRPAll[,"Year"]),median)
    CatchBioBRPMedian=aggregate(CatchBioBRPAll$x,by=list(CatchBioBRPAll[,"Year"]),median)
    
    AbuBRPMin=aggregate(AbuBRPAll$x,by=list(AbuBRPAll[,"Year"]),min)
    BioBRPMin=aggregate(BioBRPAll$x,by=list(BioBRPAll[,"Year"]),min)
    CatchAbuBRPMin=aggregate(CatchAbuBRPAll$x,by=list(CatchAbuBRPAll[,"Year"]),min)
    CatchBioBRPMin=aggregate(CatchBioBRPAll$x,by=list(CatchBioBRPAll[,"Year"]),min)
    
    AbuBRPMax=aggregate(AbuBRPAll$x,by=list(AbuBRPAll[,"Year"]),max)
    BioBRPMax=aggregate(BioBRPAll$x,by=list(BioBRPAll[,"Year"]),max)
    CatchAbuBRPMax=aggregate(CatchAbuBRPAll$x,by=list(CatchAbuBRPAll[,"Year"]),max)
    CatchBioBRPMax=aggregate(CatchBioBRPAll$x,by=list(CatchBioBRPAll[,"Year"]),max)
    
    FMSYBRPTargetMedian=BRPSettingProject[5,2]
    BBRPTargetMedian=BRPSettingProject[7,2]
    FMSYBRPLimitMedian=BRPSettingProject[6,2]
    BBRPLimitMedian=BRPSettingProject[8,2]
    
    if(BRPSettingProject[3,2]==0) #Abundance-Based
    {
      
      #calculate probability of exceeding target and limit reference points in any give years during the simulation
      
      if(BRPSettingProject[4,2]==1) #F-Based
      {
        FMSYBRPAll=CatchAbuBRPAll$x/(AbuBRPAll$x+CatchAbuBRPAll$x)
        FMSYBRPMedian=c(CatchAbuBRPBase$x/(AbuBRPBase$x+CatchAbuBRPBase$x),CatchAbuBRPMedian$x/(AbuBRPMedian$x+CatchAbuBRPMedian$x)) #exploitation rate for each year and area	
        FMSYBRPMin=c(CatchAbuBRPBase$x/(AbuBRPBase$x+CatchAbuBRPBase$x),CatchAbuBRPMin$x/(AbuBRPMin$x+CatchAbuBRPMin$x)) #exploitation rate for each year and area	
        FMSYBRPMax=c(CatchAbuBRPBase$x/(AbuBRPBase$x+CatchAbuBRPBase$x),CatchAbuBRPMax$x/(AbuBRPMax$x+CatchAbuBRPMax$x)) #exploitation rate for each year and area
      }
      else if(BRPSettingProject[4,2]==0) #C-Based
      {
        FMSYBRPAll=CatchAbuBRPAll$x
        FMSYBRPMedian=c(CatchAbuBRPBase$x,CatchAbuBRPMedian$x) #exploitation rate for each year and area	
        FMSYBRPMin=c(CatchAbuBRPBase$x,CatchAbuBRPMin$x) #exploitation rate for each year and area	
        FMSYBRPMax=c(CatchAbuBRPBase$x,CatchAbuBRPMax$x) #exploitation rate for each year and area
      }
      
      BBRPAll=AbuBRPAll$x
      
      BBRPMedian=c(AbuBRPBase$x,AbuBRPMedian$x)
      BBRPMin=c(AbuBRPBase$x,AbuBRPMin$x)
      BBRPMax=c(AbuBRPBase$x,AbuBRPMax$x)			
      MSYBRPMedian=c(CatchAbuBRPBase$x,CatchAbuBRPMedian$x)
      MSYBRPMin=c(CatchAbuBRPBase$x,CatchAbuBRPMin$x)
      MSYBRPMax=c(CatchAbuBRPBase$x,CatchAbuBRPMax$x)	
      
    }
    if(BRPSettingProject[3,2]==1) #Biomass-Based
    {
      if(BRPSettingProject[4,2]==1) #F-Based
      {
        FMSYBRPAll=CatchBioBRPAll/(BioBRPAll+CatchBioBRPAll)
        FMSYBRPMedian=c(CatchBioBRPBase$x/(BioBRPBase$x+CatchBioBRPBase$x),CatchBioBRPMedian$x/(BioBRPMedian$x+CatchBioBRPMedian$x)) #exploitation rate for each year and area	
        FMSYBRPMin=c(CatchBioBRPBase$x/(BioBRPBase$x+CatchBioBRPBase$x),CatchBioBRPMin$x/(BioBRPMin$x+CatchBioBRPMin$x)) #exploitation rate for each year and area	
        FMSYBRPMax=c(CatchBioBRPBase$x/(BioBRPBase$x+CatchBioBRPBase$x),CatchBioBRPMax$x/(BioBRPMax$x+CatchBioBRPMax$x)) #exploitation rate for each year and area
      }
      else if(BRPSettingProject[4,2]==0) #C-Based
      {
        FMSYBRPAll=CatchBioBRPAll
        FMSYBRPMedian=c(CatchBioBRPBase$x,CatchBioBRPMedian$x) #exploitation rate for each year and area	
        FMSYBRPMin=c(CatchBioBRPBase$x,CatchBioBRPMin$x) #exploitation rate for each year and area	
        FMSYBRPMax=c(CatchBioBRPBase$x,CatchBioBRPMax$x) #exploitation rate for each year and area
      }
      
      BBRPAll=BioBRPAll
      
      BBRPMedian=c(BioBRPBase$x,BioBRPMedian$x)
      BBRPMin=c(BioBRPBase$x,BioBRPMin$x)
      BBRPMax=c(BioBRPBase$x,BioBRPMax$x)			
      MSYBRPMedian=c(CatchBioBRPBase$x,CatchBioBRPMedian$x)
      MSYBRPMin=c(CatchBioBRPBase$x,CatchBioBRPMin$x)
      MSYBRPMax=c(CatchBioBRPBase$x,CatchBioBRPMax$x)		
    }
    
    ProbExceedFMSYBRPTarget=ifelse(FMSYBRPAll>FMSYBRPTargetMedian,1,0)
    ProbExceedFMSYBRPLimit=ifelse(FMSYBRPAll>FMSYBRPLimitMedian,1,0)
    ProbExceedBBRPTarget=ifelse(BBRPAll<BBRPTargetMedian,1,0)	
    ProbExceedBBRPLimit=ifelse(BBRPAll<BBRPLimitMedian,1,0)
    
    ProbExceedFMSYBRPTarget=sum(ProbExceedFMSYBRPTarget)/length(ProbExceedFMSYBRPTarget)
    ProbExceedFMSYBRPLimit=sum(ProbExceedFMSYBRPLimit)/length(ProbExceedFMSYBRPLimit)
    ProbExceedBBRPTarget=sum(ProbExceedBBRPTarget)/length(ProbExceedBBRPTarget)
    ProbExceedBBRPLimit=sum(ProbExceedBBRPLimit)/length(ProbExceedBBRPLimit)	
    
    ProbExceedReferencePoints=rbind(ProbExceedReferencePoints,data.frame(ProbExceedFMSYBRPTarget=ProbExceedFMSYBRPTarget,ProbExceedFMSYBRPLimit=ProbExceedFMSYBRPLimit,ProbExceedBBRPTarget=ProbExceedBBRPTarget,ProbExceedBBRPLimit=ProbExceedBBRPLimit,ProjectionScenario=pscen))
    
    PhasePlotAll=rbind(PhasePlotAll,data.frame(Scenario=pscen,Abundance=AbuBRPAll$x,Biomass=BioBRPAll$x,CatchAbu=CatchAbuBRPAll$x,CatchBio=CatchBioBRPAll$x,AbuBRPAll[,c("Year","Iteration")]))
    
    AbuBRPAll=c()
    BioBRPAll=c()
    CatchAbuBRPAll=c()
    CatchBioBRPAll=c()
    
    MedianSimYearProject=c(SimulationFishingYear,SimulationFishingYearProjection)
    
    print(PhasePlotFunction(FMSYBRPMedian/FMSYBRPTargetMedian,BBRPMedian/BBRPTargetMedian,FMSYBRPTargetMedian/FMSYBRPTargetMedian,BBRPTargetMedian/BBRPTargetMedian,FMSYBRPLimitMedian/FMSYBRPTargetMedian,BBRPLimitMedian/BBRPTargetMedian,MedianSimYearProject,TRUE,TRUE,paste("Whole area for scenario",pscen),ifelse(BRPSettingProject[3,2]==0,"Abundance (A/ATarget)","Biomass (B/BTarget)"),ifelse(BRPSettingProject[4,2]==0,"Cacth (C/CTarget)","Exploitation rate (F/FTarget)"),round(FMSYBRPTargetMedian,4),format(BBRPTargetMedian,scientific=T,digits=2),round(FMSYBRPLimitMedian,4),format(BBRPLimitMedian,scientific=T,digits=2),NULL,NULL,NULL,NULL))
   # savePlot(filename=paste("Phase plot projection for whole area for scenario",pscen),type="png")
    
    #geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1)
  }
  
  CatchProjectionMeasures=rbind(CatchAbuProjectionMeasures,CatchBioProjectionMeasures)
  LegalProjectionMeasures=rbind(LegalAbuProjectionMeasures,LegalBioProjectionMeasures)
  
  #projection plots
  # [1] "Abundance (number)"                                        
  # [2] "Biomass (weight)"                                          
  # [3] "Legal Abundance (number)"                                  
  # [4] "Legal Biomass (weight)"                                    
  # [5] "Dead Legal Abundance (number)"                             
  # [6] "Dead Legal Biomass (weight)"                               
  # [7] "Spawning Stock Abundance (number)"                         
  # [8] "Spawning Stock Biomass (weight)"                           
  # [9] "Recruitment (number)"                                      
  # [10] "Recruitment (weight)"                                      
  # [11] "Dead Lobster due to Natural M (number)"                    
  # [12] "Dead Lobster due to Natural M (weight)"                    
  # [13] "Dead Lobster due to Molt M (number)"                       
  # [14] "Dead Lobster due to Molt M (weight)"                       
  # [15] "Lobster caught with V-Notched (number)"                    
  # [16] "Lobster caught with V-Notched (weight)"                    
  # [17] "Landed Catch (number)"                                     
  # [18] "Landed Catch (weight)"                                     
  # [19] "Dead Lobster due to Handling M (number)"                   
  # [20] "Dead Lobster due to Handling M (weight)"                   
  # [21] "Exploitation rate (Landed Catch in number/Legal Abundance)"
  # [22] "Exploitation rate (Landed Catch in weight/Legal Biomass)"
  # add to the list if you want more then the selected ones
  
  
  Summary1LiveBase=Summary1Base[Summary1Base$Name %in% SummaryList[c(1,2,3,4,7,8)],]
  Summary1DieBase=Summary1Base[!(Summary1Base$Name %in% SummaryList[c(1,2,3,4,7,8)]),]	
  
  AnnualSummary1LiveBase=aggregate(Summary1LiveBase$Quantity,by=as.list(Summary1LiveBase[,c("Name","Area","Sex", "Year","Timestep")]),sum)
  if(ManagementSetting[8,2]==1) AnnualSummary1LiveBase=subset(AnnualSummary1LiveBase,Timestep==ManagementSetting[8,2])[,c("x","Name","Area","Sex", "Year")]
  else  AnnualSummary1LiveBase=subset(AnnualSummary1LiveBase,Timestep==(ManagementSetting[8,2]-1))[,c("x","Name","Area","Sex", "Year")]
  AnnualSummary1DieBase=aggregate(Summary1DieBase$Quantity,by=as.list(Summary1DieBase[,c("Name","Area","Sex", "Year")]),sum)
  AnnualAreaSummary1Base=rbind(AnnualSummary1LiveBase,AnnualSummary1DieBase)	
  
  AnnualSummary1Base=aggregate(AnnualAreaSummary1Base$x,by=as.list(AnnualAreaSummary1Base[,c("Name","Sex", "Year")]),sum)
  
  AnnualFisherySummary2Base=aggregate(Summary2Base$Quantity,by=as.list(Summary2Base[,c("Name","Fishery","Sex", "Year")]),sum)	
  AnnualSummary2Base=aggregate(Summary2Base$Quantity,by=as.list(Summary2Base[,c("Name","Sex","Year")]),sum)
  
  AnnualSummary1BaseAll=c()
  AnnualSummary2BaseAll=c()
  AnnualAreaSummary1BaseAll=c()
  AnnualFisherySummary2BaseAll=c()
  
  for(pscen in 1:ProjectionSetting[2,2])
  {
    for(piter in 1:ProjectionSetting[3,2])
    {
      AnnualSummary1Base$Scenario=paste("Scenario",pscen)
      AnnualSummary2Base$Scenario=paste("Scenario",pscen)
      AnnualAreaSummary1Base$Scenario=paste("Scenario",pscen)
      AnnualFisherySummary2Base$Scenario=paste("Scenario",pscen)
      
      AnnualSummary1Base$Iteration=piter
      AnnualSummary2Base$Iteration=piter
      AnnualAreaSummary1Base$Iteration=piter
      AnnualFisherySummary2Base$Iteration=piter
      
      AnnualSummary1BaseAll=rbind(AnnualSummary1BaseAll,AnnualSummary1Base)
      AnnualSummary2BaseAll=rbind(AnnualSummary2BaseAll,AnnualSummary2Base)
      AnnualAreaSummary1BaseAll=rbind(AnnualAreaSummary1BaseAll,AnnualAreaSummary1Base)
      AnnualFisherySummary2BaseAll=rbind(AnnualFisherySummary2BaseAll,AnnualFisherySummary2Base)
    }
  }
  
  AnnualSummary1BaseAll$PlotYear=paste("H",AnnualSummary1BaseAll$Year,sep="")
  AnnualSummary2BaseAll$PlotYear=paste("H",AnnualSummary2BaseAll$Year,sep="")
  AnnualAreaSummary1BaseAll$PlotYear=paste("H",AnnualAreaSummary1BaseAll$Year,sep="")
  AnnualFisherySummary2BaseAll$PlotYear=paste("H",AnnualFisherySummary2BaseAll$Year,sep="")
  
  AnnualProject$PlotYear=as.numeric(levels(AnnualProject$Year))[as.integer(AnnualProject$Year)]
  AnnualProject$PlotYear=ifelse(AnnualProject$PlotYear>=10,paste("P",AnnualProject$Year,sep=""),paste("P0",AnnualProject$Year,sep=""))
  
  AnnualCatchProject$PlotYear=as.numeric(levels(AnnualCatchProject$Year))[as.integer(AnnualCatchProject$Year)]
  AnnualCatchProject$PlotYear=ifelse(AnnualCatchProject$PlotYear>=10,paste("P",AnnualCatchProject$PlotYear,sep=""),paste("P0",AnnualCatchProject$PlotYear,sep=""))
  
  for(i in c(3,4,7,8,9,10,17,18))
  {   
    if(i<17)
    {
      temp=subset(AnnualProject,Name==SummaryList[[i]])
      temp=aggregate(temp$x,by=as.list(temp[,c("Sex","Iteration","Scenario","PlotYear")]),sum)
      tempbase=subset(AnnualSummary1BaseAll,Name==SummaryList[[i]])[,c("Sex","Iteration","Scenario","PlotYear","x")]
      temp=rbind(temp,tempbase)
      print(ggplot(temp,aes(PlotYear,x))+geom_boxplot()+facet_grid(Scenario~Sex)+ labs(y=NULL)+labs(title=paste(SummaryList[[i]],"for whole area"))+theme_bw())
      #savePlot(filename=paste("Projected",SummaryList[[i]],"for whole area"),type="png")
      
      if(NumArea>1)
      {	
        for(a in 1:NumArea)
        {
          temp=subset(AnnualProject,Name==SummaryList[[i]] & Area==paste("Area",a))[,c("Sex","Iteration","Scenario","PlotYear","x","Area")]
          tempbase=subset(AnnualAreaSummary1BaseAll,Name==SummaryList[[i]] & Area==paste("Area",a))[,c("Sex","Iteration","Scenario","PlotYear","x","Area")]
          temp=rbind(temp,tempbase)
          print(ggplot(temp,aes(PlotYear,x))+geom_boxplot()+facet_grid(Scenario~Sex)+ labs(y=NULL)+labs(title=paste(SummaryList[[i]],"for Area",a))+theme_bw())
          #savePlot(filename=paste("Projected",SummaryList[[i]],"for Area",a),type="png")
        }
      }
    }
    else
    {
      
      temp=subset(AnnualCatchProject,Name==SummaryList[[i]])
      temp=aggregate(temp$x,by=as.list(temp[,c("Sex","Iteration","Scenario","PlotYear")]),sum)
      tempbase=subset(AnnualSummary2BaseAll,Name==SummaryList[[i]])[,c("Sex","Iteration","Scenario","PlotYear","x")]
      temp=rbind(temp,tempbase)			
      print(ggplot(temp,aes(PlotYear,x))+geom_boxplot()+facet_grid(Scenario~Sex)+ labs(y=NULL)+labs(title=paste(SummaryList[[i]],"for all fisheries"))+theme_bw())
     # savePlot(filename=paste("Projected",SummaryList[[i]],"for all fisheries"),type="png")
      
      
      if(ManagementSetting[7,2]>0)
      {
        for(f in 1:ManagementSetting[7,2])
        {
          temp=subset(AnnualCatchProject,Name==SummaryList[[i]] & Fishery==paste("Fishery",f))[,c("Sex","Iteration","Scenario","PlotYear","Fishery","x")]
          tempbase=subset(AnnualFisherySummary2BaseAll,Name==SummaryList[[i]] & Fishery==paste("Fishery",f))[,c("Sex","Iteration","Scenario","PlotYear","Fishery","x")]
          temp=rbind(temp,tempbase)					
          print(ggplot(temp,aes(PlotYear,x))+geom_boxplot()+facet_grid(Scenario~Sex)+ labs(y=NULL)+labs(title=paste(SummaryList[[i]],"for fishery",f))+theme_bw())    
          #savePlot(filename=paste("Projected",SummaryList[[i]],"for Fishery",f),type="png")
        }
      }
      
    }
  }
  
  if(NumArea==1) FromToArea=1
  else FromToArea=NumArea+1
  
  if(ManagementSetting[7,2]==1) FromToFishery=1
  else FromToFishery=ManagementSetting[7,2]+1
  
  for(jj in 1:FromToFishery)
  {	
    for(ii in 1:FromToArea)
    {
      if((FromToArea==1 | ii==NumArea+1) & (FromToFishery==1 | jj==ManagementSetting[7,2]+1)) #for whole fisheries and area
      {
        CatchMeasurePlotData=aggregate(CatchProjectionMeasures$x,by=as.list(CatchProjectionMeasures[,c("Measure","Scenario","Unit")]),sum)
        plottitle="whole area and fishery"
      }
      else if(FromToArea==1 & FromToFishery>1) #one area and more then one fishery
      {
        CatchMeasurePlotData=subset(CatchProjectionMeasures, Fishery==paste("Fishery", jj))			
        plottitle=paste("whole area and","Fishery",jj)
      }
      else if(FromToArea>1 & FromToFishery==1) #one fishery and more then one area	
      {
        CatchMeasurePlotData=subset(CatchProjectionMeasures, Area==paste("Area",ii))			
        plottitle=paste("whole fishery and","Area",ii)				
      }			
      else #more then one fishery and more then one area
      {
        CatchMeasurePlotData=subset(CatchProjectionMeasures, Fishery==paste("Fishery", jj) & Area==paste("Area",ii))
        plottitle=paste("Area",ii,"Fishery",jj)
      }
      
      if(ProjectionSwitches[2,2]==0) CatchMeasurePlotData=subset(CatchMeasurePlotData,Unit=="Abundance")
      if(ProjectionSwitches[2,2]==1) CatchMeasurePlotData=subset(CatchMeasurePlotData,Unit=="Biomass")
      
      for(kk in unique(CatchMeasurePlotData$Unit))
      {
        if(kk=="Abundance") plotlab="number"
        if(kk=="Biomass") plotlab="weight"	
        
        p1=ggplot(subset(CatchMeasurePlotData,Measure=="AveAnnualCatch"),aes(Scenario,x))+labs(y=NULL)+labs(title=paste("Averaged annual catch (",plotlab,") for ",plottitle,sep=""))+geom_boxplot()+theme_bw()
        p3=ggplot(subset(CatchMeasurePlotData,Measure=="AnnualVarCatch"),aes(Scenario,x))+labs(y=NULL)+labs(title=paste("Annual catch (",plotlab,") variation for ",plottitle,sep=""))+geom_boxplot()+theme_bw()
        multiplot(p1, p3,cols=2)
        #savePlot(filename=paste("Projection catch measures in ",plotlab," for ",plottitle,sep=""),type="png")
        
      }		
    }
  }
  
  for(ii in 1:FromToArea)
  {
    if(FromToArea==1 | ii==NumArea+1) 
    {
      LegalMeasurePlotData=aggregate(LegalProjectionMeasures$x,by=as.list(LegalProjectionMeasures[,c("Measure","Scenario","Unit")]),sum)
      plottitle="whole area"
    }
    else
    {
      LegalMeasurePlotData=subset(LegalProjectionMeasures, Area==paste("Area",ii))
      plottitle=paste("Area",ii)
    }
    
    if(ProjectionSwitches[2,2]==0) LegalMeasurePlotData=subset(LegalMeasurePlotData,Unit=="Abundance")
    if(ProjectionSwitches[2,2]==1) LegalMeasurePlotData=subset(LegalMeasurePlotData,Unit=="Biomass")
    
    for(kk in unique(LegalMeasurePlotData$Unit))
    {			
      if(kk=="Abundance") plotlab="abundance"
      if(kk=="Biomass") plotlab="biomass"	
      p2=ggplot(subset(LegalMeasurePlotData,Measure=="EndAnnualLegal"),aes(Scenario,x))+labs(y=NULL)+labs(title=paste("Legal ",plotlab," at the end of time series for ",plottitle,sep=""))+geom_boxplot()+theme_bw()
      p4=ggplot(subset(LegalMeasurePlotData,Measure=="MinAnnualLegal"),aes(Scenario,x))+labs(y=NULL)+labs(title=paste("Minimal legal ",plotlab," among time series for ",plottitle,sep=""))+geom_boxplot()+theme_bw()
      multiplot(p2, p4,cols=2)
     # savePlot(filename=paste("Projection legal measures in ",plotlab," for ", plottitle,sep=""),type="png")			
    }		
  }	
  
  
  names(AnnualProject)[which(names(AnnualProject)=="Year")]=c("FishingYear")
  write.xlsx(AnnualProject,file="ProjectionOutput.xlsx", sheetName="AnnualProjectInfo", row.names=FALSE)
  
  names(AnnualCatchProject)[which(names(AnnualCatchProject)=="Year")]=c("FishingYear")
  write.xlsx(AnnualCatchProject,file="ProjectionOutput.xlsx", sheetName="AnnualCatchProjectInfo", row.names=FALSE, append=TRUE)
  
  write.xlsx(CatchProjectionMeasures,file="ProjectionOutput.xlsx", sheetName="CatchProjectionMeasures", row.names=FALSE, append=TRUE)
  write.xlsx(LegalProjectionMeasures,file="ProjectionOutput.xlsx", sheetName="LegalProjectionMeasures", row.names=FALSE, append=TRUE)
  write.xlsx(ProbExceedReferencePoints,file="ProjectionOutput.xlsx", sheetName="ProbExceedReferencePoints", row.names=FALSE, append=TRUE)
  write.xlsx(ChangeInManagementStrategy4NextYearAll,file="ProjectionOutput.xlsx", sheetName="ChangeInManagementStrategy", row.names=FALSE, append=TRUE)
  write.xlsx(PhasePlotAll,file="ProjectionOutput.xlsx", sheetName="PhasePlotAll", row.names=FALSE, append=TRUE)
  
}




