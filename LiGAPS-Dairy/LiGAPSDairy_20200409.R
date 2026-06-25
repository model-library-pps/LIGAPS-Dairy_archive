  #######################################################################################
  #                                                                                     #
  # LiGAPS-Dairy                                                                        #                                                                                     
  #                                                                                     #
  # (Livestock simulator for Generic analysis of Animal Production Systems -            #
  # Dairy cattle)                                                                       #
  #                                                                                     #
  #                                                                                     #
  #               #    #  ##   ##  ###   ###    ###   ##   #  ####  #  #                #
  #               #      #    #  # #  # #       #  # #  #     #  #  #  #                #
  #               #    # # ## #### ###   ##  ## #  # ####  #  ###   ####                #
  #               #    # #  # #  # #       #    #  # #  #  #  # #      #                #
  #               #### #  ##  #  # #    ###     ###  #  #  #  #  #   ##                 #
  #                                                                                     #
  #                                                                                     #
  #                                                                                     #
  #                                                                                     # 
  #                                                                                     #
  # The model LiGAPS-Dairy is described in the paper:                                   #
  # Yield gap analysis in dairy production systems using the mechanistic model          #
  # LiGAPS-Dairy                                                                        # 
  #                                                                                     #
  # Authors: A. van der Linden 1,2,*, S.J. Oosting 1, G.W.J. van de Ven 2,              #
  # M.K. van Ittersum 2, P.J. Gerber 1,3, and I.J.M. de Boer 1.                         #
  #                                                                                     #
  #                                                                                     #
  # 1                                                                                   #
  # Animal Production Systems group                                                     # 
  # Wageningen University                                                               #
  # P.O. Box 338                                                                        #
  # De Elst 1                                                                           #
  # 6700 AH  Wageningen                                                                 #
  # The Netherlands                                                                     #
  #                                                                                     #
  # 2                                                                                   #
  # Plant Production Systems group                                                      #
  # Wageningen University                                                               #
  # P.O. Box 430                                                                        #
  # Droevendaalsesteeg 1                                                                #
  # 6700 AK  Wageningen                                                                 #
  # The Netherlands                                                                     #
  #                                                                                     #
  # 3                                                                                   #
  # The World Bank                                                                      #
  # Radnicka cesta 80/IX                                                                #
  # HR-10000 Zagreb                                                                     #
  # Croatia                                                                             #
  #                                                                                     #
  # * Contact: aart.vanderlinden@wur.nl (Aart van der Linden)                           #                                                   
  #                                                                                     #
  # LiGAPS-Dairy aims to simulate potential and feed-limited milk production for dairy  #
  # cows in different farming systems across the world. Potential and feed-limited      #
  # production are used to calculate the yield gap, which is defined as the difference  #
  # between potential or feed-limited production and actual production. The model       #
  # identifies the biophysical factors that define (genotype, climate) and limit milk   #
  # production (feed quality and feed quantity available).                              #
  #                                                                                     #
  #                                                                                     #
  # Description program code:                                                           #
  # This program code of the model LiGAPS-Dairy includes a thermoregulation sub-model,  #
  # a feed intake and digestion sub-model, and an energy and protein utilization        #
  # sub-model.The code is based on the program code of the model LiGAPS-Beef, which is  #
  # described in van der Linden et al. (2019, LiGAPS-Beef, a mechanistic model to       #
  # explore potential and feed-limited beef production 1: model description and         #
  # illustration, Animal 13 : 845-855, https://www.ncbi.nlm.nih.gov/pubmed/29996958)    #
  # LiGAPS-Beef was adapted to dairy cattle by extensions and adaptations in the code   #
  # that relate to feed intake and milk production.                                     #
  #                                                                                     #
  # This specific version of LiGAPS-Dairy is used to evaluate the model for three       #
  # experiments with dairy cows in Lelystad, the Netherlands.                           #
  #                                                                                     #
  # Last update: 10-March-2020                                                          #
  #                                                                                     #
  #######################################################################################
  
  #######################################################################################
  #                                   Model settings                                    #
  ####################################################################################### 
  
  # This section contains the major settings for the model
  
  ## Genotype (i.e. breed, defining factor)
  BREED <- 1 # 1 = Holstein Friesian, 2 = Boran, 3 = crossbred HF x Boran, 
             # 4 = Boran (small, used by pastoralists)
  
  ## Climate (used to read weather data file)
  LOCATION <- 'NETHERLANDS'   # Options: ETHIOPIA, FRANCE, NETHERLANDS, NEWZEALAND,
                              # ZAMBIA     
  # Housing of cattle (see )
  STABLE <- 0 # 0 = outdoors grazing; 1 = kept in stables
  
  ## Feed quality and feed quantity
  POTENTIAL <- 0   # Potential diet 1 = yes, 0 = no
                   # The potential diet consists of 19.5% soybean meal, 31.8% wheat,
                   # and 48.6% good quality hay, and it is fed ad libitum.
  FEEDQUALITY <- 1 # Ad libitum diet, i.e. without feed quantity limitation 1 = yes, 
                   # 0 = no. Feed quality limitation is simulated by selecting 1, if
                   # 0 is selected for POTENTIAL and FEEDQUALITY, the model simulates 
                   # both feed quality limitation and feed quantity limitation.
  
  #######################################################################################
  #                         Code specific for model illustration                        #
  #######################################################################################
  
  EXPERIMENTNR <- c(8,22,63) # Experiment number from WLR database, 8 = experiment con- 
                             # conducted at 't Gen (in dataset Zom et al., 2012b), 22 = 
                             # experiment described in Meijer et al. (1998), 63 = 
                             # experiment described in van Duinkerken et al. (2005)
  CALDATA <- c(rep(0,16))    # Vector listing performance indicators of the model
  CALDATAAB <- NULL          # Vector listing performance indicators for the experiment
                             # conducted at 't Gen.
  CALDATAAC <- NULL          # Vector listing performance indicators for Meijer et al. 
                             # (1998)
  CALDATABC <- NULL          # Vector listing performance indicators for van Duinkerken
                             # et al. (2005)
  Printcows <- 0             # Prints the percentage of cows simulated (out of the 220)
                             # cows in the dataset
  DATALACT <- NULL           # Vector to list the milk production per lactation 
  
  #######################################################################################
  
  relDEFLIMFACTORSexpsall <- c(rep(0,6)) # Initial vector for listing the bioph. factors   
  TempHEATSTRESSexpsall <- NULL          # Initial vector for maximum temperature on days 
                                         # in which heat stress occurs
  TempCOLDSTRESSexpsall <- NULL          # Initial vector for minimum temperature on days 
                                         # in which cold stress occurs
  ProtDefCPexpsall <-NULL                # Initial vector listing the protein content of
                                         # the diet on days in which protein deficiency
                                         # occurs
  ProtDefmilkexpsall <-NULL              # Initial vector listing the genetic potential
                                         # fat- and protein-corrected milk (FPCM)
                                         # production
  
  GENfactall <- c(rep(0,1500))           # Initial vector for identification of the 
                                         # genotype as a defining factor
  HEATSTRESSfactall <- c(rep(0,1500))    # Initial vector for identification of heat 
                                         # stress as a defining factor 
  COLDSTRESSfactall <- c(rep(0,1500))    # Initial vector for identification of cold
                                         # stress as a defining factor 
  FILLGITfactall <- c(rep(0,1500))       # Initial vector for identification of digestion
                                         # capacity as a limiting factor
  NEDEFfactall <- c(rep(0,1500))         # Initial vector for identification of energy
                                         # deficiency due to limited feed availability
  PROTDEFfactall <- c(rep(0,1500))       # Initial vector for identification of protein
                                         # deficiency as a limiting factor 
  ActualSEQ <- c(rep(0,210))             # Initial vector for the measued FPCM in the 
                                         # first 210 days of the lactation for each cow
  FeedlimSEQ <- c(rep(0,210))            # Initial vector for the simulated feed-limited
                                         # FPCM production in the first 210 days of the 
                                         # lactation for each cow
  PotSEQ <- c(rep(0,210))                # nitial vector for the simulated milk
                                         # according to the genetic potential in the 
                                         # first 210 days of the lactation for each cow
  
  #######################################################################################
  
  # The c-loop below indicates the simulations for the three experiments (1:3)
  
  for(c in c(1:3)) {
  
  ####################################################################################### 
  #                            Parameters from model calibration                        #
  #######################################################################################
  
  # Several parameters have been tested whether any change in their value resulted in a
  # significant change in the output of the model. For more information see the section
  # 'Methods for calibration and evaluation' in the Materials and methods of the paper
  # describing LiGAPS-Dairy.
    
  # During calibration, some parameters have been adjusted to minimize the root mean
  # square error (RMSE) resulting from the difference between the simulated feed quality
  # limited FPCM production and the actual, measured FPCM production.
    
  # The vector pars (parameters) consists of the following parameters
    # [1] NE for maintenance (kJ NE kg EBW-0.75, for dairy cattle)
    #     This parameter is based on the standard value for B. taurus cattle (311 kJ NE
    #     kg EBW-0.75), and an addition of 20%, which corresponds to the higher 
    #     requirements of dairy cattle. This parameter was not calibrated.
    # [2] Conversion from digestible energy (DE) to metabolisable energy (ME). This
    #     parameter was not calibrated
    # [3] Efficiency of conversion of NE to milk (on an energy basis). This parameter was
    #     calibrated (see Table S2)
    # [4] Parameter Wood's curve A. This parameter was calibrated (see Table S2)
    # [5] Parameter Wood's curve B. This parameter was calibrated (see Table S2)  
    # [6] Parameter Wood's curve C. This parameter was calibrated (see Table S2)
    # [7] Parameter MF1, used in Eq. 8 of the paper. This parameter was calibrated (see 
    #     Table S2)
    # [8] Minimum reduction factor for the genetic potential milk yield (RFMY), used in
    #     Eq. 6 of the paper. This parameter was calibrated (see Table S2)
  
  # Note: Curvature parameter ?? for feed intake after time to maximum intake during  
  #       lactation (ALFA2 in this code) was calibrated as well, and the original value
  #       of 0.5 was increased to 0.6
      
  pars <- c(373.2, 0.82, 1, 35.01*0.85, 0.098*0.85, 0.00278*0.85, 1, 0.2)
  
  if(c==1) change <- -0.10
  if(c==2) change <- 0.00
  if(c==3) change <- -0.01
  
  if(c==1) pars <- pars*c(1,1,0.855*0.96,1-change,1-change,1+change,1.0,1.25)
  if(c==2) pars <- pars*c(1,1,0.821*0.98,1-change,1-change,1+change,0.5,1.25)
  if(c==3) pars <- pars*c(1,1,0.843*0.94,1-change,1-change,1+change,0.5,1.25)
  
  # The parameters used for simulation of one experiment thus depends on the calibration 
  # of the model for the other two experiments
  
  #######################################################################################
  
  EVALSCORESexps <-c(rep(0,18))       # Initial vector for performance indicators model
                                      # for an experiment
  relDEFLIMFACTORSexps <- c(rep(0,6)) # Initial vector for defining and limiting factors
                                      # in an experiment
  TempHEATSTRESSexps <- NULL          # Initial vector for identification of heat stress
                                      # in an experiment 
  TempCOLDSTRESSexps <- NULL          # Initial vector for identification of cold stress
                                      # in an experiment 
  ProtDefCPexps <- NULL               # Initial vector for protein content of the diet in
                                      # an experiment
  ProtDefmilkexps <- NULL             # Initial vector for genetic potential FPCM 
                                      # production in an experiment
  relDEFLIMFACTORScow <- c(rep(0,6))  # Initial vector for listing the defining and 
                                      # limiting factors for cows in an experiment
  TempHEATSTRESScow <- NULL           # Initial vector for listing the maximum daily 
                                      # temperature at days when heat stress occurs
  TempCOLDSTRESScow <- NULL           # Initial vector for listing the minimum daily
                                      # temperature at days when cold stress occurs
  ProtDefCPcow <- NULL                # Initial vector for listing the crude protein 
                                      # content of the diet at days when protein 
                                      # deficiency occurs
  ProtDefmilkcow <- NULL              # Initial vector for listing the genetic potential
                                      # for FPCM production at days when protein
                                      # deficiency occurs
  
  #######################################################################################
  
  GENfactexps        <- c(rep(0,1500)) # Initial vector for the defining factor genotype
                                       # per experiment (daily basis) 
  HEATSTRESSfactexps <- c(rep(0,1500)) # Initial vector for the defining factor heat 
                                       # stress per experiment 
  COLDSTRESSfactexps <- c(rep(0,1500)) # Initial vector for the defining factor cold
                                       # stress per experiment 
  FILLGITfactexps    <- c(rep(0,1500)) # Initial vector for the limiting factor digestion
                                       # capacity per experiment
  NEDEFfactexps      <- c(rep(0,1500)) # Initial vector for the limiting factor energy
                                       # deficiency due to limited feed availability, per
                                       # experiment
  PROTDEFfactexps    <- c(rep(0,1500)) # Initial vector for the limiting factor protein
                                       # deficiency per experiment
  
  GENfactcow        <- c(rep(0,1500))  # Initial vector for the defining factor genotype
                                       # per cow
  HEATSTRESSfactcow <- c(rep(0,1500))  # Initial vector for the defining factor heat
                                       # stress per cow 
  COLDSTRESSfactcow <- c(rep(0,1500))  # Initial vector for the defining factor cold
                                       # stress per cow 
  FILLGITfactcow    <- c(rep(0,1500))  # Initial vector for the limiting factor digestion
                                       # capacity per cow
  NEDEFfactcow      <- c(rep(0,1500))  # Initial vector for the limiting factor energy
                                       # deficiency due to limited feed availability, per
                                       # cow
  PROTDEFfactcow    <- c(rep(0,1500))  # Initial vector for the limiting factor protein
                                       # deficiency per cow
  
  #######################################################################################
  
  # The experiments are simulated in the following order
  if(c==1) E1 <- 3  # Van Duinkerken et al. (2005)
  if(c==2) E1 <- 2  # Meijer et al. (1998)
  if(c==3) E1 <- 1  # The experiment conducted at 't Gen; described in Zom et al. (2012b)
  
  # Performance indicators for the model LiGAPS-Dairy
  EVALUATIONALLexp <- rep(0,4)         # Performance indicators for FPCM production per
                                       # lactation (kg FPCM per lactation)
  EVALUATIONFIALLexp <- rep(0,4)       # Performance indicators for feed intake per
                                       # lactation (kg DM per lactation)
  EVALUATIONDAILYALLexp <- rep(0,15)   # Performance indicators for daily FPCM production
                                       # (kg FPCM per cow per day)
  EVALUATIONDAILYFIALLexp <- rep(0,15) # Performance indicators for daily feed intake
                                       # (kg DM per cow per day)
  
  #######################################################################################
  
  # The e-loop was used previously during calibration to link the two experiments out of
  # the three. This loop is used once, so it is not an actual loop during model 
  # evaluation
  
  for(e in c(E1)) { 
  
  #######################################################################################
    
  # Code to adapt housing considitions (stable vs outdoor grazing) in different periods
  # of the year. Stable (STABLE) is specified l.90
  # Housing 0 = housed in a stable; 1 = outdoors (search for ill.housing for how this 
  # code is used)
    
  ill.housing1 <- rep(c(STABLE,STABLE,STABLE,1,1,0,0,1,1,1,0,0),each=13) 
  ill.housing2 <- rep(c(STABLE,STABLE,STABLE,0,1,0,0,1,0,1,0,0),each=13) 
  ill.housing3 <- rep(c(STABLE,STABLE,STABLE,1,1,0,0,1,1,1,0,0),each=13) 
    
  # Diet numbers. This option is not used, because information on the diet is obtained 
  # from the database of Wageningen Livestock Research, which contains information on the
  # diets fed in all experiments
  FEEDNR <- rep(1,1000) 
  
  COUNT = 0           # Initial number for counting the cows in an experiment
  TABLEOUTPUT = NULL  # Initial matrix for production metrics
  
  EXPERIMENT = EXPERIMENTNR[e]       # Selects experiment number 8, 22, or 63
  EVALUATIONCOWS <- rep(0,26)        # Initial vector for 26 performance metrics of
                                     # individual cows
  EVALUATIONALL <- rep(0,4)          # Initial vector for milk production per lactation,
                                     # per experiment
  EVALUATIONFIALL <- rep(0,4)        # Initial vector for feed intake per experiment 
  EVALUATIONDAILYALL <- rep(0,15)    # Initial vector for daily milk production per  
                                     # experiment
  EVALUATIONDAILYFIALL <- rep(0,15)  # Initial vector for daily feed intake per 
                                     # experiment
  
  #######################################################################################
  
  # Some of the cows in the experiments of Meijer et al. (1998) and van Duinkerken et al. 
  # (2005) had multiple lactations while being part of the experiment. Therefore, a
  # minimum period without lactation is defined to separate between individual
  # lactations. This minimum period is DIFFDAYSMAX, and was set at 20 days for both 
  # experiments mentioned above. Data were available per week, so during lactation the
  # minimum intervals are 7 days.
  
  if(EXPERIMENT==8) DIFFDAYSMAX <- 30 else DIFFDAYSMAX <- 20
  
  LACTATINNRexp <- NULL       # Initial vector for number of lactations per cow in the 
                              # experiment
  MILKMEASUREMENTSexp <- NULL # Initial vector for the number of milk measurements per
                              # cow per experiment
  
  #######################################################################################
  
  # z-loop for individual cows 
  
  # Cows not meeting the requirements (30 weekly measurements required) were excluded 
  # from the analysis. See also Table 3 of the paper.
  
  if(EXPERIMENT == 8)  zseries <- c(1:77)                   # 't Gen
  if(EXPERIMENT == 22) zseries <- c(1:54,56:60,62:69,71:79) # Meijer et al. (1998)
  if(EXPERIMENT == 63) zseries <- c(1:31,33:57,59:69)       # van Duinkerken et al. 
                                                            # (2005)
  # Experiment 8 includes 77 cows, experiment 22 includes 76 cows, experiment 63 includes
  # 67 cows
  
  #######################################################################################
  
  for (z in zseries) { 
                       
  # Code below is designed for sensitivity analysis (not used in model illustration)
    
  # Settings for sensitivity analysis (par. 119 is reference/standard), s indicates the
  # sensitivity loop
    
    NPAR            = 118       # number of parameters in sensitivity analysis 
    NPAR            = NPAR + 1  # includes base scenario
    RELDIFF         = -0.10     # relative increase or decrease of parameters (fraction)
                                # to do a one-at-a-time sensitivity analysis
    
  # The four lines below create a multiplication matrix for sensitivity analysis 
    SENSDAT <- c(rep(c(1,rep(0,NPAR)),(NPAR-1)),1)
    SENSMAT <- matrix(nrow=NPAR, ncol=NPAR, data = (SENSDAT * RELDIFF+1))
    SENSMAT[119,119] <- 1.0     # Base scenario adjustment
    SENSMAT <- SENSMAT[1:NPAR,]
    
  # End of the sensitivity analysis part
    
    FESENSIND <- c(rep(0,NPAR))  # Matrix for feed efficiency for individual bull calves
    FESENSHERD <- c(rep(0,NPAR)) # Matrix for feed efficiency for herd (units)
    FESENSREPR <- c(rep(0,NPAR)) # Matrix for feed efficiency for cows
    
  #######################################################################################
    
      for(s in 119){  # Model simulations are run for the base scenario, with original 
                      # parameters
      
  #######################################################################################
  # 1.                                  Initial section                                 #    
  #######################################################################################
      
  #######################################################################################
  # 1.1                             Farming system description                          #    
  #######################################################################################
      
  # The farming system is described in this section of the model
      
  # Cattle breed and location have been specified above
      
      SCALE = 2                 # Scale of the system (1 = individual animal; 2 = repr. 
                                # cow + offspring; i.e. herd scale)
      ANIMALSEX = 0             # Animal sex (0 = male; 1 = female), only for 
                                # individual animals (i.e. if scale equals 1)
      
  # Cattle management
      
      MAXCALFNR = 8             # Number of calves per cow (max = 8; only for cows)
      imax = 4300               # Maximum duration of simulation (expressed in number of  
                                # days, almost 11 years for cows)
      MAXFATCARC = 0.0          # maximum fat percentage in the carcass for slaughter 
                                # (indicator of beef quality). Set to zero, so beef 
                                # quality does not affect the simulation.
      MAXLIFETIME = 11.62       # maximum # years a productive animal can live (years)
      MAXCONCAGE = 9.00         # maximum conception age of a reproductive animal (years) 
      CULL = 0.0                # culling rate (fraction reproductive cows per year)  
      SWMALES = 48              # slaughter weight male calf/calves (kg)
      SWFEMALES = 43.1          # slaughter weight female calf/calves (kg)
  
  #######################################################################################
      
  # Day of the year (Julian day) in which the first animal in an experiment is born
      if(EXPERIMENT==8) STDOYBASE1 = 10 else if(EXPERIMENT==22) STDOYBASE1 = 1 else 
        if(EXPERIMENT==63) STDOYBASE1 = 75
      
  # Measured output cow is read from .csv files that contain the production data for each
  # cow in the experiments.
      
      if(EXPERIMENT ==  8) LISTOUTFILES <- list.files("M:/My Documents/PostDoc Investing in Sustainable Livestock/LiGAPS_Dairy/Measured output/Experiment 8")
      if(EXPERIMENT == 22) LISTOUTFILES <- list.files("M:/My Documents/PostDoc Investing in Sustainable Livestock/LiGAPS_Dairy/Measured output/Experiment 22")
      if(EXPERIMENT == 63) LISTOUTFILES <- list.files("M:/My Documents/PostDoc Investing in Sustainable Livestock/LiGAPS_Dairy/Measured output/Experiment 63")
      
      if(EXPERIMENT ==  8) OUTFILENAME <- paste("M:/My Documents/PostDoc Investing in Sustainable Livestock/LiGAPS_Dairy/Measured output/Experiment 8/", LISTOUTFILES[z], sep = "")
      if(EXPERIMENT == 22) OUTFILENAME <- paste("M:/My Documents/PostDoc Investing in Sustainable Livestock/LiGAPS_Dairy/Measured output/Experiment 22/", LISTOUTFILES[z], sep = "")
      if(EXPERIMENT == 63) OUTFILENAME <- paste("M:/My Documents/PostDoc Investing in Sustainable Livestock/LiGAPS_Dairy/Measured output/Experiment 63/", LISTOUTFILES[z], sep = "")
  
  # Data from Access database (Access databases.R) of Wageningen Livestock Research     
      MEASUREDOUTPUT <- read.csv(file=OUTFILENAME, header=T) 
  
  #######################################################################################
      
      
  # Initial weight of cows at the start of experiments 
  # If the weight in the first week is unknown, the weight in the second weeks is 
  # assumed to apply for the first week.
      if(is.na(MEASUREDOUTPUT$WeightKG[1])&& EXPERIMENT==8 || EXPERIMENT==22) 
        MEASUREDOUTPUT$WeightKG[1] <- MEASUREDOUTPUT$WeightKG[2] 
  
  # Differences in time sampled/measured (days)    
      DIFFDAYS <- c(na.omit(c(MEASUREDOUTPUT$Time,NA)-c(NA,MEASUREDOUTPUT$Time))) 
      DIFFDAYS2 <- c(DIFFDAYS[2:length(DIFFDAYS)],0)
    
  # Start day based on weather data
  # Standard start day is 4 years before start of the experiment 
  # (Exp. 8 = 10-01-1991, Exp. 22 = 01-01-1994, Exp. 63 = 16-03-1998)
  # Start dates experiments correspond to day 2200, 3287, and 4822 for experiments 8, 
  # 22, and 63, respectively
  # Birth dates are the start days of the experiments minus 1460 days (4 * 365 days)
      
      
      YRCOR <- 0
      if(EXPERIMENT==8 && MEASUREDOUTPUT$WeightKG[2] < 604*0.8) YRCOR <- 730 else
        if(EXPERIMENT==8 && MEASUREDOUTPUT$WeightKG[2] >= 604*0.8 && 
           MEASUREDOUTPUT$WeightKG[2] < 693*0.8) YRCOR <- 365 else
          if(EXPERIMENT==8 && MEASUREDOUTPUT$WeightKG[2] >= 693*0.8) YRCOR <- 0
      
      if(EXPERIMENT==22 && MEASUREDOUTPUT$WeightKG[2] < 604*0.8) YRCOR <- 730 +(1827-1460) else
        if(EXPERIMENT==22 && MEASUREDOUTPUT$WeightKG[2] >= 604*0.8 && 
           MEASUREDOUTPUT$WeightKG[2] < 693*0.8) YRCOR <- 365+(1827-1460) else
          if(EXPERIMENT==22 && MEASUREDOUTPUT$WeightKG[2] >= 693*0.8) YRCOR <- 0+(1827-1460)
      
      if(EXPERIMENT==8) STDOY = 740 + YRCOR      
      
      if(EXPERIMENT==22) STDOY = 1827 + YRCOR  
      
      if(EXPERIMENT==63) STDOY = 3362 
      
  #######################################################################################
  
  # Housing 
  # Numbers indicate days in a specific phase corresponding to animals kept in stables 
  # or animals outdoors
      
      PHASE1 <- rep(ill.housing1[z],120) # Housing period 1 (0 = stable or feedlot, 1 = 
                                         # outdoors, free grazing system; 2 = outdoors, 
                                         # open feedlot)
      PHASE2 <- rep(ill.housing2[z],214) # Housing period 2 (Sum of all phases should 
                                         # equal 365 days)
      PHASE3 <- rep(ill.housing3[z],31)  # Housing period 3
      
      WINDMAX = 20          # maximum wind speed (m s-1, prevents unrealistic high wind 
                            # speeds if weather data are not too reliable) 
      RADTRANS = 0.0        # fraction of solar radiation in stable (related to roof 
                            # construction, 0.0 means all light is intercepted by the roof)
      WINDRED = 0.1         # fraction reduction of wind speed in stable (related to 
                            # construction)
      TINCR = 0             # increase in stable temperature compared to outdoor at 0 
                            # degrees Celsius (parameter depends on stable type) 
      Tdelta = 1            # increase in stable temperature per degree Celius increase 
                            # in outdoor temperature (parameter depends on stable type)
      
  ###########################################################################################
  # 1.2                                    Weather data                                     #
  ###########################################################################################
      
  # Library with weather data (file selected depends on LOCATION)
  # Files with weather data must be saved in a directory, and the reference to this directory 
  # must be valid! File names must match with the location (LOCATION)
   
  # New users must adapt the directory of the weather file! 
      
  # Library with weather data (file chosen depends on LOCATION)
      
  if(LOCATION == 'NETHERLANDS') 
    WEATHER <-read.csv("M:/My Documents/R/NLDLEL19912018_ext.csv", head=TRUE,sep=",") 
      
  # Maximum wind speed equals WINDMAX to avoid unrealistically high winds speeds
  
      if(max(WEATHER$WIND) > WINDMAX) WINDHIGH <- "Yes" else WINDHIGH <- "No"
      WEATHER$WIND[WEATHER$WIND > WINDMAX] <- WINDMAX  
      
  # Connect the phases where cattle are outdoors or kept in stables. The line below can be  
  # used to represent housing in winter and grazing outdoors in summer.
      PHASE <- c(PHASE1,PHASE2,PHASE3)  # Connect all phases in one year
      HOUSING <- rep(PHASE,30)    # Thirty years with housing (stable/free grazing) 
      
      
  # Modify weather data if cattle are housed in stables  
      for(i in 1:imax){
      
  # Roof over stable reduces or fully blocks radiation levels (RADTRANS)
        if(HOUSING[i] == 0) WEATHER$RAD[i]   <- WEATHER$RAD[i] * RADTRANS            
  # Stable construction reduces wind speed (WINDRED)
        if(HOUSING[i] == 0) WEATHER$WIND[i]  <- WEATHER$WIND[i] * (1-WINDRED)            
  # Increase in stable temperature rel. to outdoor temperature
        if(HOUSING[i] == 0) WEATHER$MINT[i]  <- Tdelta * WEATHER$MINT[i] + TINCR     
  # Increase in stable temperature rel. to outdoor temperature
        if(HOUSING[i] == 0) WEATHER$MAXT[i]  <- Tdelta * WEATHER$MAXT[i] + TINCR     
      }
      
  # If the first animal is born after the 1st of January, the weather file is adapted 
  # accordingly.
      
      HOUSING <- HOUSING[STDOY:length(HOUSING)] 
      WEATHER <- WEATHER[STDOY:nrow(WEATHER),] 
                                               
  # Calculates day of the year (DOY) if days are numbered ascending for years     
      DOY <- WEATHER$DOY-floor(WEATHER$DOY/365)*365  
  # For simplicity, one year is assumed to have 365 days per year instead of 365.24 days per year
      DOY[DOY==0] <-365                              
      
      WEATHERORIG <- WEATHER # Copies the weather data file 
      
  # End of the section related to weather data
      
  ###########################################################################################
  # 1.3                                    Parameters                                       #
  ###########################################################################################
      
  ###########################################################################################
  # 1.3.1               Genetic parameters (related to BREED and SEX)                    #
  ###########################################################################################
      
  # This genetic parameter section contains a list with parameters (a LIBRARY) which are 
  # specific for breed and sex. Numbers before the parameter description refer to the order  
  # of parameters in the LIBRARY, which is not the same as in the supplementary material of
  # van der Linden et al. (2019, Animal 13 : 845-855) 
  # The numbers after the parameter description (between brackets) refer to parameter numbers 
  # in Table S2 of the supplementary material beloninging to the paper of Van der Linden et 
  # al. (2019).
  #  
  # 1 reflectivity coat [5], 2 coat length [3], 3 area corr[1], 4 max body core-skin 
  # conductance [4], 5 birth weight [9], 6-10 genetic potential growth according to Gompertz 
  # curve [9-12,19], 11-12 Wood's equation parameter1 and 2 (A = 0, no milk production male) 
  # [13,14], 13 adult max. weight [19], 14 sex (0= male, 1 = female), 15-16 milk prod., 2x 
  # start milk param. [13,14], 17 fraction TBW fertility [20], 18 maintenance factor [16], 
  # 19 min perc. ((subc. + interm. fat)/TBW) for gestation [21], 20 lipid fraction bone 
  # parameter[15], 21 carcass fraction [17], 22 muscle:bone ratio [18], 23 min. cond. 
  # core-skin. par. [4], 24-26 sweating parameters, latent heat release [6-8] 27 Wood's 
  # equation parameter 3. 
  
  ###########################################################################################    
      
  # Parameters for Holstein-Friesian bulls
      
  # 1-5     reflectivity coat, coat length, area corr, max body core-skin conductance, birth
  # weight 
      LIBRARY10 <- c(0.30, 0.01075, 1.00 , 64.1*(50/30), 42.9,
  # 6-10    genetic potential growth according to Gompertz curve                     
                     1079, 42.9, 2.98, 1.33, 52.6,                             
  # 11-16   Wood's equation parameter 1 and 2 (A = 0, no milk production male), adult max.  
  # weight, milk prod., 2x start milk param. (milk from mother)
                     0.0000, 0.147, 1027, 0, 34.98, 0.147,                     
  # 17-19   fraction TBW fertility, maintenance factor, min perc. ((subc. + interm. fat)/TBW) 
  # for gestation 
                     0.49, 1.09, 0.10,
  # 20-23   lipid fraction bone parameter, carcass fraction, muscle:bone ratio, min. cond. 
  # core-skin parameter  
                     11.1, 0.58, 3.22, 1.00,
  # 24-26   sweating parameters, latent heat release, 27 Wood's equation parameter 3
                     3.08, 1.73, 35.3, 0.00368)                                    
  
  ###########################################################################################    
                     
  # Parameters for Holstein-Friesian heifers / cows
  
  # 1-5     reflectivity coat, coat length, area corr, max body core-skin conductance, birth 
  # weight
      LIBRARY11 <- c(0.30, 0.01075, 1.00, 64.1*(50/30), 40.1,
  # 6-10    genetic potential growth according to Gompertz curve                   
                     732.5, 40.1, 3.06, 1.40, 32.5,
  # 11-16   Wood's equation parameter 1 and 2 (A = 1, milk production female), adult max. 
  # weight, milk prod., 2x start milk param. (milk from mother)
                     pars[4], pars[5], 700, 1, pars[4], pars[5],
  # 17-19   fraction TBW fertility, maintenance factor, min perc. ((subc. + interm. fat)/TBW) 
  # for gestation
                     0.49, 1.09, 0.10,                                         
  # 20-23   lipid fraction bone parameter, carcass fraction, muscle:bone ratio, min. cond. 
  # core-skin parameter 
                     11.8, 0.55, 3.02, 1.00,
  # 24-26   sweating parameters, latent heat release, 27 Wood's equation parameter 3
                     3.08, 1.73, 35.3, pars[6])                                
                               
  ###########################################################################################
  
  # Simulations with individual cows or other animal do not include birth of calves, so the
  # scale (SCALE) has to be set at one.
      if(SCALE== 1) MAXCALFNR <- 0 else MAXCALFNR <- MAXCALFNR         
  
      SEXC <- c(1,0,0,0,0,0,0,0,0,0,0,0,0)  # Sex offspring cow (1= female; 0=male)
  # The sex of calves is not that relevant, because in the model illustration we remove 
  # calves right after birth (corresponds to practice in the Netherlands)
      
      if(SCALE== 1) SEX <- ANIMALSEX else SEX <- c(1,SEXC) # Sex cow + offspring (1= female; 
                                                           #0=male)  
  # Number of animals in a herd unit (if the cows gives birth to the maximum number of 
  # calves)
      
      jmax <- MAXCALFNR + 1
  
  # Vector to indicate reproductive animals (1= reproductive, 0 = productive)    
      if(SCALE== 2) REPRODUCTIVE <- c(1,0,0,0,0,0,0,0,0,0,0,0,0) else  
        REPRODUCTIVE <- c(0,0,0,0,0,0,0,0,0,0,0,0,0) 
  # Vector to indicate replacement animals (1= replacement, 0 = other)
      REPLACEMENT  <- c(0,1,0,0,0,0,0,0,0,0,0,0,0,0)                   
  # Vector to indicate productive animals (1= productive, 0 = other)
      PRODUCTIVE   <- c(0,0,1,1,1,1,1,1,1,1,1,1,1,1)                   
  # Vector helps to read the correct weather data for each animal    
      ORDER <- c(0,1,2,3,4,5,6,7,8,9,10)                               
      
  # End of the section related to genetic parameters
        
  ###########################################################################################
  # 1.3.2                                Feed parameters                                    #
  ###########################################################################################
      
  # Feed library
  # Feed parameters after often from Chilibroste et al, 1997 and Jarrige et al., 1986 
  # (Fill units)
  #
  # Chilibroste P, Aguilar C and Garcia F 1997. Nutritional evaluation of diets. 
  # Simulation model of digestion and passage of nutrients through the rumen-reticulum. 
  # Animal Feed Science and Technology 68, 259-275.
  #
  # Jarrige R, Demarquilly C, Dulphy JP, Hoden A, Robelin J, Beranger C, Geay Y, Journet 
  # M, Malterre C, Micol D and Petit M, 1986. 
  # The INRA fill unit system for predicting the voluntary intake of forage-based diets 
  # in ruminants - a review. Journal of Animal Science 63, 1737-1758.
  # 
  # Abbreviations
  # HIF = Heat Increment of feeding (MJ MJ-1 metabolisable energy, see Table S4 of the 
  # supplementary material of Van der Linden et al., 2019)
  #
  # Van der Linden A, van de Ven GWJ, Oosting SJ, van Ittersum MK, ANDde Boer IJM, 2019.
  # LiGAPS-Beef, a mechanistic model to explore potential and feed-limited beef 
  # production 1: model description and illustration. Animal 13:845-855,
  # https://doi.org/10.1017/S1751731118001726
  #
  # FU = Fill Units (-)
  # SNSC = Soluble, Non-Structural Carbohydrates (g kg-1 DM)
  # INSC = Insoluble, Non-Structural Carbohydrates (g kg-1 DM)
  # DNDF = Digestible Neutral Detergent Fibre (g kg-1 DM)
  # SCP = Soluble Crude Protein (g kg-1 DM)
  # DCP = Digestible Crude Protein (g kg-1 DM)
  # kdINSC = digestion rate Insoluble, Non-Structural Carbohydrates (% hr-1)
  # kdNDF = digestion rate Neutral Detergent Fibre (% hr-1)
  # kdDCP = digestion rate Digestible Crude Protein (% hr-1)
  # kdPass = standard passage rate in the rumen (% hr-1)
  # UNDF = Undegradable Neutral Detergent Fibre (g kg-1 DM)
  # pef = physical effectiveness factor for Neutral Detergent Fibre (-)
  # CP = crude protein (g kg DM-1)
  # GE = gross energy (MJ kg DM-1)    
  # The vectors below correspond to Table S3 and S4 of the supplementary material   
  #
  #                        HIF    FU    SNSC INSC PNDF     SCP    PICP   LEFT   kdINSC kdPNDF kdPICP kdPASS  UNDF    NDF    pef   CP    GE                
  #                         1      2     3    4      5       6      7      8       9      10     11     12    13      14     15   16   17
  BARLEY             <- c(0.245, 0.573, 389, 214, 156.00,  34.50, 82.80, 116.70, 0.242, 0.145, 0.125, 0.040,  21.00, 0.210, 0.70, 110, 18.4) #            # Kolver,2000
  CONCENTRATE        <- c(0.249, 0.619, 262, 175, 243.10,  72.80, 87.36, 161.74, 0.150, 0.060, 0.100, 0.040,  42.90, 0.286, 0.70, 182, 18.5) #            # Chilibroste et al, 1997 Last value is eNDF for barley, Mertens, 1997, Fill unit estimated  
  HAY                <- c(0.318, 1.120, 100, 150, 345.80,  48.16, 74.30, 281.74, 0.300, 0.040, 0.085, 0.035, 148.20, 0.494, 1.00, 172, 18.5) #            # Chilibroste et al, 1997 Last value is eNDF, Mertens, 1997, Fill unit from Jarrige, 1989                      
  HAYPOOR            <- c(0.420, 1.370,  73,  73, 462.00,  20.30,149.10, 347.90, 0.300, 0.040, 0.085, 0.035, 198.00, 0.660, 1.00,  70, 18.2) #            # Kolver,2000
  GRASSSPRING        <- c(0.304, 0.960, 130,  30, 360.00,  66.25, 97.40, 361.30, 0.300, 0.040, 0.085, 0.035, 120.00, 0.400, 0.40, 265, 18.6) #            # Kolver,2000
  GRASSSUMMER        <- c(0.356, 1.120, 100,  60, 376.00,  49.50, 76.50, 385.00, 0.300, 0.040, 0.085, 0.035, 141.00, 0.470, 0.50, 180, 18.4) #            # Kolver,2000
  GRASSSUMMERDRY     <- c(0.447, 1.280,  50,  60, 409.50,  23.00, 69.00, 411.50, 0.300, 0.040, 0.085, 0.035, 175.50, 0.585, 1.00, 115, 18.1) #            # Kolver,2000 
  MAIZE              <- c(0.237, 0.438, 202, 532, 101.70,  20.10, 86.56,  57.64, 0.040, 0.051, 0.035, 0.050,  11.30, 0.113, 0.40, 134, 17.0) #            # Chilibroste et al, 1997 Last value is eNDF, Mertens, 1997, Fill unit estimated
  MAIZESILAGE        <- c(0.289, 1.000, 100, 351, 239.00,  54.94, 23.00, 483.06, 0.250, 0.040, 0.040, 0.030, 239.00, 0.478, 0.93,  82, 18.5)
  MOLASSES           <- c(0.050, 0.200, 828,   0,      0,    3.8,   0.2,      0,     0,     0, 0.125, 0.040,      0,     0,    0,   0, 17.0) #
  SBM                <- c(0.242, 0.526, 107,   0, 138.60, 202.80,243.40, 232.00, 0.242, 0.145, 0.125, 0.040,   0.00, 0.210, 0.40, 507, 19.7)
  STRAWCER           <- c(0.557, 1.800,  14,  78, 401.00,  10.00,  5.00, 370.00, 0.300, 0.040, 0.085, 0.035,   0.00, 0.210, 1.00,  40, 18.3) #            # MAFF, 1986 UK table book, p. 24 
  WHEAT              <- c(0.234, 0.475, 475, 212,  80.00,  39.90, 69.80,    0.0, 0.182, 0.150, 0.080, 0.040,  34.20, 0.114, 0.70, 133, 18.2)              # MAFF, 1986 UK table book, p. 24 
      
  PASTURE            <- c(0.323, 1.195, 100,  60, 376.00,  49.50, 76.50, 385.00, 0.300, 0.040, 0.085, 0.035, 141.00, 0.470, 0.50, 180, 18.4)
  PASTURE1           <- c(0.358, 1.12,  50,  60, 551.00,  23.00, 69.00, 411.50, 0.300, 0.040, 0.085, 0.035, 175.50, 0.585, 1.00, 115, 18.1) #               
      
  # Diet under potential production: mix between soy bean meal, good quality hay, and wheat 
  MIX                <- (0.19526*SBM+0.31827*WHEAT+0.48647*HAY) 
 
  ###########################################################################################
  
  # Lists with files are created for each experiment (8,22 and 63). Each file contains 
  # feed data for one cow. The directories must correspond to the correct directories on the
  # computer drive. The diets corresponds to the measured feed quality and feed intake.
  
  if(EXPERIMENT ==  8) LISTFEEDFILES <- list.files("M:/My Documents/PostDoc Investing in Sustainable Livestock/LiGAPS_Dairy/Feed/Experiment 8")
  if(EXPERIMENT == 22) LISTFEEDFILES <- list.files("M:/My Documents/PostDoc Investing in Sustainable Livestock/LiGAPS_Dairy/Feed/Experiment 22")
  if(EXPERIMENT == 63) LISTFEEDFILES <- list.files("M:/My Documents/PostDoc Investing in Sustainable Livestock/LiGAPS_Dairy/Feed/Experiment 63")
  
  # Selects the name of a file in the z-loop    
  if(EXPERIMENT ==  8) FILENAME <- paste("M:/My Documents/PostDoc Investing in Sustainable Livestock/LiGAPS_Dairy/Feed/Experiment 8/", LISTFEEDFILES[z], sep = "")
  if(EXPERIMENT == 22) FILENAME <- paste("M:/My Documents/PostDoc Investing in Sustainable Livestock/LiGAPS_Dairy/Feed/Experiment 22/", LISTFEEDFILES[z], sep = "")
  if(EXPERIMENT == 63) FILENAME <- paste("M:/My Documents/PostDoc Investing in Sustainable Livestock/LiGAPS_Dairy/Feed/Experiment 63/", LISTFEEDFILES[z], sep = "")
      
  # A file with a particular name is read.
  FEEDINPUT <- read.csv(file=FILENAME, header=T)
  
  ###########################################################################################
  
  # If potential production is simulated, the measured diet is not applicable any more. 
  # Instead, the diet under potential production is used. This diet consists of 19.5% soybean
  # meal, 31.8% wheat, and 48.6 good% quality hay.
  if(POTENTIAL == 1) POTENTIALFEED <- read.csv(file="M:/My Documents/PostDoc Investing in Sustainable Livestock/LiGAPS_Dairy/Feed/Experiment 8/Feedinput.csv", header=T) 
  if(POTENTIAL == 1) FEEDINPUT[,4:12] <- POTENTIALFEED[1,2:10]  
        
  # Cows are simulated from birth onwards. The standard age of cows at the start of 
  # experiments is assumed to be 4 years (365 days x 4 years = 1460 days)
  if(EXPERIMENT ==  8)  FEEDINPUT$Time <- FEEDINPUT$Time + 1460 - YRCOR
  if(EXPERIMENT ==  22) FEEDINPUT$Time <- FEEDINPUT$Time + 1460 - YRCOR
  if(EXPERIMENT ==  63) FEEDINPUT$Time <- FEEDINPUT$Time + 1460 - YRCOR
  
  # Daily total (measured) feed intake is calculated, expressed in kg DM per cow per day      
  FIDAILYTOTAL <- FEEDINPUT$Feed1QNTY + FEEDINPUT$Feed2QNTY  
  
  # The feed intake of feed 1 and 2 are both set at 50 kg DM per animal per day under feed 
  # quality limited production. The total of 100 kg DM feed available per animal per day
  # is considered to be ad libitum under all conditions.
  if(FEEDQUALITY == 1) FEEDINPUT$Feed1QNTY <- rep(50,length(FEEDINPUT$Feed1QNTY))
  if(FEEDQUALITY == 1) FEEDINPUT$Feed2QNTY <- rep(50,length(FEEDINPUT$Feed2QNTY))
       
  # Calving day for a cow is calculated     
  if(MEASUREDOUTPUT$Time[1]<=365) STDOYBASE <- STDOYBASE1 + MEASUREDOUTPUT$Time[1] - 1 
  TIMEOUTPUT <- MEASUREDOUTPUT$Time[1]
  if(MEASUREDOUTPUT$Time[1]>365) STDOYBASE <- STDOYBASE1 + MEASUREDOUTPUT$Time[1]-
    floor(MEASUREDOUTPUT$Time[1]/365)*365 -1
      
  # The age at first calving in an experiment is included in the outputfile.  
  MEASUREDOUTPUT$Time <- MEASUREDOUTPUT$Time + 1460 - YRCOR
  MILKMEASUREMENTSexp[z] <- length(MEASUREDOUTPUT$MilkKG)
      
  # This file includes time since the start of the experiment
      
  # Experiment  8: start 1991-01-10
  # Experiment 22: start 1994-01-01
  # Experiment 63: start 1998-03-16
  
  ###########################################################################################
   
  # Feed data are read from the feed input file for an individual cow, and converted into
  # a format that can be used in the feed digestion sub-model.
  F1i <- cbind(c(0.0001044*FEEDINPUT$Feed1Dig^2 - 0.0195376*FEEDINPUT$Feed1Dig + 1.1501084),
               c(-2.2755*FEEDINPUT$Feed1Dig/100 + 2.5447),
               ((FEEDINPUT$Feed1Dig/100*0.82*18.4)-(0.00007222*FEEDINPUT$Feed1CP^2 - 0.00179840*FEEDINPUT$Feed1CP + 6.74998070))/0.014268+100,
               60, 376.00,  49.50, 76.50, 385.00, 0.300, 0.040, 0.085, 0.035, 141.00, 0.470, 0.50,
               FEEDINPUT$Feed1CP, 18.4)
  F2i <- cbind(c(0.0001044*FEEDINPUT$Feed2Dig^2 - 0.0195376*FEEDINPUT$Feed2Dig + 1.1501084),
               c(-2.2755*FEEDINPUT$Feed2Dig/100 + 2.5447),
               ((FEEDINPUT$Feed2Dig/100*0.82*18.4)-(0.00007222*FEEDINPUT$Feed2CP^2 - 0.00179840*FEEDINPUT$Feed2CP + 6.74998070))/0.014268+100,
               60, 376.00,  49.50, 76.50, 385.00, 0.300, 0.040, 0.085, 0.035, 141.00, 0.470, 0.50,
               FEEDINPUT$Feed2CP, 18.4)
  F3i <- cbind(c(0.0001044*FEEDINPUT$Feed3Dig^2 - 0.0195376*FEEDINPUT$Feed3Dig + 1.1501084),
               c(-2.2755*FEEDINPUT$Feed3Dig/100 + 2.5447),
               ((FEEDINPUT$Feed3Dig/100*0.82*18.4)-(0.00007222*FEEDINPUT$Feed3CP^2 - 0.00179840*FEEDINPUT$Feed3CP + 6.74998070))/0.014268+100,
               60, 376.00,  49.50, 76.50, 385.00, 0.300, 0.040, 0.085, 0.035, 141.00, 0.470, 0.50,
               FEEDINPUT$Feed3CP, 18.4)
  
  # So far, only data for days at which feed quality and feed availability were measured, 
  # were taken into account. The code below interpolates to daily feed quality and feed
  # availability. Linear interpolation is used.
  
  # HIF  = heat increment of feeding (MJ per kg DM feed)
  # FU   = fill units (Dimensionless, Jarrige et al., 1989)
  # SNSC = solid, non-structural carbohydrates (g per kg DM feed)
  # CP   = crude protein (g per kg DM feed)
  
  F1HIF   <- approx(x=c(FEEDINPUT$Time), y=c(F1i[,1]), method="linear", n=length(DOY), 
                    xout= c(1:length(DOY)), rule=2)
  F1FU    <- approx(x=c(FEEDINPUT$Time), y=c(F1i[,2]), method="linear", n=length(DOY), 
                    xout= c(1:length(DOY)), rule=2)
  F1SNSC  <- approx(x=c(FEEDINPUT$Time), y=c(F1i[,3]), method="linear", n=length(DOY), 
                    xout= c(1:length(DOY)), rule=2)
  F1CP    <- approx(x=c(FEEDINPUT$Time), y=c(F1i[,16]), method="linear", n=length(DOY), 
                    xout= c(1:length(DOY)), rule=2)
      
  F2HIF   <- approx(x=c(FEEDINPUT$Time), y=c(F2i[,1]), method="linear", n=length(DOY), 
                    xout= c(1:length(DOY)), rule=2)
  F2FU    <- approx(x=c(FEEDINPUT$Time), y=c(F2i[,2]), method="linear", n=length(DOY), 
                    xout= c(1:length(DOY)), rule=2)
  F2SNSC  <- approx(x=c(FEEDINPUT$Time), y=c(F2i[,3]), method="linear", n=length(DOY), 
                    xout= c(1:length(DOY)), rule=2)
  F2CP    <- approx(x=c(FEEDINPUT$Time), y=c(F2i[,16]), method="linear", n=length(DOY), 
                    xout= c(1:length(DOY)), rule=2)
      
  F3HIF   <- approx(x=c(FEEDINPUT$Time), y=c(F3i[,1]), method="linear", n=length(DOY), 
                    xout= c(1:length(DOY)), rule=2)
  F3FU    <- approx(x=c(FEEDINPUT$Time), y=c(F3i[,2]), method="linear", n=length(DOY), 
                    xout= c(1:length(DOY)), rule=2)
  F3SNSC  <- approx(x=c(FEEDINPUT$Time), y=c(F3i[,3]), method="linear", n=length(DOY), 
                    xout= c(1:length(DOY)), rule=2)
  F3CP    <- approx(x=c(FEEDINPUT$Time), y=c(F3i[,16]), method="linear", n=length(DOY), 
                    xout= c(1:length(DOY)), rule=2)
      
  # Data on feed quality (based on interpolation)
  FEED1 <-cbind(F1HIF$y,F1FU$y,F1SNSC$y,60, 376.00,  49.50, 76.50, 385.00, 0.300, 0.040, 
                0.085, 0.035, 141.00, 0.470, 0.50,F1CP$y,18.4)
  FEED2 <-cbind(F2HIF$y,F2FU$y,F2SNSC$y,60, 376.00,  49.50, 76.50, 385.00, 0.300, 0.040, 
                0.085, 0.035, 141.00, 0.470, 0.50,F2CP$y,18.4)
  FEED3 <-cbind(F3HIF$y,F3FU$y,F3SNSC$y,60, 376.00,  49.50, 76.50, 385.00, 0.300, 0.040, 
                0.085, 0.035, 141.00, 0.470, 0.50,F3CP$y,18.4)
  # The fourth feed type is fixed, and cannot vary over time. The first three feed types can.
  FEED4 <- HAY  
     
  ###########################################################################################
  
  # The code below interpolates the measured weekly feed intake to daily feed intake. This
  # information is only used under feed-limited production, where the available feed 
  # quantity can limit milk production. Linear interpolation is used.
  
  FEED1QNTY <- approx(x=c(FEEDINPUT$Time), y=c(FEEDINPUT$Feed1QNTY), method="linear", 
                      n=length(DOY), xout= c(1:length(DOY)), rule=2) 
  FEED1QNTY <- FEED1QNTY$y
      
  FEED2QNTY <- approx(x=c(FEEDINPUT$Time), y=c(FEEDINPUT$Feed2QNTY), method="linear", 
                      n=length(DOY), xout= c(1:length(DOY)), rule=2) 
  FEED2QNTY <- FEED2QNTY$y
      
  FEED3QNTY <- approx(x=c(FEEDINPUT$Time), y=c(FEEDINPUT$Feed3QNTY), method="linear", 
                      n=length(DOY), xout= c(1:length(DOY)), rule=2) 
  FEED3QNTY <- FEED3QNTY$y
      
  # Maximum fractions of feed types in the diet over time. This information can be used to
  # set a maximum to the fraction of concentrates in the diet. A value of 0.5 for FEED1 
  # means that FEED1 can contribute to the diet for a maximum of 50%. 
  if(FEEDNR[z] == 1) FEED1fr <- 0.5 else if(FEEDNR[z] == 2) FEED1fr <- 0.50 else 
    if(FEEDNR[z] == 3) FEED1fr <- 0.50 else if(FEEDNR[z] == 4) FEED1fr <- 0.50 else 
      if(FEEDNR[z] == 5) FEED1fr <- 0.50    
  if(FEEDNR[z] == 1) FEED2fr <- 0.5 else if(FEEDNR[z] == 2) FEED2fr <- 0.50 else 
    if(FEEDNR[z] == 3) FEED2fr <- 0.50 else if(FEEDNR[z] == 4) FEED2fr <- 0.50 else 
      if(FEEDNR[z] == 5) FEED2fr <- 0.50
      
  FEED3fr <- 1.00 # The diet can consist of 100% FEED3
  FEED4fr <- 1.00 # The diet can consist of 100% FEED4. This feed type is not used in the
  # calibration and evaluation of LiGAPS-Dairy.
  
  # Feed quantity available (kg DM per animal per day) for feed type 1-3    
  FEEDQNTYTOT <- FEED1QNTY * FEED1fr + FEED2QNTY * FEED2fr + FEED3QNTY * FEED3fr 
      
  ###########################################################################################
  # 1.3.3                               General parameters                                  #
  ###########################################################################################
      
  # General parameters used in physics and chemistry
  # Numbers between brackets refer to parameter numbers in Table S5 of the supplementary 
  # material to the paper of Van der Linden et al. (2019, Animal 13:845-855, see
  # https://doi.org/10.1017/S1751731118001726)
      
  CtoK            = 273.15             # [1] absolute zero temperature (K)
  KtoR            = 9/5                # [2] conversion degrees Kelvin to degrees Rankine
  kJdaytoW        = 1000/(3600*24)     # [3] conversion kJ day-1 to Watt
  RUC             = 0.00078            # [4] resistance conversion from s m-1 to W m-2 K-1 
                                       #     (Cena and Clark, 1978)
  EMISS           = 0.98               # [5] emissivity factor LWR (dimensionless)
  GRAV            = 9.81               # [6] gravitational constant (m s-2)
  L               = 2260               # [7] latent heat of vapour (kJ kg-1)
  GAMMA           = 66                 # [8] psychrometric constant (Pa K-1)
  TR0             = 524                # [9] reference temperature air (degrees Rankine) 
  REFLEgrass      = 0.10               # [10] albedo vegetation (-)
  Schmidt         = 0.61               # [11] Schmidt number, dimensionless constant for 
                                       #      calculation of the Grashof number
  Rwater          = 461.495            # [12] specific gas constant water vapour (J kg-1 K-1)
  Cp              = 1.005              # [13] specific heat of air (J kg-1 K-1)
  P               = 101325             # [14] standard air pressure at sea level (Pa)
  MuSt            = 1.827 * 10^(-5)    # [15] standard air viscosity (N s-1 m-2)
  SIGMA           = 5.67037 * 10^-8    # [16] Stefan-Boltzmann constant (W m-2 K-4)
  ST              = 120                # [17] Sutherlands constant in standard air (degrees 
                                       #      Rankine) for calculation air viscosity    
  Rdair           = 287.058            # [18] universal gas constant (J kg-1 K-1)
  CALTOJOULE      = 4.184              # [19] conversion factor from calories to joules
  NtoCP           = 6.25               # [20] conversion from N to crude protein
  GECARB          = 17.4               # [21] gross energy carbohydrates, combustion value 
                                       #      (MJ kg-1 DM) 
  GEFEED          = 18.5               # [22] gross energy feed types in general, combustion 
                                       #      value (MJ kg-1 DM)  
  GELIPID         = 39.6               # [23] gross energy lipid, combustion value (MJ kg-1 
                                       #      DM) (Emmans, 1994)
  GEPROT          = 23.8               # [24] gross energy protein, combustion value (MJ kg-1 
                                       #      DM) (Emmans, 1994)
  REFLEconcr      = 0.50               # albedo of a feedlot made of concrete (-)
        
  ###########################################################################################
  
  # Parameters for cattle
  # Numbers between brackets indicate parameter numbers as given in Table S6 of the 
  # Supplementary material of Van der Linden et al. (2019, Animal 13:845-855, see
  # https://doi.org/10.1017/S1751731118001726)
      
  CoatConst       = 1.90 * 10^(-5)      # [9] constant (m) (McGovern and Bruce, 2000)
  ZC              = 11000               # [10] coat resistance (s m-2) (McGovern and Bruce, 
                                        # 2000)
  TbodyC          = 40                  # [8] body temperature animal (degrees Celsius) 
                                        # (McGovern and Bruce, 2000) 
  LASMIN          = 10                  # [21] minimum latent heat release skin (W m-2) 
                                        # (Turnpenny et al., 2000a; Turnpenny et al., 2000b)
  PHFEEDCAP       = 110 #123            # [27] maximum feed intake of reference grass (g DM 
                                        # kg TBW-0.75) (Estimated from Jarrige et al., 1986) 
  
  if(BREED==2) PHFEEDCAP <- 160  # Feed intake capacity, expressed in fill units, can be made
  if(BREED==4) PHFEEDCAP <- 171  # breed-specific if literature indicates the intake capacity
                                 # is different from the standard intake capacity.
  
  PASSRED <- c(1,0.85,0.65,0.55) # Passage reduction factors (Chilibroste et al, 1997)
  
  RESPINCR        = 7.64                # [18] maximum increase in air exchange rate under 
                                        #      heat stress (Calculated from McGovern and 
                                        #      Bruce, 2000)
  PROTFRACBONE    = 0.23                # [48] protein fraction in bone (Field et al., 1974)
  PROTFRACMUSCLE  = 0.21                # [51] protein fraction in muscle (Consoleanu)
  LIPFRACMUSCLE   = 0.005               # [47] lipid fraction in muscle (Warren et al., 2008)
  PROTFRACFAT     = 0.08                # [49] protein fraction in fat tissue (Thonney, 2012)
  LIPFRACFAT      = 0.70                # [46] lipid fraction in fat tissue (Thonney, 2012)
  INCARC          = 0.50                # [35] fraction carcass at birth (estimate)
  RUMENFRAC       = 0.20                # [52] fraction rumen in total body weight (estimate)
  NEm             = pars[1]             # [78] NE for maintenance (kJ NE kg EBW-0.75, for B. 
                                        #      taurus cattle) (Ouellet et al, 1998)
  NEpha           = 70                  # [79] NE for physical activity (kJ NE kg EBW-0.75) 
                                        #      (CSIRO, 2007)
  BONEFRACMAX     = 0.25                # [64] maximum fraction bone in carcass (estimated 
                                        # from Berg and Butterfield, 1968)
  LIPNONCMAX      = 0.80                # [65] maximum fraction lipid accretion in the 
                                        # non-carcass tissue (assumption, resembles fat 
                                        # tissue)
  LIPNONCMIN      = 0.15                # [67] minimum fraction lipid accretion in the 
                                        #      non-carcass tissue (assumption) 
  PROTEFF         = 0.54                # [76] NE efficiency of protein accretion (MSU, 2014)
  LIPIDEFF        = 0.74                # [75] NE efficiency of lipid accretion (MSU, 2014)
  DERMPL          = 0.11                # [39] dermal protein loss protein (g kg-0.75 EBW 
                                        #      day-1) (CSIRO, 2007)
  PROTNE          = 2.0 / CALTOJOULE    # [88] protein requirement for NE (g MJ-1 NE) (CSIRO,
                                        #      2007)
  GestPer         = 276                 # [53] gestation period (days) (Blanc and Agabriel, 
                                        #      2008)
  
  # Parameters for cattle specific for LiGAPS-Dairy
  ALFA1           = 0.9                 # Curvature coefficient, see Table 2 of Johnson et 
                                        # al. (2016)
  ALFA2           = 0.6                 # Curvature coefficient, see Table 2 of Johnson et
                                        # al. (2016). Parameter value is calibrated.
  tmaxlact        = 80                  # Time to maximum intake during lactation, in days 
                                        # after parturition. See Table 2 of Johnson et al. 
                                        # (2019)
  fimaxlact       = 2.2                 # Multiplier maximum intake, see Eqs 20 and 25 of
                                        # Johnson et al. (2016) 
  BrevenLG        = 95                  # Time at which catabolism of fat tissues equals 
                                        # zero, expressed in days after parturition, see 
                                        # Table 2 of Johnson et al. (2019) 
  GE_FPCM         = 3.0933              # Gross energy content FPCM, MJ per kg (WLR, 2017)
  PROTFRACMILK1   = 5.00                # Parameters 1,2,3, and 4 are fitted with empirical
                                        # data of Meijer et al. (1998). Parameter values are 
  PROTFRACMILK2   = 1.25986             # used in Eq. 4 of the paper describing LiGAPS-Dairy.
  PROTFRACMILK3   = 0.11520
  PROTFRACMILK4   = 0.003839991
 
  ###########################################################################################
       
  # GESTINTERVAL and WEANINGTIME for calves
  # Experiment 22 and 63 have cows with multiple lactations
  
  # If there is no dry period, the weaning/lactation time equals the time between the start  
  # of the lactation and the end of the experiment.
  
  # If there is a dry period, the weaning/lactation time equals the time between the start of  
  # the first lactation and the end of the first lactation, the start of the second lactation 
  # and the end of the second lactation, etc.
  
  if(max(DIFFDAYS) <= DIFFDAYSMAX) WEANINGTIME = 
    MEASUREDOUTPUT$Time[length(MEASUREDOUTPUT$Time)] - MEASUREDOUTPUT$Time[1] + 1  
   if(max(DIFFDAYS) <= DIFFDAYSMAX) WEANINGTIMEvec <- NA
      if(max(DIFFDAYS) > DIFFDAYSMAX) WEANINGTIMEvec <- 
    c(MEASUREDOUTPUT$Time[DIFFDAYS>DIFFDAYSMAX],
      MEASUREDOUTPUT$Time[length(MEASUREDOUTPUT$Time)]) -
        c(MEASUREDOUTPUT$Time[1],MEASUREDOUTPUT$Time[DIFFDAYS2>DIFFDAYSMAX]) else 
          WEANINGTIMEvec <- 0
      
  if(max(DIFFDAYS) > DIFFDAYSMAX && length(WEANINGTIMEvec)>=3) 
    {TIMESDIFF <- MEASUREDOUTPUT$Time[2:length(MEASUREDOUTPUT$Time)]
    TIMESDIFF <- c(MEASUREDOUTPUT$Time[DIFFDAYS>DIFFDAYSMAX],NA)-
      c(NA,TIMESDIFF[DIFFDAYS>DIFFDAYSMAX])
    WEANINGTIMEvec[2] <- TIMESDIFF[2]
    if(length(WEANINGTIMEvec)>3) WEANINGTIMEvec[3] <- TIMESDIFF[3]}  
  
  # Number of lactations for an individual cow in an experiment      
  LACTATINNRexp[z] <-length(WEANINGTIMEvec)  
  
  ###########################################################################################
  
  GESTINTERVAL    = 365    # Minimum calving interval (days)
  # Length of the dry period between lactations is listed as a vector  
  if(max(DIFFDAYS) > DIFFDAYSMAX) GESTINTERVALvec <- c(DIFFDAYS[DIFFDAYS>DIFFDAYSMAX])
        
  ###########################################################################################    
      
  FtoConcW        = 75/45           # [37] conversion foetus weight to total concepta weight 
                                    #      (Jarrige et al., 1986, p. 99)   
  FATFACTOR       = 0.065           # [45] factor determing fat accretion ()
  RAINEXP         = 0.50            # [16] fraction animal area exposed to rain
  FRACVEG         = 0.50            # [17] fraction of the animal facing the vegetation in 
                                    #      free grazing systems
  COMPFACT        = 4               # [44] factor indicating the magnitude in compensatory 
                                    # growth (dimensionless)
  NEIEFFGEST      = 0.766           # [derived from 73] inefficiency of NE for gestation 
                                    # (1-efficiency) Calculated based on Jarrige, (1989) and 
                                    # Rattray et al. (1974)                          
  CPGEST          = 4.322           # [38] protein requirements for gestation (g protein MJ-1
                                    # NE)
  MILKDIG         = 0.95            # [40] digestible fraction of milk, based on energy 
                                    # content of milk
  NEEFFMILK       = pars[3]         # [74] efficiency of conversion of NE to milk (on an 
                                    # energy basis)
  PROTEFFMILK     = 0.68            # [82] protein efficiency for milk production (CSIRO, 
                                    # 2007)
  COMPFACTTIS     = 1.20            # maximum multiplicative for compensatory growth (set at
                                    # 120% of genetic potential)
  FATTISCOMP      = 0.80            # [36] if fat tissue is lower than 80% of the potential, 
                                    #      energy is allocated to the fat tissue for regain
                                    #      of body tissues
  TTDIGINSC       = 0.97            # [31] fraction total tract digestibility of insoluble, 
                                    #      non-structural carbohydrates (Moharrery et al, 
                                    #      2014)
  DETOME          = pars[2]         # [26] conversion from digestible energy (DE) to 
                                    # metabolisable energy (ME)
  DISSEFF         = 0.90            # [41] efficiency of dissimilation of protein and lipid
  RAINFRAC        = 0.3             # [derived from 22] 30% reduction in conductance due to 
                                    # rain/precipitation (Mount and Brown, 1982)
  BONEGROWTH1     = 0.6436          # [33] bone growth parameter (kg)
  if(BREED == 2) BONEGROWTH1     = 0.53   # The bone growth parameter can be adjusted to the
  if(BREED == 3) BONEGROWTH1     = 0.5868 # breed based on the literature.
  if(BREED == 4) BONEGROWTH1     = 0.50
  BONEGROWTH2     = 0.21            # [34] bone growth parameter (kg)
  MUSCLEGROWTH1   = -2*10^-5        # [68] muscle growth parameter
  MUSCLEGROWTH2   = 1.564           # [69] muscle growth parameter
  IMFGROWTH1      = 0.0001          # [56] intramuscular fat growth parameter
  IMFGROWTH2      = 0.01            # [57] intramuscular fat growth parameter
  IMFGROWTH3      = 0.04            # [58] intramuscular fat growth parameter
  PROTNONCM1      = -7.014*(10^-3)  # [80] max. protein content non-carcass 
  PROTNONCM2      = 20.4            # [81] max. protein content non-carcass
  RESPDUR         = 0.25            # [23] fraction day maximum respiration is used
  BODYAREA1       = 0.14            # [4] parameter to calculate body area (m-2)
  BODYAREA2       = 0.57            # [5] parameter to calculate body area (m-2)
  DIAMETER1       = 0.06            # [6] parameter to calculate body diameter (m-2)
  DIAMETER2       = 0.39            # [7] parameter to calculate body diameter (m-2)
  BASALRR1        = 73.8            # [1] basal respiration rate (min-1)
  BASALRR2        = -0.286          # [2] basal respiration rate
  BASALTV         = 0.0117          # [3] basal tidal volume (L min-1) 
  TEXHALED1       = 17              # [12] exhaled temperature (degrees Celsius)
  TEXHALED2       = 0.3             # [13] exhaled temperature
  TEXHALED3       = 0.01611         # [14] exhaled temperature
  TEXHALED4       = 0.0387          # [15] exhaled temperature
  MINCCS1         = 0.03            # [19] min. conductance core-skin (W m-2 K-1)
  MINCCS2         = 0.33            # [20] min. conductance core-skin (kg-1 total body weight)
  RAINEVAP1       = 0.15            # [11] evaporation rain from coat
  GEMILK1         = 5.50114         # [54] parameter 1 for gross energy of milk (kJ L-1)
  GEMILK2         = 1.79446         # [55] parameter 2 for gross energy of milk (kJ L-1)
  GEMILK3         = 0.10347         # parameter 3 for gross energy of milk 
  GEMILK4         = GEMILK3/60      # parameter 4 for gross energy of milk 
  GEMILK5         = 1.03            # parameter 5 - Density milk (kg per L)
  LIPBONE1        = 0.075           # [59] lipid fraction bone 
  LIPBONE2        = 3.0496          # [60] lipid fraction bone
  LIPBONE3        = 3.3268          # [61] lipid fraction bone
  LIPNONC1        = 4.7915*10^-7    # [62] lipid fraction non-carcass 
  LIPNONC2        = 0.00030         # [62] lipid fraction non-carcass 
  LIPNONC3        = 0.085717        # [62] lipid fraction non-carcass  
  LIPNONC4        = 2.1723          # [62] lipid fraction non-carcass
  PROTNONC1       = 8.7492*10^-10   # [83] protein fraction non-carcass  
  PROTNONC2       = 9.0732*10^-7    # [84] protein fraction non-carcass 
  PROTNONC3       = 0.00033117      # [85] protein fraction non-carcass 
  PROTNONC4       = 0.061756        # [86] protein fraction non-carcass 
  PROTNONC5       = 22.26           # [87] protein fraction non-carcass 
  RUMENDEV1       = 0.007246        # [29] parameter rumen development
  RUMENDEV2       = 0.101449        # [30] parameter rumen development
  NDFDIGEST       = 0.9             # [32] total tract DNDF digestibility
  NDFPASS         = 0.125           # [28] passage rate DNDF 
  LUCAS1          = 0.9             # [24] slope Lucas equation (g protein g-1 CP)
  LUCAS2          = 32              # [25] intercept Lucas equation (g CP kg-1 DM)
  ENNONC1         = 0.60            # [42] energy partitioning non-carcass 
  ENNONC2         = 0.03            # [43] energy partitioning non-carcass
  NRECYCL1        = 160             # [70] Parameter 1 N recycling 
  NRECYCL2        = 12.01           # [71] Parameter 2 N recycling
  NRECYCL3        = 0.3235          # [72] Parameter 3 N recycling
  
  ###########################################################################################
  
  # Gross energy content subcutaneous and intermuscular fat tissue (MJ kg-1 tissue)    
  GEFATTIS        = GEPROT * PROTFRACFAT + GELIPID * LIPFRACFAT
  # Gross energy content muscle tissue (MJ kg-1 tissue)
  GEMUSCLETIS     = GEPROT * PROTFRACMUSCLE + GELIPID * LIPFRACMUSCLE 
      
  ###########################################################################################
  #                                                                                         #                           
  #                 Greenhouse Gas Emissions from Livestock Production Systems              #
  #                                                                                         #                      
  ###########################################################################################
      
  # Project: FAO/WUR 'Investing in Sustainable Livestock' (P160021)
  # Aim:  The aim of this source code is to assess the GHG emissions from farming systems 
  # with dairy or beef cattle (up to the farm gate). Outcomes of GHG emissions are not 
  # presented in the paper describing LiGAPS-Dairy.

  # Equations in the source code are mainly derived from: 
  # '2006 IPCC Guidelines for National Greenhouse Gas Inventories', volume 4, Agriculture,  
  # Forestry and Other Land Use, Eds Simon Eggleston, Leandro Buendia, Kyoko Miwa, Todd Ngara,  
  # Kiyoto Tanabe, National Greenhouse Gas Inventories, Programme, IGES
  # Several Chapters have been updated since the publication
  # For updates, see: http://www.ipcc-nggip.iges.or.jp/public/2006gl/vol4.html
      
  # Main sources of GHG emissions from livestock systems
      
      # 1. Enteric methane
      # 2. Methane from manure management
      # 3. Direct N20 emissions from managed soils
      # 4. Direct N20 emissions from manure management
      # 5. Indirect N20 emissions from manure management
      # 6. Indirect N20 emissions from leaching
      # 7. Emissions related to production of farm inputs
      # 8. Emissions arable crops used as feed
      # 9. Emissions from crop residues
      #10. CO2 emissions from liming
  
  ########################################################################################### 
  # Module for the assessment of greenhouse gas emissions related to livestock production   #
  #                                                                                         #
  # Processes included:                                                                     #
  # * Enteric methane emission                                                              #
  # * Methane from manure management                                                        #
  # * N20 from manure management                                                            #
  # This section specifies the parameter values from an Excel file                          #
  ###########################################################################################
      
  # The directory of the Excel file must correspond to the directory specified below 
  INPUTDATA <- read.csv("M:/My Documents/PostDoc Investing in Sustainable Livestock/Results/Input_Ethiopia.csv", head=T)
   
  # Table numbers and chapters referred to are from IPCC (2006)   
  B0          = INPUTDATA[1,2]  # volative solid excretion (m3 CH4 kg-1)  
  MCFgr       = INPUTDATA[2,2]  # Consult Table 10.17 or 10A-5
  MCFfl       = INPUTDATA[3,2]  # Consult Table 10.17 or 10A-5
  MS          = INPUTDATA[4,2]  # Consult Table 10A-5, can be different for different systems
  EF3         = INPUTDATA[5,2]  # IPCC, Chapter 11, Table 11.1
  FracgasMS   = INPUTDATA[6,2]  # fraction managed manure nitrogen volatilised, IPCC, Chapter 
                                # 11, Table 11.3
  FracleachMS = INPUTDATA[7,2]  # fraction managed manure nitrogen leached
  GE          = INPUTDATA[8,2]
  UE          = INPUTDATA[9,2]  # 0.02 for ruminants fed more than 85% grains
  ASH         = INPUTDATA[10,2]
  CH4c        = INPUTDATA[11,2]
  Ncon        = INPUTDATA[12,2] # conversion kg N2O to N 
  EF4         = INPUTDATA[13,2]
  EF5         = INPUTDATA[14,2]
  CH4MJtoKG   = INPUTDATA[15,2] # Conversion factor from energy to mass
  GWPCH4      = INPUTDATA[16,2] # IPCC, 2013
  GWPN2O      = INPUTDATA[17,2] # 298
  Type        = INPUTDATA[18,2] # 1 = beef cattle; 2 = dairy cattle
  
  ENTCH4PAR1beef <- INPUTDATA[19,2]  # Based on Ellis et al. (2006)
  ENTCH4PAR2beef <- INPUTDATA[20,2]  # Based on Ellis et al. (2006)
  ENTCH4PAR3beef <- INPUTDATA[21,2]  # Based on Ellis et al. (2006)
  ENTCH4PAR1dairy <- INPUTDATA[22,2] # Based on Ellis et al. (2006)
  ENTCH4PAR2dairy <- INPUTDATA[23,2] # Based on Ellis et al. (2006)
  ENTCH4PAR3dairy <- INPUTDATA[24,2] # Based on Ellis et al. (2006)
  
  # Selection of parameters for either dairy or beef cattle.    
  if(Type==1) ENTCH4PAR1 <- ENTCH4PAR1beef else if(Type==2) ENTCH4PAR1 <- ENTCH4PAR1dairy  
  if(Type==1) ENTCH4PAR2 <- ENTCH4PAR2beef else if(Type==2) ENTCH4PAR2 <- ENTCH4PAR2dairy
  if(Type==1) ENTCH4PAR3 <- ENTCH4PAR3beef else if(Type==2) ENTCH4PAR3 <- ENTCH4PAR3dairy
  
  # End of the section on greenhouse gas emissions.
  

  ###########################################################################################
  # 1.4                             Specification of variables                              #
  ###########################################################################################

  ###########################################################################################
  # 1.4.1     Specification of variables for the feed intake and digestion sub-model        #
  ###########################################################################################
  
  # Specification of available feed quantity, in kg DM per day per feed type
  FEED1QNTY <- matrix(nrow=length(DOY), ncol = jmax, byrow = F, rep(FEED1QNTY,jmax))
  FEED1QNTY <- FEED1QNTY[1:imax,] 
      
  FEED2QNTY <- matrix(nrow=length(DOY), ncol = jmax, byrow = F, rep(FEED2QNTY,jmax))
  FEED2QNTY <- FEED2QNTY[1:imax,]
      
  FEED3QNTY <- matrix(nrow=length(DOY), ncol = jmax, byrow = F, rep(FEED3QNTY,jmax))
  FEED3QNTY <- FEED3QNTY[1:imax,]
                     
  FEED4QNTY <- c(rep(rep( 0.0,imax),jmax))  
      
  FEED1QNTY <- matrix(nrow=imax, ncol=jmax, FEED1QNTY) 
  FEED2QNTY <- matrix(nrow=imax, ncol=jmax, FEED2QNTY) 
  FEED3QNTY <- matrix(nrow=imax, ncol=jmax, FEED3QNTY) 
  FEED4QNTY <- matrix(nrow=imax, ncol=jmax, FEED4QNTY) 
    
  # Total available feed quantity, sum of all feed types (kg DM per animal per day)
  FEEDQNTY <- FEED1QNTY + FEED2QNTY + FEED3QNTY + FEED4QNTY
      
  # Maximum fraction of a feed type in the diet (-)
  FRACFEED1 <- matrix(nrow=imax, ncol=jmax)
  FRACFEED2 <- matrix(nrow=imax, ncol=jmax)
  FRACFEED3 <- matrix(nrow=imax, ncol=jmax)
  FRACFEED4 <- matrix(nrow=imax, ncol=jmax)
      
  # Feed intake based on fill units (kg DM per animal per day)
  FEED1QNTYA <- matrix(nrow=imax, ncol=jmax) 
  FEED2QNTYA <- matrix(nrow=imax, ncol=jmax) 
  FEED3QNTYA <- matrix(nrow=imax, ncol=jmax) 
  FEED4QNTYA <- matrix(nrow=imax, ncol=jmax) 
      
  PASSDIFF <- matrix(nrow=imax, ncol=jmax)
      
  # Specification of variables in the feed digestion sub-model of Chilibroste et al. (1997) 
  PENDF           <- matrix(nrow=imax, ncol=jmax)
  Digestfracfeed  <- matrix(nrow=imax, ncol=jmax)
  INSC            <- matrix(nrow=imax, ncol=jmax)
  INSCTOTAL       <- matrix(nrow=imax, ncol=jmax)
  INSCDIG         <- matrix(nrow=imax, ncol=jmax)
  INSCINT         <- matrix(nrow=imax, ncol=jmax)
  INSCINTDIG      <- matrix(nrow=imax, ncol=jmax)
  NDF             <- matrix(nrow=imax, ncol=jmax)
  NDFTOTAL        <- matrix(nrow=imax, ncol=jmax)
  NDFDIG          <- matrix(nrow=imax, ncol=jmax)
  NDFINT          <- matrix(nrow=imax, ncol=jmax)
  NDFINTDIG       <- matrix(nrow=imax, ncol=jmax)
  NDFINTDIGTOT    <- matrix(nrow=imax, ncol=jmax)
  PICP            <- matrix(nrow=imax, ncol=jmax)
  PROTTOTAL       <- matrix(nrow=imax, ncol=jmax)
  PROTINT         <- matrix(nrow=imax, ncol=jmax)
  PROTUPT         <- matrix(nrow=imax, ncol=jmax)
  PROTEXCR        <- matrix(nrow=imax, ncol=jmax)
  PROTDIGRU       <- matrix(nrow=imax, ncol=jmax)
  PROTDIGWT       <- matrix(nrow=imax, ncol=jmax)
  PROTBAL         <- matrix(nrow=imax, ncol=jmax)
  PROTREDFACT     <- matrix(nrow=imax, ncol=jmax)
  DIGFRAC         <- matrix(nrow=imax, ncol=jmax)
  CHEXCR          <- matrix(nrow=imax, ncol=jmax)
  EXCRFRAC        <- matrix(nrow=imax, ncol=jmax)
  GEEXCR          <- matrix(nrow=imax, ncol=jmax)
  GEUPTAKE        <- matrix(nrow=imax, ncol=jmax)
  MEUPTAKE        <- matrix(nrow=imax, ncol=jmax)
  ENDIGEST        <- matrix(nrow=imax, ncol=jmax)
  PHFEEDINT       <- matrix(nrow=imax, ncol=jmax)
  PHFEEDINTKG     <- matrix(nrow=imax, ncol=jmax)
  PASSAGE         <- matrix(nrow=imax, ncol=jmax)
  PASSAGE1        <- matrix(nrow=imax, ncol=jmax)
  FUFEED1         <- matrix(nrow=imax, ncol=jmax)
  FUFEED2         <- matrix(nrow=imax, ncol=jmax)
  FUFEED3         <- matrix(nrow=imax, ncol=jmax)
  FUFEED4         <- matrix(nrow=imax, ncol=jmax)
  AVGDIGFRAC      <- matrix(nrow=imax, ncol=jmax)
  MEDAILYMAX      <- matrix(nrow=imax, ncol=jmax)
  MEDIGLIMGR      <- matrix(nrow=imax, ncol=jmax) 
  FEEDINTAKE      <- matrix(nrow=imax, ncol=jmax)
  FILLGIT         <- matrix(nrow=imax, ncol=jmax)
  ADGHIGH         <- matrix(nrow=imax+1, ncol=jmax)
  ADGHIGH[1,1:jmax] <- 0 # Initial values set to zero
      
      
  ###########################################################################################
  # 1.4.2         Specification of variables for the thermoregulation sub-model             #
  ###########################################################################################
      
  # 1. Respiration
  TBW          = matrix(nrow=imax+1, ncol=jmax)
  AREA         = matrix(nrow=imax, ncol=jmax)
  DIAMETER     = matrix(nrow=imax, ncol=jmax)
  LENGTH       = matrix(nrow=imax, ncol=jmax)
  brr          = matrix(nrow=imax, ncol=jmax)
  btv          = matrix(nrow=imax, ncol=jmax)
  Vtb          = matrix(nrow=imax, ncol=jmax)
  brv          = matrix(nrow=imax, ncol=jmax)
  irv          = matrix(nrow=imax, ncol=jmax)
  TAVGC        = matrix(nrow=imax, ncol=jmax)
  TAVGK        = matrix(nrow=imax, ncol=jmax)
  VPSATAIR     = matrix(nrow=imax, ncol=jmax) 
  VPAIRTOT     = matrix(nrow=imax, ncol=jmax)
  RHAIR        = matrix(nrow=imax, ncol=jmax)
  RHOVP        = matrix(nrow=imax, ncol=jmax)
  RHODAIR      = matrix(nrow=imax, ncol=jmax)
  RHOAIR       = matrix(nrow=imax, ncol=jmax)
  CHIAIR       = matrix(nrow=imax, ncol=jmax)
  VISCAIR      = matrix(nrow=imax, ncol=jmax)
  Texh         = matrix(nrow=imax, ncol=jmax)
  VPSATAIROUT  = matrix(nrow=imax, ncol=jmax)
  RHOVPOUT     = matrix(nrow=imax, ncol=jmax)
  RHODAIROUT   = matrix(nrow=imax, ncol=jmax)
  RHOAIROUT    = matrix(nrow=imax, ncol=jmax)
  CHIAIROUT    = matrix(nrow=imax, ncol=jmax)
  AIREXCH      = matrix(nrow=imax, ncol=jmax)
  LHEATRESP    = matrix(nrow=imax, ncol=jmax)
  CHEATRESP    = matrix(nrow=imax, ncol=jmax)
  TGRESP       = matrix(nrow=imax, ncol=jmax)
  TNRESP       = matrix(nrow=imax, ncol=jmax)
  TNRESPH      = matrix(nrow=imax, ncol=jmax)
  NERESP       = matrix(nrow=imax, ncol=jmax)
  NERESPWM     = matrix(nrow=imax, ncol=jmax)
  NERESPC      = matrix(nrow=imax, ncol=jmax)
  MetheatSKIN  = matrix(nrow=imax, ncol=jmax)
  TskinC       = matrix(nrow=imax, ncol=jmax)
  TskinCH      = matrix(nrow=imax, ncol=jmax)
  CBSMIN       = matrix(nrow=imax, ncol=jmax)
  CONDBS       = matrix(nrow=imax, ncol=jmax)
      
  # 2. Sweating 
  DLC          = matrix(nrow=imax, ncol=jmax)
  DIFFC        = matrix(nrow=imax, ncol=jmax)
  RV           = matrix(nrow=imax, ncol=jmax)
  VPSKINTOT    = matrix(nrow=imax, ncol=jmax)
  LASMAXENV    = matrix(nrow=imax, ncol=jmax)
  LASMAXPHYS   = matrix(nrow=imax, ncol=jmax)
  LASMAXCORR   = matrix(nrow=imax, ncol=jmax)
  ACTSW        = matrix(nrow=imax, ncol=jmax)
  ACTSWH       = matrix(nrow=imax, ncol=jmax)
  CSC          = matrix(nrow=imax, ncol=jmax)
  MetheatCOAT  = matrix(nrow=imax, ncol=jmax)
  TcoatC       = matrix(nrow=imax, ncol=jmax)
  TcoatCH      = matrix(nrow=imax, ncol=jmax)
  TcoatK       = matrix(nrow=imax, ncol=jmax)
  QSC          = matrix(nrow=imax, ncol=jmax)
      
  # 3.LWR heat balance of the coat
  LWRSKY       = matrix(nrow=imax, ncol=jmax)
  LWRENV       = matrix(nrow=imax, ncol=jmax)
  LB           = matrix(nrow=imax, ncol=jmax)
  LWRCOAT      = matrix(nrow=imax, ncol=jmax)
  LWRCOATH     = matrix(nrow=imax, ncol=jmax)
      
  # 4.Convective heat losses from the coat
  TAVGR        = matrix(nrow=imax, ncol=jmax)
  Ea           = matrix(nrow=imax, ncol=jmax)                                        
  Ec           = matrix(nrow=imax, ncol=jmax)                      
  GRASHOF      = matrix(nrow=imax, ncol=jmax)
  WINDSP       = matrix(nrow=imax, ncol=jmax)
  REYNOLDS     = matrix(nrow=imax, ncol=jmax)
  ReH          = matrix(nrow=imax, ncol=jmax)                                                                 
  ReL          = matrix(nrow=imax, ncol=jmax)
  NUSSELTH     = matrix(nrow=imax, ncol=jmax)
  NUSSELTL     = matrix(nrow=imax, ncol=jmax)
  NUSSELT      = matrix(nrow=imax, ncol=jmax)
  NUSSELTM     = matrix(nrow=imax, ncol=jmax)
  ka           = matrix(nrow=imax, ncol=jmax)
  CONVCOAT     = matrix(nrow=imax, ncol=jmax)
  CONVCOATH    = matrix(nrow=imax, ncol=jmax)
      
  # 5. Incoming SWR (solar radiation) to coat
  SAAC         = matrix(nrow=imax, ncol=jmax)
  SWRS         = matrix(nrow=imax, ncol=jmax)
  SWRC         = matrix(nrow=imax, ncol=jmax)
  ISWRC        = matrix(nrow=imax, ncol=jmax)
  REFLE        = NULL
  SWR          = matrix(nrow=imax, ncol=jmax)
  RAINEVAP     = matrix(nrow=imax, ncol=jmax)
      
  # Synthesis and optimization with repeat {} function
  MetheatBAL   = matrix(nrow=imax, ncol=jmax)
  Metheatopt   = matrix(nrow=imax, ncol=jmax)
  # Inital heat production is set at 100 first, and is adjusted later in the code.
  METABFEED0   = matrix(rep(100,imax*jmax),nrow=imax, ncol=jmax)
      
      
  ###########################################################################################
  # 1.4.3     Specification of variables for the energy and protein utilization model       #
  ###########################################################################################
      
  # Synthesis and optimisation
  REDHP        = matrix(nrow=imax, ncol=jmax)
      
  # Weight and derivative body tissues 
  TBWCHECK     = matrix(nrow=imax+1, ncol=jmax)
  CARCW        = matrix(nrow=imax+1, ncol=jmax)
  BONETIS      = matrix(nrow=imax+1, ncol=jmax)
  MUSCLETIS    = matrix(nrow=imax+1, ncol=jmax)
  INTRAMFTIS   = matrix(nrow=imax+1, ncol=jmax)
  MISCFATTIS   = matrix(nrow=imax+1, ncol=jmax)
  NONCARCTIS   = matrix(nrow=imax+1, ncol=jmax)
  RUMEN        = matrix(nrow=imax+1, ncol=jmax)
      
  DERBONE      = matrix(nrow=imax+1, ncol=jmax)
  DERMUSCLE    = matrix(nrow=imax+1, ncol=jmax)
  DERINTRAMF   = matrix(nrow=imax+1, ncol=jmax)
  DERMISCFAT   = matrix(nrow=imax+1, ncol=jmax)
  DERNONC      = matrix(nrow=imax+1, ncol=jmax)
  DERRUMEN     = matrix(nrow=imax+1, ncol=jmax)
  DERTOTAL     = matrix(nrow=imax+1, ncol=jmax)
      
  # Lipid and protein concentrations in body tissues
  LIPFRACBONE     = matrix(nrow=imax+1, ncol=jmax)
  LIPFRACBONEACT  = matrix(nrow=imax+1, ncol=jmax)
  LIPFRACNONC     = matrix(nrow=imax+1, ncol=jmax)
  LIPFRACNONCACT  = matrix(nrow=imax+1, ncol=jmax)
  PROTFRACNONC    = matrix(nrow=imax+1, ncol=jmax)
  PROTFRACNONCACT = matrix(nrow=imax+1, ncol=jmax)
      
  ENFEEDGROWTH    = matrix(nrow=imax+1, ncol=jmax)
  ENFEEDGROWTHQ   = matrix(nrow=imax+1, ncol=jmax)
      
  # Bone carcass
  LIPIDBONE    = matrix(nrow=imax+1, ncol=jmax)
  PROTBONE     = matrix(nrow=imax+1, ncol=jmax)
  ENGRBONE     = matrix(nrow=imax+1, ncol=jmax)
      
  # Muscle carcass
  LIPIDMUSCLE  = matrix(nrow=imax+1, ncol=jmax)
  PROTMUSCLE   = matrix(nrow=imax+1, ncol=jmax)
  ENGRMUSCLE   = matrix(nrow=imax+1, ncol=jmax)
      
  # Intramuscular fat
  LIPIDIMF     = matrix(nrow=imax+1, ncol=jmax)               
  PROTIMF      = matrix(nrow=imax+1, ncol=jmax)
  ENGRIMF      = matrix(nrow=imax+1, ncol=jmax)
      
  # Subcutaneous and intermuscular fat
  LIPIDFAT     = matrix(nrow=imax+1, ncol=jmax)                
  PROTFAT      = matrix(nrow=imax+1, ncol=jmax)
  ENGRFAT      = matrix(nrow=imax+1, ncol=jmax)
      
  # Non carcass tissue
  LIPIDNONC    = matrix(nrow=imax+1, ncol=jmax)                 
  PROTNONC     = matrix(nrow=imax+1, ncol=jmax)
  ENGRNONC     = matrix(nrow=imax+1, ncol=jmax)
      
  # Actual growth
  ENGRNONCACT   =   matrix(nrow=imax+1, ncol=jmax)
  ENGRBONEACT   =   matrix(nrow=imax+1, ncol=jmax)
  ENGRIMFACT    =   matrix(nrow=imax+1, ncol=jmax)
  ENGRMUSCLEACT =   matrix(nrow=imax+1, ncol=jmax)
  ENGRFATACT    =   matrix(nrow=imax+1, ncol=jmax)
  ENGRTOTAL     =   matrix(nrow=imax+1, ncol=jmax)
  ENGRTOTALHIGH =   matrix(nrow=imax+1, ncol=jmax)
  ENGRTOTALHIGH[1,1:jmax] <- 0 # Initial values set to zero
  ENGRTOTALHIGH1 =  matrix(nrow=imax+1, ncol=jmax)
  ENGRTOTALHIGH1[1,1:jmax] <- 0 # Initial values set to zero
  REL           =   matrix(nrow=imax+1, ncol=jmax)
  REL[1,1:jmax] <- 0 # Initial value set to zero
  ENGRTOTALORIG =   matrix(nrow=imax+1, ncol=jmax)
      
  FRENGRNONCACT   = matrix(nrow=imax+1, ncol=jmax)
  FRENGRBONEACT   = matrix(nrow=imax+1, ncol=jmax)
  FRENGRIMFACT    = matrix(nrow=imax+1, ncol=jmax)
  FRENGRMUSCLEACT = matrix(nrow=imax+1, ncol=jmax)
  FRENGRFATACT    = matrix(nrow=imax+1, ncol=jmax)
  FRENGRTOTAL     = matrix(nrow=imax+1, ncol=jmax)
      
  BONETISACT      = matrix(nrow=imax+1, ncol=jmax)
  MUSCLETISACT    = matrix(nrow=imax+1, ncol=jmax)
  INTRAMFTISACT   = matrix(nrow=imax+1, ncol=jmax)
  MISCFATTISACT   = matrix(nrow=imax+1, ncol=jmax)
  NONCARCTISACT   = matrix(nrow=imax+1, ncol=jmax)
  TBWACT          = matrix(nrow=imax+1, ncol=jmax)
  EBWACTMET       = matrix(nrow=imax+1, ncol=jmax)
      
  MISCFATFRAC     = matrix(nrow=imax+1, ncol=jmax)
  LIPIDBONEACT    = matrix(nrow=imax+1, ncol=jmax)
  LIPIDNONCACT    = matrix(nrow=imax+1, ncol=jmax)
  PROTNONCACT     = matrix(nrow=imax+1, ncol=jmax)
  LIPIDMUSCLEACT  = matrix(nrow=imax+1, ncol=jmax)
  LIPIDIMFACT     = matrix(nrow=imax+1, ncol=jmax)
  LIPIDFATACT     = matrix(nrow=imax+1, ncol=jmax)
  LIPIDTOTW       = matrix(nrow=imax+1, ncol=jmax)
  LIPIDPERCCARC   = matrix(nrow=imax+1, ncol=jmax)
      
  ENCONTENTNONCACT = matrix(nrow=imax+1, ncol=jmax)
      
  # Maintenance
  NEMAINT     = matrix(nrow=imax, ncol=jmax)
  NEMAINTWM   = matrix(nrow=imax, ncol=jmax)
  PROTDERML   = matrix(nrow=imax, ncol=jmax)
  PROTMAINT   = matrix(nrow=imax, ncol=jmax)
  PROTRESP    = matrix(nrow=imax, ncol=jmax)
      
  # Physical activity
  NEPHYSACT   = matrix(nrow=imax, ncol=jmax)
  NEPHYSACTWM = matrix(nrow=imax, ncol=jmax)
  PROTPHACT   = matrix(nrow=imax, ncol=jmax)
      
  # Gestation
  CALFTBW   = matrix(nrow=imax+1, ncol=jmax)
  CALFNR    = matrix(nrow=imax+1, ncol=jmax)
  BIRTHW1   = matrix(nrow=imax, ncol=jmax)
      
  GEST1 = matrix(nrow=imax, ncol=jmax)
  GEST2 = matrix(nrow=imax, ncol=jmax)
  GEST3 = matrix(nrow=imax, ncol=jmax)
  GEST4 = matrix(nrow=imax, ncol=jmax)
  GEST5 = matrix(nrow=imax, ncol=jmax)
  GEST6 = matrix(nrow=imax+1, ncol=jmax)
  GEST  = matrix(nrow=imax, ncol=jmax) 
      
  GESTDAY         = matrix(nrow=imax+1, ncol=jmax)
  NEREQGEST       = matrix(nrow=imax, ncol=jmax)
  NEREQGESTADD    = matrix(nrow=imax+1, ncol=jmax)
  NEREQGESTTOT    = matrix(nrow=imax, ncol=jmax)
  HEATGEST        = matrix(nrow=imax, ncol=jmax)
  PROTGESTG       = matrix(nrow=imax, ncol=jmax)
  TBWADD          = matrix(nrow=imax+1, ncol=jmax)
      
  # Milk production
  MILKDAYST       = matrix(nrow=imax, ncol=jmax)
  MILKDAY         = matrix(nrow=imax+1, ncol=jmax)
  MILKWEEK        = matrix(nrow=imax+1, ncol=jmax)
  ADDMILK1        = matrix(nrow=imax, ncol=jmax)
  ADDMILK2        = matrix(nrow=imax, ncol=jmax)
      
  POTMILKPROD     = matrix(nrow=imax, ncol=jmax)
  GEMILK          = matrix(nrow=imax, ncol=jmax)
  GEMILKTOT       = matrix(nrow=imax, ncol=jmax)
  MEMILKCALF      = matrix(nrow=imax, ncol=jmax)
  MEMILKCALFINIT  = matrix(nrow=imax, ncol=jmax)
  NEMILKCOW       = matrix(nrow=imax, ncol=jmax)
  CALFLIVENR      = matrix(nrow=imax, ncol=jmax)
  CALFWEANNR      = matrix(nrow=imax, ncol=jmax)
  MILKPRODACT     = matrix(nrow=imax, ncol=jmax)
  HEATMILK        = matrix(nrow=imax, ncol=jmax)
  NETMILKEN       = matrix(nrow=imax+1, ncol=jmax) 
  PROTMILK        = matrix(nrow=imax, ncol=jmax)
  PROTMILKG       = matrix(nrow=imax, ncol=jmax)
  
  # Variables on milk production specific for LiGAPS-Dairy
  SFFeedIntG       = matrix(nrow=imax, ncol=jmax)
  ALFA             = matrix(nrow=imax, ncol=jmax)
  SFFeedIntL       = matrix(nrow=imax, ncol=jmax)
  RatioLG          = matrix(nrow=imax, ncol=jmax)
  ENFEEDLACT       = matrix(nrow=imax, ncol=jmax)
  ENFEEDLACT[1,] <- rep(0,jmax)
  ENFEEDVAR        = matrix(nrow=imax, ncol=jmax)
  RLG              = matrix(nrow=imax, ncol=jmax)
  MILKPRODACTFPCM  = matrix(nrow=imax, ncol=jmax)
  MILKPRODPOTFPCM  = matrix(nrow=imax, ncol=jmax)
  PROTFRACMILK     = matrix(nrow=imax, ncol=jmax)
  PROTREDFACTMILK  = matrix(nrow=imax, ncol=jmax)
  NUE              = NULL
  
  # ME total
  MEREQTOTAL       = matrix(nrow=imax, ncol=jmax)
  NETMILKEN        = matrix(nrow=imax, ncol=jmax)
      
  # Cold stress
  Metheatcold      = matrix(rep(NA,imax*jmax), nrow=imax, ncol=jmax)
  # Inital heat production under cold stress is set at 100 first, and is adjusted later in 
  # the code.
  METABSTARTCOLD   = matrix(rep(100,imax*jmax),nrow=imax, ncol=jmax)
  TOTHEAT          = matrix(nrow=imax, ncol=jmax)
  FATBURN          = matrix(nrow=imax, ncol=jmax)
  REDTIS2          = matrix(nrow=imax, ncol=jmax)
  REDTIS3          = matrix(nrow=imax, ncol=jmax)
  MAINTFRAC        = matrix(nrow=imax, ncol=jmax)
  FATFRACCARC      = matrix(nrow=imax, ncol=jmax)
      
  ###########################################################################################
  # 1.4.4                    Variables for integration between sub-models                   #
  ###########################################################################################
      
  # Specification of the variables for integration of sub-models
  COMPGROWTH     = matrix(nrow=imax+1, ncol=jmax)
  COMPGROWTH1    = matrix(nrow=imax+1, ncol=jmax)
  COMPGROWTH2    = matrix(nrow=imax+1, ncol=jmax)
  COMPGROWTH3    = matrix(nrow=imax+1, ncol=jmax)
  COMPGROWTH4    = matrix(nrow=imax+1, ncol=jmax)
  COMPGROWTH5    = matrix(nrow=imax+1, ncol=jmax)
      
  ENGRTOTALCOMP  = matrix(nrow=imax+1, ncol=jmax)
      
  HEATBONEACT    = matrix(nrow=imax+1, ncol=jmax)
  HEATMUSCLEACT  = matrix(nrow=imax+1, ncol=jmax)
  HEATIMFACT     = matrix(nrow=imax+1, ncol=jmax)
  HEATMISCFATACT = matrix(nrow=imax+1, ncol=jmax)
  HEATNONCACT    = matrix(nrow=imax+1, ncol=jmax)
      
  HEATTOTALACT   = matrix(nrow=imax+1, ncol=jmax)
      
  ENBONEACT      = matrix(nrow=imax+1, ncol=jmax)
  ENMUSCLEACT    = matrix(nrow=imax+1, ncol=jmax)
  ENIMFACT       = matrix(nrow=imax+1, ncol=jmax)
  ENMISCFATACT   = matrix(nrow=imax+1, ncol=jmax)
  ENNONCACT      = matrix(nrow=imax+1, ncol=jmax)
  ENTOTALACT     = matrix(nrow=imax+1, ncol=jmax)
      
  PROTBONEACT    = matrix(nrow=imax+1, ncol=jmax)
  PROTMUSCLEACT  = matrix(nrow=imax+1, ncol=jmax)
  PROTIMFACT     = matrix(nrow=imax+1, ncol=jmax)
  PROTMISCFATACT = matrix(nrow=imax+1, ncol=jmax)
  PROTNONCACT1   = matrix(nrow=imax+1, ncol=jmax)
  PROTTOTALACT   = matrix(nrow=imax+1, ncol=jmax)
  PROTGROSS      = matrix(nrow=imax+1, ncol=jmax)
  NRECYCLPT      = matrix(nrow=imax+1, ncol=jmax)
  PROTNETT       = matrix(nrow=imax+1, ncol=jmax)
  PROTACCR       = matrix(nrow=imax+1, ncol=jmax)
  PROTLW         = matrix(nrow=imax+1, ncol=jmax)
  PROTBEEF       = matrix(nrow=imax+1, ncol=jmax)
      
  HEATCLIMGEN    = matrix(nrow=imax+1, ncol=jmax)
  DIFFEN         = matrix(nrow=imax+1, ncol=jmax)
      
  HEATIFEEDMAINT     = matrix(nrow=imax, ncol=jmax)
  HEATIFEEDMAINTWM   = matrix(nrow=imax, ncol=jmax)
  HEATIFEEDGROWTH    = matrix(nrow=imax, ncol=jmax)
  HEATIFEEDGROWTHWM  = matrix(nrow=imax, ncol=jmax)
  HEATIFEEDGROWTHC   = matrix(nrow=imax, ncol=jmax)
  HEATIFEEDGROWTHCWM = matrix(nrow=imax, ncol=jmax)
  REDMAINT           = matrix(nrow=imax, ncol=jmax)
  REDMAINT2          = matrix(nrow=imax, ncol=jmax)
  REDMAINT3          = matrix(nrow=imax, ncol=jmax)
  REDTIS             = matrix(nrow=imax, ncol=jmax)
  REDTISPROT         = matrix(nrow=imax, ncol=jmax)
  CHECK              = matrix(nrow=imax, ncol=jmax)
      
  ###########################################################################################
  # 1.4.5                     Variables for herd dynamics and output                        #
  ###########################################################################################
      
  TIME       = matrix(nrow=imax, ncol=jmax)
  TIME2      = matrix(nrow=imax+1, ncol=jmax)
    
  TIMEYEAR   = matrix(nrow=imax, ncol=jmax)
  TIMEYEAR2  = matrix(nrow=imax+1, ncol=jmax)
      
  BIRTHDAYCALF1 = 1 # Initial values for days of birth for calves, calculated as days after 
  BIRTHDAYCALF2 = 1 # birth reproductive animal. Values are recalculated elsewhere in the 
  BIRTHDAYCALF3 = 1 # source code.
  BIRTHDAYCALF4 = 1
  BIRTHDAYCALF5 = 1
  BIRTHDAYCALF6 = 1
  BIRTHDAYCALF7 = 1
  BIRTHDAYCALF8 = 1
  BIRTHDAYCALF9 = 1
  BIRTHDAY = NULL
  WNDAY  = NULL
      
  # Birth of calves and parity. 
  
  # Reproductive cow in first parity (1= at least one calf, 0 = no calf)    
  PARITY1 = matrix(nrow=imax, ncol=jmax)  
  # Reproductive cow in second parity (1= at least two calves, 0 = less than two calves)
  PARITY2 = matrix(nrow=imax, ncol=jmax)  
  # Reproductive cow in third parity (1= at least three calves, 0 = less than three calves)
  PARITY3 = matrix(nrow=imax, ncol=jmax)  
  # Reproductive cow in fourth parity (1= at least four calves, 0 = less than four calves)
  PARITY4 = matrix(nrow=imax, ncol=jmax)  
  # Reproductive cow in fifth parity (1= at least five calves, 0 = less than five calves)
  PARITY5 = matrix(nrow=imax, ncol=jmax)  
  # Reproductive cow in sixth parity (1= at least six calves, 0 = less than six calves)
  PARITY6 = matrix(nrow=imax, ncol=jmax)  
  # Reproductive cow in seventh parity (1= at least seven calves, 0 = less than seven calves)
  PARITY7 = matrix(nrow=imax, ncol=jmax)  
  # Reproductive cow in eighth parity (1= at least eight calves, 0 = less than eight calves)
  PARITY8 = matrix(nrow=imax, ncol=jmax)  
  # Reproductive cow in nineth parity (1= at least nine calves, 0 = less than nine calves)
  PARITY9 = matrix(nrow=imax, ncol=jmax)  
      
  # Herd dynamics
  BEEFPROD          = matrix(nrow=imax+1, ncol=jmax)
  BEEFPRODYEAR      = matrix(nrow=imax+1, ncol=jmax)
      
  BEEFPRODACT       = matrix(nrow=imax+1, ncol=jmax)
  LWPRODACT         = matrix(nrow=imax+1, ncol=jmax)
  CARCPRODACT       = matrix(nrow=imax+1, ncol=jmax)
  LWPROD            = matrix(nrow=imax+1, ncol=jmax)
  LWPRODYEAR        = matrix(nrow=imax+1, ncol=jmax)
  LWPRODHERD        = NULL
      
  SLAUGHTERDAYACT       = matrix(nrow=imax+1, ncol=jmax)
  SLAUGHTERDAYACTpl     = matrix(nrow=imax+1, ncol=jmax)
  SLAUGHTERDAYACTHEIFER = matrix(nrow=imax+1, ncol=jmax)
  ENDDAY   <- c(1,1,1,1,1,1,1,1,1)
      
  SUMFEED1 = matrix(nrow=imax, ncol=jmax)
  SUMFEED2 = matrix(nrow=imax, ncol=jmax)
  SUMFEED3 = matrix(nrow=imax, ncol=jmax)
  SUMFEED4 = matrix(nrow=imax, ncol=jmax)
  SUMFEED  = matrix(nrow=imax, ncol=jmax)
      
  CUMULFEED1 = matrix(nrow=imax, ncol=jmax)
  CUMULFEED2 = matrix(nrow=imax, ncol=jmax)
  CUMULFEED3 = matrix(nrow=imax, ncol=jmax)
  CUMULFEED4 = matrix(nrow=imax, ncol=jmax)
  CUMULFEED  = matrix(nrow=imax, ncol=jmax)
      
  FATBURNCUMUL  = matrix(nrow=imax+1, ncol=jmax)
  HEATBURNCUMUL = matrix(nrow=imax, ncol=jmax)
  ALIVE         = matrix(nrow=imax+1, ncol=jmax)
      
  FCR           = matrix(nrow=imax, ncol=jmax)
  FCRBEEF       = matrix(nrow=imax, ncol=jmax)
  FCRBEEFENDDAY = matrix(nrow=imax, ncol=jmax)
      
  MILKSTART     = matrix(nrow=imax, ncol=jmax)
  MILKSTARTPR   = matrix(nrow=imax, ncol=jmax)
  MILKSTARTPRHF = matrix(nrow=imax, ncol=jmax)
      
  METABFEED     = matrix(nrow=imax, ncol=jmax)
  METABFEEDC    = matrix(nrow=imax, ncol=jmax)  
  METABFEEDCH   = matrix(nrow=imax, ncol=jmax)
      
  CHECKHEAT1    = matrix(nrow=imax, ncol=jmax, NA)
  CHECKHEAT2    = matrix(nrow=imax, ncol=jmax, NA)
  CHECKHEAT3    = matrix(nrow=imax, ncol=jmax, NA)
  CHECKCOMP     = matrix(nrow=imax, ncol=jmax)
      
  MAXW1         = NULL
      
  CALVESPERANIMAL = NULL
      
  BEEFPRODHERD    = NULL
  FCRHERDBEEF     = NULL
  CUMULFEEDHERD   = NULL
  CUMULFEED1HERD  = NULL
  CUMULFEED2HERD  = NULL
  CUMULFEED3HERD  = NULL
  CUMULFEED4HERD  = NULL
  ANIMALYEARS     = NULL
  AVANWEIGHT      = matrix(nrow=imax, ncol=jmax)
  AVANMETWEIGHT   = matrix(nrow=imax, ncol=jmax)
      
  ANIMALINFO      = NULL
  HERDINFO        = NULL
  HERDINFO1       = NULL
  FATCOMP         = matrix(nrow=imax, ncol=jmax)
  NONCF           = matrix(nrow=imax, ncol=jmax)
  PERCFI          = matrix(nrow=imax, ncol=jmax)
  REPS            = matrix(nrow=imax, ncol=jmax)
      
  MEMET           = matrix(nrow=imax, ncol=jmax)
  MERED           = matrix(nrow=imax, ncol=jmax)
      
  PROTNONG        = matrix(nrow=imax, ncol=jmax)
  HIFM            = matrix(nrow=imax, ncol=jmax)
  PROTNONGM       = matrix(nrow=imax, ncol=jmax)
  CPAVG           = matrix(nrow=imax, ncol=jmax)
      
  OUTPUTHERDS     = NULL
  OUTPUTHERDS1    = NULL
      
  CUMULMILK       = NULL
      
      
  ###########################################################################################
  # Module for the assessment of greenhouse gas emissions related to livestock production   #
  #                                                                                         #
  # Processes included:                                                                     #
  # * Enteric methane emission                                                              #
  # * Methane from manure management                                                        #
  # * N20 from manure management                                                            #
  # This section specifies the variables                                                    #
  ###########################################################################################
      
  ENTCH4MJ   = matrix(nrow=imax, ncol=jmax) # Energy content enteric methane emissions per 
                                            # animal per day
  ENTCH4KG   = matrix(nrow=imax, ncol=jmax) # Weight enteric methane emissions per animal per
                                            # day
  ENTCH4frac = matrix(nrow=imax, ncol=jmax) # Fraction enteric methane emissions from feed
  ENTCH4EQ   = matrix(nrow=imax, ncol=jmax) # CO2 equivalents enteric methane, in kg per 
                                            # animal per day
  VS         = matrix(nrow=imax, ncol=jmax) # Volatile solid excretion rates
  EF         = matrix(nrow=imax, ncol=jmax) # CH4 emissions factor from manure management
  CH4MM      = matrix(nrow=imax, ncol=jmax) # Methane emission from manure management per 
                                            # animal per day
  CH4MMEQ    = matrix(nrow=imax, ncol=jmax) # CO2 equivalents methane from dung and manure, 
                                            # in kg per animal per day
  Nexcr      = matrix(nrow=imax, ncol=jmax) # N excreted by the animal in dung and urine
  N2OD       = matrix(nrow=imax, ncol=jmax) # Direct N20 emissions from manure management per
                                            # kg DM intake  
  N2ODan     = matrix(nrow=imax, ncol=jmax) # Direct N20 emissions from manure management per
                                            # animal
  N2ODanEQ   = matrix(nrow=imax, ncol=jmax) # CO2 equivalents N2O from dung and urine on 
                                            # pasture, in kg per animal per day
  NvolMMS    = matrix(nrow=imax, ncol=jmax) # N losses due to volatilisation from manure 
                                            # management
  N2OG       = matrix(nrow=imax, ncol=jmax) # Indirect N2O emissions due to volatilisation of
                                            # N from manure management
  N2OGan     = matrix(nrow=imax, ncol=jmax) # Indirect N20 emissions from manure management 
                                            # per animal
  N2OGanEQ   = matrix(nrow=imax, ncol=jmax) # Indirect CO2 equivalents N2O from dung and 
                                            # urine, in kg per animal per day
  NleaMMS    = matrix(nrow=imax, ncol=jmax) # N losses due to leaching from manure management 
                                            # systems
  N2OL       = matrix(nrow=imax, ncol=jmax) # Indirect N2O emissions due to leaching per kg 
                                            # DM
  N2OLan     = matrix(nrow=imax, ncol=jmax) # Indirect N2O emissions due to leaching per 
                                            # animal
  N2OLanEQ   = matrix(nrow=imax, ncol=jmax) # Indirect CO2 equivalents N2O from N leaching, 
                                            # in kg per animal per day
  CO2EQ      = matrix(nrow=imax, ncol=jmax) # CO2 equivalents for enteric methane and manure 
                                            # management per animal per day
  MCF        = matrix(nrow=imax, ncol=jmax) # Methane conversion factor (from manure)
      
  CUMULENTCH4EQ  = NULL # Total enteric methane emission per animal
  CUMULCH4MMEQ   = NULL # Total methane emissions from dung and urine per animal
  CUMULN2ODanEQ  = NULL # Total direct N2O emissions from dung and manure
  CUMULN2OGanEQ  = NULL # Indirect N2O emissions from dung and manure
  CUMULN2OLanEQ  = NULL # Indirect N2O emissions from dung and manure (leaching)
  CUMULPROTTOTAL = NULL # Total CP intake by animals
  CUMULPROTLW    = NULL # Total CP in LW
  CUMULPROTBEEF  = NULL # Total CP in LW
      
     
  ###########################################################################################
  #                            Dynamic part of the model (animals)                          #
  ###########################################################################################
  
  HOUSING1 <- HOUSING # Makes a copy of the housing conditions (0 = stables, 1 = outdoors) 
  FEED11 <- FEED1     # Makes a copy of the amount of feed type 1 available for animals
  FEED21 <- FEED2     # Makes a copy of the amount of feed type 2 available for animals
  FEED31 <- FEED2     # Makes a copy of the amount of feed type 3 available for animals
      
  # If breakFlaganim is TRUE, the simulation stops for this particular animal    
  breakFlaganim <- FALSE 
  
  # The j-loop simulates an individual animal (only one animal at animal level, when SCALE =
  # 1; j = 1) and multiple animals at herd level (i.e. a herd unit, when SCALE = 2)    
  
  for (j in 1:jmax){ 
                         
  # Maximum number of days an animal in a herd unit is simulated
  imax <- c(imax,2500,2500,2500,2500,2500,2500,2500,2500)   
  # Gives housing conditions for calves in a herd unit
  if(j>1) HOUSING <- HOUSING1[BIRTHDAY[j]:length(HOUSING1)] 
  # Gives feed availability for calves in a herd unit (feed type 1)
  if(j>1) FEED1 <- FEED11[BIRTHDAY[j]:length(HOUSING1),]    
  # Gives feed availability for calves in a herd unit (feed type 2)
  if(j>1) FEED2 <- FEED21[BIRTHDAY[j]:length(HOUSING1),]    
  # Gives feed availability for calves in a herd unit (feed type 3)
  if(j>1) FEED3 <- FEED31[BIRTHDAY[j]:length(HOUSING1),]    
    
  # The i-loop is the loop that simulates the dynamics of animal j over time. One time step 
  # equals one day (i).
        
  for (i in 1:imax[j]) { 
        
  # If breakFlagtime is TRUE, the simulation stops at this particular time(step). The 
  # conditions under which breakFlagtime is TRUE are given elsewhere in this code.  
  breakFlagtime <- FALSE # 
      
  ###########################################################################################
  # 1.5                          Initial values for individual animals                      #
  ########################################################################################### 
       
  # Time in days, start from day 1
  TIME      <- matrix(rep(1:imax[j],jmax), nrow=imax[j], ncol=jmax)
  # Time in days, start from day 0  
  TIME2     <- matrix(rep(0:imax[j],jmax), nrow=imax[j]+1, ncol=jmax) 
  # Time in years, start from day 1
  TIMEYEAR  <- matrix(rep(1:imax[j]/365,jmax), nrow=imax[j], ncol=jmax)
  # Time in years, start from day 0
  TIMEYEAR2 <- matrix(rep(0:imax[j]/365,jmax), nrow=imax[j]+1, ncol=jmax) 
  
  # This code selects which breed- and sex-specific parameters should be chosen, based on the
  # genotype/breed and sex listed above.   
        if(BREED ==1 & SEX[j] == 0) LIBRARY <- LIBRARY10 else
          if(BREED ==1 & SEX[j] == 1) LIBRARY <- LIBRARY11 else
            if(BREED ==2 & SEX[j] == 0) LIBRARY <- LIBRARY20 else
              if(BREED ==2 & SEX[j] == 1) LIBRARY <- LIBRARY21 else 
                if(BREED ==3 & SEX[j] == 0) LIBRARY <- LIBRARY30 else
                  if(BREED ==3 & SEX[j] == 1) LIBRARY <- LIBRARY31 else
                    if(BREED ==4 & SEX[j] == 0) LIBRARY <- LIBRARY40 else 
                      if(BREED ==4 & SEX[j] == 1) LIBRARY <- LIBRARY41 else
                        if(BREED ==5 & SEX[j] == 0) LIBRARY <- LIBRARY50
        
  # Some breed and sex-specific parameters from the LIBRARY are re-named:
  REFLC      = LIBRARY[1]  # fraction light reflected from coat (-)
  LC         = LIBRARY[2]  # coat length (m)
  AREAFACTOR = LIBRARY[3]  # surface area Bos taurus (1.00) or Bos indicus (1.12)
  CBSMAX     = LIBRARY[4]  # maximum body - skin conductivity (W m-2 K-1)
  TBW[1,j]   = LIBRARY[5]  # Birth weight (kg total body weight)
  MAXW       = LIBRARY[6]  # Maximum adult weight (kg total body weight)
  BIRTHW     = LIBRARY[7]  # Birth weight (kg total body weight)
  CPAR       = LIBRARY[8]  # Gompertz curve, constant of integration 
  DPAR       = LIBRARY[9]  # Gompertz curve, rate constant
  EPAR       = LIBRARY[10] # Gompertz curve, reduction
  MAXW1[j]   = LIBRARY[13] # maximum adult weight (kg total body weight)
  MILKPARA   = LIBRARY[11] # Lactation curve parameter (Wood, 1967)
  MILKPARB   = LIBRARY[12] # Lactation curve parameter (Wood, 1967)
  MILKPARC   = LIBRARY[27] # Lactation curve parameter (Wood, 1967)
  RBCSf      = LIBRARY[23] # Parameter to calculate minimum conductance between body core and
                           # skin (-)        
  
  # Initial values for weight of body tissues (day 1 corresponds to birth, so time i = 1)      
  
  # Initial carcass weight (kg)
  CARCW[1,j]         = LIBRARY[5]*INCARC 
  # Initial bone weight (kg)
  BONETIS[1,j]       = CARCW[1,j]*min(BONEFRACMAX,BONEGROWTH1*CARCW[1,j]^-BONEGROWTH2) 
  # Initial muscle weight (kg) 
  MUSCLETIS[1,j]     = BONETIS[1,j]*min(LIBRARY[22],MUSCLEGROWTH1*CARCW[1,j]^2+
                                          LIBRARY[22]/100*CARCW[1,j]+MUSCLEGROWTH2)   
  # Initial intramuscular fat weight (kg)
  INTRAMFTIS[1,j]    = IMFGROWTH1*(BONETIS[1,j]+MUSCLETIS[1,j])^2+IMFGROWTH2*
    (BONETIS[1,j]+MUSCLETIS[1,j])-IMFGROWTH3 
  # Initial subcutaneous and intermuscular fat weight (kg)
  MISCFATTIS[1,j]    = CARCW[1,j]-BONETIS[1,j]-MUSCLETIS[1,j]-INTRAMFTIS[1,j] 
  # Initial non-carcass weight (kg)
  NONCARCTIS[1,j]    = TBW[1,j]*(1-RUMENFRAC)-CARCW[1,j] 
  # Initial rumen weight (kg)
  RUMEN[1,j]         = TBW[1,j]*RUMENFRAC 
        
  # Inital lipid and protein content of body tissues
  
  # Initial lipid in bone tissue (kg)
  LIPIDBONE[1,j] = BONETIS[1,j]*(LIBRARY[20]* log(BONETIS[1,j]))/100 
  # Initial protein in bone tissue (kg)
  PROTBONE[1,j] = BONETIS[1,j]*PROTFRACBONE                          
  # Initial lipid in muscle tissue (kg)
  LIPIDMUSCLE[1,j] = MUSCLETIS[1,j]*LIPFRACMUSCLE                    
  # Initial protein in muscle tissue (kg)
  PROTMUSCLE[1,j] = MUSCLETIS[1,j]*PROTFRACMUSCLE                    
  # Initial lipid in intramuscular tissue (kg)
  LIPIDIMF[1,j] = INTRAMFTIS[1,j]*LIPFRACFAT                         
  # Initial protein in intramuscular tissue
  PROTIMF[1,j] = INTRAMFTIS[1,j]*PROTFRACFAT                         
  # Initial lipid in fat tissue (kg)
  LIPIDFAT[1,j] = MISCFATTIS[1,j]*LIPFRACFAT                         
  # Initial protein in fat tissue (kg)
  PROTFAT[1,j] = MISCFATTIS[1,j]*PROTFRACFAT                         
  # Inital lipid in non-carcass tissue (kg)
  LIPIDNONC[1,j] = 1.00                                              
  # Initial protein in non-carcass tissue (kg)
  PROTNONC[1,j] = NONCARCTIS[1,j]*((PROTNONCM1)*TBW[1,j] + (PROTNONCM2)) / 100 
        
  # The variables below allow to assess the genetic growth of tissues, and their growth 
  # constrained by climate, feed quality, and feed quantity.
  BONETISACT[1,j]     = BONETIS[1,j]
  MUSCLETISACT[1,j]   = MUSCLETIS[1,j]
  INTRAMFTISACT[1,j]  = INTRAMFTIS[1,j]
  MISCFATTISACT[1,j]  = MISCFATTIS[1,j]
  NONCARCTISACT[1,j]  = NONCARCTIS[1,j]
  TBWACT[1,j]         = TBW[1,j]
  EBWACTMET[1,j]      = (TBW[1,j]*(1-RUMENFRAC))^0.75
  MISCFATFRAC[1,j]    = MISCFATTISACT[1,j]/TBWACT[1,j]
  LIPIDBONEACT[1,j]   = LIPIDBONE[1,j]
  LIPIDNONCACT[1,j]   = LIPIDNONC[1,j]
  PROTNONCACT[1,j]    = PROTNONC[1,j]
  LIPIDMUSCLEACT[1,j] = LIPIDMUSCLE[1,j]
  LIPIDIMFACT[1,j]    = LIPIDIMF[1,j]
  LIPIDFATACT[1,j]    = LIPIDFAT[1,j]
  LIPIDTOTW[1,j]      = (LIPIDBONEACT[1,j]+LIPIDNONCACT[1,j]+LIPIDMUSCLEACT[1,j]+
                           LIPIDIMFACT[1,j]+LIPIDFATACT[1,j])
  LIPIDPERCCARC[1,j]  = (LIPIDBONEACT[1,j]+LIPIDMUSCLEACT[1,j]+LIPIDIMFACT[1,j]+
                           LIPIDFATACT[1,j])/(TBWACT[1,j]-NONCARCTISACT[1,j])
      
  HEATTOTALACT[1,j]   = 9.00 # Assumption for the first day for heat release (MJ per day).
  FATBURNCUMUL[1,j]   = 0    # Assumption that the animal is not under heat stress in the 
                             # first day of its life
  HEATBURNCUMUL[1,j]  = 0    # Cumulative additional energy used to maintain body temperature 
                             # starts at zero, assumption is that the animal is not 
                             # experiencing cold stress. 
  ALIVE[1,j]          = 1    # Indicates whether an animal is alive or already slaughtered
      
  # Gestation (female calves can conceive later on in their life if six requirements are 
  # met)
  
  CALFTBW[1,j] = 0.0     # A calf cannot have a calf at day 1 (i=1) 
  CALFNR[1,j] = 0        # A calf has given birth to zero calves at day 1 (i=1)
        
  GEST1[1,j]= 0          # Requirement 1: Total body weight has to be above a minimum 
                         # weight. This is not the case at day 1. 
  GEST2[1,j]= 1          # Requirement 2: A cow cannot conceive if she is already 
                         # gestating.
  GEST3[1,j]= 1          # Requirement 3: A minimum gestation interval exists between 
                         # calvings.
  GEST4[1,j]= 0          # Requirement 4: Body fat reserves have to exceed a minimum 
                         # percentage of the carcass weight.  
  GEST5[1,j]= 0          # Requirement 5: Cows conceive and calve at a specific day of the 
                         # year.
  GEST6[1,j]= 1          # Requirement 6: Cows are culled after exceeding the maximum 
                         # number of calves per animal
  GESTDAY[1,j] = 0
  NEREQGESTADD[1,j] = 0
  TBWADD[1,j] = 0 
        
  # Parieties of cows (0 = no, 1 = yes)
        
  PARITY1[1,j] = 0 # 0 if the number of calves of a cow is below 1
  PARITY2[1,j] = 0 # 0 if the number of calves of a cow is below 2
  PARITY3[1,j] = 0 # 0 if the number of calves of a cow is below 3
  PARITY4[1,j] = 0 # 0 if the number of calves of a cow is below 4
  PARITY5[1,j] = 0 # 0 if the number of calves of a cow is below 5
  PARITY6[1,j] = 0 # 0 if the number of calves of a cow is below 6
  PARITY7[1,j] = 0 # 0 if the number of calves of a cow is below 7
  PARITY8[1,j] = 0 # 0 if the number of calves of a cow is below 8
        
  # Milk production
  
  MILKDAY[1,j] = 0
  MILKWEEK[1,j] = 0
  
        
  ###########################################################################################
  # 2.                                   Dynamic section                                    #
  #                                     (time and animals)                                  #                                                                  
  ###########################################################################################
        
  ###########################################################################################
  # 2.1                             Thermoregulation submodel                               #
  ###########################################################################################
      
  # Aim: To calculate the maximum and minimum heat release (W m-2) of an animal with its 
  # environment 
      
  # Five flows of energy between an animal and its environment
  #   1. Latent and convective heat release from respiration
  #   2. Latent heat release from the skin
  #   3. Long wave radiation balance of the coat
  #   4. Convective heat losses from the coat
  #   5. Solar radiation intecepted by the coat
       
  ###########################################################################################
  # 2.1.1                             Maximum heat release                                  #
  ###########################################################################################    
        
  # Heat release mechanisms of cattle at maximum heat release
  TISSUEFRAC = 1.00       # Vasodilatation (0 = minimum and 1 = maximum vasodilatation)
  SWEATING   = 1.00       # Sweating (0 = basal and 1 = maximum physiological sweating rate)
  PANTING    = RESPDUR    # Panting (0 = basal respiration, 1 = maximum panting)
      
  # Calculations related to weather conditions
  
  # average temperature (degrees Celsius)    
  TAVGC[i,j]     <- (WEATHER$MINT[i]+WEATHER$MAXT[i])/2                     
  # average temperature (degrees Kelvin)
  TAVGK[i,j]     <- CtoK + TAVGC[i,j]                                       
  # saturated vapour pressure air (Pa)
  VPSATAIR[i,j]  <- 6.1078*10^((7.5*TAVGC[i,j])/(TAVGC[i,j]+237.3))*100     
  # real vapour pressure air (kPa)
  VPAIRTOT[i,j]  <- WEATHER$VPR[i]*1000                                     
  # relative humidity (-)
  RHAIR[i,j]     <- VPAIRTOT[i,j] / VPSATAIR[i,j] *100                      
  # water vapour density (kg m-3)
  RHOVP[i,j]     <- VPAIRTOT[i,j]/ (Rwater*TAVGK[i,j])
  # dry air density (kg m-3)
  RHODAIR[i,j]   <- (P-VPAIRTOT[i,j]) / (Rdair*TAVGK[i,j])                  
  # air density (kg m-3)
  RHOAIR[i,j]    <- RHOVP[i,j] + RHODAIR[i,j]                               
  # water vapour density (kg kg-1)
  CHIAIR[i,j]    <- RHOVP[i,j]*RHOAIR[i,j]                                  
                  
  ###########################################################################################
  #                1. Latent and convective heat release from respiration                   #
  ###########################################################################################
            
  # Animal characteristics
  
  # animal surface area (m2), equation from McGovern and Bruce (2000)
  AREA[i,j] = BODYAREA1*TBWACT[i,j]^BODYAREA2 * AREAFACTOR                
  # animal diameter (m), equation from McGovern and Bruce (2000)
  DIAMETER[i,j] = DIAMETER1*TBWACT[i,j]^DIAMETER2                         
  # animal length (m)
  LENGTH[i,j] = (AREA[i,j]-0.5*pi*DIAMETER[i,j]^2)/(pi*DIAMETER[i,j])     
            
  # basal respiration rate (min-1), equation from McGovern and Bruce (2000)
  brr[i,j] <- BASALRR1 * TBWACT[i,j]^BASALRR2                              
  # basal tidal volume (L), equation from McGovern and Bruce (2000)
  btv[i,j] <- BASALTV * TBWACT[i,j]                                        
  # basal respiration volume (L min-1) 
  brv[i,j] <- brr[i,j]*btv[i,j]                                            
  # increased respiration volume (L min-1) 
  irv[i,j] <- brv[i,j] + PANTING*((RESPINCR-1)*brv[i,j])                   
  
  # temperature exhaled air (degrees Celsius), based on Stevens (1981)        
  Texh[i,j] <- TEXHALED1 + TEXHALED2 * TAVGC[i,j] + exp(TEXHALED3 * RHAIR[i,j]  +
                                                          TEXHALED4 * TAVGC[i,j])  
            
  # Assumption: exhaled air is saturated with water (RH = 100%)
  
  # saturated vapour pressure exhaled air (Pa)
  VPSATAIROUT[i,j]  <- 6.1078*10^((7.5*Texh[i,j])/(Texh[i,j]+237.3))*100    
  # water vapour density exhaled air (kg m-3)
  RHOVPOUT[i,j]     <- VPSATAIROUT[i,j]/ (Rwater*(Texh[i,j]+CtoK))          
  # dry air density exhaled air (kg m-3)
  RHODAIROUT[i,j]   <- (P-VPSATAIROUT[i,j]) / (Rdair*(Texh[i,j]+CtoK))      
  # air density exhaled air (kg m-3)
  RHOAIROUT[i,j]    <- RHOVPOUT[i,j] + RHODAIROUT[i,j]                      
  # water vapour density exhaled air (kg kg-1) 
  CHIAIROUT[i,j]    <- RHOVPOUT[i,j]*RHOAIROUT[i,j]                         
  # air exchange between animal and environment (kg air m-2 day-1)    
  AIREXCH[i,j] <- (irv[i,j]*60*24/1000*RHOAIR[i,j])/AREA[i,j]                  
            
  # latent heat release via respiration (W m-2) 
  LHEATRESP[i,j] <- AIREXCH[i,j] * L *(CHIAIROUT[i,j]-CHIAIR[i,j])* kJdaytoW   
  # convective heat release via respiration (W m-2) 
  CHEATRESP[i,j] <- AIREXCH[i,j] * Cp *(Texh[i,j]-TAVGC[i,j]) * kJdaytoW       
  # total heat release from the respiratory system (W m-2)          
  TGRESP[i,j] <- LHEATRESP[i,j] + CHEATRESP[i,j]                               
            
  #Energy for respiration (panting)
  
  # NE required for respiration (W m-2), equation from McGovern and Bruce (2000)
  NERESPWM[i,j] <- 1.1*(RESPINCR*brr[i,j])^2.78 * 10^-5 * PANTING   
  # NE required for respiration (kJ NE day-1)      
  NERESP[i,j] <-NERESPWM[i,j] / kJdaytoW                               
            
  # net energy loss from the respiratory system (W m-2)
  TNRESP[i,j] <- TGRESP[i,j]-NERESPWM[i,j]                          
  # net energy loss respiratory system under maximum heat release (i.e. heat stress)         
  TNRESPH[i,j] <- TNRESP[i,j] 
  
  ###########################################################################################
  #                                    1a. Skin temperature                                 #
  ###########################################################################################
        
  # Resistance body core and skin
  
  # minimum body - skin conductivity (W m-2 K-1), equation from McGovern and Bruce (2000)
  CBSMIN[i,j] = RBCSf/((MINCCS1) * TBWACT[i,j]^(MINCCS2))
  # conductivity body core to skin (W m-2 K-1)   McGovern and Bruce (2000)
  CONDBS[i,j] = CBSMIN[i,j] + TISSUEFRAC*(CBSMAX-CBSMIN[i,j])  
        
  # Note on conversion:
  # 100 S m-1 = 0.078 K m2 W-1 (Cena and Clark, 1978) 
  # Cattle --> 50 s m-1 (Turnpenny, 2000a) --> 0.039 K m2 W-1  = 25.6 W m-2 K-1
    
      
  ###########################################################################################
  #                            2. Latent heat release from the skin                         #
  ###########################################################################################
        
  # Latent energy flow between skin and air
  
  # reduction in coat depth (m) McGovern and Bruce (2000)
  DLC[i,j] = (CoatConst * WEATHER$WIND[i])/((CoatConst * WEATHER$WIND[i])/LC+1/(ZC*LC)) 
  # diffusion constant water vapour in air (m2 s-1) (Denny, 1993) 
  DIFFC[i,j] = 0.187 * 10^-9 * TAVGK[i,j]^2.072                                         
        
  ###########################################################################################
  #                                    2a. Coat temperature                                 #
  ###########################################################################################
        
  # Resistance and conductivity between skin and coat
  
  # conductance skin to coat (W m-2 K-1)
  CSC[i,j] = 1 /(RUC * ZC * (LC-DLC[i,j]))                              
  # increase in conductance due to rain (Mount and Brown, 1982)      
  CSC[i,j] <- CSC[i,j]/ (1-min(RAINFRAC, WEATHER$RAIN[i]*RAINFRAC/24))  
        
  ###########################################################################################
  #                            3. Long wave radiation from the coat                         #
  ###########################################################################################
        
  # Incoming long wave radiation (LWR) from the sky in (W m-2), equation from McGovern and 
  # Bruce (2000) 
  LWRSKY[i,j] = (1-WEATHER$OCTA[i]/8)*(SIGMA*TAVGK[i,j]^4)*
    (1-0.261*exp(-0.000777*(273-TAVGK[i,j])^2)) + (WEATHER$OCTA[i]/8) * 
    (SIGMA * TAVGK[i,j]^4 - 9) # 
        
  # Incoming LWR from soil surface (W m-2)
  LWRENV[i,j] = SIGMA * TAVGK[i,j]^4   
  
  # If housed, LWR is calculated from the stable temperature (W m-2)      
  if(HOUSING[i] == 0) LWRSKY[i,j] <- LWRENV[i,j]   
          
  ###########################################################################################
  #                          4. Convective heat losses from the coat                        #
  ###########################################################################################
        
  # Calculation of the air viscosity 
  # average air temperature (degrees Rankine)
  TAVGR[i,j] = TAVGK[i,j] * KtoR                                                     
  # actual air viscosity (N s-1 m-2), based on Smits and Dussaunge (2006)
  VISCAIR[i,j] =(MuSt*((0.555*TR0+ST)/(0.555*TAVGR[i,j]+ST)*(TAVGR[i,j]/TR0)^(3/2)))   
        
  # Calculation of the Grashof number  
  # vapour pressure air (mBar)
  Ea[i,j] = WEATHER$VPR[i]*10                                                     
        
  # Calculation of the Reynolds number 
  # wind speed (m s-1)
  WINDSP[i,j] = WEATHER$WIND[i]                                                          
  # Reynolds number (dimensionless)
  REYNOLDS[i,j] = WINDSP[i,j] * DIAMETER[i,j] * RHOAIR[i,j] / VISCAIR[i,j]         
  # calculation step representing natural convection (dimensionless)      
  ReH[i,j] = 16*REYNOLDS[i,j]^2                                                                               
  # calculation step representing forced convection (dimensionless)
  ReL[i,j] = 0.1*REYNOLDS[i,j]^2                                             
  # themal conductivity air (W m-1 K-1)
  ka[i,j] = 1.5207 * 10^(-11) * TAVGK[i,j]^3 - 4.8574 * 10^(-8) * 
    TAVGK[i,j]^2 + 1.0184 * 10^-4 *TAVGK[i,j] - 0.00039333 
        
  ###########################################################################################
  #                        5. Solar radiation intercepted by the coat                       #
  ###########################################################################################
        
  # Direct solar radiation
  
  # Ah/A factor: Shade area / animal coat area (m2 m-2)
  SAAC[i,j] <- WEATHER$AHA[i]                 
  # incoming direct solar radiation soil surface (W m-2 day-1)
  SWRS[i,j] <- WEATHER$RAD[i]*kJdaytoW        
  # incoming direct solar radiation animal coat (W m-2 day-1)
  SWRC[i,j] <- SWRS[i,j]*SAAC[i,j]*(1-REFLC)  
        
  # Indirect solar radiation
  # Reflectivity based on housing / free grazing (dimensionless)
  if(HOUSING[i]==1) REFLE[i] <- REFLEgrass else 
    if(HOUSING[i]==2) REFLE[i] <- REFLEconcr else REFLE[i] <- 0  
  # incoming indirect solar radiation,animal coat (W m-2 day-1)
  ISWRC[i,j] <- FRACVEG*REFLE[i]*SWRS[i,j]    
        
  # Total solar radiation (W m-2 day-1)                                    
  SWR[i,j] <- SWRC[i,j] + ISWRC[i,j]          
        
  # Heat loss by evaporation of rainwater from coat (W m-2 day-1)
  RAINEVAP[i,j] <- (RAINEVAP1)*(LENGTH[i,j]*DIAMETER[i,j])/AREA[i,j] * 
    min(24,WEATHER$RAIN[i]) * L * kJdaytoW 
  # Note: Factor seems important under heat stress.  
  
  ###########################################################################################
        
  # First a guesstimate for heat production under maximum heat release (METABFEED, in W m-2)
  # is used. This guesstimate is used as an initial value. The maximum heat release is 
  # calculated from this initial value.
  
  # Guesstimates of maximum heat release can be based on the expected level of heat stress in
  # a particular country to reduce computer processor time, but this is no absolute necessity.
    if(LOCATION == 'FRANCE') METABFEED[i,j] <- 190 else 
      if(LOCATION == 'ETHIOPIA')  METABFEED[i,j] <- 155 else
        if(LOCATION == 'NETHERLANDS')  METABFEED[i,j] <- 207 else METABFEED[i,j] <- 170     
        
  repeat {  
         
  ###########################################################################################
  #                                    1a. Skin temperature                                 #
  ###########################################################################################
  
  # heat flow from body core to skin (W m-2)          
  MetheatSKIN[i,j] = METABFEED[i,j] - TNRESP[i,j]                   
  # skin temperature (degrees Celsius)
  TskinC[i,j] = TbodyC - MetheatSKIN[i,j]/CONDBS[i,j] 
  
  # heat production under maximum heat release          
  METABFEEDCH[i,j] = METABFEED[i,j]
  
  ###########################################################################################
  #                            2. Latent heat release from the skin                         #
  ###########################################################################################
  
  # maximum physological latent heat release from skin (W m-2)             
  LASMAXPHYS[i,j] = LASMIN + LIBRARY[24]*exp(LIBRARY[25]*(TskinC[i,j]-LIBRARY[26])) * L/3600 
  # resistance to water vapour transfer (s m-1) based on Thompson et al.         
  RV[i,j] = (LC-DLC[i,j])/(DIFFC[i,j]*(1+1.54*((LC-DLC[i,j])/DIAMETER[i,j]) * 
                                         (TskinC[i,j]-min(TAVGC[i,j],TskinC[i,j]))^0.7))                      
  # saturated vapour pressure skin (Pa)
  VPSKINTOT[i,j] = 6.1078*10^((7.5*TskinC[i,j])/(TskinC[i,j]+237.3))*100       
            
  # maximum latent heat release from the skin to the ambient environment (W m-2)        
  LASMAXENV[i,j] = (RHOAIR[i,j] * Cp * 1000) / GAMMA * 
    (VPSKINTOT[i,j]-VPAIRTOT[i,j]) / RV[i,j] 
  # maximum latent heat release from skin (W m-2)
  LASMAXCORR[i,j] = min(LASMAXPHYS[i,j],LASMAXENV[i,j])                           
  # actual latent heat release from skin (W m-2)
  ACTSW[i,j] = LASMIN + SWEATING * (LASMAXCORR[i,j]-LASMIN)                       
  
  # Sweating rate under maximum heat release (W m-2)        
  ACTSWH[i,j] = ACTSW[i,j]  
            
  ###########################################################################################
  #                                    2a. Coat temperature                                 #
  ###########################################################################################
            
  # Heat transported to the coat layer (W m-2)
  MetheatCOAT[i,j] = MetheatSKIN[i,j] - ACTSW[i,j]      
  # coat temperature (degrees Celsius)          
  TcoatC[i,j] = TskinC[i,j] - MetheatCOAT[i,j]/CSC[i,j] 
  # coat temperature (Kelvin)
  TcoatK[i,j] = TcoatC[i,j] + CtoK                      
            
  ###########################################################################################
  #                            3. Long wave radiation from the coat                         #
  ###########################################################################################
  
  # energy flow from skin to coat (W m-2)          
  QSC[i,j] = CSC[i,j] * (TskinC[i,j] - TcoatC[i,j])     
  # LWR release from body (W m-2)
  LB[i,j] = EMISS * SIGMA * TcoatK[i,j]^4               
            
  # LWR balance (net energy loss is a negative value, in W m-2)
  
  # Net LWR loss (W m-2)
  LWRCOAT[i,j] = (EMISS * ((LWRSKY[i,j]+LWRENV[i,j])/2) - LB[i,j]) * 
    (1-min(RAINFRAC,WEATHER$RAIN[i]*RAINFRAC/24))                 
  # Heat loss via LWR under maximum heat release (W m-2)
  LWRCOATH[i,j] = LWRCOAT[i,j] 
            
  ###########################################################################################
  #                          4. Convective heat losses from the coat                        #
  ###########################################################################################
  
  # vapour pressure coat (mBar)          
  Ec[i,j] = ((6.1078*10^((7.5*TskinC[i,j])/(TskinC[i,j]+237.3)))+Ea[i,j])/2      
  
  # Grashof number (dimensionless)          
  GRASHOF[i,j] = (GRAV*DIAMETER[i,j]^3*P/100*(TcoatC[i,j]-TAVGC[i,j])+Schmidt*
                    (Ec[i,j]*TcoatC[i,j]-Ea[i,j]*TAVGC[i,j]))/(273*P/100*VISCAIR[i,j]^2) 
            
  # Calculation Nusselt number(dimensionless). #Formula taken from Turnpenny (2000a).
  if(GRASHOF[i,j]>ReH[i,j]) NUSSELT[i,j] <- 0.48*GRASHOF[i,j]^0.25 else 
  if(GRASHOF[i,j]<ReL[i,j]) NUSSELT[i,j] <- 0.0112*REYNOLDS[i,j]^0.875 else
  NUSSELT[i,j] <- max(0.48*GRASHOF[i,j]^0.25,0.0112*REYNOLDS[i,j]^0.875)    
                
  # Energy flow between coat and air
  
  # convective heat transfer (W m-2)          
  CONVCOAT[i,j] = (ka[i,j] * NUSSELT[i,j]) / DIAMETER[i,j] *(TcoatC[i,j]-TAVGC[i,j]) / 
    (1-min(RAINFRAC,WEATHER$RAIN[i]*RAINFRAC/24)) 
  # Convective heat transfer under maximum heat release (Wm-2)
  CONVCOATH[i,j] =  CONVCOAT[i,j] 
            
  ###########################################################################################
  #                                         Synthesis                                       #
  ###########################################################################################
  
  # net energy balance (W m-2)          
  MetheatBAL[i,j] <- (MetheatCOAT[i,j] + SWR[i,j] - RAINEVAP[i,j] + LWRCOAT[i,j] - 
                        CONVCOAT[i,j])     
  
  # If the heat balance is positive, heat production is reduced          
  if(MetheatBAL[i,j] > 0.1) METABFEED[i,j] <- (METABFEED[i,j]-0.1*MetheatBAL[i,j])  
  # If the heat balance is negative, heat production is increased
  if(MetheatBAL[i,j] < -0.1) METABFEED[i,j] <-(METABFEED[i,j]-0.1*MetheatBAL[i,j]) 
  # heat production from metabolic processes (under maximum heat production, in Wm-2)
  Metheatopt[i,j] <- METABFEED[i,j] 
  # If the heat balance is close to zero, the loop is completed.
  if(MetheatBAL[i,j] < 0.1 & MetheatBAL[i,j] > -0.1) CHECKHEAT1[i,j] <- "CORRECT" else 
    CHECKHEAT1[i,j] <- "FALSE" 
  # Break loop if heat balance is close to zero
  if(CHECKHEAT1[i,j] == "CORRECT") {break} 
            
  } # End of loop where Metheatopt[i,j] is calculated.
          
  ###########################################################################################
  # 2.1.2                             Minimum heat release                                  #
  ###########################################################################################
      
  # Heat release mechanisms of cattle at minimum heat release
  TISSUEFRAC = 0.0      # Vasodilatation (0 = minimum and 1 = maximum vasodilatation)
  SWEATING   = 0.0      # Sweating (0 = minimum and 1 = maximum physiological sweating rate)
  PANTING    = 0.0      # Panting (0 = basal respiration, 1 is maximum panting)   
                                                 
  ###########################################################################################
  #                1. Latent and convective heat release from respiration                   #
  ###########################################################################################
  
  # actual respiration rate (L min-1)      
  irv[i,j] <- brv[i,j] + PANTING*((RESPINCR-1)*brv[i,j])                         
  # air exchanged between the animal and its environment (kg air m-2 day-1)      
  AIREXCH[i,j] <- (irv[i,j]*60*24/1000*RHOAIR[i,j])/AREA[i,j]                  
  # latent heat release via respiratory system (W m-2)      
  LHEATRESP[i,j] <- AIREXCH[i,j] * L *(CHIAIROUT[i,j]-CHIAIR[i,j])*kJdaytoW    
  # convective heat release via respiratory system (W m-2)
  CHEATRESP[i,j] <- AIREXCH[i,j] * Cp *(Texh[i,j]-TAVGC[i,j])*kJdaytoW         
        
  # total heat release from the respiratory system (W m-2)
  TGRESP[i,j] <- LHEATRESP[i,j] + CHEATRESP[i,j]                               
  # nett heat release from the respiratory system (W m-2)      
  TNRESP[i,j] <- TGRESP[i,j]                                                   
        
  ###########################################################################################
  #                                    1a. Skin temperature                                 #
  ###########################################################################################
  
  # conductivity body core to skin (W m-2 K-1)      
  CONDBS[i,j] = CBSMIN[i,j]    
        
  ###########################################################################################
  #                            2. Latent heat release from the skin                         #
  ###########################################################################################
  
  # actual latent heat release from skin (W m-2)      
  ACTSW[i,j] = LASMIN          
        
  # Initial guesstimate for heat production at minimum heat release (METABFEEDC, W m-2 
  # day-1). This initial value is further modified in the repeat loop below. To reduce 
  # computer processing time, one can make the initial value country-specific based on the
  # expected occurrence of cold stress, but this it is not absolutely necessary to do so.
  
  if(LOCATION == 'FRANCE') METABFEEDC[i,j] <- 95 else 
    if(LOCATION == 'ETHIOPIA') METABFEEDC[i,j] <- 55 
      if(LOCATION == 'NETHERLANDS') METABFEEDC[i,j] <- 100 else METABFEEDC[i,j] <-80 
    
  repeat {  
          
  ###########################################################################################
  #                                    1a. Skin temperature                                 #
  ###########################################################################################
          
  # Skin temperature
          
  # amount of heat from body core to skin (W m-2)
  MetheatSKIN[i,j] = METABFEEDC[i,j] - TNRESP[i,j]
  # skin temperature (degrees Celsius)
  TskinC[i,j] = TbodyC - MetheatSKIN[i,j]/CONDBS[i,j]
  # skin temperature at minimum heat release (degrees Celsius)
  TskinCH[i,j] =  TskinC[i,j]                          
          
  ###########################################################################################
  #                                    2a. Coat temperature                                 #
  ###########################################################################################
          
  # Coat temperature
  
  # heat transported to the coat layer (W m-2)        
  MetheatCOAT[i,j] = MetheatSKIN[i,j] - ACTSW[i,j]       
  
  # coat temperature (degrees Celsius)        
  TcoatC[i,j] = TskinC[i,j] - MetheatCOAT[i,j]/CSC[i,j]
  # coat temperature (Kelvin)
  TcoatK[i,j] = TcoatC[i,j] + CtoK
  # skin temperature at minimum heat release (degrees Celsius)
  TcoatCH[i,j] = TcoatC[i,j]                             
          
  ###########################################################################################
  #                            3. Long wave radiation from the coat                         #
  ###########################################################################################
          
  # energy flow from skin to coat (W m-2)
  QSC[i,j] = CSC[i,j] * (TskinC[i,j] - TcoatC[i,j])      
  
  # LWR release from body (W m-2)        
  LB[i,j] = EMISS * SIGMA * TcoatK[i,j]^4                
          
  # LWR balance (net energy loss is a negative value, in W m-2)
  LWRCOAT[i,j] = (EMISS * ((LWRSKY[i,j]+LWRENV[i,j])/2) - LB[i,j]) * 
    (1-min(RAINFRAC,WEATHER$RAIN[i]*RAINFRAC/24))   
          
  ###########################################################################################
  #                          4. Convective heat losses from the coat                        #
  ###########################################################################################
  
  # vapour pressure coat (mBar)        
  Ec[i,j] = ((6.1078*10^((7.5*TcoatC[i,j])/(TcoatC[i,j]+237.3)))+Ea[i,j])/2      
  
  # Grashof number (dimensionless)        
  GRASHOF[i,j] = (GRAV*DIAMETER[i,j]^3*P/100*(TcoatC[i,j]-TAVGC[i,j])+Schmidt*
                    (Ec[i,j]*TcoatC[i,j]-Ea[i,j]*TAVGC[i,j]))/(273*P/100*VISCAIR[i,j]^2) 
          
  # Calculation of the Nusselt number (dimensionless), equations from Turnpenny (2000a)
  if(GRASHOF[i,j]>ReH[i,j]) NUSSELT[i,j] <- 0.48*GRASHOF[i,j]^0.25 else 
    if(GRASHOF[i,j]<ReL[i,j]) NUSSELT[i,j] <- 0.0112*REYNOLDS[i,j]^0.875 else
       NUSSELT[i,j] <- max(0.48*GRASHOF[i,j]^0.25,0.0112*REYNOLDS[i,j]^0.875)    
          
  # Energy flow between coat and air
  
  # convective heat transfer (W m-2)        
  CONVCOAT[i,j] = (ka[i,j] * NUSSELT[i,j]) / DIAMETER[i,j] *(TcoatC[i,j]-TAVGC[i,j]) / 
    (1-min(RAINFRAC,WEATHER$RAIN[i]*RAINFRAC/24)) 
          
  ###########################################################################################
  #                                         Synthesis                                       #
  ###########################################################################################
  
  # net energy balance (W m-2)        
  MetheatBAL[i,j] <- (MetheatCOAT[i,j] + SWR[i,j] - RAINEVAP[i,j] + LWRCOAT[i,j] - 
                        CONVCOAT[i,j])     
  
  # If the heat balance is positive, heat production is reduced         
  if(MetheatBAL[i,j] > 0.1) METABFEEDC[i,j] <- (METABFEEDC[i,j]-0.05*MetheatBAL[i,j]) 
  # If the heat balance is negative, heat production is increased 
  if(MetheatBAL[i,j] < -0.1) METABFEEDC[i,j] <-(METABFEEDC[i,j]-0.05*MetheatBAL[i,j]) 
  # heat metabolic processes under minimum heat release (W m-2)
  Metheatcold[i,j] <- METABFEEDC[i,j]                                                 
  # If the heat balance is close to zero, the loop is completed.
  if(MetheatBAL[i,j] < 0.1 & MetheatBAL[i,j] > -0.1) CHECKHEAT2[i,j] <- "CORRECT" else
    CHECKHEAT2[i,j] <- "FALSE" 
  # Break loop if heat balance is close to zero
  if(CHECKHEAT2[i,j] == "CORRECT") {break} 
          
  } # End of the repeat-loop that calculates Metheatcold[i,j]
      
  # End of the thermoregulation sub-model
  
  ###########################################################################################
  # 2.2                     Energy and protein utilization sub-model                        #
  ###########################################################################################
          
  ############################## 
  # Growth (genetic potential) #
  ##############################
        
  # TBW and carcass weight
  
  # Total body weight (kg); Gompertz curve, breed and sex-specific parameters
  TBW[i+1,j]           = (BIRTHW+(MAXW-BIRTHW)*exp(-CPAR*exp(TIME[i,j]/365*-DPAR)))-EPAR            
  # Carcass weight (kg)
  CARCW[i+1,j]         = TBW[i+1,j]*INCARC+TBW[i+1,j]*(LIBRARY[21]-INCARC)*
    (TBW[i+1,j]-BIRTHW)/(MAXW1[j]-BIRTHW)        
  # Derivative average daily gain (kg day-1)
  ADGHIGH[i+1,j]       = max(ADGHIGH[i,j],TBW[i+1,j]-TBW[i,j]) 
        
  # Bone and muscle weight
  
  # Derivative potential growth bone (kg day-1)
  DERBONE[i,j]         = CARCW[i+1,j]*min(BONEFRACMAX,BONEGROWTH1*CARCW[i+1,j]^-BONEGROWTH2)-
    CARCW[i,j]*min(BONEFRACMAX, BONEGROWTH1*CARCW[i,j]^-BONEGROWTH2) 
  # Derivative potential growth muscle (kg day-1)
  DERMUSCLE[i,j]       = DERBONE[i,j]*min(LIBRARY[22],MUSCLEGROWTH1*CARCW[i+1,j]^2+
                                            LIBRARY[22]/100*CARCW[i+1,j]+MUSCLEGROWTH2)  
  # Weight bones (kg); Standard formula: new state = previous state + rate of change over 
  # time
  BONETIS[i+1,j]       = BONETIS[i,j] + DERBONE[i,j]       
  # Weight muscle (kg)
  MUSCLETIS[i+1,j]     = MUSCLETIS[i,j] + DERMUSCLE[i,j]   
          
  # Intramuscular fat, miscellaneous fat and non-carcass tissues
  
  # Derivative potential growth intramuscular fat (kg day-1)
  DERINTRAMF[i,j]      = (IMFGROWTH1*(BONETIS[i+1,j]+MUSCLETIS[i+1,j])^2+IMFGROWTH2*
                            (BONETIS[i+1,j]+MUSCLETIS[i+1,j])-IMFGROWTH3)-
    (IMFGROWTH1*(BONETIS[i,j]+MUSCLETIS[i,j])^2+IMFGROWTH2*(BONETIS[i,j]+MUSCLETIS[i,j])-
       IMFGROWTH3)
  # Intramuscular fat weight (kg)
  INTRAMFTIS[i+1,j]    = INTRAMFTIS[i,j] + DERINTRAMF[i,j] 
  
  # Derivative potential growth subcutaneous and intermuscular fat (kg day-1)
  DERMISCFAT[i,j]      = (CARCW[i+1,j]-CARCW[i,j])-DERBONE[i,j] - DERMUSCLE[i,j] - 
    DERINTRAMF[i,j]
  # Subcutaneous and intermuscular fat weight (kg)
  MISCFATTIS[i+1,j]    = MISCFATTIS[i,j] + DERMISCFAT[i,j] 
  
  # Derivative potential growth non-carcass tissue (kg day-1)
  DERNONC[i,j]         = (TBW[i+1,j]*(1-RUMENFRAC)-TBW[i,j]*(1-RUMENFRAC))-
    (CARCW[i+1,j]-CARCW[i,j])
  # Non-carcass weight (kg)
  NONCARCTIS[i+1,j]    = NONCARCTIS[i,j] + DERNONC[i,j]    
  
  # Derivative rumen growth (kg day-1)
  DERRUMEN[i,j]        = (TBW[i+1,j]*RUMENFRAC-TBW[i,j]*RUMENFRAC) 
  # Rumen content weight (kg)
  RUMEN[i+1,j]         = RUMEN[i,j] + DERRUMEN[i,j]        
  
  # Potential growth all tissues and rumen (kg day-1)        
  DERTOTAL[i,j]        = DERBONE[i,j]+DERMUSCLE[i,j]+DERINTRAMF[i,j]+DERMISCFAT[i,j]+
    DERNONC[i,j]+DERRUMEN[i,j] 
          
  # Check in the source code: the sum of all tissues must equal the total body weight
  # calculated via the Gompertz curve.
  TBWCHECK[i+1,j]      = BONETIS[i+1,j]+ MUSCLETIS[i+1,j]+INTRAMFTIS[i+1,j]+
    MISCFATTIS[i+1,j]+NONCARCTIS[i+1,j]+RUMEN[i+1,j]  
          
  # Dynamics of lipid and protein in body tissues
  
  # Lipid fraction in bone (-)
  LIPFRACBONE[i,j]     = (LIBRARY[20]* log10(BONETIS[i,j]))/100                                                            
  # Lipid fraction in the  non carcass tissues (-)  
  LIPFRACNONC[i,j]     = min(LIPNONCMAX,max(LIPNONCMIN, LIPNONCMIN+(NONCARCTIS[i,j]/
                            (LIBRARY[13]*(1-RUMENFRAC)*(1-LIBRARY[21])))^2*LIPNONCMAX)) 
  # Protein fraction for accretion in the non carcass tissues (-)
  PROTFRACNONC[i,j]    = (PROTNONCM1*TBW[i,j] + PROTNONCM2) / 100 
          
  # Tissue concentrations (lipid and protein)
  
  # Lipid in bone tissue (kg); Standard formula: new state = previous state + rate of change 
  # over time 
  LIPIDBONE[i+1,j]     = LIPIDBONE[i,j] + DERBONE[i,j]*LIPFRACBONE[i,j] 
  # Protein in bone tissue (kg) 
  PROTBONE[i+1,j]      = PROTBONE[i,j] + DERBONE[i,j]*PROTFRACBONE 
  # NE required for bone growth (MJ per day)
  ENGRBONE[i+1,j]      = DERBONE[i,j]*LIPFRACBONE[i,j]*GELIPID/LIPIDEFF + DERBONE[i,j] * 
    PROTFRACBONE*GEPROT/PROTEFF    
          
  # Lipid in muscle tissue (kg) 
  LIPIDMUSCLE[i+1,j]   = LIPIDMUSCLE[i,j] + DERMUSCLE[i,j]*LIPFRACMUSCLE 
  # Protein in muscle tissue (kg) 
  PROTMUSCLE[i+1,j]    = PROTMUSCLE[i,j] + DERMUSCLE[i,j]*PROTFRACMUSCLE 
  # NE required for muscle growth (MJ per day)
  ENGRMUSCLE[i+1,j]    = DERMUSCLE[i,j]*LIPFRACMUSCLE*GELIPID/LIPIDEFF + DERMUSCLE[i,j] * 
    PROTFRACMUSCLE*GEPROT/PROTEFF 
          
  # Lipid in intramuscular fat tissue (kg)  
  LIPIDIMF[i+1,j]      = LIPIDIMF[i,j] + DERINTRAMF[i,j]*LIPFRACFAT 
  # Protein in intramuscular fat tissue (kg) 
  PROTIMF[i+1,j]       = PROTIMF[i,j] + DERINTRAMF[i,j]*PROTFRACFAT 
  # NE required for intramuscular fat growth (MJ per day)
  ENGRIMF[i+1,j]       = DERINTRAMF[i,j]*LIPFRACFAT*GELIPID/LIPIDEFF + DERINTRAMF[i,j] * 
    PROTFRACFAT*GEPROT/PROTEFF 
          
  # Lipid in subcutaneous and intermuscular fat tissue (kg)
  LIPIDFAT[i+1,j]      = LIPIDFAT[i,j] + DERMISCFAT[i,j]*LIPFRACFAT   
  # Protein in subcutaneous and intermuscular fat tissue (kg)
  PROTFAT[i+1,j]       = PROTFAT[i,j] + DERMISCFAT[i,j]*PROTFRACFAT 
  # NE required for subcutaneous and intermuscular fat growth (MJ per day)
  ENGRFAT[i+1,j]       = DERMISCFAT[i,j]*LIPFRACFAT*GELIPID/LIPIDEFF + DERINTRAMF[i,j] * 
    PROTFRACFAT*GEPROT/PROTEFF 
  
  # Lipid in non-carcass tissue (kg)        
  LIPIDNONC[i+1,j]     = LIPIDNONC[i,j] + DERNONC[i,j] * LIPFRACNONC[i,j]    
  # Protein in non-carcass tissue (kg) 
  PROTNONC[i+1,j]      = PROTNONC[i,j] + DERNONC[i,j] * PROTFRACNONC[i,j] 
  # NE required for non-carcass tissue growth (MJ per day)
  ENGRNONC[i+1,j]      = DERNONC[i,j]*LIPFRACNONC[i,j]*GELIPID/LIPIDEFF + DERNONC[i,j] * 
    PROTFRACNONC[i,j]*GEPROT/PROTEFF  
          
  # Total NE requirements to realise potential growth (MJ per day)
  ENGRTOTAL[i+1,j]     = ENGRBONE[i+1,j]+ENGRNONC[i+1,j]+ENGRMUSCLE[i+1,j]+ENGRIMF[i+1,j]+
    ENGRFAT[i+1,j]  
  # Copies NE requirements for growth (MJ per day)        
  ENGRTOTALORIG[i+1,j] = ENGRTOTAL[i+1,j]   
          
  # NE for growth increases if the body weight is lower than the genetic potential body 
  # weight, under compensatory growth, provided that adequate feed of high quality is 
  # available to the animal (i.e. energy and protein requirements can be met)
  
  # Weaning time (days after calving) is calculated before compensatory growth
  if(max(DIFFDAYS)>DIFFDAYSMAX) WEANINGTIME <- WEANINGTIMEvec[1] 
    if(max(DIFFDAYS)>DIFFDAYSMAX && j == 1 && CALFNR[max(1,i-GestPer),j] == 1) 
      WEANINGTIME <- WEANINGTIMEvec[1] else
      if(max(DIFFDAYS)>DIFFDAYSMAX && j == 1 && CALFNR[max(1,i-GestPer),j] == 2) 
        WEANINGTIME <- WEANINGTIMEvec[2] else
        if(max(DIFFDAYS)>DIFFDAYSMAX && j == 1 && CALFNR[max(1,i-GestPer),j] == 3 && 
           length(WEANINGTIMEvec)>2) WEANINGTIME <- WEANINGTIMEvec[3] else
          WEANINGTIME <- WEANINGTIME
                  
  # Compensatory growth is assumed not to occur before weaning. After weaning, the NE for
  # growth is the maximum of the highest NE requirement for growth previously calculated
  # in the lifetime of the animal (MJ per day).
  
  ENGRTOTALHIGH[i+1,j] <- if(TIME[i,j] <= WEANINGTIME) ENGRTOTALHIGH[i+1,j] <- 0 else 
     ENGRTOTALHIGH[i+1,j] <- max(ENGRTOTALHIGH[i,j], ENGRTOTAL[i+1,j])
   
  # The NE for (compensatory) growth is increased, depending on the total body weight and
  # genetic potential body weight (MJ per day)
  ENGRTOTALHIGH1[i+1,j] = ENGRTOTALHIGH[i+1,j] * min(1.0,(1-(TBWACT[i,j]/TBW[i,j]))*COMPFACT)  
          
  # Relative increase in NE for growth (-)        
  REL[i+1,j]  = ENGRTOTALHIGH1[i+1,j]/ENGRTOTAL[i+1,j] 
          
  # NE for growth (MJ per day)
  if(TIME[i,j] <= WEANINGTIME) ENGRTOTAL[i+1,j] <- ENGRTOTAL[i+1,j] else 
    ENGRTOTAL[i+1,j] <- max(ENGRTOTAL[i+1,j], min(1.0,(1-(TBWACT[i,j]/TBW[i,j]))*COMPFACT)*
                              ENGRTOTALHIGH[i+1,j])      
  # Percentage lipid in the carcass (measured from all carcass tissues, %)    
  LIPIDPERCCARC[i+1,j] = (LIPIDBONE[i+1,j]+LIPIDMUSCLE[i+1,j]+LIPIDIMF[i+1,j]+
                            LIPIDFAT[i+1,j])/(TBW[i+1,j]-NONCARCTIS[i+1,j]-RUMEN[i+1,j])*100 
         
          
  ###############    
  # Maintenance #
  ###############    
        
  # Energy balance
  
  # NE for (fasting) maintenance (kJ per animal per day)
  NEMAINT[i,j] = EBWACTMET[i,j] * NEm * LIBRARY[18]
  # NE for (fasting) maintenance (W m-2 coat). All NE for maintenance is converted to heat. 
  NEMAINTWM[i,j] = NEMAINT[i,j] * 1000 / (3600 * 24 * AREA[i,j])       
           
  # Protein balance
  
  # Dermal protein loss (CSIRO, 2007) (g protein day-1)
  PROTDERML[i,j] <- DERMPL* EBWACTMET[i,j]                              
  # Protein requirement for (fasting) maintenance (g protein day-1) (CSIRO, 2008). 
  # PROTNE: 2g N / 4.18 = 0.478 (CSIRO, 2008) 
  PROTMAINT[i,j] <- NEMAINT[i,j] * PROTNE / 1000 * NtoCP               
        
  #####################
  # Physical activity #
  #####################
            
  # Energy balance
  
  # NE for physical activity (kJ per animal per day), function of metabolic body weight
  # (EBWACTMET). It is assumed that animals in stables do not have requirements for 
  # physical activity due to limited movement.
  if(HOUSING[i] >= 1) NEPHYSACT[i,j] = EBWACTMET[i,j] * NEpha else NEPHYSACT[i,j] <- 0                          
  # Physical activity (W m-2 coat), all energy converted to heat
  NEPHYSACTWM[i,j] = NEPHYSACT[i,j] * 1000 / (3600 * 24 * AREA[i,j])                            
            
  # Protein balance
  
  # Protein requirement for (fasting) maintenance (g protein day-1) (CSIRO, 2008). 
  # PROTNE: 2g N / 4.18 = 0.478 (CSIRO, 2008) 
  PROTPHACT[i,j] <- NEPHYSACT[i,j] * PROTNE / 1000 * NtoCP                                
      
  #############
  # Gestation #
  #############    
  
  # The difference in days where milk is produced in experiments with Holstein-Friesian cows
  # in the Netherlands is used to determine the calving interval. If a cow does not produce 
  # milk for 30 days, the lactation is assumed to end. Once milk production starts this is 
  # from a new lactation. The calving interval is calculated in days.
  if(max(DIFFDAYS)>DIFFDAYSMAX && j == 1 && CALFNR[max(1,i-GestPer),j] == 1) 
    GESTINTERVAL <- WEANINGTIMEvec[1] + GESTINTERVALvec[1] else
    if(max(DIFFDAYS)>DIFFDAYSMAX && j == 1 && CALFNR[max(1,i-GestPer),j] == 2 && 
       length(GESTINTERVALvec)>1) 
      GESTINTERVAL <- WEANINGTIMEvec[2] + GESTINTERVALvec[2] else
       GESTINTERVAL <- 365
  
  # Cows have to meet six conditions to conceive. A condition that is met is indicated by
  # a 1, and a condition that is not met is indicated by a 0.
  
  # Condition 1: Cows can conceive if their body weight is higher that an minimum percentage
  # of the total body weight (GEST1) 
  if(TBWACT[i,j]<(LIBRARY[17]*MAXW1[j])) GEST1[i,j] <- 0 else GEST1[i,j] <- 1                   
  
  # Condition 2: Cows cannot conceive when already gestating (GEST2). 
  if(CALFTBW[i,j]==0) GEST2[i,j] <- 1 else GEST2[i,j] <- 0
  
  # Condition 3: A cow can conceive when the last calving happened longer ago than the
  # minimum gestation interval (GEST3)   
  if(sum(CALFTBW[i,j])-sum(CALFTBW[max(0,i-(GESTINTERVAL-GestPer-1)),j])==0) 
    GEST3[i,j] <- 1 else GEST3[i,j] <- 0
  
  # Condition 4: Cows can only conceive when the fraction fat tissues in the carcass (i.e.
  # body reserves) meet the minimum level specified in LIBRARY[19] (GEST4). Hence, if the 
  # body condition score of a cow is too low, the calving interval will be longer.
  if((MISCFATTISACT[i,j]+INTRAMFTISACT[i,j])/(TBWACT[i,j]-NONCARCTISACT[i,j]-RUMEN[i,j]) < 
     LIBRARY[19]) GEST4[i,j] <- 0 else GEST4[i,j] <- 1                
            
  # Condition 5 (optional): Cattle conceive at a specific date in the year (STDOYBASE). This
  # option can be used if calving dates are known, or if calving is seasonable (GEST5)
  if(DOY[i] == STDOYBASE+(GESTINTERVAL-GestPer)-
     floor((STDOYBASE+(GESTINTERVAL-GestPer))/365)*365) GEST5[i,j] <- 1 else GEST5[i,j] <- 0               
            
  if(TIME[i,j]==1) GEST5[i,j] <- GEST5[i,j] else 
    if(CALFNR[i-1,j] ==1 || CALFNR[i-1,j] ==2) GEST5[i,j] <- 1 else GEST5[i,j] <- GEST5[i,j]
  
  # In the experiments with Holstein-Friesian cattle in the Netherlands, cows cannot 
  # conceive before the start of the experiment.          
  if(TIME[i,j]<MEASUREDOUTPUT$Time[1]-290) GEST5[i,j] <- 0
  # Cows cannot conceive beyond the maximum age for conception
  if(TIME[i,j]/365<MAXCONCAGE) GEST5[i,j] <- GEST5[i,j] else GEST5[i,j] <- 0
  
  # Condition 6: If less than eight calves are born before the maximum conception age, the 
  # maximum number of calves is reduced.
  # In addition, a maximum number of calves can be set per cow (GEST6).
  if(TIME[i,j]/365>MAXCONCAGE) MAXCALFNR <- CALFNR[i,j] else MAXCALFNR <- MAXCALFNR             
  if(CALFNR[i,j] < MAXCALFNR) GEST6[i+1,j] <- 1 else GEST6[i+1,j] <- 0                          
  
  # All six conditions have to be met by a reproductive animal to conceive (GEST)          
  GEST[i,j] <- GEST1[i,j] * GEST2[i,j] * GEST3[i,j] * GEST4[i,j] * GEST5[i,j] * 
    GEST6[i+1,j] * LIBRARY[14] * REPRODUCTIVE[j]  
            
  # Count number of born and unborn calves (#) 
  CALFNR[i+1,j] = GEST[i,j] + CALFNR[i,j]                                                       
  
  # Counts the days after conception (days)          
  if(GEST[i,j]==1) GESTDAY[i+1,j] <- 1 else if (GESTDAY[i,j]>0) 
    GESTDAY[i+1,j] <- GESTDAY[i,j] + 1 else GESTDAY[i+1,j] <- 0 
  if(GEST[i,j]==1) GESTDAY[i,j] <-0  
            
  # The breed- and sex-specific birth weight of a calf is selected.  The birth weight of 
  # male and female calves is different.
  if(BREED == 1)  BIRTHW1[i,j] <- LIBRARY10[5]-SEX[CALFNR[i,j]+1]*
    (LIBRARY10[5]-LIBRARY11[5]) else    
    if(BREED == 2)  BIRTHW1[i,j] <- LIBRARY20[5]-SEX[CALFNR[i,j]+1]*
    (LIBRARY20[5]-LIBRARY21[5]) else  
      if(BREED == 3)  BIRTHW1[i,j] <- LIBRARY30[5]-SEX[CALFNR[i,j]+1]*
    (LIBRARY30[5]-LIBRARY31[5]) else
        if(BREED == 4)  BIRTHW1[i,j] <- LIBRARY40[5]-SEX[CALFNR[i,j]+1]*
    (LIBRARY40[5]-LIBRARY41[5]) else
          if(BREED == 5)  BIRTHW1[i,j] <- LIBRARY50[5]-SEX[CALFNR[i,j]+1]*
    (LIBRARY50[5]-LIBRARY50[5])

  # Net energy is required for gestation after conception (MJ day-1) (Fox et al, 1988)          
  if(GESTDAY[i+1,j] >=1 & GESTDAY[i+1,j] <= GestPer)  
  NEREQGEST[i,j]    <- ((9.527001*(0.0000000681-0.000000000197*GESTDAY[i,j])*
                           (exp((0.0885-0.0001238*GESTDAY[i,j])*GESTDAY[i,j])) + 
                           5.505*((0.00003452-0.0000001094*GESTDAY[i,j])*
                                    (exp((0.0589-0.00009334*GESTDAY[i,j])*
                                           GESTDAY[i,j]))))*CALTOJOULE*10*
                          (BIRTHW1[i,j]/37.2)) else NEREQGEST[i,j] = 0       
  
  # Efficiency energy accretion gestation is only 14% in total (Jarrige, 1989, Rattray et  
  # al., 1974). This ends up in the body tissues of the developing calf. Another 9.4% is   
  # used for extra body tissue of the reproductive cow (i.e. concepta) (Jarrige, 1989)          
  
  # Heat production from gestation (MJ per day)
  HEATGEST[i,j] = NEREQGEST[i,j] * NEIEFFGEST             
  # Cumulative energy for a gestation (MJ per gestation period)
  NEREQGESTADD[i+1,j] = NEREQGESTADD[i,j]+NEREQGEST[i,j]                                 
            
  # Total NE requirements for gestation, breed and sex-specific (MJ per calf). The total NE
  # requirements are asumed to be proportional to the birth weight.
  if(BREED == 1 & SEX[CALFNR[i,j]+1] == 0) 
    NEREQGESTTOT = 58.658*LIBRARY10[7]+0.5502 else 
    if(BREED == 1 & SEX[CALFNR[i,j]+1] == 1) 
      NEREQGESTTOT = 58.658*LIBRARY11[7]+0.5502 else
      if(BREED == 2 & SEX[CALFNR[i,j]+1] == 0) 
        NEREQGESTTOT = 58.658*LIBRARY20[7]+0.5502 else
        if(BREED == 2 & SEX[CALFNR[i,j]+1] == 1) 
          NEREQGESTTOT = 58.658*LIBRARY21[7]+0.5502 else
          if(BREED == 3 & SEX[CALFNR[i,j]+1] == 0) 
            NEREQGESTTOT = 58.658*LIBRARY30[7]+0.5502 else
            if(BREED == 3 & SEX[CALFNR[i,j]+1] == 1) 
              NEREQGESTTOT = 58.658*LIBRARY31[7]+0.5502 else
              if(BREED == 4 & SEX[CALFNR[i,j]+1] == 0) 
                NEREQGESTTOT = 58.658*LIBRARY40[7]+0.5502 else
                if(BREED == 4 & SEX[CALFNR[i,j]+1] == 1) 
                  NEREQGESTTOT = 58.658*LIBRARY41[7]+0.5502 else
                  if(BREED == 5 & SEX[CALFNR[i,j]+1] == 0) 
                    NEREQGESTTOT = 58.658*LIBRARY50[7]+0.5502
  
  # Weight of the developing calf, assumed to be proportional to NE for gestation                                                
  CALFTBW[i+1,j] = NEREQGESTADD[i+1,j]/NEREQGESTTOT*BIRTHW1[i,j] 
      
  # Weight of the developing calf is calculated during gestation (kg)
  if(GESTDAY[i+1,j] >=1 & GESTDAY[i+1,j] <= GestPer) 
    CALFTBW[i+1,j] <- CALFTBW[i+1,j] else CALFTBW[i+1,j] <- 0  
  # NE requirements for gestation are re-set to zero after birth (MJ per gestation)
  if((CALFTBW[i,j]-CALFTBW[i+1,j])>BIRTHW1[i,j]-1) NEREQGESTADD[i+1,j] <- 0                                     
            
  # Weight of concepta are added to the total body weight of a cow, and maintenance 
  # requirements for this additional weight are assumed to be covered by the NE requirements
  # for gestation (Jarrige, 1986, p. 99)
  TBWADD[i+1,j] = CALFTBW[i+1,j] * FtoConcW    
            
  # Protein balance
  
  # Protein requirements for gestation are assumed to be proportional to NE requirements
  # for gestation (CSIRO, 2007), conversion 4.322 g protein per MJ NE (g protein day-1) 
  PROTGESTG[i,j]   <- NEREQGEST[i,j] * CPGEST    
  
  # It is assumed that formula given in CSIRO (2007) gives the gross protein requirement for
  # gestation    
        
  # Scaling factor for feed intake during gestation, based on Johnson et al (2016, J. Dairy 
  # Sc. 99:1605-1618), equation 18
  # Increased intake during lactation is assumed to be proportional to the increase in  
  # energy requirements for gestation. Intake is controlled by the maximum digestion 
  # capacity.
  
  # Relative increase in maximum digestion capacity during gestation (-)           
  if(GESTDAY[i,j] >0 && GESTDAY[i,j] <= GestPer ) 
    SFFeedIntG[i,j] <- (NEMAINT[i,j]+NEPHYSACT[i,j]+NEREQGEST[i,j]*1000)/
    (NEMAINT[i,j]+NEPHYSACT[i,j]) else SFFeedIntG[i,j] <- 1
            
  ###################    
  # Milk production #
  ###################
            
  # Milk production starts when the calf is born, and is indicated by MILKDAYST
  if(GESTDAY[i,j] == GestPer-1) MILKDAYST[i,j] <- 1 else MILKDAYST[i,j] <- 0              
  
  # Start days after calving          
  if (MILKDAYST[i,j]==1) ADDMILK2[i,j] <- 1 else ADDMILK2[i,j] <- 0
  # Add up days after calving
  if (MILKDAY[i,j] >0 ) ADDMILK1[i,j] <- 1 else ADDMILK1[i,j] <- 0                        
  
  # Days in milk production / after calving (days)          
  MILKDAY[i+1,j] = MILKDAY[i,j] + ADDMILK1[i,j] + ADDMILK2[i,j]                           
  
  # Cow and calf are separated during weaning, and milk production is zero afterwards          
  if(MILKDAY[i+1,j] == WEANINGTIME+1) MILKDAY[i+1,j] <-0                                  
  
  # Calculates the number of calves born per cow (#)          
  if(CALFNR[i,j] == 0) CALFLIVENR[i,j] <- 0 else CALFLIVENR[i,j] <- CALFNR[i-GestPer,j]      
  # Calculates the number of weaned calves per cow
  if(CALFLIVENR[i,j] == 0) CALFWEANNR[i,j] <- 0 else 
    CALFWEANNR[i,j] <- CALFLIVENR[i-WEANINGTIME,j] 
  
  # Conversion from days in milk to weeks in milk           
  MILKWEEK[i,j] = MILKDAY[i,j]/7                                                          
  
  # Maximum milk production according to the breed-specific genetic potential (L day-1). 
  # The equation used is the Wood's equation (Wood, 1967)           
  if(MILKWEEK[i,j] >0) POTMILKPROD[i,j] <- MILKPARA*MILKDAY[i,j]^MILKPARB*
    exp(-MILKPARC*MILKDAY[i,j]) else POTMILKPROD[i,j] <- 0 
  
  # Gross energy in milk (MJ GE kg-1). This empirical equation accounts for fat and protein 
  # dynamics during the lactation. See also Equation 5 in the paper describing LiGAPS-
  # Dairy
  GEMILK[i,j] = GEMILK5 * (GEMILK1 - GEMILK2*MILKDAY[i,j]^GEMILK3 * 
                             exp(-GEMILK4*MILKDAY[i,j]))  
            
  # Scaling factor for feed intake during lactation, based on Johnson et al. (2016, J. Dairy 
  # Sc. 99:1605-1618), Equation 20 and 21. Feed intake is controlled by the maximum 
  # digestion capacity.
  
  # The curvature coefficient alfa is dependent on the peak in potential intake, as 
  # explained in Equation 25 of Johnson et al. (2016)          
  if(MILKDAY[i,j] < tmaxlact) ALFA[i,j] <- ALFA1 else ALFA[i,j] <- ALFA2
  # The relative increase in feed intake during lactation (SFFeedIntL) is calculated from
  # equation 20 and 21 of Johnson et al. (2016).
  if(MILKDAY[i,j] > 0) SFFeedIntL[i,j] <- 1+(fimaxlact-1)*(MILKDAY[i,j]/tmaxlact)^ALFA[i,j]*
    exp(-ALFA[i,j]*(MILKDAY[i,j]/tmaxlact-1)) else SFFeedIntL[i,j] <- 1
  
  # The degree of fat catabolism (in the beginning of the lactation) and gain of body 
  # tissues (in the last phases of the lactation) was calculated from Equation 16 of Johnson
  # et al. (2016). BrevenLG indicates the day in the lactation where a cow has to shift from 
  # fat catabolism to (re)gaining body tissues (i.e. body reserves)
  if(MILKDAY[i,j] > 0) RatioLG[i,j] <- (MILKDAY[i,j]-BrevenLG)/(MILKDAY[i,j]+BrevenLG) else 
    RatioLG[i,j] <- NA
  
  # Max GE milk production (MJ GE day-1)      
  GEMILKTOT[i,j] = GEMILK[i,j] * POTMILKPROD[i,j]         
  # Max ME milk production (MJ ME day-1)
  MEMILKCALF[i,j] = MILKDIG * GEMILKTOT[i,j]              
  # NE requirement for milk production for reproductive cow (MJ NE day-1)
  NEMILKCOW[i,j] = GEMILKTOT[i,j] / NEEFFMILK             
            
  #################
  #    Growth     #
  #################
        
  # Compensatory growth: potential growth can be exceeded under compensatory growth. 
  # Compensatory growth is assumed to be proportional to the difference between actual TBW 
  # and genetic potential total body weight of the animal.
  
  # Compensatory growth factor for the non-carcass tissue (dimensionless) 
  COMPGROWTH1[i,j] <- min(10,  max(1,1+(1/(NONCARCTISACT[i,j]/NONCARCTIS[i,j])^2-1))) 
  # Compensatory growth factor for the bone tissue (dimensionless)
  COMPGROWTH2[i,j] <- min(COMPFACTTIS,  max(1,BONETIS[i,j]/BONETISACT[i,j]))       
  # Compensatory growth factor for the muscle tissue (dimensionless)
  COMPGROWTH3[i,j] <- min(COMPFACTTIS,  max(1,MUSCLETIS[i,j]/MUSCLETISACT[i,j]))   
  # Compensatory growth factor for the intramuscular tissues (dimensionless)
  COMPGROWTH4[i,j] <- min(COMPFACTTIS,  max(1,INTRAMFTIS[i,j]/INTRAMFTISACT[i,j])) 
  # Compensatory growth factor for the subcutaneous and intermuscular tissues 
  # (dimensionless)
  COMPGROWTH5[i,j] <- min(COMPFACTTIS,  max(1,MISCFATTIS[i,j]/MISCFATTISACT[i,j])) 
        
  # Compensatory growth factor, based on a weighted average of body tissues (dimensionless)
  COMPGROWTH[i,j]  <- COMPGROWTH1[i,j] * (NONCARCTISACT[i,j]/(NONCARCTISACT[i,j]+
                      BONETISACT[i,j]+MUSCLETISACT[i,j]+INTRAMFTISACT[i,j]+
                        MISCFATTISACT[i,j])) + 
                      COMPGROWTH2[i,j] * (BONETISACT[i,j]   /(NONCARCTISACT[i,j]+
                      BONETISACT[i,j]+MUSCLETISACT[i,j]+INTRAMFTISACT[i,j]+
                        MISCFATTISACT[i,j])) +
                      COMPGROWTH3[i,j] * (MUSCLETISACT[i,j] /(NONCARCTISACT[i,j]+
                      BONETISACT[i,j]+MUSCLETISACT[i,j]+INTRAMFTISACT[i,j]+
                        MISCFATTISACT[i,j])) +
                      COMPGROWTH4[i,j] * (INTRAMFTISACT[i,j]/(NONCARCTISACT[i,j]+
                      BONETISACT[i,j]+MUSCLETISACT[i,j]+INTRAMFTISACT[i,j]+
                        MISCFATTISACT[i,j])) +
                      COMPGROWTH5[i,j] * (MISCFATTISACT[i,j]/(NONCARCTISACT[i,j]+
                      BONETISACT[i,j]+MUSCLETISACT[i,j]+INTRAMFTISACT[i,j]+
                        MISCFATTISACT[i,j])) 
        
  # Lipid fraction of bone tissue accreted (-)
  LIPFRACBONEACT[i,j] = max(LIPBONE1,(LIPBONE2*log(BONETISACT[i,j]) + LIPBONE3)/100)                                                          
  # Lipid fraction in the non-carcass tissue (-)
  LIPFRACNONCACT[i,j] = (LIPNONC1*NONCARCTISACT[i,j]^3 - LIPNONC2*NONCARCTISACT[i,j]^2 + 
        LIPNONC3*NONCARCTISACT[i,j] - LIPNONC4)/100 * 
        (3.916E-10* ((1-LIBRARY[21]-RUMENFRAC)*LIBRARY[13])^4 - 
        7.058E-07* ((1-LIBRARY[21]-RUMENFRAC)*LIBRARY[13])^3 + 
        4.868E-04* ((1-LIBRARY[21]-RUMENFRAC)*LIBRARY[13])^2 - 
        1.593E-01* ((1-LIBRARY[21]-RUMENFRAC)*LIBRARY[13]) + 2.286E+01)
  # Protein fraction in the non-carcass tissues (-)
  PROTFRACNONCACT[i,j]    = ((PROTNONC1)*NONCARCTISACT[i,j]^4 - 
                               (PROTNONC2)*NONCARCTISACT[i,j]^3 + 
                               (PROTNONC3)*NONCARCTISACT[i,j]^2 - 
                               (PROTNONC4)*NONCARCTISACT[i,j] + (PROTNONC5))/100 
        
  # Under energy limitation (related to feed quality or quantity limitation), tissues get 
  # energy according to their position in the hierarchy:
  # 1. Non carcass tissue (with organs) 2. Bone tissue (in the carcass) 3. Muscle tissue 
  # 4. Intramuscular fat tissue 5. Subcutaneous and intermuscular fat tissue
  # However, when the body reserves of an animal are almost used, a key priority is to 
  # allocate energy to the fat tissues to prevent that the fat tissues are fully used.
        
  # Additional energy to recover depleted subcutaneous and intermuscular fat tissues (-) 
  FATCOMP[i,j] <- max(0, FATTISCOMP-MISCFATTISACT[i,j]/MISCFATTIS[i,j])*
    (TBWACT[i,j]*(1-RUMENFRAC))^0.75*FATFACTOR 
            
  ###########################################################################################
  # 2.3                       Feed intake and digestion sub-model                           #
  ###########################################################################################
        
  # Maximum digestion capacity (Fill units per animal per day). This equation accounts for an
  # rumen development during the first months in the life of a calf.
  PHFEEDINT[i,j] = TBWACT[i,j]^0.75 * PHFEEDCAP/1000 * max(SFFeedIntL[i,j]) * 
    max(0,min(1, (RUMENDEV1 * TIME[i,j] -RUMENDEV2)))   
        
  # This section allows to feed percentages of the total body weight. Cattle weights at the 
  # start of the experiment were available for experiments 8 and 22 ('t Gen and Meijer et al.
  # respectively). The feed availability of cattle is reduced between day 500 and the start
  # of the experiment if they exceed the measured weight at the start of the experiment.
  # This happens both for feed type 1 and feed type 2.
  if(EXPERIMENT==8 || EXPERIMENT==22){
  if(j==1 && TIME[i]>500 && TIME[i]<MEASUREDOUTPUT$Time[1] && 
     TBWACT[i,j]>MEASUREDOUTPUT$WeightKG[1]) 
    FEED1QNTY[i,j] <- FEED1QNTY[i,j]*0.23*8.0/FEED1QNTY[i,j] 
  
  if(j==1 && TIME[i]>500 && TIME[i]<MEASUREDOUTPUT$Time[1] && 
     TBWACT[i,j]>MEASUREDOUTPUT$WeightKG[1]) 
    FEED2QNTY[i,j] <- FEED2QNTY[i,j]*0.23*8.0/FEED1QNTY[i,j]} 
        
  # Feed intake is reduced when rumen digestive capacity is exceeded
  
  # Feed type 1 and 2 are put together in a feed mix
  
  # Fill units for feed type 1 (fill units per animal per day)
  FUFEED1[i,j] <- FEED1QNTY[i,j]*FEED1[i,2] 
  # Maximum feed intake feed type 1 (kg DM per animal per day) given the fill units
  if((PHFEEDINT[i,j] - FUFEED1[i,j]) < 0) 
    FEED1QNTYA[i,j] <- PHFEEDINT[i,j]/(FEED1fr*FEED1[i,2]+(1-FEED1fr)*FEED2[i,2])*
    FEED1fr else FEED1QNTYA[i,j] <- FEED1QNTY[i,j]  
  
  # Fill units for feed type 1 and 2 (fill units per animal per day)      
  FUFEED2[i,j] <- FEED1QNTYA[i,j]*FEED1[i,2] + FEED2QNTY[i,j]*FEED2[i,2] 
  # Maximum feed intake feed type 2 (kg DM per animal per day) given the fill units
  if((PHFEEDINT[i,j] - FUFEED2[i,j]) < 0) 
    FEED2QNTYA[i,j] <- PHFEEDINT[i,j]/(FEED1fr*FEED1[i,2]+(1-FEED1fr)*FEED2[i,2])*
    (1-FEED1fr) else FEED2QNTYA[i,j] <- FEED2QNTY[i,j]   
        
  # Fill units for feed type 1, 2 and 3 (fill units per animal per day)  
  FUFEED3[i,j] <- FEED1QNTYA[i,j]*FEED1[i,2] + FEED2QNTYA[i,j]*FEED2[i,2] + 
    FEED3QNTY[i,j]*FEED3[i,2] 
  # Maximum feed intake feed type 3 (kg DM per animal per day) given the fill units
  if((PHFEEDINT[i,j] - FUFEED3[i,j]) < 0) 
    FEED3QNTYA[i,j] <- max(0,(PHFEEDINT[i,j]-FUFEED2[i,j])/FEED3[i,2]) else 
      FEED3QNTYA[i,j] <- FEED3QNTY[i,j] # Maximum feed intake feed 3 (kg DM)   
  
  # Fill units left for feed type 4 (fill units per animal per day)        
  FUFEED4[i,j] <- FEED1QNTY[i,j]*FEED1[i,2] + FEED2QNTY[i,j]*FEED2[i,2] + 
    FEED3QNTY[i,j]*FEED3[i,2] + FEED4QNTY[i,j]*FEED4[2]
  # Maximum feed intake feed type 4 (kg DM per animal per day) given the fill units
  if((PHFEEDINT[i,j] - FUFEED4[i,j]) < 0) 
    FEED4QNTYA[i,j] <- max(0,(PHFEEDINT[i,j]-FUFEED3[i,j])/FEED4[2]) else 
      FEED4QNTY[i,j] <- FEED4QNTY[i,j]   
  FEED4QNTYA[i,j] <- min(FEED4QNTY[i,j], FEED4fr*PHFEEDINT[i,j]/FEED4[2]) 
       
  # Rumen fill classes, calculated according to Chilibroste et al. (1997)
  # Fill classes indicate the fraction rumen fill for all feed types (i.e. total diet)
  # The rumen fill affects the passage rate of the feed types. Rumen fill is represented
  # by four classes, from 1 (rumen fill >85%) to 4 (rumen fill <0.45)
  if(FUFEED4[i,j] > PHFEEDINT[i,j]* 0.85) PASSAGE[i,j] <- 1 else         
    if(FUFEED4[i,j] > PHFEEDINT[i,j]* 0.65 & FUFEED4[i,j] < PHFEEDINT[i,j]* 0.85) 
      PASSAGE[i,j] <- 2 else
      if(FUFEED4[i,j] > PHFEEDINT[i,j]* 0.45 & FUFEED4[i,j] < PHFEEDINT[i,j]* 0.65) 
        PASSAGE[i,j] <- 3 else PASSAGE[i,j] <- 4
        
  # Average crude protein content of the total diet (g kg-1 DM), based on digstion capacity
  CPAVG[i,j] <- (FEED1QNTYA[i,j]*FEED1[i,16] + FEED2QNTYA[i,j]*FEED2[i,16] + 
                 FEED3QNTYA[i,j]*FEED3[i,16] + FEED4QNTYA[i,j]*FEED4[16]) /   
                 (FEED1QNTYA[i,j] + FEED2QNTYA[i,j] + FEED3QNTYA[i,j] + FEED4QNTYA[i,j]) 
        
      
  ###########################################################################################
  # 2.4 Integration thermoregulation, energy and protein utilization, and feed digestion    #  
  #     sub-models                                                                          #
  ###########################################################################################
          
  # Initial parameters for the repeat loop that integrates the sub-models. Initial values are 
  # re-calculated in other parts of this source code.
  
  # reduction in NE availability due to heat stress is zero (no heat stress)
  REDHP[i,j] <- 0               
  # increase in heat prodcution due to cold stress is zero (no cold stress)  
  HEATIFEEDGROWTHC[i,j] <- 0
  # first guesstimate for feed intake (kg DM head-1 day-1). This guesstimate is too high,
  # but it is reduced later.
  FEEDINTAKE[i,j] <- 40         
  # number of iterations of this loop
  REPS[i,j] <- 0                
  # assumption that the animal is fed above the maintenance level
  REDMAINT[i,j] <- 0            
                 
  repeat { # Start of the loop where sub-models are integrated 
        
  # minimum feed quantities based on rumen digestive capacity, feed quantity offered and feed
  # fractions in the diet.
  
  # Intake feed type 1 (kg DM per day)        
  if(REPRODUCTIVE[j] == 1) 
    FEED1QNTY[i,j] <- max(0,min(FEED1QNTYA[i,j], FEED1fr*FEEDINTAKE[i,j])) else
    if(PRODUCTIVE[j] == 1 && SEX[j] == 0) 
      FEED1QNTY[i,j] <- max(0,min(FEED1QNTYA[i,j], FEED1fr*FEEDINTAKE[i,j])) * 1 else
      if(PRODUCTIVE[j] == 1 && SEX[j] == 1) 
        FEED1QNTY[i,j] <- max(0,min(FEED1QNTYA[i,j], FEED1fr*FEEDINTAKE[i,j])) * 1 else
        FEED1QNTY[i,j] <- max(0,min(FEED1QNTYA[i,j], FEED1fr*FEEDINTAKE[i,j]))
  
  # Intake feed type 2 (kg DM per day)           
  if(REPRODUCTIVE[j] == 1) 
    FEED2QNTY[i,j] <- max(0,min(FEED2QNTYA[i,j], FEED2fr*FEEDINTAKE[i,j], 
                                FEEDINTAKE[i,j]-FEED1QNTY[i,j])) else
    if(PRODUCTIVE[j] == 1 && SEX[j] == 0) 
      FEED2QNTY[i,j] <- max(0,min(FEED2QNTYA[i,j], FEED2fr*FEEDINTAKE[i,j], 
                                  FEEDINTAKE[i,j]-FEED1QNTY[i,j])) * 1 else
      if(PRODUCTIVE[j] == 1 && SEX[j] == 1) 
        FEED2QNTY[i,j] <- max(0,min(FEED2QNTYA[i,j], FEED2fr*FEEDINTAKE[i,j], 
                                    FEEDINTAKE[i,j]-FEED1QNTY[i,j])) * 1 else
        FEED2QNTY[i,j] <- max(0,min(FEED2QNTYA[i,j], FEED2fr*FEEDINTAKE[i,j], 
                                    FEEDINTAKE[i,j]-FEED1QNTY[i,j]))
              
  # Intake feed type 3 (kg DM per day)            
  if(REPRODUCTIVE[j] == 1) 
    FEED3QNTY[i,j] <- max(0,min(FEED3QNTYA[i,j], FEED3fr*FEEDINTAKE[i,j], 
                                FEEDINTAKE[i,j]-FEED1QNTY[i,j]-FEED2QNTY[i,j])) else
    if(PRODUCTIVE[j] == 1 && SEX[j] == 0) 
      FEED3QNTY[i,j] <- max(0,min(FEED3QNTYA[i,j], FEED3fr*FEEDINTAKE[i,j], 
                                  FEEDINTAKE[i,j]-FEED1QNTY[i,j]-FEED2QNTY[i,j])) * 1 else
      if(PRODUCTIVE[j] == 1 && SEX[j] == 1) 
        FEED3QNTY[i,j] <- max(0,min(FEED3QNTYA[i,j], FEED3fr*FEEDINTAKE[i,j], 
                                    FEEDINTAKE[i,j]-FEED1QNTY[i,j]-FEED2QNTY[i,j])) * 1 else
        FEED3QNTY[i,j] <- max(0,min(FEED3QNTYA[i,j], FEED3fr*FEEDINTAKE[i,j], 
                                    FEEDINTAKE[i,j]-FEED1QNTY[i,j]-FEED2QNTY[i,j]))
    
  # Intake feed type 4 (kg DM per day)               
  if(REPRODUCTIVE[j] == 1) 
    FEED4QNTY[i,j] <- max(0,min(FEED4QNTYA[i,j], FEED4fr*FEEDINTAKE[i,j], 
                                FEEDINTAKE[i,j]-FEED1QNTY[i,j]-FEED2QNTY[i,j]-
                                  FEED3QNTY[i,j])) else
    if(PRODUCTIVE[j] == 1 && SEX[j] == 0) 
      FEED4QNTY[i,j] <- max(0,min(FEED4QNTYA[i,j], FEED4fr*FEEDINTAKE[i,j], 
                                  FEEDINTAKE[i,j]-FEED1QNTY[i,j]-FEED2QNTY[i,j]-
                                    FEED3QNTY[i,j])) else
      if(PRODUCTIVE[j] == 1 && SEX[j] == 1) 
        FEED4QNTY[i,j] <- max(0,min(FEED4QNTYA[i,j], FEED4fr*FEEDINTAKE[i,j], 
                                    FEEDINTAKE[i,j]-FEED1QNTY[i,j]-FEED2QNTY[i,j]-
                                      FEED3QNTY[i,j])) else
        FEED4QNTY[i,j] <- max(0,min(FEED4QNTYA[i,j], FEED4fr*FEEDINTAKE[i,j], 
                                    FEEDINTAKE[i,j]-FEED1QNTY[i,j]-FEED2QNTY[i,j]-
                                      FEED3QNTY[i,j]))
            
  # Total feed intake (kg DM day-1)        
  FEEDQNTY[i,j] <- FEED1QNTY[i,j] + FEED2QNTY[i,j] + FEED3QNTY[i,j] + FEED4QNTY[i,j]     
  
  # Crude protein content of the diet (g kg DM-1 feed)        
  if(FEEDQNTY[i,j] == 0) CPAVG[i,j] <- 0 else 
    CPAVG[i,j] <- (FEED1QNTY[i,j]*FEED1[i,16] + FEED2QNTY[i,j]*FEED2[i,16] + 
                     FEED3QNTY[i,j]*FEED3[i,16] + FEED4QNTY[i,j]*FEED1[16]) / FEEDQNTY[i,j] 
  
  # Fraction feed type 1 in diet (-) 
  if(TIME[i,j] <= 14 | FEEDQNTY[i,j] == 0) FRACFEED1[i,j] <- 0 else 
    FRACFEED1[i,j] <- FEED1QNTY[i,j]/FEEDQNTY[i,j] 
  # Fraction feed type 2 in diet (-) 
  if(TIME[i,j] <= 14 | FEEDQNTY[i,j] == 0) FRACFEED2[i,j] <- 0 else 
    FRACFEED2[i,j] <- FEED2QNTY[i,j]/FEEDQNTY[i,j] 
  # Fraction feed type 3 in diet (-) 
  if(TIME[i,j] <= 14 | FEEDQNTY[i,j] == 0) FRACFEED3[i,j] <- 0 else 
    FRACFEED3[i,j] <- FEED3QNTY[i,j]/FEEDQNTY[i,j] 
  # Fraction feed type 4 in diet (-) 
  if(TIME[i,j] <= 14 | FEEDQNTY[i,j] == 0) FRACFEED4[i,j] <- 0 else 
    FRACFEED4[i,j] <- FEED4QNTY[i,j]/FEEDQNTY[i,j] 
          
  ###########################################################################################
  #                     Feed intake and digestion sub-model (integration)                   #             
  ###########################################################################################
          
  # maximum feed intake in kg, based on fill units ([FU FU-1] kg DM per day)
  PHFEEDINTKG[i,j] <- PHFEEDINT[i,j]/(FRACFEED1[i,j]*FEED1[i,2]+FRACFEED2[i,j]*FEED2[i,2]+
                                        FRACFEED3[i,j]*FEED3[i,2]+FRACFEED4[i,j]*FEED4[2])   
          
  # Carbohydrate digestion (INSC = insoluble, non-structural carbohydrates, which is assumed
  # to be predominantly starch) 
  
  # Digestion insoluble, non-structural carbohydrates (g day-1)        
  INSC[i,j] <- FEED1QNTY[i,j] * FEED1[i,4] * FEED1[i,9]  / 
               (FEED1[i,9]  + FEED1[i,12]*PASSRED[PASSAGE[i,j]]) + 
               FEED2QNTY[i,j] * FEED2[i,4] * FEED2[i,9]  / 
               (FEED2[i,9]  + FEED2[i,12]*PASSRED[PASSAGE[i,j]]) +
               FEED3QNTY[i,j] * FEED3[i,4] * FEED3[i,9]  / 
               (FEED3[i,9]  + FEED3[i,12]*PASSRED[PASSAGE[i,j]]) +
               FEED4QNTY[i,j] * FEED4[4] * FEED4[9]  / 
               (FEED4[9]  + FEED4[12]*PASSRED[PASSAGE[i,j]]) 
  
  # Total intake insoluble, non-structural carbohydrates (g day-1)        
  INSCTOTAL[i,j] <-  FEED1QNTY[i,j] * FEED1[i,4] + FEED2QNTY[i,j] * FEED2[i,4] + 
    FEED3QNTY[i,j] * FEED3[i,4] + FEED4QNTY[i,j] * FEED4[4]
  # Fraction insoluble, non-structural carbohydrates digested in rumen (compare to Owens, 
  # 1986) 
  INSCDIG[i,j]   <-  INSC[i,j]/INSCTOTAL[i,j] 
  
  # Total tract digestibility of INSC is assumed to be 97% for all feeds (Moharrery et al, 
  # 2014)        
  INSCINT[i,j]   <-  max(0,(INSCTOTAL[i,j]*TTDIGINSC)-INSC[i,j]) 
  # Fraction INSC digested in the intestines (-)    
  INSCINTDIG[i,j] <- INSCINT[i,j]/INSCTOTAL[i,j] 
          
  
  # Digestion potentially degradable NDF (g day-1)
  NDF[i,j] <- FEED1QNTY[i,j] * FEED1[i,5] * FEED1[i,10] / 
    (FEED1[i,10] + FEED1[i,12]*PASSRED[PASSAGE[i,j]]) +  
    FEED2QNTY[i,j] * FEED2[i,5] * FEED2[i,10] / 
    (FEED2[i,10] + FEED2[i,12]*PASSRED[PASSAGE[i,j]]) +
    FEED3QNTY[i,j] * FEED3[i,5] * FEED3[i,10] / 
    (FEED3[i,10] + FEED3[i,12]*PASSRED[PASSAGE[i,j]]) +
    FEED4QNTY[i,j] * FEED4[5] * FEED4[10] / 
    (FEED4[10] + FEED4[12]*PASSRED[PASSAGE[i,j]])  
  
  # Total intake NDF (g day-1)        
  NDFTOTAL[i,j] <-   FEED1QNTY[i,j] * FEED1[i,5] + FEED2QNTY[i,j] * FEED2[i,5] + 
    FEED3QNTY[i,j] * FEED3[i,5] + FEED4QNTY[i,j] * FEED4[5]    
  # Fraction degradable NDF digested in the rumen (-)
  NDFDIG[i,j]   <-   NDF[i,j]/NDFTOTAL[i,j] 
          
  # Digestion of degradable NDF in the intestines (g day-1). # Volative fatty acids released
  # are assumed not to be taken up by the animal in the intestines. See also # Cabral et al.,
  # (2011, http://www.scielo.br/pdf/rbz/v40n9/a20v40n9.pdf) 
  NDFINT[i,j]   <-   FEED1QNTY[i,j] * FEED1[i,5] * (1- FEED1[i,10] / (FEED1[i,10] + 
                     FEED1[i,12]*PASSRED[PASSAGE[i,j]])) * (FEED1[i,10]*NDFDIGEST) / 
                     (FEED1[i,10]*NDFDIGEST + NDFPASS) +  
                     FEED2QNTY[i,j] * FEED2[i,5] * (1- FEED2[i,10] / (FEED2[i,10] + 
                     FEED2[i,12]*PASSRED[PASSAGE[i,j]])) * (FEED2[i,10]*NDFDIGEST) / 
                     (FEED2[i,10]*NDFDIGEST + NDFPASS) +   
                     FEED3QNTY[i,j] * FEED3[i,5] * (1- FEED3[i,10] / (FEED3[i,10] + 
                     FEED3[i,12]*PASSRED[PASSAGE[i,j]])) * (FEED3[i,10]*NDFDIGEST) / 
                     (FEED3[i,10]*NDFDIGEST + NDFPASS) +  
                     FEED4QNTY[i,j] * FEED4[5] * (1- FEED4[10] / (FEED4[10] + 
                     FEED4[12]*PASSRED[PASSAGE[i,j]])) * (FEED4[10]*NDFDIGEST) / 
                     (FEED4[10]*NDFDIGEST + NDFPASS) 
  
  # Fraction degradable NDF digested in intestines (-)        
  NDFINTDIG[i,j] <-  NDFINT[i,j]/NDFTOTAL[i,j]
  
  # Fraction degradable NDF digested in intestines, on DM basis (-)
  NDFINTDIGTOT[i,j] <- NDFINT[i,j]/(FEEDQNTY[i,j]*1000)  
          
  # Degradable protein digestion in the rumen (g day-1)
  PICP[i,j] <- FEED1QNTY[i,j] * FEED1[i,7] * FEED1[i,11] / 
               (FEED1[i,11] + FEED1[i,12]*PASSRED[PASSAGE[i,j]]) +  
               FEED2QNTY[i,j] * FEED2[i,7] * FEED2[i,11] / 
               (FEED2[i,11] + FEED2[i,12]*PASSRED[PASSAGE[i,j]]) +
               FEED3QNTY[i,j] * FEED3[i,7] * FEED3[i,11] / 
               (FEED3[i,11] + FEED3[i,12]*PASSRED[PASSAGE[i,j]]) +
               FEED4QNTY[i,j] * FEED4[7] * FEED4[11] / 
               (FEED4[11] + FEED4[12]*PASSRED[PASSAGE[i,j]])  
  
  # Total crude protein intake (g day-1)         
  PROTTOTAL[i,j]  <- (FEED1QNTY[i,j] * FEED1[i,16] + FEED2QNTY[i,j] * FEED2[i,16] +
                        FEED3QNTY[i,j] * FEED3[i,16] + FEED4QNTY[i,j] * FEED4[16]) 
  
  # Protein passed on to the intestines (g day-1)
  PROTINT[i,j]    <- PROTTOTAL[i,j] - (FEED1QNTY[i,j] * FEED1[i,6] + 
                                       FEED2QNTY[i,j] * FEED2[i,6] + 
                                       FEED3QNTY[i,j] * FEED3[i,6] + 
                                       FEED4QNTY[i,j] * FEED4[6]) - PICP[i,j]
  # Lucas equation, empirical formula to calculate protein digestion for the whole digestive 
  # tract (g protein day-1)   
  PROTUPT[i,j]    <- LUCAS1 * PROTTOTAL[i,j] - LUCAS2 * FEEDQNTY[i,j]
  # Protein excreted (g protein day-1)
  PROTEXCR[i,j]   <- PROTTOTAL[i,j] - PROTUPT[i,j]   
  
  # Fraction protein digested in rumen (-)        
  PROTDIGRU[i,j]  <- (PROTTOTAL[i,j]-PROTINT[i,j])/ PROTTOTAL[i,j]
  # Fraction protein digested in the whole digestive tract (-)
  PROTDIGWT[i,j]  <- PROTUPT[i,j] / PROTTOTAL[i,j] 
          
  # Digestion and excretion
  
  # Feed dry matter digested in the whole digestive tract(g DM day-1)
  DIGFRAC[i,j] <-    FEED1QNTY[i,j] * (FEED1[i,3]+FEED1[i,6]) +   
                     FEED2QNTY[i,j] * (FEED2[i,3]+FEED2[i,6]) +
                     FEED3QNTY[i,j] * (FEED3[i,3]+FEED3[i,6]) +
                     FEED4QNTY[i,j] * (FEED4[3]+FEED4[6]) +
                     INSC[i,j] + INSCINT[i,j] + NDF[i,j] + NDFINT[i,j] + PROTUPT[i,j]     
  
  # Carbohydrates excreted (g day-1) are calculated as carbohydrates (CHs) present in feed
  # minus CHs digested        
  CHEXCR[i,j]   <- FEEDQNTY[i,j]*1000-DIGFRAC[i,j]-PROTEXCR[i,j]                 
  # Feed dry matter excreted (g DM day-1) 
  EXCRFRAC[i,j] <- FEEDQNTY[i,j]*1000-DIGFRAC[i,j]                      
  
  # Gross energy (GE) content excreted biomass (MJ GE kg-1 DM)        
  GEEXCR[i,j]   <- (PROTEXCR[i,j] * GEPROT + CHEXCR[i,j] * GECARB) / 
                   (PROTEXCR[i,j] + CHEXCR[i,j])  
          
  # Gross energy (GE) content feed (MJ GE kg-1 DM)
  GEUPTAKE[i,j] <- (PROTUPT[i,j] * GEPROT + (DIGFRAC[i,j]-PROTUPT[i,j]) * GECARB) / 
                   (DIGFRAC[i,j])   
  # ME uptake (MJ ME day-1)           
  if(EXCRFRAC[i,j] == 0) MEUPTAKE[i,j] <-0 else 
    MEUPTAKE[i,j] <- DIGFRAC[i,j]/1000 * GEUPTAKE[i,j] * DETOME 
  
  # Digestibility feed, calculated on an energy basis (g g-1 DM)        
  if(EXCRFRAC[i,j] == 0) ENDIGEST[i,j] <-0 else 
    ENDIGEST[i,j] = DIGFRAC[i,j]/(FEEDQNTY[i,j]*1000) * (GEUPTAKE[i,j] /GEFEED) 
          
  # Average heat increment of feeding (MJ MJ-1 metabolisable energy)
  if((FEED1QNTY[i,j] + FEED2QNTY[i,j] + FEED3QNTY[i,j] + FEED4QNTY[i,j]) == 0) 
    Digestfracfeed[i,j] <- 0.3 else 
     Digestfracfeed[i,j] <- (FEED1QNTY[i,j] * FEED1[i,1] + FEED2QNTY[i,j] * FEED2[i,1] +
                             FEED3QNTY[i,j] * FEED3[i,1] + FEED4QNTY[i,j] * FEED4[1])/  
                        (FEED1QNTY[i,j] + FEED2QNTY[i,j] + FEED3QNTY[i,j] + FEED4QNTY[i,j])                    
          
  # If feed intake is not reduced due to heat stress, no energy requirement for respiration 
  # is required under maximum heat release.
  
  # Net energy (NE) requirement for increased respiration under heat stress(MJ NE day-1) 
  if(REDHP[i,j] == 0) NERESPC[i,j] <- 0 else NERESPC[i,j] <- NERESP[i,j]/1000   
  # Protein requirements under maximum heat release (g day-1) 
  PROTRESP[i,j] <- NERESPC[i,j] * PROTNE * NtoCP 
          
  ###########################################################################################
          
  # Milk for the calf from the cow 
  
  # Metabolisable energy supply via milk (MJ day-1)
  if(TIME[i,j]<=WEANINGTIME) MILKSTART[i,j] <- LIBRARY[15]*TIME[i,j]^MILKPARB*
    exp(-MILKPARC*TIME[i,j]) * (((5.5109*TIME[i,j])+2771)/1000) * MILKDIG else 
      MILKSTART[i,j] <- 0   
  
  # Protein content of the milk (-). See equation 4 in the paper describing LiGAPS-Dairy.         
  PROTFRACMILK[i,j] <- (PROTFRACMILK1-PROTFRACMILK2*TIME[i,j]^PROTFRACMILK3*
                          exp(-PROTFRACMILK4*TIME[i,j]))/100
  # Protein supply via milk (g day-1)
  if(TIME[i,j]<=WEANINGTIME) MILKSTARTPR[i,j] <- LIBRARY[15]*TIME[i,j]^MILKPARB*
    exp(-MILKPARC*TIME[i,j]) * PROTFRACMILK[i,j]*1000 * MILKDIG else MILKSTARTPR[i,j] <- 0 
  
  # Milk from cow for calf, accounts for heat increment of feeding (MJ ME day-1)        
  MEMILKCALFINIT[i,j] <- MILKSTART[i,j] * (1+(Digestfracfeed[i,j]/(1-Digestfracfeed[i,j])))  
          
  # Protein for purposes other than lactation and growth
  
  # Total fixed protein requirements, excl. lactation and growth (g protein day-1)
  PROTNONG[i,j]  <- PROTDERML[i,j] + PROTMAINT[i,j] + PROTPHACT[i,j] + 
                    PROTGESTG[i,j] + PROTRESP[i,j]
  
  # Heat increment of feeding, excl. lactation and growth (MJ day-1)   
  HIFM[i,j]      <- (NEMAINT[i,j]/1000+NEPHYSACT[i,j]/1000+NEREQGEST[i,j]+
                       NERESPC[i,j]/1000) * (Digestfracfeed[i,j]/(1-Digestfracfeed[i,j])) 
  # Metabolisable protein requirements, excl. lactation and growth (g protein day-1)
  PROTNONGM[i,j] <- PROTNONG[i,j] + HIFM[i,j] * PROTNE * NtoCP      
             
  # Energy not allocated to lactation and growth, converted into heat (MJ day-1)
  HEATIFEEDMAINT[i,j] = NEMAINT[i,j]/1000 + NEPHYSACT[i,j]/1000 + HEATGEST[i,j] + 
    NERESPC[i,j]/1000 + HIFM[i,j] + HEATIFEEDGROWTHC[i,j]/DISSEFF + REDMAINT[i,j] 
          
  # Sum of all heat released which is not related to lactation and growth, includes heat 
  # increment of feeding (W m-2)
  HEATIFEEDMAINTWM[i,j] = HEATIFEEDMAINT[i,j]/(3600*24* AREA[i,j])*1000000 
  
  # Heat allowed for lactation and growth (W m-2)        
  HEATIFEEDGROWTHWM[i,j] = Metheatopt[i,j]-HEATIFEEDMAINTWM[i,j]             
  # Heat allowed for lactation and growth (MJ day-1)
  HEATIFEEDGROWTH[i,j] = HEATIFEEDGROWTHWM[i,j]*(3600*24* AREA[i,j])/1000000 
          
  # If heat release under heat stress is lower than the metabolic processes excluding 
  # lactation and growth, feed intake has to be reduced. The excess heat under this
  # condition is calculated below (MJ day-1)
  if(HEATIFEEDGROWTH[i,j] < 0) REDMAINT[i,j] <- HEATIFEEDGROWTH[i,j] else 
    REDMAINT[i,j] <- 0
  
  # Net energy available for lactation and growth (MJ NE day-1)        
  ENFEEDGROWTHQ[i+1,j] <- ((MEUPTAKE[i,j]) - HEATIFEEDMAINT[i,j] + MEMILKCALFINIT[i,j]) / 
    (1+(Digestfracfeed[i,j]/(1-Digestfracfeed[i,j])))     
  ENFEEDGROWTHQ[i+1,j] <- ENFEEDGROWTHQ[i+1,j] - 0.134*NEREQGEST[i,j] 
   
  # Integrates the net energy for lactation and growth, based on the genetic potential for 
  # lactation (POTMILKPROD), growth(ENGRTOTAL), climate (REDHP), and feed-limitation 
  # (ENFEEDGROWTHQ) (MJ day-1)
  ENFEEDVAR[i+1,j]  = min(ENFEEDGROWTHQ[i+1,j], ENGRTOTAL[i+1,j] * COMPGROWTH[i,j]+
                            NEMILKCOW[i,j]) + REDHP[i,j]
  
  # The energy for lactation and growth is allocated between these two processes in the 
  # lines below.
          
  # Allocation of energy to milk production
          
  # Energy balance
  
  # Energy requirements for milk production relative to the total energy available for 
  # lactation and growth (-)
  RLG[i,j] <- NEMILKCOW[i,j] / (ENGRTOTAL[i+1,j] * COMPGROWTH[i,j]+NEMILKCOW[i,j])
  
  # Energy allocation to milk during early lactation with fat catabolism (MJ day-1). This 
  # equation corresponds to Equation 8 in the paper describing LiGAPS-Dairy.
  if(MILKDAY[i,j] >0 && RatioLG[i,j] <0)  ENFEEDLACT[i,j] <- min(NEMILKCOW[i,j] * 
     min(1,sqrt(MISCFATTISACT[i,j]/MISCFATTIS[i,j])+pars[8]), 
     ENFEEDVAR[i+1,j]*(1+pars[7]*abs(RatioLG[i,j])))  
          
  # Energy allocation to milk in phases of regain of body reserves (MJ day-1). This equation
  # corresponds to Equation 9 in the paper describing LiGAPS-Dairy.
  if(MILKDAY[i,j] >0 && RatioLG[i,j] >=0) ENFEEDLACT[i,j] <- min(NEMILKCOW[i,j] * 
     min(1,sqrt(MISCFATTISACT[i,j]/MISCFATTIS[i,j])+pars[8]), 
     ENFEEDVAR[i+1,j]*(1-abs(RatioLG[i,j])*2.00*
                         (1-(min(1,MISCFATTISACT[i,j]/MISCFATTIS[i,j]))))) 
  
  # If an animal does not produce milk, the energy allocated to milk production equals zero
  # (MJ day-1)
  if(MILKDAY[i,j] == 0) ENFEEDLACT[i,j] <-0
  
  # Energy for lactation cannot be lower than zero (MJ day-1)
  ENFEEDLACT[i,j] <- max(0, ENFEEDLACT[i,j])
  # Amount of milk produced from the energy supply to milk (L day-1)
  MILKPRODACT[i,j] = ENFEEDLACT[i,j]/GEMILK[i,j]*NEEFFMILK 
  # Amount of milk produced from the energy supply to milk (kg FPCM day-1)        
  MILKPRODACTFPCM[i,j] = MILKPRODACT[i,j] * GEMILK[i,j] / GE_FPCM
  # Amount of milk produced from the energy supply to milk, based on the genetic potential
  # of the animal, which is represented by the Wood's curve (kg FPCM day-1) 
  MILKPRODPOTFPCM[i,j] = POTMILKPROD[i,j] * GEMILK[i,j] / GE_FPCM
  
  # Heat generation for milk synthesis (MJ day-1)        
  HEATMILK[i,j] = ENFEEDLACT[i,j] * (1-NEEFFMILK) 
  
  # Remaining energy (if any) is for growth (MJ day-1).         
  ENFEEDGROWTH[i+1,j] <- ENFEEDVAR[i+1,j]-ENFEEDLACT[i,j]
  # To avoid errors elsewhere in the source code, the energy for growth is not completely 
  # set to zero (MJ day-1)
  if(ENFEEDGROWTH[i+1,j]==0) ENFEEDGROWTH[i+1,j] <- 1.0*10^-7
          
  # Protein balance
  
  # Protein content of milk (-). See also Equation 4 in the paper describing LiGAPS-Dairy 
  PROTFRACMILK[i,j] <- (PROTFRACMILK1-PROTFRACMILK2*MILKDAY[i,j]^PROTFRACMILK3*
                          exp(-PROTFRACMILK4*MILKDAY[i,j]))/100
  # Protein in milk (g protein day-1)
  PROTMILK[i,j]     <- MILKPRODACT[i,j] * PROTFRACMILK[i,j] * 1000     
  # Total protein requirements for milk production (g protein day-1). The efficiency for
  # milk production equals 68% (CSIRO, 2007).
  PROTMILKG[i,j]    <- PROTMILK[i,j] / PROTEFFMILK
  
  # Metabolisable protein requirements, excl. growth (g protein day-1)
  PROTNONGM[i,j]    <- PROTNONGM[i,j] + PROTMILKG[i,j] + PROTMILKG[i,j] * 
    (Digestfracfeed[i,j]/(1-Digestfracfeed[i,j])) * PROTNE * NtoCP   
          
  # Fraction energy left after recovery of depleted fat tissues(-)
  FRENGRNONCACT[i+1,j]   = max(0,(ENFEEDGROWTH[i+1,j]-FATCOMP[i,j])/ENFEEDGROWTH[i+1,j])         
  FRENGRBONEACT[i+1,j]   = max(0,(ENFEEDGROWTH[i+1,j]-FATCOMP[i,j])/ENFEEDGROWTH[i+1,j]) 
  FRENGRMUSCLEACT[i+1,j] = max(0,(ENFEEDGROWTH[i+1,j]-FATCOMP[i,j])/ENFEEDGROWTH[i+1,j])
  FRENGRIMFACT[i+1,j]    = max(0,(ENFEEDGROWTH[i+1,j]-FATCOMP[i,j])/ENFEEDGROWTH[i+1,j])
          
  # Non-carcass tissue cannot grow beyond the genetic potential (-) 
  if(NONCARCTISACT[i,j]/NONCARCTIS[i,j] < 1) 
    FRENGRNONCACT[i+1,j] <- FRENGRNONCACT[i+1,j] else FRENGRNONCACT[i+1,j] <- 0
  
  # NE for the non-carcass tissue (MJ day-1). Energy supply is least limited for non-carcass 
  # tissue if feed quality or feed availability is limiting production. 
  ENGRNONCACT[i+1,j]   = FRENGRNONCACT[i+1,j]* (ENGRNONC[i+1,j]/ENGRTOTALORIG[i+1,j]) * 
    (ENFEEDGROWTH[i+1,j]) * COMPGROWTH1[i,j]  
  # Energy supply cannot be negative (MJ day-1)
  ENGRNONCACT[i+1,j]   = max(ENGRNONCACT[i+1,j],0)               
  
  # Bone tissue cannot grow beyond the genetic potential (-)                       
  if(BONETISACT[i,j]/BONETIS[i,j] < 1) 
    FRENGRBONEACT[i+1,j] <- FRENGRBONEACT[i+1,j] else FRENGRBONEACT[i+1,j] <- 0
  
  # NE for the bone tissue (MJ day-1)
  ENGRBONEACT[i+1,j]   = FRENGRBONEACT[i+1,j]* (ENGRBONE[i+1,j]/ENGRTOTALORIG[i+1,j]) * 
    (ENFEEDGROWTH[i+1,j]) * COMPGROWTH2[i,j] 
  # Energy supply cannot be negative (MJ day-1)
  ENGRBONEACT[i+1,j]   = max(ENGRBONEACT[i+1,j],0)     
                      
  # Muscle tissue cannot grow beyond the genetic potential (-)
  if(MUSCLETISACT[i,j]/MUSCLETIS[i,j] < 1) 
    FRENGRMUSCLEACT[i+1,j] <- FRENGRMUSCLEACT[i+1,j] else FRENGRMUSCLEACT[i+1,j] <- 0 
  # NE for the muscle tissue (MJ day-1)
  ENGRMUSCLEACT[i+1,j]   = FRENGRMUSCLEACT[i+1,j]* (ENGRMUSCLE[i+1,j]/ENGRTOTALORIG[i+1,j]) *
    (ENFEEDGROWTH[i+1,j]) * COMPGROWTH3[i,j] 
  # Energy supply cannot be negative (MJ day-1)
  ENGRMUSCLEACT[i+1,j]   = max(ENGRMUSCLEACT[i+1,j],0) 
                                                                         
  # Intramuscular fat tissue cannot growth beyond the genetic potential (-)
  if(INTRAMFTISACT[i,j]/INTRAMFTIS[i,j] < 1) 
    FRENGRIMFACT[i+1,j] <- FRENGRIMFACT[i+1,j] else FRENGRIMFACT[i+1,j] <- 0 
  # NE for the intramuscular fat tissue (MJ day-1) 
  ENGRIMFACT[i+1,j]    = FRENGRIMFACT[i+1,j]* (ENGRIMF[i+1,j]/ENGRTOTALORIG[i+1,j]) * 
    (ENFEEDGROWTH[i+1,j]) * COMPGROWTH4[i,j] 
  # Energy supply cannot be negative (MJ day-1)
  ENGRIMFACT[i+1,j]    = max(ENGRIMFACT[i+1,j],0)      
  
  # NE for the subcutaneous and intermuscular fat tissue (MJ day-1); balancing variable     
  ENGRFATACT[i+1,j]    = ENFEEDGROWTH[i+1,j]-ENGRNONCACT[i+1,j]-ENGRBONEACT[i+1,j]-
    ENGRMUSCLEACT[i+1,j]-ENGRIMFACT[i+1,j] 
  # Energy supply cannot be negative (MJ day-1)
  ENGRFATACT[i+1,j]    = max(ENGRFATACT[i+1,j],0) 
          
  # Check: ENGRTOTALCOMP (MJ NE day-1) should be equal to ENFEEDGROWTH. # Positive values are
  # wrong              
  ENGRTOTALCOMP[i+1,j] = ENGRNONCACT[i+1,j] + ENGRBONEACT[i+1,j] + ENGRMUSCLEACT[i+1,j] + 
    ENGRIMFACT[i+1,j] + ENGRFATACT[i+1,j] 
  CHECKCOMP[i+1,j] = ENFEEDGROWTH[i+1,j] - ENGRTOTALCOMP[i+1,j]  
              
  # Heat production for growth (13.9 MJ for synthesis of 1 kg lipid, and 20.2 MJ for 
  # synthesis of 1 kg protein)
  
  # Heat production bone tissue (MJ day-1)
  HEATBONEACT[i,j]     = DERBONE[i,j]    * ENGRBONEACT[i+1,j]/ENGRBONE[i+1,j] * 
    (LIPFRACBONEACT[i,j] * (GELIPID/LIPIDEFF-GELIPID) + PROTFRACBONE * 
       (GEPROT/PROTEFF-GEPROT))  
  # Heat production muscle tissue (MJ day-1) 
  HEATMUSCLEACT[i,j]   = DERMUSCLE[i,j]  * ENGRMUSCLEACT[i+1,j]/ENGRMUSCLE[i+1,j] * 
    (LIPFRACMUSCLE * (GELIPID/LIPIDEFF-GELIPID) + PROTFRACMUSCLE * (GEPROT/PROTEFF-GEPROT)) 
  # Heat production intramuscular tissue (MJ day-1) 
  HEATIMFACT[i,j]      = DERINTRAMF[i,j] * ENGRIMFACT[i+1,j]/ENGRIMF[i+1,j] * 
    (LIPFRACFAT * (GELIPID/LIPIDEFF-GELIPID) + PROTFRACFAT * (GEPROT/PROTEFF-GEPROT)) 
  # Heat production subcutaneous and intermuscular tissue (MJ day-1) 
  HEATMISCFATACT[i,j]  = DERMISCFAT[i,j] * ENGRFATACT[i+1,j]/ENGRFAT[i+1,j] * 
    (LIPFRACFAT * (GELIPID/LIPIDEFF-GELIPID) + PROTFRACFAT * (GEPROT/PROTEFF-GEPROT)) 
  # Heat production non-carcass tissue (MJ day-1)  
  HEATNONCACT[i,j]     = DERNONC[i,j]    * ENGRNONCACT[i+1,j]/ENGRNONC[i+1,j] * 
    (LIPFRACNONCACT[i,j] * (GELIPID/LIPIDEFF-GELIPID) + PROTFRACNONCACT[i,j] * 
       (GEPROT/PROTEFF-GEPROT)) 
  
  # Total heat production from the net energy for growth (MJ day-1)          
  HEATTOTALACT[i,j]    = HEATBONEACT[i,j] + HEATMUSCLEACT[i,j] + HEATIMFACT[i,j] + 
    HEATMISCFATACT[i,j] + HEATNONCACT[i,j] 
                  
  # Energy requirements for growth (44.0 MJ gross energy per kg protein, 53.7 MJ gross 
  # energy per kg lipid)    
  
  # Energy requirement for growth of bone tissue (MJ NE day-1)
  ENBONEACT[i,j]     = DERBONE[i,j]    * ENGRBONEACT[i+1,j]/ENGRBONE[i+1,j] * 
    (LIPFRACBONEACT[i,j] * GELIPID/LIPIDEFF + PROTFRACBONE * GEPROT/PROTEFF)   
  # Energy requirement for growth of muscle tissue (MJ NE day-1)  
  ENMUSCLEACT[i,j]   = DERMUSCLE[i,j]  * ENGRMUSCLEACT[i+1,j]/ENGRMUSCLE[i+1,j] * 
    (LIPFRACMUSCLE * GELIPID/LIPIDEFF + PROTFRACMUSCLE * GEPROT/PROTEFF) 
  # Energy requirement for growth of intramuscular tissue (MJ NE day-1) 
  ENIMFACT[i,j]      = DERINTRAMF[i,j] * ENGRIMFACT[i+1,j]/ENGRIMF[i+1,j] * 
    (LIPFRACFAT * GELIPID/LIPIDEFF + PROTFRACFAT * GEPROT/PROTEFF) 
  # Energy requirement for growth of subcutaneous and intermuscular tissue (MJ NE day-1) 
  ENMISCFATACT[i,j]  = DERMISCFAT[i,j] * ENGRFATACT[i+1,j]/ENGRFAT[i+1,j] * 
    (LIPFRACFAT * GELIPID/LIPIDEFF + PROTFRACFAT * GEPROT/PROTEFF) 
  # Energy requirement for growth of non-carcass tissue (MJ NEday-1) 
  ENNONCACT[i,j]     = DERNONC[i,j]    * ENGRNONCACT[i+1,j]/ENGRNONC[i+1,j] * 
    (LIPFRACNONCACT[i,j] * GELIPID/LIPIDEFF + PROTFRACNONCACT[i,j] * GEPROT/PROTEFF) 
  
  # Total net energy for growth (MJ NE day-1)            
  ENTOTALACT[i,j]    = ENBONEACT[i,j] + ENMUSCLEACT[i,j] + ENIMFACT[i,j] + 
    ENMISCFATACT[i,j] + ENNONCACT[i,j] 
        
  # Protein requirements for growth
  
  # Protein for bone growth (g protein day-1). Protein use efficiency is assumed to be 54% 
  # (gross energy protein = 23.8 kJ g-1, requirement = 44.0 kJ g-1, efficiency = 0.54)   
  PROTBONEACT[i,j]    = DERBONE[i,j]    * ENGRBONEACT[i+1,j]/ENGRBONE[i+1,j] * 
    PROTFRACBONE / PROTEFF * 1000          
  # Protein for muscle growth (g protein day-1)
  PROTMUSCLEACT[i,j]  = DERMUSCLE[i,j]  * ENGRMUSCLEACT[i+1,j]/ENGRMUSCLE[i+1,j] * 
    PROTFRACMUSCLE / PROTEFF * 1000    
  # Protein for intra-muscular fat growth (g protein day-1) 
  PROTIMFACT[i,j]     = DERINTRAMF[i,j] * ENGRIMFACT[i+1,j]/ENGRIMF[i+1,j] * 
    PROTFRACFAT / PROTEFF * 1000             
  # Protein for intermuscular and subcutaneous fat growth (g protein day-1) 
  PROTMISCFATACT[i,j] = DERMISCFAT[i,j] * ENGRFATACT[i+1,j]/ENGRFAT[i+1,j] * 
    PROTFRACFAT / PROTEFF * 1000             
  # Protein for non-carcass tissue growth (g protein day-1) 
  PROTNONCACT1[i,j]    = DERNONC[i,j]    * ENGRNONCACT[i+1,j]/ENGRNONC[i+1,j] * 
    PROTFRACNONCACT[i,j] / PROTEFF * 1000   
            
  # Total protein requirement for growth (g protein day-1)      
  PROTTOTALACT[i,j]   = PROTBONEACT[i,j] + PROTMUSCLEACT[i,j] + PROTIMFACT[i,j] + 
    PROTMISCFATACT[i,j] + PROTNONCACT1[i,j]              
            
  # Total protein requirement (g protein day-1). This excludes recycling of N
  PROTGROSS[i,j] <- PROTNONGM[i,j] + PROTTOTALACT[i,j] + ENTOTALACT[i,j]*
    (Digestfracfeed[i,j]/(1-Digestfracfeed[i,j])) * PROTNE * NtoCP + HEATTOTALACT[i,j] * 
    PROTNE * NtoCP    
  # Percentage of urea N recycled, from N intake (-) (Russel et al, 1992)  
  NRECYCLPT[i,j] <- NRECYCL1 - NRECYCL2*(CPAVG[i,j]/10) + NRECYCL3*(CPAVG[i,j]/10)^2 
  NRECYCLPT[i,j] <- min(60,max(50,NRECYCLPT[i,j]))
  # For simplicity, no recycling is assumed to occur when a calf is only fed with milk in 
  # the first two weeks of its life (g day-1)
  if(TIME[i,j]<=14) NRECYCLPT[i,j] <- 0        
          
  # Net protein requirement (g protein day-1), including recycling     
  PROTNETT[i,j]  <- PROTGROSS[i,j] - (NRECYCLPT[i,j]/100) * (CPAVG[i,j] * FEEDQNTY[i,j])  
  
  # Heat increment of feeding is assumed to be negligible for milk consumption, so more 
  # energy and protein can be allocated to metabolic processes other than heat increment of 
  # feeding        
  MILKSTARTPRHF[i,j] <- MILKSTARTPR[i,j] * (1+(Digestfracfeed[i,j]/(1-Digestfracfeed[i,j]))) 
          
  # Additional energy requirement under cold conditions
  
  # Heat generation under cold conditions (MJ day-1)        
  HEATIFEEDGROWTHC[i,j] <- HEATIFEEDGROWTHC[i,j] + max(0,(Metheatcold[i,j]-
    HEATIFEEDMAINTWM[i,j])*(3600*24* AREA[i,j])/1000000 - HEATTOTALACT[i,j] - 
      ENTOTALACT[i,j]*(Digestfracfeed[i,j]/(1-Digestfracfeed[i,j])) - HEATMILK[i,j] - 
      ENFEEDLACT[i,j]*(Digestfracfeed[i,j]/(1-Digestfracfeed[i,j])))
  
  # HEATIFEEDGROWTHC[i,j] <- 0
  
  ###########################################################################################
  #                                   ME to feed conversion                                 #
  ###########################################################################################
  
  # Metabolisable energy requirements (MJ per day)
  MEREQTOTAL[i,j] = max(0,HEATIFEEDMAINT[i,j]-MEMILKCALFINIT[i,j]+(ENFEEDGROWTH[i+1,j]+
                    (0.134)*NEREQGEST[i,j]+ENFEEDLACT[i,j])*(1+(Digestfracfeed[i,j]/
                    (1-Digestfracfeed[i,j])))+ REDMAINT[i,j]/(Digestfracfeed[i,j]-
                    (1-DISSEFF))*(1-(Digestfracfeed[i,j]-(1-DISSEFF))))
  # Note: metabolisable energy requirements refer only to energy from feed, energy in milk
  # for the calf(MEMILKCALFINIT) is subtracted. The 0.134 in the equation is calculated as: 
  # (1-NEIEFFGEST)*(45/75)
         
  # Reduction in metablisable energy intake due to heat stess (MJ per day)
  MERED[i,j] <- REDMAINT[i,j]/(Digestfracfeed[i,j]-(1-DISSEFF))*(1-(Digestfracfeed[i,j]-
                (1-DISSEFF)))
  
  # Feed quantity to meet the requirements for metabolisable energy (kg DM day-1)       
  if(MEUPTAKE[i,j] == 0) FEEDINTAKE[i,j] <- 0 else 
    FEEDINTAKE[i,j] <- (MEREQTOTAL[i,j] / (ENDIGEST[i,j]*GEFEED))/DETOME       
  # Gastro-instestinal tract fill fraction (-) / Fraction maximum digestion capacity used (-)   
  if(MEUPTAKE[i,j] == 0) FILLGIT[i,j] <- 0 else 
    FILLGIT[i,j] <- FEEDQNTY[i,j] / PHFEEDINTKG[i,j]    
        
  # Rumen/ digestion capacity classes as defined by Chilibroste et al.(1997)
  if(FILLGIT[i,j] > 0.85) PASSAGE1[i,j] <- 1 else         
    if(FILLGIT[i,j] <= 0.85 & FILLGIT[i,j] > 0.65) PASSAGE1[i,j] <- 2 else
      if(FILLGIT[i,j] <= 0.65 & FILLGIT[i,j] > 0.45) PASSAGE1[i,j] <- 3 else 
        PASSAGE1[i,j] <- 4    
        
  # Difference in digestion capacity class between iterations (-) 
  PASSDIFF[i,j] <- max(0,PASSAGE1[i,j]-PASSAGE[i,j]) 
  if(TIME[i,j] > 15) PASSAGE[i,j] <- PASSAGE[i,j] + PASSDIFF[i,j]      
        
  # Times the integration loop has iterated (#)
  REPS[i,j] <- REPS[i,j] + 1   
        
  # Optimization statement in the intergration loop (among the different sub-models)
  
  # Protein balance (g day-1)
  PROTBAL[i,j] <- PROTUPT[i,j]* (1+NRECYCLPT[i,j]/100) + MILKSTARTPRHF[i,j] - PROTGROSS[i,j] 
  
  #PROTBAL[i,j] <- max(PROTBAL[i,j],0) # Use this line to exclude protein deficiency
  
  # Fraction reduction in milk production due to protein deficiency (-)      
  if(MILKDAY[i,j]>0) PROTREDFACTMILK[i,j] <- max(0, -PROTBAL[i,j]/(PROTMILKG[i,j] + 
                     PROTMILKG[i,j] * (Digestfracfeed[i,j]/(1-Digestfracfeed[i,j])) * 
                       PROTNE * NtoCP)) else PROTREDFACTMILK[i,j] <- 0
  # Milk production based on energy and protein flows (kg per day)
  MILKPRODACT[i,j] <- MILKPRODACT[i,j] * (1-PROTREDFACTMILK[i,j])
  # Milk production expressed as fat- and protein-corrected milk, based on energy and protein
  # flows (kg per day)
  MILKPRODACTFPCM[i,j] <- MILKPRODACTFPCM[i,j] * (1-PROTREDFACTMILK[i,j])
        
  # If non-lactating animals experience protein defiency, their growth is reduced 
  # proportionally for all body tissues      
  if(MILKDAY[i,j]==0) PROTREDFACT[i,j] <- 1- min(1,max(0,((PROTBAL[i,j]*-1)/(PROTGROSS[i,j]-
                      PROTNONGM[i,j])))) else PROTREDFACT[i,j] <-1   
        
  # This equation indicates that heat production from growth (HEATTOTALACT plus HIF for 
  # ENTOTALACT), which cannot exceed the maximum heat release (HEATIFEEDGROWTH) by more 
  # than 0.5 MJ day-1 (MJ day-1)       
  DIFFEN[i,j]        = (HEATTOTALACT[i,j]+ENTOTALACT[i,j]*(Digestfracfeed[i,j]/
                       (1-Digestfracfeed[i,j])) + HEATMILK[i,j]+ENFEEDLACT[i,j]*
                         (Digestfracfeed[i,j]/(1-Digestfracfeed[i,j]))) - 
                         max(0,HEATIFEEDGROWTH[i,j])   
  
  # If heat production exceeds the maximum heat release (i.e. heat stress occurs), feed 
  # intake is reduced via REDHP (MJ day-1).                     
  if(DIFFEN[i,j] >  0.5) REDHP[i,j] <- (REDHP[i,j]-0.1*DIFFEN[i,j]) else 
    REDHP[i,j] <- REDHP[i,j]  
               
  # If the passage rate is not correct YET, the loop does not change passage rate and feed 
  # intake reduction (represented by REDHP) at the same time (MJ day-1) 
  if(TIME[i,j] > 15 & PASSDIFF[i,j] != 0) REDHP[i,j] <- 0 
        
  # The repeat-loop has to iterate at least two times (CORRECT/FALSE)
  if(REPS[i,j] < 2)       CHECKHEAT3[i,j] <- "FALSE" else CHECKHEAT3[i,j] <- "CORRECT" 
  
  # The passage rate has to be correct before the repeat-loop terminates
  if(TIME[i,j] > 15 && PASSDIFF[i,j] != 0) CHECKHEAT3[i,j] <- "FALSE" 
  
  # Heat production cannot exceed maximum heat release before the repeat-loop terminates            
  if(DIFFEN[i,j] > 0.5)    CHECKHEAT3[i,j] <- "FALSE" 
  
  # If all conditions described above are met, the repeat-loop for integration of sub-models
  # is terminated       
  if(CHECKHEAT3[i,j] == "CORRECT") {break} 
            
  } # End of the integration loop
          
      
  # Fraction physically effective neutral detergent fibre in the diet (-)  
  PENDF[i,j]     <- FRACFEED1[i,j]*FEED1[i,14]*FEED1[i,15] + 
                    FRACFEED2[i,j]*FEED2[i,14]*FEED2[i,15] +      
                    FRACFEED3[i,j]*FEED3[i,14]*FEED3[i,15] + 
                    FRACFEED4[i,j]*FEED4[14]*FEED4[15]
  
  # Tissue weights are corrected for protein deficiency (PROTREDFACT)
  
  # Lipid in bone tissue (kg)
  LIPIDBONEACT[i+1,j]     = LIPIDBONEACT[i,j] + DERBONE[i,j]* ENGRBONEACT[i+1,j]/
                            ENGRBONE[i+1,j] * LIPFRACBONEACT[i,j] * PROTREDFACT[i,j] 
  # Lipid non carcass tissue (kg)
  LIPIDNONCACT[i+1,j]     = LIPIDNONCACT[i,j] + DERNONC[i,j]* ENGRNONCACT[i+1,j]/
                            ENGRNONC[i+1,j] * LIPFRACNONCACT[i,j] * PROTREDFACT[i,j] 
  # Protein non carcass tissue (kg)
  PROTNONCACT[i+1,j]      = PROTNONCACT[i,j] + DERNONC[i,j]* ENGRNONCACT[i+1,j]/
                            ENGRNONC[i+1,j] * PROTFRACNONCACT[i,j] * PROTREDFACT[i,j] 
        
  # Weight bone tissue (kg)
  BONETISACT[i+1,j]    = BONETISACT[i,j] + DERBONE[i,j] * ENGRBONEACT[i+1,j]/
                         ENGRBONE[i+1,j] * PROTREDFACT[i,j]  
  # Weight muscle tissue (kg)
  MUSCLETISACT[i+1,j]  = MUSCLETISACT[i,j] + DERMUSCLE[i,j] * ENGRMUSCLEACT[i+1,j]/
                         ENGRMUSCLE[i+1,j] * PROTREDFACT[i,j] 
  # Weight intramuscular tissue (kg)
  INTRAMFTISACT[i+1,j] = INTRAMFTISACT[i,j] + DERINTRAMF[i,j] * ENGRIMFACT[i+1,j]/
                         ENGRIMF[i+1,j] * PROTREDFACT[i,j] 
  # Weight subcutaneous and intermuscular fat tissue (kg)
  MISCFATTISACT[i+1,j] = MISCFATTISACT[i,j] + DERMISCFAT[i,j] * ENGRFATACT[i+1,j]/
                         ENGRFAT[i+1,j] * PROTREDFACT[i,j] 
  # Weight non carcass tissue (kg)
  NONCARCTISACT[i+1,j] = NONCARCTISACT[i,j] + DERNONC[i,j] * ENGRNONCACT[i+1,j]/
                         ENGRNONC[i+1,j] * PROTREDFACT[i,j] 
  
  # Gross energy content non carcass tissues (MJ kg-1 non-carcass)        
  ENCONTENTNONCACT[i,j]   = (LIPIDNONCACT[i,j] * GELIPID + PROTNONCACT[i,j] * GEPROT) / 
                            NONCARCTISACT[i,j]      
          
  # Muscle tissue dissimilated when protein supply is below maintenance. Note: A protein 
  # dissimilation (i.e. catabolism) efficiency of 90% assumed (kg per day).
  
  REDTISPROT[i,j] <- min(0, PROTGROSS[i,j]+PROTBAL[i,j]) / (PROTFRACMUSCLE* DISSEFF * 1000) 
          
  # Reduction in muscle tissue (kg day-1) as a result of protein deficiency  
  MUSCLETISACT[i+1,j] <- MUSCLETISACT[i+1,j] + REDTISPROT[i,j] 
      
  # Accouting for heat stress (REDTIS): reduction in feed intake heat release in under 
  # sub-maintenance intake. Weight loss due to sub-maintenance intake in (kg fat per day).
  # The gross energy content of fat tissue is 29.624 MJ kg-1. Fat catabolism is assumed
  # to occur with a 90% energy efficiency.   
          
  REDTIS[i,j] = REDMAINT[i,j]/(Digestfracfeed[i,j]-(1-DISSEFF))*(1-(Digestfracfeed[i,j]-
                (1-DISSEFF)))/GEFATTIS 
  REDTIS[is.nan(REDTIS)] <- 0                                                                                   
  
  # Fat tissue (cumulative gross energy) catabolised due to heat stress (MJ)        
  HEATBURNCUMUL[i,j] = sum(REDTIS[1:i,j])*GEFATTIS  
  
  # To avoid heat stress, subcutaneous and intermuscular fat is dissimilated (kg day-1)        
  MISCFATTISACT[i+1,j] <- MISCFATTISACT[i+1,j] + REDTIS[i,j] * 0.9  
  # To avoid heat stress, non-carcass tissue is dissimilated (kg day-1) 
  NONCARCTISACT[i+1,j] <- NONCARCTISACT[i+1,j] + REDTIS[i,j] * 0.1 * 
    (GEFATTIS/ENCONTENTNONCACT[i,j])/DISSEFF 
  
  # Feed intake cannot be negative due to heat stress (good/wrong)   
  if((NEMAINT[i,j]+NEPHYSACT[i,j]+NERESP[i,j])*1/DISSEFF < -1*REDTIS[i,j]) 
    CHECK[i,j] <- "wrong" else CHECK[i,j] <-"good"   
          
  # If energy for growth is negative, fat tissue is catabolized (kg per day)
  if(REDTIS[i,j] == 0 && ENFEEDGROWTH[i+1,j] < 0) 
    REDTIS2[i,j] <- (-ENFEEDGROWTH[i+1,j]/GEFATTIS)/DISSEFF else REDTIS2[i,j] <- 0
  
  # To correct for negative growth, subcutaneous and intermuscular fat tissueis catabolised 
  # (kg per day)     
  MISCFATTISACT[i+1,j] <- MISCFATTISACT[i+1,j] - REDTIS2[i,j] * 0.75  
  # To correct for negative growth, non-carcass tissue is catabolised (kg per day) 
  NONCARCTISACT[i+1,j] <- NONCARCTISACT[i+1,j] - REDTIS2[i,j] * 0.25 * 
    (GEFATTIS/ENCONTENTNONCACT[i,j])/DISSEFF 
  
  # Energy required to maintain body temperature under cold stress    
        
  # Cumulative energy required to maintain body temperature (MJ)
  FATBURNCUMUL[i+1,j] = FATBURNCUMUL[i,j] + HEATIFEEDGROWTHC[i,j] 
  
  # Total body weight in the next time step (kg TBW)  
  TBWACT[i+1,j]        = (BONETISACT[i+1,j] + MUSCLETISACT[i+1,j] + INTRAMFTISACT[i+1,j] + 
                            MISCFATTISACT[i+1,j] + NONCARCTISACT[i+1,j])/(1-RUMENFRAC) 
  
  # Metabolic body weight in the next time step (kg EBW^0.75) 
  EBWACTMET[i+1,j]     <- (TBWACT[i+1,j]*(1-RUMENFRAC))^0.75 
  
  # Protein accretion in tissues and in milk production (g day-1)
  PROTACCR[i,j] <- PROTGESTG[i,j]*0.5 + PROTMILK[i,j] + PROTTOTALACT[i,j] 
        
  # Fraction energy used for maintenance (-)
  MAINTFRAC[i,j] = (HEATIFEEDMAINT[i,j]-HEATIFEEDGROWTHC[i,j]+MEMILKCALFINIT[i,j])/
                   MEREQTOTAL[i,j] 
          
  ###################################################
  #         Culling and slaughter of cattle         #
  ###################################################
        
  # Animals can be slaughter if the fraction of fat tissues in the carcass equal a pre-
  # defined value (-). This reflects meat quality.
  FATFRACCARC[i,j] = (MISCFATTISACT[i,j]+INTRAMFTISACT[i,j])/
    (MISCFATTISACT[i,j]+INTRAMFTISACT[i,j]+MUSCLETISACT[i,j]+BONETISACT[i,j]) 
  
  # Reproductive animals can be slaughtered after they have given birth to a maximum number
  # of calves. The maximum number of calves per animal is calculated (#).
  CALVESPERANIMAL <- REPRODUCTIVE * MAXCALFNR 
        
  # Beef production (Beef is deboned carcass, kg)
  # A model simulation for an individual animal stops when the animal has reached  a specific
  # fat content of the carcass (option 1), or an animal animal reaches a maximum age (option 
  # 2), or an animal dies due to depletion of the fat tissues (option 3).  
  
  # Option 1: fat content reaches a specific level for animals. CALFLIVENR should be met for 
  # reproductive animals         
    if(TBWACT[i+1,j] > SWMALES && SEX[j] == 0) 
      BEEFPRODACT[i,j] <- (MUSCLETISACT[i,j] + INTRAMFTISACT[i,j] + MISCFATTISACT[i,j]) else
      if(TBWACT[i+1,j] > SWFEMALES && SEX[j] == 1 && REPRODUCTIVE[j] == 0) 
      BEEFPRODACT[i,j] <- (MUSCLETISACT[i,j] + INTRAMFTISACT[i,j] + MISCFATTISACT[i,j]) else
        if(SEX[j] == 1  && FATFRACCARC[i,j] > MAXFATCARC & 
           CALFWEANNR[i,j] == CALVESPERANIMAL[j] & TIME[i,j] > 800) 
        {BEEFPRODACT[i,j] <- (MUSCLETISACT[i,j] + INTRAMFTISACT[i,j] + MISCFATTISACT[i,j])} else
            {BEEFPRODACT[i,j] = 0}
            
  # Option 2: An animal is culled after a certain age in the (re)productive herd
  if(TIME[i,j]/365 > MAXLIFETIME & BEEFPRODACT[i,j] == 0) 
    BEEFPRODACT[i,j] <- (MUSCLETISACT[i,j] + INTRAMFTISACT[i,j] + MISCFATTISACT[i,j]) 
  if(REPRODUCTIVE[j] == 1 & CALFWEANNR[i,j] == CALVESPERANIMAL[j] & TIME[i,j]/365 > 
     MAXLIFETIME) 
    BEEFPRODACT[i,j] <- (MUSCLETISACT[i,j] + INTRAMFTISACT[i,j] + MISCFATTISACT[i,j]) 
          
  # Option 3: Death of an animal due to depletion of the fat reserves        
  if(MISCFATTISACT[i,j] < 0) BEEFPRODACT[i,j] <- -1 
  if(BEEFPRODACT[i,j]!=0)  SLAUGHTERDAYACT[i,j] <- TIME[i,j]  else 
    SLAUGHTERDAYACT[i,j] <- 9999
        
  # Live weight production (kg total body weight)
  
  # Option 1: fat content reaches a specific level for animals. CALFLIVENR should be met for 
  # reproductive animals        
  if(TBWACT[i+1,j] > SWMALES && SEX[j] == 0) LWPRODACT[i,j] <- TBWACT[i+1,j] else
    if(TBWACT[i+1,j] > SWFEMALES && SEX[j] == 1 && REPRODUCTIVE[j] == 0) 
      LWPRODACT[i,j] <- TBWACT[i+1,j] else
      if(SEX[j] == 1  && FATFRACCARC[i,j] > MAXFATCARC & 
         CALFWEANNR[i,j] == CALVESPERANIMAL[j] & TIME[i,j] > 800) 
        LWPRODACT[i,j] <- TBWACT[i+1,j] else LWPRODACT[i,j] = 0
             
  # Option 2: An animal is culled after a certain age in the (re)productive herd
  if(TIME[i,j]/365 > MAXLIFETIME & BEEFPRODACT[i,j] == 0) LWPRODACT[i,j] <- TBWACT[i+1,j] 
  if(REPRODUCTIVE[j] == 1 & CALFWEANNR[i,j] == CALVESPERANIMAL[j] &
     TIME[i,j]/365 > MAXLIFETIME) LWPRODACT[i,j] <- TBWACT[i+1,j] 
        
  # Option 3: Death of an animal due to depletion of the fat reserves      
  if(MISCFATTISACT[i,j] < 0) LWPRODACT[i,j] <- -1 
        
  # Carcass weigt (kg) 
  
  # Option 1: fat content reaches a specific level for animals. CALFLIVENR should be met for 
  # reproductive animals 
  if(TBWACT[i+1,j] > SWMALES && SEX[j] == 0) 
    CARCPRODACT[i,j] <- (MUSCLETISACT[i,j] + INTRAMFTISACT[i,j] + MISCFATTISACT[i,j]  + 
                           BONETISACT[i,j]) else
     if(TBWACT[i+1,j] > SWFEMALES && SEX[j] == 1 && REPRODUCTIVE[j] == 0) 
       CARCPRODACT[i,j] <- (MUSCLETISACT[i,j] + INTRAMFTISACT[i,j] + MISCFATTISACT[i,j] + 
                              BONETISACT[i,j]) else
       if(SEX[j] == 1  && FATFRACCARC[i,j] > MAXFATCARC & 
          CALFWEANNR[i,j] == CALVESPERANIMAL[j] & TIME[i,j] > 800) 
         CARCPRODACT[i,j] <- (MUSCLETISACT[i,j] + INTRAMFTISACT[i,j] + MISCFATTISACT[i,j] + 
                                BONETISACT[i,j]) else CARCPRODACT[i,j] = 0
              
  # Option 2: An animal is culled after a certain age in the (re)productive herd
  if(TIME[i,j]/365 > MAXLIFETIME & BEEFPRODACT[i,j] == 0) 
    CARCPRODACT[i,j] <- (MUSCLETISACT[i,j] + INTRAMFTISACT[i,j] + MISCFATTISACT[i,j] + 
                           BONETISACT[i,j]) 
  if(REPRODUCTIVE[j] == 1 & CALFWEANNR[i,j] == CALVESPERANIMAL[j] & 
     TIME[i,j]/365 > MAXLIFETIME) 
    CARCPRODACT[i,j] <- (MUSCLETISACT[i,j] + INTRAMFTISACT[i,j] + MISCFATTISACT[i,j] + 
                           BONETISACT[i,j]) 
        
  # Option 3: Death of an animal due to depletion of the fat reserves   
  if(MISCFATTISACT[i,j] < 0) BEEFPRODACT[i,j] <- -1 
  
  ###########################################################################################
  
  # Day of culling/slaughter in the lifetime of an animal (days). # This value is initially 
  # 9999, but is replaced by the correct day at culling/slaughter         
  ENDDAY[j] <- min(SLAUGHTERDAYACT[1:i,j]) 
                 
  # Beef production at slaughter (kg)
  if(ENDDAY[j] < 9999) BEEFPROD[j] <- BEEFPRODACT[ENDDAY[j],j] 
  # Beef production (kg beef per animal per year)
  if(ENDDAY[j] < 9999) BEEFPRODYEAR[j] <- BEEFPRODACT[ENDDAY[j],j]/(TIME[ENDDAY[j],j]/365) 
  
  # Live weight production at slaughter (kg)     
  if(ENDDAY[j] < 9999) LWPROD[j] <- LWPRODACT[ENDDAY[j],j] 
  # Live weight production (kg live weight per animal per year)
  if(ENDDAY[j] < 9999) LWPRODYEAR[j] <- LWPRODACT[ENDDAY[j],j]/(TIME[ENDDAY[j],j]/365) 
          
  # If the animal is slaughtered, the weight is set to 0 via the vector ALIVE (kg). The line
  # below produces a vector that indicates whether the animal is alive or culled/ 
  # slaughtered.      
  if(ENDDAY[j] == 9999) ALIVE[i+1,j] <- 1 else ALIVE[i+1,j] <- 0   
  
  # The lines below indicate the parity of a cow. A zero indicates that a cow is not in a 
  # certain parity yet, and a one indicates that the cow is or has been in a certain parity. 
  
  # Cow is in/has had first parity (1= true, 0=not true) 
  if(CALFLIVENR[i,j] >= 1) PARITY1[i,j] = 1 else PARITY1[i,j] = 0 
  # Cow is in/has had second parity (1= true, 0=not true)
  if(CALFLIVENR[i,j] >= 2) PARITY2[i,j] = 1 else PARITY2[i,j] = 0 
  # Cow is in/has had third parity (1= true, 0=not true)
  if(CALFLIVENR[i,j] >= 3) PARITY3[i,j] = 1 else PARITY3[i,j] = 0 
  # Cow is in/has had fourth parity (1= true, 0=not true)
  if(CALFLIVENR[i,j] >= 4) PARITY4[i,j] = 1 else PARITY4[i,j] = 0 
  # Cow is in/has had fifth parity (1= true, 0=not true)
  if(CALFLIVENR[i,j] >= 5) PARITY5[i,j] = 1 else PARITY5[i,j] = 0 
  # Cow is in/has had sixth parity (1= true, 0=not true)
  if(CALFLIVENR[i,j] >= 6) PARITY6[i,j] = 1 else PARITY6[i,j] = 0 
  # Cow is in/has had seventh parity (1= true, 0=not true)
  if(CALFLIVENR[i,j] >= 7) PARITY7[i,j] = 1 else PARITY7[i,j] = 0 
  # Cow is in/has had eighth parity (1= true, 0=not true)
  if(CALFLIVENR[i,j] >= 8) PARITY8[i,j] = 1 else PARITY8[i,j] = 0 
  # Cow is in/has had nineth parity (1= true, 0=not true)  
  if(CALFLIVENR[i,j] >= 9) PARITY9[i,j] = 1 else PARITY9[i,j] = 0 
  
  # The lines below indicate the birthday of a calf compared to the birth day of the cow 
  # (days)
  if(CALFLIVENR[i,j] == 1) BIRTHDAYCALF1 <- TIME[i]-sum(PARITY1[1:i,j]) else 
    BIRTHDAYCALF1 <- BIRTHDAYCALF1 # Birth day calf 1
  if(CALFLIVENR[i,j] == 2) BIRTHDAYCALF2 <- TIME[i]-sum(PARITY2[1:i,j]) else 
    BIRTHDAYCALF2 <- BIRTHDAYCALF2 # Birth day calf 2  
  if(CALFLIVENR[i,j] == 3) BIRTHDAYCALF3 <- TIME[i]-sum(PARITY3[1:i,j]) else 
    BIRTHDAYCALF3 <- BIRTHDAYCALF3 # Birth day calf 3
  if(CALFLIVENR[i,j] == 4) BIRTHDAYCALF4 <- TIME[i]-sum(PARITY4[1:i,j]) else 
    BIRTHDAYCALF4 <- BIRTHDAYCALF4 # Birth day calf 4
  if(CALFLIVENR[i,j] == 5) BIRTHDAYCALF5 <- TIME[i]-sum(PARITY5[1:i,j]) else 
    BIRTHDAYCALF5 <- BIRTHDAYCALF5 # Birth day calf 5
  if(CALFLIVENR[i,j] == 6) BIRTHDAYCALF6 <- TIME[i]-sum(PARITY6[1:i,j]) else 
    BIRTHDAYCALF6 <- BIRTHDAYCALF6 # Birth day calf 6
  if(CALFLIVENR[i,j] == 7) BIRTHDAYCALF7 <- TIME[i]-sum(PARITY7[1:i,j]) else 
    BIRTHDAYCALF7 <- BIRTHDAYCALF7 # Birth day calf 7
  if(CALFLIVENR[i,j] == 8) BIRTHDAYCALF8 <- TIME[i]-sum(PARITY8[1:i,j]) else 
    BIRTHDAYCALF8 <- BIRTHDAYCALF8 # Birth day calf 8 
  if(CALFLIVENR[i,j] == 9) BIRTHDAYCALF9 <- TIME[i]-sum(PARITY9[1:i,j]) else 
    BIRTHDAYCALF9 <- BIRTHDAYCALF9 # Birth day calf 9 
  
  # Average digestibility of the diet (-)
  AVGDIGFRAC[i,j] = FRACFEED1[i,j]*FEED1[i,1] + FRACFEED2[i,j]*FEED2[i,1] + 
                    FRACFEED3[i,j]*FEED3[i,1] + FRACFEED4[i,j]*FEED4[1]
          
  # Total feed consumption (kg day-1)
  
  # Cumulative amount feed 1 (kg day-1)      
  CUMULFEED1[i,j] = sum(FEED1QNTY[1:i,j])
  # Cumulative amount feed 2 (kg day-1)  
  CUMULFEED2[i,j] = sum(FEED2QNTY[1:i,j])
  # Cumulative amount feed 3 (kg day-1)  
  CUMULFEED3[i,j] = sum(FEED3QNTY[1:i,j])
  # Cumulative amount feed 4 (kg day-1)  
  CUMULFEED4[i,j] = sum(FEED4QNTY[1:i,j]) 
  
  # Cumulative amount feed consumed (kg day-1)       
  CUMULFEED[i,j]  = sum(CUMULFEED1[i,j]+ CUMULFEED2[i,j] + CUMULFEED3[i,j] + CUMULFEED4[i,j]) 
   
  # Feed conversion ratio (FCR) )        
  # Feed conversion ratio, based on live weight  (kg DM kg-1 live weight)
  FCR[i,j] = CUMULFEED[i,j]/(TBWACT[i,j]-TBWACT[1,j])          
  # Daily Feed conversion ratio, based on beef weight (kg DM kg-1 beef)
  FCRBEEF[i,j] = CUMULFEED[i,j]/((MUSCLETISACT[i+1,j] + INTRAMFTISACT[i+1,j] + 
                MISCFATTISACT[i+1,j])-(MUSCLETISACT[1,j] + INTRAMFTISACT[1,j] + 
                                         MISCFATTISACT[1,j]))     
  # Feed conversion ratio (kg DM kg-1 beef at slaughter)            
  if(ENDDAY[j] < 9999) FCRBEEFENDDAY[j] <- CUMULFEED[ENDDAY[j]]/BEEFPROD[ENDDAY[j]] 
  
  # Percentage feed intake relative to the total body weight (%)        
  PERCFI[i,j] <- FEEDQNTY[i,j]/TBWACT[i,j]*100 
               
  ###########################################################################################
  # Module for the assessment of greenhouse gas emissions related to livestock production   #
  #                                                                                         #
  # Processes included:                                                                     #
  # * Enteric methane emission                                                              #
  # * Methane from manure management                                                        #
  # * N20 from manure management                                                            #
  # This section specifies the equations to calculate the variables                         #
  ###########################################################################################
          
  # 1. Enteric methane
          
  #   The equation given by the IPCC is not very specific for feeds (Equation 10.21, 
  #   Appendix II). Equation from Ellis et al. (2006) Prediction of methane production 
  # from dairy and beef cattle, Journal of Dairy Science 90, 3456-3467
          
  #   Table 5 gives equations to predict CH4 emissions in MJ per day
  #   Equation [7b(beef)] CH4 (MJ/d)  = 3.05 (1 1.21) + 
  #                                     0.0371 (1 0.0170) W ME intake (MJ/d) + 
  #                                     0.801 (1 0.223) W NDF (kg/d)
  #   Equation [7d(dairy)] CH4 (MJ/d) = 1.64 (1 1.56) + 
  #                                     0.396 (1 0.0170) W ME intake (MJ/d) + 
  #                                     1.45 (1 0.521) W NDF (kg/d)
          
  # Energy in enteric methane emissions (MJ CH4 per day)
  ENTCH4MJ[i,j] <- ENTCH4PAR1 + ENTCH4PAR2*MEUPTAKE[i,j] + ENTCH4PAR3*NDFTOTAL[i,j]/1000 
  # Enteric methane emissions (kg per day) 
  ENTCH4KG[i,j] <- ENTCH4MJ[i,j] / CH4MJtoKG
  # Fraction feed energy converted into methane (-)
  ENTCH4frac[i,j] <- (ENTCH4MJ[i,j]/GE)/FEEDQNTY[i,j]
  # Global warming potential methane (CO2-equivalents)
  ENTCH4EQ[i,j] <- ENTCH4KG[i,j] * GWPCH4 
          
  # 2. Methane from manure management
  
  # Methane conversion factor        
  MCF[i,j] <- MCFgr
          
  # Volatilisation from manure, based on Equation 10.24         
  VS[i,j] <- (GE*(1-ENDIGEST[i,j]) + UE*GE) * ((1-ASH)/GE)
  # Methane emissions based on Equation 10.23 (g CH4 kg-1 DM)
  EF[i,j] <- VS[i,j] * (B0 * CH4c * MCF[i,j] * MS) 
  # Total weight methane emissions from manure management(kg day-1)
  CH4MM[i,j] <- EF[i,j] * FEEDQNTY[i,j]
  # Total weight methane emissions from manure management(kg CO2-equivalents day-1)
  CH4MMEQ[i,j] <- CH4MM[i,j] * GWPCH4
          
  # 3. Direct N20 emissions from managed soils (Not included in LiGAPS-Dairy)
          
  # 4. Direct N20 emissions from manure management
  
  # N excretion via dung and urine (g N per kg DM feed and milk)         
  Nexcr[i,j] <- (PROTTOTAL[i,j] + MILKSTARTPR[i,j] - PROTMILK[i,j] - 
                   PROTTOTALACT[i,j]*PROTEFF)/((FEEDQNTY[i,j]+MILKSTART[i,j])*NtoCP) 
  # Direct N2O emission, based on Equation 10.25 (g N20 per kg DM intake)
  N2OD[i,j] <- MS * Nexcr[i,j] * EF3 * Ncon 
  # Direct N2O emission from manure management (g N2O day-1)
  N2ODan[i,j] <- N2OD[i,j]*(FEEDQNTY[i,j]+MILKSTART[i,j])
  # Direct N2O emission from manure management (CO2-equivalents day-1)
  N2ODanEQ[i,j] <- N2ODan[i,j]/1000 * GWPN2O
          
  # 5. Indirect N20 emissions from manure management
  
  # Volatilisation of N, based on Equation 10.26 (g N kg-1 DM)         
  NvolMMS[i,j] <- Nexcr[i,j] * MS * FracgasMS 
  # N2O emissions, based on Equation 10.27 (g N2O kg DM-1 feed intake)  
  N2OG[i,j] <- NvolMMS[i,j]* EF4 * Ncon 
  # Indirect N20 emission from manure management (g N2O day-1)
  N2OGan[i,j] <- N2OG[i,j]*(FEEDQNTY[i,j]+MILKSTART[i,j])
  # Indirect N20 emission from manure management (CO2-equivalents day-1)
  N2OGanEQ[i,j] <- N2OGan[i,j]/1000 * GWPN2O
          
  # 6. Indirect N20 emissions from leaching
          
  # Leaching, based on Equation 10.28 (g N per kg DM)
  NleaMMS[i,j] <- Nexcr[i,j] * MS * FracleachMS 
  # Leaching, based on Equation 10.29 (g N2O per kg DM intake) 
  N2OL[i,j] <- NleaMMS[i,j] * EF5 * Ncon
  # Indirect N2O emission from leaching (g N2O day-1)
  N2OLan[i,j] <- N2OL[i,j]*(FEEDQNTY[i,j]+MILKSTART[i,j])
  # Indirect N2O emission from leaching (CO2-equivalents day-1)
  N2OLanEQ[i,j] <- N2OLan[i,j]/1000 * GWPN2O
          
  # 7. Emissions related to production of farm inputs (Not included in LiGAPS-Dairy)
  # 8. Emissions arable crops used as feed (Not included in LiGAPS-Dairy)
  # 9. Emissions from crop residues (Not included in LiGAPS-Dairy)
  #10. CO2 emissions from liming (Not included in LiGAPS-Dairy)
          
  # Greenhouse gas emissions (CO2-equivalents day-1)
  CO2EQ[i,j] <- (ENTCH4KG[i,j]+CH4MM[i,j])*GWPCH4 + 
                (N2ODan[i,j]+N2OGan[i,j]+N2OLan[i,j])/1000*GWPN2O
  
  # End of the mondule on GHG emissions        
  ###########################################################################################
  
  # Protein in live weight (kg protein per animal)
  PROTLW[i,j] <- sum(MISCFATTISACT[i,j]*PROTFRACFAT+MUSCLETISACT[i,j]*PROTFRACMUSCLE+
                     INTRAMFTISACT[i,j]*PROTFRACFAT+BONETISACT[i,j]*PROTFRACBONE+
                     NONCARCTISACT[i,j]*PROTFRACNONCACT[i,j])
  
  # Protein in beef (kg protein per animal)
  PROTBEEF[i,j] <- sum(MISCFATTISACT[i,j]*PROTFRACFAT+MUSCLETISACT[i,j]*PROTFRACMUSCLE+
                       INTRAMFTISACT[i,j]*PROTFRACFAT)
          
  # An animal is culled or slaughtered when it meets the requirements for culling or 
  # slaughter are met.
  if(SLAUGHTERDAYACT[i,j] < 9000) {breakFlagtime <- TRUE 
                                   break}
  # If the animal is culled or slaughtered, the time-loop for the animal is terminated      
  # } for the time-loop
  }
      
  # If a herd unit is simulated, a weather file is created for each of the calves in the 
  # offspring of the reproductive cow. Weather file are based on the birthdays of calves.
  
  WEATHERCALF1 <- WEATHERORIG[BIRTHDAYCALF1:(imax[j]+BIRTHDAYCALF1+2),]
  WEATHERCALF2 <- WEATHERORIG[BIRTHDAYCALF2:(imax[j]+BIRTHDAYCALF2+2),]
  WEATHERCALF3 <- WEATHERORIG[BIRTHDAYCALF3:(imax[j]+BIRTHDAYCALF3+2),]
  WEATHERCALF4 <- WEATHERORIG[BIRTHDAYCALF4:(imax[j]+BIRTHDAYCALF4+2),]
  WEATHERCALF5 <- WEATHERORIG[BIRTHDAYCALF5:(imax[j]+BIRTHDAYCALF5+2),]
  WEATHERCALF6 <- WEATHERORIG[BIRTHDAYCALF6:(imax[j]+BIRTHDAYCALF6+2),]
  WEATHERCALF7 <- WEATHERORIG[BIRTHDAYCALF7:(imax[j]+BIRTHDAYCALF7+2),]
  WEATHERCALF8 <- WEATHERORIG[BIRTHDAYCALF8:(imax[j]+BIRTHDAYCALF8+2),]
  WEATHERCALF9 <- WEATHERORIG[BIRTHDAYCALF9:(imax[j]+BIRTHDAYCALF9+2),]
  
  # The lines below assign the correct weather file to each calf.    
  if(ORDER[j] == 0) {WEATHER <- WEATHERCALF1}
  if(ORDER[j] == 1) {WEATHER <- WEATHERCALF2}
  if(ORDER[j] == 2) {WEATHER <- WEATHERCALF3}
  if(ORDER[j] == 3) {WEATHER <- WEATHERCALF4}
  if(ORDER[j] == 4) {WEATHER <- WEATHERCALF5}
  if(ORDER[j] == 5) {WEATHER <- WEATHERCALF6}
  if(ORDER[j] == 6) {WEATHER <- WEATHERCALF7}
  if(ORDER[j] == 7) {WEATHER <- WEATHERCALF8}
  if(ORDER[j] == 8) {WEATHER <- WEATHERCALF9}
      
  # Output parameters for individual animals
  
  # Beef production (kg)
  BEEFPRODHERD[j] <- c(BEEFPRODACT[ENDDAY[j],j]) 
  # Live weight production (kg)
  LWPRODHERD[j] <- c(LWPRODACT[ENDDAY[j],j])   
  # Feed conversion ratio (kg DM feed per kg beef)
  FCRHERDBEEF[j] <- c(FCRBEEF[ENDDAY[j],j])     
  # Cumulative feed intake whole life span (kg DM)
  CUMULFEEDHERD[j] <- c(CUMULFEED[ENDDAY[j],j])   
  # Cumulative feed type 1 intake whole life span (kg DM)
  CUMULFEED1HERD[j] <- c(CUMULFEED1[ENDDAY[j],j])  
  # Cumulative feed type 2 intake whole life span (kg DM)
  CUMULFEED2HERD[j] <- c(CUMULFEED2[ENDDAY[j],j])  
  # Cumulative feed type 3 intake whole life span (kg DM)
  CUMULFEED3HERD[j] <- c(CUMULFEED3[ENDDAY[j],j])  
  # Cumulative feed type 4 intake whole life span (kg DM)
  CUMULFEED4HERD[j] <- c(CUMULFEED4[ENDDAY[j],j])  
      
  # Animal lifetime (years)
  ANIMALYEARS[j] <- ENDDAY[j]/365 
  # Average weight (kg total body weight)
  AVANWEIGHT[j] <- mean(TBWACT[1:ENDDAY[j],j]) 
  # Average metabolic weight (kg empty body weight^0.75)       
  AVANMETWEIGHT[j] <- mean(EBWACTMET[1:ENDDAY[j],j]) 
  # Total milk production (kg milk)   
  CUMULMILK[j] <- sum(MILKPRODACT[1:ENDDAY[j],j]) 
      
  # Output of GHG emissions over an animal's lifetime
  
  # Total enteric metane emission per animal (kg CO2-equivalents)
  CUMULENTCH4EQ[j] <- sum(ENTCH4EQ[1:ENDDAY[j],j]) 
  # Total methane emissions from dung and urine per animal (kg CO2-equivalents)
  CUMULCH4MMEQ[j] <- sum(CH4MMEQ[1:ENDDAY[j],j])
  # Total direct N2O emissions from dung and manure (kg CO2-equivalents)
  CUMULN2ODanEQ[j] <- sum(N2ODanEQ[1:ENDDAY[j],j]) 
  # Indirect N2O emissions from dung and manure (kg CO2-equivalents) 
  CUMULN2OGanEQ[j] <- sum(N2OGanEQ[1:ENDDAY[j],j])
  # Indirect N2O emissions from dung and manure, leaching (kg CO2-equivalents) 
  CUMULN2OLanEQ[j] <- sum(N2OLanEQ[1:ENDDAY[j],j])
  # Crude protein uptake (kg CO2-equivalents)
  CUMULPROTTOTAL[j] <- sum(PROTTOTAL[1:ENDDAY[j],j])/1000
  # Protein in body tissues (kg CO2-equivalents)
  CUMULPROTLW[j] <- sum(PROTLW[ENDDAY[j],j])
  # Protein in beef (kg CO2-equivalents)
  CUMULPROTBEEF[j] <- sum(PROTBEEF[ENDDAY[j],j])
  
  # Creates a vector with birthdays of the calves      
  BIRTHDAY <- c(0,BIRTHDAYCALF1,BIRTHDAYCALF2,BIRTHDAYCALF3,BIRTHDAYCALF4,BIRTHDAYCALF5,
                BIRTHDAYCALF6,BIRTHDAYCALF7,BIRTHDAYCALF8)
  
  # Weaning days for calves are calculated from their birthdays
  WNDAY  <- BIRTHDAY+WEANINGTIME 
  WNDAY[1] <- ENDDAY[1]
        
  # The vector ANIMALINFO lists key information on animal performance 
  ANIMALINFO <- cbind(REPRODUCTIVE[j], REPLACEMENT[j], PRODUCTIVE[j], SEX[j], 
                      BEEFPRODHERD[j], CUMULFEEDHERD[j],FCRHERDBEEF[j], ANIMALYEARS[j], 
                      AVANWEIGHT[j], AVANMETWEIGHT[j], ENDDAY[j], BIRTHDAY[j], 
                      WNDAY[j], LWPRODHERD[j], CUMULFEED1HERD[j], CUMULFEED2HERD[j], 
                      CUMULFEED3HERD[j], CUMULFEED4HERD[j],CUMULENTCH4EQ[j],
                      CUMULCH4MMEQ[j],CUMULN2ODanEQ[j],CUMULN2OGanEQ[j],CUMULN2OLanEQ[j],
                      CUMULPROTTOTAL[j], CUMULPROTLW[j], CUMULPROTBEEF[j], CUMULMILK[j])
  
  # Information for individual animals is added to information for other animals     
  HERDINFO   <- rbind(HERDINFO,ANIMALINFO) 
  
  # If the reproductive cow has produced the maximum number of calves, or the simulated
  # number of calves, the animal loop is terminated      
  if(j == MAXCALFNR+1) {breakFlaganim <- TRUE         
                            break}                         
  # } for the animal-loop      
  } 
      
  ###########################################
  #         Upscaling to herd level         #
  ###########################################    
  
  # After simulations for a reproductive animal and her offspring, results can be calculated
  # for a herd unit.
  
  # Matrix to collect results for herd units    
  HERDINFO1   <- rbind(HERDINFO, matrix(nrow=0, ncol=ncol(HERDINFO), data=0))
      
  # Culling of reproductive cows, vector with probabilities on yearly survival  
  AZZAMCUMCORR <- c(CULL, (1-CULL)-(1-CULL)^2, (1-CULL)^2-(1-CULL)^3, 
                    (1-CULL)^3-(1-CULL)^4, (1-CULL)^4-(1-CULL)^5, 
                    (1-CULL)^5-(1-CULL)^6, (1-CULL)^6-(1-CULL)^7, 
                    (1-CULL)^7-(1-CULL)^8)
  
  # Probabilities for simulations where less than eight calves are born per reproductive
  # animal.
  if(MAXCALFNR < 8) AZZAMCUMCORR[MAXCALFNR] <- 1-sum(AZZAMCUMCORR[1:MAXCALFNR-1])
  if(MAXCALFNR < 8) AZZAMCUMCORR[(MAXCALFNR+1):8] <- 0
      
  # Cows and calves are separated at weaning (WNDAY)
  # Calculate the probability that a cow is still in the herd at a WNDAY of a calf.
  
  # Moment of conception for calves (days)
  AA <- (WNDAY[WNDAY >0]/365)-(GestPer+WEANINGTIME)/365  
  # Moment of conception rounded (days)
  BB <- floor(AA)+1                                      
  BB[1] <- 1
  # Probability for productive calves to be boren, probability for replacement calf equals 1
  # to maintain herd size
  CC <- AZZAMCUMCORR
  CC[length(CC)] <- 1-sum(CC[1:length(CC)-1]) 
  
  # Variables used to calculate the main results for an reproductive animal in a herd unit    
  REPRBEEF     = NULL
  REPRFEED     = NULL
  REPRFEED1    = NULL
  REPRFEED2    = NULL
  REPRFEED3    = NULL
  REPRFEED4    = NULL
      
  REPRFCR      = NULL
  REPRAVANW    = NULL
  REPRAVANWMET = NULL
  REPRLW       = NULL
  REPRMILK     = NULL
      
  REPRCUMULENTCH4EQ = NULL
  REPRCUMULCH4MMEQ  = NULL
  REPRCUMULN2ODanEQ = NULL                 
  REPRCUMULN2OGanEQ = NULL                  
  REPRCUMULN2OLanEQ = NULL
  REPRCUMULPROTTOTAL= NULL
  REPRCUMULPROTLW   = NULL
  REPRCUMULPROTBEEF = NULL
      
  # The p-loop below calculates the beef production and feed intake for different culling 
  # scenarios for the reproductive animal.
  
  # p-loop, where p indicates the number of calves
  for(p in 1:length(AA-8+MAXCALFNR)){
  
  # Beef production from reproductive animal (kg)  
  REPRBEEF[p]     <- MUSCLETISACT[WNDAY[p],1] + INTRAMFTISACT[WNDAY[p],1] + 
                     MISCFATTISACT[WNDAY[p],1]
  # live weight production from reproductive animal (kg TBW)  
  REPRLW[p]       <- TBWACT[WNDAY[p],1]
  # Feed intake reproductive animal (kg DM)  
  REPRFEED[p]     <- CUMULFEED[WNDAY[p],1]
  # Feed intake from feed type 1 by the reproductive animal (kg DM)  
  REPRFEED1[p]    <- CUMULFEED1[WNDAY[p],1]
  # Feed intake from feed type 2 by the reproductive animal (kg DM)
  REPRFEED2[p]    <- CUMULFEED2[WNDAY[p],1]
  # Feed intake from feed type 3 by the reproductive animal (kg DM)
  REPRFEED3[p]    <- CUMULFEED3[WNDAY[p],1]
  # Feed intake from feed type 4 by the reproductive animal (kg DM)
  REPRFEED4[p]    <- CUMULFEED4[WNDAY[p],1]
  # Feed conversion ratio, based on beef production of the reproductive animal (kg DM kg 
  # live weight)
  REPRFCR[p]      <- FCR[WNDAY[p],1]
  # Average total body weight of the reproductive animal (kg)
  REPRAVANW[p]    <- mean(TBWACT[1:WNDAY[p],1])
  # Average metabolic body weight of the reproductive animal (kg EBW^0.75)
  REPRAVANWMET[p] <- mean(EBWACTMET[1:WNDAY[p],1])
  # Milk production over the animal's lifetime (kg)
  REPRMILK[p]     <- sum(MILKPRODACT[1:WNDAY[p],1]) 
        
  # Enteric methane emissions (kg CO2-equivalents) 
  REPRCUMULENTCH4EQ[p] <- sum(ENTCH4EQ[1:WNDAY[p],1])
  # Methane emissions from manure management (kg CO2-equivalents) 
  REPRCUMULCH4MMEQ[p]  <- sum(CH4MMEQ[1:WNDAY[p],1])
  # Direct N2O emission from manure management (kg CO2-equivalents)  
  REPRCUMULN2ODanEQ[p] <- sum(N2ODanEQ[1:WNDAY[p],1])                 
  # Indirect N2O emission from manure management (kg CO2-equivalents)  
  REPRCUMULN2OGanEQ[p] <- sum(N2OGanEQ[1:WNDAY[p],1])
  # Indirect N2O emission from manure management, leaching (kg CO2-equivalents)
  REPRCUMULN2OLanEQ[p] <- sum(N2OLanEQ[1:WNDAY[p],1])
  # Total digested protein (kg)
  REPRCUMULPROTTOTAL[p]<- sum(PROTTOTAL[1:WNDAY[p],1])/1000
  # Total protein in body tissues (kg)
  REPRCUMULPROTLW[p]   <- sum(PROTLW[WNDAY[p],1])
  # Total protein in beef (kg)
  REPRCUMULPROTBEEF[p] <- sum(PROTBEEF[WNDAY[p],1])
  
  # End of the p-loop
  }
  
  # Vector CC is duplicated
  CC1 <- CC                  
  # At least one calf (replacement female) is born in every herd unit, so the first number
  # in the vector CC1 is a zero.
  CC1 <- c(0,CC1[1:8])
  # If less than nine calves are born per herd unit, the calves that are nog born have a
  # probability of zero.
  CC1 <- c(CC1,rep(0,(9-length(CC1))))
      
  # Vector REPRINFO lists key information on the cow in a herd unit
  REPRINFO  <- matrix(nrow=(length(AA)), ncol = 27, 
                      data= c(REPRODUCTIVE[1:length(AA)], REPLACEMENT[1:length(AA)], 
                              PRODUCTIVE[1:length(AA)],SEX[1:length(AA)],REPRBEEF,REPRFEED,
                              REPRFCR, WNDAY[1:length(AA)]/365, REPRAVANW,REPRAVANWMET,
                              WNDAY[1:length(AA)], REPRLW, CC1, CC1, REPRFEED1, REPRFEED2, 
                              REPRFEED3, REPRFEED4,REPRCUMULENTCH4EQ,REPRCUMULCH4MMEQ,
                              REPRCUMULN2ODanEQ,REPRCUMULN2OGanEQ,REPRCUMULN2OLanEQ,
                              REPRCUMULPROTTOTAL,REPRCUMULPROTLW,REPRCUMULPROTBEEF, 
                              REPRMILK))
  
  # Multiplication of production and feed intake with probabilities, which add up to a 
  # probability of 1)      
  REPRINFO1 <- REPRINFO * CC1 
  # Lists production and feed intake of the cow, including the (culling) probabilities
  REPRINFO2 <- colSums(REPRINFO1)
  
  # Key information on the calves in a herd unit
  
  # If no calves are born, there is no information on calves. The replacement calf (j=2) is
  # not included in the calculations, because this calf will give rise to a herd unit 
  # itself. Hence, there need to be at least one more calf than the replacement calf to fill
  # out PRODINFO.
  if(MAXCALFNR == 0) PRODINFO <- rep(0,18) else  
    if(MAXCALFNR < 2) PRODINFO <- HERDINFO1[3,] else 
      PRODINFO  <- HERDINFO1[3:(MAXCALFNR+1),] 
      
  # Cumulative Probabilities for calves to be born
  DD <- c(1,1,(1-(CC[1])),(1-(sum(CC[1:2]))),(1-(sum(CC[1:3]))),(1-(sum(CC[1:4]))),
          (1-(sum(CC[1:5]))),(1-(sum(CC[1:6]))),(1-(sum(CC[1:7]))),(1-(sum(CC[1:8]))),
          (1-(sum(CC[1:9])))) 
      
  # Multiplies production and feed intake of calves not used for replacement with 
  # probabilities      
  if(MAXCALFNR < 3) PRODINFO1 <- PRODINFO else PRODINFO1 <- PRODINFO * DD[3:(MAXCALFNR+1)] 
  # Lists production and feed intake for calves not used for replacement, including the 
  # culling probabilities
  if(MAXCALFNR < 3) PRODINFO2 <- PRODINFO else PRODINFO2 <- colSums(PRODINFO1) 
      
      
  # The vectors below list the information for the herd unit (header)
  header <- c("Reproductive","Replacement","Productive","Sex", "Meat prod. (kg)", 
              "Feed cons.(kg)","FCR", "An. years", "An. avg. weight (kg)", 
              "An. avg. met. weight (kg)","Endday", "Birthday", "Weaning day")
      
  # Information on the reproductive animal and the calves (excl. replacement calf) is merged
  HERDINFO2 <- colSums(rbind(REPRINFO2, PRODINFO2))
  
  # Collects information for herd units
  OUTPUTHERD <- rbind(REPRINFO2, PRODINFO2, HERDINFO2)
  # Collects information on thermoregulation
  OUTPUT1 <- cbind(Metheatopt, TNRESP, ACTSW, LWRCOAT, CONVCOAT, SWR, TskinC, TcoatC, TAVGC, 
                   MetheatBAL)
  # More calculations in the information for herd units
  OUTPUT2 <- cbind(HERDINFO2[5], HERDINFO2[6], HERDINFO2[5]/HERDINFO2[6], HERDINFO2[6]/
                     HERDINFO2[5])
  
  # Collection of output parameters    
  OUTPUTHERDS  <- rbind(OUTPUTHERDS,OUTPUTHERD)
  OUTPUTHERDS2 <- c(OUTPUTHERDS[1,12], OUTPUTHERDS[2,14], 
                    OUTPUTHERDS[1,12]+OUTPUTHERDS[2,14]) 
  OUTPUTHERDS1 <- rbind(OUTPUTHERDS1, c(OUTPUTHERD[3,6]/OUTPUTHERD[3,5], 
                                        OUTPUTHERD[3,5]/OUTPUTHERD[3,8]))
  
  # Feed efficiency (g beef kg DM intake) for the calves in a herd unit
  FESENSIND[s] <- ANIMALINFO[5]/ANIMALINFO[6]*1000         
  # Feed efficiency (g beef kg DM intake) for cow in a herd unit
  FESENSHERD[s] <- OUTPUTHERDS[3,5]/OUTPUTHERDS[3,6]*1000  
  # Feed efficiency (g beef kg DM intake) for the herd unit
  FESENSREPR[s] <- OUTPUTHERDS[1,5]/OUTPUTHERDS[1,6]*1000  
  
  # End of the sensitivity-loop (not used to evaluate the performance of LiGAPS-Dairy)    
  }
     
  # Feed efficiency for individual cattle under sensitivity analysis    
  FESENSIND <- matrix(ncol=1, nrow=NPAR, data = FESENSIND) 
    
  
    
  ###########################################################################################
  #                                     3. Output section                                   #
  ###########################################################################################
  
  # Milk production starts after parturition
  
  # End of milk production coincides with weaning (days)
  ENDMILKWEANING <- BIRTHDAY + WEANINGTIME - 1
  ENDMILKWEANING[1] <- 0  
  
  # Table with key information on cattle performance. This information is presented in the 
  # paper describing LiGAPS-Dairy
  
  # Milk production (L per lactation)
  Lactation <- c(sum(MILKPRODACT[BIRTHDAY[2]:ENDMILKWEANING[2],1]),
                 sum(MILKPRODACT[BIRTHDAY[3]:ENDMILKWEANING[3],1]),
                 sum(MILKPRODACT[BIRTHDAY[4]:ENDMILKWEANING[4],1]),
                 sum(MILKPRODACT[BIRTHDAY[5]:ENDMILKWEANING[5],1]),
                 sum(MILKPRODACT[BIRTHDAY[6]:ENDMILKWEANING[6],1]),
                 sum(MILKPRODACT[BIRTHDAY[7]:ENDMILKWEANING[7],1]),
                 sum(MILKPRODACT[BIRTHDAY[8]:ENDMILKWEANING[8],1]),
                 sum(MILKPRODACT[BIRTHDAY[9]:ENDMILKWEANING[9],1]))
  
  # Genetic potential for milk production (L per lactation)
  LactationPOT <- c(sum(POTMILKPROD[BIRTHDAY[2]:ENDMILKWEANING[2],1]),
                    sum(POTMILKPROD[BIRTHDAY[3]:ENDMILKWEANING[3],1]),
                    sum(POTMILKPROD[BIRTHDAY[4]:ENDMILKWEANING[4],1]),
                    sum(POTMILKPROD[BIRTHDAY[5]:ENDMILKWEANING[5],1]),
                    sum(POTMILKPROD[BIRTHDAY[6]:ENDMILKWEANING[6],1]),
                    sum(POTMILKPROD[BIRTHDAY[7]:ENDMILKWEANING[7],1]),
                    sum(POTMILKPROD[BIRTHDAY[8]:ENDMILKWEANING[8],1]),
                    sum(POTMILKPROD[BIRTHDAY[9]:ENDMILKWEANING[9],1]))
  
  # Relative yield gap, based on the genetic potential for milk production (%). Note: this
  # is not the relative yield gap according to concepts of production ecology, because 
  # that yield gap is benchmarked against potential production, which is defined by both the 
  # genotype and the climate.
  
  LactationYG <- (1-Lactation/LactationPOT)*100 
  
  # Protein use efficiency (%)
  NUE <- (OUTPUTHERDS[3,25]+sum(MILKPRODACT[1:ENDDAY[1],1]*PROTFRACMILK[1:ENDDAY[1],1]))/
         OUTPUTHERDS[1,24]*100 
  
  # Collection of performance parameters 
  DATAt3 <-c(OUTPUTHERDS[1,5],TBWACT[ENDDAY[1],1],OUTPUTHERDS[1,19], 
             (OUTPUTHERDS[1,19]/OUTPUTHERDS[1,27]),OUTPUTHERDS[1,24],OUTPUTHERDS[3,25],
             OUTPUTHERDS[1,26],OUTPUTHERDS[1,27], OUTPUTHERDS[1,27]/
               ((REPRINFO2[11]-BIRTHDAY[2])/(365)),
             (sum(POTMILKPROD[BIRTHDAY[2]:ENDMILKWEANING[2],1])*8)/(7+WEANINGTIME/365), 
             (1-(OUTPUTHERDS[1,27]/((REPRINFO2[11]-BIRTHDAY[2])/(365)))/
                ((sum(POTMILKPROD[BIRTHDAY[2]:ENDMILKWEANING[2],1])*(MAXCALFNR))/
                   (MAXCALFNR-1+WEANINGTIME/365)))*100,
             sum(MILKPRODACT[1:ENDDAY[1],1]*PROTFRACMILK[1:ENDDAY[1],1]), 
             OUTPUTHERDS[1,27]*1000/OUTPUTHERDS[1,6],
             NUE)
  
  # Creates a table from the collected performance parameters
  TABLEDATA <- matrix(ncol=14, nrow=1, data=DATAt3)
  
  rownames(TABLEDATA) <- z
  colnames(TABLEDATA) <- c("Beef production repr. cow (kg)",
                           "Slaughter weight repr. cow (kg)", 
                           "Enteric CH4 (kg CO2 eq.)", 
                           "Enteric CH4 (kg CO2 eq. per kg milk)",  
                           "CP uptake","Prot. LW (kg)","Prot. beef (kg)",
                           "Milk prod. (L)", "Milk prod. (L year-1)",
                           "Pot. milk prod. (L year-1)","Rel. YG (%)",
                           "Prot. milk (kg)", "FE milk (mL kg-1 DM)","NUE")
  
  # Count of the number of individual animals or herd units (#)
  COUNT = COUNT + 1
  
  if(COUNT==1) TABLEOUTPUT <- TABLEDATA
  if(COUNT> 1) TABLEOUTPUT <- rbind(TABLEOUTPUT,TABLEDATA)
  
  # Days in milk / days after parturition (days) 
  DIM <- c(MEASUREDOUTPUT$Time-min(MEASUREDOUTPUT$Time)) 
  # Days in milk / days after parturition, calculated from feed intake data (days)
  DIM2 <- c(FEEDINPUT$Time-min(FEEDINPUT$Time)) 
  
  # Difference simulated and measured weight at the start of the experiment (kg total body
  # weight)
  DIFFWEIGHT    <- TBWACT[min(MEASUREDOUTPUT$Time),1] - MEASUREDOUTPUT$WeightKG[1]
  # Difference simulated and measured weight at the end of the experiment (kg total body
  # weight)
  DIFFENDWEIGHT <- TBWACT[max(MEASUREDOUTPUT$Time),1] - 
                   MEASUREDOUTPUT$WeightKG[length(MEASUREDOUTPUT$WeightKG)]
  
  # Interpolation of measured weekly weights (kg total body weight) to daily weights
  MEASUREDWEIGHT <- approx(x=MEASUREDOUTPUT$Time, y=MEASUREDOUTPUT$WeightKG, 
                         method="linear", 
                         xout= c(min(MEASUREDOUTPUT$Time):max(MEASUREDOUTPUT$Time)), rule=2)
  MEASUREDWEIGHT <- MEASUREDWEIGHT$y
  
  # Average measured weight during the experiments (kg total body weight)
  MEASUREDWEIGHTlact <- mean(MEASUREDWEIGHT) 
  # Average simulated weight during the experiments (kg total body weight)
  SIMULATEDWEIGHTlact <- mean(TBWACT[min(MEASUREDOUTPUT$Time):max(MEASUREDOUTPUT$Time)])
  # Difference average simulated and measured weight during experiments (kg total body 
  # weight) 
  DIFFAVGWEIGHT <- SIMULATEDWEIGHTlact-MEASUREDWEIGHTlact
  
  # Interpolation of the fat- and protein-corrected (FPCM) milk production from weekly 
  # production to daily production. 
  MEASUREDFPCM <- approx(x=MEASUREDOUTPUT$Time, y=MEASUREDOUTPUT$FPCMKG, 
                             method="linear", xout= c(min(MEASUREDOUTPUT$Time):
                                                        max(MEASUREDOUTPUT$Time)), rule=2)
  MEASUREDFPCM <- MEASUREDFPCM$y
  
  # If two or more calves are born during experiments (i.e. two or more lactations), no milk
  # is produced in the time between weaning and  birth. 
  if(length(WEANINGTIMEvec) >= 2) MEASUREDFPCM[(WEANINGTIMEvec[1]+1):
                                                 (WEANINGTIMEvec[1]+GESTINTERVALvec[1])] <-0 
  
  # Measured FPCM production in the first 210 days of an experiment (kg FPCM per day) 
  ActualSEQ <- rbind(ActualSEQ,MEASUREDFPCM[1:210])
  # Simulated FPCM production in the first 210 days of an experiment (kg FPCM per day)
  FeedlimSEQ <- rbind(FeedlimSEQ,
                      na.omit(c(MILKPRODACTFPCM[min(MEASUREDOUTPUT$Time):
                                                  (min(MEASUREDOUTPUT$Time)+210)])))
  # Measured milk production per lactation (kg FPCM per lactation)
  MEASUREDFPCMlact <- sum(MEASUREDFPCM)/length(WEANINGTIMEvec) 
  # Measured milk production per lactation (kg FPCM per lactation)
  SIMULATEDFPCMlact <- sum(na.omit(c(MILKPRODACTFPCM[min(MEASUREDOUTPUT$Time):
                      max(MEASUREDOUTPUT$Time)])))/length(WEANINGTIMEvec) 
  # Difference simulated and measured FPCM production (kg FPCM per lactation)
  DIFFlact <- SIMULATEDFPCMlact-MEASUREDFPCMlact 
  # Relative difference simulated and measured FPCM production (%), expressed as a % of 
  # measured FPCM per lactation
  relDIFFlact <- DIFFlact/MEASUREDFPCMlact * 100 
  
  # The matrix evaluation collects the data om measured and simulated FPCM production
  EVALUATION <- c(MEASUREDFPCMlact,SIMULATEDFPCMlact,DIFFlact,relDIFFlact)
  EVALUATIONALL <- rbind(EVALUATIONALL,EVALUATION)
  
  # Daily fat- and protein-corrected milk production (kg FPCM per day) 
  
  # Mean Absolute Error daily FPCM production (kg FPCM per day)
  MAEdaily <- abs(c(MILKPRODACTFPCM[min(MEASUREDOUTPUT$Time):max(MEASUREDOUTPUT$Time)])-
                    MEASUREDFPCM) 
  # Relative Mean Absolute Error (%), expressed as a % of measured FPCM per day
  relMAEdaily <- MAEdaily/MEASUREDFPCM * 100 
  relMAEdaily <- relMAEdaily[is.finite(relMAEdaily)]
  # Average relative Mean Absolute Error (%), expressed as a % of measured FPCM per day
  relMAEdailylact <- mean(relMAEdaily) 
  
  # Root Mean Squared Error for daily FPCM production (kg FPCM per day)
  RMSEdaily <- (sum((c(MILKPRODACTFPCM[min(MEASUREDOUTPUT$Time):max(MEASUREDOUTPUT$Time)])-
                       MEASUREDFPCM)^2)/length(MEASUREDFPCM[MEASUREDFPCM>0]))^0.5 
  # Relative Root Mean Squared Error (%), expressed as a % of measured FPCM per day
  relRMSEdaily <- RMSEdaily/MEASUREDFPCM * 100 
  relRMSEdaily <- relRMSEdaily[is.finite(relRMSEdaily)]
  # Average of the relative Root Mean Squared Error (%)
  relRMSEdailylact <- mean(relRMSEdaily) 
  
  # Mean Squared Error (kg2 FPCM day-2)
  MSEdaily <- (sum((c(MILKPRODACTFPCM[min(MEASUREDOUTPUT$Time):max(MEASUREDOUTPUT$Time)])-
                      MEASUREDFPCM)^2)/length(MEASUREDFPCM[MEASUREDFPCM>0])) 
  # Simulated FPCM production (kg FPCM per experiment)
  SIMULATEDFPCM <- c(MILKPRODACTFPCM[min(MEASUREDOUTPUT$Time):max(MEASUREDOUTPUT$Time)])
  
  # Approach of Theil (1966) and Bibby and Toutenburg (1977) to split up the MSE or RMSE in 
  # three components: bias, slope, and random. More information on this method can be found
  # in McPhee and Walmsley (2017).
  
  # Linear model for simulated and measured FPCM production (kg FPCM per day)
  stats<-summary(lm(MEASUREDFPCM~SIMULATEDFPCM))
  # Mean Squared Prediction Error (kg2 FPCM day-2)
  MSPE <- sum((SIMULATEDFPCM-MEASUREDFPCM)^2)/length(SIMULATEDFPCM)
  # Bias component of MSPE (kg2 FPCM day-2)
  BIAS <- (mean(SIMULATEDFPCM)-mean(MEASUREDFPCM))^2
  # Relative bias as % of MSPE
  biasrelRMSEdailylact <- BIAS/MSPE*100 
  # Slope component of MSPE (kg2 FPCM day-2)
  SLOPE2 <- sum((SIMULATEDFPCM-mean(SIMULATEDFPCM))^2)/length(SIMULATEDFPCM)*
    (1-stats$coefficients[2,1])^2
  # Relative slope as % of MSPE
  sloperelRMSEdailylact <- SLOPE2/MSPE*100 
  # Random component of MSPE (kg2 FPCM day-2) 
  RANDOM <- (1-stats$r.squared)*sum((MEASUREDFPCM-mean(MEASUREDFPCM))^2)/
    length(MEASUREDFPCM)
  # Relative random component as % of MSPE
  randomrelRMSEdailylact <- RANDOM/MSPE*100 
  
  # Collection of the indicators for the evaluation of daily FPCM production
  EVALUATIONDAILY <- c(mean(MEASUREDFPCM),mean(SIMULATEDFPCM),
                       mean(SIMULATEDFPCM)-mean(MEASUREDFPCM),
                       (mean(SIMULATEDFPCM)-mean(MEASUREDFPCM))/mean(MEASUREDFPCM)*100,
                       mean(MAEdaily),mean(relMAEdaily),RMSEdaily,mean(relRMSEdaily),
                       biasrelRMSEdailylact,sloperelRMSEdailylact,randomrelRMSEdailylact,
                       stats$coefficients[1,4],stats$coefficients[2,4],
                       stats$coefficients[1,1],stats$coefficients[2,1])
  EVALUATIONDAILYALL <- rbind(EVALUATIONDAILYALL,EVALUATIONDAILY)
  
  
  # Feed intake per lactation
  
  # Interpolates weekly feed intake to daily feed intake (kg DM day-1) 
  MEASUREDFI <- approx(x=FEEDINPUT$Time, y=c(FIDAILYTOTAL), 
                       method="linear", xout= c(min(FEEDINPUT$Time):max(FEEDINPUT$Time)), 
                       rule=2)
  MEASUREDFI <- MEASUREDFI$y
  # Average measured feed intake per lactation (kg DM per lactation)
  MEASUREDFIlact <- sum(MEASUREDFI)/length(WEANINGTIMEvec) 
  # Average simulated feed intake per lactation (kg DM per lactation)
  SIMULATEDFIlact <- sum(FEEDQNTY[min(FEEDINPUT$Time):max(FEEDINPUT$Time),1])/
    length(WEANINGTIMEvec) 
  # Diffence simulated and measured feed intake per lactation (kg DM per lactation)
  DIFFFIlact <- SIMULATEDFIlact-MEASUREDFIlact
  # Relative difference simulated and measured feed intake (%), expressed as % of the 
  # measured feed intake
  relDIFFFIlact <- DIFFFIlact/MEASUREDFIlact * 100
  # Measured feed intake as fraction of simulated feed intake (-)
  IntaketoSupply <- MEASUREDFIlact/SIMULATEDFIlact
  
  # Collection of indicators for evaluation of the feed intake per lactation
  EVALUATIONFI <- c(MEASUREDFIlact,SIMULATEDFIlact,DIFFFIlact,relDIFFFIlact)
  EVALUATIONFIALL <- rbind(EVALUATIONFIALL,EVALUATIONFI)
  
  # Measured feed intake production per experiment (kg DM per experiment)
  MEASUREDFIlacttotal <- sum(MEASUREDFI)
  # Simulated feed intake per experiment (kg DM per experiment)
  SIMULATEDFIlacttotal <- sum(na.omit(c(FEEDQNTY[min(FEEDINPUT$Time):max(FEEDINPUT$Time)]))) 
  # Mean Absolute Error between simulated and measured feed intake per experiment (kg DM 
  # per experiment)
  MAEFIlact <- abs(SIMULATEDFIlacttotal-MEASUREDFIlacttotal) 
  # Relative Mean Absolute Error (%), expressed as a % of measured feed intake per 
  # experiment
  relMAEFIlact <- MAEFIlact/MEASUREDFIlacttotal * 100 
  
  # Daily feed intake
  
  # Mean Absolute Error for daily feed intake (kg DM per day) 
  MAEdaily <- abs(c(FEEDQNTY[min(FEEDINPUT$Time):max(FEEDINPUT$Time)])-MEASUREDFI) 
  # Relative Mean Absolute Error (%), expressed as a % of measured daily feed intake
  relMAEdaily <- MAEdaily/MEASUREDFI * 100
  relMAEdaily <- relMAEdaily[is.finite(relMAEdaily)]
  # Average of the relative Mean Absolute Error (kg DM day-1), expressed as a % of measured 
  # feed intake
  relMAEdailylact <- mean(relMAEdaily) 
  
  # Root Mean Squared Error for daily feed intake (kg DM per day)
  RMSEdaily <- (sum((c(FEEDQNTY[min(FEEDINPUT$Time):max(FEEDINPUT$Time)])-MEASUREDFI)^2)/
                  length(MEASUREDFI[MEASUREDFI>0]))^0.5 
  # Relative Mean Absolute Error (%), expressed as a % of measured feed intake per day
  relRMSEdaily <- RMSEdaily/MEASUREDFI * 100 
  relRMSEdaily <- relRMSEdaily[is.finite(relRMSEdaily)]
  # Average of the relative Root Mean Squared Error (kg DM per day)
  relRMSEdailylact <- mean(relRMSEdaily) 
  # Mean Squared Error for feed intake (kg2 DM day-2)
  MSEdaily <- (sum((c(MILKPRODACTFPCM[min(MEASUREDOUTPUT$Time):max(MEASUREDOUTPUT$Time)])-
                      MEASUREDFPCM)^2)/length(MEASUREDFPCM[MEASUREDFPCM>0])) 
  # Simulated feed intake (kg DM per day)
  SIMULATEDFI <- c(FEEDQNTY[min(FEEDINPUT$Time):max(FEEDINPUT$Time)]) 
  
  # Approach of Theil (1966) and Bibby and Toutenburg (1977) to split up the MSE or RMSE in 
  # three components: bias, slope, and random. More information on this method can be found
  # in McPhee and Walmsley (2017).
  
  # Linear model for simulated and measured feed intake (kg DM per day) 
  stats<-summary(lm(MEASUREDFI~SIMULATEDFI))
  # Mean Squared Prediction Error (kg2 DM day-2)
  MSPE <- sum((SIMULATEDFI-MEASUREDFI)^2)/length(SIMULATEDFI)
  # Bias component of MSPE (kg2 DM day-2)
  BIAS <- (mean(SIMULATEDFI)-mean(MEASUREDFI))^2
  # Relative bias as % of MSPE
  biasrelRMSEdailylact <- BIAS/MSPE*100
  # Slope component of MSPE (kg2 DM day-2)
  SLOPE2 <- sum((SIMULATEDFI-mean(SIMULATEDFI))^2)/length(SIMULATEDFI)*
    (1-stats$coefficients[2,1])^2
  # relative slope as % of MSPE
  sloperelRMSEdailylact <- SLOPE2/MSPE*100
  # Random component of MSPE (kg2 DM day-2)
  RANDOM <- (1-stats$r.squared)*sum((MEASUREDFI-mean(MEASUREDFI))^2)/length(MEASUREDFI)
  # relative random component as % of MSPE
  randomrelRMSEdailylact <- RANDOM/MSPE*100 
  
  # Collection of indicators for evaluation of daily feed intake
  EVALUATIONDAILYFI <- c(mean(MEASUREDFI),mean(SIMULATEDFI),
                         mean(SIMULATEDFI)-mean(MEASUREDFI),
                         (mean(SIMULATEDFI)-mean(MEASUREDFI))/mean(MEASUREDFI)*100,
                         mean(MAEdaily),mean(relMAEdaily),RMSEdaily,mean(relRMSEdaily),
                         biasrelRMSEdailylact,sloperelRMSEdailylact,randomrelRMSEdailylact,
                         stats$coefficients[1,4],stats$coefficients[2,4],
                         stats$coefficients[1,1],stats$coefficients[2,1])
  EVALUATIONDAILYFIALL <- rbind(EVALUATIONDAILYFIALL,EVALUATIONDAILYFI)
  
  # Simulated genetic potential milk production per lactation (kg FPCM per lactation)
  SIMULATEDFPCMPOTlact <- sum(c(MILKPRODPOTFPCM[min(MEASUREDOUTPUT$Time):
                                                  max(MEASUREDOUTPUT$Time)])) 
  # Simulated genetic potential milk production in the first 210 days of the lactation (kg 
  # FPCM per lactation)
  PotSEQ <- rbind(PotSEQ,na.omit(c(MILKPRODPOTFPCM[min(MEASUREDOUTPUT$Time):
                                                     (min(MEASUREDOUTPUT$Time)+210)])))
  
  # Yield gap for fat- and protein-corrected milk production per lactation (kg FPCM per 
  # lactation). Benchmark is the genetic potential production.
  YGp     <- SIMULATEDFPCMPOTlact - MEASUREDFPCMlact
  # Yield gap for fat- and protein-corrected milk production per lactation (kg FPCM per 
  # lactation). Benchmark is the feed-limited production.
  YGfl    <- SIMULATEDFPCMlact - MEASUREDFPCMlact
  
  # Relative yield gap for fat- and protein-corrected milk production per lactation (%). 
  # Benchmark is the genetic potential production.
  relYGp  <- YGp/SIMULATEDFPCMPOTlact * 100
  # Relative yield gap for fat- and protein-corrected milk production per lactation (%). 
  # Benchmark is the feed-limited production.
  relYGfl <- YGfl/SIMULATEDFPCMlact * 100
  
  # Collection of performance parameters for individual cows. Data on weights are included
  # for experiments 8 and 22. Measured weights were not available for experiment 63 (van 
  # Duinkerken et al. 2005)
  if(EXPERIMENT ==8 || EXPERIMENT ==22) 
    EVALUATIONCOW <- c(MEASUREDOUTPUT$Cow[1],SIMULATEDFPCMPOTlact,SIMULATEDFPCMlact,
                       MEASUREDFPCMlact,YGp,YGfl,relYGp,relYGfl,DIFFlact,relDIFFlact,
                       relMAEdailylact, MEASUREDOUTPUT$WeightKG[1], DIFFWEIGHT,DIFFAVGWEIGHT,
                       DIFFENDWEIGHT,IntaketoSupply, relRMSEdailylact,biasrelRMSEdailylact,
                       sloperelRMSEdailylact,randomrelRMSEdailylact,MAEFIlact,relMAEFIlact,
                       NA,NA,NA,NA,NA, DIFFFIlact,MEASUREDFIlact,SIMULATEDFIlact)
  
  if(EXPERIMENT ==63) 
    EVALUATIONCOW <- c(MEASUREDOUTPUT$Cow[1],SIMULATEDFPCMPOTlact,SIMULATEDFPCMlact,
                       MEASUREDFPCMlact,YGp,YGfl,relYGp,relYGfl,DIFFlact,relDIFFlact,
                       relMAEdailylact, NA, NA,NA,NA,IntaketoSupply, relRMSEdailylact,
                       biasrelRMSEdailylact,sloperelRMSEdailylact,randomrelRMSEdailylact,
                       MAEFIlact,relMAEFIlact,NA,NA,NA,NA,NA, DIFFFIlact,MEASUREDFIlact,
                       SIMULATEDFIlact)
  
  EVALUATIONCOWS <- rbind(EVALUATIONCOWS,EVALUATIONCOW)
  
  # Most defining and limiting factors for milk production and growth
  
  # Days were feed intake is reduced due to heat stress (MJ day-1)
  HEATSTRESS <- REDHP
  # Number of days with heat stress during an experiment(#)
  NRHEATSTRESS <- length(which(HEATSTRESS[min(MEASUREDOUTPUT$Time):
                                            max(MEASUREDOUTPUT$Time),1] < 0)) 
  # Days with heat stress (0 = no heat stress; 1 = heat stress)
  HEATSTRESSfact <- HEATSTRESS[min(MEASUREDOUTPUT$Time):max(MEASUREDOUTPUT$Time),1]
  HEATSTRESSfact[HEATSTRESSfact<0] <-1 
    
  # Average daily maximum temperature during experiments (degrees Celsius)
  TAVGCexpmax <- WEATHER$MAXT[min(MEASUREDOUTPUT$Time):max(MEASUREDOUTPUT$Time)]
  # Average daily minimum temperature during experiments (degrees Celsius)
  TAVGCexpmin <- WEATHER$MINT[min(MEASUREDOUTPUT$Time):max(MEASUREDOUTPUT$Time)]
  # Average daily maximum temperature during days at which heat stress occurs (degrees 
  # Celsius)
  TempHEATSTRESS <- TAVGCexpmax[REDHP[min(MEASUREDOUTPUT$Time):
                                        max(MEASUREDOUTPUT$Time),1]<0]
  # Days with cold stess (W m-2)
  COLDSTRESS <- Metheatcold-(HEATIFEEDMAINTWM+(HEATMILK[i,j]+HEATTOTALACT[i,j]+
                (ENFEEDLACT[i,j]+ENTOTALACT[i,j])*(Digestfracfeed[i,j]/
                (1-Digestfracfeed[i,j])))*1000000/(3600*24*AREA[i,j]))
  # Number of days with heat stress during an experiment(#)
  NRCOLDSTRESS <- length(which(COLDSTRESS[min(MEASUREDOUTPUT$Time):
                                            max(MEASUREDOUTPUT$Time),1] > 0))
  # Average daily minimum temperature during days at which cold stress occurs (degrees 
  # Celsius)
  TempCOLDSTRESS <- TAVGCexpmin[COLDSTRESS[min(MEASUREDOUTPUT$Time):
                                             max(MEASUREDOUTPUT$Time),1]>0]
  # Days with cold stress (0 = no cold stress; 1 = cold stress)
  COLDSTRESSfact <- COLDSTRESS[min(MEASUREDOUTPUT$Time):max(MEASUREDOUTPUT$Time),1]
  COLDSTRESSfact[COLDSTRESSfact>0] <-1
  COLDSTRESSfact[COLDSTRESSfact<0] <-0
  
  # Rumen fill during experiments (fraction maximum digestion capacity)
  FILLGIT1 <- FILLGIT[min(MEASUREDOUTPUT$Time):max(MEASUREDOUTPUT$Time),1]
  # If cold stress is the primary cause, an the additional feed requirements result in a
  # feed intake that equals the maximum digestion capacity, cold stress is considered to be
  # the defining factor for growth, not digestion capacity.
  FILLGIT1[COLDSTRESSfact==1] <-0
  # Number of days in experiments were digestion capacity is limiting (#)
  NRFILLGIT <- length(which(FILLGIT1>= 0.999))
  # Days with digestion capacity limitations (0 = no limitation; 1 = limitation)
  FILLGITfact <- FILLGIT1
  FILLGITfact[FILLGITfact>= 0.9990] <-1
  FILLGITfact[FILLGITfact<0.9990] <-0
  
  # Protein balance (g protein per day). Negative values indicate protein deficiency
  PROTGRAPH <- PROTBAL
  PROTGRAPH[PROTGRAPH<0] <- 0.5
  PROTGRAPH[PROTGRAPH!=0.5] <- NA
  PROTGRAPH[FILLGIT>=0.999] <- NA
  PROTGRAPH[HEATSTRESS<0] <- NA
  PROTGRAPH[COLDSTRESS>0] <- NA
  # Number of days in experiments with protein deficiency (#)
  NRPROTGRAPH <- length(which(PROTGRAPH[min(MEASUREDOUTPUT$Time):
                                          max(MEASUREDOUTPUT$Time),1] == 0.5))
  # Days with protein deficiency (0 = no limitation; 1 = limitation)
  PROTDEFfact <- PROTGRAPH[min(MEASUREDOUTPUT$Time):max(MEASUREDOUTPUT$Time),1]
  PROTDEFfact[PROTDEFfact==0.5] <-1
  PROTDEFfact[is.na(PROTDEFfact)] <-0
  
  # Daily average protein content of the diet (g CP per kg DM)
  CPAVG1 <- CPAVG[min(MEASUREDOUTPUT$Time):max(MEASUREDOUTPUT$Time)]
  # Daily average protein content of the diet at days with protein deficiency (g CP per kg 
  # DM)
  ProtDefCP   <- c(na.omit(CPAVG1[PROTGRAPH[min(MEASUREDOUTPUT$Time):
                                              max(MEASUREDOUTPUT$Time),1]==0.5]))
  # Fat- and protein-corrected milk production at days with protein deficiency (kg FPCM per
  # day)
  ProtDefmilk <- c(na.omit(SIMULATEDFPCM[PROTGRAPH[min(MEASUREDOUTPUT$Time):
                                                     max(MEASUREDOUTPUT$Time),1]==0.5]))
  
  # Days where the available feed is below the amount of feed required (kg DM day-1) 
  NELIM <- FEEDQNTYTOT[1:imax[1]] - FEEDQNTY
  # If other factors (digestion capacity and protein deficiency) limit production, these
  # factors are considered to be the primary limitation, and not energy deficiency due to
  # limited availability of feed.
  NELIM <- NELIM[HEATSTRESS>0 || PROTGRAPH>0 || FILLGIT <=0.999] 
  NELIM[NELIM>0.00001] <- NA
  NELIM[NELIM<-0.00001] <- NA
  # Number of days with energy deficiency in an experiment (#)
  NRNELIM <- length(which(NELIM[min(MEASUREDOUTPUT$Time):
                                  max(MEASUREDOUTPUT$Time)] >-0.00001))
  # Days with energy deficiency (0 = no limitation; 1 = limitation)
  NEDEFfact <- NELIM[min(MEASUREDOUTPUT$Time):max(MEASUREDOUTPUT$Time)]
  NEDEFfact[is.na(NEDEFfact)] <-0
  
  # Number of days where the genotype defines production in an experiment (#)
  NRGENDEF <- length(c(min(MEASUREDOUTPUT$Time):max(MEASUREDOUTPUT$Time)))-NRHEATSTRESS-
    NRCOLDSTRESS-NRFILLGIT-NRNELIM-NRPROTGRAPH
  # Days where the genotype defines milk production (0 = not defining; 1 = defining)
  GENfact <- rep(1,length(min(MEASUREDOUTPUT$Time):max(MEASUREDOUTPUT$Time)))-
    HEATSTRESSfact-COLDSTRESSfact-FILLGITfact-PROTDEFfact-NEDEFfact
  
  # Experiments are extended to 1500 days with zeros, to make the length of the vector
  # the same for all cows.
  GENfact <- c(c(GENfact), c(rep(0,1500-length(GENfact))))
  HEATSTRESSfact <- c(c(HEATSTRESSfact), c(rep(0,1500-length(HEATSTRESSfact))))
  COLDSTRESSfact <- c(c(COLDSTRESSfact), c(rep(0,1500-length(COLDSTRESSfact))))
  FILLGITfact <- c(c(FILLGITfact), c(rep(0,1500-length(FILLGITfact))))
  NEDEFfact <- c(c(NEDEFfact), c(rep(0,1500-length(NEDEFfact))))
  PROTDEFfact <- c(c(PROTDEFfact), c(rep(0,1500-length(PROTDEFfact))))
  
  # Vector listing the number of days where a specific factor defines or limits milk 
  # production of a cow in an experiment (#)
  DEFLIMFACTORS <- c(NRGENDEF,NRHEATSTRESS,NRCOLDSTRESS,NRFILLGIT,NRNELIM,NRPROTGRAPH)
  # Vector listing the percentage of days where a specific factor defines or limits milk 
  # production in an experiment (%)
  relDEFLIMFACTORS <- DEFLIMFACTORS/(length(c(min(MEASUREDOUTPUT$Time):
                                                max(MEASUREDOUTPUT$Time))))*100
  
  # The lines below collect information for multiple cows in experiments
  
  # Defining and limiting factors for all cows in an experiment
  relDEFLIMFACTORScow <- rbind(relDEFLIMFACTORScow,relDEFLIMFACTORS)
  # Average daily maximum temperature during days at which heat stress occurs (degrees 
  # Celsius)
  TempHEATSTRESScow <- c(TempHEATSTRESScow,TempHEATSTRESS)
  # Average daily minimum temperature during days at which heat stress occurs (degrees 
  # Celsius)
  TempCOLDSTRESScow <- c(TempCOLDSTRESScow,TempCOLDSTRESS)
  # Daily average protein content of the diet at days with protein deficiency (g CP per kg 
  # DM) 
  ProtDefCPcow <- c(ProtDefCPcow,ProtDefCP)
  # Fat- and protein-corrected milk production at days with protein deficiency (kg FPCM per
  # day)
  ProtDefmilkcow <- c(ProtDefmilkcow,ProtDefmilk)
  
  # Days where the genotype defines milk production (0 = not defining; 1 = defining)
  GENfactcow        <- rbind(GENfactcow,GENfact)
  # Days where heat stress defines milk production (0 = not defining; 1 = defining)
  HEATSTRESSfactcow <- rbind(HEATSTRESSfactcow,HEATSTRESSfact)
  # Days where cold stress defines milk production (0 = not defining; 1 = defining)
  COLDSTRESSfactcow <- rbind(COLDSTRESSfactcow,COLDSTRESSfact)
  # Days where digestion capacity limits milk production (0 = not limiting; 1 = limiting)
  FILLGITfactcow    <- rbind(FILLGITfactcow,FILLGITfact)
  # Days where energy deficiency limits milk production (0 = not limiting; 1 = limiting)
  NEDEFfactcow      <- rbind(NEDEFfactcow,NEDEFfact)
  # Days where protein deficiency limits milk production (0 = not limiting; 1 = limiting)
  PROTDEFfactcow    <- rbind(PROTDEFfactcow,PROTDEFfact)
  
  # Number of cows in simulated so far (#) 
  Printcows <- Printcows + 1
  
  # The total number of cows simulated in the three experiments is 220. Printperc indicates
  # what percentage of the simulation is completed. 
  Printperc <- round(Printcows/220*100,digits=1)
  print(Printperc)
  
  # End of the z-loop for cows
  }  
  
  # Collection of the most important variables for the evaluation of LiGAPS-Dairy
  EVALUATIONCOWS <- EVALUATIONCOWS[2:nrow(EVALUATIONCOWS),]
  colnames(EVALUATIONCOWS) <- c("Cow nr", "Yp", "Yfl", "Ya", "YGp","YGfl",
                                "relYGp","relYGfl","DIFFlact","relDIFFlact",
                                "relMAEdailylact","Start kg","Diff. kg start", 
                                "Diff. avg kg","Diff kg end","Int.:Supp","relRMSEdailylact",
                                "bias%","slope%","random%","FI MAElact","FI relMAElact",
                                "FI reldailyMAElact","FI reldailyRMSElact","FI bias%",
                                "FI slope%","FI random%","Diff. FI (kg)","Measured FI (kg)",
                                "Simulated FI (kg)")
  # Average numbers for important variables per experiment
  EVALSCORES <- c(mean(EVALUATIONCOWS[,8]), mean(EVALUATIONCOWS[,11]), 
                  mean(na.omit(EVALUATIONCOWS[,15])),mean(EVALUATIONCOWS[,16]),
                  mean(EVALUATIONCOWS[,3]/EVALUATIONCOWS[,2]*100), mean(EVALUATIONCOWS[,17]),
                  mean(EVALUATIONCOWS[,18]),mean(EVALUATIONCOWS[,19]),
                  mean(EVALUATIONCOWS[,20]),mean(EVALUATIONCOWS[,10]),
                  mean(EVALUATIONCOWS[,21]),mean(EVALUATIONCOWS[,22]),
                  mean(EVALUATIONCOWS[,23]),mean(EVALUATIONCOWS[,24]),
                  mean(EVALUATIONCOWS[,25]),mean(EVALUATIONCOWS[,26]),
                  mean(EVALUATIONCOWS[,27]),mean(EVALUATIONCOWS[,28]))
  
  # Names of the variables in the vector EVALSCORES
  names(EVALSCORES) <- c("relYGfl","relMAEdailylact","Diff. kg end","Int.:Supp","S:P%",
                         "relRMSEdailylact","bias%","slope%","random%","relDIFFlact",
                         "FI MAElact","FI relMAElact","FI reldailyMAElact", 
                         "FI reldailyRMSElact","FI bias%","FI slope%","FI random%",
                         "Diff. FI (kg)")
  # Prints EVALSCORES per experiment
  print(EVALSCORES)
  # Collects the EVALSCORES for each experiment 
  EVALSCORESexps <- rbind(EVALSCORESexps,EVALSCORES)
  
  # Vector listing the percentage of days where a specific factor defines or limits milk 
  # production for all experiments (%)  
  relDEFLIMFACTORSexps <- rbind(relDEFLIMFACTORSexps,
                                relDEFLIMFACTORScow[2:nrow(relDEFLIMFACTORScow),])
  # Average daily maximum temperature during days at which heat stress occurs, includes all
  # experiments (degrees Celsius)  
  TempHEATSTRESSexps <- c(TempHEATSTRESSexps,TempHEATSTRESScow)
  # Average daily minimum temperature during days at which heat stress occurs, includes all
  # experiments (degrees Celsius) 
  TempCOLDSTRESSexps <- c(TempCOLDSTRESSexps,TempCOLDSTRESScow)
  # Daily average protein content of the diet at days with protein deficiency, includes all 
  # experiments (g CP per kg DM)
  ProtDefCPexps <- c(ProtDefCPexps,ProtDefCPcow)
  # Daily average milk production at days with protein deficiency, includes all 
  # experiments (kg FPCM per day)
  ProtDefmilkexps <- c(ProtDefmilkexps,ProtDefmilkcow)
  
  # Days where the genotype defines milk production (0 = not defining; 1 = defining)
  GENfactexps <- rbind(GENfactexps, GENfactcow[2:nrow(GENfactcow),])
  # Days where heat stress defines milk production (0 = not defining; 1 = defining)
  HEATSTRESSfactexps <- rbind(HEATSTRESSfactexps, 
                              HEATSTRESSfactcow[2:nrow(HEATSTRESSfactcow),])
  # Days where cold stress defines milk production (0 = not defining; 1 = defining)
  COLDSTRESSfactexps <- rbind(COLDSTRESSfactexps, 
                              COLDSTRESSfactcow[2:nrow(COLDSTRESSfactcow),])
  # Days where digestion capacity limits milk production (0 = not limiting; 1 = limiting)
  FILLGITfactexps <- rbind(FILLGITfactexps, FILLGITfactcow[2:nrow(FILLGITfactcow),])
  # Days where energy deficiency limits milk production (0 = not limiting; 1 = limiting)
  NEDEFfactexps <- rbind(NEDEFfactexps, NEDEFfactcow[2:nrow(NEDEFfactcow),])
  # Days where protein deficiency limits milk production (0 = not limiting; 1 = limiting)
  PROTDEFfactexps <- rbind(PROTDEFfactexps, PROTDEFfactcow[2:nrow(PROTDEFfactcow),])
  
  # Data per experiment for all cows
  EVALUATIONALL <- EVALUATIONALL[2:nrow(EVALUATIONALL),]
  EVALUATIONFIALL <- EVALUATIONFIALL[2:nrow(EVALUATIONFIALL),]
  EVALUATIONDAILYALL <- EVALUATIONDAILYALL[2:nrow(EVALUATIONDAILYALL),]
  EVALUATIONDAILYFIALL <- EVALUATIONDAILYFIALL[2:nrow(EVALUATIONDAILYFIALL),] 
  
  # Collect data for all (two) experiments in the model evaluation
  EVALUATIONALLexp <- rbind(EVALUATIONALLexp,EVALUATIONALL)
  EVALUATIONFIALLexp <- rbind(EVALUATIONFIALLexp,EVALUATIONFIALL) 
  EVALUATIONDAILYALLexp <- rbind(EVALUATIONDAILYALLexp,EVALUATIONDAILYALL) 
  EVALUATIONDAILYFIALLexp <- rbind(EVALUATIONDAILYFIALLexp,EVALUATIONDAILYFIALL)
  
  
  # Data collection for a graph on measured and simulated FPCM production for all (two) 
  # experiments
  
  if(e==1) DATALACT <- cbind(EVALUATIONCOWS[,4],EVALUATIONCOWS[,3]) 
  if(c==3 && e==2) DATALACT <- cbind(EVALUATIONCOWS[,4],EVALUATIONCOWS[,3]) 
  
  if(c==1 && e==3) FIG2AA <- cbind(EVALUATIONCOWS[,29],EVALUATIONCOWS[,30],
                                   EVALUATIONCOWS[,28])  
  if(c==2 && e==2) FIG2BA <- cbind(EVALUATIONCOWS[,29],EVALUATIONCOWS[,30],
                                   EVALUATIONCOWS[,28])  
  if(c==3 && e==1) FIG2CB <- cbind(EVALUATIONCOWS[,29],EVALUATIONCOWS[,30],
                                   EVALUATIONCOWS[,28])
  
  # End of the e-loop for two experiments (used for evaluation)
  } 
  
  # Collect data for all (two) experiments
  EVALUATIONALLexp <- EVALUATIONALLexp[2:nrow(EVALUATIONALLexp),]
  EVALUATIONFIALLexp <- EVALUATIONFIALLexp[2:nrow(EVALUATIONFIALLexp),]
  EVALUATIONDAILYALLexp <- EVALUATIONDAILYALLexp[2:nrow(EVALUATIONDAILYALLexp),] 
  EVALUATIONDAILYFIALLexp <- EVALUATIONDAILYFIALLexp[2:nrow(EVALUATIONDAILYFIALLexp),]   
  
  # Columns 2 and 4 of Table 3 in the paper describing LiGAPS-Dairy
  TABLE3COL2 <- colMeans(EVALUATIONDAILYALLexp)
  TABLE3COL4 <- colMeans(EVALUATIONDAILYFIALLexp)
  
  # Analysis of the data per lactation
  
  # Milk production per lactation
  
  # Mean Absolute Error for milk production per lactation (kg FPCM per lactation)
  MAEmilk <- mean(abs(EVALUATIONALLexp[,1]-EVALUATIONALLexp[,2]))
  # Relative Mean Absolute Error for milk production per lactation (%)
  relMAEmilk <- MAEmilk/mean(EVALUATIONALLexp[,1])*100
  # Root Mean Squared Error for milk production per lactation (kg FPCM per lactation)
  RMSEmilk <- (sum((EVALUATIONALLexp[,2]-EVALUATIONALLexp[,1])^2)/
                 length(EVALUATIONALLexp[,1]))^0.5 
  # Relative Root Mean Squared Error for milk production per lactation (%)
  relRMSEmilk <- RMSEmilk/mean(EVALUATIONALLexp[,1])*100
  
  # Approach of Theil (1966) and Bibby and Toutenburg (1977) to split up the MSE or RMSE in 
  # three components: bias, slope, and random. More information on this method can be found
  # in McPhee and Walmsley (2017).
  
  # Linear model for simulated and measured milk production (kg FPCM per lactation) 
  stats<-summary(lm(EVALUATIONALLexp[,1]~EVALUATIONALLexp[,2]))
  # Mean Squared Prediction Error (kg2 FPCM lactation-2)
  MSPE <- sum((EVALUATIONALLexp[,2]-EVALUATIONALLexp[,1])^2)/length(EVALUATIONALLexp[,2])
  # Bias component of MSPE (kg2 FPCM lactation-2)
  BIAS <- (mean(EVALUATIONALLexp[,2])-mean(EVALUATIONALLexp[,1]))^2
  # relative bias as % of MSPE
  biasmilk <- BIAS/MSPE*100 
  # Slope component of MSPE (kg2 FPCM lactation-2)
  SLOPE2 <- sum((EVALUATIONALLexp[,2]-mean(EVALUATIONALLexp[,2]))^2)/
    length(EVALUATIONALLexp[,2])*(1-stats$coefficients[2,1])^2
  # Relative slope as % of MSPE
  slopemilk <- SLOPE2/MSPE*100 
  # Random component of MSPE (kg2 FPCM lactation-2)
  RANDOM <- (1-stats$r.squared)*sum((EVALUATIONALLexp[,1]-mean(EVALUATIONALLexp[,1]))^2)/
    length(EVALUATIONALLexp[,1])
  # Relative random component as % of MSPE
  randommilk <- RANDOM/MSPE*100 
  
  # Information presented in the first column of Table 3 in the paper describing 
  # LiGAPS-Dairy
  TABLE3COL1 <- c(mean(EVALUATIONALLexp[,1]),mean(EVALUATIONALLexp[,2]),
                  mean(EVALUATIONALLexp[,2])-mean(EVALUATIONALLexp[,1]),
                  (mean(EVALUATIONALLexp[,2])-mean(EVALUATIONALLexp[,1]))/mean(EVALUATIONALLexp[,1])*100,
                  MAEmilk,relMAEmilk,RMSEmilk,relRMSEmilk,
                  biasmilk,slopemilk,randommilk,
                  stats$coefficients[1,4],stats$coefficients[2,4],
                  stats$coefficients[1,1],stats$coefficients[2,1])
  
  #print(nrow(EVALUATIONALLexp))
  
  # Feed intake per lactation
  
  # Mean Absolute Error feed intake 
  MAEfeed <- mean(abs(EVALUATIONFIALLexp[,1]-EVALUATIONFIALLexp[,2]))
  # Relative Mean Absolute Error feed intake (%)
  relMAEfeed <- MAEfeed/mean(EVALUATIONFIALLexp[,1])*100
  # Root Mean Squared Error feed intake (kg DM per lactation) 
  RMSEfeed <- (sum((EVALUATIONFIALLexp[,2]-EVALUATIONFIALLexp[,1])^2)/
                 length(EVALUATIONFIALLexp[,1]))^0.5 
  # Relative Root Mean Squared Error feed intake (%)
  relRMSEfeed <- RMSEfeed/mean(EVALUATIONFIALLexp[,1])*100
  
  # Approach of Theil (1966) and Bibby and Toutenburg (1977) to split up the MSE or RMSE in 
  # three components: bias, slope, and random. More information on this method can be found
  # in McPhee and Walmsley (2017).
  
  # Linear model for simulated and measured feed intake (kg DM per lactation)
  stats<-summary(lm(EVALUATIONFIALLexp[,1]~EVALUATIONFIALLexp[,2]))
  # Mean Squared Prediction Error (kg2 DM lactation-2)
  MSPE <- sum((EVALUATIONFIALLexp[,2]-EVALUATIONFIALLexp[,1])^2)/
    length(EVALUATIONFIALLexp[,2])
  # Bias component of MSPE (kg2 DM lactation-2)
  BIAS <- (mean(EVALUATIONFIALLexp[,2])-mean(EVALUATIONFIALLexp[,1]))^2
  # Relative bias as % of MSPE
  biasfeed <- BIAS/MSPE*100 
  # Slope component of MSPE (kg2 DM lactation-2)
  SLOPE2 <- sum((EVALUATIONFIALLexp[,2]-mean(EVALUATIONFIALLexp[,2]))^2)/
    length(EVALUATIONFIALLexp[,2])*(1-stats$coefficients[2,1])^2
  # relative slope as % of MSPE
  slopefeed <- SLOPE2/MSPE*100 
  # Random component of MSPE (kg2 DM lactation-2)
  RANDOM <- (1-stats$r.squared)*sum((EVALUATIONFIALLexp[,1]-
             mean(EVALUATIONFIALLexp[,1]))^2)/length(EVALUATIONFIALLexp[,1])
  # Relative random component as % of MSPE
  randomfeed <- RANDOM/MSPE*100 
  
  # Third column of Table 3 in the paper describing LiGAPS-Dairy
  TABLE3COL3 <- c(mean(EVALUATIONFIALLexp[,1]),mean(EVALUATIONFIALLexp[,2]),
                  mean(EVALUATIONFIALLexp[,2])-mean(EVALUATIONFIALLexp[,1]),
                  (mean(EVALUATIONFIALLexp[,2])-mean(EVALUATIONFIALLexp[,1]))/mean(EVALUATIONFIALLexp[,1])*100,
                  MAEfeed,relMAEfeed,RMSEfeed,relRMSEfeed,
                  biasfeed,slopefeed,randomfeed,
                  stats$coefficients[1,4],stats$coefficients[2,4],
                  stats$coefficients[1,1],stats$coefficients[2,1])
  
  # Columns 1-4 of Table 3 in the paper describing LiGAPS-Dairy
  TABLE3 <- cbind(TABLE3COL1,TABLE3COL2,TABLE3COL3,TABLE3COL4)
  # Columns 1-4 of Table 3 are obtained for the three combinations of two experiments
  if(c==1) TABLE3_AB <- TABLE3
  if(c==2) TABLE3_AC <- TABLE3
  if(c==3) TABLE3_BC <- TABLE3
  
  # Collection of variables for all experiments
  
  # EVALSCORES for each experiment 
  EVALSCORESexps <- EVALSCORESexps[2:nrow(EVALSCORESexps),]
  # Vector listing the percentage of days where a specific factor defines or limits milk 
  # production for all experiments (%) 
  relDEFLIMFACTORSexps <- relDEFLIMFACTORSexps[2:nrow(relDEFLIMFACTORSexps),]
  relDEFLIMFACTORSexpsall <- rbind(relDEFLIMFACTORSexpsall,relDEFLIMFACTORSexps)
  # Average daily maximum temperature during days at which heat stress occurs, includes all
  # experiments (degrees Celsius)
  TempHEATSTRESSexpsall <- c(TempHEATSTRESSexpsall,TempHEATSTRESSexps)
  # Average daily minimum temperature during days at which cold stress occurs, includes all
  # experiments (degrees Celsius)
  TempCOLDSTRESSexpsall <- c(TempCOLDSTRESSexpsall,TempCOLDSTRESSexps)
  # Daily average protein content of the diet at days with protein deficiency, includes all 
  # experiments (g CP per kg DM)
  ProtDefCPexpsall <- c(ProtDefCPexpsall,ProtDefCPexps)
  # Daily average milk production at days with protein deficiency, includes all 
  # experiments (kg FPCM per day)
  ProtDefmilkexpsall <- c(ProtDefmilkexpsall,ProtDefmilkexps)
  
  # Days where the genotype defines milk production (0 = not defining; 1 = defining)
  GENfactall <- rbind(GENfactall, GENfactexps[2:nrow(GENfactexps),])
  # Days where heat stress defines milk production (0 = not defining; 1 = defining)
  HEATSTRESSfactall <- rbind(HEATSTRESSfactall, 
                             HEATSTRESSfactexps[2:nrow(HEATSTRESSfactexps),])
  # Days where cold stress defines milk production (0 = not defining; 1 = defining)
  COLDSTRESSfactall <- rbind(COLDSTRESSfactall, 
                             COLDSTRESSfactexps[2:nrow(COLDSTRESSfactexps),])
  # Days where digestion capacity limits milk production (0 = not limiting; 1 = limiting)
  FILLGITfactall <- rbind(FILLGITfactall, FILLGITfactexps[2:nrow(FILLGITfactexps),])
  # Days where energy deficiency limits milk production (0 = not limiting; 1 = limiting)
  NEDEFfactall <- rbind(NEDEFfactall, NEDEFfactexps[2:nrow(NEDEFfactexps),])
  # Days where protein deficiency limits milk production (0 = not limiting; 1 = limiting)
  PROTDEFfactall <- rbind(PROTDEFfactall, PROTDEFfactexps[2:nrow(PROTDEFfactexps),])
                      
  #Numbers of cows in experiments
  
  # Combination of experiment at 't Gen and the experiment of Meijer et al. (1998)
  if(c==1) nrcows1 <- 77 # Number of cattle at 't Gen
  if(c==1) nrcows2 <- 76 # Number of cattle in the experiment of Meijer et al. (1998)
  # Combination of experiment at 't Gen and the experiment of van Duinkerken et al. (2005)
  if(c==2) nrcows1 <- 77 # Number of cattle at 't Gen
  if(c==2) nrcows2 <- 67 # Number of cattle in the experiment of van Duinkerken et al. (2005)
  # Combination of experiments of Meijer et al. (1998) and van Duinkerken et al. (2005)
  if(c==3) nrcows1 <- 76 # Number of cattle in the experiment of Meijer et al. (1998)
  if(c==3) nrcows2 <- 67 # Number of cattle in the experiment of van Duinkerken et al. (2005)
  
  # Collection of important variables for all experiments
  EVALSCORESexpsfinal <- EVALSCORESexps
  # Names of variables in EVALSCORESexps
  names(EVALSCORESexpsfinal) <- c("relYGfl","relMAEdailylact","Diff. kg end","Int.:Supp",
                                  "S:P%","relRMSEdailylact","bias%","slope%","random%",
                                  "relDIFFlact","FI MAElact","FI relMAElact",
                                  "FI reldailyMAElact", "FI reldailyRMSElact","FI bias%",
                                  "FI slope%","FI random%","Diff. FI (kg)")
  
  # Collect the measured and simulated FPCM production for all combinations of experiments
  TOTALLACT <- rbind(DATALACT,cbind(EVALUATIONCOWS[,4],EVALUATIONCOWS[,3]))
  
  # Approach of Theil (1966) and Bibby and Toutenburg (1977) to split up the MSE or RMSE in 
  # three components: bias, slope, and random. More information on this method can be found
  # in McPhee and Walmsley (2017).
  
  # Linear model for simulated and measured FPCM production (kg FPCM per lactation)
  stats<-summary(lm(TOTALLACT[,1]~TOTALLACT[,2]))
  # Mean Squared Prediction Error (kg2 FPCM lactation-2)
  MSPE <- sum((TOTALLACT[,1]-TOTALLACT[,2])^2)/length(TOTALLACT[,1])
  # Bias component of MSPE (kg2 FPCM lactation-2)
  BIAS <- (mean(TOTALLACT[,1])-mean(TOTALLACT[,2]))^2
  # relative bias as % of MSPE
  biasrellact <- BIAS/MSPE*100 
  # Slope component of MSPE (kg2 FPCM lactation-2)
  SLOPE2 <- sum((TOTALLACT[,2]-mean(TOTALLACT[,2]))^2)/length(TOTALLACT[,2])*
    (1-stats$coefficients[2,1])^2
  # relative slope as % of MSPE
  sloperellact <- SLOPE2/MSPE*100
  # Random component of MSPE (kg2 FPCM lactation-2)
  RANDOM <- (1-stats$r.squared)*sum((TOTALLACT[,1]-mean(TOTALLACT[,1]))^2)/
    length(TOTALLACT[,1])
  # relative random component as % of MSPE
  randomrellact <- RANDOM/MSPE*100 
  
  # Average bias simulated and measured FPCM production (kg FPCM per lactation)
  BIASabs <- mean(TOTALLACT[,2])-mean(TOTALLACT[,1])
  # Relative average bias simulated and measured FPCM production (kg FPCM per lactation). 
  # The benchmark used is the measured FPCM production.
  relBIAS <- BIASabs/mean(mean(TOTALLACT[,1]))*100
  # Root Mean Squared Error for simulated and measured FPCM production (kg FPCM per 
  # lactation)
  RMSE <- (sum((TOTALLACT[,2]-TOTALLACT[,1])^2)/length(TOTALLACT[,1]))^0.5
  # Relative Root Mean Squared Error for simulated and measured FPCM production (kg FPCM per
  # lactation). The benchmark used is the measured FPCM production.
  relRMSE <- RMSE/mean(TOTALLACT[,1])*100 # % actual production 
  # Mean Absolute Error simulated and measured FPCM production (kg FPCM per lactation) 
  MAE     <- mean(abs(TOTALLACT[,2]-TOTALLACT[,1]))
  # Relative Mean Absolute Error simulated and measured FPCM production (%). The benchmark 
  # used is the measured FPCM production.
  relMAE  <- mean(abs(TOTALLACT[,2]-TOTALLACT[,1]))/mean(TOTALLACT[,1])*100 
  
  # Collection of performance indicators for the evaluation of FPCM production
  CALDATA <- c(mean(TOTALLACT[,1]),mean(TOTALLACT[,2]),BIASabs,relBIAS,
               MAE,relMAE, RMSE,relRMSE,
               biasrellact,sloperellact,randomrellact,
               stats$coefficients[1,4],stats$coefficients[2,4],
               stats$coefficients[1,1],stats$coefficients[2,1],NA,
               EVALSCORESexpsfinal[6],EVALSCORESexpsfinal[7],EVALSCORESexpsfinal[8],
               EVALSCORESexpsfinal[9],EVALSCORESexpsfinal[2])
  
  # The performance indicators for a combination of two experiments are written to different
  # vectors (CALDATAAB,CALDATAAC,CALDATABC)
  
  # Combination of experiment at 't Gen and the experiment of Meijer et al. (1998)
  if(c==1) CALDATAAB <- CALDATA
  # Combination of experiment at 't Gen and the experiment of van Duinkerken et al. (2005)
  if(c==2) CALDATAAC <- CALDATA
  # Combination of experiments of Meijer et al. (1998) and van Duinkerken et al. (2005)
  if(c==3) CALDATABC <- CALDATA
  
  # End of the c-loop for the three combinations of two experiments.
  } 
  
  ###########################################################################################
  
  # The matrix CALDATATOTAL presents key results for the three combinations of two 
  # experiments
  CALDATATOTAL <- cbind(CALDATAAB,CALDATAAC,CALDATABC)
  # Names of the variables presented in CALDATATOTAL
  rownames(CALDATATOTAL) <- c("Actual_production", "Feed_quality_limited_production",
                              "Difference","relativeDifference","MAE","relMAElact",
                              "RMSElact","relRMSElact",
                              "biaslact%","slopelact%","randomlact%",
                              "P Intercept","P Slope",
                              "Intercept","Slope",NA,
                              "relRMSEdailylact","biasdaily%","slopedaily%",
                              "randomdaily%","relMAEdailylact")
  # After the third combination of two experiments, the results for all experiments are 
  # listed
  if(c==3) clusterTABLE3 <- list(TABLE3_AB,TABLE3_AC,TABLE3_BC)
  
  # The averages of the key results for the three combinations of two experiments are 
  # calculated
  if(c==3) MEAN_TABLE3 <- round(apply(array(unlist(clusterTABLE3), c(15, 4, 3)), 
                                      c(1,2), mean),3)
  # The standard error of the key results for the three combinations of two experiments are 
  # calculated. The standard error is calculated from the standard deviation of the 
  # variables in the three combinations, divided by the squared root of three.
  if(c==3) SE_TABLE3 <- round((apply(array(unlist(clusterTABLE3), c(15, 4, 3)), 
                                     c(1,2), sd))/sqrt(3),3)
  
  ###########################################################################################
  #                                        Table 3                                          #
  ###########################################################################################
  
  # Table 3 in the paper describing LiGAPS-Dairy
  TABLE3full <- cbind(MEAN_TABLE3[,1],SE_TABLE3[,1],
                      MEAN_TABLE3[,2],SE_TABLE3[,2],
                      MEAN_TABLE3[,3],SE_TABLE3[,3],
                      MEAN_TABLE3[,4],SE_TABLE3[,4])
  
  # Column names of Table 3 in the paper describing LiGAPS-Dairy
  colnames(TABLE3full) <- c("Milk yield per lactation (mean)",
                            "Milk yield per lactation (SE)",
                            "Daily milk yield (mean)",
                            "Daily milk yield (SE)",
                            "Feed intake per lactation (mean)",
                            "Feed intake per lactation (SE)",
                            "Daily feed intake (mean)",
                            "Daily feed intake (SE)")
  
  # Row names of Table 3 in the paper describing LiGAPS-Dairy
  rownames(TABLE3full) <- c("Average measured","Average simulated","Average difference",
                            "Relative difference","MAE","relative MAE","RMSE", 
                            "relative RMSE","Bias %","Slope %", "Random %",
                            "Intercept P-value", "Slope P-value","Intercept","Slope")
  
  
  # The lines below use the library 'writexl' to write the results for Table 3 and for
  # individual combinations of two experiments to a directory. Make sure library 'writexl' 
  # is available under the installed R-packages.
  library(writexl)
  # Directory of the output file
  outputfile <- paste("M:/My Documents/PostDoc Investing in Sustainable Livestock/LiGAPS_Dairy/Model evaluation/Evaluation.xlsx")
  # Table 3 is written as an Excel file
  write_xlsx(data.frame(TABLE3full),path = outputfile, col_names = T)
  
  # Tables for results of individual combinations of two experiments 
  # Row names
  rownames(TABLE3_AB) <- rownames(TABLE3full)
  rownames(TABLE3_AC) <- rownames(TABLE3full)
  rownames(TABLE3_BC) <- rownames(TABLE3full)
  # Column names 
  colnames(TABLE3_AB) <- c("Milk yield per experiment","Daily milk yield",
                           "Feed intake per experiment","Daily feed intake")
  colnames(TABLE3_AC) <- c("Milk yield per experiment","Daily milk yield",
                           "Feed intake per experiment","Daily feed intake")
  colnames(TABLE3_BC) <- c("Milk yield per experiment","Daily milk yield",
                           "Feed intake per experiment","Daily feed intake")
  
  # Three directories are specified for the three combinations 
  outputfileAB <- paste("M:/My Documents/PostDoc Investing in Sustainable Livestock/LiGAPS_Dairy/Model evaluation/Evaluation_AB.xlsx")
  outputfileAC <- paste("M:/My Documents/PostDoc Investing in Sustainable Livestock/LiGAPS_Dairy/Model evaluation/Evaluation_AC.xlsx")
  outputfileBC <- paste("M:/My Documents/PostDoc Investing in Sustainable Livestock/LiGAPS_Dairy/Model evaluation/Evaluation_BC.xlsx")
  # Three files are written for the three combinations 
  write_xlsx(data.frame(TABLE3_AB), outputfileAB, col_names = T)
  write_xlsx(data.frame(TABLE3_AC), outputfileAC, col_names = T)
  write_xlsx(data.frame(TABLE3_BC), outputfileBC, col_names = T)
  
  
  #######################################################################################
  #                                Figures model evaluation                             #
  #######################################################################################
  
  # Figure 1 on model calibration is created via the R-file 'LiGAPSDairy20200302.R'
  # Figure 4 in the paper is created via the R-file 'FigureYGA_20200304.R' 
  
  #######################################################################################
  #                                       Figure 2                                      #
  #######################################################################################
  
  # Upper part of the figure
  
  a<-0     # Minimum value for FPCM production
  b<-20000 # Maximum value for FPCM production
  
  tiff("M:/Figure2_ev.tiff", width = 5.0, height = 8.0, units = 'in', res = 150)
  
  par(mfrow=c(2,1))
  layout(matrix(c(1,2), 2, 1, byrow = TRUE), 
         widths=c(1,1), heights=c(1,1))
  
  par(mar = c(0,5,2,2))
  
  plot(FIG2AA[,1]~FIG2AA[,2], xlim=c(0,20000),ylim=c(0,20000),
       las=1, xlab= expression(paste("Simulated milk yield (kg FPCM per lactation)")), 
       ylab=NA, pch=22, bg="grey67",
       yaxs="i", xaxs="i",xaxt="n")
  lines(c(0,20000)~c(a,b), lty = "dashed", lwd = 2)
  points(FIG2BA[,1]~FIG2BA[,2], pch = 23, bg="grey33")
  points(FIG2CB[,1]~FIG2CB[,2], pch = 19)
  legend("topleft",legend = c("'t Gen","Meijer et al. (1998)",
                              "Van Duinkerken et al. (2005)"), pch= c(19,23,22),
         pt.bg=c(NA,"grey33","grey67"),bty="n", cex = 0.9)
  mtext(side = 2, text = "Measured milk yield (kg FPCM per lactation)", line = 4, 
        cex =1.00)
  
  # Parameters regression lines are obtained with the function lm() -> linear model
  stats1<-summary(lm(FIG2AA[,1]~FIG2AA[,2])) 
  stats2<-summary(lm(FIG2BA[,1]~FIG2BA[,2]))
  stats3<-summary(lm(FIG2CB[,1]~FIG2CB[,2]))
  
  # Regression lines are plotted
  lines(c(stats1$coefficients[1,1],stats1$coefficients[1,1]+
            20000*stats1$coefficients[2,1])~
        c(0,20000), col="grey67", lwd=1.5,lty="solid")
  lines(c(stats2$coefficients[1,1],stats2$coefficients[1,1]+
            20000*stats2$coefficients[2,1])~
          c(0,20000), col="grey33", lwd=1.5,lty="solid")
  lines(c(stats3$coefficients[1,1],stats3$coefficients[1,1]+
            20000*stats3$coefficients[2,1])~
          c(0,20000), col="black", lwd=1.5,lty="solid")
  
  # Regression equations are plotted
  text(7000,2000,labels =(bquote("y ="~.(format(stats1$coefficients[2,1],digits = 3))~"x -"~.(format(abs(stats1$coefficients[1,1]),digits = 2)))),col = "grey67", cex = 0.8)
  text(9000,15000,labels =(bquote("y ="~.(format(stats2$coefficients[2,1],digits = 3))~"x +"~.(format(stats2$coefficients[1,1],digits = 2)))),col = "grey33", cex = 0.8)
  text(16000,10000,labels =(bquote("y ="~.(format(stats3$coefficients[2,1],digits = 2))~"x +"~.(format(stats3$coefficients[1,1],digits = 2)))),col = "black", cex = 0.8)
  
  # lower part of the figure
  par(mar = c(5,5,0,2))  
  plot(c(0,0)~c(a,b), type= "l", lty="dashed", las=1, lwd=2.0,
       xlab= expression(paste("Simulated milk yield (kg FPCM per lactation)")),
       ylab= NA,
       xlim = c(a,b), ylim = c(-4800,4800), xaxs = "i", yaxs="i", cex.axis=1)
  points(-FIG2AA[,3]~FIG2AA[,2], pch = 22, bg="grey67")
  points(-FIG2BA[,3]~FIG2BA[,2], pch = 23, bg="grey33")
  points(-FIG2CB[,3]~FIG2CB[,2], pch = 19)
  
  # Parameters regression lines are obtained with the function lm() -> linear model
  stats1<-summary(lm(-FIG2AA[,3]~FIG2AA[,2]))
  stats2<-summary(lm(-FIG2BA[,3]~FIG2BA[,2]))
  stats3<-summary(lm(-FIG2CB[,3]~FIG2CB[,2]))
  
  # Regression lines are plotted
  lines(c(stats1$coefficients[1,1],stats1$coefficients[1,1]+20000*stats1$coefficients[2,1])~
          c(0,20000), col="grey67", lwd=1.5,lty="solid")
  lines(c(stats2$coefficients[1,1],stats2$coefficients[1,1]+20000*stats2$coefficients[2,1])~
          c(0,20000), col="grey33", lwd=1.5,lty="solid")
  lines(c(stats3$coefficients[1,1],stats3$coefficients[1,1]+20000*stats3$coefficients[2,1])~
          c(0,20000), col="black", lwd=1.5,lty="solid")
  
  mtext(side = 2, text = "Residuals (kg FPCM per lactation)", line = 4, cex =1.00)
  
  dev.off()
  closeAllConnections()
  
  #######################################################################################
  #                                       Figure 3                                      #
  #######################################################################################
  
  tiff("M:/Figure3_ev.tiff", width = 5.0, height = 8.0, units = 'in', res = 150)
  
  par(mfrow=c(2,1))
  layout(matrix(c(1,2), 2, 1, byrow = TRUE), 
         widths=c(1,1), heights=c(1,1))
  
  par(mar = c(0,5,2,2))
  
  # Upper part of the figure
  plot(FIG2AA[,1]~FIG2AA[,2], xlim=c(0,14000),ylim=c(0,14000),
       las=1, xlab= NA, 
       ylab=NA, pch=22, bg="grey67",
       yaxs="i", xaxs="i",xaxt="n")
  b <-14000
  lines(c(0,14000)~c(a,b), lty = "dashed", lwd = 2)
  points(FIG2BA[,1]~FIG2BA[,2], pch = 23, bg="grey33")
  points(FIG2CB[,1]~FIG2CB[,2], pch = 19)
  legend("topleft",legend = c("'t Gen","Meijer et al. (1998)","Van Duinkerken et al. (2005)"), pch= c(19,23,22),
         pt.bg=c(NA,"grey33","grey67"),bty="n", cex = 0.9)
  mtext(side = 2, text = "Actual feed intake (kg DM per lactation)", line = 4, cex =1.00)
  
  # Parameters regression lines are obtained with the function lm() -> linear model
  stats1<-summary(lm(FIG2AA[,1]~FIG2AA[,2]))
  stats2<-summary(lm(FIG2BA[,1]~FIG2BA[,2]))
  stats3<-summary(lm(FIG2CB[,1]~FIG2CB[,2]))
  
  # Regression lines are plotted
  lines(c(stats1$coefficients[1,1],stats1$coefficients[1,1]+14000*stats1$coefficients[2,1])~
          c(0,14000), col="grey67", lwd=1.5,lty="solid")
  lines(c(stats2$coefficients[1,1],stats2$coefficients[1,1]+14000*stats2$coefficients[2,1])~
          c(0,14000), col="grey33", lwd=1.5,lty="solid")
  lines(c(stats3$coefficients[1,1],stats3$coefficients[1,1]+14000*stats3$coefficients[2,1])~
          c(0,14000), col="black", lwd=1.5,lty="solid")
  
  # Regression equations are plotted
  text(5500,2000,labels =(bquote("y ="~.(format(stats1$coefficients[2,1],digits = 3))~"x -"~.(format(abs(stats1$coefficients[1,1]),digits = 2)))),col = "grey67", cex = 0.8)
  text(2500,5500,labels =(bquote("y ="~.(format(stats2$coefficients[2,1],digits = 3))~"x +"~.(format(stats2$coefficients[1,1],digits = 2)))),col = "grey33", cex = 0.8)
  text(4000,1000,labels =(bquote("y ="~.(format(stats3$coefficients[2,1],digits = 2))~"x -"~.(format(abs(stats3$coefficients[1,1]),digits = 2)))),col = "black", cex = 0.8)
  
  # Lower part of the figure
  par(mar = c(5,5,0,2))  
  plot(c(0,0)~c(a,b), type= "l", lty="dashed", las=1, lwd=2.0,
       xlab= expression(paste("Simulated feed intake (kg DM per lactation)")),
       ylab= NA,
       xlim = c(a,b), ylim = c(-3900,3900), xaxs = "i", yaxs="i", cex.axis=1)
  mtext(side = 2, text = "Residuals (kg FPCM per lactation)", line = 4, cex =1.00)
  points(-FIG2AA[,3]~FIG2AA[,2], pch = 22, bg="grey67")
  points(-FIG2BA[,3]~FIG2BA[,2], pch = 23, bg="grey33")
  points(-FIG2CB[,3]~FIG2CB[,2], pch = 19)
  
  # Parameters regression lines are obtained with the function lm() -> linear model
  stats1<-summary(lm(-FIG2AA[,3]~FIG2AA[,2]))
  stats2<-summary(lm(-FIG2BA[,3]~FIG2BA[,2]))
  stats3<-summary(lm(-FIG2CB[,3]~FIG2CB[,2]))
  
  # Regression lines are plotted
  lines(c(stats1$coefficients[1,1],stats1$coefficients[1,1]+20000*stats1$coefficients[2,1])~
          c(0,20000), col="grey67", lwd=1.5,lty="solid")
  lines(c(stats2$coefficients[1,1],stats2$coefficients[1,1]+20000*stats2$coefficients[2,1])~
          c(0,20000), col="grey33", lwd=1.5,lty="solid")
  lines(c(stats3$coefficients[1,1],stats3$coefficients[1,1]+20000*stats3$coefficients[2,1])~
          c(0,20000), col="black", lwd=1.5,lty="solid")
  
  dev.off()
  closeAllConnections()
  
  #######################################################################################
  #                                       Figure 5                                      #
  #######################################################################################
  
  # This figure shows the defining and limiting factors of the 220 cows in their first
  # 210 days in lactation in the experiments. The occurrence of these factors is 
  # expressed as a percentage (1 cow = 100/220 = 0.45%).
  
  # Each day is represented by a bar (100%). The bar can consist of the factors genotype
  # heat stress, cold stress, digestion capacity, and protein deficiency. Energy 
  # deficiency due to limited feed availability (NEDEFfactall) does not occur, because
  # feed is fed ad libitum in all experiments.
  
  relDEFLIMFACTORSexpsall <- relDEFLIMFACTORSexpsall[2:nrow(relDEFLIMFACTORSexpsall),]
  
  GENfactall        <- GENfactall[2:nrow(GENfactall),]
  HEATSTRESSfactall <- HEATSTRESSfactall[2:nrow(HEATSTRESSfactall),]
  COLDSTRESSfactall <- COLDSTRESSfactall[2:nrow(COLDSTRESSfactall),]
  FILLGITfactall    <- FILLGITfactall[2:nrow(FILLGITfactall),]
  NEDEFfactall      <- NEDEFfactall[2:nrow(NEDEFfactall),]
  PROTDEFfactall    <- PROTDEFfactall[2:nrow(PROTDEFfactall),]
  
  BARS <- matrix(nrow=6,ncol=210, data=NA)
  BARS[6,] <- colMeans(GENfactall[,1:210]*100)
  BARS[5,] <- colMeans(HEATSTRESSfactall[,1:210]*100)
  BARS[4,] <- colMeans(COLDSTRESSfactall[,1:210]*100)
  BARS[3,] <- colMeans(FILLGITfactall[,1:210]*100)
  BARS[2,] <- colMeans(NEDEFfactall[,1:210]*100)
  BARS[1,] <- colMeans(PROTDEFfactall[,1:210]*100)
  
  # A few missing data: factors assumed to be proportional.
  factorperc <- 100/colSums(BARS)
  factorperc <- rbind(factorperc,factorperc,factorperc,factorperc,factorperc,factorperc)
  
  BARS <- BARS * factorperc
  
  # Average potential, feed quality limited, and actual milk production in the first 210 days of
  # the lactation for the 220 cows, in kg FPCM per cow per day. 
  PotSEQ <- PotSEQ[2:nrow(PotSEQ),]
  FeedlimSEQ <- FeedlimSEQ[2:nrow(FeedlimSEQ),]
  ActualSEQ <- ActualSEQ[2:nrow(ActualSEQ),]
  
  meanPotSEQ <- colMeans(na.omit(PotSEQ))
  meanFeedlimSEQ <- colMeans(na.omit(FeedlimSEQ))
  meanActualSEQ <- colMeans(na.omit(ActualSEQ))
  
  # Start plot figure 5
  tiff("M:/Figure5_ev.tiff", width = 8.0, height = 9.0, units = 'in', res = 150)
  layout(matrix(c(1,2),ncol=1), heights=c(1.4,2))
  
  # Upper part of the figure (A)
  par(mar=c(1,6,1,11))
  plot(0~0, xaxs="i", xaxt="n", pch = 19, col="white", 
       las=1,yaxs="i",
       ylab=expression(paste("Milk yield (kg FPCM day"^"-1"*")")),
       xlim=c(0,210), xlab=NA, ylim=c(20,45))
  
  # Potential, feed quality limited, and actual FPCM production
  lines(meanPotSEQ~c(0:210), lwd=1.4, lty="solid", col="black")
  lines(meanFeedlimSEQ~c(0:210), lwd=1.4, lty="dashed")
  lines(meanActualSEQ~c(1:210), lwd=1.4, lty="dotted")
  
  par(new=T, xpd=TRUE)
  legend("topright",inset=c(-0.45,0), 
         legend=c("Genetic potential",expression(paste("Y"[L])),expression(paste("Y"[A]))), 
         lty=c("solid","dashed","dotted"),lwd=1.4, col="black", 
         cex=1, bty = "n")
  
  axis(1, at=seq(0,210, by=50), labels=F, tick = T)
  text(-40,45,"A",xpd=T, cex = 1.2)
  
  # Lower part of the figure (B)
  par(mar=c(6,6,1,11))
  
  barplot(as.matrix(BARS), space = 0, border = NA, xaxt = "n", ylim=c(0,100), xaxs= "i", las=1,
          col= c("#009E73","#F4EDCA","#E69F00", "#4E84C4","#D16103","#F4EDCA"),
          ylab = "Defining and limiting factors (%)")
  text(-40,100,"B",xpd=T,cex=1.2)
  par(new=T, xpd=TRUE)
  plot(c(0,0)~c(1,210), xaxs="i", yaxt="n", pch = 19, col="#D16103", cex=0.0001, ylab="", las=1,
       xlim=c(0,210), xlab="Time (days after calving)")
  
  legend("topright",inset=c(-0.45,0), 
         legend=rev(c("Protein deficiency","Digestion capacity","Cold stress","Heat stress","Genotype")), 
         fill = rev(c("#009E73","#E69F00", "#4E84C4","#D16103","#F4EDCA")), 
         cex=1, bty = "n", title="Biophysical factors")
  
  
  dev.off()
  closeAllConnections() 
  
  #######################################################################################
  #                                       Figure 6                                      #
  #######################################################################################
  
  # This figure shows the occurrence of heat stress and the daily maximum temperature (A)
  # and the occurrence of cold stress and the daily minimum temperature (B). Bars 
  # indicate cow days. Data on heat and cold stress apply to the whole experimental
  # period, so not just for the first 201 days in lactation.
  
  tiff("M:/Figure6_ev.tiff", width = 9.0, height = 4.5, units = 'in', res = 150)
  par(mfrow=c(1,2))
  
  # Left part of the figure (A)
  par(mar=c(6,6,1,1))
  hist(TempHEATSTRESSexpsall, main = NA,
       xlab = expression(paste("Maximum daily temperature ("^"o"*"C)")),
       ylab = "Number of cow days",
       xlim = c(0,40), las=1, breaks=20,
       ylim = c(0,2000),
       col="grey75")
  text("A",x=4, y= 2000)
  
  # Right part of the figure (B)
  hist(TempCOLDSTRESSexpsall, main = NA,
       xlab = expression(paste("Minimum daily temperature ("^"o"*"C)")),
       ylab = "Number of cow days",
       xlim = c(-16,-2), las=1, breaks=7,
       ylim = c(0,150),
       col="grey75")
  text("B",x=-14.6, y= 150)
  dev.off()
  closeAllConnections()
  
  #######################################################################################
  #                                       Figure 7                                      #
  #######################################################################################
  
  # This figure plots the FPCM production and the crude protein (CP) content of the diet.
  # Data on protein deficiency apply to the whole experimental period, so not just for 
  # the first 201 days in lactation.
  
  # * Small black squares indicate days where the model identified protein defiency as a 
  #   limiting factor.
  # * The red circle indicates the average FPCM production and CP content under protein
  #   defiency
  # * The yellow square indicates the average FPCM production and CP content if protein
  #   deficiency would not occur.
  # * The blue diamond indicates the average FPCM productoi and CP content for all cows
  #   during the whole experimental period (days with protein limitation and abundance
  #   included.)
  
  tiff("M:/Figure7_ev.tiff", width = 5.0, height = 5.0, units = 'in', res = 150)
  par(mar=c(6,6,1,1))
    plot(ProtDefmilkexpsall~ProtDefCPexpsall,
         xlim=c(100,230),ylim=c(0,45),las=1,xaxs="i",yaxs="i",
         xlab= expression(paste("CP content (g kg"^"-1"*" DM)")),
         ylab= "Milk production (kg FPCM per cow per day)",
         pch=".")
    points(mean(ProtDefmilkexpsall)~mean(ProtDefCPexpsall), 
           pch=21,bg="firebrick", lwd=1.5, cex=2)
    points(mean(ProtDefmilkexpsall)*1.044954~mean(ProtDefCPexpsall), 
           pch=22,bg="yellow3", lwd=1.5, cex=2)
    points(mean(31.038657)~mean(168.089423), pch=23,bg="dodgerblue1", lwd=1.5,cex=2)
    
    legend("bottomright",
           legend = c("Cow days with protein deficiency",
                      "Average with protein deficiency",
                      "Average without protein deficiency",
                      "Average experiments"), 
           pch= c(15,21,22,23), pt.bg=(c(NA,"firebrick","yellow3","dodgerblue1")), 
           pt.cex=c(0.3,1.5,1.5,1.5), bty="n", pt.lwd = 1.5, cex=0.82)
    
   dev.off()
   closeAllConnections()
   
   ####################################################################################### 