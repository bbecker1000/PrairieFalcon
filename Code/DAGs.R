# DAGs

library(dagitty)

# PRFA COvariates

# Dependent Var = State 0,1,2

# AreaType (0/1)
# Park Visitor Number (cont)
# PEFA (0/1)
# Extreme Events (0/1)
# Dec-Feb Total Precipitation (cont)
# short-term drought blend
# long-term drought blend
# max temperature during nestling stage



# Paths
#Core -> Visitor -> state

#PEFA -> state

#Short Drought -> veg(U) -> state

#Long Drought -> veg(U) -> state

#rain -> parasite(U) -> state

#extreme event -> state
  
#max temp -> state

DAG_PRFA <- dagitty("dag{ 
  Core -> Visitor -> STATE ;
  PEFA -> STATE ;
  ShortDrought -> Veg -> STATE;
  LongDrought -> Veg -> STATE;
  Rain -> Parasite -> STATE;
  Extreme -> STATE;
  MaxTemp -> STATE
  Core [exposure]
  Visitor [exposure]
  STATE [outcome]
  ShortDrought [exposure]
  LongDrought [exposure]
  Veg [unobserved]
  Parasite [unobserved]
  Extreme [exposure]
  MaxTemp [exposure]

                     }")

plot(DAG_PRFA)


##### Big Model DAG #####

# Psi
DAG_modBig_psi <- dagitty("dag{ 
  BreedingYear -> Psi;
  AreaType -> Psi;    
  Visitor -> Psi;  
  PEFA -> Psi;
  WinterPrecip -> VegU -> Psi;
  ShortDrought -> VegU -> Psi;
  LongDrought -> VegU -> Psi;
  WinterPrecip -> ShortDrought;
  LongDrought -> ShortDrought
  BreedingYear [exposure]
  AreaType [exposure]
  Visitor [exposure]
  PEFA [exposure]
  WinterPrecip [exposure]
  ShortDrought [exposure]
  LongDrought [exposure]
  Psi [outcome]
  VegU [unobserved]
                     }")
coordinates(DAG_modBig_psi) <- list(x=c(BreedingYear=1,AreaType=2,Visitor=3,PEFA=4,WinterPrecip=5,ShortDrought=5, LongDrought =5, Psi = 3, VegU= 4, PreyU = 4),
                                    y=c(BreedingYear=4,AreaType=4,Visitor=4,PEFA=4,WinterPrecip=3,ShortDrought=2, LongDrought =1, Psi = 2, VegU=2, PreyU = 1))
plot(DAG_modBig_psi)

DAG_modBig_R <- dagitty("dag{ 
  BreedingYear -> R;
  AreaType -> R;    
  Visitor -> R;  
  PEFA -> R;
  WinterPrecip -> VegU -> R;
  ShortDrought -> VegU -> R;
  LongDrought -> VegU -> R;
  RainDuringBreeding -> EctoparasiteU -> R;
  RainDuringBreeding-> R;
  WinterTemp -> R
  MaxTempDuringBreeding -> R
  MinTempDuringBreeding -> R
  WinterTemp -> MinTempDuringBreeding
  WinterPrecip -> ShortDrought;
  LongDrought -> ShortDrought
  BreedingYear [exposure]
  AreaType [exposure]
  Visitor [exposure]
  PEFA [exposure]
  WinterPrecip [exposure]
  ShortDrought [exposure]
  LongDrought [exposure]
  RainDuringBreeding [exposure]
  WinterTemp [exposure]
  MaxTempDuringBreeding [exposure]
  MinTempDuringBreeding [exposure]
  R [outcome]
  VegU [unobserved]
  EctoparasiteU [unobserved]
                     }")
coordinates(DAG_modBig_R) <- list(x=c(BreedingYear=1,AreaType=2,Visitor=3,PEFA=4,WinterPrecip=5,ShortDrought=5, LongDrought =5,RainDuringBreeding = 4,EctoparasiteU = 4,R = 3, WinterTemp = 2, PreyU = 3, MinTempDuringBreeding = 1,MaxTempDuringBreeding = 1, VegU = 4),
                                    y=c(BreedingYear=5,AreaType=5,Visitor=5,PEFA=5,WinterPrecip=4,ShortDrought=3, LongDrought =2,RainDuringBreeding = 1,EctoparasiteU = 2,R = 3, WinterTemp = 1, PreyU = 2, MinTempDuringBreeding = 2,MaxTempDuringBreeding = 3, VegU= 3))

plot(DAG_modBig_R)




# Examples from User Mannual
g <- dagitty("dag{
a -> b b -> c c -> d
}")
# Paths can be specified in one go; the semicolon below is
# optional
g <- dagitty("dag{
a -> b ->c ; c -> d
}")
plot(g)
# Edges can be written in reverse notation
g <- dagitty("dag{
a -> b -> c <- d
}")
plot(g)
# Spaces are optional as well
g <- dagitty("dag{a->b->c<-d}")
plot(g)
# Variable attributes can be set in square brackets
# Example: DAG with one exposure, one outcome, and one unobserved variable
g <- dagitty("dag{
x -> y ; x <- z -> y
x [exposure]
y [outcome]
z [unobserved]
}")
plot(g)
# The same graph as above
g <- dagitty("dag{x[e]y[o]z[u]x<-z->y<-x}")
plot(g)
# A two-factor latent variable model
g <- dagitty("dag {
X <-> Y
X -> a X -> b X -> c X -> d
Y -> a Y -> b Y -> c Y -> d
}")
plot(g)
# Curly braces can be used to "group" variables and
# specify edges to whole groups of variables
# The same two-factor model
g <- dagitty("dag{ {X<->Y} -> {a b c d} }")
plot(g)
# A MAG
g <- dagitty("mag{ a -- x -> y <-> z }")
plot(g)
# A PDAG
g <- dagitty("pdag{ x -- y -- z }")
plot(g)
# A PAG
#g <- dagitty("pag{ x @-@ y @-@ z }")
#plot(g)
