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
