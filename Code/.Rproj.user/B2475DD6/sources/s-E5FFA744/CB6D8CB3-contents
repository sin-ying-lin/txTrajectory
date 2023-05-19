###############
#Load Packages#
###############

packages = c('dplyr', 'psych', 'finalfit')
for (package in packages){
  if(!require(package, character.only = T)){
    install.packages(package)
    library(package, character.only = T)
  }
}


###########
#Load Data#
###########
pid5FactorScore = read.csv('../Data/pid5FactorScore.csv') 

demo = read.csv('../Data/PSU HiTOP Demo.csv')
apply(demo, 2, as.factor)
demo$Age = as.numeric(demo$Age)
demo$YEARS_OF_EDUCATION = as.numeric(demo$YEARS_OF_EDUCATION)

ipdeDx = read.csv('../Data/IPDE_Dx.csv')

top = read.csv('../Data/PSU HiTOP Outcomes UPDATED.csv')
top = top[-which(apply(top, 1, function(x) sum(is.na(x))) == 12), ]
length(unique(top$ID))

###################
#Descriptive Stats#
###################

#Merge PID5, PD and Demographic data
pid5PDDemo = merge(pid5FactorScore, demo, by = 'ID', all.x = T)
pid5PDDemo = merge(pid5PDDemo, ipdeDx, by = 'ID', all.x = T)
pid5PDDemo = pid5PDDemo[!duplicated(pid5PDDemo$ID),]
pid5PDDemo$GENDER = as.factor(pid5PDDemo$GENDER)
str(pid5PDDemo)

write.csv(ff_glimpse(pid5PDDemo, digits = 2)[[1]], '../Results/Descriptive_pid5PDDemo_Numeric.csv', row.names = F)
write.csv(ff_glimpse(pid5PDDemo, digits = 2)[[2]], '../Results/Descriptive_pid5PDDemo_Categorical.csv', row.names = F)

#Merge PID5, PD, Demographic, and TOP Data
pid5PDDemoTop = merge(pid5PDDemo, top, all = F, by = 'ID')
pid5PDDemoTop$ID = as.factor(pid5PDDemoTop$ID)

#Mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Get mode for factorial variables and mean for numeric variables
getMeanOrMode <- function(x) {
  if (is.factor(x)){
    return(getmode(x))
  }
  else return(mean(x, na.rm = T))
}

descriptivepid5PDDemoTop = 
  pid5PDDemoTop %>%
  group_by(ID) %>%
  mutate(sessionN = n()) %>%
  summarise_all(getMeanOrMode) %>%
  mutate(GENDER = as.factor(GENDER))%>%
  ff_glimpse(digits = 2) 

write.csv(descriptivepid5PDDemoTop[[1]], 
          '../Results/Descriptive_pid5PDDemoTop_Numeric.csv', row.names = F)
write.csv(descriptivepid5PDDemoTop[[2]],
          '../Results/Descriptive_pid5PDDemoTop_Categorical.csv', row.names = F)


#Exclude individual records after a long treatment break (i.e., more than 30 days)
#Exclude individuals who attended less than three sessions

pid5PDDemoTop_valid = 
  pid5PDDemoTop %>%
  group_by(ID) %>%
  arrange(DaysSinceFirst, .by_group = T ) %>%
  #Remove session records after a long treatment break (i.e., more than 30 days)
  mutate(invalidSessionN = min(which(IntervalinDays > 30))) %>%
  filter(row_number() < invalidSessionN) %>% 
  filter(IntervalinDays  != 0 | row_number() == 1) %>%
  #Exclude people who attend less than three sessions
  filter(n() >= 2) %>%
  mutate(SESSION = c(1:n()))%>%
  as.data.frame()

write.csv(pid5PDDemoTop_valid, '../Data/pid5PDDemoTop_valid.csv', row.names = F)
write.csv(pid5PDDemoTop_valid, '../Results/pid5PDDemoTop_valid.csv', row.names = F)


descriptivepid5PDDemoTop_valid = 
  pid5PDDemoTop_valid %>%
    group_by(ID) %>%
    mutate(sessionN = n()) %>%
    summarise_all(getMeanOrMode) %>%
    mutate(GENDER = as.factor(GENDER))%>%
    ff_glimpse(digits = 2) 

write.csv(descriptivepid5PDDemoTop_valid[[1]], 
          '../Results/Descriptive_pid5PDDemoTop_Valid_Numeric.csv', row.names = F)
write.csv(descriptivepid5PDDemoTop_valid[[2]],
          '../Results/Descriptive_pid5PDDemoTop_Valid_Categorical.csv', row.names = F)


#PD Prevalence Rates
pdPrev = 
  pid5PDDemoTop_valid %>%
  filter(SESSION == 1) %>%
  select(contains('PD')) %>%
  describe()

write.csv(pdPrev, '../Results/pdPrev.csv')

#ipdeSx
ipdeSxStats = 
  pid5PDDemoTop_valid %>%
    filter(SESSION == 1) %>%
    dplyr::select(ID) %>%
    merge(., ipdeSxD, by = 'ID', all.x = T) %>%
    psych::describe() %>%
    as.data.frame() %>%
    mutate(Sx_Code = rownames(.)) %>%
    merge(., desc, by = 'Sx_Code')

write.csv(ipdeSxStats, '../Results/ipdsSxStats.csv')  
    
#PID5 Stats
pid5Stats =     
  pid5PDDemoTop_valid %>%
  filter(SESSION == 1) %>%
  select('NEG', 'DET', 'ANT', 'DIS', 'PSY')  %>%
  describe()

write.csv(pid5Stats, '../Results/pid5Stats.csv')

#First Session 
top1stSessionStats = 
  pid5PDDemoTop_valid %>%
  filter(SESSION == 1) %>%
  select('DEPRS', 'MANIC', 'PANIC', 'PSYCS', 'SA', 'SLEEP', 
         'SUICD', 'VIOLN',
         'SCONF', 'WORKF', 'SEXFN', 'LIFEQ')  %>%
  describe()

write.csv(top1stSessionStats, '../Results/top1stSessionStats.csv')

#Change Scores 
topDiffStats = 
  pid5PDDemoTop_valid %>%
  group_by(ID) %>%
  select('DEPRS', 'MANIC', 'PANIC', 'PSYCS', 'SA', 'SLEEP', 
         'SUICD', 'VIOLN',
         'SCONF', 'WORKF', 'SEXFN', 'LIFEQ')  %>%
  mutate_all( ~.[n()] - .[1])  %>%
  slice(1)  %>%
  describe()

write.csv(topDiffStats, '../Results/topDiffStats.csv')

#Excluded People
exclude = c()
for (id in unique(pid5PDDemoTop$ID)){
  if(length(which(unique(pid5PDDemoTop_valid$ID) == id)) < 1){
    exclude = append(exclude, id)
  }
} 


##PdPrevi in Excluded People

pdPrevExc = 
  pid5PDDemoTop %>%
  filter(ID %in% exclude) %>%
  group_by(ID) %>%
  filter(row_number() == 1) %>%
  select(contains('PD')) %>%
  describe()

write.csv(pdPrevExc, '../Results/pdPrevExc.csv')

#Treatment Length
descriptivepid5PDDemoTop_excluded = 
  pid5PDDemoTop %>%
  filter(ID %in% exclude) %>%
  group_by(ID) %>%
  mutate(sessionN = n()) %>%
  summarise_all(getMeanOrMode) %>%
  ff_glimpse(digits = 2) 

write.csv(descriptivepid5PDDemoTop_excluded[[1]], 
          '../Results/Descriptive_pid5PDDemoTop_Excluded_Numeric.csv', row.names = F)
write.csv(descriptivepid5PDDemoTop_excluded[[2]],
          '../Results/Descriptive_pid5PDDemoTop_Excluded_Categorical.csv', row.names = F)

str(df)
length(unique(df$ID))
colnames(df)

TOP_corr = 
df %>% 
  filter(SESSION == 1) %>%
  select(47:58) %>%
  cor(., use= 'pair')


write.csv(TOP_corr, '../Results/Top_corr.csv')
