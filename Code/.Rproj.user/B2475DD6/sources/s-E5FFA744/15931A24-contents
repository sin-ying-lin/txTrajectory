library(shiny)
runApp()
read.csv('TxOutcomePrediction/pid5TOP_valid.csv')

input = list(
  outcome = 'DEPRS',
  iniSev = c(1.5,6),
  NEG = c(1,3),
  DET = c(2,3.5),
  ANT = c(-2,4),
  DIS = c(-2,4),
  PSY = c(-1.5,0)
)

input = list(
  outcome = 'DEPRS',
  iniSev = c(2,9),
  NEG = c(-1,1),
  DET = c(-1,1),
  ANT = c(-1,1),
  DIS = c(-1,1),
  PSY = c(-1,2)
)


input = list(
  outcome = 'DEPRS',
  iniSev = c(-1,1),
  Paranoid = 'Present',
    Schizoid = 'Absent',
    Schizotypal = 'Absent',
    Antisocial = 'Absent',
    Borderline = 'Absent',
    Histrionic = 'Absent',
    Narcissistic = 'Absent',
    Avoidant = 'Absent',
    Dependent = 'Absent',
    Obsessive_compulsive = 'Absent'
)

rsconnect::setAccountInfo(name='sin-ying-lin',
                          token='86490DFEEF16CB12E77913BBDBA78C05',
                          secret='10QuatNCvkiObVJh1pXDeJgA6w0ThhxgmZR+dYmb')
library(rsconnect)

deployApp()
?deployApp
