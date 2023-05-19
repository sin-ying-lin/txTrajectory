library(plotly)

df = read.csv('../Data/pid5PDDemoTop_valid.csv')

x = df$SESSION
y = df$DEPRS
pid5 = c('NEG', 'DET') #, 'ANT', 'DIS', 'PSY')

levels = 6
for (pid in pid5){
  assign(paste0(pid,'_cut'), 
         cut(df[,pid], c(-Inf, 0, 0.5, 1, 1.5, 2.5, Inf), labels = c(1,2,3,4,5,6)))
  
  aval = list()
  for(i in 1:levels){
    aval[[i]] <-list(visible = FALSE,
                     name = paste0(pid,'-',i),
                     ID = df$ID[get(paste0(pid,'_cut')) == i],
                     X = x[get(paste0(pid,'_cut')) == i],
                     Y = y[get(paste0(pid,'_cut')) == i])
    
  }
  aval[[1]]$visible = TRUE
  assign(paste0('aval_', pid), aval)
  
  
  
}


str(aval_NE)
# create steps and plot all traces

fig = plot_ly()
vertical = -60
sliderList = list()
listIndex = 0 

for (pid in pid5){
  steps <- list()
  for (i in 1:levels) {
    fig = 
      add_lines(fig, 
                x = get(paste0('aval_',pid))[i][[1]]$X,  
                y = get(paste0('aval_',pid))[i][[1]]$Y,
                mode = 'line',
                color = as.factor(get(paste0('aval_',pid))[i][[1]]$ID),
                visible = get(paste0('aval_',pid))[i][[1]]$visible,
                name = get(paste0('aval_',pid))[i][[1]]$ID, 
                showlegend = F)
    
    step <- list(args = list('visible', 
                             rep(FALSE, length(get(paste0('aval_',pid))))),
                 method = 'update', label = paste0(i))
    
    step$args[[2]][i] = TRUE  
    steps[[i]] = step
    assign(paste0('steps_',pid), steps)
    
  }
  
  listIndex = listIndex + 1
  vertical = vertical + 80
  sliderList[[listIndex]] = list(active = 0, len = 0.3, xanchor = 'auto',
                                 x = 1.3, xanchor = 'left',
                                 y = 1, yanchor = 'center',
                            currentvalue = list(prefix = paste(pid,'Level: '), 
                                                font = list(size = 12)),
                            font = list(size = 12),
                            pad = list(t = vertical, l = 0),
                            steps = get(paste0('steps_',pid)))
}

 
# add slider control to plot
fig <- fig %>%
  layout(sliders = sliderList)

fig
