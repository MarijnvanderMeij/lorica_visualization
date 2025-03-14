# functions ####

##  DEVELOPMENTS
# - combine read functions into one
# - Select soil profiles on the order of the rows/cols inputs, instead of the order of the soils dataframe
# - This should also enable using one profile twice in a transect 
# - Check version of packages, gave an error with an old terra package and re-installation of rasterVis


packages = c('rasterVis', 'terra', 'tidyverse', 'RColorBrewer', 'readxl')
for(package in packages)
{
  if(!package%in%installed.packages())
  {
    install.packages(package)
    }
}
for(package in packages)
{
  require(package, character.only = T)
}
rm(package)
rm(packages)

## Reads in the DTMs for every timestep [years]. Necessary to retrieve elevation information
read_rasters = function(directory = "", years = NULL, scenario_name = "")
{
  # if years are not provided, derive them from the files stored in the location
  if(is.null(years))
  {
    years = list.files(path = directory) %>%
      keep(.p=str_detect, pattern = "dtm") %>%
      str_sub(3, end = -13) %>%
      parse_number() %>%
      sort()
    assign(paste("years",scenario_name, sep = "_"), years, envir = .GlobalEnv)
  }
  
  # Start with the DTM. These files are always available
  dtm_stack = c(dtm_0, rast(paste0(directory,"/0_",years,"_out_dtm.asc")))
  names(dtm_stack) = c("dtm_0", paste0("dtm_",years))
  
  # Add the specified scenario name to the object name and write it to the global environment
  if(nchar(scenario_name)>0){filename = paste("dtm",scenario_name, sep= "_")} else { filename = "dtm" }
  assign(filename, dtm_stack, envir = .GlobalEnv)
  
  # Then check for each different output raster type if these files are available. If so, read them in a raster stack
  variables = c('water_erosion','tillage', 'dz_soil', "soildepth")
  for (v in variables)
  {
    if(file.exists(paste0(directory,"/0_",years[1],"_out_",v,".asc")))
    {
      stack = c(rast(paste0(directory,"/0_",years,"_out_",v,".asc")))
      names(stack) = paste(v, years, sep="_")
      if(nchar(scenario_name)>0){filename = paste(v,scenario_name, sep= "_")} else { filename = v }
      assign(filename, stack, envir = .GlobalEnv)
    }
  }
}

# Read all soils for all time time steps as specified in [years] and writes them to one large file called soils
read_soils = function(directory = "", years = NULL, scenario_name = "")
{
  # if years are not provided, derive them from the files stored in the location
  if(is.null(years))
  {
    years = list.files(path = directory) %>%
      keep(.p=str_detect, pattern = "dtm") %>%
      str_sub(3, end = -13) %>%
      parse_number() %>%
      sort()
  }
  
  # Select location names and read in the soil files
  filenames = list.files(path = directory) %>%
    keep(.p=str_detect, pattern = "allsoils.csv") 
  
  soils = read_csv(paste(directory,filenames, sep = "/")) %>%
    arrange(t, row, col, nlayer)

  if(nchar(scenario_name)>0){filename = paste("soils",scenario_name, sep= "_")} else { filename = "soils" }
  assign(filename, soils, envir = .GlobalEnv)
  
  # Read the excel spreadsheet with descriptions of the variables
  desc = read_xlsx("Description Lorica output.xlsx", sheet = 2)
  assign("output_descriptions", desc, envir = .GlobalEnv)
}


depth_plot = function(soils, row_plot, col_plot, t_plot, variablename, xlab, title = NULL, ylim = NULL)
{
  
  soil = soils %>%
    dplyr::filter(t==t_plot, row == row_plot, col ==col_plot) %>%
    dplyr::select(c("midthick_m", variablename)) %>%
    set_names(c("depth", "variable"))
  
  axis_label = output_descriptions %>%
    dplyr::filter(Variable == variablename) %>%
    dplyr::select(Description)
  
  if(is.null(ylim)){ylim = rev(range(soil$depth))}
  
  p = ggplot(soil, aes(x = variable, y = depth)) +
    geom_line() + 
    scale_y_reverse(lim = ylim) + 
    labs(x = axis_label, y = "Depth [m]", title = title) + 
    theme_bw()
  return(p)
}

transect_plot = function(soils, rows, cols, year_plot, variablename, max_depth = NULL, min_elev = NULL, plot_z0 = TRUE)
{
  # Create matching numbers of rows and columns
  if(length(rows) == 1) { rows = rep(rows, length(cols))}
  if(length(cols) == 1) { rows = rep(cols, length(rows))}
  if(length(rows) != length(cols)){return(print("Error, number of rows and columns not identical"))}
  
  # Calculate distances between observations
  dx = res(dtm_0)[1]
  dx_left = dx * sqrt(c(0,abs(diff(rows)))^2 + c(0,diff(cols))^2)
  dx_right = dx * sqrt(c(abs(diff(rows)), 0)^2 + c(diff(cols), 0)^2)
  
  # Determine elevations for start of simulations
  z_0 = data.frame(x = xFromCol(dtm_0, cols+1),         # Determine x and y coordinates for rows and columns
                   y = yFromRow(dtm_0, rows+1)) %>%
    terra::extract(x = dtm_0, y = .) %>%                # Extract values from dtm_0 for x and y
    set_names(c('ID', 'z')) %>%
    select(z) %>%
    tibble(.,                                           # save to tibble  
           x = cumsum(sqrt(c(0,diff(rows*dx))^2 + c(0,diff(cols*dx))^2))) 

  # When required, filter depth data
  if(!is.null(max_depth)) {soil = soil %>% filter(midthick_m < max_depth)}
  if(!is.null(min_elev)) {soil = soil %>% filter(z > min_elev)}
  
  # Determine legend label
  legend_label = output_descriptions %>%
    dplyr::filter(Variable == variablename) %>%
    dplyr::select(Description)
  
  # wrangle the data and plot
  ## Does not work for for duplicate rows and columns, and non-increasing orders
  soils %>%
    dplyr::mutate(row_col = paste(row, col, sep= "_")) %>%
    dplyr::filter(row_col %in% paste(rows,cols, sep = "_"),
                  t == year_plot) %>%
    
    # Calculate distances between each location with Pythagoras
    dplyr::mutate(x = cumsum(sqrt(c(0,diff(row*dx))^2 + c(0,diff(col*dx))^2))) %>% 
    # Group by rows and columns
    dplyr::group_by(row,col) %>%
    # Calculate distances between soils based on dx_left and dx_right, and the IDs of the groups as index
    dplyr::mutate(xmin = x - dx_left[cur_group_id()]/2,
                  xmax = x + dx_right[cur_group_id()]/2,
                  ymin = z - thick_m,
                  ymax = z) %>%
      ungroup() %>%
    ggplot() + 
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = get(variablename))) + 
    scale_fill_continuous(type = "viridis") + 
    geom_line(data = z_0,mapping = aes(x=x,y=z, linetype = ifelse(plot_z0,"Initial elevation", "")), color = ifelse(plot_z0,"red",NA))+ 
    labs(x = "Distance [m]", y = "Elevation [m]", fill = str_wrap(legend_label, width = 15), linetype = "") + 
    theme_bw() %>%
    suppressWarnings() %>%
    return()
}


  
profile_development = function(soils, row_plot, col_plot, years_plot = years, variablename, max_depth = NULL, min_elev = NULL)
{
  dt = diff(c(0,years_plot))
  
  # Select soil
  soil = soils %>%
    dplyr::filter(row == row_plot, col == col_plot, t %in% years_plot) %>%
    dplyr::group_by(nlayer) %>%
    dplyr::mutate(xmin = t - dt/2, xmax = t + dt/2,ymax = z, ymin = z - thick_m)
  
  # When required, filter depth data
  if(!is.null(max_depth)) {soil = soil %>% filter(midthick_m < max_depth)}
  if(!is.null(min_elev)) {soil = soil %>% filter(z > min_elev)}
  
  # Determine legend label
  legend_label = output_descriptions %>%
    dplyr::filter(Variable == variablename) %>%
    dplyr::select(Description)
  
  ggplot(soil, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = get(variablename))) + 
    geom_rect() + 
    labs(x = "Time [a]", y = "Elevation [m]", fill = str_wrap(legend_label, width = 15)) +
    scale_fill_continuous(type = "viridis") + 
    theme_bw()
}

elevation_transect_development = function(rasterstack, years_plot = years, rows, cols, ylab = "")
{
  dx = res(rasterstack)[1]
  rows = rows + 1
  cols = cols + 1
  if(length(rows) == 1) { rows = rep(rows, length(cols))}
  if(length(cols) == 1) { rows = rep(cols, length(rows))}
  
  # Select indexes of yearly outputs
  years_ind = which(years_plot %in% parse_number(str_sub(names(rasterstack),5,-1)))
  
  elev = data.frame(rasterstack[rows, cols,years_ind])
  names(elev) = years_plot
  if(length(unique(rows))>1) {elev$x_dim = rows} else
  {elev$x_dim = cols}
  
  elev_long = elev %>%
    tidyr::pivot_longer(cols = -x_dim, names_to = "Year", values_to = "elev") %>%
    dplyr::mutate(Year = as.numeric(Year))
  
  ggplot(elev_long, aes(x = x_dim * dx, y = elev, group = Year, color = Year)) + 
    geom_line() + 
    labs(x= "Distance [m]", y = ylab) + 
    theme_bw()
}

plot_raster_layer = function(rasterstack, year_of_plot, years_in_rasterstack)
{
  ind = which(years_in_rasterstack %in% year_of_plot)
  ras = rasterstack[[ind]]
  pl = plot_raster_stack(ras)
  return(pl)
}


create_SOM_stock_map = function(soils, year_of_plot, initial_DEM)
{
  dx = res(dtm_0)[1]
  soils %>%
    filter(t == year_of_plot) %>%
    group_by(row,col) %>%
    summarize(SOM_stock = sum(YOM_kg, OOM_kg), .groups = "drop") %>%
    mutate(x = col * dx + dx / 2,
           y = max(row+1)*dx - (row * dx + dx / 2)) %>%
    select(x,y,SOM_stock) %>%
    rast() %>%
    values(.,mat=F) %>%
    terra::subst(x = initial_DEM, from = values(initial_DEM, mat = F), to = .) %>%
    return()
}

plot_SOM_stock_map = function(soils, year_of_plot, initial_DEM)
{
  create_SOM_stock_map(soils, year_of_plot, initial_DEM) %>%
    plot_raster_stack(legend_lab = "SOM stock [kg/m2]") %>%
    return()
}

plot_raster_stack = function(rasterstack, legend_lab = "Elevation (change) [m]") 
{
  pl = gplot(rasterstack) + 
    geom_tile(aes(fill=value)) + 
    facet_wrap(~ variable) + 
    scale_fill_continuous(type = "viridis") + 
    labs(fill = str_wrap(legend_lab, width = 15)) + 
    theme_bw()
  return(pl)
}




# ## Displays location of row and col on a DTM
# rowcoldisplay=function(row,col,dtm,locpoint=T)
# {
#   layout=c(1,1)
#   plot(dtm)
#   abline(h=extent(dtm)[4]-((row+1)-0.5)*res(dtm)[1])
#   abline(v = extent(dtm)[1]+((col+1)-0.5)*res(dtm)[1])
#   if(locpoint==T){points(y=(extent(dtm)[4]-((row+1)-0.5)*res(dtm)[1]),x=(extent(dtm)[1]+((col+1)-0.5)*res(dtm)[1]),cex=2,pch=16)}
# }
# 
# analyse_elevation_transect_development = function(scenario = "", rows, cols, years,dx, title)
# {
#   if(nchar(scenario)>0) {objectname = paste("dtm", scenario, sep = "_")} else {objectname = dtm}
#   if(exists(objectname))
#   {
#     DTMs = get(objectname)
#   }else {
#     print("Elevation maps for this scenario are not available in the working environment.")
#   }
#   years = c(0,years)
#   par(mfrow = c(1,1))
#   par(mar = c(5.1,4.1,4.1,2.1))
#   rows = rows+1
#   cols = cols+1
#   elevs = matrix(nrow = length(years),ncol = max(length(rows),length(cols)))
#   count = 1
#   for(y in years)
#   {
#     elevs[count,] = as.vector(DTMs[[which(names(DTMs)==paste0('dtm_',y))]][unique(rows),unique(cols)])
#     count = count + 1
#   }
#   colors = gray.colors(n = length(unique(years)),start = 0.1,end = 0.9)
#   
#   layout(t(1:2),widths = c(5,1))
#   # Plot elevations
#   plot(0,pch = NA,xlim = c(0,ncol(elevs)*dx),ylim = range(elevs, na.rm=T),
#        xlab = 'distance [m]', ylab = 'elevation [m]', main = title)
#   xval = (c(0:(ncol(elevs)-1))*dx)
#   for(i in 1:nrow(elevs))
#   {
#     lines(elevs[i,]~xval,col = colors[i])
#   }
#   lines(elevs[1,]~xval,lwd = 2, col = 'blue')
#   lines(elevs[nrow(elevs),]~xval,lwd = 2,col = 'red')
#   
#   # Plot legend
#   par(mar = c(2.5,0.5,2.5,2.5))
#   legend_seq = years
#   image(x=1,y=legend_seq,
#         t(legend_seq),
#         
#         col = colors,axes=F)
#   box(which= 'plot')
#   axis(side = 4,at=round(legend_seq,1))
#   abline(h = c(min(legend_seq),max(legend_seq)), col = c('blue','red'), lwd = 5)
#   title('years [a]', cex.main = 0.75)
#   
#   par(mar=c(5.1, 4.1, 4.1, 2.1), mfrow = c(1,1)) # reset plotting parameters
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
# ## Displays a transect along indicated rows and columns.
# analyse_transect = function(scenario = "", rows,cols,t,dx,variablename, title, bool_t0=F,finesoil=T,variable_range=NULL, ylim = NULL)
# {
#   if(nchar(scenario)>0) {objectname = paste('soils', scenario, sep = '_')} else {objectname = 'soils'}
#   Soils = get(objectname)
#   if(nchar(scenario)>0) {objectname = paste('dtm', scenario, sep = '_')} else {objectname = 'dtm'}
#   DTMs = get(objectname)
#   
#   # check orientation of the transect
#   if(length(rows)<length(cols)){orientation = 'col'; rows = rep(rows, length(cols))}
#   if(length(cols)<length(rows)){orientation = 'row'; cols = rep(cols, length(rows))}
#   
#   # get elevation information
#   xyz_soil = matrix(ncol=5)
#   dtm_0_line = matrix(ncol=2)
#   dem = DTMs[[which(names(DTMs)==paste("dtm",t,sep="_"))]]
#   dxtemp = 0
#   z_trans=c()
#   slope_trans=c(0)
#   
#   z_trans = dem[unique(rows+1),unique(cols+1)]
#   
#   for(i in 2:(length(rows)-1))
#   {
#     if((i-1==0)|(i+1>length(rows))|is.na(z_trans[i-1])|is.na(z_trans[i+1]))
#     {
#       slope_trans = c(slope_trans,0)
#     }
#     else
#     {
#       slope_trans = c(slope_trans,(z_trans[i-1]-z_trans[i+1])/(2*dx))
#     }
#   }
#   slope_trans=c(slope_trans,0)
#   
#   # Derive soil property from each layer from each location along the transect
#   for(i in 1:length(z_trans))
#   {
#     row=rows[i]
#     col=cols[i]
#     z_ini=z_trans[i]
#     slope_ini=slope_trans[i]
#     dtot = 0
#     if(!is.na(z_ini))
#     {
#       depth = z_ini
#       soil = Soils[Soils$row==row&Soils$col==col&Soils$t==t,]
#       for (lay in 1:(nrow(soil)))
#       {
#         dlayer = -soil[lay,"thick_m"]
#         dtot = dtot+dlayer
#         depth=z_ini-soil[lay,'cumth_m']
#         plotvar = sum(soil[lay,variablename])
#         if(finesoil==T)
#         {
#           plotvar = plotvar/(1-sum(soil[lay,' f_coarse']))
#         }
#         
#         xyz_soil=rbind(xyz_soil,c(dxtemp,depth,plotvar,dlayer,slope_ini))
#       }
#       if(i==1)
#       {
#         dtm_0_line=rbind(dtm_0_line,c(0,dtm_0[row+1,col+1]))
#       }
#       # dtm_0_line=rbind(dtm_0_line,c(dist_0,dtm_0[row+1,col+1]))
#       # dist_0=dist_0+dx
#       # dtm_0_line=rbind(dtm_0_line,c(dxtemp-0.5*dx,dtm_0[row+1,col+1]))
#       dxtemp=dxtemp+dx
#       # dtm_0_line=rbind(dtm_0_line,c(dxtemp-0.5*dx,dtm_0[row+1,col+1]))
#       if(bool_t0==T){dtm_0_line=rbind(dtm_0_line,c(dxtemp-0.5*dx,dtm_0[row+1,col+1]))}
#       
#     }
#     
#     
#   }
#   if(bool_t0==T){dtm_0_line=rbind(dtm_0_line,c(dtm_0_line[nrow(dtm_0_line),1]+dx/2,dtm_0_line[nrow(dtm_0_line),2]))}
#   z_trans=z_trans[!is.na(z_trans)]
#   
#   
#   xyz_soil=data.frame(xyz_soil[complete.cases(xyz_soil),])
#   names(xyz_soil)=c('x','z','fplotvar','dz','slope')
#   
#   if(is.null(variable_range)){variable_range = c(min(xyz_soil$fplotvar,na.rm=T),max(xyz_soil$fplotvar,na.rm=T))}
#   
#   if(is.null(ylim)){ylim = c(min(xyz_soil$z),max(xyz_soil$z))}
#   layout(t(1:2),widths = c(5,1))
#   # lay-out: http://stackoverflow.com/questions/24221473/adding-a-continuous-color-gradient-legend-strip-to-plot
#   par(mar=c(4.5,4.5,1.5,1.5))#, oma=rep(3, 4))
#   plot(0,pch = NA, xlim =c(min(xyz_soil$x-dx/2),max(xyz_soil$x)+dx/2),ylim = ylim,
#        xlab = 'dxy (m)',ylab='dz (m)',
#        main = title)
#        # main = paste("Clay (%) transect through rows",min(rows),":",max(rows), ", cols",min(cols),":",max(cols)))
#   # a=gray.colors(101,start = 0,end = 1)
#   # a=terrain.colors(101)
#   a = gray.colors(0.1,0.9,n = 101)
#   
#   slopecount=1
#   for (row1 in 1:nrow(xyz_soil))
#   {
#     if(row1>1)
#     {
#       if(xyz_soil$x[row1]!=xyz_soil$x[row1-1]){slopecount = slopecount +1}
#       
#     }
#     # rect(xleft = xyz_soil$x[row1]-dx/2,xright = xyz_soil$x[row1]+dx/2,
#     #      ytop = xyz_soil$z[row1]-xyz_soil$dz[row1],ybottom = xyz_soil$z[row1],
#     #      col = a[(xyz_soil$fplotvar[row1]-min(xyz_soil$fplotvar))/(max(xyz_soil$fplotvar)-min(xyz_soil$fplotvar))*100+1],border=NA)
#     for (ind in c(-1,1))
#     {
#       if(slopecount+ind==0|slopecount+ind>length(unique(xyz_soil$x))){slope=0}else {slope = (z_trans[slopecount+ind]-z_trans[slopecount])/2}
#       
#       polygon(x = c(xyz_soil$x[row1]+ind*dx/2,
#                     xyz_soil$x[row1]+ind*dx/2,
#                     xyz_soil$x[row1],
#                     xyz_soil$x[row1])+0.5*dx,
#               y=c(xyz_soil$z[row1]+slope,
#                   xyz_soil$z[row1]-xyz_soil$dz[row1]+slope,
#                   xyz_soil$z[row1]-xyz_soil$dz[row1],
#                   xyz_soil$z[row1]),
#               # col = a[(xyz_soil$fplotvar[row1]-min(xyz_soil$fplotvar))/(max(xyz_soil$fplotvar)-min(xyz_soil$fplotvar))*100+1],
#               col = a[round((xyz_soil$fplotvar[row1]-variable_range[1])/(variable_range[2]-variable_range[1])*100+1)],
#               border=NA)
#     }
#     
#     # a[(xyz_soil$fplotvar[row1]-min(xyz_soil$fplotvar))/(max(xyz_soil$fplotvar)-min(xyz_soil$fplotvar))*100+1],border=NA)
#     
#   }
#   if(bool_t0==T){lines(dtm_0_line[,2]~dtm_0_line[,1],col='red',lwd=2)}
#   legend('bottomleft',legend = paste0("t ",t))
#   
#   
#   par(mar = c(2.5,0.5,2.5,2.5))
#   legend_seq = round(seq(from = floor(variable_range[1]*100),
#                          to = ceiling(variable_range[2]*100),
#                          length.out = 10))/100
#   # (ceiling(variable_range[2]*100)-floor(variable_range[1]*100))/101)/100
#   if(legend_seq[1]>10) {legend_seq = round(legend_seq)}
#   legend_seq = unique(legend_seq)
#   legend_col = seq(from = legend_seq[1], to = legend_seq[length(legend_seq)], length.out = 100)
#   image(x=1,y=legend_col,
#         t(legend_col),
#         col = a,axes=F)
#   box(which= 'plot')
#   axis(side = 4,at=round(legend_seq,2))
#   
#   par(mar=c(5.1, 4.1, 4.1, 2.1), mfrow = c(1,1)) # reset plotting parameters
# }
# 
# ## Displays how a soil parameter changes over time in one soil profile
# 
# visualize_elevation_changes = function(years = years, scenario = "")
# {
#   variables = c('water_erosion','tillage', 'dz_soil')
#   
#   out.pos = array(dim = c(length(years),length(variables)))
#   out.neg = array(dim = c(length(years),length(variables)))
#   
#   # Read data from output maps
#   for (var in 1:length(variables))
#   {
#     if(nchar(scenario)>0) {objectname = paste(variables[var], scenario, sep = "_")} else {objectname = variables[var]}
#     if(exists(objectname))
#     {
#       stack = get(objectname)
#       for(t in 1:length(years))
#       {
#         ras = stack[[which(names(stack)==paste(variables[var], years[t], sep = "_"))]]
# 
#         out.pos[t,var] = sum(ras[ras>=0])
#         out.neg[t,var] = sum(ras[ras<=0])
#       }
#     }
#   }
# 
# 
#   # Change total catchment change to catchment-averaged rate
#   years1 = c(0,years)
#   out.pos = out.pos / (dim(ras)[1]*dim(ras)[2]*res(ras)[1]*res(ras)[2]) # convert to m / m2
#   out.neg = out.neg / (dim(ras)[1]*dim(ras)[2]*res(ras)[1]*res(ras)[2])
#   
#   # Calculate elevation changes between two output moments
#   for(var in 1:length(variables))
#   {
#     out.pos[,var] = c(0,diff(out.pos[,var]))
#     out.neg[,var] = c(0,diff(out.neg[,var]))
#   }
# 
#   # Divide by the amount of years in between two outputs
#   out.neg[,] = out.neg[,] / (years - years1[1:(length(years1)-1)])
#   out.pos[,] = out.pos[,] / (years - years1[1:(length(years1)-1)])
#   
#   # Visualization
#   plot(0, pch=NA, 
#        xlim = c(0,length(years)), 
#        ylim = range(c(out.neg, out.pos), na.rm = T), 
#        axes = F, 
#        xlab = 'years [a]', 
#        ylab = 'Rate [m/m2/a]',
#        main = 'Catchment-averaged rates of elevation change')
#   axis(side = 2)
#   axis(1, at = c(0:20), labels = c(0,years))
#   box(which = 'plot')
#   abline(h = 0)
#   legend('topleft', 'Increase', bty = 'n')
#   legend('bottomleft', 'decrease', bty = 'n')
# 
#   for (var in 1:length(variables))
#   {
#     lines(out.neg[,var]~c(0:19), col = var,lwd=2)
#     lines(out.pos[,var]~c(0:19), col = var,lwd=2)
#     lines((out.pos[,var]+out.neg[,var])~c(0:19), col = var, lwd = 2, lty = 2)
#   }
#   legend('topright',legend = c(variables, 'net change'), col = c(1:length(variables),1), lty = c(rep(1,length(variables)), 2), lwd = 2,bg = 'white', )
# }
