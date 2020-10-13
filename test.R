# Title     : TODO
# Objective : TODO
# Created by: rodemarck
# Created on: 01/10/2020

br <- subset(pwt8.0, country=="Brazil", select = c(rgdpna, avh, xr))
br$prod <- br$rgdpna / br$avh
write.table(br, file='dados.txt')