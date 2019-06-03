# Cloud seeding data
#install.packages('HSAUR')
library(HSAUR)
data('clouds', package='HSAUR')
clouds

clouds$seeding <- as.factor(clouds$seeding)

plot(clouds['rainfall'])

# with/without seeding
plot(clouds$seeding, clouds$rainfall)
plot(clouds$cloudcover, clouds$rainfall)
plot(clouds$prewetness, clouds$rainfall)
