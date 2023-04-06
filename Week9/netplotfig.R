# Loading the data
library(netplot)
library(igraph)
library(igraphdata)

data("UKfaculty")

col <- V(UKfaculty)$Group

set.seed(1231)

ans <- nplot(
  UKfaculty,
  vertex.color = "yellow",
  edge.line.breaks = 10,
  # edge.curvature = pi/3*2,
  bg.col = "black"
)

png("netplotfig.png", width = 1024, height = 780)
print(ans)
dev.off()

saveRDS(ans, file = "netplotfig.rds")
