library(igraph)
path <- "E:/data/mat"
samplename <- paste("sample", 1:length(dir(path)))
wedges <- lapply(
               sapply(dir(path), function(x) { paste(path, x, sep = '/') }
                     ), function(x) {
                         as.matrix(read.table(x))
                     }
              )
names(wedges) <- samplename
summary(wedges)
for (threshold in 0.1) {
    #对于graph1 阈值大于 0.47以上开始出现孤立节点
    graph <- lapply(1:length(wedges), function(x) {
        matrix <- wedges[[x]]
        graph.adjacency(matrix * (abs(matrix) >= threshold), mode = "undirected", weighted = TRUE, diag = FALSE)
    })
    names(graph) <- samplename
    summary(graph)
    #生成图

    #sink("E:/data/mat/gproperty.txt", split = TRUE)
    ifelse(is_connected(graph[[1]]), print("graph connected."), print("vertex alone exist."))
    #cdis <- component_distribution(graph[[1]],cumulative = FALSE, mul.size = FALSE ,mode = "weak")
    print(mean_distance(graph[[1]], directed = FALSE, unconnected = FALSE))
    print(mean_distance(graph[[1]], directed = FALSE, unconnected = TRUE))
    print(transitivity(graph[[1]], type = "global")) #also named clustering coefficient
    degree <- degree(graph[[1]], mode = "all")
    print("Hub:")
    print(order(degree, decreasing = TRUE)[1:5])
    #统计参数

    #jpeg("sample01.jpg")
    plot(degree, type = "h")
    #dev.off()
    #plot(degree_distribution(graph[[1]]), type = "h")
    #plot(graph[[1]], mark.groups = list(), mark.shape = 1 / 2, mark.expand = 15)
    #plot.igraph(graph[[1]], layout = layout.fruchterman.reingold, vertex.size = 6, vertex.label.cex = .5, edge.arrow.size = .5)
    #绘图
}