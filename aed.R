sorted = sort(dados$peso)
splited = split(sorted, rep(1:3, length.out = length(sorted), each = ceiling(length(sorted)/3)))
pts = c(median(unlist(splited[1])),median(unlist(splited[2])),median(unlist(splited[3])))
pts
