
maxo
for (xx in 1:ncol(g))
{
  # print(xx)
  print(colnames(g)[xx])
  print(max(nchar(g[,xx])))
}
