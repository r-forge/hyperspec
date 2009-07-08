pca <- prcomp (~ spc, data = chondro$., center = TRUE)
scores <- decomposition (chondro, pca$x, label.wavelength = "PC", label.spc = "score / a.u.")
loadings <- decomposition (chondro, t(pca$rotation), scores = FALSE,label.spc = "loading I / a.u.")
center <- decomposition (chondro, t(pca$center), scores = FALSE,label.spc = "I / a.u.")

background <- apply (chondro, 2, quantile, probs = 0.05)
corrected <- sweep (chondro, 2, background, "-")

dist <- pearson.dist (corrected[[]])
dend <- hclust (dist, method = "ward")
plot (dend, main ="chondro")
X11 ()
z <- cutree (dend, k = 3)
print (plotmap (ch, z = as.factor (z)), trellis.args = list (lattice.options = list (main = "chondro")))

cols <- matlab.palette (6)

for (i in 10 : 15){
	i <- 25
	ch <- scores [,,1:i, index = TRUE] %*% loadings [1:i]
	ch <- sweep (ch, 2, center, "+")

	cat ("i",i, "\t range",
	range (ch - chondro), "\t sd",
	sd (as.vector((ch - chondro)[[]])), "\n")
	
	
	background <- apply (ch, 2, quantile, probs = 0.05)
	corrected <- sweep (ch, 2, background, "-")

	dist <- pearson.dist (corrected[[]])
	dend <- hclust (dist, method = "ward")
	X11 ()
	plot (dend, col = "blue")
	
	X11 ()
	z <- cutree (dend, k = 3)
	print (plotmap (ch, z = as.factor (z)))
}




















chondro <- scan.txt.Renishaw ("080606d.txt")
spikiness <- spikefilter2d (chondro[[]])

spc <- spikes.interactive (chondro, spikiness, npts = 20, nspc = 2)
which (is.na (spc))

[1]  255690  256600  257510  258420  271649  272559  284052  284962  295808
[10]  422194  423104  424014  424924  474143  475053  475399  475963  476309
[19]  477219  478129  504019  504929  505839  531402  531419  532329  533239
[28]  604295  605205  606115  651872  652782  653692  654602  698936  699846
[37]  700756  733385  734295  735205  736115 1112963 1113873 1114783 1141921
[46] 1142831 1143741 1144651

chondro [[]] <- spc

plotmap (chondro)
chondro <- chondro [chondro$y > -5]
plotmap (chondro)

write.txt.long (chondro,  file = "chondro.txt",
		cols = c("y", "x", ".wavelength", "spc"), 
		order = c("y", "x", ".wavelength"),
		decreasing = c(FALSE, FALSE, TRUE), 
		na.last = FALSE, quote = FALSE,
		sep = "\t", row.names = FALSE, col.names = FALSE,
		col.labels = FALSE, append = FALSE)


write.txt.long (flu,  file = "flu.txt", cols = c("wavelength", "spc", "c"), 
		order = c("c", "wavelength"),
		decreasing = c(FALSE, TRUE), 
		na.last = FALSE, quote = FALSE,
		sep = "\t", row.names = FALSE, col.names = TRUE,
		col.labels = TRUE, append = FALSE)


write.txt.long (flu,  file = "flu.txt", cols = c("wavelength", "spc"), 
		order = c("wavelength"),
		decreasing = c(TRUE), 
		na.last = FALSE, quote = FALSE,
		sep = "\t", row.names = FALSE, col.names = TRUE,
		col.labels = TRUE, append = FALSE)

write.txt.long (flu[,-1,],  file = "flu.txt", cols = c("wavelength", "spc"), 
		order = c("wavelength"),
		decreasing = c(TRUE), 
		na.last = FALSE, quote = FALSE,
		sep = "\t", row.names = FALSE, col.names = TRUE,
		col.labels = TRUE, append = FALSE)

write.txt.wide (flu,  file = "flu.txt", cols = c("c", "spc"), 
		col.labels = TRUE, header.lines = 2, row.names = TRUE)

write.txt.long (flu,  file = "flu.txt", cols = c("wavelength", "spc", "c"), 
		order = c("c", "wavelength"),
		decreasing = c(FALSE, TRUE))

read.txt.long (file = "flu.txt", cols = list (wavelength = "lambda / nm", spc="I / a.u", c=expression ("/"("c", "mg/l"))))

read.txt.wide (file = "flu.txt", cols = list (c=expression ("/"("c", "mg/l")), spc="I / a.u", .wavelength = "lambda / nm"),
		header = T)












#### chondro vignette data analysis

chondro <- scan.txt.Renishaw ("chondro.txt")

plot (chondro)
summary (chondro)

chondro <- spc.loess(chondro, seq (602, 1800, 4)) 

save (chondro, file = "chondro.rda")

pca <- prcomp (~ spc, data = chondro$., center = TRUE)
.chondro.scores <- decomposition (chondro, pca$x[,1:25], label.wavelength = "PC", label.spc = "score / a.u.", short = "PCA scores")
.chondro.loadings <- decomposition (chondro, t(pca$rotation [, 1:25]), scores = FALSE,label.spc = "loading I / a.u.", short = "PCA loadings")
.chondro.center <- decomposition (chondro, t(pca$center), scores = FALSE,label.spc = "I / a.u.", short = "PCA center")
save (.chondro.scores, .chondro.loadings,.chondro.center, file = ".chondro.rda")


ch <- sweep (.chondro.scores %*% .chondro.loadings, 2, .chondro.center, "+")
summary (ch)