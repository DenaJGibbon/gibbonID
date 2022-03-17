#' UMAPBiplotAddSpectrograms
#'
#' @param input.dir.Focal
#' @param output.dir.Focal
#' @param min.freq
#' @param max.freq
#' @param main
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples
UMAPBiplotAddSpectrograms <- function(input.dir.Focal,output.dir.Focal,min.freq,max.freq,main,pattern = '.wav')
  {


Focal.exemplars <- list.files(input.dir.Focal,full.names = T,pattern = '.wav')


print('Creating Spectrograms Step 1 of 3')

if (!dir.exists(output.dir.Focal)){
  dir.create(output.dir.Focal)
  print(paste('Created output dir',output.dir.Focal))
} else {
  print(paste(output.dir.Focal,'already exists'))
}

for(b in 1:length(Focal.exemplars)) {
  #print(b)
  short.wav <- tuneR::readWave(Focal.exemplars[[b]])

  png(filename = paste(output.dir.Focal,b,'Focal.png',sep=''), width=1000)
  temp.spec <- signal::specgram(short.wav@left, Fs = short.wav@samp.rate, n = 1024, overlap = 0)
  plot(temp.spec, xlab = "", ylab = "", ylim = c(min.freq, max.freq), rev(gray(0:512 / 512)),
       axes=F,useRaster = TRUE)

  graphics.off()

}

print('Calculating MFCCs Step 2 of 3')
AcousticSignalsMFCCs <- MFCCFunction(input.dir=input.dir.Focal,
                            min.freq = min.freq, max.freq = max.freq,
                            n.windows = 9, num.cep = 12)

AcousticSignals.umap <-
  umap::umap(AcousticSignalsMFCCs[,c(3:ncol(AcousticSignalsMFCCs))],
             labels=as.factor(AcousticSignalsMFCCs$Class),n_neighbors = 12,
             controlscale=TRUE,scale=3)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],
                   AcousticSignalsMFCCs$class)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2", "Class")

plot.for.AcousticSignals$Class <- as.factor(plot.for.AcousticSignals$Class)

my_plot_AcousticSignals <-
  ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                    y = "Dim.2",
                    color  = "Class") +
  geom_point(size = 3) +
  scale_color_manual(values = matlab::jet.colors (length(unique(plot.for.AcousticSignals$Class)))) +
  theme_bw() + ggtitle(main) + xlab('UMAP: Dim 1')+ylab('UMAP: Dim 2')+ theme(legend.position = "none")

my_plot_AcousticSignals

col.index <- unique(plot.for.AcousticSignals$Class)
xrange <- (abs(range(plot.for.AcousticSignals$Dim.1)[1])+abs(range(plot.for.AcousticSignals$Dim.1)[2]))/25
yrange <- (abs(range(plot.for.AcousticSignals$Dim.2)[1])+abs(range(plot.for.AcousticSignals$Dim.2)[2]))/25
color.vals <- matlab::jet.colors (length(unique(plot.for.AcousticSignals$Class)))

print('Adding Spectrograms to Plot Step 3 of 3')
for(y in 1:length(Focal.exemplars)) {

  #print(y, 'out of', length(Focal.exemplars))
  figure1.png <- magick::image_trim(magick::image_read(paste(output.dir.Focal,y,'Focal.png',sep='')))
  figure1.png <- magick::image_modulate(figure1.png,brightness = 300)

  figure1.png <- magick::image_border(figure1.png,col=color.vals[which(col.index==plot.for.AcousticSignals[y,]$Class)])

  figure1.png <- as.raster(figure1.png)
  #exemplar.index <- Focal.cluster.results@idx[y]

  clust.df.subset <- plot.for.AcousticSignals[y,]
  xmin= clust.df.subset$Dim.1-xrange
  xmax=clust.df.subset$Dim.1+xrange
  ymin=clust.df.subset$Dim.2 + yrange
  ymax=clust.df.subset$Dim.2 -yrange
  my_plot_AcousticSignals <-
    my_plot_AcousticSignals + annotation_raster(figure1.png, xmin,xmax,ymin,ymax)
}


return(my_plot_AcousticSignals)
}

