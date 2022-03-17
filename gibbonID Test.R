# Load required libraries
library(devtools)
library(ggpubr)
set.seed(13)
# Add relevant packages
# usethis::use_package('ggpubr')

# To load and test package
load_all()

# Calculate MFCCs for each soundfile
AcousticSignalsMFCCs <- gibbonID::MFCCFunction("/Users/denaclink/Desktop/RStudio Projects/gibbonID/data/FocalTest")

UMAPBiplotAddSpectrograms(input.dir.Focal="/Users/denaclink/Desktop/RStudio Projects/gibbonID/data/FocalTest",
                          output.dir.Focal = "/Users/denaclink/Desktop/RStudio Projects/gibbonID/data/FocalTest/Thumbnails/",
                          min.freq = 400, max.freq = 1800,main='Female Gibbon Calls')

# Divide parts 1-3 into their own functions

AcousticSignals.umap <-
  umap::umap(AcousticSignalsMFCCs[,c(3:ncol(AcousticSignalsMFCCs))],
             labels=as.factor(AcousticSignalsMFCCs$Class),
             controlscale=TRUE,scale=3)

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[,1:2],
                   AcousticSignalsMFCCs$class)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2", "Class")

plot.for.AcousticSignals$Class <- as.factor(plot.for.AcousticSignals$Class)



ggpubr::ggscatter(data = plot.for.AcousticSignals,x = "Dim.1",
                  y = "Dim.2",
                  color  = "Class") +
  geom_point(size = 3)+geom_text(
  label=plot.for.AcousticSignals$Class ,
  nudge_x = 0.25, nudge_y = 0.25,
  check_overlap = T
)



UMAPBiplotAddSpectrograms(input.dir.Focal="/Users/denaclink/Desktop/RStudio Projects/BioacousticsLabActivity/FocalRecordings",
                          output.dir.Focal = "/Users/denaclink/Desktop/RStudio Projects/BioacousticsLabActivity/FocalRecordings/Thumbnails/",
                          min.freq = 400, max.freq = 1500,main='Sound classes')


ExampleBiplot <- gibbonID::UMAPBiplotAddSpectrograms(input.dir.Focal="/Users/denaclink/Desktop/RStudio Projects/gibbonID/data/MultipleSoundClasses",
                                                     output.dir.Focal = "/Users/denaclink/Desktop/RStudio Projects/gibbonID/data/MultipleSoundClasses/Thumbnails/",
                                                     min.freq = 400, max.freq = 6000,main='Multiclass List')

ExampleBiplot
