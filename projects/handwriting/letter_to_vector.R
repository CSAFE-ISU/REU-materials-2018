# convert the letters into data

library(tidyverse)
library(magick)
library(handwriter)
library(googlesheets)
# id for sheet
key <- "1myKHYX1YTJHbiyDhQ_YjnuO5Ft0kPrB7DQ-OG3go_sM"
# find the sheet 
letters <- gs_key(key)
# get the data
dat <- gs_read(letters)
head(dat)

# first, nest the data by filename

dat %>% filter(filename != "test") %>% nest(-filename) %>% 
  mutate(letters = map2(.x = data, .y = filename, .f = letter_to_vector)) -> test2

# write a function that will extract the data from the file for each letter and store it. 

# step 1: read in file from filename 

path <- unique(datsub$filename)
img <- crop(readPNGBinary(path))
# img <- image_read(path)

thinimg <- thinImage(img)
plotImageThinned(img, thinimg)

# step 2: get subsets 
xmin <- floor(datsub[1,]$data_xmin) + 1
xmax <- ceiling(datsub[1,]$data_xmax) +1 
ymin <- floor(datsub[1,]$data_ymin) + 1
ymax <- ceiling(datsub[1,]$data_ymax) +1
# width <- dim(img[[1]])[2]
# height <-dim(img[[1]])[3]
# crop <- paste0(xmax-xmin, "x", ymax-ymin ,"+", xmin, "+", height-ymax)
# # "500x300+10+20" – Crop image to 500 by 300 at position 10,20
# subimg <- image_crop(img, crop)
# subimg

subimg <- img[(ymax - dim(img)[1]+ 1):(dim(img)[1] - ymin  + 1),(xmin+1):(xmax +1)]
plotImage(subimg)

xmin2 <- floor(datsub[1,]$subdata_xmin) + 1
xmax2 <- ceiling(datsub[1,]$subdata_xmax) +1 
ymin2 <- floor(datsub[1,]$subdata_ymin) + 1
ymax2 <- ceiling(datsub[1,]$subdata_ymax) +1

# width <- dim(subimg[[1]])[2]
# height <-dim(subimg[[1]])[3]
# crop <- paste0(xmax2-xmin2, "x", ymax2-ymin2 ,"+", xmin2, "+", height-ymax2)
# # "500x300+10+20" – Crop image to 500 by 300 at position 10,20
# image_crop(subimg, crop)

subsubimg <- subimg[(ymax2 -dim(subimg)[1]+ 1):(dim(subimg)[1] - ymin2  + 1),(xmin2+1):(xmax2 +1)]
plotImage(subsubimg)
subsubimg2 <- image_read(as.raster(subsubimg))

subsubimgvectorized <- as.vector(unique(col2rgb(as.raster(image_scale(subsubimg2, "28x28!")))))


# function to take a data frame from 1 image and turn it into 28x28 letters
# in vector 0-255 greyscale form 

letter_to_vector <- function(imgdf, filename){
  require(handwriter)
  require(magick)
  N <- nrow(imgdf)
  # get image 
  img <- crop(readPNGBinary(filename))
  width <- dim(img)[2]
  height <- dim(img)[1]
  newdata <- data.frame(matrix(0, nrow = N, ncol = 28^2 + 1))
  names(newdata) <- c(paste0("pixel", 1:(28^2)), "label")
  for (i in 1:N){
    rowdat <- imgdf[i,]
    # get first subimg
    xmin <- floor(rowdat$data_xmin) - 1
    xmax <- ceiling(rowdat$data_xmax) +1 
    ymin <- floor(rowdat$data_ymin) - 1
    ymax <- ceiling(rowdat$data_ymax) +1
    if (ymax > height){
      subimg <- img[ (ymax - height):(height - ymin) , xmin:xmax ]
    } else{
      subimg <- img[ (height - ymax):(height - ymin) , xmin:xmax ]
    }
    # get second subimg
    xmin2 <- floor(rowdat$subdata_xmin) - 1
    xmax2 <- ceiling(rowdat$subdata_xmax) +1 
    ymin2 <- floor(rowdat$subdata_ymin) - 1
    ymax2 <- ceiling(rowdat$subdata_ymax) +1
    width2 <- dim(subimg)[2]
    height2 <- dim(subimg)[1]
    if (ymax2 > height2){
      subsubimg <- subimg[ (ymax2 - height2):(height2 - ymin2) , xmin2:xmax2 ]
    } else {
      subsubimg <- subimg[ (height2 - ymax2):(height2 - ymin2) , xmin2:xmax2 ]
    }
    # convert to an image magick object for easier resizing
    subsubimg <- image_read(as.raster(subsubimg))
    # resize the image to 28 x 28
    subsubimg <- image_scale(subsubimg, "28x28!")
    # turn into greyscale vector
    subsubimgvectorized <- as.vector(unique(col2rgb(as.raster(subsubimg))))
    newdata[i,1:784] <- subsubimgvectorized
    newdata[i,]$label <- imgdf$letter[i]
  }
  return(newdata)
}


# next, get all the files together where all authors had all 7 letters 

img_files <- data_frame(paths = list.files("cvl_pngs/", recursive = T, pattern = ".png", full.names = F))
img_files <-img_files %>% separate(paths, into = c("cvl", "doc","authorid", "docid","png"),remove = F)
summary(img_files)
authors <- (img_files %>% count(authorid) %>% filter(n == 7) %>% select(authorid))[[1]] %>% as.vector()
img_files %>% filter(authorid %in% authors)
