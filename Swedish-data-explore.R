x <- read.table("C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Swedish data/DEAD_TOTAL.txt",
                fill = T, header = T, sep = "/t")
y <- x[x$FORMID==20,]
