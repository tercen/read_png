library(tercen)
library(tidyr)
library(dplyr)


ctx = tercenCtx()

if (!any(ctx$cnames == "documentId")) stop("Column factor documentId is required")

# extract files
df <- ctx$cselect()

docId = df$documentId[1]
doc = ctx$client$fileService$get(docId)
filename = tempfile()
writeBin(ctx$client$fileService$download(docId), filename)
on.exit(unlink(filename))

img <- png::readPNG(filename)

matrix <- 0.299 * img[,,1] + 0.587 * img[,,2] + 0.114 * img[,,3]

col_num <- 1:ncol(matrix)
row_num <- 1:nrow(matrix)

col_id <-sprintf(paste0("c%0", max(nchar(as.character(col_num))), "d"), col_num)
row_id <-sprintf(paste0("r%0", max(nchar(as.character(row_num))), "d"), row_num)

colnames(matrix) <- col_id
rownames(matrix) <- row_id

matrix_table <- as_tibble(matrix) %>% 
  mutate(row_id = row_id) %>%
  pivot_longer(-row_id, names_to ="col_id", values_to = "pixel_value") %>%
  mutate_if(is.integer, as.double) %>%
  mutate(.ci = 0L) %>%
  ctx$addNamespace() 


matrix_table %>%
  ctx$save()