library(tercen)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)


options("tercen.workflowId" = "3ac390fd32fea6c2f28ea5a014000c70")
options("tercen.stepId"     = "008be523-7d4f-459c-b3f8-ca0ab87f28ae")

getOption("tercen.workflowId")
getOption("tercen.stepId")
getOption("tercen.serviceUri")

options("tercen.serviceUri"="http://tercen:5400/api/v1/")
options("tercen.username"= "admin")
options("tercen.password"= "admin")

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
  mutate(.ci = 0) %>%
  ctx$addNamespace() %>%
  ctx$save()
