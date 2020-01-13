# --- DATASET PREPARATION
# save dataset and resampling instance

# --- DISCUSS WITH MARTIN
# the procedure is not correct
# we use the resampling instance from mlr to perform an outer resampling (on each instance, we run our algorithm)
# however, the final algorithm is evaluated using cross validation on the same splits 
# should we improve this? 
# Taking another "independent" resampling instance? 
# --- 

library(mlr)
library(OpenML)

# --- benchmark tasks ---
datasets = list("sonar" = 39)# , "ionosphere" = 57, "madelon" = 145853,
# 	"hill-valley" = 9893, "wdbc" = 145878, "tecator" = 3716, "lsvt" = 9975, 
# 	"clean1" = 146193, "USPS" = 190142, "isolet" = 190143, "cnae-9" = 190144, 
# 	"semeion" = 190150)

datasets = list("madelon" = 9976, "madeline" = 190392, "AP_Colon_Kidney" = 3971, 
  "arcene" = 10092, "AP_Breast_Colon" = 3979)

path = "data/"

for (i in 1:length(datasets)) {
  OMLtask = convertOMLTaskToMlr(getOMLTask(datasets[[i]]))
  task = OMLtask$mlr.task
  rin = OMLtask$mlr.rin
  task.id = task$task.desc$id
  dir.create(file.path(path, task.id))
  saveRDS(task, paste(path, task.id, "task.rds", sep = "/"))
  saveRDS(rin, paste(path, task.id, "rin.rds", sep = "/"))
}



# --- INITIALLY CREATE DATASETS ON OPENML FOR BETTER COMPARABILITY
# --- ONLY TO BE PERFORMED ONCE

# saveOMLConfig(apikey = "44cbe516b92c80b010115126d74afe04", overwrite = TRUE)

# tocreate = c(1501)

# for (id in tocreate) {

# 	OMLtask = getOMLDataSet(tocreate[[1]])

# 	data = OMLtask$data
# 	data = setDT(data)[Class %in% c(1, 2), ]
# 	data$Class = factor(data$Class, levels = c(1, 2))

# 	dsc = "Binarized version of the semeion dataset (see version 1). Only instances with class labels 1 and 2 from the original dataset are considered."

# 	# Create the description object
# 	desc = makeOMLDataSetDescription(name = "semeion",
# 	  description = dsc,
# 	  url = "https://www.openml.org/d/1501",
# 	  default.target.attribute = "Class")

# 	# Create the OpenML data set
# 	ds = makeOMLDataSet(desc = desc,
# 	  data = data,
# 	  colnames.old = colnames(data),
# 	  colnames.new = colnames(data),
# 	  target.features = "Class")

# 	# Upload dataset
# 	dataset.id = uploadOMLDataSet(ds)
# }