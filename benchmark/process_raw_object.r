library(plyr)
library(reshape2)

object <- read.csv("data/object.csv")
object$start[object$start == 'null'] <- NA
object$start <- as.POSIXct(gsub('T', ' ', object$start), tz="GMT")

# process the 'children' column to create a list of vectors, convert this to a data frame
split_res <- strsplit(as.character(object$children), ' ')
num_children <- vapply(split_res, function(x) length(x), 1)

object_children_expansion <- object[, 'module_id', drop = FALSE]
for (i in 1:max(num_children)) {
  object_children_expansion[, paste0('child', i)] <- vapply(1:length(split_res), function(n) split_res[[n]][i] , '')
}
# use melt to establish child-parent relations
long_obj <- melt(object_children_expansion, id.vars = "module_id", na.rm = TRUE, variable.name = 'child_field', value.name = 'child')
parent_child_map <- unique(long_obj[, c('module_id', 'child')])
colnames(parent_child_map)[colnames(parent_child_map) == 'module_id'] <- 'parent'

# stop if there is not a unique parent for every child
stopifnot(length(unique(parent_child_map$child)) == nrow(parent_child_map))

parent_child_map_2 <- parent_child_map
colnames(parent_child_map_2)[colnames(parent_child_map_2) == 'child'] <- 'module_id'

# construct tree data (root and level)
tree_data <- unique(object[, 'module_id', drop=FALSE])
tree_data$active <- tree_data$module_id

tree_data$tree_level <- rep(NA, nrow(tree_data))
tree_data$root <- rep(NA, nrow(tree_data))
tree_data_2 <- parent_child_map
colnames(tree_data_2)[match(c('parent', 'child'), colnames(tree_data_2))] <- c('next_active', 'active')

keep_going <- TRUE
level <- 1

while (keep_going) {
  tree_data <- join(tree_data, tree_data_2, by="active", type="left")
  
  level_selector <- is.na(tree_data$next_active) & is.na(tree_data$tree_level)
  tree_data$tree_level[level_selector] <- level
  tree_data$root[level_selector] <- as.character(tree_data$active[level_selector])
  
  tree_data$active <- tree_data$next_active
  tree_data$next_active <- NULL
  level <- level + 1
  keep_going <- any(is.na(tree_data$tree_level))
}
tree_data$active <- NULL

# create base for enhanced object
object_enhanced <- unique(object[, c("module_id", "course_id", "category", "start")])

# ensure that there is only one row for each module id
stopifnot(nrow(object_enhanced) == length(unique(object_enhanced$module_id)))

# add num children column
child_counts <- ddply(parent_child_map, .(parent), nrow)
colnames(child_counts)[match(c('parent', 'V1'), colnames(child_counts))] <- c('module_id', 'num_children')
object_enhanced <- join(object_enhanced, child_counts, type='left', by='module_id')
object_enhanced$num_children[is.na(object_enhanced$num_children)] <- 0

# add parent data
object_enhanced <- join(object_enhanced, parent_child_map_2, by = 'module_id')

# check that no news rows were created with join
stopifnot(nrow(object_enhanced) == length(unique(object_enhanced$module_id)))

# add level data
object_enhanced <- join(object_enhanced, tree_data, by="module_id")

max_level <- max(object_enhanced$tree_level)


# level <- max_level
# current_level_selector <- object_enhanced$tree_level == level
# level_col_name <- paste0('level_', level)
# object_enhanced[, level_col_name] <- rep(NA, nrow(object_enhanced))
# object_enhanced[current_level_selector, level_col_name] <- 


for (level in 1:max_level) {
  current_level_selector <- object_enhanced$tree_level == level
  level_col_name <- paste0('level_', level)
  object_enhanced[, level_col_name] <- rep(NA, nrow(object_enhanced))
  object_enhanced[current_level_selector, level_col_name] <- as.character(object_enhanced$module_id[current_level_selector])
}

for (level in (max_level-1):1) {
  prev_level_col_name <- paste0('level_', level+1)
  level_col_name <- paste0('level_', level)
  
  level_selector <- !is.na(object_enhanced[, prev_level_col_name])
  
  parent_selector <- match(object_enhanced[level_selector,prev_level_col_name], parent_child_map$child)
  object_enhanced[level_selector, level_col_name] <- as.character(parent_child_map$parent[parent_selector])
}

stopifnot(all(object_enhanced$root == object_enhanced$level_1))



stopifnot(length(unique(object_enhanced$module_id)) == length(object_enhanced$module_id))

object_enhanced$num_descendants <- rep(NA, nrow(object_enhanced))
object_enhanced$num_descendant_leaves <- rep(NA, nrow(object_enhanced))

object_enhanced_leaves <- object_enhanced[object_enhanced$num_children == 0, ]

for (level in 1:max_level) {
  col_name <- paste0('level_', level)
  counts <- as.data.frame(table(object_enhanced[, col_name]))
  counts$Freq <- counts$Freq - 1 # a module cannot be its own descendant
  colnames(counts) <- c("module_id", "num_descendants")
  
  leaf_counts <- as.data.frame(table(object_enhanced_leaves[, col_name]))
  leaf_counts$Freq <- leaf_counts$Freq - 1 # a module cannot be its own descendant
  colnames(leaf_counts) <- c("module_id", "num_descendant_leaves")
  
  selector <- object_enhanced$tree_level == level
  object_enhanced$num_descendants[selector] <- counts$num_descendants[match(object_enhanced$module_id[selector], counts$module_id)]
  object_enhanced$num_descendant_leaves[selector] <- leaf_counts$num_descendant_leaves[match(object_enhanced$module_id[selector], leaf_counts$module_id)]
}

object_enhanced$level_1 <- NULL

# do some reordering
first_cols <- c("module_id", "course_id", "category", "parent", "start", "tree_level", "num_children", "num_descendants", "num_descendant_leaves")
all_cols <- c(first_cols, setdiff(colnames(object_enhanced), first_cols))
object_enhanced <- object_enhanced[, match(all_cols, colnames(object_enhanced))]

write.csv(object_enhanced, "data/object_enhanced.csv", quote=FALSE, row.names=FALSE)
